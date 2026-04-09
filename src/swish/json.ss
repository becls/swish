;;; Copyright 2018 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!chezscheme
(library (swish json)
  (export
   json-stack->string
   json:bytevector->object
   json:cells
   json:custom-write
   json:delete!
   json:extend-object
   json:key<?
   json:make-object
   json:object->bytevector
   json:object->string
   json:object?
   json:pretty
   json:read
   json:read-options
   json:ref
   json:set!
   json:size
   json:string->object
   json:update!
   json:write
   json:write-object
   json:write-options
   json:write-structural-char
   stack->json
   )
  (import
   (chezscheme)
   (swish dsm)
   (swish erlang)
   (swish io)
   (swish meta)
   (swish options)
   (swish string-utils)
   )

  (include "unsafe.ss")

  (define-options json:read-options
    (optional
     [extended-identifiers?
      (default #f)
      (must-be boolean?)]
     [inflate-object
      (default #f)
      (must-be valid-inflate-object?)]
     [inflate-symbol
      (default #f)
      (must-be (lambda (x) (or (not x) (procedure/arity? #b10 x))))]
     [json5?
      (default #t)
      (must-be boolean?)]
     ))

  (define-options json:write-options
    (optional
     [custom-write
      (default #f)
      (must-be valid-custom-write?)]
     [json5?
      (default #t)
      (must-be boolean?)]
     ))

  (define-syntax extend-object-internal
    (syntax-rules ()
      [(_ x $ht (key val) ...)
       (let ([ht $ht])
         (declare-unsafe-primitives symbol-hashtable-set!)
         (symbol-hashtable-set! ht (parse-key key x) val)
         ...
         ht)]))

  (define-syntax (parse-key x)
    (syntax-case x (unquote)
      [(_ id form) (identifier? #'id) #'(quote id)]
      [(_ (unquote e) form) #'e]
      [(_ key form) (syntax-error #'form (format "invalid key ~s in" (datum key)))]))

  (define (verify-json-object who x)
    (unless (json:object? x)
      (bad-arg who x))
    x)

  (define-syntax (json:extend-object x)
    (syntax-case x ()
      [(_ $ht (key val) ...)
       #`(extend-object-internal #,x (verify-json-object 'json:extend-object $ht)
           (key val) ...)]))

  (define-syntax (json:make-object x)
    (syntax-case x ()
      [(_ (key val) ...)
       #`(extend-object-internal #,x (make-json-object)
           (key val) ...)]))

  (define (make-json-object)
    (make-hashtable symbol-hash eq?))

  (define (json:object? x)
    (symbol-hashtable? x))

  (define (json:cells x)
    (declare-unsafe-primitives hashtable-cells)
    (hashtable-cells (verify-json-object 'json:cells x)))

  (define (walk-path who obj full-path extend? default found)
    (declare-unsafe-primitives symbol-hashtable-ref symbol-hashtable-set!)
    (verify-json-object who obj)
    (when (null? full-path) (bad-arg who full-path))
    (if (symbol? full-path)
        (found obj full-path default)
        (let lp ([obj obj] [path full-path])
          (match path
            [(,key)
             (guard (symbol? key))
             (found obj key default)]
            [(,key1 . ,more)
             (guard (symbol? key1))
             (let ([hit (symbol-hashtable-ref obj key1 #f)])
               (cond
                [(json:object? hit) (lp hit more)]
                [extend?
                 (let ([new (json:make-object)])
                   (symbol-hashtable-set! obj key1 new)
                   (lp new more))]
                [else default]))]
            [,_ (bad-arg who full-path)]))))

  (define (json:ref obj path default)
    (declare-unsafe-primitives symbol-hashtable-ref)
    (walk-path 'json:ref obj path #f default
      (lambda (obj sym default)
        (symbol-hashtable-ref obj sym default))))

  (define (json:set! obj path value)
    (declare-unsafe-primitives symbol-hashtable-set!)
    (walk-path 'json:set! obj path #t value
      (lambda (obj key value)
        (symbol-hashtable-set! obj key value))))

  (define (json:update! obj path f default)
    (declare-unsafe-primitives symbol-hashtable-update!)
    (unless (procedure? f) (bad-arg 'json:update! f))
    (walk-path 'json:update! obj path #t default
      (lambda (obj key default)
        (symbol-hashtable-update! obj key f default))))

  (define (json:delete! obj path)
    (declare-unsafe-primitives symbol-hashtable-delete!)
    (walk-path 'json:delete! obj path #f (void)
      (lambda (obj key default)
        (symbol-hashtable-delete! obj key))))

  (define (json:size obj)
    (declare-unsafe-primitives symbol-hashtable-size)
    (hashtable-size (verify-json-object 'json:size obj)))

  (define (json:unexpected context what pos name)
    (throw `#(json:unexpected ,context ,what ,pos ,name)))

  (define (unexpected-input context what ip)
    (json:unexpected context what
      (and (port-has-port-position? ip)
           ;; read-char advanced the port-position if it returned a
           ;; character.
           (let ([pos (port-position ip)])
             (if (eof-object? what)
                 pos
                 (- pos 1))))
      (port-name ip)))

  (define (unexpected-str context str ip)
    (let ([len (string-length str)])
      (json:unexpected context
        (if (= len 1)
            (string-ref str 0)
            (string->symbol str))
        (and (port-has-port-position? ip) (- (port-position ip) len))
        (port-name ip))))

  (define (next-char context ip)
    (declare-unsafe-primitives read-char)
    (let ([x (read-char ip)])
      (if (eof-object? x)
          (unexpected-input context x ip)
          x)))

  (define (structural? c)
    (memv c '(#\{ #\} #\: #\, #\[ #\])))

  (define (ws? c json5?)
    (declare-unsafe-primitives char=?)
    (cond
     [(not json5?)
      (memv c '(#\x20 #\x09 #\x0A #\x0D))]
     [(char-whitespace? c)] ; char-whitespace? includes everything in Zs
     [(char=? c #\xFEFF)]   ; Byte order mark
     [else #f]))

  (define (next-non-ws ip json5?)
    (declare-unsafe-primitives char=? peek-char read-char)
    (define (read-line-comment)
      (let ([c (read-char ip)])
        (cond
         [(eof-object? c) c]
         [(memv c '(#\newline #\return #\x2028 #\x2029))
          (next-non-ws ip json5?)]
         [else (read-line-comment)])))
    (define (read-block-comment)
      (let ([c (next-char 'comment ip)])
        (cond
         [(char=? c #\*)
          (let inner-lp ()
            (let ([c2 (next-char 'comment ip)])
              (cond
               [(char=? c2 #\/) (next-non-ws ip json5?)]
               [(char=? c2 #\*) (inner-lp)]
               [else (read-block-comment)])))]
         [else (read-block-comment)])))
    (let ([c (read-char ip)])
      (cond
       [(eof-object? c) c]
       [(ws? c json5?)
        (next-non-ws ip json5?)]
       [(and json5? (char=? c #\/))
        (let ([c2 (peek-char ip)])
          (cond
           [(eof-object? c2) c]
           [(char=? c2 #\/)
            (read-char ip)
            (read-line-comment)]
           [(char=? c2 #\*)
            (read-char ip)
            (read-block-comment)]
           [else
            c]))]
       [else c])))

  (define (unicode-escape context ip op)
    (declare-unsafe-primitives fx+ fx<= fxlogand fxsll integer->char write-char)
    (let ([x (read-4hexdig ip)])
      (cond
       [(fx<= #xD800 x #xDBFF) ;; high surrogate
        (expect-char context #\\ ip)
        (expect-char context #\u ip)
        (let ([y (read-4hexdig ip)])
          (unless (fx<= #xDC00 y #xDFFF)
            (throw 'invalid-surrogate-pair))
          (write-char
           (integer->char
            (fx+ (fxsll (fxlogand x #x3FF) 10)
                 (fxlogand y #x3FF)
                 #x10000))
           op))]
       [(fx<= #xDC00 x #xDFFF) (throw 'invalid-surrogate-pair)]
       [else (write-char (integer->char x) op) #t])))

  (define (string-escape ip op)
    (declare-unsafe-primitives char=? unread-char write-char)
    (let ([c (next-char 'string ip)])
      (cond
       [(char=? c #\") (write-char c op) #t]
       [(char=? c #\\) (write-char c op) #t]
       [(char=? c #\/) (write-char c op) #t]
       [(char=? c #\b) (write-char #\x08 op) #t]
       [(char=? c #\f) (write-char #\x0C op) #t]
       [(char=? c #\n) (write-char #\x0A op) #t]
       [(char=? c #\r) (write-char #\x0D op) #t]
       [(char=? c #\t) (write-char #\x09 op) #t]
       [(char=? c #\u) (unicode-escape 'string ip op)]
       [else
        (unread-char c ip)
        #f])))

  (define (string-escape5 ip op)
    (declare-unsafe-primitives char<=? char=? unread-char write-char)
    (or (string-escape ip op)
        (let ([c (read-char ip)])
          (cond
           [(char=? c #\') (write-char c op) #t]
           [(char=? c #\v) (write-char #\x0B op) #t]
           [(char=? c #\0)
            (let ([c (next-char 'string ip)])
              (cond
               [(char<=? #\0 c #\9)
                (unread-char c ip)
                #f]
               [else
                (unread-char c ip)
                (write-char #\x00 op)
                #t]))]
           [(char=? c #\x)
            (let ([x (read-2hexdig ip)])
              (write-char (integer->char x) op) #t)]
           [(char=? c #\newline) #t]
           [(char=? c #\return)
            (let ([c (next-char 'string ip)])
              (unless (char=? c #\newline)
                (unread-char c ip))
              #t)]
           [(char=? c #\x2028) #t]      ; line separator
           [(char=? c #\x2029) #t]      ; paragraph separator
           [(char<=? #\1 c #\9)
            (unread-char c ip)
            #f]
           [else
            (write-char c op) #t]))))

  (define (read-string ip op mark json5?)
    (declare-unsafe-primitives char<=? char=? write-char)
    (let ([c (next-char 'string ip)])
      (cond
       [(char=? c mark) (get-json-buffer-string op)]
       [(char=? c #\\)
        (or (if json5?
                (string-escape5 ip op)
                (string-escape ip op))
            (unexpected-input 'string (read-char ip) ip))
        (read-string ip op mark json5?)]
       [(if json5?
            (memv c '(#\newline #\return))
            (char<=? c #\x1F))
        (unexpected-input 'string c ip)]
       [else (write-char c op) (read-string ip op mark json5?)])))

  (define (json5-strict-char? c first?)
    (cond
     [(memq (char-general-category c) '(Lu Ll Lt Lm Lo Nl)) #t]
     [(memv c '(#\$ #\_)) #t]
     [first? #f]
     [(memq (char-general-category c) '(Mn Mc Nd Pc)) #t]
     [(memv c '(#\x200C #\x200D)) #t]   ; ZWNJ, ZWJ
     [else #f]))

  (define (identifier-helper context ip op first? clean?)
    (declare-unsafe-primitives char=? peek-char read-char unread-char write-char)
    (let lp ([first? first?] [clean? clean?])
      (let ([c (peek-char ip)])
        (cond
         [(or (eof-object? c)
              (char-whitespace? c)
              (structural? c))
          (values (get-output-string op) clean?)]
         [(char=? c #\\)
          (read-char ip)
          (let ([c (next-char context ip)])
            (cond
             [(char=? c #\u)
              (unicode-escape context ip op)
              (lp #f clean?)]
             [else
              (unexpected-input context c ip)]))]
         [else
          (read-char ip)
          (cond
           [(and (char=? c #\/)
                 (memv (peek-char ip) '(#\/ #\*)))
            (unread-char c ip)
            (values (get-output-string op) clean?)]
           [else
            (write-char c op)
            (lp #f (and clean? (json5-strict-char? c first?)))])]))))

  (define (read-unquoted-key ip op extended?)
    (define strict? (not extended?))
    (let-values ([(str clean?) (identifier-helper '|object key| ip op #t strict?)])
      (cond
       [(and strict? (not clean?))
        (unexpected-str '|object key| str ip)]
       [else
        (let ([sym (string->symbol str)])
          (when (memq sym '(true false null))
            (unexpected-str '|object key| str ip))
          sym)])))

  (define (read-identifier chars ip op json5? extended? inflate-symbol)
    (define strict? (not extended?))
    (declare-unsafe-primitives write-char)
    (let lp ([chars chars] [first? #t] [clean? strict?])
      (match chars
        [()
         (let-values ([(str clean?) (identifier-helper 'value ip op first? clean?)])
           (let ([sym (string->symbol str)])
             (cond
              [(eq? sym 'true) #t]
              [(eq? sym 'false) #f]
              [(eq? sym 'null) 'null]
              [(and json5?
                    (match sym
                      [Infinity +inf.0]
                      [+Infinity +inf.0]
                      [-Infinity -inf.0]
                      [NaN +nan.0]
                      [+NaN +nan.0]
                      [-NaN -nan.0]
                      [,_ #f]))]
              [(and strict? (not clean?))
               (unexpected-str 'value str ip)]
              [inflate-symbol
               (inflate-symbol sym)]
              [else
               (unexpected-str 'value str ip)])))]
        [(,c . ,chars)
         (write-char c op)
         (lp chars #f (and clean? (json5-strict-char? c first?)))])))

  (define (read-4hexdig ip)
    (declare-unsafe-primitives fx+ fxsll)
    (let* ([x (hex-digit ip)]
           [x (fx+ (fxsll x 4) (hex-digit ip))]
           [x (fx+ (fxsll x 4) (hex-digit ip))]
           [x (fx+ (fxsll x 4) (hex-digit ip))])
      x))

  (define (read-2hexdig ip)
    (declare-unsafe-primitives fx+ fxsll)
    (let* ([x (hex-digit ip)]
           [x (fx+ (fxsll x 4) (hex-digit ip))])
      x))

  (define (expect-char context expected ip)
    (declare-unsafe-primitives char=?)
    (let ([c (next-char context ip)])
      (unless (char=? c expected)
        (unexpected-input context c ip))))

  (define-syntax make-write-string
    (syntax-rules ()
      [(_ s op)
       (lambda (s op)
         (declare-unsafe-primitives char->integer char<=? fx+ fx= string-length string-ref write-char)
         (write-char #\" op)
         (do ([i 0 (fx+ i 1)] [n (string-length s)]) [(fx= i n)]
           (let ([c (string-ref s i)])
             (cond
              [(memv c '(#\" #\\)) (write-char #\\ op) (write-char c op)]
              [(char<=? c #\x1F) (fprintf op "\\u~4,'0x" (char->integer c))]
              [else (write-char c op)])))
         (write-char #\" op))]))

  (define write-string (make-write-string s op))

  (define (char->hex-digit c)
    (declare-unsafe-primitives char->integer char<=? fx-)
    (cond
     [(char<=? #\0 c #\9) (digit-value c)]
     [(char<=? #\A c #\F) (fx- (char->integer c) (fx- (char->integer #\A) 10))]
     [(char<=? #\a c #\f) (fx- (char->integer c) (fx- (char->integer #\a) 10))]
     [else #f]))

  (define (hex-digit ip)
    (let ([c (next-char '|hexadecimal number| ip)])
      (or (char->hex-digit c)
          (unexpected-input '|hexadecimal number| c ip))))

  (define (digit-value c)
    (declare-unsafe-primitives char->integer fx-)
    (fx- (char->integer c) (char->integer #\0)))

  (define (read-unsigned* chars ip json5?)
    (declare-unsafe-primitives fx- unread-char)
    (let-values ([(chars mantissa n c) (read-digits chars ip 0 0)])
      (cond
       [(eqv? c #\.)
        (let-values ([(chars mantissa m c) (read-digits chars ip mantissa 0)])
          (cond
           [(and (not json5?) (eqv? m 0))
            (unexpected-input 'number c ip)]
           [(memv c '(#\e #\E)) (read-exp ip mantissa m)]
           [else
            (unless (eof-object? c)
              (unread-char c ip))
            (scale mantissa (fx- m))]))]
       [(memv c '(#\e #\E)) (read-exp ip mantissa 0)]
       [else
        (unless (eof-object? c)
          (unread-char c ip))
        mantissa])))

  (define (read-unsigned chars ip json5?)
    (declare-unsafe-primitives peek-char read-char)
    (let ([x (read-unsigned* chars ip json5?)])
      (let ([c (peek-char ip)])
        (unless (or (eof-object? c)
                    (ws? c json5?)
                    (structural? c))
          (read-char ip)
          (unexpected-input 'number c ip)))
      x))

  (define (read-digits chars ip mantissa n)
    (declare-unsafe-primitives car cdr char<=? fx+)
    (let ([c (if (null? chars)
                 (read-char ip)
                 (car chars))]
          [chars (if (null? chars)
                     chars
                     (cdr chars))])
      (if (or (eof-object? c)
              (not (char<=? #\0 c #\9)))
          (values chars mantissa n c)
          (read-digits chars ip (+ (* mantissa 10) (digit-value c)) (fx+ n 1)))))

  (define (read-hex-digits ip mantissa n json5?)
    (declare-unsafe-primitives fx+ read-char unread-char)
    (let ([c (read-char ip)])
      (cond
       [(eof-object? c)
        (values mantissa n c)]
       [(or (ws? c json5?)
            (structural? c))
        (unread-char c ip)
        (values mantissa n c)]
       [(char->hex-digit c) =>
        (lambda (value)
          (read-hex-digits ip (+ (* mantissa 16) value) (fx+ n 1) json5?))]
       [else
        (unexpected-input '|hexadecimal number| c ip)])))

  (define (read-hex ip json5?)
    (let-values ([(mantissa n c) (read-hex-digits ip 0 0 json5?)])
      (when (and (eqv? n 0) (eqv? mantissa 0))
        (unexpected-input '|hexadecimal number| c ip))
      mantissa))

  (define (read-exp ip mantissa m)
    (declare-unsafe-primitives unread-char)
    (let ([c (next-char 'exponent ip)])
      (case c
        [(#\+) (scale mantissa (- (read-int ip) m))]
        [(#\-) (scale mantissa (- (- (read-int ip)) m))]
        [else (unread-char c ip) (scale mantissa (- (read-int ip) m))])))

  (define (read-int ip)
    (declare-unsafe-primitives unread-char)
    (let-values ([(chars int n c) (read-digits '() ip 0 0)])
      (cond
       [(eqv? n 0) (unexpected-input 'exponent c ip)]
       [else
        (unless (eof-object? c)
          (unread-char c ip))
        int])))

  (define (scale mantissa exponent)
    (cond
     [(eqv? mantissa 0) 0.0]
     [(> exponent 308) +inf.0]
     [(>= exponent 0) (inexact (* mantissa (expt 10 exponent)))]
     [else (inexact (/ mantissa (expt 10 (- exponent))))]))

  (define (string->key s)
    (declare-unsafe-primitives char=? fx- fx>= string-length string-ref)
    (let ([len (string-length s)])
      (or (and (fx>= len 6)
               (char=? (string-ref s 0) #\#)
               (char=? (string-ref s 1) #\{)
               (char=? (string-ref s (fx- len 1)) #\})
               (guard (c [else #f])
                 (read (open-input-string s))))
          (string->symbol s))))

  (define (make-weak-process-local init refresh!)
    (define param (make-process-parameter #f))
    (lambda ()
      (let ([val (cond [(param) => car] [else #!bwp])])
        (if (not (eq? val #!bwp))
            (refresh! val)
            (let ([val (init)])
              (param (weak-cons val #f))
              val)))))

  (define json-buffer (make-weak-process-local open-output-string
                        (lambda (op) (set-port-output-index! op 0) op)))
  (define (get-json-buffer-string op)
    ;; We don't reset string output port's buffer via get-output-string since
    ;; we will likely have to regrow the buffer. The collector can reclaim the
    ;; buffer when the R rd call is complete.
    (let* ([end (port-output-index op)]
           [str (make-string end)])
      (string-copy! (port-output-buffer op) 0 str 0 end)
      (set-port-output-index! op 0)
      str))

  ;; Strings and objects are common enough that it appears
  ;; to be worth resolving json-buffer eagerly and making
  ;; it available via json-buf within R.
  (define-syntactic-monad R
    json-buf
    extended-identifiers?
    inflate-object
    inflate-symbol
    json5?
    )

  (R define (rd ip eof-who)
    (declare-unsafe-primitives char=?)
    (let ([c (next-non-ws ip json5?)])
      (cond
       [(eof-object? c)
        (if eof-who
            (unexpected-input eof-who c ip)
            c)]
       [(memv c '(#\} #\: #\, #\] #\\)) (unexpected-input 'value c ip)]
       [(or (char=? c #\") (and json5? (char=? c #\')))
        (read-string ip json-buf c json5?)]
       [(char=? c #\[)
        (R read-array () ip)]
       [(char=? c #\{)
        (R read-object () ip)]
       [else
        (R read-number-or-identifier () c ip)])))

  (R define (read-array ip)
    (declare-unsafe-primitives char=? unread-char)
    (let lp ([acc '()])
      (let ([c (next-non-ws ip json5?)])
        (cond
         [(eof-object? c) (unexpected-input 'array c ip)]
         [(and (char=? c #\]) (null? acc)) '()]
         [else
          (unread-char c ip)
          (let* ([acc (cons (R rd () ip 'array) acc)]
                 [c (next-non-ws ip json5?)])
            (cond
             [(eof-object? c) (unexpected-input 'array c ip)]
             [(char=? c #\,)
              (if json5?
                  (let ([c (next-non-ws ip json5?)])
                    (cond
                     [(eof-object? c) (lp acc)]
                     [(char=? c #\]) (reverse acc)]
                     [else
                      (unread-char c ip)
                      (lp acc)]))
                  (lp acc))]
             [(char=? c #\]) (reverse acc)]
             [else (unexpected-input 'array c ip)]))]))))

  (R define (read-object ip)
    (declare-unsafe-primitives char=? hashtable-size symbol-hashtable-set! unread-char)
    (inflate-object
     (let ([obj (json:make-object)])
       (define (read-key)
         (let ([c (next-non-ws ip json5?)])
           (cond
            [(eof-object? c) (unexpected-input '|object key| c ip)]
            [(or (char=? c #\") (and json5? (char=? c #\')))
             (read-value (string->key (read-string ip json-buf c json5?)))]
            [(and (char=? c #\}) (eqv? (hashtable-size obj) 0)) obj]
            [(and json5? (not (memv c '(#\} #\: #\, #\]))))
             (unread-char c ip)
             (read-value (read-unquoted-key ip json-buf extended-identifiers?))]
            [else (unexpected-input '|object key| c ip)])))
       (define (read-value key)
         (let ([c (next-non-ws ip json5?)])
           (if (or (eof-object? c)
                   (not (char=? c #\:)))
               (unexpected-input '|object value| c ip)
               (symbol-hashtable-set! obj key (R rd () ip '|object value|))))
         (let ([c (next-non-ws ip json5?)])
           (cond
            [(eof-object? c)
             (unexpected-input 'object c ip)]
            [(char=? c #\,)
             (if json5?
                 (let ([c (next-non-ws ip json5?)])
                   (cond
                    [(eof-object? c) (read-key)]
                    [(char=? c #\}) obj]
                    [else
                     (unread-char c ip)
                     (read-key)]))
                 (read-key))]
            [(char=? c #\}) obj]
            [else (unexpected-input 'object c ip)])))
       (read-key))))

  (R define (read-number-or-identifier c ip)
    (declare-unsafe-primitives char<=? char=? peek-char read-char)
    (define (sign rchars)
      ;; A decimal point is allowed without a leading digit by JSON5.
      (let ([c (peek-char ip)])
        (cond
         [(or (eof-object? c)
              (ws? c json5?)
              (structural? c))
          (start-identifier rchars)]
         [(char=? #\0 c)
          (read-char ip)
          (zero (cons c rchars))]
         [(char<=? #\1 c #\9)
          (read-char ip)
          (start-number (cons c rchars))]
         [(and json5? (char=? c #\.))
          (read-char ip)
          (point (cons c rchars))]
         [else
          (read-char ip)
          (start-identifier (cons c rchars))])))
    (define (zero rchars)
      (let ([c (peek-char ip)])
        (cond
         [(or (eof-object? c)
              (ws? c json5?)
              (structural? c))
          (start-number rchars)]
         [(char<=? #\0 c #\9)
          (read-char ip)
          (unexpected-input 'number #\0 ip)]
         [(not json5?)
          (start-number rchars)]
         [else
          (read-char ip)
          (start-number (cons c rchars))])))
    (define (point rchars)
      (let ([c (peek-char ip)])
        (cond
         [(or (eof-object? c)
              (ws? c json5?)
              (structural? c))
          (start-identifier rchars)]
         [(char<=? #\0 c #\9)
          (read-char ip)
          (start-number (cons c rchars))]
         [else
          (read-char ip)
          (start-identifier (cons c rchars))])))
    (define (start-number rchars)
      (let lp ([chars (reverse rchars)])
        (match chars
          [(#\- . ,rest) (- (lp rest))]
          [(#\+ . ,rest) (lp rest)]
          [(#\0 ,x)
           (guard (memv x '(#\x #\X)))
           (read-hex ip json5?)]
          [,_
           (read-unsigned chars ip json5?)])))
    (define (start-identifier rchars)
      (read-identifier (reverse rchars) ip json-buf json5? extended-identifiers? inflate-symbol))
    (cond
     [(char=? c #\-) (sign (list c))]
     [(char=? c #\0) (zero (list c))]
     [(char<=? #\1 c #\9) (start-number (list c))]
     [(not json5?) (start-identifier (list c))]
     [(char=? c #\+) (sign (list c))]
     [(char=? c #\.) (point (list c))]
     [else (start-identifier (list c))]))

  (define (no-inflate-object x) x)

  (define json:read
    (case-lambda
     [(ip) (json:read ip (json:read-options))]
     [(ip options)
      (declare-unsafe-primitives peek-char)
      (arg-check 'json:read
        [ip input-port? textual-port?]
        [options (json:read-options is?)])
      (let ([x (peek-char ip)])
        (cond
         [(eof-object? x) x]
         [else
          (match options
            [`(<json:read-options>
               ,extended-identifiers?
               ,inflate-object
               ,inflate-symbol
               ,json5?)
             (let ([inflate-object (or inflate-object no-inflate-object)])
               (R rd ([json-buf (json-buffer)]) ip #f))])]))]))

  (define (newline-and-indent indent op)
    (declare-unsafe-primitives fx+ fx= newline write-char)
    (newline op)
    (do ([i 0 (fx+ i 1)]) ((fx= i indent))
      (write-char #\space op)))

  (define (write-structural-char x indent op)
    (declare-unsafe-primitives char=? fx+ fx- write-char)
    (cond
     [(not indent)
      (write-char x op)
      #f]
     [(memv x '(#\[ #\{))
      (let ([indent (fx+ indent 2)])
        (write-char x op)
        (newline-and-indent indent op)
        indent)]
     [(memv x '(#\] #\}))
      (let ([indent (fx- indent 2)])
        (newline-and-indent indent op)
        (write-char x op)
        indent)]
     [(char=? x #\:)
      (write-char x op)
      (write-char #\space op)
      indent]
     [(char=? x #\,)
      (write-char x op)
      (newline-and-indent indent op)
      indent]
     [else
      not-reached]))

  (define (json:write-structural-char x indent op)
    (arg-check 'json:write-structural-char
      [x (lambda (x) (memv x '(#\[ #\] #\{ #\} #\: #\,)))]
      [indent valid-indent?]
      [op output-port? textual-port?])
    (write-structural-char x indent op))

  (define-syntax json-key->sort-key
    (syntax-rules ()
      [(_ expr)
       (let ([x expr])
         (if (gensym? x)
             (gensym->unique-string x)
             (symbol->string x)))]))

  (define-syntax json-key->string
    (syntax-rules ()
      [(_ expr)
       (let ([x expr])
         (if (gensym? x)
             (parameterize ([print-gensym #t]) (format "~s" x))
             (symbol->string x)))]))

  (define display-fixnum
    (let ([len (string-length (number->string (most-negative-fixnum)))])
      (define display-fixnum-buffer
        (make-weak-process-local
         (lambda () (make-string len))
         values))
      (declare-unsafe-primitives char->integer fx+ fx- fx< fx<= fx= fxabs
        fxdiv-and-mod integer->char put-string string-set! write-char)
      (define (digit->char d)
        (integer->char (fx+ d (char->integer #\0))))
      (lambda (x op)
        (cond
         [(fx<= 0 x 9) (write-char (digit->char x) op)]
         [(eq? x (most-negative-fixnum)) (fprintf op "~d" x)]
         [else
          (let ([buf (display-fixnum-buffer)])
            (let lp ([n (fxabs x)] [i (fx- len 1)])
              (let-values ([(n r) (fxdiv-and-mod n 10)])
                (string-set! buf i (digit->char r))
                (cond
                 [(fx= n 0)
                  (when (fx< x 0) (write-char #\- op))
                  (put-string op buf i (fx- len i))]
                 [else (lp n (fx- i 1))]))))]))))

  (define (valid-custom-write? x)
    (or (not x) (procedure/arity? #b10000 x)))

  (define (valid-inflate-object? x)
    (or (not x) (procedure/arity? #b10 x)))

  (define json:custom-write
    (make-process-parameter #f
      (lambda (x)
        (arg-check 'json:custom-write
          [x valid-custom-write?])
        x)))

  (define json:key<?
    (make-process-parameter #t
      (lambda (x)
        (arg-check 'json:key<?
          [x (lambda (x) (or (boolean? x) (procedure/arity? #b100 x)))])
        x)))

  (define (sort-cells! key<? v)
    (declare-unsafe-primitives car)
    (vector-sort!
     (lambda (x y)
       (key<? (json-key->sort-key (car x)) (json-key->sort-key (car y))))
     v))

  (define-syntactic-monad W op indent custom-write key<? json5?)

  (W define (finish end-char)
    (or (write-structural-char end-char indent op)
        ;; Always return a non-false value (either a fixnum indent or void).
        ;; This ensures that we recognize when `custom-write` has handled the input
        ;; if it tail-calls `wr` on a list or JSON object.
        (and custom-write (void))))

  (W define (wr x)
    (declare-unsafe-primitives display-string fl= hashtable-cells hashtable-size)
    (cond
     [(eq? x #t) (display-string "true" op)]
     [(eq? x #f) (display-string "false" op)]
     [(eq? x 'null) (display-string "null" op)]
     [(string? x) (write-string x op)]
     [(fixnum? x) (display-fixnum x op)]
     [(bignum? x) (display-string (number->string x) op)]
     [(flonum? x)
      (cond
       [(finite? x)
        (parameterize ([print-precision #f] [print-subnormal-precision #f])
          (display-string (number->string x) op))]
       [(not json5?)
        (throw `#(json:invalid-datum ,x))]
       [(fl= x +inf.0) (display-string "Infinity" op)]
       [(fl= x -inf.0) (display-string "-Infinity" op)]
       [else (display-string "NaN" op)])]
     [(and custom-write (custom-write op x indent)) (void)]
     [(null? x) (display-string "[]" op)]
     [(pair? x)
      (let ([indent (write-structural-char #\[ indent op)])
        (declare-unsafe-primitives car cdr)
        (W wr () (car x))
        (let lp ([p x])
          (let ([ls (cdr p)])
            (cond
             [(null? ls) (void)]
             [(pair? ls)
              (write-structural-char #\, indent op)
              (W wr () (car ls))
              (lp ls)]
             [else
              (throw `#(json:invalid-datum ,p))])))
        (W finish () #\]))]
     [(json:object? x)
      (if (eqv? (hashtable-size x) 0)
          (display-string "{}" op)
          (let ([indent (write-structural-char #\{ indent op)])
            (let ([v (hashtable-cells x)])
              (declare-unsafe-primitives fx+ fx= fx> vector-length vector-ref)
              (when key<? (sort-cells! key<? v))
              (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length v)))
                (when (fx> i 0)
                  (write-structural-char #\, indent op))
                (match-let* ([(,key . ,val) (vector-ref v i)])
                  (write-string (json-key->string key) op)
                  (write-structural-char #\: indent op)
                  (W wr () val))))
            (W finish () #\})))]
     [else (throw `#(json:invalid-datum ,x))]))

  (define (internal-write op x indent options default-key<?)
    (declare-unsafe-primitives newline)
    (match-define `(<json:write-options> ,custom-write ,json5?) options)
    (define custom-writer (or custom-write (json:custom-write)))
    (define key<?
      (let ([x (json:key<?)])
        (cond
         [(eq? x #t) default-key<?]
         [else x])))
    (let ([custom-write
           (and custom-writer
                (letrec ([custom-adapter (lambda (op x indent) (custom-writer op x indent wr-adapter))]
                         [wr-adapter (lambda (op x indent) (W wr ([custom-write custom-adapter]) x))])
                  custom-adapter))])
      (W wr () x)
      (when (eqv? indent 0)
        (newline op))))

  (define (valid-indent? x)
    (declare-unsafe-primitives fx>=)
    (or (not x) (and (fixnum? x) (fx>= x 0))))

  (define json:write
    (case-lambda
     [(op x) (json:write op x #f)]
     [(op x indent) (json:write op x indent (json:write-options))]
     [(op x indent options)
      (arg-check 'json:write
        [op output-port? textual-port?]
        [indent valid-indent?]
        [options (json:write-options is?)])
      (internal-write op x indent options string<?)]))

  (define json:object->string
    (case-lambda
     [(x) (json:object->string x #f)]
     [(x indent) (json:object->string x indent (json:write-options))]
     [(x indent options)
      (let-values ([(op get) (open-string-output-port)])
        (json:write op x indent options)
        (get))]))

  (define json:string->object
    (case-lambda
     [(x) (json:string->object x (json:read-options))]
     [(x options)
      (->object (open-string-input-port x) options)]))

  (define json:object->bytevector
    (case-lambda
     [(x) (json:object->bytevector x #f)]
     [(x indent) (json:object->bytevector x indent (json:write-options))]
     [(x indent options)
      (call-with-bytevector-output-port
       (lambda (op) (json:write op x indent options))
       (make-utf8-transcoder))]))

  (define json:bytevector->object
    (case-lambda
     [(x) (json:bytevector->object x (json:read-options))]
     [(x options)
      (->object (open-bytevector-input-port x (make-utf8-transcoder))
        options)]))

  (define (->object ip options)
    (let ([obj (json:read ip options)])
      (match-define `(<json:read-options> ,json5?) options)
      ;; Make sure there's nothing but whitespace left.
      (let ([x (next-non-ws ip json5?)])
        (if (eof-object? x)
            obj
            (unexpected-input #f x ip)))))

  (define (write-key indent pre key whole op)
    ;; pre is a token
    ;; key is a pre-rendered string
    ;; whole is a pre-rendered string with prefix and trailer included.
    (cond
     [indent
      (let ([indent (write-structural-char pre indent op)])
        (display-string key op)
        (write-structural-char #\: indent op))]
     [else
      (display-string whole op)
      #f]))

  (define-syntax json-write-kv
    (let ()
      (define write-string (make-write-string s op))
      (define (get-preamble prefix k)
        (let-values ([(op get) (open-string-output-port)])
          (write-char prefix op)
          (write-string (json-key->string k) op)
          (write-char #\: op)
          (get)))
      (define (get-key k)
        (let-values ([(op get) (open-string-output-port)])
          (write-string (json-key->string k) op)
          (get)))
      (define (format-value wr val)
        (and (free-identifier=? wr #'json:write)
             ;; this should be a subset of the cases before custom-write call
             ;; in (W define (wr x) ...) above.
             (or (boolean? val) (fixnum? val) (string? val)
                 (and (flonum? val) (finite? val))
                 (eq? val 'null))
             (eval `(let () (import (swish json)) (json:object->string ,val)))))
      (lambda (x)
        (syntax-case x ()
          [(_ op #f wr prefix k v #f)
           (let ([pfx (get-preamble (datum prefix) (datum k))]
                 [val (format-value #'wr (datum v))])
             ;; someday cp0 might consolidate adjacent display-string calls
             (if val
                 #`(begin (display-string #,(string-append pfx val) op) #f)
                 #`(begin (display-string #,pfx op) (wr op v #f) #f)))]
          [(_ op indent-expr wr prefix k v #f)
           (with-syntax ([whole (get-preamble (datum prefix) (datum k))]
                         [key (get-key (datum k))])
             #'(let ([indent (write-key indent-expr prefix key whole op)])
                 (wr op v indent)
                 indent))]
          [(_ op indent-expr wr prefix k v-expr wfv-expr)
           (with-syntax ([whole (get-preamble (datum prefix) (datum k))]
                         [key (get-key (datum k))])
             #'(let* ([write-field-value wfv-expr] [v v-expr]
                      [indent (write-key indent-expr prefix key whole op)])
                 (write-field-value op v indent wr)
                 indent))]))))

  (define-syntax (json:write-object x)
    (define (sort-key x)
      (syntax-case x ()
        [(key . rest) (json-key->sort-key (datum key))]))
    (define (valid? keys)
      (let ([novel (make-hashtable symbol-hash eq?)])
        (define (ok? key)
          (and (symbol? key)
               (symbol-hashtable-ref novel key #t)
               (begin (symbol-hashtable-set! novel key #f) #t)))
        (andmap ok? keys)))
    (define (parse-clause c)
      ;; The undocumented write-field-value expression must evaluate to a
      ;; procedure (lambda (op value indent wr) ...) that writes the field
      ;; value. Unlike a custom-write procedure, it cannot return #f to defer
      ;; to the default writer.
      (syntax-case c ()
        [(key field) #'(key field #f)]
        [(key field write-field-value) c]
        [_ (syntax-error c)]))
    (define (maybe-sort ls)
      (let ([key<?
             (let ([x ((eval '(let () (import (swish json)) json:key<?)))])
               (cond
                [(eq? x #t) string<?]
                [else x]))])
        (if (not key<?)
            ls
            (sort (lambda (x y) (key<? (sort-key x) (sort-key y)))
              ls))))
    (syntax-case x ()
      [(_ op-expr indent-expr wr-expr)
       #'(let ([indent indent-expr] [op op-expr])
           wr-expr
           (display-string "{}" op)
           (when (eqv? indent 0)
             (newline op))
           #t)]
      [(_ op-expr indent-expr wr-expr [key . spec] ...)
       (valid? (datum (key ...)))
       (with-syntax ([([k0 f0 wfv0] [k1 f1 wfv1] ...)
                      (maybe-sort (map parse-clause #'([key . spec] ...)))])
         (if (and (eq? (datum indent-expr) #f)
                  (identifier? #'wr-expr)
                  (free-identifier=? #'wr-expr #'json:write))
             #'(let ([op op-expr])
                 ;; see cp0 note above
                 (json-write-kv op #f json:write #\{ k0 f0 wfv0)
                 (json-write-kv op #f json:write #\, k1 f1 wfv1)
                 ...
                 (write-structural-char #\} #f op)
                 #t)
             #'(let ([op op-expr] [indent indent-expr] [wr wr-expr])
                 (let ([indent (json-write-kv op indent wr #\{ k0 f0 wfv0)])
                   (json-write-kv op indent wr #\, k1 f1 wfv1)
                   ...
                   (write-structural-char #\} indent op))
                 (when (eqv? indent 0)
                   (newline op))
                 #t)))]))

  (define json:pretty
    (case-lambda
     [(x) (json:pretty x (current-output-port))]
     [(x op/opt)
      (if ((json:write-options is?) op/opt)
          (json:pretty x (current-output-port) op/opt)
          (json:pretty x op/opt (json:write-options)))]
     [(x op options)
      (arg-check 'json:pretty
        [op output-port? textual-port?]
        [options (json:write-options is?)])
      (internal-write op x 0 options natural-string-ci<?)]))

  (define stack->json
    (case-lambda
     [(k) (stack->json k 'default)]
     [(k max-depth)
      (define who 'stack->json)
      (define (set-source! obj field x)
        (when (source-object? x)
          (let ([sfd (source-object-sfd x)])
            (json:set! obj field
              (json:make-object
               [bfp (source-object-bfp x)]
               [efp (source-object-efp x)]
               [path (source-file-descriptor-path sfd)]
               [checksum (source-file-descriptor-checksum sfd)])))))
      (define (var->json var)
        (json:make-object
         [name (format "~s" (car var))]
         [value (format "~s" (cdr var))]))
      (define obj (inspect/object k))
      (unless (eq? 'continuation (obj 'type))
        (bad-arg who k))
      (parameterize ([print-graph #t])
        (let ([stack (json:make-object [type "stack"] [depth (obj 'depth)])])
          (json:set! stack 'frames
            (walk-stack k '()
              (lambda (description source proc-source free)
                (let ([frame
                       (json:make-object
                        [type "stack-frame"]
                        [description description])])
                  (set-source! frame 'source source)
                  (set-source! frame 'procedure-source proc-source)
                  (when free (json:set! frame 'free (map var->json free)))
                  frame))
              (lambda (frame base depth next)
                (json:set! frame 'depth depth)
                (cons frame (next base)))
              who
              max-depth
              (lambda (base depth)
                (json:set! stack 'truncated depth)
                base)))
          stack))]))

  (define json-stack->string
    (let ()
      (define who 'json-stack->string)
      (define ($json-stack->string op x)
        (define (dump-src prefix)
          (lambda (src)
            (fprintf op "~@[ ~a~] at offset ~a of ~a" prefix
              (json:ref src 'bfp "?")
              (json:ref src 'path "?"))))
        (define (dump-frame x)
          (fprintf op "~a" (json:ref x 'description "?"))
          (cond
           [(json:ref x 'source #f) => (dump-src #f)]
           [(json:ref x 'procedure-source #f) => (dump-src "in procedure")])
          (newline op)
          (for-each
           (lambda (free)
             (fprintf op "  ~a: ~a\n"
               (json:ref free 'name "?")
               (json:ref free 'value "?")))
           (json:ref x 'free '())))
        (unless (and (json:object? x) (equal? "stack" (json:ref x 'type #f)))
          (bad-arg who x))
        (for-each dump-frame (json:ref x 'frames '()))
        (cond
         [(json:ref x 'truncated #f) =>
          (lambda (max-depth)
            (fprintf op "Stack dump truncated due to max-depth = ~a.\n"
              max-depth))]))
      (case-lambda
       [(op x)
        (arg-check who [op output-port? textual-port?])
        ($json-stack->string op x)]
       [(x)
        (let-values ([(op get) (open-string-output-port)])
          ($json-stack->string op x)
          (get))])))
  )
