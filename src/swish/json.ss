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
   json:bytevector->object
   json:cells
   json:delete!
   json:extend-object
   json:make-object
   json:object->bytevector
   json:object->string
   json:object?
   json:read
   json:ref
   json:set!
   json:size
   json:string->object
   json:update!
   json:write
   json:write-object
   json:write-structural-char
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish io)
   )

  (define-syntax extend-object-internal
    (syntax-rules ()
      [(_ x $ht (key val) ...)
       (let ([ht $ht])
         (hashtable-set! ht (parse-key key x) val)
         ...
         ht)]))

  (define-syntax (parse-key x)
    (syntax-case x (unquote)
      [(_ id form) (identifier? #'id) #'(quote id)]
      [(_ (unquote e) form) #'e]
      [(_ key form) (syntax-error #'form (format "invalid key ~s in" (datum key)))]))

  (define-syntax (json:extend-object x)
    (syntax-case x ()
      [(_ $ht (key val) ...)
       #`(extend-object-internal #,x $ht (key val) ...)]))

  (define-syntax (json:make-object x)
    (syntax-case x ()
      [(_ (key val) ...)
       #`(extend-object-internal #,x (make-hashtable symbol-hash json:key=?)
           (key val) ...)]))

  (define (json:key=? x y) (eq? x y))

  (define (json:object? x)
    (and (hashtable? x)
         (eq? (#3%hashtable-equivalence-function x) json:key=?)))

  (define (json:cells x)
    (unless (json:object? x) (bad-arg 'json:cells x))
    (#3%hashtable-cells x))

  (define (walk-path who obj full-path extend? default found)
    (unless (json:object? obj) (bad-arg who obj))
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
             (let ([hit (#3%symbol-hashtable-ref obj key1 #f)])
               (cond
                [(json:object? hit) (lp hit more)]
                [extend?
                 (let ([new (json:make-object)])
                   (#3%symbol-hashtable-set! obj key1 new)
                   (lp new more))]
                [else default]))]
            [,_ (bad-arg who full-path)]))))

  (define (json:ref obj path default)
    (walk-path 'json:ref obj path #f default
      (lambda (obj sym default)
        (#3%symbol-hashtable-ref obj sym default))))

  (define (json:set! obj path value)
    (walk-path 'json:set! obj path #t value
      (lambda (obj key value)
        (#3%symbol-hashtable-set! obj key value))))

  (define (json:update! obj path f default)
    (unless (procedure? f) (bad-arg 'json:update! f))
    (walk-path 'json:update! obj path #t default
      (lambda (obj key default)
        (#3%symbol-hashtable-update! obj key f default))))

  (define (json:delete! obj path)
    (walk-path 'json:delete! obj path #f (void)
      (lambda (obj key default)
        (#3%symbol-hashtable-delete! obj key))))

  (define (json:size obj)
    (unless (json:object? obj) (bad-arg 'json:size obj))
    (#3%hashtable-size obj))

  (define (unexpected-input c ip)
    (if (eof-object? c)
        (raise 'unexpected-eof)
        (raise `#(unexpected-input ,c
                   ,(and (port-has-port-position? ip)
                         (- (port-position ip) 1))))))

  (define (next-char ip)
    (let ([x (read-char ip)])
      (if (eof-object? x)
          (raise 'unexpected-eof)
          x)))

  (define (ws? x)
    (memv x '(#\x20 #\x09 #\x0A #\x0D)))

  (define (next-non-ws ip)
    (let ([c (next-char ip)])
      (if (ws? c)
          (next-non-ws ip)
          c)))

  (define (seek-non-ws ip)
    (let ([c (read-char ip)])
      (cond
       [(eof-object? c) c]
       [(ws? c) (seek-non-ws ip)]
       [else c])))

  (define (read-string ip op)
    (let ([c (next-char ip)])
      (case c
        [(#\") (get-output-string op)]
        [(#\\)
         (let ([c (next-char ip)])
           (case c
             [(#\" #\\ #\/) (write-char c op)]
             [(#\b) (write-char #\x08 op)]
             [(#\f) (write-char #\x0C op)]
             [(#\n) (write-char #\x0A op)]
             [(#\r) (write-char #\x0D op)]
             [(#\t) (write-char #\x09 op)]
             [(#\u)
              (let ([x (read-4hexdig ip)])
                (cond
                 [(<= #xD800 x #xDBFF) ;; high surrogate
                  (expect-char #\\ ip)
                  (expect-char #\u ip)
                  (let ([y (read-4hexdig ip)])
                    (unless (<= #xDC00 y #xDFFF)
                      (raise 'invalid-surrogate-pair))
                    (write-char
                     (integer->char
                      (+ (ash (bitwise-and x #x3FF) 10)
                         (bitwise-and y #x3FF)
                         #x10000))
                     op))]
                 [(<= #xDC00 x #xDFFF) (raise 'invalid-surrogate-pair)]
                 [else (write-char (integer->char x) op)]))]
             [else (unexpected-input c ip)]))
         (read-string ip op)]
        [else (write-char c op) (read-string ip op)])))

  (define (read-4hexdig ip)
    (let* ([x (hex-digit ip)]
           [x (+ (ash x 4) (hex-digit ip))]
           [x (+ (ash x 4) (hex-digit ip))]
           [x (+ (ash x 4) (hex-digit ip))])
      x))

  (define (expect-char expected ip)
    (let ([c (next-char ip)])
      (unless (char=? c expected)
        (unexpected-input c ip))))

  (define-syntax make-write-string
    (syntax-rules ()
      [(_ s op)
       (lambda (s op)
         (write-char #\" op)
         (do ([i 0 (+ i 1)] [n (string-length s)]) [(= i n)]
           (let ([c (string-ref s i)])
             (cond
              [(memv c '(#\" #\\)) (write-char #\\ op) (write-char c op)]
              [(char<=? c #\x1F) (fprintf op "\\u~4,'0x" (char->integer c))]
              [else (write-char c op)])))
         (write-char #\" op))]))

  (define write-string (make-write-string s op))

  (define (hex-digit ip)
    (let ([c (next-char ip)])
      (cond
       [(char<=? #\0 c #\9) (digit-value c)]
       [(char<=? #\A c #\F) (- (char->integer c) (- (char->integer #\A) 10))]
       [(char<=? #\a c #\f) (- (char->integer c) (- (char->integer #\a) 10))]
       [else (unexpected-input c ip)])))

  (define (digit-value c) (- (char->integer c) (char->integer #\0)))

  (define (read-unsigned ip)
    (let-values ([(mantissa n c) (read-digits ip 0 0)])
      (cond
       [(eqv? n 0) (unexpected-input c ip)]
       [(eqv? c #\.)
        (let-values ([(mantissa m c) (read-digits ip mantissa 0)])
          (cond
           [(eqv? m 0) (unexpected-input c ip)]
           [(memv c '(#\e #\E)) (read-exp ip mantissa m)]
           [else
            (unless (eof-object? c)
              (unread-char c ip))
            (scale mantissa (- m))]))]
       [(memv c '(#\e #\E)) (read-exp ip mantissa 0)]
       [else
        (unless (eof-object? c)
          (unread-char c ip))
        mantissa])))

  (define (read-digits ip mantissa n)
    (let ([c (read-char ip)])
      (cond
       [(eof-object? c) (values mantissa n c)]
       [(char<=? #\0 c #\9)
        (read-digits ip (+ (* mantissa 10) (digit-value c)) (+ n 1))]
       [else
        (values mantissa n c)])))

  (define (read-exp ip mantissa m)
    (let ([c (next-char ip)])
      (case c
        [(#\+) (scale mantissa (- (read-int ip) m))]
        [(#\-) (scale mantissa (- (- (read-int ip)) m))]
        [else (unread-char c ip) (scale mantissa (- (read-int ip) m))])))

  (define (read-int ip)
    (let-values ([(int n c) (read-digits ip 0 0)])
      (cond
       [(eqv? n 0) (unexpected-input c ip)]
       [else
        (unless (eof-object? c)
          (unread-char c ip))
        int])))

  (define (scale mantissa exponent)
    (if (>= exponent 0)
        (inexact (* mantissa (expt 10 exponent)))
        (inexact (/ mantissa (expt 10 (- exponent))))))

  (define (string->key s)
    (let ([len (string-length s)])
      (or (and (fx>= len 6)
               (char=? (string-ref s 0) #\#)
               (char=? (string-ref s 1) #\{)
               (char=? (string-ref s (fx- len 1)) #\})
               (guard (c [else #f])
                 (read (open-input-string s))))
          (string->symbol s))))

  (define json:read
    (case-lambda
     [(ip) (json:read ip no-custom-inflate)]
     [(ip custom-inflate)
      (define buffer (open-output-string))
      (define (rd ip)
        (let ([c (next-non-ws ip)])
          (cond
           [(eqv? c #\t)
            (expect-char #\r ip)
            (expect-char #\u ip)
            (expect-char #\e ip)
            #t]
           [(eqv? c #\f)
            (expect-char #\a ip)
            (expect-char #\l ip)
            (expect-char #\s ip)
            (expect-char #\e ip)
            #f]
           [(eqv? c #\n)
            (expect-char #\u ip)
            (expect-char #\l ip)
            (expect-char #\l ip)
            #\nul]
           [(eqv? c #\") (read-string ip buffer)]
           [(eqv? c #\[)
            (let lp ([acc '()])
              (let ([c (next-non-ws ip)])
                (cond
                 [(and (eqv? c #\]) (null? acc)) '()]
                 [else
                  (unread-char c ip)
                  (let* ([acc (cons (rd ip) acc)]
                         [c (next-non-ws ip)])
                    (case c
                      [(#\,) (lp acc)]
                      [(#\]) (reverse acc)]
                      [else (unexpected-input c ip)]))])))]
           [(eqv? c #\{)
            (custom-inflate
             (let lp ([obj (json:make-object)])
               (let ([c (next-non-ws ip)])
                 (cond
                  [(eqv? c #\")
                   (let* ([key (string->key (read-string ip buffer))]
                          [c (next-non-ws ip)])
                     (unless (eqv? c #\:)
                       (unexpected-input c ip))
                     (#3%symbol-hashtable-set! obj key (rd ip)))
                   (let ([c (next-non-ws ip)])
                     (case c
                       [(#\,) (lp obj)]
                       [(#\}) obj]
                       [else (unexpected-input c ip)]))]
                  [(and (eqv? c #\}) (eqv? (#3%hashtable-size obj) 0)) obj]
                  [else (unexpected-input c ip)]))))]
           [(eqv? c #\-) (- (read-unsigned ip))]
           [else (unread-char c ip) (read-unsigned ip)])))
      (let ([x (seek-non-ws ip)])
        (cond
         [(eof-object? x) x]
         [else
          (unread-char x ip)
          (rd ip)]))]))

  (define (newline-and-indent indent op)
    (newline op)
    (do ([i 0 (+ i 1)]) ((= i indent))
      (write-char #\space op)))

  (define (json:write-structural-char x indent op)
    (if (not indent)
        (begin
          (write-char x op)
          #f)
        (match x
          [#\[
           (let ([indent (+ indent 2)])
             (write-char #\[ op)
             (newline-and-indent indent op)
             indent)]
          [#\]
           (let ([indent (- indent 2)])
             (newline-and-indent indent op)
             (write-char #\] op)
             indent)]
          [#\{
           (let ([indent (+ indent 2)])
             (write-char #\{ op)
             (newline-and-indent indent op)
             indent)]
          [#\}
           (let ([indent (- indent 2)])
             (newline-and-indent indent op)
             (write-char #\} op)
             indent)]
          [#\:
           (write-char #\: op)
           (write-char #\space op)
           indent]
          [#\,
           (write-char #\, op)
           (newline-and-indent indent op)
           indent])))

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

  (define json:write
    (case-lambda
     [(op x) (json:write op x #f)]
     [(op x indent) (json:write op x indent no-custom-write)]
     [(op x indent custom-write)
      (define (wr op x indent)
        (cond
         [(eq? x #t) (display-string "true" op)]
         [(eq? x #f) (display-string "false" op)]
         [(eqv? x #\nul) (display-string "null" op)]
         [(string? x) (write-string x op)]
         [(or (fixnum? x) (bignum? x) (and (flonum? x) (finite? x)))
          (display-string (number->string x) op)]
         [(custom-write op x indent wr)]
         [(null? x) (display-string "[]" op)]
         [(pair? x)
          (let ([indent (json:write-structural-char #\[ indent op)])
            (wr op (car x) indent)
            (for-each
             (lambda (x)
               (json:write-structural-char #\, indent op)
               (wr op x indent))
             (cdr x))
            (json:write-structural-char #\] indent op))]
         [(json:object? x)
          (if (zero? (#3%hashtable-size x))
              (display-string "{}" op)
              (let ([indent (json:write-structural-char #\{ indent op)])
                (let ([v (#3%hashtable-cells x)])
                  (vector-sort!
                   (lambda (x y)
                     (string<? (json-key->sort-key (car x)) (json-key->sort-key (car y))))
                   v)
                  (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length v)))
                    (when (fx> i 0)
                      (json:write-structural-char #\, indent op))
                    (let ([p (vector-ref v i)])
                      (write-string (json-key->string (car p)) op)
                      (json:write-structural-char #\: indent op)
                      (wr op (cdr p) indent))))
                (json:write-structural-char #\} indent op)))]
         [else (raise `#(invalid-datum ,x))]))
      (when (and indent (or (not (fixnum? indent)) (negative? indent)))
        (bad-arg 'json:write indent))
      (wr op x indent)
      (when (eqv? indent 0)
        (newline op))]))

  (define json:object->string
    (case-lambda
     [(x) (json:object->string x #f)]
     [(x indent) (json:object->string x indent no-custom-write)]
     [(x indent custom-write)
      (let-values ([(op get) (open-string-output-port)])
        (json:write op x indent custom-write)
        (get))]))

  (define json:string->object
    (case-lambda
     [(x) (json:string->object x no-custom-inflate)]
     [(x custom-inflate)
      (->object (open-string-input-port x) custom-inflate)]))

  (define json:object->bytevector
    (case-lambda
     [(x) (json:object->bytevector x #f)]
     [(x indent) (json:object->bytevector x indent no-custom-write)]
     [(x indent custom-write)
      (call-with-bytevector-output-port
       (lambda (op) (json:write op x indent custom-write))
       (make-utf8-transcoder))]))

  (define json:bytevector->object
    (case-lambda
     [(x) (json:bytevector->object x no-custom-inflate)]
     [(x custom-inflate)
      (->object (open-bytevector-input-port x (make-utf8-transcoder))
        custom-inflate)]))

  (define (->object ip custom-inflate)
    (let ([obj (json:read ip custom-inflate)])
      ;; Make sure there's nothing but whitespace left.
      (let ([x (seek-non-ws ip)])
        (if (eof-object? x)
            obj
            (unexpected-input x ip)))))

  (define (no-custom-inflate x) x)

  (define (no-custom-write op x indent wr) #f)

  (define (write-key indent pre key whole op)
    ;; pre is a token
    ;; key is a pre-rendered string
    ;; whole is a pre-rendered string with prefix and trailer included.
    (cond
     [indent
      (let ([indent (json:write-structural-char pre indent op)])
        (display key op)
        (json:write-structural-char #\: indent op))]
     [else
      (display whole op)
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
      (lambda (x)
        (syntax-case x ()
          [(_ op indent wr prefix k v #f)
           (with-syntax ([whole (get-preamble (datum prefix) (datum k))]
                         [key (get-key (datum k))])
             #'(let ([indent (write-key indent prefix key whole op)])
                 (wr op v indent)
                 indent))]
          [(_ op indent wr prefix k v-expr cw-expr)
           (with-syntax ([whole (get-preamble (datum prefix) (datum k))]
                         [key (get-key (datum k))])
             #'(let* ([custom-writer cw-expr] [v v-expr]
                      [indent (write-key indent prefix key whole op)])
                 (if custom-writer
                     (custom-writer op v indent wr)
                     (wr op v indent))
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
      (syntax-case c ()
        [(key field) #'(key field #f)]
        [(key field custom-write) c]
        [_ (syntax-error c)]))
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
       (with-syntax ([([k0 f0 cw0] [k1 f1 cw1] ...)
                      (sort
                       (lambda (x y)
                         (string<? (sort-key x) (sort-key y)))
                       (map parse-clause #'([key . spec] ...)))])
         #'(let ([op op-expr] [indent indent-expr] [wr wr-expr])
             (let ([indent (json-write-kv op indent wr #\{ k0 f0 cw0)])
               (json-write-kv op indent wr #\, k1 f1 cw1)
               ...
               (json:write-structural-char #\} indent op))
             (when (eqv? indent 0)
               (newline op))
             #t))]))
  )
