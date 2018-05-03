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

(library (swish json)
  (export
   json:extend-object
   json:make-object
   json:object->bytevector
   json:object->string
   json:read
   json:string->object
   json:write
   json:write-object
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish io)
   )

  (define-syntax json:extend-object
    (syntax-rules ()
      [(_ $ht (key val) ...)
       (let ([ht $ht])
         (hashtable-set! ht key val)
         ...
         ht)]))

  (define-syntax json:make-object
    (syntax-rules ()
      [(_ (key val) ...)
       (json:extend-object (make-hashtable string-hash string=?)
         (key val) ...)]))

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

  (define (read-string ip)
    (let lp ([op (open-output-string)])
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
           (lp op)]
          [else (write-char c op) (lp op)]))))

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

  (define json:read
    (case-lambda
     [(ip) (json:read ip no-custom-inflate)]
     [(ip custom-inflate)
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
           [(eqv? c #\") (read-string ip)]
           [(eqv? c #\[)
            (let lp ([acc '()])
              (let ([c (next-non-ws ip)])
                (case c
                  [(#\]) '()]
                  [else
                   (unread-char c ip)
                   (let ([acc (cons (rd ip) acc)])
                     (let ([c (next-non-ws ip)])
                       (case c
                         [(#\,) (lp acc)]
                         [(#\]) (reverse acc)]
                         [else (unexpected-input c ip)])))])))]
           [(eqv? c #\{)
            (custom-inflate
              (let lp ([obj (json:make-object)])
                (let ([c (next-non-ws ip)])
                  (case c
                    [(#\")
                     (let ([key (read-string ip)])
                       (let ([c (next-non-ws ip)])
                         (unless (eqv? c #\:)
                           (unexpected-input c ip)))
                       (hashtable-set! obj key (rd ip)))
                     (let ([c (next-non-ws ip)])
                       (case c
                         [(#\,) (lp obj)]
                         [(#\}) obj]
                         [else (unexpected-input c ip)]))]
                    [(#\}) obj]
                    [else (unexpected-input c ip)]))))]
           [(eqv? c #\-) (- (read-unsigned ip))]
           [else (unread-char c ip) (read-unsigned ip)])))
      (rd ip)]))

  (define json:write
    (case-lambda
     [(op x) (json:write op x no-custom-write)]
     [(op x custom-write)
      (define (wr op x)
        (cond
         [(eq? x #t) (display-string "true" op)]
         [(eq? x #f) (display-string "false" op)]
         [(eqv? x #\nul) (display-string "null" op)]
         [(string? x) (write-string x op)]
         [(or (fixnum? x) (bignum? x) (and (flonum? x) (finite? x)))
          (display-string (number->string x) op)]
         [(custom-write op x wr)]
         [(null? x) (display-string "[]" op)]
         [(pair? x)
          (write-char #\[ op)
          (wr op (car x))
          (for-each
           (lambda (x)
             (write-char #\, op)
             (wr op x))
           (cdr x))
          (write-char #\] op)]
         [(hashtable? x)
          (write-char #\{ op)
          (let-values ([(keys vals) (hashtable-entries x)])
            (let ([v (vector-map cons keys vals)])
              (vector-sort! (lambda (x y) (string<? (car x) (car y))) v)
              (do ([i 0 (fx+ i 1)]) ((fx= i (vector-length v)))
                (when (fx> i 0)
                  (write-char #\, op))
                (let ([p (vector-ref v i)])
                  (write-string (car p) op)
                  (write-char #\: op)
                  (wr op (cdr p))))))
          (write-char #\} op)]
         [else (raise `#(invalid-datum ,x))]))
      (wr op x)]))

  (define json:object->string
    (case-lambda
     [(x) (json:object->string x no-custom-write)]
     [(x custom-write)
      (let-values ([(op get) (open-string-output-port)])
        (json:write op x custom-write)
        (get))]))

  (define json:string->object
    (case-lambda
     [(x) (json:string->object x no-custom-inflate)]
     [(x custom-inflate)
      (let* ([ip (open-string-input-port x)]
             [obj (json:read ip custom-inflate)])
        ;; Make sure there's nothing but whitespace left.
        (let lp ()
          (let ([x (read-char ip)])
            (cond
             [(eof-object? x) obj]
             [(ws? x) (lp)]
             [else (unexpected-input x ip)]))))]))

  (define json:object->bytevector
    (case-lambda
     [(x) (json:object->bytevector x no-custom-write)]
     [(x custom-write)
      (call-with-bytevector-output-port
       (lambda (op) (json:write op x custom-write))
       (make-utf8-transcoder))]))

  (define (no-custom-inflate x) x)

  (define (no-custom-write op x wr) #f)

  (define-syntax json-write-kv
    (let ()
      (define write-string (make-write-string s op))
      (define (get-preamble prefix k)
        (let-values ([(op get) (open-string-output-port)])
          (write-char prefix op)
          (write-string k op)
          (write-char #\: op)
          (get)))
      (lambda (x)
        (syntax-case x ()
          [(_ op wr prefix k v #f)
           (with-syntax ([str (get-preamble (datum prefix) (datum k))])
             #'(begin (display str op) (wr op v)))]
          [(_ op wr prefix k v-expr cw-expr)
           (with-syntax ([str (get-preamble (datum prefix) (datum k))])
             #`(let ([custom-writer cw-expr] [v v-expr])
                 (display str op)
                 (if custom-writer
                     (custom-writer op v wr)
                     (wr op v))))]))))

  (define-syntax (json:write-object x)
    (define (get-key x)
      (syntax-case x ()
        [(key . rest) (datum key)]))
    (define (valid? keys)
      (let ([novel (make-hashtable string-hash string=?)])
        (define (ok? key)
          (and (string? key)
               (hashtable-ref novel key #t)
               (begin (hashtable-set! novel key #f) #t)))
        (andmap ok? keys)))
    (define (parse-clause c)
      (syntax-case c ()
        [(key field) #'(key field #f)]
        [(key field custom-write) c]
        [_ (syntax-error c)]))
    (syntax-case x ()
      [(_ op-expr wr-expr [key . spec] ...)
       (valid? (datum (key ...)))
       (with-syntax ([([k0 f0 cw0] [k1 f1 cw1] ...)
                      (sort
                       (lambda (x y)
                         (string<? (get-key x) (get-key y)))
                       (map parse-clause #'([key . spec] ...)))])
         #'(let ([op op-expr] [wr wr-expr])
             (json-write-kv op wr #\{ k0 f0 cw0)
             (json-write-kv op wr #\, k1 f1 cw1)
             ...
             (write-char #\} op)))]))

  )
