;; Copyright 2019 Beckman Coulter, Inc.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

#!chezscheme
(library (swish digest)
  (export
   bytevector->hex-string
   close-digest
   current-digest-provider
   default-digest-provider
   digest-count
   digest-provider-name
   get-hash
   hash!
   hash->hex-string
   hex-string->hash
   make-digest-provider
   open-digest
   print-digests
   )
  (import
   (scheme)
   (swish erlang)
   (swish foreign)
   (swish io)
   (swish meta)
   )

  (define-record-type (digest-provider $make-digest-provider digest-provider?)
    (nongenerative)
    (fields
     (immutable name)
     (immutable open)
     (immutable hash!)
     (immutable get)
     (immutable close)))

  (define (make-digest-provider name open hash! get close)
    (arg-check 'make-digest-provider
      [name symbol?]
      [open procedure?]
      [hash! procedure?]
      [get procedure?]
      [close procedure?])
    ($make-digest-provider name open hash! get close))

  (define-record-type digest
    (parent digest-provider)
    (nongenerative)
    (fields
     (mutable ctx)
     (immutable algorithm)
     (immutable create-time)))

  (define-foreign osi_open_SHA1)
  (define-foreign osi_hash_data (digest uptr) (bv ptr) (start_index size_t) (size unsigned-32))
  (define-foreign osi_get_SHA1 (digest uptr))
  (define osi_close_SHA1* (foreign-procedure "osi_close_SHA1" (uptr) void))
  (define (osi_open_digest algorithm hmac-key)
    (cond
     [(not (equal? algorithm "SHA1")) 'algorithm]
     [hmac-key 'hmac-key]
     [else (osi_open_SHA1)]))

  (define default-digest-provider
    (make-digest-provider 'swish
      osi_open_digest
      osi_hash_data
      osi_get_SHA1
      osi_close_SHA1*))

  (define current-digest-provider
    (make-process-parameter
     default-digest-provider
     (lambda (x)
       (unless (digest-provider? x)
         (bad-arg 'current-digest-provider x))
       x)))

  (define digests
    (make-foreign-handle-guardian 'digests
      digest-ctx
      digest-ctx-set!
      digest-create-time
      (lambda (d) ($close-digest d))
      (lambda (op d ctx)
        (fprintf op "  ~d: ~a ~a opened ~d\n" ctx
          (digest-provider-name d)
          (digest-algorithm d)
          (digest-create-time d)))))

  (define digest-count (foreign-handle-count 'digests))
  (define print-digests (foreign-handle-print 'digests))

  (define (open-digest* who algorithm hmac-key provider)
    (let* ([digest-name
            (cond
             [(string? algorithm) algorithm]
             [(symbol? algorithm) (string-upcase (symbol->string algorithm))]
             [else (bad-arg who algorithm)])]
           [maybe-hmac
            (cond
             [(or (not hmac-key) (bytevector? hmac-key)) hmac-key]
             [(string? hmac-key) (string->utf8 hmac-key)]
             [else (bad-arg who hmac-key)])])
      (with-interrupts-disabled
       (match ((digest-provider-open provider) digest-name maybe-hmac)
         [,ctx
          (guard (integer? ctx))
          (@make-digest provider ctx digest-name)]
         [algorithm (bad-arg who algorithm)]
         [hmac-key (bad-arg who hmac-key)]
         [(,whence . ,err) (io-error who whence err)]))))

  (define (@make-digest provider ctx algorithm)
    (let ([d (make-digest
              (digest-provider-name provider)
              (digest-provider-open provider)
              (digest-provider-hash! provider)
              (digest-provider-get provider)
              (digest-provider-close provider)
              ctx
              algorithm
              (erlang:now))])
      (digests d ctx)))

  (define ($close-digest digest)
    (with-interrupts-disabled
     (let ([ctx (digest-ctx digest)])
       (when ctx
         (digests digest #f)
         ((digest-provider-close digest) ctx)
         (void)))))

  (define (close-digest digest)
    (arg-check 'close-digest [digest digest?])
    ($close-digest digest))

  (define (closed-digest-error who what digest)
    (errorf who "cannot ~a closed ~a" what
      (record-type-name (record-rtd digest))))

  (define open-digest
    (case-lambda
     [(algorithm)
      (open-digest algorithm #f)]
     [(algorithm hmac-key)
      (open-digest* 'open-digest algorithm hmac-key (current-digest-provider))]
     [(algorithm hmac-key provider)
      (unless (digest-provider? provider)
        (bad-arg 'open-digest provider))
      (open-digest* 'open-digest algorithm hmac-key provider)]))

  (define (help-hash! digest data start-index size)
    (arg-check 'hash! [digest digest?])
    (with-interrupts-disabled
     (let ([ctx (digest-ctx digest)])
       (if ctx
           ((digest-provider-hash! digest) ctx data start-index size)
           (closed-digest-error 'hash! "write to" digest))
       (void))))

  (define (fixnum>= lower-bound)
    (lambda (n)
      (and (fixnum? n) (fx>= n lower-bound))))

  (define hash!
    (case-lambda
     [(digest data)
      (arg-check 'hash! [data bytevector?])
      (help-hash! digest data 0 (bytevector-length data))]
     [(digest data start-index size)
      (arg-check 'hash!
        [data bytevector?]
        [start-index (fixnum>= 0)]
        [size (fixnum>= 1)])
      (help-hash! digest data start-index size)]))

  (define (get-hash digest)
    (arg-check 'get-hash [digest digest?])
    (with-interrupts-disabled
     (let ([ctx (digest-ctx digest)])
       (if ctx
           ((digest-provider-get digest) ctx)
           (closed-digest-error 'get-hash "read from" digest)))))

  (define (hash->hex-string bv)
    (define digits "0123456789abcdef")
    (arg-check 'hash->hex-string [bv bytevector?])
    (let* ([len (#3%bytevector-length bv)]
           [s (make-string (fx* 2 len))])
      (do ([i 0 (#3%fx+ i 1)]) ((#3%fx= i len))
        (let ([j (#3%fx* i 2)]
              [b (#3%bytevector-u8-ref bv i)])
          (let-values ([(hi lo) (values (#3%fxsrl b 4) (#3%fxlogand b #xF))])
            (#3%string-set! s j (#3%string-ref digits hi))
            (#3%string-set! s (#3%fx+ j 1) (#3%string-ref digits lo)))))
      s))

  (define-syntax char-value
    (syntax-rules (else)
      [(_ ignore [else e0 e1 ...]) (begin e0 e1 ...)]
      [(_ expr [(lo n hi) val] more ...)
       (let* ([c expr] [x (#3%char->integer c)])
         (if (#3%fx<= (char->integer lo) x (char->integer hi))
             (let ([n (#3%fx- x (char->integer lo))]) val)
             (char-value c more ...)))]))

  (define (hex-string->hash s)
    (define (bad-input) (bad-arg 'hex-string->hash s))
    (arg-check 'hex-string->hash [s string?])
    (let ([len (string-length s)])
      (unless (even? len) (bad-input))
      (let ([bv (make-bytevector (fx/ len 2))])
        (define (hex-val s i)
          (char-value (#3%string-ref s i)
            [(#\0 n #\9) n]
            [(#\a n #\f) (fx+ 10 n)]
            [(#\A n #\F) (fx+ 10 n)]
            [else (bad-input)]))
        (do ([i 0 (#3%fx+ i 2)]) ((#3%fx= i len))
          (#3%bytevector-u8-set! bv (#3%fxsrl i 1)
            (#3%fxlogor (#3%fxsll (hex-val s i) 4) (hex-val s (#3%fx+ i 1)))))
        bv)))

  (define default-block-size (expt 2 14)) ;; under .5ms on 2012 laptop
  (define bytevector->hex-string
    (case-lambda
     [(bv algorithm) (bytevector->hex-string bv algorithm default-block-size)]
     [(bv algorithm block-size)
      (arg-check 'bytevector->hex-string
        [bv bytevector?]
        [block-size (lambda (x) (and (fixnum? x) (fx> x 0)))])
      (let ([md (open-digest* 'bytevector->hex-string algorithm #f
                  (current-digest-provider))])
        (on-exit (close-digest md)
          (let ([len (bytevector-length bv)])
            (do ([i 0 (#3%fx+ i block-size)]) ((#3%fx>= i len))
              (hash! md bv i (#3%fxmin block-size (#3%fx- len i)))))
          (hash->hex-string (get-hash md))))]))

  (record-writer (record-type-descriptor digest-provider)
    (lambda (r p wr)
      (display-string "#<digest-provider" p)
      (let ([name (digest-provider-name r)])
        (when name
          (write-char #\space p)
          (wr name p)))
      (write-char #\> p)))

  (record-writer (record-type-descriptor digest)
    (lambda (r p wr)
      (fprintf p "#<~a digest>" (digest-algorithm r))))
  )
