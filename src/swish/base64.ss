#!chezscheme
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
(library (swish base64)
  (export
   base64-decode-bytevector
   base64-encode-bytevector
   base64url-decode-bytevector
   base64url-encode-bytevector
   )
  (import
   (scheme)
   (swish erlang)
   (swish io))

  ;; see RFC 4648 https://tools.ietf.org/html/rfc4648

  (define pad (char->integer #\=))
  (define (pad? b) (fx= b pad))

  (define (base64-encoded-size in-size)
    (let-values ([(d m) (fxdiv-and-mod in-size 3)])
      (fx* 4 (if (fx= m 0) d (fx+ d 1)))))

  (define (base64-decoded-size in-size)
    (fx* 3 (fx/ in-size 4)))

  (define-syntax define-encoding
    (let ()
      (define (make-encode ranges)
        (let-values ([(op get-bv) (open-bytevector-output-port)])
          (define add-range
            (lambda (s)
              (match (map char->integer (string->list s))
                [(,i) (put-u8 op i)]
                [(,start ,end)
                 (do ([i start (+ i 1)]) ((> i end))
                   (put-u8 op i))])))
          (for-each add-range ranges)
          (get-bv)))
      (define (encode->decode bv)
        (let ([end (bytevector-length bv)]
              [out (make-bytevector 256 -1)])
          (do ([i 0 (fx1+ i)]) ((fx= i end))
            (bytevector-u8-set! out (bytevector-u8-ref bv i) i))
          out))
      (lambda (x)
        (syntax-case x ()
          [(_ encode decode str ...)
           (andmap string? (datum (str ...)))
           (let ([encode-bv (make-encode (datum (str ...)))])
             #`(begin
                 (define encode '#,encode-bv)
                 (define decode '#,(encode->decode encode-bv))))]))))

  (define-encoding base64-encoding base64-decoding "AZ" "az" "09" "+" "/")
  (define-encoding base64url-encoding base64url-decoding "AZ" "az" "09" "-" "_")

  ;; dump encoding tables in the orientation used in RFC 4648
  #;
  (define (dump x)
    (do ([r 0 (+ r 1)]) ((> r 16))
      (do ([c 0 (+ c 17)]) ((> c 51))
        (let ([i (+ r c)])
          (when (< i (bytevector-length x))
            (printf "~v@a ~a" (if (= c 0) 10 14) i (integer->char (bytevector-u8-ref x i))))))
      (newline)))

  (define (base64-decode-bytevector bv)
    (arg-check 'base64-decode-bytevector [bv bytevector?])
    (base64-decode bv base64-decoding 'base64-decode-bytevector))

  (define (base64url-decode-bytevector bv)
    (arg-check 'base64url-decode-bytevector [bv bytevector?])
    (base64-decode bv base64url-decoding 'base64url-decode-bytevector))

  ;; RFC 4648 recommends rejecting bad inputs
  (define (base64-decode bv decoding who)
    (define input-size (#3%bytevector-length bv))
    (unless (fxzero? (fxremainder input-size 4))
      (bad-arg who bv))
    (if (fx= input-size 0)
        '#vu8()
        (let ([end (fx- input-size 4)]
              [out (make-bytevector (base64-decoded-size input-size))])
          (define (get i offset) (#3%bytevector-s8-ref bv (fx+ i offset)))
          (define (check bits) (if (fx>= bits 0) bits (bad-arg who bv)))
          (define (shift bits offset) (#3%fxsll bits (- 24 (* 6 (+ offset 1)))))
          (define (extract-bytes i) (values (get i 0) (get i 1) (get i 2) (get i 3)))
          (define (->bits byte) (#3%bytevector-s8-ref decoding byte))
          (define (pad-or-bits byte) (if (fx= byte pad) 0 (->bits byte)))
          (define (check-get-bits byte) (check (->bits byte)))
          (define (combine check-get-bits a b c d)
            (#3%fxlogor
             (shift (check (->bits a)) 0)
             (shift (check-get-bits b) 1)
             (shift (check-get-bits c) 2)
             (shift (check-get-bits d) 3)))
          (define (finish i j)
            (let-values ([(a b c d) (extract-bytes i)])
              (#3%bytevector-u24-set! out j (combine pad-or-bits a b c d) 'big)
              (cond
               [(pad? b) (bad-arg who bv)] ;; already checked a
               [(pad? c)
                (unless (pad? d) (bad-arg who bv))
                (#3%bytevector-truncate! out (fx+ j 1))]
               [(pad? d)
                (#3%bytevector-truncate! out (fx+ j 2))])))
          (do ([i 0 (fx+ i 4)] [j 0 (fx+ j 3)]) ((fx= i end) (finish i j))
            (let-values ([(a b c d) (extract-bytes i)])
              (#3%bytevector-u24-set! out j (combine check-get-bits a b c d) 'big)))
          out)))

  (define (base64-encode-bytevector bv)
    (arg-check 'base64-encode-bytevector [bv bytevector?])
    (base64-encode bv base64-encoding))

  (define (base64url-encode-bytevector bv)
    (arg-check 'base64url-encode-bytevector [bv bytevector?])
    (base64-encode bv base64url-encoding))

  (define (base64-encode bv encoding)
    (define input-size (#3%bytevector-length bv))
    (if (fx= input-size 0)
        '#vu8()
        (let ([out (make-bytevector (base64-encoded-size input-size))])
          (define (write-encoded base in)
            (let ([bits (#3%bytevector-u24-ref bv in 'big)])
              (meta-cond
               [(< (integer-length (most-positive-fixnum)) 32)
                (#3%bytevector-u16-set! out base
                  (#3%fxlogor
                   (#3%fxsll (translate bits 3/4) 8)
                   (#3%fxsll (translate bits 2/4) 0))
                  'big)
                (#3%bytevector-u16-set! out (fx+ base 2)
                  (#3%fxlogor
                   (#3%fxsll (translate bits 1/4) 8)
                   (#3%fxsll (translate bits 0/4) 0))
                  'big)]
               [else
                (#3%bytevector-u32-set! out base
                  (#3%fxlogor
                   (#3%fxsll (translate bits 3/4) 24)
                   (#3%fxsll (translate bits 2/4) 16)
                   (#3%fxsll (translate bits 1/4) 8)
                   (#3%fxsll (translate bits 0/4) 0))
                  'big)])))
          (define (write-padded base bits n)
            (set base 0 bits 3/4)
            (set base 1 bits 2/4)
            (if (= n 1)
                (set base 2 bits 1/4)
                (#3%bytevector-u8-set! out (#3%fx+ base 2) pad))
            (#3%bytevector-u8-set! out (#3%fx+ base 3) pad))
          (define (set base offset bits shift)
            (#3%bytevector-u8-set! out (#3%fx+ base offset)
              (translate bits shift)))
          (define (translate bits shift)
            (#3%bytevector-u8-ref encoding
              (#3%fxlogand (#3%fxsra bits (* 24 shift)) #x3F)))
          (define (get-bits i)
            (#3%fxlogor
             (get i 16)
             (get (#3%fx+ i 1) 8)
             (get (#3%fx+ i 2) 0)))
          (define (get i shift)
            (if (#3%fx>= i input-size)
                0
                (#3%fxsll (#3%bytevector-u8-ref bv i) shift)))
          (let loop ([in 0] [base 0])
            (let ([next (#3%fx+ in 3)])
              (cond
               [(#3%fx< next input-size)
                (write-encoded base in)
                (loop next (#3%fx+ base 4))]
               [(#3%fx= next input-size)
                (write-encoded base in)]
               [else
                (write-padded base (get-bits in) (- next input-size))])))
          out))))
