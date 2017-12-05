;;; Copyright 2017 Beckman Coulter, Inc.
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

(library (swish string-utils)
  (export
   ends-with-ci?
   ends-with?
   format-rfc2822
   join
   split
   split-n
   starts-with-ci?
   starts-with?
   symbol-append
   )
  (import
   (except (chezscheme) define-record exit))

  (define join
    (case-lambda
     [(ls separator) (join ls separator separator)]
     [(ls separator last-separator)
      (if (null? ls)
          ""
          (let-values ([(op extractor) (open-string-output-port)])
            (display (car ls) op)
            (unless (null? (cdr ls))
              (do ([ls (cdr ls) (cdr ls)])
                  ((null? (cdr ls))
                   (display last-separator op)
                   (display (car ls) op))
                (display separator op)
                (display (car ls) op)))
            (extractor)))]))

  (define symbol-append
    (lambda ls (string->symbol (apply string-append (map symbol->string ls)))))

  (define (split-n str separator n)
    (let ([limit (string-length str)])
      (let lp ([s 0] [e 0] [n n])
        (cond
         [(or (fx= e limit) (fx= n 1))
          (cons (substring str s limit) '())]
         [(char=? (string-ref str e) separator)
          (cons (substring str s e) (lp (fx+ e 1) (fx+ e 1) (fx- n 1)))]
         [else (lp s (fx+ e 1) n)]))))

  (define (split str separator)
    (split-n str separator (+ (string-length str) 1)))

  (define (starts-with? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([i 0])
             (or (= i p-len)
                 (and (char=? (string-ref s i) (string-ref p i))
                      (lp (+ i 1))))))))

  (define (starts-with-ci? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([i 0])
             (or (= i p-len)
                 (and (char-ci=? (string-ref s i) (string-ref p i))
                      (lp (+ i 1))))))))

  (define (ends-with? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([si (- s-len 1)] [pi (- p-len 1)])
             (or (< pi 0)
                 (and (char=? (string-ref s si) (string-ref p pi))
                      (lp (- si 1) (- pi 1))))))))

  (define (ends-with-ci? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([si (- s-len 1)] [pi (- p-len 1)])
             (or (< pi 0)
                 (and (char-ci=? (string-ref s si) (string-ref p pi))
                      (lp (- si 1) (- pi 1))))))))

  (define (format-rfc2822 d)
    (format "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d ~c~2,'0d~2,'0d"
      (vector-ref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
        (date-week-day d))
      (date-day d)
      (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep"
                     "Oct" "Nov" "Dec")
        (- (date-month d) 1))
      (date-year d)
      (date-hour d)
      (date-minute d)
      (date-second d)
      (if (>= (date-zone-offset d) 0) #\+ #\-)
      (quotient (abs (date-zone-offset d)) 3600)
      (remainder (quotient (abs (date-zone-offset d)) 60) 60)))
  )
