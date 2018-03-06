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

(import
 (chezscheme)
 (swish erlang)
 (swish ht)
 (swish mat)
 )

(define new-value
  (let ([n -1]
        [v (vector
            (lambda (n) n)
            (lambda (n) (- -1 n))
            (lambda (n) (* (atan 1) (+ n 1)))
            (lambda (n) (* (atan -1) (+ n 1)))
            (lambda (n) (format "Value ~a" n)))])
    (lambda ()
      (set! n (+ n 1))
      ((vector-ref v (remainder n (vector-length v)))
       (quotient n (vector-length v))))))

(mat operations ()
  (define new-key
    (let ([n 0])
      (lambda ()
        (set! n (+ n 1))
        (format "key ~a" n))))

  (define (mix-case s)
    (let* ([len (string-length s)]
           [r (make-string len)])
      (let mix ([i 0])
        (cond
         [(eqv? i len) r]
         [else
          (string-set!
           r i
           (if (eqv? (random 2) 0)
               (char-upcase (string-ref s i))
               (char-downcase (string-ref s i))))
          (mix (+ i 1))]))))

  (define (make-tests n)
    (define alst '())
    (define len 0)

    (define ht (ht:make string-ci-hash string-ci=? string?))

    (define (add! key value)
      (set! alst (cons (cons key value) alst))
      (set! len (+ len 1)))

    (define (del! cell)
      (set! alst (remq! cell alst))
      (set! len (- len 1)))

    (define (count-test)
      (assert (eqv? (length (ht:keys ht)) len))
      (assert (eqv? (ht:size ht) len)))

    (define (consistency-test)
      (define default (cons 0 0))
      (let lp ([ls alst])
        (match ls
          [() #t]
          [((,key . ,val) . ,rest)
           (let ([ht-val (ht:ref ht key default)])
             (assert (equal? val ht-val))
             (lp rest))])))

    (define (get-non-existent-key-test)
      (let ([default (new-value)])
        (assert (equal? (ht:ref ht (new-key) default) default))))

    (define (get-test)
      (unless (null? alst)
        (let ([default (new-value)]
              [cell (list-ref alst (random len))])
          (assert (equal? (ht:ref ht (mix-case (car cell)) default)
                    (cdr cell))))))

    (define (add-test)
      (let ([key (new-key)]
            [value (new-value)])
        (add! key value)
        (set! ht (ht:set ht key value))
        (count-test)))

    (define (update-test)
      (unless (null? alst)
        (let ([value (new-value)]
              [cell (list-ref alst (random len))])
          (set-cdr! cell value)
          (set! ht (ht:set ht (mix-case (car cell)) value))
          (count-test))))

    (define (remove-non-existent-key-test)
      (assert (equal? ht (ht:delete ht (new-key)))))

    (define (remove-test)
      (unless (null? alst)
        (let ([cell (list-ref alst (random len))])
          (del! cell)
          (set! ht (ht:delete ht (car cell)))
          (count-test))))

    ;; Expand
    (do ([i 0 (+ i 1)]) ((= i n))
      (if (= (random 10) 0)
          (remove-test)
          (add-test)))
    (consistency-test)
    (get-test)
    (get-non-existent-key-test)

    ;; Contract
    (do ([i 0 (+ i 1)]) ((= i n))
      (if (= (random 10) 0)
          (add-test)
          (remove-test)))
    (consistency-test)
    (get-test)
    (get-non-existent-key-test)

    ;; Random
    (let ([v (vector
              add-test
              consistency-test
              count-test
              get-non-existent-key-test
              get-test
              remove-non-existent-key-test
              remove-test
              update-test)])
      (do ([i 0 (+ i 1)]) ((= i n))
        ((vector-ref v (random (vector-length v))))))
    (consistency-test))

  (do ([i 0 (+ i 1)]) ((= i 10))
    (make-tests 1000)))

(mat typed ()
  (define types '(symbol fixnum string))
  (define (make-table type)
    (match type
      [symbol (ht:make symbol-hash eq? symbol?)]
      [fixnum (ht:make equal-hash = fixnum?)]
      [string (ht:make string-hash string=? string?)]))
  (define make-key
    (let ([n 0])
      (lambda (type)
        (set! n (+ n 1))
        (match type
          [symbol (string->symbol (format "key ~a" n))]
          [fixnum n]
          [string (format "key ~a" n)]))))
  (for-each
   (lambda (type)
     (let ([ht (make-table type)])
       (for-each
        (lambda (key-type)
          (define (bad-arg-test who proc)
            (let ([key (make-key key-type)])
              (match (catch (proc ht key))
                [#(EXIT #(bad-arg ,@who ,@key)) 'ok])))
          (cond
           [(eq? type key-type)
            (let ([key (make-key key-type)]
                  [val (new-value)]
                  [default (new-value)])
              (match-let*
               ([#t (ht:is? ht)]
                [,ht (ht:set ht key val)]
                [,@val (ht:ref ht key (new-value))]
                [,@default (ht:ref ht (make-key key-type) default)]
                [,ht (ht:delete ht key)])
               'ok))]
           [else
            (bad-arg-test 'ht:set (lambda (ht key) (ht:set ht key (new-value))))
            (bad-arg-test 'ht:ref (lambda (ht key) (ht:ref ht key (new-value))))
            (bad-arg-test 'ht:delete (lambda (ht key) (ht:delete ht key)))]))
        types)))
   types))
