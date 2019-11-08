;;; Copyright 2019 Beckman Coulter, Inc.
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

(library (swish internal)
  (export $import-internal)
  (import (scheme))

  (define-record-type (&swish-condition make-swish-condition swish-condition?)
    ;; use define-record to seal &swish-condition
    (nongenerative)
    (sealed #t)
    (parent &continuation)
    (fields
     (immutable reason swish-condition-reason)
     (immutable inner* swish-condition-inner*)))

  (define throw
    (case-lambda
     [(reason) ($throw reason '())]
     [(reason inner) ($throw reason (list inner))]))

  (define ($throw reason inner*)
    (call/cc
     (lambda (k)
       (raise
        (if (swish-condition? reason)
            (make-swish-condition k (swish-condition-reason reason) (cons reason inner*))
            (make-swish-condition k reason inner*))))))

  (define-syntax $import-internal
    (let ([allowed? #t])
      (lambda (x)
        (syntax-case x ()
          [(_)
           (begin
             (set! allowed? #f)
             #'(void))]
          [(_ id ...)
           (and allowed? (andmap identifier? #'(id ...)))
           (with-syntax ([(rhs ...) (datum->syntax #'$import-internal (datum (id ...)))])
             #'(begin (alias id rhs) ...))]))))
  )
