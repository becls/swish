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

  (define-record-type (&fault-condition make-fault-condition fault-condition?)
    ;; use define-record to seal &fault-condition
    (nongenerative)
    (sealed #t)
    (parent &continuation)
    (fields
     (immutable reason fault-condition-reason)
     (immutable inner* fault-condition-inner*)))

  (define ($with-fault-condition reason inner* handler)
    (call/cc
     (lambda (k)
       (handler
        (if (fault-condition? reason)
            (make-fault-condition k (fault-condition-reason reason) (cons reason inner*))
            (make-fault-condition k reason inner*))))))

  (define throw
    (case-lambda
     [(reason) ($with-fault-condition reason '() raise)]
     [(reason inner) ($with-fault-condition reason (list inner) raise)]))

  (define make-fault
    (case-lambda
     [(reason) ($with-fault-condition reason '() values)]
     [(reason inner) ($with-fault-condition reason (list inner) values)]))

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
