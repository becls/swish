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

  (define session-id #f)
  (define (get-session-id) session-id)
  (define (set-session-id! n)
    (unless session-id
      (set! session-id n)))

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

  (define make-fault/no-cc
    (case-lambda
     ;; We use 'no-cc for k here to distinguish faults constructed by
     ;; make-fault or throw from those introduced by normalize-exit-reason
     ;; or by ->fault-condition in try. We use this distinction to determine
     ;; how to bind the err field in the match extension for exit-reason that
     ;; is used in the match extensions for catch, DOWN, and EXIT.
     [(reason) (make-fault-condition 'no-cc reason '())]
     [(reason inner) (make-fault-condition 'no-cc reason (list inner))]))

  (define-record-type EXIT-msg
    (nongenerative)
    (sealed #t)
    (fields
     (immutable pid)
     (immutable reason)))

  (define-record-type DOWN-msg
    (nongenerative)
    (sealed #t)
    (fields
     (immutable monitor)
     (immutable pid)
     (immutable reason)))

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
