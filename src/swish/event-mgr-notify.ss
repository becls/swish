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
(library (swish event-mgr-notify)
  (export
   event-mgr:notify
   informative-exit-reason?
   normalize-exit-reason
   system-detail
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish internal)
   (swish meta)
   )

  ($import-internal make-fault-condition &fault-condition)

  (define (abnormal? reason)
    (not (memq reason '(normal shutdown))))

  (define normalize-exit-reason
    (case-lambda
     [(x)
      (match x
        [`(catch ,r ,e) (normalize-exit-reason r e)]
        [,_ (normalize-exit-reason x x)])]
     [(reason err)
      (let ([err (or err reason)])
        (if (condition? reason)
            (values 'exception (if (informative-exit-reason? err) err reason))
            (values reason
              (and (informative-exit-reason? err)
                   ;; If called via (match (catch e) [`(catch ,reason ,err) ...]),
                   ;; make fault so coerce records exit-reason->english message.
                   (if (eq? err reason)
                       (make-fault-condition #f reason '())
                       err)))))]))

  (define (informative-exit-reason? reason)
    (and
     (match reason
       [`(&fault-condition ,reason ,k)
        (or (#%$continuation? k) (abnormal? reason))]
       [,_ (abnormal? reason)])
     #t))

  (define (event-mgr:notify event)
    (cond
     [(whereis 'event-mgr) => (lambda (pid) (send pid `#(notify ,event)))]
     [else (console-event-handler event)])
    'ok)

  (define-syntax (system-detail x)
    (syntax-case x ()
      [(_ name [field value] ...)
       #`(event-mgr:notify
          (name make
            #,@(add-if-absent #'timestamp #'(erlang:now)
                 #'([field value] ...))))]))
  )
