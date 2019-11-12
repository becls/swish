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
(library (swish gatekeeper)
  (export
   gatekeeper:enter
   gatekeeper:leave
   gatekeeper:start&link
   with-gatekeeper-mutex
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish gen-server)
   )

  (define (gatekeeper:start&link) (gen-server:start&link 'gatekeeper))

  (define (gatekeeper:enter resource timeout)
    (let ([e (gen-server:call 'gatekeeper `#(enter ,resource) timeout)])
      (if (eq? e 'ok)
          'ok
          (throw e))))

  (define (gatekeeper:leave resource)
    (match (catch (gen-server:call 'gatekeeper `#(leave ,resource)))
      [ok 'ok]
      [#(EXIT ,_) 'ok]
      [,r (throw r)]))

  (define-syntax (with-gatekeeper-mutex x)
    (syntax-case x ()
      [(_ resource timeout body ...)
       (identifier? #'resource)
       #'($with-gatekeeper-mutex 'resource timeout (lambda () body ...))]))

  (define ($with-gatekeeper-mutex resource timeout body)
    (dynamic-wind
      (lambda () (gatekeeper:enter resource timeout))
      body
      (lambda () (gatekeeper:leave resource))))

  ;; state = (<mutex> ...)

  (define-tuple <mutex> resource process monitor count waiters)

  (define $cp0.orig)
  (define $np-compile.orig)
  (define pretty-print.orig)
  (define sc-expand.orig)

  (define (init)
    (with-interrupts-disabled
     (set! $cp0.orig (#%$top-level-value '$cp0))
     (set! $np-compile.orig (#%$top-level-value '$np-compile))
     (set! pretty-print.orig (#%$top-level-value 'pretty-print))
     (set! sc-expand.orig (#%$top-level-value 'sc-expand))
     (#%$set-top-level-value! '$cp0
       (lambda args
         (with-gatekeeper-mutex $cp0 60000
           (apply $cp0.orig args))))
     (#%$set-top-level-value! '$np-compile
       (lambda (original-input-expression pt?)
         (with-gatekeeper-mutex $np-compile 60000
           ($np-compile.orig original-input-expression pt?))))
     (#%$set-top-level-value! 'pretty-print
       (lambda args
         (with-gatekeeper-mutex pretty-print 60000
           (apply pretty-print.orig args))))
     (#%$set-top-level-value! 'sc-expand
       (lambda args
         (with-gatekeeper-mutex sc-expand 60000
           (apply sc-expand.orig args))))
     (current-expand sc-expand))
    (process-trap-exit #t)
    '#(ok ()))

  (define (terminate reason state)
    (with-interrupts-disabled
     (#%$set-top-level-value! '$cp0 $cp0.orig)
     (#%$set-top-level-value! '$np-compile $np-compile.orig)
     (#%$set-top-level-value! 'pretty-print pretty-print.orig)
     (#%$set-top-level-value! 'sc-expand sc-expand.orig)
     (current-expand sc-expand))
    'ok)

  (define (handle-call msg from state)
    (match-let* ([#(,process ,_) from])
      (match msg
        [#(enter ,resource)
         (let ([mutex (find-resource resource state)])
           (cond
            [(not mutex) `#(no-reply ,(enter-mutex resource from '() state))]
            [(eq? (<mutex> process mutex) process)
             `#(reply ok (,(<mutex> copy* mutex [count (+ count 1)])
                          ,@(remq mutex state)))]
            [(deadlock? process mutex state)
             (profile-me)
             `#(reply #(deadlock ,resource) ,state)]
            [else
             `#(no-reply
                (,(<mutex> copy* mutex [waiters `(,@waiters ,from)])
                 ,@(remq mutex state)))]))]
        [#(leave ,resource)
         (let ([mutex (find-resource resource state)])
           (cond
            [(not (and mutex (eq? (<mutex> process mutex) process)))
             (profile-me)
             `#(reply #(unowned-resource ,resource) ,state)]
            [else
             (let ([count (<mutex> count mutex)])
               (if (> count 1)
                   `#(reply ok
                       (,(<mutex> copy mutex [count (- count 1)])
                        ,@(remq mutex state)))
                   `#(reply ok ,(leave-mutex mutex state))))]))])))

  (define (handle-cast msg state) (match msg))

  (define (handle-info msg state)
    (match msg
      [`(DOWN ,monitor ,_ ,_)
       `#(no-reply ,(leave-mutex (find-monitor monitor state) state))]))

  (define (enter-mutex resource from waiters state)
    (gen-server:reply from 'ok)
    (match-let* ([#(,process ,_) from])
      `(,(<mutex> make
           [resource resource]
           [process process]
           [monitor (monitor process)]
           [count 1]
           [waiters waiters]) ,@state)))

  (define (leave-mutex mutex state)
    (match-let* ([`(<mutex> [resource ,resource] [monitor ,monitor]
                     [waiters ,waiters]) mutex])
      (demonitor&flush monitor)
      (if (null? waiters)
          (remq mutex state)
          (enter-mutex resource (car waiters) (cdr waiters)
            (remq mutex state)))))

  (define (deadlock? process mutex state)
    (match-let* ([`(<mutex> [process ,owner]) mutex])
      (or (eq? owner process)
          (let ([waiting
                 (find (lambda (waiting)
                         (exists (lambda (from)
                                   (match-let* ([#(,p ,_) from])
                                     (eq? p owner)))
                           (<mutex> waiters waiting)))
                   state)])
            (and waiting (deadlock? process waiting state))))))

  (define (find-resource resource state)
    (and (not (null? state))
         (match-let* ([(,(mutex <= `(<mutex> [resource ,r])) . ,rest) state])
           (if (eq? r resource)
               mutex
               (find-resource resource rest)))))

  (define (find-monitor monitor state)
    (match-let* ([(,(mutex <= `(<mutex> [monitor ,m])) . ,rest) state])
      (if (eq? m monitor)
          mutex
          (find-monitor monitor rest))))
  )
