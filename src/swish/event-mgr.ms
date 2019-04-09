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
 (swish event-mgr)
 (swish mat)
 (swish string-utils)
 (swish testing)
 )

(define (reported event)
  (receive
   (after 1000 (raise `#(event-timeout ,event)))
   [,@event 'ok]))

(define (silence-log-handler)
  (event-mgr:set-log-handler (lambda (x) x) (spawn (lambda () (receive)))))

(define (stop-event-mgr)
  (cond
   [(whereis 'event-mgr) => (lambda (pid) (kill pid 'kill))]
   [else 'ok]))

(isolate-mat basic ()
  (stop-event-mgr)
  (start-event-mgr)
  (silence-log-handler)
  (event-mgr:flush-buffer)
  (capture-events)
  (let ([e1 `(#(tag1 "data1") #(tag2 data2) #(tag3 3))]
        [e2 `(testing "testing" #(tag1 "tag1"))]
        [e3 "This is a string !"]
        [e4 `#(this is a tuple)])
    (event-mgr:notify e1)
    (reported e1)
    (event-mgr:notify e2)
    (reported e2)
    (event-mgr:notify e3)
    (reported e3)
    (event-mgr:notify e4)
    (reported e4)
    'ok))

(isolate-mat buffering ()
  (stop-event-mgr)
  (start-event-mgr)
  (silence-log-handler)
  (capture-events)
  (let ([e1 `(#(tag1 "data1") #(tag2 data2) #(tag3 3))]
        [e2 `(testing "testing" #(tag1 "tag1"))]
        [e3 "This is a string !"]
        [e4 `#(this is a tuple)])
    (event-mgr:notify e1)
    (event-mgr:notify e2)
    (event-mgr:notify e3)
    (event-mgr:notify e4)
    (event-mgr:flush-buffer)
    (reported e1)
    (reported e2)
    (reported e3)
    (reported e4)
    'ok))

(isolate-mat system-detail ()
  (define-tuple <foo> bar timestamp)
  (stop-event-mgr)
  (start-event-mgr)
  (silence-log-handler)
  (event-mgr:flush-buffer)
  (capture-events)
  ;; explicit timestamp
  (system-detail <foo> [bar 'bell] [timestamp 1234])
  (receive (after 1000 (raise `#(event-timeout 1)))
    [`(<foo> [bar bell] [timestamp 1234]) 'ok])
  ;; add timestamp if not present
  (let* ([before (erlang:now)]
         [_ (system-detail <foo> [bar 'none])]
         [after (erlang:now)])
    (receive (after 1000 (raise `#(event-timeout 2)))
      [`(<foo> [bar none] ,timestamp)
       (assert (<= before timestamp after))])))

(isolate-mat crash ()
  (process-trap-exit #t)
  (stop-event-mgr)
  (start-event-mgr)
  (silence-log-handler)
  (event-mgr:flush-buffer)
  (capture-events)
  (let* ([caller self]
         [pid (spawn&link
               (lambda ()
                 (event-mgr:add-handler raise)
                 (send caller 'ready)
                 (receive (after 1000 (raise 'timeout)))))])
    (receive
     (after 1000 (raise 'timeout-waiting-to-start))
     [ready 'ok])
    (event-mgr:notify 'crash)
    (reported 'crash)
    (receive (after 1000 (raise 'timeout-waiting))
      [#(EXIT ,@pid crash) (void)]))
  'ok)

(isolate-mat console ()
  (stop-event-mgr)
  (let-values ([(p get) (open-string-output-port)])
    (parameterize ([console-error-port p])
      (event-mgr:notify 'console-event))
    (let ([lines (split (get) #\newline)])
      (match-prefix lines "Date:")
      (match-prefix lines "Timestamp:")
      (match-prefix lines "Event: console-event")))
  (start-event-mgr)
  (event-mgr:flush-buffer)
  (let-values ([(p get) (open-string-output-port)])
    (parameterize ([console-error-port p])
      (event-mgr:notify 'console-event)
      (sleep-ms 10))
    (let ([lines (split (get) #\newline)])
      (match-prefix lines "Date:")
      (match-prefix lines "Timestamp:")
      (match-prefix lines "Event: console-event"))))

(isolate-mat dump-stack ()
  (stop-event-mgr)
  (start-event-mgr)
  (event-mgr:flush-buffer)
  (let-values ([(p get) (open-string-output-port)])
    (parameterize ([console-error-port p])
      (let ([x (catch (cdr 7))])
        (event-mgr:notify x)
        (event-mgr:notify x)
        (sleep-ms 10)))
    (let ([lines (split (get) #\newline)])
      (define (assert-prefix! pattern)
        (match lines
          [() (raise `#(pattern-not-found ,pattern))]
          [(,line . ,rest)
           (set! lines rest)
           (unless (starts-with? line pattern)
             (assert-prefix! pattern))]))
      (assert-prefix! "Date:")
      (assert-prefix! "Timestamp:")
      (assert-prefix! "Event: #(EXIT #<compound condition: Exception in cdr: 7 is not a pair>)")
      (assert-prefix! "Condition: Exception in cdr: 7 is not a pair")
      (assert-prefix! "Stack:")
      (assert-prefix! "Date:")
      (assert-prefix! "Timestamp:")
      (assert-prefix! "Event: #(EXIT #<compound condition: Exception in cdr: 7 is not a pair>)")
      (match lines ;; Should dump stack just once. Expect two blank lines.
        [("" "" . ,_) 'ok]))))

(isolate-mat handler-error ()
  (stop-event-mgr)
  (event-mgr:start&link)
  (event-mgr:flush-buffer)
  (match-let*
   ([#(error #(invalid-procedure foo)) (event-mgr:add-handler 'foo)]
    [#(error #(invalid-owner bar)) (event-mgr:add-handler (lambda (x) x) 'bar)]
    [#(error #(invalid-procedure foo)) (event-mgr:set-log-handler 'foo self)]
    [#(error #(invalid-owner bar)) (event-mgr:set-log-handler (lambda (x) x) 'bar)]
    [ok (event-mgr:set-log-handler (lambda (x) x) self)]
    [#(error log-handler-already-set) (event-mgr:set-log-handler (lambda (x) x) self)])
   'ok))
