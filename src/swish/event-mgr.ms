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
   (after 1000 (throw `#(event-timeout ,event)))
   [,@event 'ok]))

(define (silence-log-handler)
  (event-mgr:set-log-handler (lambda (x) x) (spawn (lambda () (receive)))))

(define (stop-event-mgr)
  (cond
   [(whereis 'event-mgr) => (lambda (pid) (kill pid 'kill))]
   [else 'ok]))

;; many event-mgr mats spawn processes that isolate-mat will clean up
;; for us, but we don't need to wait.
(define-syntax event-mgr-mat
  (syntax-rules ()
    [(_ name (tag ...) e0 e1 ...)
     (isolate-mat name
       (settings
        [tags (tag ...)]
        [process-kill-delay 0]
        [process-cleanup-deadline 0])
       e0 e1 ...)]))

(event-mgr-mat basic ()
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
    (event-mgr:unregister)
    (assert (not (whereis 'event-mgr)))
    'ok))

(event-mgr-mat buffering ()
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

(event-mgr-mat system-detail ()
  (define-tuple <foo> bar timestamp)
  (stop-event-mgr)
  (start-event-mgr)
  (silence-log-handler)
  (event-mgr:flush-buffer)
  (capture-events)
  ;; explicit timestamp
  (system-detail <foo> [bar 'bell] [timestamp 1234])
  (receive (after 1000 (throw `#(event-timeout 1)))
    [`(<foo> [bar bell] [timestamp 1234]) 'ok])
  ;; add timestamp if not present
  (let* ([before (erlang:now)]
         [_ (system-detail <foo> [bar 'none])]
         [after (erlang:now)])
    (receive (after 1000 (throw `#(event-timeout 2)))
      [`(<foo> [bar none] ,timestamp)
       (assert (<= before timestamp after))])))

(event-mgr-mat crash ()
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
                 (receive (after 1000 (throw 'timeout)))))])
    (receive
     (after 1000 (throw 'timeout-waiting-to-start))
     [ready 'ok])
    (event-mgr:notify 'crash)
    (reported 'crash)
    (receive (after 1000 (throw 'timeout-waiting))
      [`(EXIT ,@pid crash) (void)]))
  'ok)

(event-mgr-mat console ()
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

(event-mgr-mat dump-stack ()
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
          [() (throw `#(pattern-not-found ,pattern))]
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

(event-mgr-mat dump-stack2 ()
  (stop-event-mgr)
  (start-event-mgr)
  (event-mgr:flush-buffer)
  (let-values ([(p get) (open-string-output-port)])
    (parameterize ([console-error-port p])
      (let ([x (try (list (cons 2 (+ 7 (throw 'long)))))])
        (event-mgr:notify x)
        (event-mgr:notify x)
        (sleep-ms 10)))
    (let ([lines (split (get) #\newline)])
      (define (assert-regexp! pattern)
        (match lines
          [() (throw `#(pattern-not-found ,pattern))]
          [(,line . ,rest)
           (set! lines rest)
           (unless (pregexp-match pattern line)
             (assert-regexp! pattern))]))
      (assert-regexp! "^Date:")
      (assert-regexp! "^Timestamp:")
      (assert-regexp! "^Event: #<fault long>")
      ;; omit Condition: since it's already shown in the Event: and the
      ;; ugly display-condition output doesn't provide much of interest here
      (assert-regexp! "^Stack:")
      (assert-regexp! "^#<continuation.*>.*event-mgr.ms")
      (assert-regexp! "^Date:")
      (assert-regexp! "^Timestamp:")
      (assert-regexp! "^Event: #<fault long>")
      ;; again we omit Condition: for fault-condition
      (match lines ;; Should dump stack just once. Expect two blank lines.
        [("" "" . ,_) 'ok]))))

(event-mgr-mat handler-error ()
  (define op (open-output-string))
  (parameterize ([console-error-port op])
    (stop-event-mgr)
    (event-mgr:start&link)
    (event-mgr:flush-buffer)
    (process-trap-exit #t)
    (match-let*
     ([#(error #(invalid-procedure foo)) (event-mgr:add-handler 'foo)]
      [#(error #(invalid-owner bar)) (event-mgr:add-handler (lambda (x) x) 'bar)]
      [#(error #(invalid-procedure foo)) (event-mgr:set-log-handler 'foo self)]
      [#(error #(invalid-owner bar)) (event-mgr:set-log-handler (lambda (x) x) 'bar)]
      [#(error #(invalid-procedure baz)) (event-mgr:set-log-handler values self 'baz)]
      [,pid (spawn&link (lambda () (receive)))]
      [ok (event-mgr:set-log-handler (lambda (x) (raise x)) pid)]
      [#(error log-handler-already-set) (event-mgr:set-log-handler (lambda (x) x) self)])
     (event-mgr:notify 'crash)
     (receive (after 1000 (throw 'timeout))
       [`(EXIT ,@pid crash) 'ok])
     (match-regexps '(seek "^Date: " "^Timestamp: " "^Event: crash")
       (split (get-output-string op) #\newline))
     (assert (process? (whereis 'event-mgr))))))

;; log handler fails for certain events, but is configured to endure
(event-mgr-mat log-handler-endures ()
  (define op (open-output-string))
  (parameterize ([console-error-port op])
    (stop-event-mgr)
    (event-mgr:start&link)
    (event-mgr:flush-buffer)
    ;; do not trap exit: crash the test if event-mgr kills pid with the raised
    ;; condition
    (match-let*
     ([,pid (spawn&link (lambda () (receive)))]
      [,me self]
      [ok (event-mgr:set-log-handler
           (lambda (x)
             (unless (number? x)
               (raise `#(unhandled-event ,x)))
             (send me x))
           pid
           (lambda (e) #t))])
     (for-each event-mgr:notify '(1 2 3 crash 4 5 6))
     (for-each
      (lambda (x)
        (match (receive [,x x])
          [,@x 'ok]))
      '(1 2 3 4 5 6))
     (match-regexps
      '(seek
        "^Date: " "^Timestamp: " "^Event: crash"
        seek
        "^Date: " "^Timestamp: " "^Event: #\\(unhandled-event crash\\)")
      (split (get-output-string op) #\newline))
     (assert (process? (whereis 'event-mgr))))))

;; log handler endure? predicate fails
(event-mgr-mat log-handler-shield-fails ()
  (define op (open-output-string))
  (parameterize ([console-error-port op])
    (stop-event-mgr)
    (event-mgr:start&link)
    (event-mgr:flush-buffer)
    (process-trap-exit #t)
    (match-let*
     ([,pid (spawn&link (lambda () (receive)))]
      [,me self]
      [ok (event-mgr:set-log-handler
           (lambda (x)
             (unless (number? x)
               (raise `#(unhandled-event ,x)))
             (send me x))
           pid
           (lambda (e)
             (send me 'saving-throw)
             (raise 'shield-failed)))])
     (for-each event-mgr:notify '(1 2 3 crash 4 5 6))
     (for-each
      (lambda (x)
        (match (receive [,x x])
          [,@x 'ok]))
      '(1 2 3 saving-throw))
     (receive (after 1000 (throw 'timeout))
      [`(EXIT ,@pid #(unhandled-event crash)) 'ok])
     (match-regexps
      '(seek "^Date: " "^Timestamp: " "^Event: crash"
        seek "^Date: " "^Timestamp: " "^Event: 4"
        seek "^Date: " "^Timestamp: " "^Event: 5"
        seek "^Date: " "^Timestamp: " "^Event: 6")
      (split (get-output-string op) #\newline))
     (assert (process? (whereis 'event-mgr))))))

(event-mgr-mat exit-reason ()
  (match-let*
   ([#t (informative-exit-reason? (make-fault 'normal))]
    [#f (informative-exit-reason? (make-fault/no-cc 'normal))]
    [#f (informative-exit-reason? (try (raise 'normal)))]
    [#t (informative-exit-reason? (try (throw 'normal)))]
    [#t (informative-exit-reason? (make-fault 'shutdown))]
    [#f (informative-exit-reason? (make-fault/no-cc 'shutdown))]
    [#f (informative-exit-reason? (try (raise 'shutdown)))]
    [#t (informative-exit-reason? (try (throw 'shutdown)))]
    [#t (informative-exit-reason? (try (raise 'bar)))]
    [#t (informative-exit-reason? (make-fault 'oops))]
    [#t (informative-exit-reason? (make-fault/no-cc 'oops))]
    [#t (informative-exit-reason? 'bug)]
    [#f (informative-exit-reason? 'normal)]
    [#f (informative-exit-reason? 'shutdown)])
   'ok)
  (match-let*
   ([,normalize
     (lambda x
       (let-values ([(reason details) (apply normalize-exit-reason x)])
         (list reason details)))]
    [,c (guard (c [else c]) (cdr 2))]
    [(exception ,@c) (normalize c)]
    [(exception ,@c) (normalize c #f)]
    [(exception ,@c) (normalize c (try (raise 'normal)))]
    [(exception `(catch ,@c)) (normalize c (try (raise c)))]
    [(normal #f) (normalize 'normal)]
    [(normal #f) (normalize 'normal 'normal)]
    [(normal #f) (normalize 'normal 'shutdown)]
    [(shutdown #f) (normalize 'shutdown 'shutdown)]
    [,fault-normal (make-fault 'normal)]
    [,fault-shutdown (make-fault 'shutdown)]
    [(normal ,@fault-normal) (normalize fault-normal)]
    [(normal ,@fault-normal) (normalize 'normal fault-normal)]
    [(shutdown ,@fault-shutdown) (normalize fault-shutdown)]
    [(shutdown ,@fault-shutdown) (normalize 'shutdown fault-shutdown)]
    [,no-cc-fault-normal (make-fault/no-cc 'normal)]
    [,no-cc-fault-shutdown (make-fault/no-cc 'shutdown)]
    [(normal #f) (normalize no-cc-fault-normal)]
    [(normal #f) (normalize 'normal no-cc-fault-normal)]
    [(normal #f) (normalize (try (raise 'normal)))]
    [(normal #f) (normalize (catch (raise 'normal)))]
    [(normal #f) (normalize 'normal (try (raise 'normal)))]
    [(shutdown #f) (normalize no-cc-fault-shutdown)]
    [(shutdown #f) (normalize 'shutdown no-cc-fault-shutdown)]
    [(shutdown #f) (normalize (try (raise 'shutdown)))]
    [(shutdown #f) (normalize (catch (raise 'shutdown)))]
    [(shutdown #f) (normalize 'shutdown (try (raise 'shutdown)))]
    [,r0 '#(bad-arg foo bar)]
    [(,@r0 `(catch ,@r0)) (normalize r0)]
    [(,@r0 `(catch ,@r0)) (normalize (catch (raise r0)))]
    [(,@r0 `(catch ,@r0)) (normalize r0 #f)]
    [(,@r0 `(catch ,@r0)) (normalize (try (throw r0)))]
    [(,@r0 `(catch ,@r0)) (normalize (make-fault r0))]
    [(,@r0 `(catch ,@r0)) (normalize (make-fault/no-cc r0))]
    [(red-flag `(catch red-flag red-flag)) (normalize 'red-flag)]
    [,e1 (try (raise 'red-flag))]
    [(red-flag `(catch red-flag red-flag)) (normalize e1)]
    [,e2 (try (throw 'red-flag))]
    [(red-flag `(catch red-flag ,@e2)) (normalize e2)])
   'ok))
