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
 (swish events)
 (swish gen-server)
 (swish mat)
 (swish supervisor)
 (swish testing)
 (swish watcher)
 )

(module (supervisor-1:start-child)
  (define (init x)
    (match x
      [normal
       (process-trap-exit #t)
       `#(ok ())]))
  (define (terminate reason state) 'ok)
  (define (handle-call msg from state) `#(reply ,msg ,state))
  (define (handle-cast msg state) (match msg))
  (define (handle-info msg state)
    (match msg
      [die `#(stop died ,state)]
      [stop `#(stop normal ,state)]
      [(raise ,reason) (raise reason)]
      [(throw ,reason ,inner) (throw reason inner)]
      [#(sleep ,t)
       (sleep-ms t)
       `#(no-reply ,state)]
      [,_ `#(no-reply ,state)]))
  (define child-ignored (make-process-parameter #f))
  (define start-child-error (make-process-parameter #f))

  (define (supervisor-1:start-child . args)
    (match args
      [() (gen-server:start&link #f 'normal)]
      [(ignore)
       (cond
        [(child-ignored) (supervisor-1:start-child)]
        [else
         (child-ignored #t)
         'ignore])]
      [(error)
       (cond
        [(start-child-error) (gen-server:start&link #f 'error)]
        [else
         (start-child-error #t)
         (supervisor-1:start-child)])])))

(define (naughty-child:start&link pid)
  (define (init pid)
    (unlink pid)
    `#(ok ()))
  (define (terminate reason state) 'ok)
  (define (handle-call msg from state) `#(reply ok ,state))
  (define (handle-cast msg state) `#(no-reply ,state))
  (define (handle-info msg state) `#(no-reply ,state))
  (gen-server:start&link 'naughty-foo pid))

(define (bad-child:start&link)
  (define (init) (raise 'crash))
  (define (terminate reason state) 'ok)
  (define (handle-call msg from state) `#(reply ok ,state))
  (define (handle-cast msg state) `#(no-reply ,state))
  (define (handle-info msg state) `#(no-reply ,state))
  (gen-server:start&link #f))

(define (start strategy max-intensity period start-specs)
  (supervisor:start&link 'sup-test strategy max-intensity period start-specs))

(define (supervisor-mat-cleanup)
  (cond
   [(whereis 'sup-test) =>
    (lambda (pid)
      (when (process-alive? pid 0)
        (kill pid 'shutdown)
        (receive (after 5000 (throw 'no-shutdown))
          [`(EXIT ,@pid shutdown) 'ok]
          [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])))]))

(define (get-events timeout)
  (receive (after timeout '())
    [,x (cons x (get-events timeout))]))

(define-syntax supervisor-mat
  (syntax-rules ()
    [(_ name tags e1 e2 ...)
     (isolate-mat name tags
       (process-trap-exit #t)
       (on-exit (supervisor-mat-cleanup)
         e1 e2 ...))]))

(define (get-child-counts supervisor)
  (let lp ([children (supervisor:get-children supervisor)]
           [specs 0] [active 0] [supers 0] [workers 0])
    (match children
      [() (list specs active supers workers)]
      [(`(<child> [pid ,pid] [type ,type]) . ,children)
       (lp children
         (+ specs 1)
         (+ active (if (and pid (process-alive? pid 0)) 1 0))
         (+ supers (if (eq? type 'supervisor) 1 0))
         (+ workers (if (eq? type 'worker) 1 0)))])))

(define (which-children supervisor)
  (map
   (lambda (x)
     (match x
       [`(<child> [name ,name] [pid ,pid] [type ,type])
        `#(,name ,pid ,type)]))
   (supervisor:get-children supervisor)))

(supervisor-mat normal ()
  (match-let* ([#(ok ,pid) (start 'one-for-one 2 3600 '())])
    (kill pid 'shutdown)
    (receive (after 2000 (throw 'no-exit-reason))
      [`(EXIT ,@pid shutdown) 'ok]
      [`(EXIT ,@pid ,reason ,e)
       (throw `#(bad-exit-reason ,reason) e)]))
  'ok)

(supervisor-mat ignore-child ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,(lambda () (supervisor-1:start-child 'ignore))
                 permanent 1000 worker)]
    [,child2 `#(child2 ,supervisor-1:start-child
                 permanent 1000 worker)]
    [#(ok #f) (supervisor:start-child 'sup-test child1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [(#(child2 ,@cpid2 worker) #(child1 #f worker)) (which-children 'sup-test)]
    [(2 1 0 2) (get-child-counts 'sup-test)])
   'ok))

(supervisor-mat bad-child ()
  (define child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker))
  (define child2 `#(child2 ,bad-child:start&link permanent 1000 worker))
  (capture-events)
  (match-let*
   ([#(error shutdown) (start 'one-for-one 2 3600 (list child1 child2))]
    [(`(<child-start> ,supervisor
         [pid ,pid1]
         [name child1]
         [restart-type permanent]
         [shutdown 1000]
         [type worker])
      `(<supervisor-error> ,@supervisor
         [error-context start-error]
         [reason crash]
         [details `(catch crash)]
         [child-pid #f]
         [child-name child2])
      `(EXIT ,@supervisor shutdown)
      `(<child-end> [pid ,@pid1] [killed 1] [reason shutdown] [details #f]))
     (get-events 0)])
   'ok))

(supervisor-mat bad-child2 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,bad-child:start&link permanent 1000 worker)]
    [#(error crash) (supervisor:start-child 'sup-test child1)]
    [(0 0 0 0) (get-child-counts 'sup-test)])
   'ok))

(supervisor-mat bad-child3 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,(lambda () (raise 'crash)) permanent 1000 worker)]
    [#(error crash) (supervisor:start-child 'sup-test child1)]
    [(0 0 0 0) (get-child-counts 'sup-test)])
   'ok))

(supervisor-mat bad-child4 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,(lambda () 'flubber) permanent 1000 worker)]
    [#(error #(bad-return-value flubber))
     (supervisor:start-child 'sup-test child1)]
    [(0 0 0 0) (get-child-counts 'sup-test)])
   'ok))

(supervisor-mat bad-child5 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,(lambda () `#(ok flubber)) permanent 1000 worker)]
    [#(error #(bad-return-value #(ok flubber)))
     (supervisor:start-child 'sup-test child1)]
    [(0 0 0 0) (get-child-counts 'sup-test)])
   'ok))

(supervisor-mat bad-child6 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,child2 `#(child2 ,(lambda () `#(ok ,cpid1)) permanent 1000 worker)]
    [#(error #(duplicate-process ,@cpid1))
     (supervisor:start-child 'sup-test child2)])
   'ok))

(supervisor-mat bad-child7 ()
  (define global-name (gensym))
  (define child1 `#(child1 ,(lambda()
                              (match (supervisor-1:start-child)
                                [#(ok ,pid)
                                 (register global-name pid)
                                 `#(ok ,pid)]
                                [,other other]))
                     permanent 1000 worker))
  (define child2 `#(child2 ,(lambda () `#(ok ,(whereis global-name)))
                     permanent 1000 worker))
  (capture-events)
  (match-let*
   ([#(error shutdown) (start 'one-for-one 2 3600 (list child1 child2))])

   (receive
    (after 0 (throw 'timeout-waiting-for-error-report))
    [`(<supervisor-error>
       [timestamp ,_]
       [supervisor ,_]
       [error-context start-error]
       [reason #(duplicate-process ,_)]
       [details `(catch #(duplicate-process ,_))]
       [child-pid #f]
       [child-name child2])
     'ok])))

(supervisor-mat event-order ()
  (define child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker))
  (capture-events)
  (match-let*
   ([#(ok ,supervisor) (start 'one-for-one 2 3600 (list child1))]
    [(`(<child-start> ,@supervisor
         [pid ,pid1]
         [name child1]
         [restart-type permanent]
         [shutdown 1000]
         [type worker]))
     (get-events 0)]
    [ok (supervisor:terminate-child supervisor 'child1)]
    [(`(<child-end> [pid ,@pid1] [killed 1] [reason shutdown] [details #f]))
     (get-events 0)]
    [#(ok ,pid2) (supervisor:restart-child supervisor 'child1)]
    [(`(<child-start> ,@supervisor [pid ,@pid2]
         [name child1]
         [restart-type permanent]
         [shutdown 1000]
         [type worker]))
     (get-events 0)]
    [,_ (send pid2 `(raise ruckus))]
    [(`(<gen-server-terminating>
        [pid ,@pid2]
        [last-message (raise ruckus)]
        [state ()]
        [reason ruckus]
        [details `(catch ruckus)])
      `(<child-end> [pid ,@pid2] [killed 0] [reason ruckus] [details `(catch ruckus)])
      `(<child-start> ,@supervisor
         [pid ,pid3]
         [name child1]
         [restart-type permanent]
         [shutdown 1000]
         [type worker]))
     (get-events 100)]
    [,_ (supervisor-mat-cleanup)]
    [(`(<child-end> [pid ,@pid3] [killed 1] [reason shutdown] [details #f]))
     (get-events 0)])
   'ok)
  ;; now with low tolerance for restart to trigger supervisor-error
  (match-let*
   ([#(ok ,supervisor) (start 'one-for-one 0 1 (list child1))]
    [(`(<child-start> ,@supervisor
         [pid ,pid1]
         [name child1]
         [restart-type permanent]
         [shutdown 1000]
         [type worker]))
     (get-events 0)]
    [,_ (send pid1 `(raise objection))]
    [(`(<gen-server-terminating>
        [pid ,@pid1]
        [last-message (raise objection)]
        [state ()]
        [reason objection]
        [details `(catch objection ,e0)])
      `(EXIT ,@supervisor shutdown)
      `(<child-end> [pid ,@pid1] [killed 0] [reason objection] [details `(catch objection ,e1)])
      `(<supervisor-error> ,@supervisor
         [error-context shutdown]
         [reason reached-max-restart-intensity]
         [details `(catch reached-max-restart-intensity)]
         [child-pid ,@pid1]
         [child-name child1]))
     (get-events 100)]
    [() (exit-reason->stacks e0)]
    [() (exit-reason->stacks e1)])
   'ok)
  ;; now with low tolerance for restart to trigger supervisor-error, but throw
  (match-let*
   ([#(ok ,supervisor) (start 'one-for-one 0 1 (list child1))]
    [,oops (make-fault 'fumble)]
    [(`(<child-start> ,@supervisor
         [pid ,pid1]
         [name child1]
         [restart-type permanent]
         [shutdown 1000]
         [type worker]))
     (get-events 0)]
    [,_ (send pid1 `(throw ball ,oops))]
    [(`(<gen-server-terminating>
        [pid ,@pid1]
        [last-message (throw ball ,@oops)]
        [state ()]
        [reason ball]
        [details `(catch ball ,e0)])
      `(EXIT ,@supervisor shutdown)
      `(<child-end> [pid ,@pid1] [killed 0] [reason ball] [details `(catch ball ,e1)])
      `(<supervisor-error> ,@supervisor
         [error-context shutdown]
         [reason reached-max-restart-intensity]
         [details `(catch reached-max-restart-intensity)]
         [child-pid ,@pid1]
         [child-name child1]))
     (get-events 100)]
    [(,oops-k) (exit-reason->stacks oops)]
    [(,@oops-k ,k) (exit-reason->stacks e0)]
    [(,@oops-k ,@k) (exit-reason->stacks e1)])
   'ok))

(supervisor-mat start-error ()
  (match-let* ([#(error ,(reason <= #(invalid-strategy foo)))
                (start 'foo 'bar 'baz '())])
    (receive (after 2000 (throw 'no-exit-reason))
      [`(EXIT ,_ ,@reason) 'ok]
      [`(EXIT ,_ ,reason ,e)
       (throw `#(bad-exit-reason ,reason) e)])))

(supervisor-mat start-error2 ()
  (match-let* ([#(error ,(reason <= #(invalid-intensity bar)))
                (start 'one-for-one 'bar 'baz '())])
    (receive (after 2000 (throw 'no-exit-reason))
      [`(EXIT ,_ ,@reason) 'ok]
      [`(EXIT ,_ ,reason ,e)
       (throw `#(bad-exit-reason ,reason) e)])))

(supervisor-mat start-error3 ()
  (match-let* ([#(error ,(reason <= #(invalid-period baz)))
                (start 'one-for-one 0 'baz '())])
    (receive (after 2000 (throw 'no-exit-reason))
      [`(EXIT ,_ ,@reason) 'ok]
      [`(EXIT ,_ ,reason ,e)
       (throw `#(bad-exit-reason ,reason) e)])))

(supervisor-mat stop-infinity ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent infinity supervisor)]
    [,child2 `#(child2 ,supervisor-1:start-child permanent infinity worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(error #(invalid-shutdown infinity))
     (supervisor:start-child 'sup-test child2)])
   (kill pid 'shutdown)
   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@pid shutdown) 'ok]
     [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid1 shutdown) 'ok]
     [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   'ok))

(supervisor-mat stop-timeout ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [,child2 `#(child2 ,supervisor-1:start-child permanent 1000 worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])
   (send cpid2 `#(sleep 200000))
   (kill pid 'shutdown)

   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@pid shutdown) 'ok]
     [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid1 shutdown) 'ok]
     [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid2 killed) 'ok]
     [`(EXIT ,@cpid2 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   'ok))

(supervisor-mat stop-timeout2 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [,child2 `#(child2 ,supervisor-1:start-child permanent 1000 worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   ;; This causes the error case of monitor-child to occur and flow
   ;; thru shutdown.
   (with-interrupts-disabled
    (send cpid2 'die)
    (kill pid 'shutdown))

   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@pid shutdown) 'ok]
     [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid1 shutdown) 'ok]
     [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid2 died) 'ok]
     [`(EXIT ,@cpid2 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   'ok))

(supervisor-mat stop-timeout3 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [,child2 `#(child2 ,supervisor-1:start-child permanent 1000 worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   ;; This causes the error case of monitor-child to occur and flow
   ;; thru shutdown.
   (with-interrupts-disabled
    (send cpid2 `(throw darts ,(make-fault 'bad-aim)))
    (kill pid 'shutdown))

   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@pid shutdown) 'ok]
     [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid1 shutdown) 'ok]
     [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid2 darts ,e)
      (match-let* ([(,k0 ,k1) (exit-reason->stacks e)])
        'ok)]
     [`(EXIT ,@cpid2 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   'ok))

(supervisor-mat stop-brutal-kill ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [,child2 `#(child2 ,supervisor-1:start-child permanent brutal-kill worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])
   (send cpid2 `#(sleep 200000))
   (kill pid 'shutdown)

   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@pid shutdown) 'ok]
     [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid1 shutdown) 'ok]
     [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid2 killed) 'ok]
     [`(EXIT ,@cpid2 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   'ok))

(supervisor-mat stop-brutal-kill2 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [,child2 `#(child2 ,supervisor-1:start-child permanent brutal-kill worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   ;; This causes the error case of monitor-child to occur and flow
   ;; thru shutdown.
   (with-interrupts-disabled
    (send cpid2 'die)
    (kill pid 'shutdown))

   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@pid shutdown) 'ok]
     [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid1 shutdown) 'ok]
     [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid2 died) 'ok]
     [`(EXIT ,@cpid2 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   'ok))

(supervisor-mat stop-brutal-kill3 ()
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [,child2 `#(child2 ,supervisor-1:start-child permanent brutal-kill worker)]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   ;; This causes the error case of monitor-child to occur and flow
   ;; thru shutdown.
   (with-interrupts-disabled
    (send cpid2 `(throw confetti #f))
    (kill pid 'shutdown))

   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@pid shutdown) 'ok]
     [`(EXIT ,@pid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid1 shutdown) 'ok]
     [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

   (receive (after 2000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid2 confetti ,e)
      (match-let* ([(,k0) (exit-reason->stacks e)])
        'ok)]
     [`(EXIT ,@cpid2 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   'ok))

(supervisor-mat child-admin ()
  (match-let*
   ([,child `#(child1 ,supervisor-1:start-child permanent 1000 worker)]
    [#(ok ,pid) (start 'one-for-one 2 3600 (list child))]
    [(#(child1 ,cpid worker)) (which-children 'sup-test)]
    [(1 1 0 1) (get-child-counts 'sup-test)]
    [,_ (link cpid)]
    ;; start again
    [#(error #(already-started ,@cpid))
     (supervisor:start-child 'sup-test child)]
    ;; termination
    [#(error not-found) (supervisor:terminate-child 'sup-test 'hej)]
    [#(EXIT #(no-process #(gen-server call ,_)))
     (catch (supervisor:terminate-child 'foo 'child1))]
    [ok (supervisor:terminate-child 'sup-test 'child1)])
   (receive (after 1000 (throw 'no-child-termination))
     [`(EXIT ,@cpid shutdown) 'ok]
     [`(EXIT ,@cpid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   (match-let*
    ([(#(child1 #f worker)) (which-children 'sup-test)]
     [(1 0 0 1) (get-child-counts 'sup-test)]
     ;; expect success
     [ok (supervisor:terminate-child 'sup-test 'child1)]
     ;; start of existing, but not running child
     [#(error already-present) (supervisor:start-child 'sup-test child)]
     ;; restart
     [#(ok ,cpid2) (supervisor:restart-child 'sup-test 'child1)]
     [(#(child1 ,@cpid2 worker)) (which-children 'sup-test)]
     [(1 1 0 1) (get-child-counts 'sup-test)]
     [#(error running) (supervisor:restart-child 'sup-test 'child1)]
     [#(error not-found) (supervisor:restart-child 'sup-test 'child2)]
     ;; deletion
     [#(error running) (supervisor:delete-child 'sup-test 'child1)]
     [#(error not-found) (supervisor:delete-child 'sup-test 'hej)]
     [#(EXIT #(no-process #(gen-server call ,_)))
      (catch (supervisor:delete-child 'foo 'child1))]
     [ok (supervisor:terminate-child 'sup-test 'child1)]
     [ok (supervisor:delete-child 'sup-test 'child1)]
     [#(error not-found) (supervisor:restart-child 'sup-test 'child1)]
     [() (which-children 'sup-test)]
     [(0 0 0 0) (get-child-counts 'sup-test)]
     ;; start
     [#(EXIT #(no-process #(gen-server call ,_)))
      (catch (supervisor:start-child 'foo child))]
     [#(ok ,cpid3) (supervisor:start-child 'sup-test child)]
     [(#(child1 ,@cpid3 worker)) (which-children 'sup-test)]
     [(1 1 0 1) (get-child-counts 'sup-test)]

     [#(EXIT #(no-process #(gen-server call (foo get-children infinity))))
      (catch (which-children 'foo))])
    'ok)))

(supervisor-mat child-restart-fail ()
  (match-let*
   ([,child `#(child1 ,(lambda () (supervisor-1:start-child 'error))
                permanent 1000 worker)]
    [#(ok ,pid) (start 'one-for-one 2 3600 (list child))]
    [ok (supervisor:terminate-child 'sup-test 'child1)]
    [#(error ,x) (supervisor:restart-child 'sup-test 'child1)])
   'ok))

(supervisor-mat child-specs ()
  (define B0 `#(child))
  (define B1 `#(child mfa permanent 1000 worker))
  (define B2 `#(child ,void prmanent 1000 worker))
  (define B3 `#(child ,void permanent -10 worker))
  (define B4 `#(child ,void permanent 10 wrker))
  (define B5 `#(child ,void permanent infinity worker))
  (define B6 `#("child" ,void permanent 1000 worker))
  (define B7 `#(child ,void permanent 10 worker))

  (match-let*
   ([#(error #(start-specs #(duplicate-child-name child)))
     (start 'one-for-one 2 3600 (list B7 B7))]
    [#(error #(start-specs ,_)) (start 'one-for-one 2 3600 (list B0))]
    [#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [#(error ,_) (supervisor:start-child 'sup-test 'hej)]
    [#(error #(invalid-child-spec ,@B0)) (supervisor:start-child 'sup-test B0)]
    [#(error #(invalid-thunk mfa)) (supervisor:start-child 'sup-test B1)]
    [#(error #(invalid-restart-type prmanent))
     (supervisor:start-child 'sup-test B2)]
    [#(error #(invalid-shutdown -10)) (supervisor:start-child 'sup-test B3)]
    [#(error #(invalid-type wrker)) (supervisor:start-child 'sup-test B4)]
    [#(error ,_) (supervisor:start-child 'sup-test B5)]
    [#(error #(invalid-name "child")) (supervisor:start-child 'sup-test B6)])
   'ok))

(supervisor-mat permanent-normal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'stop)
   (sleep-ms 100)

   (match-let*
    ([(#(child1 ,pid worker)) (which-children 'sup-test)]
     [,_ (unless (process? pid)
           (throw `#(permanent-child-not-restarted ,child1)))]
     [(1 1 0 1) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat transient-normal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child transient 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'stop)
   (sleep-ms 100)

   (match-let*
    ([(#(child1 #f worker)) (which-children 'sup-test)]
     [(1 0 0 1) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat temporary-normal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child temporary 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'stop)
   (sleep-ms 100)

   (match-let*
    ([(#(child1 #f worker)) (which-children 'sup-test)]
     [(1 0 0 1) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat watch-only-normal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child watch-only 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'stop)
   (sleep-ms 100)

   (match-let*
    ([() (which-children 'sup-test)]
     [(0 0 0 0) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat permanent-abnormal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'die)
   (sleep-ms 100)

   (match-let*
    ([(#(child1 ,pid worker)) (which-children 'sup-test)]
     [,_ (unless (process? pid)
           (throw `#(permanent-child-not-restarted ,child1)))]
     [(1 1 0 1) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat transient-abnormal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child transient 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'die)
   (sleep-ms 100)

   (match-let*
    ([(#(child1 ,pid worker)) (which-children 'sup-test)]
     [,_ (unless (process? pid)
           (throw `#(transient-child-not-restarted ,child1)))]
     [(1 1 0 1) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat temporary-abnormal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child temporary 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'die)
   (sleep-ms 100)

   (match-let*
    ([(#(child1 #f worker)) (which-children 'sup-test)]
     [(1 0 0 1) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat watch-only-abnormal ()
  (match-let*
   ([#(ok ,_) (start 'one-for-one 2 3600 '())]
    [,child1 `#(child1 ,supervisor-1:start-child watch-only 1000 worker)]

    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)])

   (send cpid1 'die)
   (sleep-ms 100)

   (match-let*
    ([() (which-children 'sup-test)]
     [(0 0 0 0) (get-child-counts 'sup-test)])
    'ok)))

(supervisor-mat one-for-one ()
  (define child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker))
  (define child2 `#(child2 ,supervisor-1:start-child permanent 1000 worker))
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 2 3600 '())]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   (send cpid1 'die)
   (receive
    [`(EXIT ,@cpid1 died) 'ok]
    [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   (sleep-ms 100)

   (let ([children (which-children 'sup-test)])
     (unless (= (length children) 2)
       (throw `#(bad-child-list ,children)))
     (cond
      [(find (lambda (x)
               (match x
                 [#(,_ ,@cpid2 ,_) #t]
                 [,_ #f]))
         children) 'ok]
      [else (throw 'bad-child)]))
   (match-let* ([(2 2 0 2) (get-child-counts 'sup-test)])
     'ok)

   ;; restart frequency
   (send cpid2 'die)
   (receive [`(EXIT ,@cpid2 ,_) 'ok])
   (sleep-ms 100)
   (match-let* ([(#(,_ ,pid4 ,_) . ,_) (which-children 'sup-test)])
     (send pid4 'die)
     (receive
      (after 3000 (throw 'restart-failed))
      [`(EXIT ,@pid ,_) 'ok]))))

(supervisor-mat one-for-one-escalation ()
  (define child1 `#(child1 ,(lambda () (supervisor-1:start-child 'error))
                     permanent 1000 worker))
  (define child2 `#(child2 ,supervisor-1:start-child permanent 1000 worker))
  (capture-events)
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 4 3600 '())]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   (send cpid1 'die)
   (receive
    [`(EXIT ,@cpid1 died) 'ok]
    [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   (receive
    (after 2000 (throw 'supervisor-alive))
    [`(EXIT ,@pid ,_) 'ok])
   (receive
    (after 4000 (throw 'all-not-terminated))
    [`(EXIT ,@cpid2 ,_) 'ok])
   (do ([i 0 (+ i 1)]) ((= i 4))
     (receive (after 0 (throw `(no-event ,i)))
       [`(<supervisor-error>
          [supervisor ,@pid]
          [error-context start-error]
          [child-pid ,@cpid1]
          [child-name child1])
        'ok]))
   (receive (after 0 (throw 'no-max-restart))
     [`(<supervisor-error>
        [supervisor ,@pid]
        [error-context shutdown]
        [reason reached-max-restart-intensity]
        [details `(catch reached-max-restart-intensity)])
      'ok])))

(supervisor-mat one-for-all ()
  (define child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker))
  (define child2 `#(child2 ,supervisor-1:start-child permanent 1000 worker))
  (match-let*
   ([#(ok ,pid) (start 'one-for-all 2 3600 '())]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   (send cpid1 'die)
   (receive
    [`(EXIT ,@cpid1 died) 'ok]
    [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   (receive [`(EXIT ,@cpid2 ,_) 'ok])
   (sleep-ms 100)

   ;; no old children should be alive
   (let ([children (which-children 'sup-test)])
     (unless (= (length children) 2)
       (throw `#(bad-child-list ,children)))
     (let ([sch (map (lambda (x) (match x [#(,_ ,p ,_) p])) children)])
       (when (memq cpid1 sch)
         (throw 'bad-child))
       (when (memq cpid2 sch)
         (throw 'bad-child))))
   (match-let* ([(2 2 0 2) (get-child-counts 'sup-test)])
     'ok)

   ;; restart frequency
   (match-let* ([(#(,_ ,pid3 ,_) . ,_) (which-children 'sup-test)])
     (send pid3 'die)
     (sleep-ms 100)
     (match-let* ([(#(,_ ,pid4 ,_) . ,_) (which-children 'sup-test)])
       (send pid4 'die)
       (receive
        (after 3000 (throw 'restart-failed))
        [`(EXIT ,@pid ,_) 'ok])
       (kill pid 'shutdown)))))

(supervisor-mat one-for-all-escalation ()
  (define child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker))
  (define child2 `#(child2 ,(lambda () (supervisor-1:start-child 'error))
                     permanent 1000 worker))
  (match-let*
   ([#(ok ,pid) (start 'one-for-all 4 3600 '())]
    [#(ok ,cpid1) (supervisor:start-child 'sup-test child1)]
    [,_ (link cpid1)]
    [#(ok ,cpid2) (supervisor:start-child 'sup-test child2)]
    [,_ (link cpid2)])

   (send cpid1 'die)
   (receive
    [`(EXIT ,@cpid1 died) 'ok]
    [`(EXIT ,@cpid1 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   (receive
    (after 2000 (throw 'all-not-terminated))
    [`(EXIT ,@cpid2 ,_) 'ok])
   (receive
    (after 4000 (throw 'supervisor-alive))
    [`(EXIT ,@pid ,_) 'ok])))

(supervisor-mat child-unlink ()
  (match-let*
   ([#(ok ,sup-pid) (start 'one-for-one 2 3600 '())]
    [,child `#(naughty-child ,(lambda () (naughty-child:start&link sup-pid))
                permanent 1000 worker)]
    [#(ok ,_) (supervisor:start-child 'sup-test child)]
    [ok (supervisor:terminate-child sup-pid 'naughty-child)]
    [(1 0 0 1) (get-child-counts 'sup-test)])
   'ok))

(supervisor-mat tree ()
  (define child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker))
  (define child2 `#(child2 ,supervisor-1:start-child permanent 1000 worker))
  (define child3 `#(child3 ,(lambda () (supervisor-1:start-child 'error))
                     permanent 1000 worker))
  (define child4 `#(child4 ,supervisor-1:start-child permanent 1000 worker))

  (define child-sup1 `#(sup-child1
                        ,(lambda ()
                           (supervisor:start&link #f 'one-for-one 4 3600
                             (list child1 child2)))
                        permanent infinity supervisor))
  (define child-sup2 `#(sup-child2
                        ,(lambda ()
                           (supervisor:start&link #f 'one-for-one 4 3600 '()))
                        permanent infinity supervisor))
  (match-let*
   ;; top supervisor
   ([#(ok ,pid) (start 'one-for-all 4 3600 '())]
    ;; child supervisors
    [#(ok ,sup1) (supervisor:start-child pid child-sup1)]
    [#(ok ,sup2) (supervisor:start-child pid child-sup2)]
    [(2 2 2 0) (get-child-counts pid)]
    ;; workers
    [(#(,_ ,cpid2 ,_) #(,_ ,cpid1 ,_)) (which-children sup1)]
    [(2 2 0 2) (get-child-counts sup1)]
    [(0 0 0 0) (get-child-counts sup2)]
    ;; dynamic children
    [#(ok ,cpid3) (supervisor:start-child sup2 child3)]
    [#(ok ,cpid4) (supervisor:start-child sup2 child4)]
    [(2 2 0 2) (get-child-counts sup1)]
    [(2 2 0 2) (get-child-counts sup2)])

   (link sup1)
   (link sup2)
   (link cpid1)
   (link cpid2)
   (link cpid3)
   (link cpid4)

   ;; only the process that dies is restarted
   (send cpid4 'die)
   (receive
    (after 10000 (throw 'child-was-not-killed))
    [`(EXIT ,@cpid4 ,_) 'ok])
   (sleep-ms 100)

   (match-let*
    ([(#(,_ ,@cpid2 ,_) #(,_ ,@cpid1 ,_)) (which-children sup1)]
     [(2 2 0 2) (get-child-counts sup1)]
     [(#(,_ ,new-cpid4 ,_) #(,_ ,@cpid3 ,_)) (which-children sup2)]
     [(2 2 0 2) (get-child-counts sup2)])
    (link new-cpid4)

    ;; supervisor tree is restarted, but not dynamic children
    (send cpid3 'die)
    (receive
     (after 1000 (throw 'child-was-not-killed))
     [`(EXIT ,@cpid3 died) 'ok]
     [`(EXIT ,@cpid3 ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])

    (sleep-ms 1000)
    (receive
     (after 1000 (throw 'child-was-not-killed))
     [`(EXIT ,@new-cpid4 ,_) 'ok])
    (receive
     (after 1000 (throw 'child-was-not-killed))
     [`(EXIT ,@sup2 ,_) 'ok])
    (receive
     (after 1000 (throw 'child-was-not-killed))
     [`(EXIT ,@cpid1 ,_) 'ok])
    (receive
     (after 1000 (throw 'child-was-not-killed))
     [`(EXIT ,@cpid2 ,_) 'ok])
    (receive
     (after 1000 (throw 'child-was-not-killed))
     [`(EXIT ,@sup1 ,_) 'ok])

    (match-let*
     ([(#(sup-child2 ,new-sup2 ,_) #(sup-child1 ,new-sup1 ,_))
       (which-children pid)]
      [(2 2 2 0) (get-child-counts pid)]

      [(#(child2 ,_ ,_) #(child1 ,_ ,_)) (which-children new-sup1)]
      [(2 2 0 2) (get-child-counts new-sup1)]

      [() (which-children new-sup2)]
      [(0 0 0 0) (get-child-counts new-sup2)])
     'ok))))

(supervisor-mat restart-period ()
  (define child1 `#(child1 ,supervisor-1:start-child permanent 1000 worker))
  (match-let*
   ([#(ok ,pid) (start 'one-for-one 3 100 (list child1))])
   (do ([i 0 (+ i 1)]) ((= i 5))
     (match-let*
      ([(#(child1 ,cpid1 worker)) (which-children 'sup-test)])
      (send cpid1 'die)
      (sleep-ms 50)))))

(supervisor-mat watcher ()
  (capture-events)
  (match-let*
   ([#(ok ,pid) (watcher:start&link #f)]
    [#(ok ,cpid)
     (watcher:start-child pid 'child 1000 supervisor-1:start-child)])

   (send cpid 'stop)
   (sleep-ms 100)

   (match-let*
    ([() (which-children pid)]
     [(0 0 0 0) (get-child-counts pid)])

    (receive
     (after 0 (throw 'no-watcher-child-log))
     [`(<child-end> [pid ,@cpid] [killed 0] [reason normal] [details #f])
      'ok]))))

(supervisor-mat watcher-shutdown ()
  (match-let*
   ([#(ok ,pid) (watcher:start&link #f)]
    [#(ok ,cpid) (watcher:start-child pid 'child 1000 supervisor-1:start-child)]
    [,_ (link cpid)])
   (watcher:shutdown-children pid)
   (receive (after 5000 (throw 'no-exit-reason))
     [`(EXIT ,@cpid shutdown) 'ok]
     [`(EXIT ,@cpid ,reason ,e) (throw `#(bad-exit-reason ,reason) e)])
   (match-let*
    ([() (which-children pid)]
     [(0 0 0 0) (get-child-counts pid)])
    'ok)))

(start-silent-event-mgr)
