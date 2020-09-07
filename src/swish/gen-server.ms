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
 (swish testing)
 )

(module (start-server start&link-server)
  (define (init arg)
    (match arg
      [() `#(ok ())]
      [ignore 'ignore]
      [stop `#(stop stopped)]
      [bad-return 'bad-return]
      [#(no-return ,from)
       (send from `#(no-return ,self))
       (receive)]
      [crash (raise 'crashed)]
      [throw (throw 'fit)]
      [native-exception (cdr 7)]
      [process-trap-exit
       (process-trap-exit #t)
       `#(ok ())]
      [#(state ,state) `#(ok ,state)]
      [(kill ,pid ,reason ,block?)
       (kill pid reason) (if block? (receive) `#(ok ()))]))
  (define (terminate reason state)
    (match state
      [#(,from report-termination)
       (send from `#(,self terminated ,reason))]
      [,_ (void)])
    (match reason
      [#(,from stopped)
       (send from `#(,self stopped))]
      [#(,from stopped-info)
       (send from `#(,self stopped-info))]
      [crash-terminate
       (raise 'crashed)]
      [throw-terminate
       (throw 'terminated)]
      [,_ (void)])
    'ok)
  (define (handle-call msg from state)
    (match msg
      [started? `#(reply ok ,state)]
      [no-answer `#(no-reply ,state)]
      [#(delayed-answer ,t) `#(no-reply #(reply-to ,from) ,t)]
      [indirect-reply
       (gen-server:reply from 'indirect-reply)
       `#(no-reply ,state)]
      [#(call-within ,t) `#(reply ok call-within ,t)]
      [next-call
       (match state
         [call-within `#(reply ok call-within-next-call)]
         [,_ `#(reply #f other-next-call)])]
      [bad-return 'bad-return]
      [stop `#(stop stopped ok ,state)]
      [crash (raise 'crashed)]
      [throw (throw 'fit)]
      [return-fault `#(reply ,(make-fault 'boom) ,state)]
      [native-exception (car 'apple)]
      [crash-terminate `#(stop crash-terminate ok ,state)]
      [throw-terminate `#(stop throw-terminate ok ,state)]
      [shutdown-reason (raise `#(shutdown reason))]
      [shutdown-during-call `#(stop shutdown ,state)]))
  (define (handle-cast msg state)
    (match msg
      [#(,from handle-cast)
       (send from `#(,self handled-cast))
       `#(no-reply ,state)]
      [#(,from delayed-cast ,t)
       `#(no-reply #(delayed-cast ,from) ,t)]
      [#(,from stop)
       `#(stop #(,from stopped) ,state)]))
  (define (handle-info msg state)
    (match msg
      [timeout
       (match state
         [#(reply-to ,from)
          (gen-server:reply from 'delayed)
          `#(no-reply ,state)]
         [call-within
          `#(no-reply call-within-hit-info)]
         [#(delayed-cast ,from)
          (send from `#(,self delayed))
          `#(no-reply ,state)]
         [#(delayed-info ,from)
          (send from `#(,self delayed-info))
          `#(no-reply ,state)])]
      [#(,from handle-info)
       (send from `#(,self handled-info))
       `#(no-reply ,state)]
      [#(,from delayed-info ,t)
       `#(no-reply #(delayed-info ,from) ,t)]
      [#(,from stop)
       `#(stop #(,from stopped-info) ,state)]
      [#(,from raise ,what)
       (send from `#(,self raising ,what))
       (raise what)]
      [#(,from throw ,what)
       (send from `#(,self throwing ,what))
       (throw what)]
      [#(,from native-exception)
       (send from `#(,self causing-native-exception))
       (cdr 11)]
      [#(set-state ,new-state)
       `#(no-reply ,new-state)]
      [,_
       `#(no-reply ,state)]))
  (define (start-server name arg)
    (gen-server:start name arg))
  (define (start&link-server name arg)
    (gen-server:start&link name arg))
  )

(define (busy-wait-for-process pid n)
  (if (process-alive? pid n)
      (throw `#(process-still-alive ,pid))
      'ok))

(isolate-mat start ()
  (define (with-monitor thunk)
    (let ([m (monitor (spawn thunk))])
      (receive (after 5000 (throw 'timeout))
        [`(DOWN ,@m ,_ ,reason) reason])))
  (define (init-does-not-return starter)
    (with-monitor
     (lambda ()
       (define dog
         (spawn&link
          (lambda ()
            (receive (after 4000 (throw 'tester-timeout))
              [#(no-return ,from)
               (kill from 'bark!)]))))
       (throw (starter #f `#(no-return ,dog))))))
  (match-let*
   ;; anonymous
   ([#(ok ,pid) (start-server #f '())]
    [ok (gen-server:call pid 'started?)]
    [ok (gen-server:call pid 'stop)]
    [ok (busy-wait-for-process pid 600)]
    [#(EXIT #(stopped ,_)) (catch (gen-server:call pid 'started? 10))]
    ;; anonymous ignored
    [ignore (start-server #f 'ignore)]
    ;; anonymous stopped
    [#(error stopped) (start-server #f 'stop)]
    ;; crash init
    [#(error crashed) (start-server #f 'crash)]
    [#(error fit) (start-server #f 'throw)]
    ;; crash with native exception
    [#(error ,c)
     (guard
      (and (condition? c)
           (string=? (exit-reason->english c)
             "Exception in cdr: 7 is not a pair.")))
     (start-server #f 'native-exception)]
    ;; bad return value inside init
    [#(error #(bad-return-value bad-return)) (start-server #f 'bad-return)]
    ;; init does not return but we have the pid and can kill it.
    [#(error bark!) (init-does-not-return start-server)]
    [bark! (init-does-not-return start&link-server)]
    ;; anonymous &linked
    [stopped
     (with-monitor
      (lambda ()
        (match-let*
         ([#(ok ,pid) (start&link-server #f '())]
          [ok (gen-server:call pid 'started?)]
          [ok (gen-server:call pid 'stop)])
         (receive))))]
    ;; registered
    [#(ok ,pid) (start-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]
    [#(error #(name-already-registered ,@pid)) (start-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'stop)]
    [ok (busy-wait-for-process pid 600)]
    [#(EXIT #(stopped ,_)) (catch (gen-server:call pid 'started? 10))]
    ;; registered & linked
    [stopped
     (with-monitor
      (lambda ()
        (match-let*
         ([#(ok ,pid) (start&link-server 'my-test-name '())]
          [ok (gen-server:call 'my-test-name 'started?)]
          [#(error #(name-already-registered ,@pid)) (start-server 'my-test-name '())]
          [ok (gen-server:call 'my-test-name 'stop)])
         (receive))))])
   'ok))

(isolate-mat enter-loop ()
  (define (do-state-effect state)
    (match state
      [#(send ,who ,what)
       (send who what)
       state]
      [#(send-parent ,who)
       (send who `#(parent ,(process-parent)))
       state]))
  (define (terminate reason state) 'ok)
  (define (handle-call msg from state) (match msg))
  (define (handle-cast msg state) (match msg))
  (define (handle-info msg state)
    (match msg
      [timeout
       `#(no-reply ,(do-state-effect state))]
      [`(EXIT ,pid ,reason)
       `#(stop ,reason ,(do-state-effect state))]))

  (define (flush-msgs)
    (receive (after 10 '())
      [,msg (cons msg (flush-msgs))]))

  (define (test-with-parent name)
    (match-let*
     ([,me self]
      [,pid (spawn&link
             (lambda ()
               (process-trap-exit #t)
               (when name
                 (register name self))
               (send me `#(armed ,self))
               (gen-server:enter-loop '())))]
      [,pid (receive (after 1000 (throw 'timeout))
              [#(armed ,pid) pid])]
      [,_ (link pid)]
      [ok (receive (after 10 'ok))]     ; give it a moment to start
      [,_ (kill pid 'stopped)]
      [(`(EXIT ,@pid stopped)
        `(<gen-server-terminating>
          [name ,@name]
          [pid ,@pid]
          [last-message `(EXIT ,@me stopped)]
          [reason stopped]))
       (flush-msgs)])
     'ok))

  (define (test-without-parent name)
    (match-let*
     ([,me self]
      [,_ (spawn
           (lambda ()
             (spawn
              (lambda ()
                (process-trap-exit #t)
                (when name
                  (register name self))
                (do () ((not (process-parent))) (gc))
                (send me `#(armed ,self))
                (gen-server:enter-loop `#(send-parent ,me))))))]
      [,pid (receive (after 1000 (throw 'timeout))
              [#(armed ,pid) pid])]
      [,_ (link pid)]
      [ok (receive (after 10 'ok))]     ; give it a moment to start
      [,_ (kill pid 'stopped)]
      [(#(parent #f)
        `(EXIT ,@pid stopped)
        `(<gen-server-terminating>
          [name ,@name]
          [pid ,@pid]
          [last-message `(EXIT ,@me stopped)]
          [reason stopped]))
       (flush-msgs)])
     'ok))

  (capture-events)
  (process-trap-exit #t)
  (match-let*
   ([,me self]
    [ok (test-with-parent #f)]
    [ok (test-with-parent (gensym "unique"))]
    [ok (test-without-parent #f)]
    [ok (test-without-parent (gensym "unique"))]
    ;; timeout
    [,pid
     (spawn&link
      (lambda ()
        (gen-server:enter-loop `#(send ,me #(,self timer-fired)) 100)))]
    [ok (receive (after 1000 (throw 'timeout))
          [#(,@pid timer-fired) 'ok])]
    ;; on-exit runs
    [,pid
     (spawn&link
      (lambda ()
        (on-exit (send me `#(,self on-exit-fired))
          (process-trap-exit #t)
          (gen-server:enter-loop '()))))]
    [ok (receive (after 10 'ok))]       ; give it a moment to start
    [,_ (kill pid 'stopped)]
    [(#(,@pid on-exit-fired)
      `(EXIT ,@pid stopped)
      `(<gen-server-terminating> [pid ,@pid] [reason stopped]))
     (flush-msgs)])
   'ok))

(isolate-mat stop ()
  (process-trap-exit #t)
  (match-let*
   ([#(ok ,pid) (start&link-server #f 'process-trap-exit)])
   (kill pid 'shutdown)
   (receive (after 5000 (throw 'not-stopped))
     [`(EXIT ,@pid shutdown) 'ok])))

(isolate-mat crash ()
  (process-trap-exit #t)
  (match-let*
   ([#(ok ,pid) (start&link-server #f '())]
    [#(EXIT #(#(shutdown reason) ,_))
     (catch (gen-server:call pid 'shutdown-reason))]
    [ok (receive [`(EXIT ,@pid #(shutdown reason)) 'ok])])
   'ok))

(isolate-mat crash2 ()
  (capture-events)
  (process-trap-exit #f)
  (match-let*
   ([#(ok ,pid) (start-server #f `#(state crash-state))]
    [#(EXIT #(crashed ,_)) (catch (gen-server:call pid 'crash))])

   (receive
    [`(<gen-server-terminating> [pid ,@pid]
        [last-message crash]
        [state crash-state]
        [reason crashed]
        [details `(catch crashed)])
     (void)]
    [,other (throw `#(unexpected-event ,other))])

   (receive (after 500 (void))
     [,other (throw `#(unexpected-event ,other))])
   'ok))

(isolate-mat crash3 ()
  (capture-events)
  (match-let*
   ([#(ok ,pid) (start-server #f `#(state crash-state))]
    [ok (gen-server:call pid 'crash-terminate)])

   (receive
    [`(<gen-server-terminating> [pid ,@pid]
        [last-message crash-terminate]
        [state crash-state]
        [reason crashed])
     (void)]
    [,other (throw `#(unexpected-event ,other))])

   (receive (after 500 (void))
     [,other (throw `#(unexpected-event ,other))])
   'ok))

(isolate-mat crash4 ()
  (process-trap-exit #t)
  (capture-events)
  (match-let*
   ([#(error coverage1) (start&link-server #f `(kill ,self coverage1 #t))]
    [#(error coverage2) (start&link-server #f `(kill ,self coverage2 #f))]
    [ok (receive (after 100 'ok) [,x `(bad: got ,x)])])
   'ok))

(isolate-mat crash5 ()
  (capture-events)
  (process-trap-exit #f)
  (match-let*
   ([#(ok ,pid) (start-server #f `#(state ignorance))]
    [#(EXIT #(,c #(gen-server call (,@pid native-exception))))
     (guard
      (and (condition? c)
           (string=? "Exception in car: apple is not a pair."
             (exit-reason->english c))))
     (catch (gen-server:call pid 'native-exception))])

   (receive
    [`(<gen-server-terminating> [pid ,@pid]
        [last-message native-exception]
        [state ignorance]
        [reason exception]
        [details ,@c])
     (void)]
    [,other (throw `#(unexpected-event ,other))])

   (receive (after 500 (void))
     [,other (throw `#(unexpected-event ,other))])
   'ok))

(isolate-mat crash6 ()
  (capture-events)
  (process-trap-exit #f)
  (match-let*
   ([#(ok ,pid) (start-server #f `#(state bliss))]
    [#(EXIT #(fit #(gen-server call (,@pid throw))))
     (catch (gen-server:call pid 'throw))])

   (receive
    [`(<gen-server-terminating> [pid ,@pid]
        [last-message throw]
        [state bliss]
        [reason fit]
        [details `(catch fit)])
     (void)]
    [,other (throw `#(unexpected-event ,other))])

   (receive (after 500 (void))
     [,other (throw `#(unexpected-event ,other))])
   'ok))

(isolate-mat crash7 ()
  (capture-events)
  (match-let*
   ([#(ok ,pid) (start-server #f `#(state crash-state))]
    [ok (gen-server:call pid 'throw-terminate)])

   (receive
    [`(<gen-server-terminating> [pid ,@pid]
        [last-message throw-terminate]
        [state crash-state]
        [reason terminated]
        [details `(catch terminated)])
     (void)]
    [,other (throw `#(unexpected-event ,other))])

   (receive (after 500 (void))
     [,other (throw `#(unexpected-event ,other))])
   'ok))

(isolate-mat call ()
  (process-trap-exit #t)
  (match-let*
   ([#(ok ,pid) (start&link-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]
    [delayed (gen-server:call 'my-test-name `#(delayed-answer 1))]
    [indirect-reply (gen-server:call 'my-test-name 'indirect-reply)]

    ;; two requests within a specified time
    [ok (gen-server:call 'my-test-name `#(call-within 1000))]
    [ok (sleep-ms 500)]
    [ok (gen-server:call 'my-test-name 'next-call)]
    [ok (gen-server:call 'my-test-name `#(call-within 1000))]
    [ok (sleep-ms 1500)]
    [#f (gen-server:call 'my-test-name 'next-call)]

    ;; timeout
    [delayed (gen-server:call 'my-test-name
               `#(delayed-answer ,(+ (erlang:now) 1)) 30)]
    [#(EXIT #(timeout ,_))
     (catch (gen-server:call 'my-test-name
              `#(delayed-answer ,(+ (erlang:now) 30)) 1))]

    ;; bad return value inside gen-server loop
    [#(EXIT #(#(bad-return-value bad-return) ,_))
     (catch (gen-server:call 'my-test-name 'bad-return))]

    ;; no-process call
    [#(EXIT #(no-process #(gen-server call ,_)))
     (catch (gen-server:call 'non-existent-server 'started?))]

    ;; shutdown during call (with timeout)
    [#(ok ,pid) (start&link-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]
    [#(EXIT #(shutdown #(gen-server call (my-test-name shutdown-during-call 10))))
     (catch (gen-server:call 'my-test-name 'shutdown-during-call 10))]

    ;; shutdown during call (no timeout)
    [#(ok ,pid) (start&link-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]
    [#(EXIT #(shutdown #(gen-server call (my-test-name shutdown-during-call))))
     (catch (gen-server:call 'my-test-name 'shutdown-during-call))]

    ;; server returns a fault, and fault is thrown in caller
    [#(ok ,pid) (start&link-server 'my-test-name '())]
    [#(EXIT boom) (catch (gen-server:call 'my-test-name 'return-fault))])
   'ok))

(isolate-mat cast ()
  (match-let*
   ([#(ok ,pid) (start-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]

    [ok (gen-server:cast 'my-test-name `#(,self handle-cast))]
    [ok (receive (after 1000 (throw 'handle-cast))
          [#(,@pid handled-cast) 'ok])]

    [ok (gen-server:cast 'my-test-name `#(,self delayed-cast 1))]
    [ok (receive (after 1000 (throw 'delayed-cast))
          [#(,@pid delayed) 'ok])]

    [ok (gen-server:cast 'my-test-name `#(,self stop))]
    [ok (receive (after 1000 (throw 'stop))
          [#(,@pid stopped) 'ok])])
   'ok))

(isolate-mat info ()
  (match-let*
   ([#(ok ,pid) (start-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]

    [,_ (send 'my-test-name `#(,self handle-info))]
    [ok (receive (after 1000 (throw 'handle-info))
          [#(,@pid handled-info) 'ok])]

    [,_ (send 'my-test-name `#(,self delayed-info 1))]
    [ok (receive (after 1000 (throw 'delayed-info))
          [#(,@pid delayed-info) 'ok])]

    [,_ (send 'my-test-name `#(set-state #(,self report-termination)))]
    [,_ (send 'my-test-name `#(,self stop))]
    [ok (receive (after 1000 (throw 'stop))
          [#(,@pid stopped-info) 'ok])]
    [ok (receive (after 1000 (throw 'bad-termination-reason))
          [#(,@pid terminated #(,@self stopped-info)) 'ok])])
   'ok))

(isolate-mat terminate1 ()
  (capture-events)
  (match-let*
   ([#(ok ,pid)
     (start-server #f `#(state #(,self report-termination)))]
    [ok (gen-server:call pid 'started?)]
    [,_ (send pid `#(,self raise objection))]
    [ok (receive (after 100 (throw 'no-objection))
          [#(,@pid raising objection) 'ok])]
    [ok (receive (after 100 (throw 'no-termination))
          [#(,@pid terminated objection) 'ok])])
   (receive (after 1000 (throw 'no-event))
     [`(<gen-server-terminating>
        [pid ,@pid]
        [last-message #(,@self raise objection)]
        [state #(,@self report-termination)]
        [reason objection]) 'ok])
   'ok))

(isolate-mat terminate2 ()
  (capture-events)
  (match-let*
   ([#(ok ,pid)
     (start-server #f `#(state #(,self report-termination)))]
    [ok (gen-server:call pid 'started?)]
    [,_ (send pid `#(,self native-exception))]
    [ok (receive (after 100 (throw 'no-exception))
          [#(,@pid causing-native-exception) 'ok])]
    [,c (receive (after 100 (throw 'no-termination))
          [#(,@pid terminated ,c)
           (guard (condition? c))
           c])]
    ["Exception in cdr: 11 is not a pair."
     (exit-reason->english c)])
   (receive (after 1000 (throw 'no-event))
     [`(<gen-server-terminating>
        [pid ,@pid]
        [last-message #(,@self native-exception)]
        [state #(,@self report-termination)]
        [reason exception]
        ,details)
      (match (exit-reason->english details)
        ["Exception in cdr: 11 is not a pair." 'ok])])
   'ok))

(isolate-mat terminate3 ()
  (capture-events)
  (match-let*
   ([#(ok ,pid)
     (start-server #f `#(state #(,self report-termination)))]
    [ok (gen-server:call pid 'started?)]
    [,_ (send pid `#(,self throw heavy-object))]
    [ok (receive (after 100 (throw 'no-throw))
          [#(,@pid throwing heavy-object) 'ok])]
    [ok (receive (after 100 (throw 'no-termination))
          [#(,@pid terminated heavy-object) 'ok])])
   (receive (after 1000 (throw 'no-event))
     [`(<gen-server-terminating>
        [pid ,@pid]
        [last-message #(,@self throw heavy-object)]
        [state #(,@self report-termination)]
        [reason heavy-object]) 'ok])
   'ok))

(isolate-mat debug ()
  (define (grab-event) (receive (after 100 #f) [,x x]))
  (define (client-test options)
    (match-let*
     ([#(ok ,pid) (start-server 'my-test-name '())]
      [ok (gen-server:debug 'my-test-name #f options)]
      [#(EXIT #(timeout #(gen-server call (my-test-name no-answer 100))))
       (catch (gen-server:call 'my-test-name 'no-answer 100))]
      [`(<gen-server-debug>
         [type 6]
         [client ,@self]
         [server ,@pid]
         ,message
         [state #f]
         ,reply)
       (guard (and (eq? message (and (memq 'message options) 'no-answer))
                   (eq? reply (and (memq 'reply options) 'timeout))))
       (grab-event)]
      [ok (gen-server:call 'my-test-name 'started?)]
      [`(<gen-server-debug>
         [type 5]
         [client ,@self]
         [server ,@pid]
         ,message
         [state #f]
         ,reply)
       (guard (and (eq? message (and (memq 'message options) 'started?))
                   (eq? reply (and (memq 'reply options) 'ok))))
       (grab-event)]
      [#(EXIT #(crashed #(gen-server call (my-test-name crash))))
       (catch (gen-server:call 'my-test-name 'crash))]
      [ok
       (receive (after 100 #f)
         [`(<gen-server-terminating> [name my-test-name]
             [last-message crash]
             [state ()]
             [reason crashed]) 'ok])]
      [`(<gen-server-debug>
         [type 6]
         [client ,@self]
         [server ,@pid]
         ,message
         [state #f]
         ,reply)
       (guard (and (eq? message (and (memq 'message options) 'crash))
                   (eq? reply (and (memq 'reply options) 'crashed))))
       (grab-event)]
      [#f (grab-event)])
     'ok))

  (define (server-test options)
    (match-let*
     ([#(ok ,pid) (start-server 'my-test-name '())]
      [ok (gen-server:debug 'my-test-name options #f)]
      [ok (gen-server:cast 'my-test-name `#(,self handle-cast))]
      [ok (receive (after 100 #f) [#(,@pid handled-cast) 'ok])]
      [`(<gen-server-debug>
         [type 2]
         [client #f]
         [server ,@pid]
         ,message
         ,state
         ,reply)
       (guard (and (equal? message
                     (and (memq 'message options) `#(,self handle-cast)))
                   (eq? state (and (memq 'state options) '()))
                   (equal? reply (and (memq 'reply options) '#(no-reply ())))))
       (grab-event)]
      [,_ (send pid 'boo)]
      [`(<gen-server-debug>
         [type 3]
         [client #f]
         [server ,@pid]
         ,message
         ,state
         ,reply)
       (guard (and (eq? message (and (memq 'message options) 'boo))
                   (eq? state (and (memq 'state options) '()))
                   (equal? reply (and (memq 'reply options) '#(no-reply ())))))
       (grab-event)]
      [ok (gen-server:call 'my-test-name 'stop)]
      [ok
       (receive (after 100 #f)
         [`(<gen-server-terminating> [name my-test-name]
             [last-message stop]
             [state ()]
             [reason stopped]) 'ok])]
      [`(<gen-server-debug>
         [type 1]
         [client ,@self]
         [server ,@pid]
         ,message
         ,state
         ,reply)
       (guard (and (eq? message (and (memq 'message options) 'stop))
                   (eq? state (and (memq 'state options) '()))
                   (equal? reply (and (memq 'reply options)
                                      '#(stop stopped ok ())))))
       (grab-event)]
      [`(<gen-server-debug>
         [type 4]
         [client #f]
         [server ,@pid]
         [message #f]
         ,state
         ,reply)
       (guard (and (eq? state (and (memq 'state options) '()))
                   (eq? reply (and (memq 'reply options) 'ok))))
       (grab-event)]
      [#f (grab-event)])
     'ok))

  (capture-events)
  (match-let*
   ([#(EXIT no-process) (catch (gen-server:debug 'foo #f #f))]
    [#(EXIT #(bad-arg gen-server:debug 12))
     (catch (gen-server:debug self 12 #f))]
    [#(EXIT #(bad-arg gen-server:debug 13))
     (catch (gen-server:debug self #f 13))]
    [ok (client-test '())]
    [ok (client-test '(message))]
    [ok (client-test '(reply))]
    [ok (client-test '(message reply))]
    [ok (server-test '())]
    [ok (server-test '(message))]
    [ok (server-test '(state))]
    [ok (server-test '(message state))]
    [ok (server-test '(reply))]
    [ok (server-test '(message reply))]
    [ok (server-test '(state reply))]
    [ok (server-test '(message state reply))])
   'ok))

(start-silent-event-mgr)
