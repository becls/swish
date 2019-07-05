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
      [process-trap-exit
       (process-trap-exit #t)
       `#(ok ())]
      [#(state ,state) `#(ok ,state)]))
  (define (terminate reason state)
    (match reason
      [#(,from stopped)
       (send from `#(,self stopped))]
      [#(,from stopped-info)
       (send from `#(,self stopped-info))]
      [crash-terminate
       (raise 'crashed)]
      [,_ (void)])
    'ok)
  (define (handle-call msg from state)
    (match msg
      [started? `#(reply ok ,state)]
      [no-answer `#(no-reply ,state)]
      [#(delayed-answer ,t) `#(no-reply #(reply-to ,from) ,t)]
      [indirect-reply
       (gen-server:reply from 'indirect-reply)
       `#(no-reply ())]
      [#(call-within ,t) `#(reply ok call-within ,t)]
      [next-call
       (match state
         [call-within `#(reply ok ())]
         [,_ `#(reply #f ())])]
      [bad-return 'bad-return]
      [stop `#(stop stopped ok ,state)]
      [crash (raise 'crashed)]
      [crash-terminate `#(stop crash-terminate ok ,state)]
      [shutdown-reason (raise `#(shutdown reason))]))
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
          `#(no-reply ())]
         [call-within
          `#(no-reply ())]
         [#(delayed-cast ,from)
          (send from `#(,self delayed))
          `#(no-reply ())]
         [#(delayed-info ,from)
          (send from `#(,self delayed-info))
          `#(no-reply ())])]
      [#(,from handle-info)
       (send from `#(,self handled-info))
       `#(no-reply ())]
      [#(,from delayed-info ,t)
       `#(no-reply #(delayed-info ,from) ,t)]
      [#(,from stop)
       `#(stop #(,from stopped-info) ,state)]
      [,_
       `#(no-reply ,state)]))
  (define (start-server name arg)
    (gen-server:start name arg))
  (define (start&link-server name arg)
    (gen-server:start&link name arg))
  )

(define (busy-wait-for-process pid n)
  (if (process-alive? pid n)
      (raise `#(process-still-alive ,pid))
      'ok))

(isolate-mat start ()
  (define (with-monitor thunk)
    (let ([m (monitor (spawn thunk))])
      (receive (after 5000 (raise 'timeout))
        [#(DOWN ,@m ,_ ,reason) reason])))
  (define (init-does-not-return starter)
    (with-monitor
     (lambda ()
       (define dog
         (spawn&link
          (lambda ()
            (receive (after 4000 (raise 'tester-timeout))
              [#(no-return ,from)
               (kill from 'bark!)]))))
       (raise (starter #f `#(no-return ,dog))))))
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

(isolate-mat stop ()
  (process-trap-exit #t)
  (match-let*
   ([#(ok ,pid) (start&link-server #f 'process-trap-exit)])
   (kill pid 'shutdown)
   (receive (after 5000 (raise 'not-stopped))
     [#(EXIT ,@pid shutdown) 'ok])))

(isolate-mat crash ()
  (process-trap-exit #t)
  (match-let*
   ([#(ok ,pid) (start&link-server #f '())]
    [#(EXIT #(#(shutdown reason) ,_))
     (catch (gen-server:call pid 'shutdown-reason))]
    [ok (receive [#(EXIT ,@pid #(shutdown reason)) 'ok])])
   'ok))

(isolate-mat crash2 ()
  (capture-events)
  (process-trap-exit #f)
  (match-let*
   ([#(ok ,pid) (start-server #f `#(state crash-state))]
    [#(EXIT #(crashed ,_)) (catch (gen-server:call pid 'crash))])

   (receive
    [`(<gen-server-terminating> [name ,@pid]
        [last-message crash]
        [state crash-state]
        [reason crashed])
     (void)]
    [,other (raise `#(unexpected-event ,other))])

   (receive (after 500 (void))
     [,other (raise `#(unexpected-event ,other))])
   'ok))

(isolate-mat crash3 ()
  (capture-events)
  (match-let*
   ([#(ok ,pid) (start-server #f `#(state crash-state))]
    [ok (gen-server:call pid 'crash-terminate)])

   (receive
    [`(<gen-server-terminating> [name ,@pid]
        [last-message crash-terminate]
        [state crash-state]
        [reason crashed])
     (void)]
    [,other (raise `#(unexpected-event ,other))])

   (receive (after 500 (void))
     [,other (raise `#(unexpected-event ,other))])
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
     (catch (gen-server:call 'non-existent-server 'started?))])
   'ok))

(isolate-mat cast ()
  (match-let*
   ([#(ok ,pid) (start-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]

    [ok (gen-server:cast 'my-test-name `#(,self handle-cast))]
    [ok (receive (after 1000 (raise 'handle-cast))
          [#(,@pid handled-cast) 'ok])]

    [ok (gen-server:cast 'my-test-name `#(,self delayed-cast 1))]
    [ok (receive (after 1000 (raise 'delayed-cast))
          [#(,@pid delayed) 'ok])]

    [ok (gen-server:cast 'my-test-name `#(,self stop))]
    [ok (receive (after 1000 (raise 'stop))
          [#(,@pid stopped) 'ok])])
   'ok))

(isolate-mat info ()
  (match-let*
   ([#(ok ,pid) (start-server 'my-test-name '())]
    [ok (gen-server:call 'my-test-name 'started?)]

    [,_ (send 'my-test-name `#(,self handle-info))]
    [ok (receive (after 1000 (raise 'handle-info))
          [#(,@pid handled-info) 'ok])]

    [,_ (send 'my-test-name `#(,self delayed-info 1))]
    [ok (receive (after 1000 (raise 'delayed-info))
          [#(,@pid delayed-info) 'ok])]

    [,_ (send 'my-test-name `#(,self stop))]
    [ok (receive (after 1000 (raise 'stop))
          [#(,@pid stopped-info) 'ok])])
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
