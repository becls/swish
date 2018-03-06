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

(library (swish gen-server)
  (export
   define-state-tuple
   gen-server:call
   gen-server:cast
   gen-server:debug
   gen-server:reply
   gen-server:start
   gen-server:start&link
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish event-mgr-notify)
   (swish events)
   )

  (define-tuple <gen-server-interface>
    init handle-call handle-cast handle-info terminate debug)

  (define-syntax (gen-server:start x)
    (syntax-case x ()
      [(k name arg ...)
       (with-implicit (k init handle-call handle-cast handle-info terminate)
         #'(start name
             (<gen-server-interface> make
               [init init]
               [handle-call handle-call]
               [handle-cast handle-cast]
               [handle-info handle-info]
               [terminate terminate]
               [debug #f])
             (list arg ...)))]))

  (define-syntax (gen-server:start&link x)
    (syntax-case x ()
      [(k name arg ...)
       (with-implicit (k init handle-call handle-cast handle-info terminate)
         #'(start&link name
             (<gen-server-interface> make
               [init init]
               [handle-call handle-call]
               [handle-cast handle-cast]
               [handle-info handle-info]
               [terminate terminate]
               [debug #f])
             (list arg ...)))]))

  (define (start name iface init-args)
    (let* ([thunk (make-thunk self name (make-starter name iface init-args))]
           [pid (spawn thunk)])
      (let ([m (monitor pid)])
        (receive
         [#(ack ,@pid ,reply)
          (demonitor&flush m)
          reply]
         [#(DOWN ,@m ,_ ,reason)
          `#(error ,reason)]))))

  (define (start&link name iface init-args)
    (let* ([thunk (make-thunk self name (make-starter name iface init-args))]
           [pid (spawn&link thunk)])
      (receive
       [#(ack ,@pid ,reply) reply]
       [#(EXIT ,@pid ,reason) `#(error ,reason)])))

  (define (make-starter name iface init-args)
    (lambda (reply parent)
      (match (catch (do-init iface init-args))
        [#(ok ,state)
         (reply `#(ok ,self))
         (loop parent (or name self) iface state 'infinity)]
        [#(ok ,state ,timeout)
         (reply `#(ok ,self))
         (loop parent (or name self) iface state (resolve-timeout timeout))]
        [#(stop ,reason)
         (reply `#(error ,reason))
         (raise reason)]
        [ignore
         (reply 'ignore)
         (raise 'normal)]
        [#(EXIT ,reason)
         (reply `#(error ,reason))
         (raise reason)]
        [,other
         (let ([reason `#(bad-return-value ,other)])
           (reply `#(error ,reason))
           (raise reason))])))

  (define (make-thunk parent name proc)
    (lambda ()
      (define (reply x)
        (send parent `#(ack ,self ,x)))
      (if (not name)
          (proc reply parent)
          (match (catch (register name self))
            [#(EXIT ,reason) (reply `#(error ,reason))]
            [#t (proc reply parent)]))))

  (define (resolve-timeout x)
    (if (and (fixnum? x) (fx<= 0 x 86400000)) ; one day
        (+ (erlang:now) x)
        x))

  (define (loop parent name iface state timeout)
    (handle-msg (receive (until timeout 'timeout) [,msg msg])
      parent name iface state timeout))

  (define (handle-msg msg parent name iface state timeout)
    (match msg
      [#(EXIT ,@parent ,reason)
       (terminate reason name msg iface state)]
      [#($gen-call ,from ,msg)
       (match (catch (do-handle-call iface msg from state))
         [#(reply ,reply0 ,new-state)
          (gen-server:reply from reply0)
          (loop parent name iface new-state 'infinity)]
         [#(reply ,reply0 ,new-state ,timeout)
          (gen-server:reply from reply0)
          (loop parent name iface new-state (resolve-timeout timeout))]
         [#(stop ,reason ,reply0 ,new-state)
          (let ([r (terminate-reason reason name msg iface new-state)])
            (gen-server:reply from reply0)
            (raise r))]
         [,other
          (handle-common-reply other parent name msg iface state)])]
      [#($gen-cast ,msg)
       (let ([reply (catch (do-handle-cast iface msg state))])
         (handle-common-reply reply parent name msg iface state))]
      [#($gen-debug ,debug)
       (loop parent name (<gen-server-interface> copy iface [debug debug])
         state timeout)]
      [,msg
       (let ([reply (catch (do-handle-info iface msg state))])
         (handle-common-reply reply parent name msg iface state))]))

  (define (handle-common-reply reply parent name msg iface state)
    (match reply
      [#(no-reply ,new-state)
       (loop parent name iface new-state 'infinity)]
      [#(no-reply ,new-state ,timeout)
       (loop parent name iface new-state (resolve-timeout timeout))]
      [#(stop ,reason ,new-state)
       (terminate reason name msg iface new-state)]
      [#(EXIT ,reason)
       (terminate reason name msg iface state)]
      [,_
       (terminate `#(bad-return-value ,reply) name msg iface state)]))

  (define (terminate reason name msg iface state)
    (raise (terminate-reason reason name msg iface state)))

  (define (terminate-reason reason name msg iface state)
    (match (catch (do-terminate iface reason state))
      [#(EXIT ,r)
       (report-terminating r name msg state)
       r]
      [,_
       (unless (or (eq? reason 'normal) (eq? reason 'shutdown))
         (report-terminating reason name msg state))
       reason]))

  (define (report-terminating reason name msg state)
    (system-detail <gen-server-terminating>
      [name name]
      [last-message msg]
      [state state]
      [reason reason]))

  (define gen-server:call
    (case-lambda
     [(server request timeout)
      (match (catch (do-call server request timeout))
        [#(ok ,res) res]
        [#(EXIT ,reason)
         (raise `#(,reason #(gen-server call (,server ,request ,timeout))))])]
     [(server request)
      (match (catch (do-call server request 5000))
        [#(ok ,res) res]
        [#(EXIT ,reason)
         (raise `#(,reason #(gen-server call (,server ,request))))])]))

  (define-syntax no-interrupts
    (syntax-rules ()
      [(_ body ...)
       (let ([x (begin (disable-interrupts) body ...)])
         (enable-interrupts)
         x)]))

  (define debug-table (make-weak-eq-hashtable))

  (define (do-call server request timeout)
    (let* ([pid (if (symbol? server)
                    (or (whereis server) (raise 'no-process))
                    server)]
           [m (monitor pid)]
           [debug (no-interrupts (eq-hashtable-ref debug-table pid #f))]
           [start (and debug (erlang:now))])
      (send pid `#($gen-call #(,self ,m) ,request))
      (receive
       (after timeout
         (demonitor&flush m)
         (when debug
           (debug-report 6 debug start self pid request #f 'timeout))
         (raise 'timeout))
       [#(,@m ,reply)
        (demonitor&flush m)
        (when debug
          (debug-report 5 debug start self pid request #f reply))
        `#(ok ,reply)]
       [#(DOWN ,@m ,_ ,reason)
        (when debug
          (debug-report 6 debug start self pid request #f reason))
        (raise reason)])))

  (define (gen-server:cast server request)
    (catch (send server `#($gen-cast ,request)))
    'ok)

  (define (gen-server:debug server server-options client-options)
    (define (check-options options)
      (unless (or (not options) (list? options))
        (bad-arg 'gen-server:debug options)))
    (let ([pid (if (symbol? server)
                   (or (whereis server) (raise 'no-process))
                   server)])
      (check-options server-options)
      (check-options client-options)
      (send pid `#($gen-debug ,server-options))
      (no-interrupts
       (if client-options
           (eq-hashtable-set! debug-table pid client-options)
           (eq-hashtable-delete! debug-table pid)))
      'ok))

  (define (gen-server:reply client reply)
    (match client
      [#(,pid ,tag)
       (send pid `#(,tag ,reply))
       'ok]))

  (define-syntax (define-state-tuple x)
    (syntax-case x ()
      [(k1 name field ...)
       (with-implicit (k1 $state)
         #'(begin
             (define-tuple name field ...)
             (define-syntax ($state x)
               (syntax-case x ()
                 [(k2 op arg (... ...))
                  (with-implicit (k2 state)
                    #'(name op state arg (... ...)))]))))]))

  (define (debug-report type debug start client server message state reply)
    (system-detail <gen-server-debug>
      [timestamp start]
      [duration (- (erlang:now) start)]
      [type type]
      [client client]
      [server server]
      [message (and (memq 'message debug) message)]
      [state (and (memq 'state debug) state)]
      [reply (and (memq 'reply debug) reply)])
    reply)

  (define (do-init iface init-args)
    (apply (<gen-server-interface> init iface) init-args))

  (define (do-handle-call iface msg from state)
    (match-let* ([`(<gen-server-interface> ,handle-call ,debug) iface])
      (if debug
          (let ([start (erlang:now)])
            (debug-report 1 debug start (vector-ref from 0) self msg state
              (handle-call msg from state)))
          (handle-call msg from state))))

  (define (do-handle-cast iface msg state)
    (match-let* ([`(<gen-server-interface> ,handle-cast ,debug) iface])
      (if debug
          (let ([start (erlang:now)])
            (debug-report 2 debug start #f self msg state
              (handle-cast msg state)))
          (handle-cast msg state))))

  (define (do-handle-info iface msg state)
    (match-let* ([`(<gen-server-interface> ,handle-info ,debug) iface])
      (if debug
          (let ([start (erlang:now)])
            (debug-report 3 debug start #f self msg state
              (handle-info msg state)))
          (handle-info msg state))))

  (define (do-terminate iface reason state)
    (match-let* ([`(<gen-server-interface> ,terminate ,debug) iface])
      (if debug
          (let ([start (erlang:now)])
            (debug-report 4 debug start #f self #f state
              (terminate reason state)))
          (terminate reason state))))
  )
