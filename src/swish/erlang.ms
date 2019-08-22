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

#!chezscheme
(import
 (chezscheme)
 (swish erlang)
 (swish gatekeeper)
 (swish gen-server)
 (swish io)
 (swish mat)
 (swish osi)
 (swish string-utils)
 (swish testing)
 )

(define-syntax assert-bad-match
  (syntax-rules ()
    [(_ v e) (assert-bad-match v ,_ e)]
    [(_ v expected-src e)
     (match (catch e)
       [#(EXIT #(bad-match ,x ,actual-src))
        (guard (and (equal? v x) (match-src expected-src actual-src)))
        #t]
       [#(EXIT #(bad-match ,x ,actual-src)) (guard (equal? v x))
        (errorf 'assert-bad-match "expected to raise bad-match with source ~s but got ~s" 'expected-src actual-src)]
       [,x
        (errorf 'assert-bad-match "failed to raise bad-match: ~s" x)])]))

(define-syntax assert-error
  (syntax-rules ()
    [(_ type e) (assert-error type ,_ e)]
    [(_ type expected-src e)
     (match (catch e)
       [#(EXIT #(type ,name ,val expected-src)) #t]
       [#(EXIT #(type ,name ,val ,actual-src))
        (guard (match-src expected-src actual-src))
        #t]
       [#(EXIT #(type ,name ,val ,actual-src))
        (errorf 'assert-error "expected to raise ~a with source ~s but got ~s" 'type 'expected-src actual-src)]
       [,x
        (errorf 'assert-error "failed to raise ~a: ~s" 'type x)])]))

(define-syntax (match-src x)
  (syntax-case x ()
    [(? expected-src actual-src)
     (equal? ',_ (datum expected-src))
     #'#t]
    [(? expected-src actual-src)
     #'(match-let* ([#(at ,_ ,expected-filename) 'expected-src])
         (match actual-src
           [#(at ,_ ,actual-filename)
            (or (equal? expected-filename actual-filename)
                (equal? expected-filename (join (split actual-filename #\\) "/")))]
           [,_ #f]))]))

(define (pid? x)
  (and (exact? x) (integer? x) (positive? x)))

(define (repl-test expected-exit code . patterns)
  (define scheme-args
    (list
     "--eedisable"
     "--program" "repl.ss" "-q"))
  (define (write-stdin op)
    (fprintf op "~s\n" code)
    (flush-output-port op))
  (define (launch)
    (match (vector->list (osi_get_argv))
      [()
       (parameterize ([cd "src"])
         (run-os-process scheme-exe scheme-args write-stdin 10000 '()))]
      [(,command . ,args)
       (run-os-process command args write-stdin 10000 '())]))
  (match (launch)
    [`(<os-result> ,stdout ,stderr ,exit-status)
     (unless (eqv? expected-exit exit-status)
       (raise `#(repl-test-unexpected-exit ,expected-exit ,exit-status)))
     (match-regexps patterns (append stdout stderr))]))

(isolate-mat many-fib28 ()
  (define n 200)
  (define (fib n)
    (if (< n 2)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))
  (process-trap-exit #t)
  (do ([n n (- n 1)]) ((= n 0))
    (spawn&link (lambda () (match-let* ([514229 (fib 28)]) 'ok))))
  (do ([n n (- n 1)]) ((= n 0))
    (receive
     [#(EXIT ,_ normal) 'ok]
     [#(EXIT ,_ ,reason) (raise reason)])))

(isolate-mat id-generator ()
  (define n 200000)
  (define (id-generator:start&link) (gen-server:start&link 'id-generator))
  (define (id-generator:get) (gen-server:call 'id-generator 'get))
  (define (init) `#(ok 1))
  (define (terminate reason next) 'ok)
  (define (handle-call msg from next)
    (match msg
      [get `#(reply ,next ,(+ next 1))]))
  (define (handle-cast msg next)
    `#(no-reply ,next))
  (define handle-info handle-cast)
  (id-generator:start&link)
  (do ([i 1 (+ i 1)]) ((> i n) 'ok)
    (match-let* ([,@i  (id-generator:get)])
      'ok)))

(isolate-mat suicide ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (kill self 'live-free-or-die-hard)))])
    (receive
     (after 1000 (raise 'timeout))
     [#(EXIT ,@pid live-free-or-die-hard) 'ok])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (raise 'no-exit-from-link))
     [#(EXIT ,@pid live-free-or-die-hard) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (raise 'link-no-fail)))])
      (receive
       (after 10 (raise 'no-exit-from-link))
       [#(EXIT ,@pid2 live-free-or-die-hard) 'ok]))))

(isolate-mat suicide-normal ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (kill self 'normal)))])
    (receive
     (after 1000 (raise 'timeout))
     [#(EXIT ,@pid normal) 'ok])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (raise 'no-exit-from-link))
     [#(EXIT ,@pid normal) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (raise 'link-no-fail)))])
      (receive
       (after 10 (raise 'timeout))
       [#(EXIT ,@pid2 link-no-fail) 'ok]))))

(isolate-mat default-parameters ()
  (match-let*
   ([#f (process-trap-exit)]
    [,x (guard (pid? x)) (process-id)]
    [,@x (process-id self)])
   'ok))

(isolate-mat bad-args ()
  (match-let*
   ([#(EXIT #(bad-arg add-finalizer 12)) (catch (add-finalizer 12))]
    [#(EXIT #(bad-arg custom-port-buffer-size -1))
     (catch (custom-port-buffer-size -1))]
    [#(EXIT #(bad-arg demonitor 12)) (catch (demonitor 12))]
    [#(EXIT #(bad-arg exit-handler 12)) (catch (exit-handler 12))]
    [#(EXIT #(bad-arg uuid->string #f)) (catch (uuid->string #f))]
    [#(EXIT #(bad-arg uuid->string #vu8(1 2 3)))
     (catch (uuid->string #vu8(1 2 3)))]
    [#(EXIT #(bad-arg kill 12)) (catch (kill 12 'crash))]
    [#(EXIT #(bad-arg link 12)) (catch (link 12))]
    [#(EXIT #(bad-arg make-process-parameter 12))
     (catch (make-process-parameter 0 12))]
    [#(EXIT #(bad-arg monitor 12)) (catch (monitor 12))]
    [#(EXIT #(bad-arg pps 12)) (catch (pps 12))]
    [#(EXIT #(bad-arg pretty-initial-indent -1))
     (catch (pretty-initial-indent -1))]
    [#(EXIT #(bad-arg pretty-line-length 0)) (catch (pretty-line-length 0))]
    [#(EXIT #(bad-arg pretty-maximum-lines -1))
     (catch (pretty-maximum-lines -1))]
    [#(EXIT #(bad-arg pretty-one-line-limit 0))
     (catch (pretty-one-line-limit 0))]
    [#(EXIT #(bad-arg pretty-standard-indent -1))
     (catch (pretty-standard-indent -1))]
    [#(EXIT #(bad-arg print-length -1)) (catch (print-length -1))]
    [#(EXIT #(bad-arg print-level -1)) (catch (print-level -1))]
    [#(EXIT #(bad-arg print-precision #t)) (catch (print-precision #t))]
    [#(EXIT #(bad-arg print-radix 1)) (catch (print-radix 1))]
    [#(EXIT #(bad-arg process-id 12)) (catch (process-id 12))]
    [#(EXIT #(bad-arg register 12)) (catch (register (gensym) 12))]
    [#(EXIT #(bad-arg register 12)) (catch (register 12 'bad))]
    [#(EXIT #(bad-arg reset-handler 12)) (catch (reset-handler 12))]
    [#(EXIT #(bad-arg send 12)) (catch (send 12 'msg))]
    [#(EXIT #(bad-arg spawn 12)) (catch (spawn 12))]
    [#(EXIT #(bad-arg spawn&link 12)) (catch (spawn&link 12))]
    [#(EXIT #(bad-arg string->uuid #f)) (catch (string->uuid #f))]
    [#(EXIT #(bad-arg string->uuid "498e2279-1301-421e-9a2e-4001acf565e"))
     (catch (string->uuid "498e2279-1301-421e-9a2e-4001acf565e"))]
    [#(EXIT #(bad-arg string->uuid "498e2279x1301-421e-9a2e-4001acf565e6"))
     (catch (string->uuid "498e2279x1301-421e-9a2e-4001acf565e6"))]
    [#(EXIT #(bad-arg string->uuid "498e2279-1301x421e-9a2e-4001acf565e6"))
     (catch (string->uuid "498e2279-1301x421e-9a2e-4001acf565e6"))]
    [#(EXIT #(bad-arg string->uuid "498e2279-1301-421ex9a2e-4001acf565e6"))
     (catch (string->uuid "498e2279-1301-421ex9a2e-4001acf565e6"))]
    [#(EXIT #(bad-arg string->uuid "498e2279-1301-421e-9a2ex4001acf565e6"))
     (catch (string->uuid "498e2279-1301-421e-9a2ex4001acf565e6"))]
    [#(EXIT #(bad-arg string->uuid "498e2279-1301-421e-9a2e-4001acf565eg"))
     (catch (string->uuid "498e2279-1301-421e-9a2e-4001acf565eg"))]
    [#(EXIT #(bad-arg string->uuid "498e2279-1301-421e-9a2ex4001acf565e67"))
     (catch (string->uuid "498e2279-1301-421e-9a2ex4001acf565e67"))]
    [#(EXIT #(bad-arg unlink 12)) (catch (unlink 12))]
    [#(EXIT #(bad-arg unregister 12)) (catch (unregister 12))]
    [#(EXIT #(bad-arg waiter-prompt-and-read 12))
     (catch (waiter-prompt-and-read 12))]
    [#(EXIT #(bad-arg whereis 12)) (catch (whereis 12))]
    [#(EXIT #(bad-arg who arg)) (catch (bad-arg 'who 'arg))])
   (print-char-name #t)
   (print-gensym 'pretty)
   (print-graph #t)
   (print-vector-length #t)
   'ok))

(isolate-mat registration ()
  (define a-name (gensym "A"))
  (define b-name (gensym "B"))
  (define (A)
    (receive
     [,msg
      (send b-name msg)
      (A)]))
  (define (B)
    (receive
     [#(ping ,who)
      (send who 'pong)
      (B)]))
  (process-trap-exit #t)
  (let ([a-pid (spawn&link A)]
        [b-pid (spawn&link B)]
        [c-pid (spawn void)])
    (register a-name a-pid)
    (register b-name b-pid)
    (monitor c-pid)
    (receive [#(DOWN ,_ ,@c-pid ,_) 'ok])
    (match-let*
     ([#(EXIT #(process-already-registered ,@a-name))
       (catch (register (gensym "A") a-pid))]
      [#(EXIT #(name-already-registered ,@a-pid))
       (catch (register a-name (spawn void)))]
      [#(EXIT #(process-dead ,@c-pid)) (catch (register (gensym "C") c-pid))]
      [,@a-pid (whereis a-name)]
      [,@b-pid (whereis b-name)]
      [,names (get-registered)])
     (assert (find (lambda (x) (eq? x a-name)) names))
     (assert (find (lambda (x) (eq? x b-name)) names))
     (send a-name `#(ping ,self))
     (receive
      (after 100 (raise 'timeout))
      [pong 'ok])
     (unregister a-name)
     (unregister b-name)
     (match-let*
      ([#(EXIT #(bad-arg unregister ,@a-name)) (catch (unregister a-name))]
       [#(EXIT #(bad-arg send ,@a-name)) (catch (send a-name `#(ping #f)))])
      'ok))))

(isolate-mat receive-after ()
  (define (try-after timeout)
    (match-let*
     ([#(EXIT #(timeout-value ,@timeout ,_))
       (catch (receive (after timeout 'ok)))])
     'ok))
  (try-after -1)
  (try-after 0.0)
  (try-after (acos -1))
  (try-after (/ 1 2))
  (try-after 'xxx))

(isolate-mat receive-until ()
  (define (try-until timeout)
    (match-let*
     ([#(EXIT #(timeout-value ,@timeout ,_))
       (catch (receive (until timeout 'ok)))])
     'ok))
  (try-until -1)
  (try-until 0.0)
  (try-until (acos -1))
  (try-until (/ 1 2))
  (try-until 'xxx))

(isolate-mat receive-big ()
  (define (start-after timeout)
    (spawn&link
     (lambda ()
       (receive (after timeout (raise 'timeout))))))
  (define (start-until timeout)
    (spawn&link
     (lambda ()
       (receive (until (+ (erlang:now) timeout) (raise 'timeout))))))
  (define (test-loop start)
    (let lp ([i 12] [pids '()])
      (cond
       [(< i 128)
        (lp (+ i 1) (cons (start (expt 2 i)) pids))]
       [else
        (receive (after 2000 'ok))
        (for-each
         (lambda (pid)
           (unlink pid)
           (kill pid 'kill))
         pids)])))
  (test-loop start-after)
  (test-loop start-until))

(isolate-mat receive-after-0 ()
  (send self 'x)
  (send self 'y)
  (match-let*
   ([timeout (receive (after 0 'timeout) [z 'ok])]
    [,_ (send self 'w)]
    [timeout (receive (after 0 'timeout))])
   (receive
    [x 'ok]
    [,other (raise `#(bad-message ,other))])))

(isolate-mat receive-until-0 ()
  (send self 'x)
  (send self 'y)
  (match-let*
   ([timeout (receive (until 0 'timeout) [z 'ok])]
    [,_ (send self 'w)]
    [timeout (receive (until 0 'timeout))])
   (receive
    [x 'ok]
    [,other (raise `#(bad-message ,other))])))

(isolate-mat panic-run-queue-empty ()
  (repl-test 80
    '(begin
       (osi_list_directory "." (lambda (x) (receive)))
       (receive))
    'seek
    "Event: run-queue-empty"))

(isolate-mat panic-event-loop-process-terminated ()
  (repl-test 80
    '(begin
       (osi_list_directory "." (lambda (x) (raise 'kaboom)))
       (receive))
    'seek
    "Event: #\\(event-loop-process-terminated kaboom\\)")
  (repl-test 80
    '(begin
       (osi_list_directory "." (lambda (x) (kill self 'kill)))
       (receive))
    'seek
    "Event: #\\(event-loop-process-terminated killed\\)"))

(isolate-mat panic-finalizer-process-terminated ()
  (add-finalizer
   (let ([go? #t])
     (lambda ()
       (when go?
         (collect)
         (collect)
         (set! go? #f)))))
  (collect)
  (repl-test 80
    '(begin
       (add-finalizer (lambda () (raise 'kaboom)))
       (collect)
       (receive))
    'seek
    "Event: #\\(finalizer-process-terminated kaboom\\)")
  (repl-test 80
    '(begin
       (add-finalizer (lambda () (kill self 'kill)))
       (collect)
       (receive))
    'seek
    "Event: #\\(finalizer-process-terminated killed\\)"))

(isolate-mat chain-reaction ()
  ;; A, B, C are linked. C dies for different reasons
  (define (start trap? m)
    (let ([host self])
      (define (wait)
        (receive
         [,any
          (send host `#(msg-from ,self ,any))
          (wait)]))
      (define (a)
        (process-trap-exit #t)
        (sync)
        (wait))
      (define (b a trap?)
        (process-trap-exit trap?)
        (link a)
        (sync)
        (wait))
      (define (c b m)
        (link b)
        (sync)
        (match m
          [#(die ,reason) (raise reason)]
          [#(divide ,n)
           (/ 1 n)
           (wait)]
          [normal #t]))
      (define (sync) (send host `#(sync ,self)))
      (let* ([A (spawn&link a)]
             [B (spawn&link (lambda () (b A trap?)))]
             [C (spawn&link (lambda () (c B m)))])
        (for-each
         (lambda (pid)
           (receive (after 1000 (raise 'timeout-syncing))
             [#(sync ,@pid) 'ok]))
         (list A B C))
        `#(,A ,B ,C))))
  (define (flush)
    (receive (after 0 'ok)
      [,_ (flush)]))
  (process-trap-exit #t)
  (match-let*
   ([#(,a ,b ,c) (start #f `#(die abc))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@a #(EXIT ,@b abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(die normal))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(divide 0))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@a #(EXIT ,@b ,cc))
           (guard (condition? cc))
           'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(die kill))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@a #(EXIT ,@b kill)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die abc))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@b #(EXIT ,@c abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die normal))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@b #(EXIT ,@c normal)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'normal)]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@b #(EXIT ,@c normal)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die kill))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@b #(EXIT ,@c kill)) 'ok])]
    [ok (flush)])
   'ok))

(isolate-mat chain-reaction2 ()
  ;; A, B, C are linked. C kills B for different reasons
  (define (start trap? m)
    (let ([host self])
      (define (wait)
        (receive
         [,any
          (send host `#(msg-from ,self ,any))
          (wait)]))
      (define (a)
        (process-trap-exit #t)
        (sync)
        (wait))
      (define (b a trap?)
        (process-trap-exit trap?)
        (link a)
        (sync)
        (wait))
      (define (c b m)
        (process-trap-exit #t)
        (link b)
        (sync)
        (kill b m)
        (wait))
      (define (sync) (send host `#(sync ,self)))
      (let* ([A (spawn&link a)]
             [B (spawn&link (lambda () (b A trap?)))]
             [C (spawn&link (lambda () (c B m)))])
        (for-each
         (lambda (pid)
           (receive (after 1000 (raise 'timeout-syncing))
             [#(sync ,@pid) 'ok]))
         (list A B C))
        `#(,A ,B ,C))))
  (define (flush)
    (receive (after 0 'ok)
      [,_ (flush)]))
  (process-trap-exit #t)
  (match-let*
   ([#(,a ,b ,c) (start #f 'abc)]
    [#f (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@a #(EXIT ,@b abc)) 'ok])]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@c #(EXIT ,@b abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f 'normal)]
    [#t (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (flush)]

    [#(,a ,b ,c) (start #f 'kill)]
    [#f (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@a #(EXIT ,@b killed)) 'ok])]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@c #(EXIT ,@b killed)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'abc)]
    [#t (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@b #(EXIT ,@c abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'normal)]
    [#t (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@b #(EXIT ,@c normal)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'kill)]
    [#f (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@a #(EXIT ,@b killed)) 'ok])]
    [ok (receive (after 1000 (raise 'timeout))
          [#(msg-from ,@c #(EXIT ,@b killed)) 'ok])]
    [ok (flush)])
   'ok))

(isolate-mat process-parameter ()
  (define skeleton (make-process-parameter #f))
  (define (spin)
    (spawn&link
     (lambda ()
       (match-let* ([#f (skeleton)])
         (skeleton 0)
         (do ([i 0 (+ i 1)]) ((= i 1000))
           (receive (until 0 #t))
           (skeleton (+ (skeleton) 1)))
         (assert (= (skeleton) 1000))))))
  (process-trap-exit #t)
  (for-each
   (lambda (pid)
     (receive
      (after 1000 (raise 'timeout))
      [#(EXIT ,@pid normal) 'ok]))
   (list (spin) (spin) (spin) (spin) (spin))))

(isolate-mat inherited-parameters ()
  (define (get p) (p))
  (parameterize ([inherited-parameters '()])
    (match-let*
     ([,ips (map (lambda (i) (make-inherited-parameter i)) (iota 10))]
      [ok (and (andmap (lambda (ip) (memq ip ips)) (inherited-parameters)) 'ok)]
      [ok (and (andmap (lambda (ip) (memq ip (inherited-parameters))) ips) 'ok)]
      [,me self]
      ;; parameters have the expected values in spawned process
      [,expected (map get ips)]
      [,pid (spawn&link (lambda () (send me `(,self ,@(map get ips)))))]
      [,@expected
       (receive (after 1000 (raise 'timeout))
         [(,@pid . ,rest) rest])]
      ;; changing parameters in child process does not affect parent parameters
      [,pid
       (spawn&link
        (lambda ()
          (for-each (lambda (p) (p (* (p) 10))) ips)
          ;; grandchild gets the new parameter values from child
          (let ([child (spawn&link (lambda () (send me `(,self ,@(map get ips)))))])
            (send me `(,self ,child)))))]
      [,@expected
       (receive (after 1000 (raise 'timeout))
         [(,@pid ,child)
          (receive (after 1000 (raise 'timeout))
            [(,@child . ,10x)
             (map (lambda (x) (/ x 10)) 10x)])])]
      ;; changing parameters in parent process does not affect child once spawned
      [,pid
       (spawn&link
        (lambda ()
          (receive (after 1000 (raise 'timeout))
            [ready (send me `(,self ,@(map get ips)))])))]
      [,@expected
       (begin
         (for-each (lambda (p) (p 'nut)) ips)
         (send pid 'ready)
         (receive (after 1000 (raise 'timeout))
           [(,@pid . ,vals) vals]))]
      [#10(nut) (list->vector (map get ips))])
     'ok))
  (let ([g1 (make-guardian)]
        [g2 (make-guardian)]
        [g3 (make-guardian)]
        [me self])
    ;; parameters and values can be collected
    (let* ([val (list->string '(#\a #\b #\c))]
           [default (list->string '(#\x #\y #\z))]
           [p (make-inherited-parameter default)])
      (g1 val)
      (g2 default)
      (g3 p)
      (match-let*
       ([,pid
         (parameterize ([p val])
           (spawn&link
            (lambda ()
              (let ([v (p)])
                (send me 'ready)
                ;; wait until first collect is done
                (receive (after 1000 'timeout)
                  [go (send me v)])))))]
        ["xyz" (p)]
        [ok (receive (after 1000 'timeout) [ready 'ok])]
        [,_ (set! p #f)]
        [,_ (inherited-parameters '())]
        [,_ (gc)]
        [(#f #f ,x) (list (g1) (g2) (g3))]
        [#t (procedure? x)]
        [,_ (gc)]
        [,_ (send pid 'go)]
        [ok (receive (after 1000 'timeout) ["abc" 'ok])]
        [,_ (gc)]
        [("abc" "xyz" #f) (list (g1) (g2) (g3))])
       'ok))))

(isolate-mat monitor ()
  (process-trap-exit #t)
  (let* ([host self]
         [pid (spawn&link
               (lambda ()
                 (if (monitor? (monitor host))
                     (raise 'success!)
                     (raise 'bad-monitor))))]
         [m (monitor pid)])
    (receive
     (after 10 (raise 'timeout))
     [#(DOWN ,@m ,@pid success!) 'ok])))

(isolate-mat pps ()
  (let-values ([(p get) (open-string-output-port)])
    (parameterize ([current-output-port p])
      (let ([dead (spawn (lambda () (car 12)))]
            [no-src (spawn (lambda () (eval '(receive (after 10 'ok)))))]
            [forever (spawn&link (lambda () (receive (until 'infinity 'ok))))])
        (register 'named self)
        (sleep-ms 1)
        (pps)))
    (let ([lines (split (get) #\newline)])
      (match-prefix lines "Processes:")))
  (let-values ([(p get) (open-string-output-port)])
    (pps p)
    (let ([lines (split (get) #\newline)])
      (match-prefix lines "Processes:"))))

(mat t1 ()
  (assert (= (match 5 (,x x)) 5)))
(mat t2 ()
  (assert (= (match 5 (1 2) (5 3)) 3)))
(mat t3 ()
  (assert-bad-match 5 (match 5)))
(mat t4 ()
  (assert (= (match-let* () 5) 5)))
(mat t5 ()
  (assert (= (match-let* ([(2 3) (list 2 3)]) 1) 1))
  (assert-bad-match '(1 2) (match-let* ([(,x) '(1 2)]) x))
  (assert-bad-match '(1 2) (match-let* ([,x (guard #f) '(1 2)]) x))
  (assert-bad-match '(1 2) (match-let* ([(,z) (guard (integer? z)) '(1 2)]) z)))
(mat t6 ()
  (assert (equal? (match-let* ([(,x ,y) (list 2 3)]) (list x y)) '(2 3))))
(mat t7 ()
  (assert
   (equal?
    (match-let* ([(,x ,y) (list 2 3)]
                 [(3 ,z) (list y x)]
                 [() '()])
      (list x y z))
    '(2 3 2))))

(mat t8 ()
  (assert-syntax-error (match-let* ([,@12 12]) 12) "invalid match pattern ,@12")
  (assert-syntax-error (match-let* ([(,x ,x) (list 3 4)]) x)
    "duplicate pattern variable")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (match 0 [`(<point> ,x ,x) x]))
   "duplicate pattern variable")
  (assert-syntax-error (match-let* ([`(no-record-type) 12]) 12)
    "unknown type no-record-type in `(no-record-type)")
  (assert-syntax-error (match 0 [,() 0]) "invalid match pattern ,()")
  (assert-syntax-error
   ;; we won't be able to use #[foo] record syntax for patterns, since the
   ;; record type might not accept patterns as field values, so use record
   ;; here to cover the "invalid match pattern" fall-through case
   (let ([orig (record-reader 'foo)])
     (define-record-type foo (nongenerative) (fields))
     (dynamic-wind
       (lambda () (record-reader 'foo (record-type-descriptor foo)))
       (lambda () (eval (read (open-input-string "(match 3 [#[foo] 4])"))))
       (lambda () (record-reader 'foo orig))))
   "invalid match pattern #[foo]")
  (assert-syntax-error (define-tuple <point> (x y)) "invalid syntax")
  (assert-syntax-error (define-tuple <point> make) "invalid field")
  (assert-syntax-error (define-tuple <point> copy) "invalid field")
  (assert-syntax-error (define-tuple <point> copy*) "invalid field")
  (assert-syntax-error (define-tuple <point> is?) "invalid field")
  (assert-syntax-error (define-tuple <point> x x) "duplicate field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> make))
   "missing field x in")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> make x))
   "invalid syntax")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> copy (<point> make [x 0] [y 0]) [x 1] [x 2]))
   "duplicate field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> open (<point> make [x 1] [y 2]) (x x))
     x)
   "duplicate field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> open (<point> make [x 1] [y 2]) (z))
     z)
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> open (<point> make [x 1] [y 2]) amazing-prefix: (x x))
     x)
   "duplicate field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> open (<point> make [x 1] [y 2]) amazing-prefix: (z))
     z)
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> open (<point> make [x 1] [y 2]) "invalid-prefix" (z))
     z)
   "invalid syntax")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> z))
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (lambda (p) (<point> z p)))
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (match 0 [`(<point> ,z) z]))
   "unknown field z in `(<point> ,z)")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (match 0 [`(<point> [z ,z]) z]))
   "unknown field z in `(<point> (z ,z))")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (lambda (p)
       (<point> open p [x y])
       (set! x 11) ;; open disallows set!
       (list x y)))
   "invalid syntax")
  (assert-syntax-error (match 0 [`((ooh!)) x]) "invalid match pattern `((ooh!))")
  )

(mat t9 ()
  (assert
   (let ()
     (define-tuple <point> x y)
     (let ([pt (<point> make [y 1] [x 2])])
       (match-let* ([`(<point> [x 1] [y 1]) (<point> copy pt [x 1])])
         #t))))
  (assert
   (let ([x 5])
     (match-let* ([,@x 5]) #t)))
  (assert (match (string #\f #\o #\o) [#vu8(1 2 3) #f] ["foo" #t]))
  (assert (match (bytevector 1 2 3) ["foo" #f] [#vu8(1 2 3) #t])))

(mat t10 ()
  (define-tuple foo a b c)
  (assert
   (equal?
    (match (foo make (a 1) (b 2) (c 3))
      [`(foo (a ,x) (b ,y) (c 2)) #f]
      [`(foo (a ,x) (b ,y) (c 3)) (list x y)])
    '(1 2)))
  (assert
   (equal?
    (match (foo make (a 1) (b 2) (c 3))
      [`(foo ,c ,b ,a) (list a b c)])
    '(1 2 3)))
  (assert
   (equal?
    (let ([a 4] [b 2])
      ;; tuple now permits ,@fld syntax
      (match (foo make (a 1) (b b) (c 3))
        [`(foo ,@a) 'not-matched]
        [`(foo ,a ,@b ,c) (list a c)]))
    '(1 3)))
  )

(mat t11 ()
  (assert
   (match '(5 #f foo "abc" #\x #(1 2 3))
     [(5 #f foo "abc" #\x #(1 2 3)) #t])))

(mat t12 ()
  (assert
   (let ([x 1])
     (match '(3 3)
       [(,x ,@x) #t]))))

(mat t13 ()
  (assert
   (let ([x '(5 #f foo "abc" #\x #(1 2 3))])
     (equal? x (match x [,(exp <= (5 #f foo . ,_)) exp])))))

(mat t14 ()
  ;; Using identifier-syntax or a library rename, the tuple
  ;; constructor should always create the original type.
  (let ()
    (define-tuple <point> x y)
    (define-syntax <should-be-a-point> (identifier-syntax <point>))
    (assert
     (equal? (<point> make [x 1] [y 2])
       (<should-be-a-point> make [x 1] [y 2]))))

  (repl-test 0
    '(begin
       (library (A)
         (export <point>)
         (import (scheme) (swish erlang))
         (define-tuple <point> x y))

       (library (B)
         (export <should-be-a-point>)
         (import (rename (A) (<point> <should-be-a-point>))))

       (let ()
         (import (scheme) (A) (B))
         (assert
          (equal? (<point> make [x 1] [y 2])
            (<should-be-a-point> make [x 1] [y 2])))
         (exit 0)))))

;; single-clause match -> match-let*
(mat t15 ()
  (let ([x '(1 "two" #t #(x y z))])
    (assert-bad-match x
      ;; single body expression, no guard
      (match x
        [(,n ,s #t #(x ,v1 fails-here))
         'not-reached]))
    ;; multiple body expressions, no guard
    (assert-bad-match x
      (match x
        [(,num ,str ((,bool)) ,vec)
         (printf "no output here\n")
         (raise 'not-reached)
         'bad]))
    (assert-bad-match x
      ;; single body expression, guard rejects
      (match x
        [(,n ,s #t #(x ,v1 ,v2))
         (guard (even? n))
         'not-reached]))
    ;; multiple body expressions, guard rejects
    (assert-bad-match x
      (match x
        [(,num ,str ,bool ,vec)
         (guard (string? bool))
         (printf "won't happen\n")
         (raise 'not-reached)
         'bad])))
  (match-let*
   ([,x '(1 "two" #t #(x y z))]
    ["two"
     ;; single body expression, no guard
     (match x
       [(,n ,s #t #(x ,v1 ,v2)) s])]
    [(z 3 y)
     ;; single body expression, guard accepts
     (match x
       [(,n ,s #t #(x ,v1 ,v2))
        (guard (odd? n))
        `(,v2 ,(* n 3) ,v1)])]
    [,reversed (reverse x)]
    [,@reversed
     ;; multiple body expressions, no guard
     (let ([var '()])
       (match x
         [(,num ,str ,bool ,vec)
          (set! var (cons num var))
          (set! var (cons str var))
          (set! var (cons bool var))
          (set! var (cons vec var))
          var]))]
    [,@reversed
     ;; multiple body expressions, guard accepts
     (let ([var '()])
       (match x
         [(,num ,str ,bool ,vec)
          (guard (and (number? num) (string? str) (boolean? bool) (vector? vec)))
          (set! var (cons num var))
          (set! var (cons str var))
          (set! var (cons bool var))
          (set! var (cons vec var))
          var]))])
   'ok)
  ;; single-clause match expands as match-let*
  (parameterize ([run-cp0 (lambda (f x) (f x))])
    (match-let*
     ([(begin
         ,invoke
         (let ([,v input])
           (if ,_ (,vector-ref ,@v 0) ,reject)))
       ;; single body expression, no guard
       (expand/optimize '(match input [#(,x) x]))]
      [(begin
         ,invoke
         (let ([,v input])
           (if ,_
               (let ([,x (,vector-ref ,@v 0)]) (foo) ,@x)
               ,reject)))
       ;; multiple body expressions, no guard
       (expand/optimize '(match input [#(,x) (foo) x]))]
      [(begin
         ,invoke
         (let ([,v input])
           (if ,_
               (let ([,x (,vector-ref ,@v 0)])
                 (if (,even? ,@x) ,@x ,reject))
               ,@reject)))
       ;; single body expression, has guard
       (expand/optimize '(match input [#(,x) (guard (even? x)) x]))]
      [(begin
         ,invoke
         (let ([,v input])
           (if ,_
               (let ([,x (,vector-ref ,@v 0)])
                 (if (,even? ,@x)
                     (begin (foo) ,@x)
                     ,reject))
               ,@reject)))
       (expand/optimize '(match input [#(,x) (guard (even? x)) (foo) x]))])
     'ok))
  )

(mat match-eof ()
  (match (read (open-input-string "")) [bad (raise 'wrong)] [#!eof 'ok])
  (match-let* ([#!eof (read (open-input-string ""))]) 'ok)
  (let ()
    (match-define #!eof (read (open-input-string "")))
    'ok)
  (match (list (vector (eof-object)))
    [#!eof (raise 'wrong)]
    [#(#!eof) (raise 'wrong)]
    [(#(#!eof)) 'ok])
  (match-let* ([#((7 . #!eof)) (vector (cons 7 (eof-object)))]) 'ok)
  (let ()
    (match-define #(#(,(var <= #!eof)) (,@var))
      (vector (vector (eof-object)) (list (eof-object))))
    'ok)
  )

(mat tuple ()
                                        ; accessors
  (assert
   (equal?
    '(a b b a)
    (let ()
      (define-tuple <point> x y)
      (let ([p (<point> make [x 'a] [y 'b])])
        (list
         (<point> x p)
         (<point> y p)
         ((<point> y) p)
         ((<point> x) p))))))
  (assert-error bad-tuple
    (let ()
      (define-tuple <box> content)
      (<box> content 'box)))
  (assert-error bad-tuple
    (let ()
      (define-tuple <box> content)
      (map (lambda (f) (f 3)) (list (<box> content)))))
                                        ; copy
  (assert
   (equal?
    '(#f #t (1 2) (1 2) (("outer x") 2) (1 #("outer y")) (5 7))
    (let ()
      (define-tuple <point> x y)
      (define (dump p) (list (<point> x p) (<point> y p)))
      (let ([p (<point> make [x 1] [y 2])]
            [x "outer x"]
            [y "outer y"])
        (list*
         (eq? p (<point> copy p))
         (equal? p (<point> copy p))
         (map dump
           (list
            p
            (<point> copy p)
            (<point> copy p [x (list x)])
            (<point> copy p [y (vector y)])
            (<point> copy p [x 5] [y 7]))))))))
  (assert-error bad-tuple
    (let ()
      (define-tuple <box> content)
      (<box> copy 27)))
                                        ; copy*
  (assert
   (equal?
    '(#f #t (1 2) (1 2) ((1 "outer y") 2) (1 #("outer x" 2)) ((1 2) #(1 2)) (2 1))
    (let ()
      (define-tuple <point> x y)
      (define (dump p) (list (<point> x p) (<point> y p)))
      (let ([p (<point> make [x 1] [y 2])]
            [x "outer x"]
            [y "outer y"])
        (list*
         (eq? p (<point> copy* p))
         (equal? p (<point> copy* p))
         (map dump
           (list
            p
            (<point> copy* p)
            (<point> copy* p [x (list x y)])
            (<point> copy* p [y (vector x y)])
            (<point> copy* p [x (list x y)] [y (vector x y)])
            (<point> copy* p [x y] [y x]))))))))
  (assert-error bad-tuple
    (let ()
      (define-tuple <box> content)
      (<box> copy* 27)))
                                        ; is?
  (assert
   (equal?
    '(#t #f #f #f #f #f #f #f #f #f #f)
    (let ()
      (define-tuple <point> x y)
      (map (lambda (p) (<point> is? p))
        (list (<point> make [x 1] [y 2]) #f #t 27 #\a "point" 'point '(point) '#(<point>) '#(<point> 1) '#(<point> 1 2 3))))))
                                        ; open
  (assert
   (equal?
    '((1 bound-y) (bound-x 2) (1 2) (bound-x bound-y 1 2))
    (let ()
      (define-tuple <point> x y)
      (let ([p (<point> make [x 1] [y 2])])
        (list
         (let ([x 'bound-x] [y 'bound-y])
           (<point> open p (x))
           (list x y))
         (let ([x 'bound-x] [y 'bound-y])
           (<point> open p (y))
           (list x y))
         (let ([x 'bound-x] [y 'bound-y])
           (<point> open p (x y))
           (list x y))
         (let ([x 'bound-x] [y 'bound-y])
           (<point> open p p. (x y))
           (list x y p.x p.y)))))))
  (assert-error bad-tuple
    (let ()
      (define-tuple <box> content)
      (<box> open 27 (content))
      content))
  (assert-error bad-tuple
    (let ()
      (define-tuple <box> content)
      (<box> open 27 magical-prefix: (content))
      magical-prefix:content))
  (assert-error bad-tuple #(at ,_ "src/swish/erlang.ms")
    (let ()
      (define-tuple <box> content)
      (<box> content 'gloves)))
  (assert-error bad-tuple #(at ,_ "src/swish/erlang.ms")
    (let ()
      (define-tuple <box> content)
      (<box> open 'gloves [content])
      content))
  (assert-bad-match 'gloves #(at ,_ "src/swish/erlang.ms")
    (let ()
      (define-tuple <box> content)
      (match 'gloves
        [`(<box> ,content) 'ok])))

  )

(define-syntax with-cp0-disabled
  (syntax-rules ()
    [(_ e ...)
     (parameterize ([run-cp0 (lambda (f x) x)])
       (eval '(let () e ...)))]))

(mat native-record ()
  (with-cp0-disabled
   ;; define-record-type
   (let ()
     (define-record-type foo
       (nongenerative)
       (fields a (mutable b) (mutable c get-c smash-c!)))
     (define-record-type bar
       (nongenerative)
       (parent foo)
       (fields a m)) ;; shadow foo's a field
     ;; basic native-record match
     (match-let*
      ([,x (make-foo 1 "bee" "sea")]
       [`(foo) x]
       [`(foo [a 1]) x]
       [`(foo ,a) x]
       [1 a]
       [`(foo [a 1] [b "bee"] [c "sea"]) x]
       [`(foo ,a ,b ,c) x]
       ["bee" b]
       ["sea" c]
       [ok (match x [`(foo [a 'nope]) 'bad] [`(foo) 'ok])]
       [ok (match x [`(foo [b 'nope]) 'bad] [`(foo) 'ok])]
       [ok (match x [`(foo [c 'nope]) 'bad] [`(foo) 'ok])]
       [ok (match x [`(bar) 'bad] [`(foo) 'ok])]
       [,! (smash-c! x "changed")]
       [`(foo [a ,@a] [b ,@b] [c "changed"]) x]
       [`(foo [b ,@b] [c "changed"] [a ,@a]) x]
       [`(foo [c "changed"] [b ,@b] [a ,@a]) x]
       [`(foo [b ,@b] [a ,@a] [c "changed"]) x])
      'ok)
     ;; record inheritance
     (match-let*
      ([,x (make-bar "foo A" "foo B" "foo C" "bar A" "bar M")]
       [`(foo) x]
       [`(bar) x]
       [`(foo [a "foo A"]) x]
       [`(foo ,a) x]
       ["foo A" a]
       [`(bar [a "bar A"]) x]
       [`(bar ,a) x]
       ["bar A" a]
       [`(foo [a "foo A"] [b "foo B"] [c "foo C"]) x]
       [`(foo ,a ,b ,c) x]
       ["foo B" b]
       ["foo C" c]
       [ok (match x [`(bar [a 'nope]) 'bad] [`(bar) 'ok])]
       [ok (match x [`(bar [b 'nope]) 'bad] [`(bar) 'ok])]
       [ok (match x [`(bar [c 'nope]) 'bad] [`(bar) 'ok])]
       [fine (match x [`(bar) 'fine] [`(foo) 'ok])]
       [,! (smash-c! x "changed")]
       [`(foo ,@a ,@b [c "changed"]) x]
       [`(foo [a ,@a] [b ,@b] [c "changed"]) x]
       [`(bar [a "bar A"] [m "bar M"] [b ,@b] [c "changed"]) x]
       [`(bar [m "bar M"] [a "bar A"] [b ,@b] [c "changed"]) x]
       [`(bar [m "bar M"] [b ,@b] [a "bar A"] [c "changed"]) x]
       [`(bar [m "bar M"] [b ,@b] [c "changed"] [a "bar A"]) x]
       [`(bar ,@b [m "bar M"] [c "changed"] [a "bar A"]) x]
       [`(bar [b ,@b] [m "bar M"] [c "changed"] [a "bar A"]) x]
       [`(bar [b ,@b] [c "changed"] [m "bar M"] [a "bar A"]) x]
       [`(bar [b ,@b] [c "changed"] [a "bar A"] [m "bar M"]) x]
       [`(bar [c "changed"] [b ,@b] [a "bar A"] [m "bar M"]) x]
       [`(bar [c "changed"] [a "bar A"] [b ,@b] [m "bar M"]) x]
       [`(bar [c "changed"] [a "bar A"] [m "bar M"] [b ,@b]) x])
      'ok))
   ;; define-record, Chez Scheme's high-octane record-definition macro
   (let ()
     (define-record foo
       (f1
        [immutable unsigned-short kurz]
        [mutable   float          sam]
        [immutable long           长]
        [mutable   unsigned-8     bits]))
     (define-record bar foo
       ([immutable uptr       there]
        [mutable   integer-64 n]
        bits))
     (define (maxval type)
       (- (expt 2 (* 8 (foreign-sizeof type))) 1))
     ;; basic native-record match
     (match-let*
      ([,ushort (maxval 'unsigned-short)]
       [,input (make-foo "formula" ushort 3.2 (maxval 'long) (maxval 'unsigned-8))]
       [`(foo) input]
       [`(foo [f1 "formula"]) input]
       [`(foo ,sam)
        (guard (< (abs (- sam 3.2)) .0001))
        input]
       [`(foo [kurz ,@ushort]) input]
       [`(foo [长 -1]) input]
       [`(foo [bits 255]) input]
       [`(foo ,f1 ,kurz ,sam ,长 ,bits) input]
       ["formula" f1]
       [,@ushort kurz]
       [#t (< (abs (- sam 3.2)) .0001)]
       [-1 长]
       [255 bits]
       [ok (match input [`(foo [f1 "racer"]) 'no] [`(foo) 'ok])]
       [ok (match input [`(foo [kurz 'gesagt]) 'no] [`(foo) 'ok])]
       [ok (match input [`(foo [长 'nope]) 'no] [`(foo) 'ok])]
       [ok (match input [`(foo [bits 123]) 'no] [`(foo) 'ok])]
       [,! (set-foo-sam! input 88.7)]
       [,bits 32]
       [,! (set-foo-bits! input bits)]
       [`(foo ,@f1 ,sam ,@长 ,@bits)
        (guard (< (abs (- sam 88.7)) .0001))
        input]
       [`(foo [f1 ,@f1] ,@sam ,@长 [bits ,@bits]) input]
       [`(foo ,@sam [f1 ,@f1] ,@长 [bits ,@bits]) input]
       [`(foo ,@sam ,@长 [f1 ,@f1] [bits ,@bits]) input]
       [`(foo ,@sam ,@长 [bits ,@bits] [f1 ,@f1]) input]
       [`(foo ,@长 ,@sam [bits ,@bits] [f1 ,@f1]) input])
      'ok)
     ;; record inheritance
     (match-let*
      ([,ushort 32768]
       [,uptr (maxval 'uptr)]
       [,input (make-bar "foo-f1" ushort 90.1 1025 201 uptr 37 '(a scheme object))]
       [`(foo) input]
       [`(bar) input]
       [`(foo [f1 "foo-f1"]) input]
       [`(bar [f1 "foo-f1"]) input]
       [`(foo ,sam)
        (guard (< (abs (- sam 90.1)) .0001))
        input]
       [`(bar ,sam)
        (guard (< (abs (- sam 90.1)) .0001))
        input]
       [`(foo [kurz ,@ushort]) input]
       [`(bar [kurz ,@ushort]) input]
       [`(foo [长 1025]) input]
       [`(bar [长 1025]) input]
       [`(foo [bits 201]) input]
       [`(bar [bits (a scheme object)]) input]
       [`(bar [there ,@uptr]) input]
       [`(bar [n 37]) input]
       [`(foo ,f1 ,kurz ,sam ,长 ,bits) input]
       [`(bar ,@f1 ,@kurz ,@sam ,@长 [bits ,bar-bits] ,there ,n) input]
       ["foo-f1" f1]
       [,@ushort kurz]
       [#t (< (abs (- sam 90.1)) .0001)]
       [1025 长]
       [201 bits]
       [(a scheme object) bar-bits]
       [,@uptr there]
       [37 n]
       [ok (match input [`(bar [f1 "racer"]) 'no] [`(bar) 'ok])]
       [ok (match input [`(bar [kurz 'gesagt]) 'no] [`(bar) 'ok])]
       [ok (match input [`(bar [长 'nope]) 'no] [`(bar) 'ok])]
       [ok (match input [`(bar [bits 123]) 'no] [`(bar) 'ok])]
       [,bits 32]
       [,! (set-foo-bits! input bits)]
       [`(foo ,@f1 ,@sam ,@长 ,@bits) input]
       [`(foo [f1 ,@f1] ,@sam ,@长 [bits ,@bits]) input]
       [`(foo ,@sam [f1 ,@f1] ,@长 [bits ,@bits]) input]
       [`(foo ,@sam ,@长 [f1 ,@f1] [bits ,@bits]) input]
       [`(foo ,@sam ,@长 [bits ,@bits] [f1 ,@f1]) input]
       [`(foo ,@长 ,@sam [bits ,@bits] [f1 ,@f1]) input]
       [`(bar ,@f1 ,@sam ,@长 [bits ,@bar-bits] ,@there ,@n) input]
       [`(bar ,@sam ,@f1 ,@长 [bits ,@bar-bits] ,@there ,@n) input]
       [`(bar ,@sam ,@长 ,@f1 [bits ,@bar-bits] ,@there ,@n) input]
       [`(bar ,@sam ,@长 [bits ,@bar-bits] ,@f1 ,@there ,@n) input]
       [`(bar ,@sam ,@长 [bits ,@bar-bits] ,@there ,@f1 ,@n) input]
       [`(bar ,@sam ,@长 [bits ,@bar-bits] ,@there ,@n ,@f1) input]
       [`(bar [长 1025] ,@sam [bits ,@bar-bits] [there ,@uptr] ,@n ,@f1) input])
      'ok))
   ))

(mat match-define-record ()
  (define-record-type foo (nongenerative) (fields a b))
  (define-record-type bar (nongenerative) (parent foo) (fields a c))
  (define-record sand ([immutable ptr x] [immutable unsigned-16 y]))
  (define-record silt sand ([immutable double y] z))
  (define (check-err exception msg)
    (starts-with?
     (let ([os (open-output-string)])
       (display-condition exception os)
       (get-output-string os))
     msg))
  ;; define-record-type
  (match-let*
   ([,x (make-foo 1 2)]
    [,y (make-bar "foo A" "foo B" "bar A" "bar C")]
    [(1 2)
     (let ()
       (match-define `(foo , a,b) x)
       (list a b))]
    [("foo A" "foo B") ;; inheritance
     (let ()
       (match-define `(foo ,a ,b) y)
       (assert (bar? y))
       (list a b))]
    [("bar A" "bar C") ;; bar's a shadows foo's a
     (let ()
       (match-define `(bar ,a ,c) y)
       (list a c))]
    [("foo B") ;; bar inherits foo's b
     (let ()
       (match-define `(bar ,b) y)
       (list b))]
    [("foo A" "bar A" "foo B" "bar C") ;; multiple binds
     (let ()
       (match-define `(foo [a ,foo.a] [b ,foo.b]) y)
       (match-define `(bar [a ,bar.a] [c ,bar.c]) y)
       (list foo.a bar.a foo.b bar.c))]
    [("foo A" "foo B" "bar A" "foo B" "bar C") ;; multiple, shadow, inheritance
     (let ()
       (match-define `(foo [a ,foo.a] [b ,foo.b]) y)
       (match-define `(bar [a ,bar.a] [b ,bar.b] [c ,bar.c]) y)
       (list foo.a foo.b bar.a bar.b bar.c))]
    [(3 2 4) ;; deeper inheritance, but no added fields
     (let () ;; spam's a is bar's a, spam's b is foo's b, spam's c is bar's c
       (define-record-type spam (nongenerative) (parent bar))
       (define z (make-spam 1 2 3 4))
       (match-define `(spam ,a ,b ,c) z)
       (list a b c))]
    [#(EXIT #(bad-match ,_ ,_))
     (catch
      (let ()
        (define-record-type tofu (nongenerative))
        (match-define `(bar ,a ,c) (make-tofu))
        (list a c)))]
    [#(EXIT #(bad-match 27 ,_))
     (catch
      (let ()
        (match-define `(foo ,a ,b) 27)
        (list a b)))]
    [#(EXIT ,reason)
     (guard (check-err reason "Exception: unknown type foo in `(foo ,bogus)"))
     ;; local foo isn't visible at top-level
     (catch (expand '(match-define `(foo ,bogus) x)))]
    [#(EXIT ,reason)
     (guard (check-err reason "Exception: unknown field bogus in `(foo ,bogus)"))
     (catch
      (expand
       '(let ()
          (define-record-type foo (nongenerative) (fields meadows))
          (match-define `(foo ,bogus) x)
          123)))]
    [#(EXIT ,reason)
     (guard (check-err reason "Exception: attempt to reference undefined variable x"))
     (catch
      (let ()
        (define-record-type foo (nongenerative) (fields x y))
        ;; wrong for same reason as (define (f x) (define x (cdr x)) x)
        (define (f x) (match-define `(foo ,x) x) x)
        (f (make-foo "X" "Y"))))])
   'ok)
  ;; define-record
  (match-let*
   ([,a (make-sand '#(whole "grain") 65000)]
    [,b (make-silt "fine" 9000 98.6 '(#t a #f))]
    [("grain" 65000)
     (let ()
       (match-define `(sand [x #(whole ,part)] ,y) a)
       (list part y))]
    [("fine" 9000)
     (let ()
       ;; inheritance
       (match-define `(sand ,x ,y) b)
       (assert (silt? b))
       (list x y))]
    ["fine" ;; silt inherits sand's x
     (let ()
       (match-define `(sand ,x) b)
       x)]
    [ok ;; silt's y shadows sand's y
     (let ()
       (match-define `(silt ,y) b)
       (and (< (abs (- 98.6 y)) .0001) 'ok))]
    [(a #t "fine" "fine" 9000) ;; prefix, multiple binds, shadow, inheritance
     (let ()
       (match-define `(sand [x ,a:x] [y ,a:y]) b)
       (match-define `(silt [x ,blot-x] [y ,blot-y] [z (#t ,blot-middle #f)]) b)
       (list
        blot-middle
        (< (abs (- blot-y 98.6)) .0001)
        blot-x
        a:x
        a:y))]
    [(2 1 3 4) ;; deeper inheritance, but no added fields
     (let () ;; spam's x is sand's x, spam's y is silt's y, spam's z is silts's z
       (define-record spam silt ())
       (define c (make-spam 1 2 3.0 4))
       (match-define `(sand ,y) c)
       (match-define `(spam [x ,z.x] [y ,z.y] [z ,z.z]) c)
       (list y z.x (exact z.y) z.z))]
    [#(EXIT #(bad-match ,_ ,_))
     (catch
      (let ()
        (define-record-type tofu (nongenerative))
        (match-define `(silt ,x ,z) (make-tofu))
        (list x z)))]
    [#(EXIT #(bad-match 27 ,_))
     (catch
      (let ()
        (match-define `(sand ,x ,y) 27)
        (list x y)))]
    [#(EXIT ,reason)
     (guard (check-err reason "Exception: unknown type sand in `(sand ,storm)"))
     ;; local foo isn't visible at top-level
     (catch (expand '(match-define `(sand ,storm) x)))]
    [#(EXIT ,reason)
     (guard (check-err reason "Exception: unknown field bogus in `(foo ,bogus)"))
     (catch
      (expand
       '(let ()
          (define-record foo (meadows))
          (match-define `(foo ,bogus) x)
          123)))]
    [#(EXIT ,reason)
     (guard (check-err reason "Exception: attempt to reference undefined variable x"))
     (catch
      (let ()
        (define-record foo (x y))
        ;; wrong for same reason as (define (f x) (define x (cdr x)) x)
        (define (f x) (match-define `(foo ,x) x) x)
        (f (make-foo "X" "Y"))))]
    [#(EXIT ,reason)
     (guard (check-err reason "Exception: invalid match pattern `(foo (a 1 opt))"))
     (catch
      (expand
       '(lambda (x)
          (define-record foo (a b))
          (match x
            ;; [field pat] must not include options
            [`(foo [a 1 opt]) 'ok]))))])
   'ok))

(mat match-define-tuple ()
  (with-cp0-disabled
   (define-tuple <foo> a b)
   (define (check-err exception msg)
     (starts-with?
      (let ([os (open-output-string)])
        (display-condition exception os)
        (get-output-string os))
      msg))
   (match-let*
    ([,x (<foo> make [a 1] [b 2])]
     [(1 2)
      (let ()
        (match-define `(<foo> ,a ,b) x)
        (list a b))]
     [(2 1) ;; prefix
      (let ()
        (match-define `(<foo> [a ,foo.a] [b ,foo.b]) x)
        (list foo.b foo.a))]
     [(2 1 1 2) ;; prefix, multiple binds
      (let ()
        (match-define `(<foo> [a ,foo.a] [b ,foo.b]) x)
        (match-define `(<foo> [a ,x.a] [b ,x.b]) x)
        (list foo.b foo.a x.a x.b))]
     [#(EXIT #(bad-match ,_ ,_))
      (catch
       (let ()
         (define-record-type tofu (nongenerative))
         (match-define `(<foo> ,a ,b) (make-tofu))
         (list a b)))]
     [#(EXIT #(bad-match 27 ,_))
      (catch
       (let ()
         (match-define `(<foo> ,a ,b) 27)
         (list a b)))]
     [#(EXIT ,reason)
      (guard (check-err reason "Exception: unknown type <foo> in `(<foo> ,bogus)"))
      ;; local foo isn't visible at top-level
      (catch (expand '(match-define `(<foo> ,bogus) x)))]
     [#(EXIT ,reason)
      (guard (check-err reason "Exception: unknown field bogus in `(<foo> ,bogus)"))
      (catch
       (expand
        '(let ()
           (define-tuple <foo> pine cone)
           (match-define `(<foo> ,bogus) x)
           123)))]
     [#(EXIT ,reason)
      (guard (check-err reason "Exception: attempt to reference undefined variable x"))
      (catch
       (let ()
         (define-tuple <foo> x y)
         ;; wrong for same reason as (define (f x) (define x (cdr x)) x)
         (define (f x) (match-define `(<foo> ,x) x) x)
         (f (<foo> make [x "X"] [y "Y"]))))]
     [#(EXIT ,reason)
      (guard (check-err reason "Exception: invalid match pattern `(<foo> (a 1 opt))"))
      (catch
       (expand
        '(lambda (x)
           (define-tuple <foo> a b)
           (match x
             ;; [field pat] must not include options
             [`(<foo> [a 1 opt]) 'ok]))))])
    'ok)))

(isolate-mat match-etc ()
  (with-cp0-disabled
   (define (f x) (match-define #(#(a ,r) (c ,d ,e)) x) (list r e d))
   ;; try nested patterns that will go haywire if match-help doesn't introduce fresh vars where it should
   (match-let*
    ([(#()) (list (vector))]
     [(3 2 1) (f '#(#(a 3) (c 1 2)))]
     [#(EXIT #(bad-match "foo" ,_)) (catch (f "foo"))]
     [,x '#(#("foo" r) (c d e))]
     [#(EXIT #(bad-match ,@x ,_)) (catch (f x))]
     [,x '#(#(a r) ("foo" d e))]
     [#(EXIT #(bad-match ,@x ,_)) (catch (f x))]
     [#(#(,a) #(,b))
      (guard (and (eqv? a 3) (eqv? b 4)))
      '#(#(3) #(4))]
     [#(#((#(,a))) #(b #(,c #(,d))))
      (guard (and (eqv? a 1) (eqv? c 3) (eqv? d 4)))
      '#(#((#(1))) #(b #(3 #(4))))]
     [,input '(() #(#() (("asdf") . #(y)) z) #(#(#(#(p) d)) q))]
     [(() #(#() (("asdf") . #(y)) z) #(#(#(#(p) d)) q)) input]
     [ok (let ()
           (match-define (() #(#() (("asdf") . #(y)) z) #(#(#(#(p) d)) q))
             input)
           'ok)]
     [,input '#(((#vu8(1 2 3 4 5))))]
     [#(((#vu8(1 2 3 4 5)))) input]
     [ok (let () (match-define #(((#vu8(1 2 3 4 5)))) input) 'ok)]
     [,input '#((a) ((#(b))) (((#(c)))) #(#(#(#(#(#(#(d) (e)) ((f))) (((g)))) ((((h)))))) "i"))]
     [#((a) ((#(b))) (((#(c)))) #(#(#(#(#(#(#(d) (e)) ((f))) (((g)))) ((((h)))))) "i")) input]
     [ok (let ()
           (match-define
            #((a)
              ((#(b)))
              (((#(c))))
              #(#(#(#(#(#(#(d) (e)) ((f))) (((g)))) ((((h)))))) "i"))
            input)
           'ok)]
     )
    'ok)
   ;; receive
   (let ([pid
          (spawn&link
           (lambda ()
             (let f ()
               (receive (after 1000 (raise 'timeout))
                 [quit 'ok]
                 [(send ,who ,what) (send who what) (f)]))))])
     (define-record-type one (nongenerative) (fields x y))
     (define-record-type two (nongenerative) (parent one) (fields y z))
     ;; vector with some nested stuff
     (send pid `(send ,self #(marmoset ((lamp) ginger) corn)))
     (receive (after 100 (raise 'timeout))
       [#(marmoset ((lamp) ,biscuit) ,chowder)
        (match-let* ([ginger biscuit] [corn chowder])
          'ok)])
     ;; native record
     (send pid `(send ,self ,(make-one 'hundred "percent")))
     (receive (after 100 (raise 'timeout))
       [`(one ,x ,y) (match-let* ([hundred x] ["percent" y]) 'ok)])
     ;; native record, inheritance
     (send pid `(send ,self ,(make-two 'many 'fold "home" "improvement")))
     (receive (after 100 (raise 'timeout))
       [`(one ,x ,y) (match-let* ([many x] [fold y]) 'ok)])
     ;; native record, inheritance
     (send pid `(send ,self ,(make-two 'many 'fold "home" "improvement")))
     (receive (after 100 (raise 'timeout))
       [`(two ,x ,y) (match-let* ([many x] ["home" y]) 'ok)])
     ;; native record, restricted match
     (send pid `(send ,self ,(make-two 'many 'fold "home" '#("improvement"))))
     (receive (after 100 (raise 'timeout))
       [`(one ,x [y "home"]) (raise "don't match one's y with two's y value")]
       [`(two [y fold]) (raise "don't match two's y with one's y value")]
       [`(two ,x ,y [z (,val)]) (raise "z val is vector not list")]
       [`(two ,x ,y [z #(,val)]) (match-let* ([many x] ["home" y] ["improvement" val]) 'ok)])
     ;; nested record match
     (send pid `(send ,self #((one ,(make-one 1 2)) (two ,(make-two 8 7 5 4)))))
     (receive (after 100 (raise 'timeout))
       [(,a ,b) (raise "should be vector")]
       [#((two ,_) (one ,_)) (raise "wrong order")]
       [#((one `(one ,x ,y)) (two `(two [x ,two.x] [y ,two.y] ,z)))
        (match-let* ([1 x] [2 y] [8 two.x] [5 two.y] [4 z]) 'ok)])
     ;; nested record match, inheritance case
     (send pid `(send ,self ,(make-one (make-two 1 2 3 4) (make-one 5 (make-one 6 7)))))
     (receive (after 100 (raise 'timeout))
       [`(one [x `(one ,x ,y)] [y `(one [x 5] [y `(one [x ,x2] [y ,y2])])])
        (match-let* ([1 x] [2 y] [6 x2] [7 y2]) 'ok)])
     ;; nested record match, another inheritance case
     (send pid `(send ,self ,(make-one (make-two 1 2 3 4) (make-two 5 (make-one 16 17) 11 12))))
     (receive (after 100 (raise 'timeout))
       [`(one [x `(two ,x ,y ,z)] [y `(one [x 5] [y `(one [x ,x2] [y ,y2])])])
        (match-let* ([1 x] [3 y] [4 z] [16 x2] [17 y2]) 'ok)])
     ;; done
     (send pid 'quit))
   ;; syntax-errors in receive
   (assert-syntax-error
    (receive [`(great-unknown) 404])
    "unknown type great-unknown in `(great-unknown)")
   (assert-syntax-error
    (receive (after 100 'ok) [`(great-unknown) 404])
    "unknown type great-unknown in `(great-unknown)")
   (assert-syntax-error
    (let ()
      (define-record-type moo (nongenerative) (fields grass straw))
      (receive [`(moo ,hay) 123]))
    "unknown field hay in `(moo ,hay)")
   (assert-syntax-error
    (let ()
      (define-record-type moo (nongenerative) (fields grass straw))
      (define-record-type goo (nongenerative) (parent moo) (fields gai pan))
      (receive [`(moo ,pan) 123]))
    "unknown field pan in `(moo ,pan)")
   ;; using pattern bindings in lieu of generalized prefix
   (let ()
     (define-record-type zip (nongenerative) (fields))
     (define-record-type blat (nongenerative) (parent zip) (fields m))
     (define-tuple <top> dog)
     (match-let*
      ([,nada (make-zip)]
       [,input `(A #(b C) ,nada ,(make-blat "frozz") #(<top> "hat"))]
       [(A C "hat" ,@nada "frozz")
        (let ()
          (match-define
           (,a.a #(,_ ,a.c) ,a.z
             `(blat [m ,a.m])
             `(<top> [dog ,a.dog]))
           input)
          (list a.a a.c a.dog a.z a.m))]
       [(1 2 3 4 A C ,@nada "frozz" food "hat")
        ;; make sure we don't bind the original non-prefixed fields
        (let ([a 1] [c 2] [z 3] [m 4] [dog 'food])
          (match-define
           (,a.a #(,_ ,a.c) ,a.z
             `(blat [m ,a.m])
             `(<top> [dog ,a.dog]))
           input)
          (list a c z m a.a a.c a.z a.m dog a.dog))]
       [(A #(b C) "frozz" orig same fine ernate unimpeded untouched "hat" days bark)
        ;; make sure we don't bind the original non-prefixed fields or prefix field rather than pattern var
        (let ([a 'orig] [b 'same] [m 'fine] [alt 'ernate] [pfx:z 'unimpeded] [pfx:m 'untouched] [dog 'days] [pfx:dog 'bark])
          (match-define (,pfx:a ,pfx:b ,_ `(blat [m ,pfx:alt]) `(<top> [dog ,pfx:pile]))
            input)
          (list pfx:a pfx:b pfx:alt a b m alt pfx:z pfx:m pfx:pile dog pfx:dog))]
       [ok
        (let ([m 'orig-m] [dog 'bark])
          ;; make sure match-let* doesn't introduce binding for field when we have pattern var
          (match-let* ([(,_ ,_ ,_ `(blat [m ,blam]) `(<top> [dog ,run])) input]
                       ["frozz" blam]
                       ["hat" run]
                       [orig-m m]
                       [bark dog])
            'ok))]
       [ok
        (let ([m 'orig-m] [dog 'bark])
          ;; make sure match doesn't introduce binding for field when we have pattern var
          (match input
            [nope 'boo]
            [(not this either) 'hiss]
            [(,_ ,_ ,_ `(blat [m ,blam]) `(<top> [dog ,run]))
             (match-let* (["frozz" blam] ["hat" run] [orig-m m] [bark dog])
               'ok)]))]
       [#(EXIT #(bad-match 17 ,_))
        ;; bad-match case where we use pattern bindings in lieu of general prefix
        (catch
         (let ()
           (match-define (`(blat [m ,x:m]) `(blat [m ,y-m])) 17)
           (list a b c)))]
       )
      'ok))
   ;; duplicate pattern-variable binding
   (assert-syntax-error
    (lambda (x)
      (define-record-type plow (nongenerative) (fields m n o))
      (match-define #((a `(plow ,m ,n)) (b `(plow ,n ,o))) x)
      (list m n o))
    "duplicate pattern variable")
   ;; no duplicate pattern-variable despite duplicate field names
   (let ()
     (define-record-type plow (nongenerative) (fields m n o))
     (match-let*
      ([,x (make-plow 3 5 7)]
       [(`(plow ,m ,n ,o) == `(plow [m ,@m] [n ,n2] [o ,o2])) (list x '== x)])
      'ok))
   ;; deeper inheritance
   (let ()
     (define-record-type plow (nongenerative) (fields m n o))
     (define-record-type share (nongenerative) (parent plow))
     (define-record-type sword (nongenerative) (parent share) (fields pommel hilt blade))
     (define-record-type shard (nongenerative) (parent sword) (fields o p))
     (define-record-type shade (nongenerative) (parent shard) (fields groan p))
     (define-record-type wraith (nongenerative) (parent shade) (fields m t))
     (match-let*
      ([,narsil (make-shard "m" "n" "o" "pommel" "hilt" "blade" "2o" "p")]
       [,casper (make-wraith "m2" "n2" "o2" "pommel2" "hilt2" "blade2" "2o2" "p2" "q2" "2p2" "2m2" "t2")]
       [(`(plow [m "m"])) (list narsil)]
       [#(`(share [m "m"] [n "n"] [o "o"])) (vector narsil)]
       [#((123 . `(sword [m "m"] [n "n"] [o "o"] [pommel "pommel"] [hilt "hilt"] [blade "blade"])))
        (vector (cons 123 narsil))]
       [(#(((`(shard [m "m"] [n "n"] [pommel "pommel"] [hilt "hilt"] [blade "blade"] [o "2o"] [p "p"]) . one))))
        (list (vector (list (cons narsil 'one))))]
       [ok (match narsil
             [`(shade ,groan) "bad"]
             [`(wraith ,m ,t) "wrong"]
             [`(shard ,m [m ,@m]) 'ok])] ;; repeated field reference okay since no duplicated pattern var
       [`(plow [m "m2"] [n "n2"] [o "o2"]) casper]
       [`(share [m "m2"] [n "n2"] [o "o2"]) casper]
       [`(sword [m "m2"] [n "n2"] [o "o2"] [pommel "pommel2"] [hilt "hilt2"] [blade "blade2"]) casper]
       [`(shard [m "m2"] [n "n2"] [pommel "pommel2"] [hilt "hilt2"] [blade "blade2"] [o "2o2"] [p "p2"]) casper]
       [`(shade [m "m2"] [n "n2"] [pommel "pommel2"] [hilt "hilt2"] [blade "blade2"] [o "2o2"] [groan "q2"] [p "2p2"]) casper]
       [`(wraith [n "n2"] [pommel "pommel2"] [hilt "hilt2"] [blade "blade2"] [o "2o2"] [groan "q2"] [p "2p2"] [m "2m2"] [t "t2"]) casper])
      'ok))
   ;; cover near-miss cases deciphering compile-time values
   (assert-syntax-error
    (let ()
      (define-syntax zor (make-compile-time-value '(a b c d)))
      (match-let* ([`(zor) "blatt"]) 'ok))
    "unknown type zor in `(zor)")
   (assert-syntax-error
    (let ()
      (define-syntax zor (make-compile-time-value '(17)))
      (match-let* ([`(zor) "blatt"]) 'ok))
    "unknown type zor in `(zor)")
   (assert-syntax-error
    (let ()
      (define-syntax zor (make-compile-time-value 404))
      (match-let* ([`(zor) "blatt"]) 'ok))
    "unknown type zor in `(zor)")
   ;; unquote-splicing with field reference
   (let ()
     (define-tuple <foo> a b)
     (define-record-type toast (nongenerative) (fields type doneness))
     (match-let* ([`(<foo> ,a ,b) '#(<foo> 1 2)]
                  [1 a]
                  [2 b])
       'ok)
     (match-let* ([,a 1]
                  [`(<foo> ,@a ,b) '#(<foo> 1 2)]
                  [2 b])
       'ok)
     (match-let* ([,type "rye"]
                  [`(toast ,@type ,doneness) (make-toast "rye" "burnt")]
                  ["burnt" doneness])
       'ok))
   ;; bad field reference in unquote-splicing
   (assert-syntax-error
    (let ()
      (define-tuple <bar> none)
      (match-let* ([,stack 'hay]
                   [`(<bar> ,@stack) (<bar> make [none 'such])])
        'ok))
    "unknown field stack in `(<bar> ,@stack)")
   (assert-syntax-error
    (let ()
      (define-record-type skate (nongenerative) (fields fins tail))
      (match-let* ([,board 'plank]
                   [`(skate ,@board) (make-stake '(dorsal pectoral) 'whiplike)])
        'ok))
    "unknown field board in `(skate ,@board)"))
  )

(isolate-mat match-extension ()
  (with-cp0-disabled
   (define-tuple <tres> a b c)
   ;; wrong number of args to define-match-extension
   (assert-syntax-error
    (let ()
      (define-match-extension foo)
      (match 3 [`(foo anything) 'ok]))
    "invalid syntax")
   ;; wrong number of args to define-match-extension
   (assert-syntax-error
    (let ()
      (define-match-extension foo bar baz blam)
      (match 3 [`(foo anything) 'ok]))
    "invalid syntax")
   ;; fail in syntax-case within handle-object
   (assert-syntax-error
    (let ()
      (define-match-extension foo
        (lambda (v pattern)
          (syntax-case pattern ())))
      (match 3 [`(foo anything) 'ok]))
    "invalid syntax")
   ;; handle-object returns invalid output
   (assert-syntax-error
    (let ()
      (define-match-extension foo
        (lambda (v pattern) '#(wrong)))
      (match 3 [`(foo) 'ok]))
    "invalid handle-object output #(wrong) in `(foo)")
   ;; return #f from handle-object to reject pattern
   (assert-syntax-error
    (let ()
      (define-match-extension reject
        (lambda (v pattern) #f))
      (match 3 [`(reject ,this) 'ok]))
    "invalid match pattern `(reject ,this)")
   ;; field specs must be ,id ,@id or [id pat opt ...]
   ;;  - ,x where x not identifier
   (assert-syntax-error
    (let ()
      (define-match-extension any
        (lambda (v pattern)
          (syntax-case pattern (quasiquote)
            [`(any spec ...)
             #`((handle-fields #t spec ...))]))
        (lambda (input fld var options context)
          #`((bind #,var #t))))
      (match 3 [`(any ,3) 'ok]))
    "invalid match pattern `(any ,3)")
   ;; field specs must be ,id ,@id or [id pat opt ...]
   ;;  - ,@x where x not identifier
   (assert-syntax-error
    (let ()
      (define-match-extension any
        (lambda (v pattern)
          (syntax-case pattern (quasiquote)
            [`(any spec ...)
             #`((handle-fields #t spec ...))]))
        (lambda (input fld var options context)
          #`((bind #,var #t))))
      (match 3 [`(any ,@(thing)) 'ok]))
    "invalid match pattern `(any ,@(thing))")
   ;; field specs must be ,id ,@id or [id pat opt ...]
   ;;  - none of the above
   (assert-syntax-error
    (let ()
      (define-match-extension any
        (lambda (v pattern)
          (syntax-case pattern (quasiquote)
            [`(any spec ...)
             #`((handle-fields #t spec ...))]))
        (lambda (input fld var options context)
          #`((bind #,var #t))))
      (match 3 [`(any #(1 home)) 'ok]))
    "invalid match pattern `(any #(1 home))")
   ;; check that handle-object gets the expected pattern
   (let ()
     (meta define expected #'`(pat ,f1 ,@f2 [f3 3]))
     (define-match-extension pat
       (lambda (v pattern)
         (assert (equal? (syntax->datum pattern) (syntax->datum expected)))
         #'((guard #t))))
     (let-syntax ([inject
                   (lambda (x)
                     #`(match 123 [#,expected 5]))])
       (match-let* ([5 inject]) 'ok)))
   ;; specify only handle-object
   ;; - handle-object returns a bad bind
   (assert-syntax-error
    (let ()
      (define-match-extension bad-bind
        (lambda (v pattern)
          (assert (identifier? v))
          ;; attempting to bind the incoming v will fail in match-define
          (syntax-case pattern (quasiquote)
            [`(bad-bind) #`((bind #,v 17))])))
      (match-define `(bad-bind) 123)
      'ok)
    (re "multiple definitions for #[{][^}]*[}] in body"))
   ;; specify only handle-object
   ;;  - handle-object returns guard, but no bind
   (let ()
     (define-match-extension seq
       (lambda (v pattern)
         (assert (identifier? v))
         (syntax-case pattern (quasiquote)
           [`(seq) #`((guard (equal? #,v (next!))))])))
     (define n 0)
     (define (next!) (set! n (+ n 1)) n)
     (match-let* ([0 n]) 'ok)
     ;; use seq in match, testing left-to-right evaluation
     (match (<tres> make [a 1] [b 3] [c 2])
       [`(<tres> [a `(seq)] [c `(seq)] [b `(seq)]) 'ok]
       ;; more than one clause so we don't convert to match-let*
       [other (match 'bad)])
     ;; use seq in match-define
     (let ()
       (match-define `(seq) 4)
       (match-define `(seq) 5)
       'ok)
     ;; use seq in match-let*
     (match-let*
      ([`(seq) 6]
       [`(seq) 7]
       [(`(seq) `(seq) `(seq)) '(8 9 10)])
      'ok))
   ;; specify only handle-object
   ;; - handle-object returns multiple guards and a bind
   (let ()
     (define-match-extension even
       (lambda (v pattern)
         (syntax-case pattern (quasiquote)
           [`(even var)
            #`((guard (number? #,v))
               (guard (even? #,v))
               (bind var (list 'var #,v)))])))
     ;; use even in match
     (match (list 1 2 3 4)
       [(`(even w) `(even x) `(even y) `(even z)) (match 'wrong)]
       [(1 `(even x) 3 `(even y))
        (match-let* ([(x 2) x] [(y 4) y]) 'ok)])
     (match 5
       [`(even x) (match 'wrong)]
       [5 'ok])
     ;; use even in match-define
     (let ()
       (match-define `(even a) 20)
       (match-define (`(even b) `(even c)) '(10 4))
       (match-let*
        ([(a 20) a]
         [(b 10) b]
         [(c 4) c])
        'ok))
     (match-let*
      ([#(EXIT #(bad-match 3 ,_))
        (catch (let () (match-define `(even x) 3) x))])
      'ok)
     ;; use even in match-let*
     (match-let*
      ([#(`(even a) b `(even c) d) '#(8 b 2 d)]
       [(a 8) a]
       [(c 2) c])
      'ok)
     (match-let*
      ([#(EXIT #(bad-match 7 ,_))
        (catch (match-let* ([`(even h) 7]) 'ok))])
      'ok))
   ;; specify only handle-object
   ;; - handle-object returns sub-match
   (let ()
     (define-match-extension twin
       (lambda (v pattern)
         (syntax-case pattern (quasiquote unquote)
           [`(twin ,var ...)
            #`((sub-match #,v (#(,var ...) (,@var ...))))])))
     ;; use twin in match
     (match '(#(a b c) (a b c))
       [`(twin ,x ,y ,z)
        (match-let* ([a x] [b y] [c z]) 'ok)]
       [other (match 'bad)])
     (match '(#(0 1 2 3) (0 1 2 7))
       [`(twin ,w ,x ,y ,z) (match 'bad)]
       [,_ 'ok])
     (match '(#(0 1 2 3) (0 1 2))
       [`(twin ,w ,x ,y ,z) (match 'bad)]
       [,_ 'ok])
     ;; use twin in match-define
     (let ()
       (define (f x)
         (match-define `(twin ,a ,b ,c) x)
         (list a b c))
       (match-let*
        ([(1 2 3) (f '(#(1 2 3) (1 2 3)))]
         [(d e f) (f '(#(d e f) (d e f)))]
         [#(EXIT #(bad-match (#(a b c) (a b d)) ,_))
          (catch (f '(#(a b c) (a b d))))])
        'ok))
     ;; use twin in match-let*
     (let ()
       (define (f x)
         (match-let* ([`(twin ,x ,y) x])
           (list x y)))
       (match-let*
        ([(1 2) (f '(#(1 2) (1 2)))]
         [(b c) (f '(#(b c) (b c)))]
         [#(EXIT #(bad-match (#(r 2) (d 2)) ,_))
          (catch (f '(#(r 2) (d 2))))])
        'ok)))
   ;; specify only handle-object
   ;; - handle-object returns guard, bind, and sub-match
   (let ()
     (define-match-extension car
       (lambda (v pattern)
         (syntax-case pattern (quasiquote)
           [`(car pat)
            (with-temporaries (tmp)
              #`((guard (pair? #,v))
                 (bind tmp (car #,v))
                 (sub-match tmp pat)))])))
     (define (check f)
       (match-let*
        ([3 (f '((zoom 3) 4 5))]
         [#(EXIT #(bad-match "string" ,_)) (catch (f "string"))])
        'ok))
     ;; use car in match
     (check
      (lambda (x)
        (match x
          [`(car (zoom ,z)) z]
          [other (match 'bad)])))
     ;; use car in match-define
     (check
      (lambda (x)
        (match-define `(car (zoom ,z)) x)
        z))
     ;; use car in match-let*
     (check
      (lambda (x)
        (match-let* ([`(car (zoom ,z)) x])
          z))))
   ;; specify only handle-object
   ;; - handle-object returns handle-fields, but pattern has bad field syntax
   (assert-syntax-error
    (let ()
      (define-match-extension goo
        (lambda (v pattern)
          (syntax-case pattern (quasiquote)
            [`(goo spec ...)
             #`((handle-fields #,v spec ...))])))
      (match 3
        [`(goo) 7] ;; okay
        [`(goo fed) 8]))
    "invalid match pattern `(goo fed)")
   ;; specify only handle-object
   ;; - handle-object returns handle-fields and tries to convert viable fields
   ;;   but we didn't provide a handle-field procedure
   (match-let*
    ([#(EXIT ,reason)
      (catch
       (expand
        '(let ()
           (define-match-extension goo
             (lambda (v pattern)
               (syntax-case pattern (quasiquote)
                 [`(goo spec ...)
                  #`((handle-fields #,v spec ...))])))
           (lambda (x)
             (match x
               [`(goo ,foo) foo]
               [other (match 'bad)])))))]
     ["Exception in define-match-extension: no handle-field procedure provided for goo."
      (exit-reason->english reason)])
    'ok)
   ;; specify handle-object and handle-field
   ;; - handle-object returns sub-match, so handle-field never called
   (let ()
     (define-match-extension bool
       (lambda (v pattern)
         (syntax-case pattern (quasiquote)
           [`(bool pat)
            #`((guard (boolean? #,v))
               (sub-match #,v pat))]))
       (lambda x (match 'not-called)))
     (define (check f)
       (match-let*
        ([#t (f '(#t #f #t))]
         [#f (f '(#f #f #f))]
         [#(EXIT #(bad-match (yes no yes) ,_)) (catch (f '(yes no yes)))])
        'ok))
     ;; use bool in match
     (check
      (lambda (x)
        (match x
          [other (match 'bad)]
          [(`(bool ,var) `(bool #f) `(bool ,@var)) var])))
     ;; use bool in match-define
     (check
      (lambda (x)
        (match-define (`(bool ,var) `(bool #f) `(bool ,@var)) x)
        var))
     ;; use bool in match-let*
     (check
      (lambda (x)
        (match-let*
         ([(`(bool ,var) `(bool #f) `(bool ,@var)) x])
         var))))
   ;; specify handle-object and handle-field
   ;; - handle-object returns handle-fields to pass info to handle-field
   (let ()
     (define-match-extension idx
       (lambda (v pattern)
         (syntax-case pattern (quasiquote)
           [`(idx [i pat] ...)
            (andmap (lambda (x) (and (fixnum? x) (nonnegative? x)))
              (datum (i ...)))
            (let ([n (apply max (datum (i ...)))])
              (with-temporaries (len)
                #`((guard (vector? #,v))
                   (bind len (vector-length #,v))
                   (guard (< #,n len))
                   (handle-fields (#,v len) [i pat] ...))))]))
       (lambda (input i var options context)
         (syntax-case options () [() 'ok])
         (assert (fixnum? (syntax->datum i)))
         (syntax-case input ()
           [(v len)
            #`((bind #,var `(#,i of ,len = ,(vector-ref v #,i))))])))
     (define (check f)
       (match-let*
        ([((1 of 4 = A) (3 of 4 = B)) (f '#(0 A 2 B))]
         [((1 of 6 = D) (3 of 6 = E)) (f '#(0 D 2 E f g))]
         [#(EXIT #(bad-match #(a b c) ,_)) (catch (f '#(a b c)))])
        'ok))
     ;; use idx in match
     (check
      (lambda (x)
        (match x
          [other (match 'bad)]
          [`(idx [1 ,a] [3 ,bar]) (list a bar)])))
     ;; use idx in match-define
     (check
      (lambda (x)
        (match-define `(idx [1 ,a] [3 ,bar]) x)
        (list a bar)))
     ;; use idx in match-let*
     (check
      (lambda (x)
        (match-let* ([`(idx [1 ,a] [3 ,bar]) x])
          (list a bar)))))
   ;; invalid output in handle-object
   (assert-syntax-error
    (let ()
      (define-match-extension bad
        (lambda (v pattern)
          #'(bind var 3)))
      (match 123 [`(bad) 7]))
    "invalid handle-object output (bind var 3) in `(bad)")
   ;; invalid output in handle-field: sub-match
   (assert-syntax-error
    (let ()
      (define-match-extension worse
        (lambda (v pattern)
          #`((handle-fields #,v [x 11] ,y)))
        (lambda (input fld var options context)
          #'((bind is okay)
             (guard (is fine))
             (sub-match #,input 404))))
      (match 123 [`(worse) 7]))
    "invalid handle-field output ((sub-match #,input 404)) in `(worse)")
   ;; invalid output in handle-field: handle-fields
   (assert-syntax-error
    (let ()
      (define-match-extension worse
        (lambda (v pattern)
          #`((handle-fields #,v [x 11] ,y)))
        (lambda (input fld var options context)
          #'((bind is okay)
             (guard (is fine))
             (handle-fields #,input ,disallowed))))
      (match 123 [`(worse) 7]))
    "invalid handle-field output ((handle-fields #,input ,disallowed)) in `(worse)")
   ;; handle-fields is not final clause in handle-object
   (assert-syntax-error
    (let ()
      (define-match-extension worse
        (lambda (v pattern)
          #`((handle-fields #,v ,x)
             (bind var something))))
      (match 123 [`(worse) 7]))
    (re "invalid handle-object output [(][(]handle-fields #[{][^}]*[}] ,x[)] [(]bind var something[)][)] in `[(]worse[)]"))
   ;; sub-match is not final clause in handle-object
   (assert-syntax-error
    (let ()
      (define-match-extension worse
        (lambda (v pattern)
          #`((sub-match #,v 7)
             (guard door))))
      (match 123 [`(worse) 7]))
    (re "invalid handle-object output [(][(]sub-match #[{][^}]*[}] 7[)] [(]guard door[)][)] in `[(]worse[)]"))
   ;; return #f from handle-field to indicate invalid field
   (assert-syntax-error
    (let ()
      (define-match-extension bad-field
        (lambda (v pattern)
          #`((handle-fields #,v [secret agent])))
        (lambda (input fld var options context)
          #f))
      (match 3 [`(bad-field anything here) 7]))
    "unknown field secret in `(bad-field anything here)")
   ;; fail with syntax-case error in handle-field
   (assert-syntax-error
    (let ()
      (define-match-extension crash
        (lambda (v pattern)
          #`((handle-fields #,v [some 1])))
        (lambda (input fld var options context)
          (syntax-case 123 ())))
      (match 3 [`(crash anything here) 7]))
    "invalid syntax")
   ;; check that handle-field field and options are as expected
   (let ()
     (define-match-extension hfop
       (lambda (v pattern)
         (syntax-case pattern (quasiquote)
           [`(hfop spec ...)
            #`((handle-fields #,v spec ...))]))
       (lambda (input fld var options context)
         (let* ([actual (syntax->datum options)]
                [expected* '(() (chutney) (peanut butter cups))]
                [found
                 (ormap (lambda (o i) (and (equal? actual o) i))
                   expected*
                   (enumerate expected*))])
           (assert found)
           #`((bind #,var (cons #,found '#,fld))))))
     (match 'anything
       [`(hfop [a ,mango chutney] [dark ,chocolate peanut butter cups] [cheese ,cake])
        (match-let*
         ([(1 . a) mango]
          [(2 . dark) chocolate]
          [(0 . cheese) cake])
         'ok)]))
   ;; check that handle-field context is as expected
   (let ()
     (meta define expected
       #'`(hfc ,x [y ,not] [z z top] ,@now))
     (define-match-extension hfc
       (lambda (v pattern)
         (syntax-case pattern (quasiquote)
           [`(hfc spec ...)
            #`((handle-fields #,v spec ...))]))
       (lambda (input fld var options context)
         (assert (equal? (syntax->datum context) (syntax->datum expected)))
         #`((bind #,var '#,fld))))
     (let-syntax ([inject
                   (lambda (x)
                     #`(let ([now 'now] [z 'snoo])
                         (match 'anything
                           [#,expected
                            (match-let* ([x x] [y not] [snoo z] [now now])
                              'ok)])))])
       inject))
   ))

(mat uuid ()
  (match-let*
   ([#vu8(#x79 #x22 #x8E #x49 #x1 #x13 #x1E #x42 #x9A #x2E #x40
           #x1 #xAC #xF5 #x65 #xE5)
      (string->uuid "498e2279-1301-421E-9a2E-4001aCf565E5")]
    ["498E2279-1301-421E-9A2E-4001ACF565E5"
     (uuid->string #vu8(#x79 #x22 #x8E #x49 #x1 #x13 #x1E #x42 #x9A #x2E #x40
                         #x1 #xAC #xF5 #x65 #xE5))])
   'ok))

(isolate-mat gatekeeper ()
  (define (eval-test)
    (eval '(lambda ()
             (define-syntax (foo x)
               (receive [continue #'0]))
             (foo))))
  (define (exit-reason p)
    (receive (after 10000 (raise 'timeout)) [#(EXIT ,@p ,r) r]))

  (process-trap-exit #t)
  (gatekeeper:start&link)
  (let* ([sup self]
         [p1 (spawn&link
              (lambda ()
                (eval-test)
                (with-gatekeeper-mutex A 10000
                  (send sup 'p1-ready)
                  (receive [continue 'ok])
                  (with-gatekeeper-mutex B 10000
                    'ok))))]
         [p2 (spawn&link
              (lambda ()
                (eval-test)
                (with-gatekeeper-mutex B 10000
                  (send sup 'p2-ready)
                  (receive [continue 'ok])
                  (with-gatekeeper-mutex C 10000
                    (receive)))))]
         [p3 (spawn&link
              (lambda ()
                (eval-test)
                (with-gatekeeper-mutex C 10000
                  (send sup 'p3-ready)
                  (receive [continue 'ok])
                  (with-gatekeeper-mutex A 10000
                    'ok))))]
         [p4 (spawn&link
              (lambda ()
                (receive [continue 'ok])
                (with-gatekeeper-mutex A 10 'ok)))])
    (send p1 'continue)
    (send p2 'continue)
    (send p3 'continue)
    (receive [p1-ready 'ok])
    (receive [p2-ready 'ok])
    (receive [p3-ready 'ok])
    (send p1 'continue)
    (send p2 'continue)
    (send p3 'continue)
    (match-let*
     ([#(timeout #(gen-server call ,_))
       (begin (sleep-ms 10) (send p4 'continue) (exit-reason p4))]
      [#(EXIT #(unowned-resource A)) (catch (gatekeeper:leave 'A))]
      [#(EXIT #(unowned-resource D)) (catch (gatekeeper:leave 'D))]
      [#(deadlock A) (exit-reason p3)]
      [stop (begin (sleep-ms 10) (kill p2 'stop) (exit-reason p2))]
      [normal (exit-reason p1)]
      [shutdown (let ([pid (whereis 'gatekeeper)])
                  (kill pid 'shutdown)
                  (exit-reason pid))])
     'ok)))

(isolate-mat application-start-failure ()
  (repl-test 1
    '(application:start (lambda () '#(error kaboom!)))
    'seek
    "Event: #\\(application-start-failed kaboom!\\)"))

(isolate-mat create-watched-process-io ()
  (let-values
      ([(to-stdin from-stdout from-stderr os-pid)
        (spawn-os-process "sort" '() self)])
    (close-input-port from-stderr)
    (on-exit (close-input-port from-stdout)
      (on-exit (force-close-output-port to-stdin)
        (put-bytevector to-stdin (string->utf8 "berry\r\ncherry\r\napple\r\n")))
      (assert (string=? "apple\r\nberry\r\ncherry\r\n"
                (utf8->string (get-bytevector-all from-stdout))))
      (receive
       (after 1000
         (osi_kill* os-pid 15)
         (raise 'timeout))
       [#(process-terminated ,@os-pid ,_ ,_) 'ok])))
  (let-values
      ([(to-stdin from-stdout from-stderr os-pid)
        (spawn-os-process "sort" '() self)])
    (on-exit
     (begin
       (force-close-output-port to-stdin)
       (close-input-port from-stdout)
       (close-input-port from-stderr))
     (osi_kill os-pid 15)
     (receive
      [#(process-terminated ,@os-pid ,_ ,_) 'ok])
     (match-let*
      ([#(EXIT #(io-error ,_ ,_ ,@UV_EPIPE))
        (catch
         (put-u8 to-stdin 10)
         (flush-output-port to-stdin))]
       [(uv_kill . ,_) (osi_kill* os-pid 15)])
      'ok))))

;; on slower systems it may be helpful to limit the available virtual memory
;; e.g., via ulimit, so that this test does not time out while paging
(isolate-mat out-of-memory ()
  (repl-test 1
    '(make-vector (most-positive-fixnum))
    "out of memory"))

(isolate-mat exit-handler ()
  (define-syntax run-spawned
    (syntax-rules ()
      [(_ expr)
       (let ([me self])
         (spawn (lambda () (send me expr)))
         (receive (after 500 (exit 'fail)) [,val val]))]))
  (match-let*
   ([,me self]
    [#(EXIT 123) (run-spawned (catch (raise 123)))]
    [#(EXIT 123) (run-spawned (catch (exit 123)))])
   'ok)
  (repl-test 1
    '(begin
       (printf "attempting to app:start\n")
       (app-sup-spec
        `(#(event-mgr ,event-mgr:start&link permanent 1000 worker)))
       (app:start)
       (exit-handler
        (let ([orig (exit-handler)])
          (lambda args
            (printf "exited properly~{ ~s~}\n" args)
            (apply orig args))))
       (exit 1 2 3))
    "attempting to app:start"
    "exited properly 1 2 3"
    'seek
    "Date: "
    "Timestamp: "
    "Event: "))

(mat dump-stack ()
  (define (assert-depth s n)
    (let ([lines (split-n s #\newline (+ n 2))])
      (assert (string=? (list-ref lines n) (format "Stack dump truncated due to max-depth = ~s." n)))))
  (let-values ([(p get) (open-string-output-port)])
    (define (wrap n thunk)
      (if (= n 0)
          (thunk)
          (list (wrap (- n 1) thunk))))
    (parameterize ([current-output-port p])
      (wrap 100 dump-stack))
    (assert-depth (get) 10)
    (wrap 100 (lambda () (call/cc (lambda (k) (dump-stack k p 4)))))
    (assert-depth (get) 4)
    (wrap 100 (lambda () (call/cc (lambda (k) (dump-stack k p #f)))))
    (match (catch (match-prefix (split (get) #\newline) "Stack dump truncated due to max-depth ="))
      [#(EXIT #(pattern-not-found ,_)) 'ok])
    (match-let*
     ([#(EXIT #(bad-arg dump-stack tofu)) (catch (dump-stack 'tofu))]
      [#(EXIT #(bad-arg dump-stack tofu)) (catch (call/cc (lambda (k) (dump-stack k p 'tofu))))]
      [#(EXIT #(bad-arg dump-stack 2.3)) (catch (call/cc (lambda (k) (dump-stack k p 2.3))))]
      [#(EXIT #(bad-arg dump-stack -2)) (catch (call/cc (lambda (k) (dump-stack k p -2))))]
      [,bignum (+ (most-positive-fixnum) 1)]
      [#(EXIT #(bad-arg dump-stack ,@bignum)) (catch (call/cc (lambda (k) (dump-stack k p bignum))))])
     'ok)))
