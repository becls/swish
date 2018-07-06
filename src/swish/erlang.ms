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

(define-syntax assert-syntax-error
  (syntax-rules ()
    [(_ e expected) ($assert-syntax-error 'e expected)]))

(define ($assert-syntax-error e expected)
  (guard
   (x
    [(and (syntax-violation? x) (string=? (condition-message x) expected))
     'ok])
   (eval e)
   (errorf 'assert-syntax-error "failed to raise syntax error: ~s" e)))

(define (pid? x)
  (and (exact? x) (integer? x) (positive? x)))

(define (repl-test expected-exit code . patterns)
  (define scheme (or (getenv "SCHEME") "scheme"))
  (define scheme-args
    (list
     "--eedisable"
     "--program" "repl.ss"))
  (define (write-stdin op)
    (fprintf op "~s\n" code)
    (flush-output-port op))
  (define (launch)
    (match (vector->list (osi_get_argv))
      [()
       (run-os-process scheme scheme-args write-stdin 10000 '())]
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
   '(osi_list_directory "." (lambda (x) (receive (after 1000 'ok))))
   "Event: run-queue-empty"))

(isolate-mat panic-event-loop-process-terminated ()
  (repl-test 80
   '(begin
      (osi_list_directory "." (lambda (x) (raise 'kaboom)))
      (receive (after 10 (osi_exit 10))))
   "Event: #\\(event-loop-process-terminated kaboom\\)")
  (repl-test 80
   '(osi_list_directory "." (lambda (x) (kill self 'kill)))
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
      (receive (after 10 (osi_exit 10))))
   "Event: #\\(finalizer-process-terminated kaboom\\)")
  (repl-test 80
   '(begin
      (add-finalizer (lambda () (kill self 'kill)))
      (collect)
      (receive (after 1000 (osi_exit 1))))
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
  (assert-bad-match '(1 2) (match-let* ([,x (guard #f) '(1 2)]) x)))
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
  (assert-syntax-error (match-let* ([,@12 12]) 12) "invalid match pattern")
  (assert-syntax-error (match-let* ([(,x ,x) (list 3 4)]) x)
    "duplicate pattern variable")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (match 0 [`(<point> ,x ,x) x]))
   "duplicate pattern variable")
  (assert-syntax-error (match-let* ([`(no-record-type) 12]) 12)
    "unknown tuple type in pattern")
  (assert-syntax-error (match 0 [,() 0]) "invalid match pattern")
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
   "unknown field z in pattern")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (match 0 [`(<point> [z ,z]) z]))
   "unknown field z in pattern")
  (assert-syntax-error (match 0 [`((ooh!)) x]) "invalid match pattern")
  (assert-syntax-error (match 0 [#!eof 0]) "invalid match pattern"))

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
    '(1 2 3))))

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

(mat record ()
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
  (define (continue p1 p2 p3)
    (receive (after 10 (void)))
    (send p1 'continue)
    (send p2 'continue)
    (send p3 'continue))
  (define (exit-reason p)
    (receive (after 1000 (raise 'timeout)) [#(EXIT ,@p ,r) r]))

  (process-trap-exit #t)
  (gatekeeper:start&link)
  (let* ([p1 (spawn&link
              (lambda ()
                (eval-test)
                (with-gatekeeper-mutex A 1000
                  (receive [continue 'ok])
                  (with-gatekeeper-mutex B 1000
                    'ok))))]
         [p2 (spawn&link
              (lambda ()
                (eval-test)
                (with-gatekeeper-mutex B 1000
                  (receive [continue 'ok])
                  (with-gatekeeper-mutex C 1000
                    (receive)))))]
         [p3 (spawn&link
              (lambda ()
                (eval-test)
                (with-gatekeeper-mutex C 1000
                  (receive [continue 'ok])
                  (with-gatekeeper-mutex A 1000
                    'ok))))]
         [p4 (spawn&link
              (lambda ()
                (receive [continue 'ok])
                (with-gatekeeper-mutex A 10 'ok)))])
    (continue p1 p2 p3)
    (continue p1 p2 p3)
    (match-let*
     ([#(timeout #(gen-server call ,_))
       (begin (sleep-ms 10) (send p4 'continue) (exit-reason p4))]
      [#(EXIT #(unowned-resource A)) (catch (gatekeeper:leave 'A))]
      [#(EXIT #(unowned-resource D)) (catch (gatekeeper:leave 'D))]
      [#(deadlock A) (begin (continue p1 p2 p3) (exit-reason p3))]
      [stop (begin (sleep-ms 10) (kill p2 'stop) (exit-reason p2))]
      [normal (exit-reason p1)]
      [shutdown (let ([pid (whereis 'gatekeeper)])
                  (kill pid 'shutdown)
                  (exit-reason pid))])
     'ok)))

(isolate-mat application-start-failure ()
  (repl-test 1
   '(application:start (lambda () '#(error kaboom!)))
   "Event: #\\(application-start-failed kaboom!\\)"))

(isolate-mat create-watched-process-io ()
  (let-values
      ([(to-stdin from-stdout from-stderr os-pid)
        (spawn-os-process "sort" '() self)])
    (close-input-port from-stderr)
    (on-exit (close-input-port from-stdout)
      (on-exit (close-output-port to-stdin)
        (put-bytevector to-stdin (string->utf8 "berry\r\ncherry\r\napple\r\n")))
      (assert (string=? "apple\r\nberry\r\ncherry\r\n"
                (utf8->string (get-bytevector-all from-stdout))))
      (receive
       (after 1000
         (osi_kill* os-pid 15)
         (raise 'timeout))
       [#(<process-terminated> ,@os-pid ,_ ,_) 'ok]))))

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
   "exited properly 1 2 3"
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
