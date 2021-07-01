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
 (swish script-testing)
 (swish string-utils)
 (swish testing)
 )

(parameterize ([library-extensions '((".ss" . ".so"))])
  (eval '(import (swish internal-testing))))

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
       (throw `#(repl-test-unexpected-exit ,expected-exit ,exit-status)))
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
     [`(EXIT ,_ normal) 'ok]
     [`(EXIT ,_ ,reason) (throw reason)])))

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
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid live-free-or-die-hard) 'ok])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid live-free-or-die-hard) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'no-exit-from-link))
       [`(EXIT ,@pid2 ,r ,e)
        ;; the r and e patterns in `(EXIT ,_ ,r ,e) are matched against the
        ;; reason directly if it is not an exception trapped by try or catch
        (match-let* ([live-free-or-die-hard r] [,@r e])
          'ok)]))))

(isolate-mat suicide-normal ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (kill self 'normal)))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid normal) 'ok])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid normal) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'timeout))
       [`(EXIT ,@pid2 link-no-fail) 'ok]))))

(isolate-mat suicide-throw-normal ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (kill self (try (throw 'normal)))))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid normal) 'ok])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid normal) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'timeout))
       [`(EXIT ,@pid2 link-no-fail) 'ok]))))

(isolate-mat kill-normal ()
  (let ([pid (spawn&link (lambda () (receive [(word-from ,p) (send p 'roger)])))])
    (kill pid 'normal)
    (send pid `(word-from ,self))
    (receive (after 1000 (throw 'normal-should-not-kill))
      [roger 'ok])))

(isolate-mat kill-throw-normal ()
  (let ([pid (spawn&link (lambda () (receive [(word-from ,p) (send p 'roger)])))])
    (kill pid (try (throw 'normal)))
    (send pid `(word-from ,self))
    (receive (after 1000 (throw 'normal-should-not-kill))
      [roger 'ok])))

(isolate-mat kill-fault-normal ()
  (let ([pid (spawn&link (lambda () (receive [(word-from ,p) (send p 'roger)])))])
    (kill pid (make-fault 'normal))
    (send pid `(word-from ,self))
    (receive (after 1000 (throw 'normal-should-not-kill))
      [roger 'ok])))

(isolate-mat link-native ()
  (define (assert-expected e)
    (match (and (condition? e) (exit-reason->english e))
      ["Exception in cdr: 7 is not a pair." 'ok]))
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (cdr 7)))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid ,c) (assert-expected c)])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid ,c) (assert-expected c)])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'no-exit-from-link))
       [`(EXIT ,@pid2 ,c) (assert-expected c)]))))

(isolate-mat link-raise ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (raise 'commotion)))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid commotion) 'ok])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid commotion) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'no-exit-from-link))
       [`(EXIT ,@pid2 commotion)
        'ok]))))

(isolate-mat link-raise-catch ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (raise (catch (raise 'ruckus)))))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid ruckus ruckus) 'ok])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid ruckus) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'no-exit-from-link))
       [`(EXIT ,@pid2 ruckus)
        'ok]))))

;; like suicide-normal, but with raise in place of kill
(isolate-mat link-raise-normal ()
  (process-trap-exit #t)
  (let ([pid0 (spawn&link (lambda () (raise 'normal)))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid0 normal) 'ok])
    (let ([pid1
           (spawn&link
            (lambda ()
              (link pid0)
              (raise 'awareness)))])
      (receive
       (after 100 (throw 'wrong-exit-reason))
       [`(EXIT ,@pid1 awareness) 'ok]))))

(isolate-mat link-throw ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (throw 'fit)))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid fit ,e)
      (match-let*
       ([`(catch fit ,@e) e]
        [(,k) (exit-reason->stacks e)])
       'ok)])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid fit) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'no-exit-from-link))
       [`(EXIT ,@pid2 fit)
        'ok]))))

(isolate-mat link-throw-try ()
  (process-trap-exit #t)
  (let ([pid (spawn&link (lambda () (throw (try (throw 'away)))))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid away ,e)
      (match-let*
       ([`(catch away ,@e) e]
        [(,k0 ,k1) (exit-reason->stacks e)])
       'ok)])
    ;; link to a dead process should immediately give EXIT message
    (link pid)
    (receive
     (after 0 (throw 'no-exit-from-link))
     [`(EXIT ,@pid away) 'ok])
    ;; link to a dead process without trap-exit should die
    (let ([pid2 (spawn&link (lambda () (link pid) (throw 'link-no-fail)))])
      (receive
       (after 10 (throw 'no-exit-from-link))
       [`(EXIT ,@pid2 away)
        'ok]))))

;; like suicide-normal, but with throw in place of kill
(isolate-mat link-throw-normal ()
  (process-trap-exit #t)
  (let ([pid0 (spawn&link (lambda () (throw 'normal)))])
    (receive
     (after 1000 (throw 'timeout))
     [`(EXIT ,@pid0 normal) 'ok])
    (let ([pid1
           (spawn&link
            (lambda ()
              (link pid0)
              (throw 'party)))])
      (receive
       (after 100 (throw 'wrong-exit-reason))
       [`(EXIT ,@pid1 party) 'ok]))))

(isolate-mat default-parameters ()
  (match-let*
   ([#f (process-trap-exit)]
    [,x (guard (pid? x)) (process-id)]
    [,@x (process-id self)]
    [#f (process-name)])
   'ok))

(isolate-mat bad-args ()
  (match-let*
   ([#(EXIT #(bad-arg add-finalizer 12)) (catch (add-finalizer 12))]
    [#(EXIT #(bad-arg command-line "abc")) (catch (command-line "abc"))]
    [#(EXIT #(bad-arg command-line-arguments "xyz"))
     (catch (command-line-arguments "xyz"))]
    [#(EXIT #(bad-arg complete-io breakfast)) (catch (complete-io 'breakfast))]
    [#(EXIT #(bad-arg custom-port-buffer-size -1))
     (catch (custom-port-buffer-size -1))]
    [#(EXIT #(bad-arg demonitor 12)) (catch (demonitor 12))]
    [#(EXIT #(bad-arg exit-handler 12)) (catch (exit-handler 12))]
    [#(EXIT #(bad-arg uuid->string #f)) (catch (uuid->string #f))]
    [#(EXIT #(bad-arg uuid->string #vu8(1 2 3)))
     (catch (uuid->string #vu8(1 2 3)))]
    [#(EXIT #(bad-arg inherited-parameters 12))
     (catch (inherited-parameters 12))]
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
    [#(EXIT #(bad-arg print-precision -3)) (catch (print-precision -3))]
    [#(EXIT #(bad-arg print-radix 1)) (catch (print-radix 1))]
    [#(EXIT #(bad-arg process-id 12)) (catch (process-id 12))]
    [#(EXIT #(bad-arg global-process-id 12)) (catch (global-process-id 12))]
    [#(EXIT #(bad-arg process-name 12)) (catch (process-name 12))]
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
    [#(EXIT #(bad-arg waiter-prompt-string 12))
     (catch (waiter-prompt-string 12))]
    [#(EXIT #(bad-arg whereis 12)) (catch (whereis 12))]
    [#(EXIT #(bad-arg who arg)) (catch (bad-arg 'who 'arg))]
    [#(EXIT #(bad-arg walk-stack-max-depth -1))
     (catch (walk-stack-max-depth -1))]
    [#(EXIT #(bad-arg with-process-details polychlorinated-byphenyl))
     (catch (with-process-details 'polychlorinated-byphenyl values))]
    [#(EXIT #(bad-arg with-process-details "spill beans"))
     (catch (with-process-details self "spill beans"))]
    [#(EXIT #(bad-arg make-inherited-parameter rain))
     (catch (make-inherited-parameter 'gentle 'rain))]
    )
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
    (receive [`(DOWN ,_ ,@c-pid ,_) 'ok])
    (match-let*
     ([#(EXIT #(process-already-registered ,@a-name))
       (catch (register (gensym "A") a-pid))]
      [#(EXIT #(name-already-registered ,@a-pid))
       (catch (register a-name (spawn void)))]
      [#(EXIT #(process-dead ,@c-pid)) (catch (register (gensym "C") c-pid))]
      [,@a-pid (whereis a-name)]
      [,@a-name (process-name a-pid)]
      [,@b-pid (whereis b-name)]
      [,@b-name (process-name b-pid)]
      [,names (get-registered)])
     (assert (find (lambda (x) (eq? x a-name)) names))
     (assert (find (lambda (x) (eq? x b-name)) names))
     (send a-name `#(ping ,self))
     (receive
      (after 100 (throw 'timeout))
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
       (receive (after timeout (throw 'timeout))))))
  (define (start-until timeout)
    (spawn&link
     (lambda ()
       (receive (until (+ (erlang:now) timeout) (throw 'timeout))))))
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
    [,other (throw `#(bad-message ,other))])))

(isolate-mat receive-until-0 ()
  (send self 'x)
  (send self 'y)
  (match-let*
   ([timeout (receive (until 0 'timeout) [z 'ok])]
    [,_ (send self 'w)]
    [timeout (receive (until 0 'timeout))])
   (receive
    [x 'ok]
    [,other (throw `#(bad-message ,other))])))

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
       (osi_list_directory "." (lambda (x) (throw 'kaboom)))
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
       (add-finalizer (lambda () (throw 'kaboom)))
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

(isolate-mat chain-reaction (settings [process-kill-delay 0])
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
          [#(die raise ,reason) (raise reason)]
          [#(die throw ,reason) (throw reason)]
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
           (receive (after 1000 (throw 'timeout-syncing))
             [#(sync ,@pid) 'ok]))
         (list A B C))
        `#(,A ,B ,C))))
  (define (flush)
    (receive (after 0 'ok)
      [,_ (flush)]))
  (process-trap-exit #t)
  (match-let*
   (
    [#(,a ,b ,c) (start #f `#(die raise abc))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(die throw abc))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(die raise normal))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(die throw normal))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(divide 0))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b ,cc))
           (guard (condition? cc))
           'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(die raise kill))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b kill)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f `#(die throw kill))]
    [#f (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b kill)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die raise abc))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die throw abc))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die raise normal))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c normal)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die throw normal))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c normal)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'normal)]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c normal)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die raise kill))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c kill)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t `#(die throw kill))]
    [#t (process-alive? b 0)]
    [#f (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c kill)) 'ok])]
    [ok (flush)])
   'ok))

(isolate-mat chain-reaction2 (settings [process-kill-delay 0])
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
           (receive (after 1000 (throw 'timeout-syncing))
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
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b abc)) 'ok])]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@c `(EXIT ,@b abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #f 'normal)]
    [#t (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (flush)]

    [#(,a ,b ,c) (start #f 'kill)]
    [#f (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b killed)) 'ok])]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@c `(EXIT ,@b killed)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'abc)]
    [#t (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c abc)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'normal)]
    [#t (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@b `(EXIT ,@c normal)) 'ok])]
    [ok (flush)]

    [#(,a ,b ,c) (start #t 'kill)]
    [#f (process-alive? b 0)]
    [#t (process-alive? c 0)]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@a `(EXIT ,@b killed)) 'ok])]
    [ok (receive (after 1000 (throw 'timeout))
          [#(msg-from ,@c `(EXIT ,@b killed)) 'ok])]
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
      (after 1000 (throw 'timeout))
      [`(EXIT ,@pid normal) 'ok]))
   (list (spin) (spin) (spin) (spin) (spin))))

(isolate-mat process-parameter-collection ()
  (define g (make-guardian))
  (define (salvaged)
    (let ([x (g)])
      (if x (cons x (salvaged)) '())))
  ;; We'll collect drop-param while processes are still live and verify that
  ;; its values were collected.
  (define drop-param (make-process-parameter 'drop))
  ;; We'll collect after processes have exited and verify that values of
  ;; keep-param were collected.
  (define keep-param (make-process-parameter 'keep))
  (define me self)
  (define (test-params val)
    (spawn&link
     (lambda ()
       (match-let*
        ([drop (drop-param)]
         [keep (keep-param)]
         [,v1 `(for-drop-param ,val)]
         [,_ (g v1)]
         [,_ (drop-param v1)]
         [,v2 `(for-keep-param ,val)]
         [,_ (g v2)]
         [,_ (keep-param v2)]
         [(for-drop-param ,@val) (drop-param)]
         [(for-keep-param ,@val) (keep-param)])
        (send me self)
        (receive (after 1000 (throw 'timeout))
          [drop-param-was-dropped 'ok])
        ;; Once the drop-param parameter is collected, the guardian should
        ;; report that the value we assigned to drop-param was collectible,
        ;; but the value we assigned to keep-param should still be live.
        (receive (after 1000 (throw 'timeout))
          [(for-drop-param ,@val) 'ok]
          [(for-keep-param ,@val) (throw 'keep-param-premature-demise)])
        (send me `(,self done))))))
  (g drop-param)
  (let* ([vals (iota 10)]
         [pids (map test-params vals)]
         [deadline (+ (erlang:now) 1000)])
    ;; wait for children to be ready
    (for-each
     (lambda (pid)
       (receive (until deadline (throw 'timeout))
         [,@pid 'ok]))
     pids)
    ;; drop drop-param and verify that it was spared from collection
    (match-let*
     ([,_ (set! drop-param #f)]
      [,_ (gc)]
      [(,p)
       (guard (procedure? p))
       (salvaged)]
      [drop (p)])
     'ok)
    ;; collect drop-param and verify that its values were collectible
    (match-let*
     ([,_ (gc)]
      [,_ (for-each (lambda (pid) (send pid 'drop-param-was-dropped)) pids)]
      ;; collect again so that drop-param values are collectible
      [,_ (gc)]
      [,collectible (salvaged)]
      [,_ (for-each
           (lambda (pid)
             (for-each (lambda (spared) (send pid spared))
               collectible))
           pids)]
      [,deadline (+ (erlang:now) 1000)]
      [,_ (for-each
           (lambda (pid)
             (receive (until deadline (throw 'timeout))
               [(,@pid done) 'ok]))
           pids)])
     'ok)
    ;; collect again now that the processes have exited (but are all still live)
    ;; and verify that the values registered for keep-param were collectible
    (match-let*
     ([,_ (gc)]
      [keep (keep-param)] ;; ensure that keep-param is live across gc
      [,collectible (salvaged)]
      [,@vals
       (sort <
         (map (lambda (x) (match x [(for-keep-param ,val) val]))
           collectible))]
      [#t (andmap process? pids)]) ;; ensure pids are live across gc
     'ok)))

(isolate-mat process-parent ()
  (define me self)
  (spawn
   (lambda ()
     (send me `#(t1 ,(process-parent)))))
  (spawn
   (lambda ()
     (spawn
      (lambda ()
        (do () ((not (process-parent))) (gc))
        (send me `#(t2 ,(process-parent)))))))
  (receive (after 1000 (throw 'timeout))
    [#(t1 ,@me) 'ok])
  (receive (after 1000 (throw 'timeout))
    [#(t2 #f) 'ok]))

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
       (receive (after 1000 (throw 'timeout))
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
       (receive (after 1000 (throw 'timeout))
         [(,@pid ,child)
          (receive (after 1000 (throw 'timeout))
            [(,@child . ,10x)
             (map (lambda (x) (/ x 10)) 10x)])])]
      ;; changing parameters in parent process does not affect child once spawned
      [,pid
       (spawn&link
        (lambda ()
          (receive (after 1000 (throw 'timeout))
            [ready (send me `(,self ,@(map get ips)))])))]
      [,@expected
       (begin
         (for-each (lambda (p) (p 'nut)) ips)
         (send pid 'ready)
         (receive (after 1000 (throw 'timeout))
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
       'ok)))
  (parameterize ([inherited-parameters '()])
    ;; test inherited parameter with filter
    (match-let*
     ([`(catch #(bad-arg good one))
       ;; be sure to call filter on the initial value
       (try
        (make-inherited-parameter 'one
          (lambda (x)
            (bad-arg 'good x))))]
      [,call-count 0]
      [,p (make-inherited-parameter 7
            (lambda (n)
              (set! call-count (+ call-count 1))
              (unless (number? n) (bad-arg 'p n))
              n))]
      [1 call-count]
      [7 (p)]
      [1 call-count]
      [,_ (p 11)]
      [11 (p)]
      [2 call-count]
      ;; filter rejects invalid value
      [`(catch #(bad-arg p five)) (try (p 'five))]
      [11 (p)]
      [3 call-count]
      ;; check that we do not call filter in spawn and friends
      [,pid (spawn values)]
      [,pid (spawn&link values)]
      [3 call-count]
      [,me self]
      [,pid (spawn (lambda () (p 100) (send me (cons self (p)))))]
      [(,@pid . 100) (receive [,x x])]
      [4 call-count]
      [,pid (spawn&link (lambda () (p 200) (send me (cons self (p)))))]
      [(,@pid . 200) (receive [,x x])]
      [5 call-count]
      )
     'ok)))

(isolate-mat monitor-raise ()
  (process-trap-exit #t)
  (let* ([host self]
         [pid (spawn&link
               (lambda ()
                 (if (monitor? (monitor host))
                     (raise 'success!)
                     (raise 'bad-monitor))))]
         [m (monitor pid)])
    (receive
     (after 10 (throw 'timeout))
     [`(DOWN ,@m ,@pid success! success!) 'ok])))

(isolate-mat monitor-raise-catch ()
  (process-trap-exit #t)
  (let* ([host self]
         [pid (spawn&link
               (lambda ()
                 (raise
                  (catch
                   (if (monitor? (monitor host))
                       (raise 'success!)
                       (raise 'bad-monitor))))))]
         [m (monitor pid)])
    (receive
     (after 10 (throw 'timeout))
     [`(DOWN ,@m ,@pid success! success!) 'ok])))

(isolate-mat monitor-throw ()
  (process-trap-exit #t)
  (let* ([host self]
         [pid (spawn&link
               (lambda ()
                 (if (monitor? (monitor host))
                     (throw 'success!)
                     (throw 'bad-monitor))))]
         [m (monitor pid)])
    (receive
     (after 10 (throw 'timeout))
     [`(DOWN ,@m ,@pid success! ,err)
      (match-let*
       ([`(catch success! ,@err) err]
        [(,k) (exit-reason->stacks err)])
       'ok)])))

(isolate-mat monitor-throw-try ()
  (process-trap-exit #t)
  (let* ([host self]
         [pid (spawn&link
               (lambda ()
                 (throw
                  (try
                   (if (monitor? (monitor host))
                       (throw 'party)
                       (throw 'bad-monitor))))))]
         [m (monitor pid)])
    (receive
     (after 10 (throw 'timeout))
     [`(DOWN ,@m ,@pid party ,e)
      (match-let* ([`(catch party ,@e) e]
                   [(,k0 ,k1) (exit-reason->stacks e)])
        'ok)])))

(isolate-mat pps ()
  (define (check-output patterns with-port)
    (let-values ([(p get) (open-string-output-port)])
      (with-port p)
      (match-regexps patterns (split (get) #\newline))))
  (define (test patterns)
    (check-output patterns (lambda (p) (pps p)))
    (check-output patterns
      (lambda (p)
        (parameterize ([current-output-port p])
          (pps)))))
  (define (test-spawn thunk state-match? pattern-suffix)
    (define pid (and thunk (spawn thunk)))
    (on-exit (when pid (kill pid 'kill))
      (define id (process-id (or pid self)))
      (define pattern (format " +~a: ~a" id pattern-suffix))
      (when (and pid state-match?)
        (wait-for-state pid state-match?))
      (test `("Processes:" seek ,pattern ""))))
  (define (wait-for-state pid match?)
    (let wait ([n 1])
      (if (> n 10)
          (throw 'timeout)
          (with-process-details pid
            (lambda (id name create-time state)
              (unless (match? state)
                (receive (after n (wait (+ n 1))))))))))
  (define parent self)
  (test
   `("Processes:"
     " +1: waiting for up to [.0-9]+s at char [0-9]+ of .*testing.ss, spawned [0-9]+"
     " +2: ready to run, spawned [0-9]+"
     ;; finalizer may be waiting or ready to run
     " +3: (?:waiting indefinitely at char [0-9]+ of .*[/\\\\]erlang.ss|ready to run), spawned [0-9]+"))
  (test-spawn #f #f "running, spawned [0-9]+")
  ;; named
  (register 'named self)
  (test-spawn #f #f "named running, spawned [0-9]+")
  ;; dead
  (test-spawn
   (lambda () (car 12))
   (lambda (state)
     (match state
       [#(exited ,_) 'ok]
       [,_ #f]))
   "exited with reason #<compound condition: Exception in car: 12 is not a pair>, spawned [0-9]+")
  ;; no-src
  (test-spawn
   (lambda () (eval '(receive (after 60000 'ok))))
   (lambda (state)
     (match state
       [#(sleep-up-to ,_ ,_) 'ok]
       [,_ #f]))
   "waiting for up to [.0-9]+s, spawned [0-9]+")
  ;; normal; covers a case in @thunk->cont
  (test-spawn exit
    (lambda (state)
      (match state
        [#(exited ,_) 'ok]
        [,_ #f]))
    "exited with reason normal, spawned [0-9]+")
  ;; forever
  (process-trap-exit #t)
  (test-spawn
   (lambda () (link parent) (receive (until 'infinity 'ok)))
   (lambda (state)
     (match state
       [#(waiting-indefinitely ,_) 'ok]
       [,_ #f]))
   "waiting indefinitely at char [0-9]+ of .*[/\\\\]erlang.ms, spawned [0-9]+")
  ;; waiting for io
  (let ([pid (spawn
              (lambda ()
                (link parent)
                (wait-for-io 'godot)
                (receive (after 0 (throw 'bad))
                  [for-coverage (send parent 'ok)])
                (receive)))])
    (wait-for-state pid
      (lambda (state)
        (match state
          [#(waiting-for godot) 'ok]
          [,_ #f])))
    (test
     `("Processes:" seek
       ,(format " +~a: waiting for godot, spawned [0-9]+"
          (process-id pid))))
    ;; covers case in @send
    (send pid 'for-coverage)
    (complete-io pid)
    (receive (after 1000 (throw 'failed))
      [ok 'ok])
    (kill pid 'abandoned)
    (test
     `("Processes:" seek
       ,(format " +~a: exited with reason abandoned, spawned [0-9]+"
          (process-id pid))))))

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
  (assert-syntax-error (define-tuple <point> (x y)) "invalid field")
  (assert-syntax-error (define-tuple <point> make) "invalid field")
  (assert-syntax-error (define-tuple <point> copy) "invalid field")
  (assert-syntax-error (define-tuple <point> copy*) "invalid field")
  (assert-syntax-error (define-tuple <point> is?) "invalid field")
  (assert-syntax-error (define-tuple <point> x x) "duplicate field")
  (assert-syntax-error
   (let ()
     (define-tuple <point> x y)
     (<point> is? "sharp" "end"))
   "invalid syntax")
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
     (<point> copy* (<point> make [x 0] [y 0]) [bogus 1] [x 2]))
   "unknown field")
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
  (assert-syntax-error
   (lambda (p)
     (define-tuple <point> x y)
     (<point> open p [51 mustang])
     p)
   "invalid field")
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
         (throw 'not-reached)
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
         (throw 'not-reached)
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
  (match (read (open-input-string "")) [bad (throw 'wrong)] [#!eof 'ok])
  (match-let* ([#!eof (read (open-input-string ""))]) 'ok)
  (let ()
    (match-define #!eof (read (open-input-string "")))
    'ok)
  (match (list (vector (eof-object)))
    [#!eof (throw 'wrong)]
    [#(#!eof) (throw 'wrong)]
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
  (let ()
    (define-tuple <point> x y)
    (define expected '(#t #f #f #f #f #f #f #f #f #f #f))
    (define inputs
      (list (<point> make [x 1] [y 2]) #f #t 27 #\a "point" 'point '(point) '#(<point>) '#(<point> 1) '#(<point> 1 2 3)))
    (assert (equal? expected (map (lambda (p) (<point> is? p)) inputs)))
    (assert (equal? expected (map (<point> is?) inputs))))
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
   (assert-syntax-error
    (let ()
      ;; match needs the rtd at expand time, but define-record-type does not
      ;; construct the rtd at expand time because the record type is generative,
      ;; i.e., it lacks the (nongenerative) clause; for details, see
      ;; Chez Scheme's s/syntax.ss.
      (define-record-type foo (fields a))
      (match 123 [`(foo) #f]))
    "unknown type foo in `(foo)")
   (let ()
     ;; define-record-type creates rtd at expand time; match finds it
     (define-record-type foo (nongenerative) (fields a))
     (match (make-foo 1) [`(foo) #t]))
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
               (receive (after 1000 (throw 'timeout))
                 [quit 'ok]
                 [(send ,who ,what) (send who what) (f)]))))])
     (define-record-type one (nongenerative) (fields x y))
     (define-record-type two (nongenerative) (parent one) (fields y z))
     ;; vector with some nested stuff
     (send pid `(send ,self #(marmoset ((lamp) ginger) corn)))
     (receive (after 100 (throw 'timeout))
       [#(marmoset ((lamp) ,biscuit) ,chowder)
        (match-let* ([ginger biscuit] [corn chowder])
          'ok)])
     ;; native record
     (send pid `(send ,self ,(make-one 'hundred "percent")))
     (receive (after 100 (throw 'timeout))
       [`(one ,x ,y) (match-let* ([hundred x] ["percent" y]) 'ok)])
     ;; native record, inheritance
     (send pid `(send ,self ,(make-two 'many 'fold "home" "improvement")))
     (receive (after 100 (throw 'timeout))
       [`(one ,x ,y) (match-let* ([many x] [fold y]) 'ok)])
     ;; native record, inheritance
     (send pid `(send ,self ,(make-two 'many 'fold "home" "improvement")))
     (receive (after 100 (throw 'timeout))
       [`(two ,x ,y) (match-let* ([many x] ["home" y]) 'ok)])
     ;; native record, restricted match
     (send pid `(send ,self ,(make-two 'many 'fold "home" '#("improvement"))))
     (receive (after 100 (throw 'timeout))
       [`(one ,x [y "home"]) (throw "don't match one's y with two's y value")]
       [`(two [y fold]) (throw "don't match two's y with one's y value")]
       [`(two ,x ,y [z (,val)]) (throw "z val is vector not list")]
       [`(two ,x ,y [z #(,val)]) (match-let* ([many x] ["home" y] ["improvement" val]) 'ok)])
     ;; nested record match
     (send pid `(send ,self #((one ,(make-one 1 2)) (two ,(make-two 8 7 5 4)))))
     (receive (after 100 (throw 'timeout))
       [(,a ,b) (throw "should be vector")]
       [#((two ,_) (one ,_)) (throw "wrong order")]
       [#((one `(one ,x ,y)) (two `(two [x ,two.x] [y ,two.y] ,z)))
        (match-let* ([1 x] [2 y] [8 two.x] [5 two.y] [4 z]) 'ok)])
     ;; nested record match, inheritance case
     (send pid `(send ,self ,(make-one (make-two 1 2 3 4) (make-one 5 (make-one 6 7)))))
     (receive (after 100 (throw 'timeout))
       [`(one [x `(one ,x ,y)] [y `(one [x 5] [y `(one [x ,x2] [y ,y2])])])
        (match-let* ([1 x] [2 y] [6 x2] [7 y2]) 'ok)])
     ;; nested record match, another inheritance case
     (send pid `(send ,self ,(make-one (make-two 1 2 3 4) (make-two 5 (make-one 16 17) 11 12))))
     (receive (after 100 (throw 'timeout))
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
    (receive (after 10000 (throw 'timeout)) [`(EXIT ,@p ,r) r]))

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
         (throw 'timeout))
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
    [#(EXIT 123) (run-spawned (catch (throw 123)))]
    [#(EXIT 123) (run-spawned (catch (exit 123)))])
   'ok)
  (repl-test 1
    '(begin
       (printf "attempting to app:start\n")
       (app-sup-spec
        `(#(event-mgr ,event-mgr:start&link permanent 1000 worker)))
       (log-file ":memory:")
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

;; generate predictable source annotations for stack dump tests
(define (compile-and-load file i? s?)
  (parameterize ([generate-inspector-information i?]
                 [generate-procedure-source-information s?])
    (compile-file file)
    (load (string-append (path-root file) ".so"))))

(mat dump-stack ()
  (define (assert-depth s n)
    (let ([lines (split-n s #\newline (+ n 2))])
      (assert (string=? (list-ref lines n) (format "Stack dump truncated due to max-depth = ~s." n)))))
  (define (assert-not-truncated s)
    (match (catch (match-prefix (split s #\newline) "Stack dump truncated due to max-depth ="))
      [#(EXIT #(pattern-not-found ,_)) 'ok]))
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
    (wrap 100 (lambda () (call/cc (lambda (k) (dump-stack k p 1)))))
    (assert-depth (get) 1)
    (match-let* ([,void-object (void)]
                 [(,@void-object)
                  (wrap 1
                    (lambda ()
                      (call/cc
                       (lambda (k)
                         (dump-stack k p #f)))))])
      (assert-not-truncated (get)))
    (wrap 100 (lambda () (call/cc (lambda (k) (dump-stack k p #f)))))
    (assert-not-truncated (get))
    (match-let*
     ([#(EXIT #(bad-arg dump-stack tofu)) (catch (dump-stack 'tofu))]
      [#(EXIT #(bad-arg dump-stack tofu)) (catch (call/cc (lambda (k) (dump-stack k p 'tofu))))]
      [#(EXIT #(bad-arg dump-stack 2.3)) (catch (call/cc (lambda (k) (dump-stack k p 2.3))))]
      [#(EXIT #(bad-arg dump-stack -2)) (catch (call/cc (lambda (k) (dump-stack k p -2))))]
      [#(EXIT #(bad-arg dump-stack 0)) (catch (call/cc (lambda (k) (dump-stack k p 0))))]
      [,bignum (+ (most-positive-fixnum) 1)]
      [#(EXIT #(bad-arg dump-stack ,@bignum)) (catch (call/cc (lambda (k) (dump-stack k p bignum))))])
     'ok)))

(mat walk-stack ()
  (define file1.ss
    (write-test-file "file1.ss"
      (lambda ()
        (printf ";; line 1\n")
        (printf "(define (proc1 n x quit)\n")
        (printf "  (if (< n x)\n")
        (printf "      (proc2 (+ n 2) x quit)\n")
        (printf "      (let* ([v1 n] [v2 x])\n")
        (printf "        (list v1 v2 n x (proc1 (- n 1) x quit)))))\n"))))
  (define file2.ss
    (write-test-file "file2.ss"
      (lambda ()
        (printf "(define (proc2 n x quit)\n")
        (printf "  (if (< n x)\n")
        (printf "      (proc3 (+ n 1) x quit)\n")
        (printf "      (let ([tmp (cons (+ 7 n) x)])\n")
        (printf "        (list tmp n x (proc2 (- n 1) x quit)))))\n"))))
  (define file3.ss
    (write-test-file "file3.ss"
      (lambda ()
        (printf "(define (proc3 n x quit)\n")
        (printf "  (if (< n x)\n")
        (printf "      (proc4 (+ n 2) x quit)\n")
        (printf "      (let ([zap (+ n x)])\n")
        (printf "        (list zap n x (proc3 (- n 1) x quit)))))\n"))))
  (define file4.ss
    (write-test-file "file4.ss"
      (lambda ()
        (printf "(define (proc4 n x quit)\n")
        (printf "  (if (< n x)\n")
        (printf "      (quit)\n")
        (printf "      (let* ([xyz (+ n x 65)] [pdq (integer->char xyz)])\n")
        (printf "        (list xyz pdq (proc4 (- n 1) x quit)))))\n"))))
  (define (get-source x)
    (and (source-object? x)
         (call-with-values
           (lambda ()
             ;; don't cache the result, since that will break
             ;; coverage for the dump-stack "at offset" case
             (locate-source-object-source x #t #f))
           list)))
  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
  (define (sort-vars als)
    (sort (lambda (a b) (symbol<? (car a) (car b)))
      als))
  (parameterize ([compile-file-message #f]
                 ;; prevent compiler from optimizing away v1, v2, tmp, etc.
                 [run-cp0 (lambda (f x) x)])
    (compile-and-load file1.ss #t #t)
    (compile-and-load file2.ss #f #t)
    (compile-and-load file3.ss #t #f)
    (compile-and-load file4.ss #f #f))
  (match-let*
   ([#(EXIT #(bad-arg walk-stack 7)) (catch (walk-stack 3 5 7 values))]
    [#(EXIT #(bad-arg walk-stack 9)) (catch (walk-stack 3 5 values 9))]
    [#(EXIT #(bad-arg walk-stack 6))
     (catch (walk-stack 3 5 values values 6 7 values))]
    [#(EXIT #(bad-arg blort -7))
     (catch (walk-stack 3 5 values values 'blort -7 values))]
    [#(EXIT #(bad-arg blort #t))
     (catch (walk-stack 3 5 values values 'blort #t values))]
    [#(EXIT #(bad-arg blort 3.4))
     (catch (walk-stack 3 5 values values 'blort 3.4 values))]
    [#(EXIT #(bad-arg blort 8))
     (catch (walk-stack 3 5 values values 'blort 'default 8))]
    [tofu (walk-stack "not a stack" 'tofu
            (lambda x (throw 'should-not-call))
            (lambda y (throw 'should-not-call)))]
    [,k (call/cc
         (lambda (return)
           ((top-level-value 'proc1) 3 0 (lambda () (call/cc return)))))]
    [(,frame0 ,frame1 ,frame2 ,frame3 ,frame4 ,frame5 ,frame6 ,frame7 ,frame8)
     (walk-stack k '() list
       (lambda (frame base depth next)
         (cons frame
           (if (= depth 8)
               base
               (next base)))))]
    [(,@frame0 ,@frame1 ,@frame2 base)
     (parameterize ([walk-stack-max-depth 3])
       (walk-stack k '(base) list
         (lambda (frame base depth next)
           (cons frame (next base)))))]
    [done
     (parameterize ([walk-stack-max-depth 0])
       (walk-stack k 'done list
         (lambda (frame base depth next)
           (cons frame (next base)))))]
    ["Stack dump truncated due to max-depth = 0.\n"
     (let ([os (open-output-string)])
       (parameterize ([current-output-port os] [walk-stack-max-depth 0])
         (let f ([n 10])
           (if (= n 0)
               (dump-stack)
               (list n (f (- n 1))))))
       (get-output-string os))]
    [("#<continuation in proc4>" #f #f ,frame0-free) frame0]
    [("#<continuation in proc4>" #f #f ,frame1-free) frame1]
    [(0 1) (sort < (map car frame0-free))]
    [(0 1) (sort < (map car frame1-free))]
    [#\A (find char? (map cdr frame0-free))]
    [#\B (find char? (map cdr frame1-free))]
    [("#<continuation in proc3>" ,k-src3 ,p-src3 ,frame2-free) frame2]
    [((n . 0) (x . 0) (zap . 0)) (sort-vars frame2-free)]
    [("#<continuation in proc2>" ,k-src2 ,p-src2 ,frame3-free) frame3]
    [(0 1 2) (map car frame3-free)]
    [(7 . 0) (find pair? (map cdr frame3-free))]
    [(0 0) (filter number? (map cdr frame3-free))]
    [("#<continuation in proc2>" ,@k-src2 ,@p-src2 ,frame4-free) frame4]
    [(0 1 2) (map car frame4-free)]
    [(0 1) (sort < (filter number? (map cdr frame4-free)))]
    [(8 . 0) (find pair? (map cdr frame4-free))]
    [("#<continuation in proc1>" ,k-src1 ,p-src1 ,frame5-free) frame5]
    [((n . 0) (v1 . 0) (v2 . 0) (x . 0)) (sort-vars frame5-free)]
    [("#<continuation in proc1>" ,@k-src1 ,@p-src1 ,frame6-free) frame6]
    [((n . 1) (v1 . 1) (v2 . 0) (x . 0)) (sort-vars frame6-free)]
    [("#<continuation in proc1>" ,@k-src1 ,@p-src1 ,frame7-free) frame7]
    [((n . 2) (v1 . 2) (v2 . 0) (x . 0)) (sort-vars frame7-free)]
    [("#<continuation in proc1>" ,@k-src1 ,@p-src1 ,frame8-free) frame8]
    [((n . 3) (v1 . 3) (v2 . 0) (x . 0)) (sort-vars frame8-free)]
    [(,@file3.ss 5 23) (get-source k-src3)]
    [(,@file3.ss 1 1) (get-source p-src3)]
    [#f (get-source k-src2)]
    [(,@file2.ss 1 1) (get-source p-src2)]
    [(,@file1.ss 6 25) (get-source k-src1)]
    [(,@file1.ss 2 1) (get-source p-src1)]
    ;; cover the dump-stack "... at offset ..." case while we have
    ;; a handy setup
    [,! (delete-file file3.ss)]
    [,extract-frames
     (let ()
       (define (make-frame desc free)
         (and desc (cons desc free)))
       (define (extract-frames lines)
         (remq #f
           (let f ([lines lines] [desc #f] [free '()])
             (match lines
               [() (list (make-frame desc free))]
               [(,line . ,lines)
                (if (starts-with? line " ")
                    (f lines desc (cons line free))
                    (cons (make-frame desc free) (f lines line '())))]))))
       extract-frames)]
    [,match-frame
     (lambda (f1 f2)
       (define (extract-free free)
         (match (pregexp-match (re "^  ([^:]+): (.*)") free)
           [(,_ ,var ,val) (cons var val)]))
       (match-let*
        ([(,desc . ,f1-free) f1]
         [(,@desc . ,f2-free) f2]
         [,f1-free-sorted (sort string<? f1-free)]
         [,f2-free-sorted (sort string<? f2-free)]
         [,named-vars-match (equal? f1-free-sorted f2-free-sorted)]
         [#t (or named-vars-match
                 ;; otherwise, must not have names, just free-var numbers
                 (let ([f1-extracted (map extract-free f1-free)]
                       [f2-extracted (map extract-free f2-free)])
                   (and (andmap string->number (map car f1-extracted))
                        (andmap string->number (map car f2-extracted))
                        (equal?
                         (sort string<? (map cdr f1-extracted))
                         (sort string<? (map cdr f2-extracted))))))])
        'ok))]
    [,compare
     (lambda (expected actual)
       (for-each match-frame
         (extract-frames expected)
         (extract-frames actual))
       'ok)]
    [,make-expected
     ;; stack->json records source info in terms of bfp,
     ;; but dump-stack may give us line numbers
     (lambda (offset-only?)
       `("#<continuation in proc4>"
         "  0: 65"
         "  1: #\\A"
         "#<continuation in proc4>"
         "  0: 66"
         "  1: #\\B"
         ,(format "#<continuation in proc3> at offset 117 of ~a" file3.ss)
         "  n: 0"
         "  x: 0"
         "  zap: 0"
         ,(format "#<continuation in proc2> in procedure at ~a of ~a"
            (if offset-only? "offset 0" "line 1, char 1")
            file2.ss)
         "  0: 0"
         "  1: 0"
         "  2: (7 . 0)"
         "Stack dump truncated due to max-depth = 4."
         ""))]
    [,expected (make-expected #f)]
    [,actual
     (let ([os (open-output-string)])
       (dump-stack k os 4)
       (split (get-output-string os) #\newline))]
    [ok (compare expected actual)]
    [,json-expected (make-expected #t)]
    [,json (stack->json k 4)]
    [,json-actual (split (json-stack->string json) #\newline)]
    [ok (compare json-expected json-actual)])
   'ok))

(mat limit-stack ()
  (define (hidden n return)
    (if (= n 5)
        (limit-stack (visible n return))
        (list n (hidden (- n 1) return))))
  (define (visible x return)
    (if (= x 0)
        (call/cc return)
        (cons x (visible (- x 1) return))))
  (define hidden-re
    (re "#<continuation in hidden> .*at offset [0-9]+ of .*erlang.ms$"))
  (define visible-re
    (re "#<continuation in visible> .*at offset [0-9]+ of .*erlang.ms$"))
  (define (->lines json)
    (let ([os (open-output-string)])
      (json-stack->string os json)
      (split (get-output-string os) #\newline)))
  (match-let*
   ([27 (limit-stack 27)]
    [(a b c)
     (call-with-values
       (lambda () (limit-stack (values 'a 'b 'c)))
       list)]
    [#t (limit-stack (call/cc limit-stack?))]
    [(#f) (limit-stack (list (call/cc limit-stack?)))]
    [,k (call/cc (lambda (return) (hidden 10 return)))]
    ;; omitting max-depth argument uses 'default, which respects limit-stack
    [,json (stack->json k)]
    [,lines (->lines json)]
    [("")
     (match-regexps
      `(,visible-re
        "  x: 1"
        ,visible-re
        "  x: 2"
        ,visible-re
        "  x: 3"
        ,visible-re
        "  x: 4"
        ,visible-re
        "  x: 5"
        "<continuation in [$]limit-stack> .*at offset [0-9]+ of .*erlang.ss$"
        "  (source|0): #[(]at [0-9]+ \".*erlang.ms\"[)]$")
      lines)]
    ;; explicit max-depth argument does not respect limit-stack
    [,json2 (stack->json k 10)]
    [,lines2 (->lines json2)]
    [("")
     (match-regexps
      `(,visible-re
        "  x: 1"
        ,visible-re
        "  x: 2"
        ,visible-re
        "  x: 3"
        ,visible-re
        "  x: 4"
        ,visible-re
        "  x: 5"
        "<continuation in [$]limit-stack> .*at offset [0-9]+ of .*erlang.ss$"
        "  (source|0): #[(]at [0-9]+ \".*erlang.ms\"[)]$"
        ,hidden-re
        "  n: 6"
        ,hidden-re
        "  n: 7"
        ,hidden-re
        "  n: 8"
        ,hidden-re
        "  n: 9"
        "Stack dump truncated due to max-depth = 10[.]")
      lines2)])
   'ok))

(mat exit-reason->stacks ()
  (define-syntax def
    (syntax-rules ()
      [(_ (proc fml ...) e0 e1 ...)
       (define proc
         (parameterize ([generate-inspector-information #t])
           (compile
            `(rec proc
               (lambda (fml ...)
                 e0 e1 ...)))))]))
  (def (clop n)
    (if (= n 0)
        (throw 'zero!)
        (+ n (clop (- n 1)))))
  (def (flop e z)
    (if (= z 0)
        (throw 'bad e)
        (list z (flop e (- z 1)))))
  (def (glop e t)
    (if (= t 0)
        (throw e)
        (- t (glop e (- t 1)))))
  (def (plop d end)
    (if (= d 0)
        (end)
        (memq d (plop (- d 1) end))))
  (define (dump k limit)
    (walk-stack k '()
      (lambda (description src p-src vars)
        (cons description vars))
      (lambda (frame base depth next)
        (cons frame (next base)))
      'dump limit
      (lambda (base depth) base)))
  (match-let*
   (
    ;; exit reasons with no stack
    [() (exit-reason->stacks 'none-here)]
    [() (exit-reason->stacks '(nor here))]
    [() (exit-reason->stacks 404)]
    [() (exit-reason->stacks (make-fault/no-cc 'nada))]
    ;; no stack from catch
    [,e0 (catch (clop 3))]
    [() (exit-reason->stacks e0)]
    [#(EXIT zero!) e0]
    [`(catch zero!) e0]
    [`(catch zero! zero!) e0]
    ;; we do get a stack from make-fault
    [(,k0)
     (guard (#%$continuation? k0))
     (exit-reason->stacks (make-fault 'one))]
    ;; we do get a stack from try
    [,e1 (try (clop 3))]
    [(,k)
     (guard (#%$continuation? k))
     (exit-reason->stacks e1)]
    [(("#<continuation in clop>" (n . 1))
      ("#<continuation in clop>" (n . 2))
      ("#<continuation in clop>" (n . 3))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k 4)]
    [`(&fault-condition [reason zero!]) e1]
    [`(catch zero!) e1]
    ;; try raise e1: passes e1 through
    [,e2 (try (plop 3 (lambda () (raise e1))))]
    [(,@k) (exit-reason->stacks e2)]
    ;; try throw e1: e1's k comes first
    [,e3 (try (plop 3 (lambda () (throw e1))))]
    [(,@k ,k2) (exit-reason->stacks e3)]
    [(("#<continuation in plop>" (d . 1))
      ("#<continuation in plop>" (d . 2))
      ("#<continuation in plop>" (d . 3))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k2 4)]
    [`(&fault-condition [reason zero!]) e2]
    [`(&fault-condition [reason zero!]) e3]
    [`(catch zero!) e2]
    [`(catch zero!) e3]
    [`(catch zero! ,@e2) e2]
    [`(catch zero! ,@e3) e3]
    ;; try (throw 'bad e3) in flop
    [,e4 (try (flop e3 4))]
    [(,@k ,@k2 ,k4) (exit-reason->stacks e4)]
    [(("#<continuation in flop>" (z . 1))
      ("#<continuation in flop>" (z . 2))
      ("#<continuation in flop>" (z . 3))
      ("#<continuation in flop>" (z . 4))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k4 5)]
    [`(&fault-condition [reason bad]) e4]
    [`(catch bad) e4]
    [`(catch bad ,@e4) e4]
    ;; try (throw e4) in glop
    [,e5 (try (glop e4 2))]
    [(,@k ,@k2 ,@k4 ,k5) (exit-reason->stacks e5)]
    [(("#<continuation in glop>" (t . 1))
      ("#<continuation in glop>" (t . 2))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k5 3)]
    [`(&fault-condition [reason bad]) e5]
    [`(catch bad) e5]
    [`(catch bad ,@e5) e5]
    ;; try (throw 'bzzt e3) in plop
    ;; throw where second arg has stack, but first has none
    [,e6 (try (plop 4 (lambda () (throw 'bzzt e5))))]
    [(,@k ,@k2 ,@k4 ,@k5 ,k6) (exit-reason->stacks e6)]
    [(("#<continuation in plop>" (d . 1))
      ("#<continuation in plop>" (d . 2))
      ("#<continuation in plop>" (d . 3))
      ("#<continuation in plop>" (d . 4))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k6 5)]
    [`(&fault-condition [reason bzzt]) e6]
    [`(catch bzzt) e6]
    [`(catch bzzt ,@e6) e6]
    ;; like e6, but raise instead of err
    [,e7 (try (plop 4 (lambda () (raise e5))))]
    [(,@k ,@k2 ,@k4 ,k5) (exit-reason->stacks e7)]
    [`(&fault-condition [reason bad]) e7])
   'ok)
  (match-let* ;; throw where both args have stack
   ([,e0 (try (clop 3))]
    [(,k0) (exit-reason->stacks e0)]
    [,e1 (try (flop e0 2))]
    [(,@k0 ,k1) (exit-reason->stacks e1)]
    [,e2 (try (glop (try (cdr 11)) 4))]
    [(,try0 ,k2 ,try1) (exit-reason->stacks e2)]
    [,e3 (try (plop 1 (lambda () (throw e1 e2))))]
    [(,@try0 ,@k2 ,@try1 ,@k0 ,@k1 ,k5) (exit-reason->stacks e3)]
    [(("#<system continuation in dynamic-wind>" . ,_)) (dump try0 1)]
    [(("#<continuation in glop>" (t . 1))
      ("#<continuation in glop>" (t . 2))
      ("#<continuation in glop>" (t . 3))
      ("#<continuation in glop>" (t . 4))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k2 5)]
    [(("#<system continuation in dynamic-wind>" . ,_)) (dump try1 1)]
    [(("#<continuation in clop>" (n . 1))
      ("#<continuation in clop>" (n . 2))
      ("#<continuation in clop>" (n . 3))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k0 4)]
    [(("#<continuation in flop>" (z . 1))
      ("#<continuation in flop>" (z . 2))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k1 3)]
    [(("#<continuation in plop>" (d . 1))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k5 2)]
    )
   'ok)
  (match-let* ;; throw where first arg has stack, but second has none
   ([,e0 (try (clop 4))]   ;; has stack
    [,e1 (catch (clop 3))] ;; no stack
    [,e2 (try (plop 2 (lambda () (throw e0 e1))))]
    [(,k0 ,k1) (exit-reason->stacks e2)]
    [(("#<continuation in clop>" (n . 1))
      ("#<continuation in clop>" (n . 2))
      ("#<continuation in clop>" (n . 3))
      ("#<continuation in clop>" (n . 4))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k0 5)]
    [(("#<continuation in plop>" (d . 1))
      ("#<continuation in plop>" (d . 2))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k1 3)]
    )
   'ok)
  (match-let* ;; throw where neither arg has stack
   ([,e0 (catch (clop 4))] ;; no stack
    [,e1 (catch (clop 2))] ;; no stack
    [,e2 (try (plop 3 (lambda () (throw e0 e1))))]
    [(,k1) (exit-reason->stacks e2)]
    [(("#<continuation in plop>" (d . 1))
      ("#<continuation in plop>" (d . 2))
      ("#<continuation in plop>" (d . 3))
      ("#<system continuation in dynamic-wind>" . ,_))
     (dump k1 4)]
    )
   'ok)
  (match-let* ;; make-fault and make-fault/no-cc combinations
   ([,e0 (make-fault 'outer (make-fault/no-cc 'inner))]
    [(,k0)
     (guard (#%$continuation? k0))
     (exit-reason->stacks e0)]
    [,e1 (make-fault/no-cc 'outer (make-fault 'inner))]
    [(,k1)
     (guard (#%$continuation? k1))
     (exit-reason->stacks e1)])
   'ok)
  )

(define (condition-equal? a b)
  (and (condition? a) (condition? b)
       (equal? (condition-who a) (condition-who b))
       (equal? (condition-message a) (condition-message b))
       (equal? (condition-irritants a) (condition-irritants b))))

(mat try-catch ()
  (define (bug x)
    (match x
      [raise (raise `#(raised ,x))]
      [throw (throw `#(thrown ,x))]
      [crash (cdr x)]))
  (define (effective origin)
    (match origin
      [out 'throw]
      [crash 'raise] ;; native error calls raise
      [,_ origin]))
  (define (check origin same?)
    (match-let*
     ([,C (catch (bug origin))]
      [,T (try (bug origin))]
      [#(EXIT ,reason) C]
      [#t (same? reason (match T [`(&fault-condition ,reason) reason]))]
      [#t (same? reason (match C [`(catch ,r) r]))]
      [#t (same? reason (match T [`(catch ,r) r]))]
      [#t (same? reason (match C [`(catch ,r ,@r) r]))]
      [#t (same? reason
            (match (effective origin)
              [throw (match T [`(catch ,r ,@T) r])]
              [raise (match T [`(catch ,r ,@r) r])]))]
      ;; catch of re-raise or re-throw
      [,@C (catch (match C [`(catch ,r ,e) (raise e)]))]
      [,@C (catch (match C [`(catch ,r ,e) (throw e)]))]
      [,C2 (catch (match T [`(catch ,r ,e) (raise e)]))]
      [,@C2 (catch (match T [`(catch ,r ,e) (throw e)]))]
      [#t (same? reason (match C2 [#(EXIT ,reason) reason]))]
      ;; try of re-raise or re-throw
      [,T2 (try (match C [`(catch ,r ,e) (raise e)]))]
      [,T3 (try (match C [`(catch ,r ,e) (throw e)]))]
      [,T4 (try (match T [`(catch ,r ,e) (raise e)]))]
      [,T5 (try (match T [`(catch ,r ,e) (throw e)]))]
      [#t (same? reason (match T2 [`(catch ,r) r]))]
      [#t (same? reason (match T3 [`(catch ,r) r]))]
      [#t (same? reason (match T4 [`(catch ,r) r]))]
      [#t (same? reason (match T5 [`(catch ,r) r]))]
      [,k2 (exit-reason->stacks T2)]
      [(,_ . ,k2) (exit-reason->stacks T3)]
      [,k4 (exit-reason->stacks T4)]
      [(,_ . ,k4) (exit-reason->stacks T5)]
      ;; catch or try between catch and use
      [,C3 (catch (bug origin) 'wrong)]
      [(ok fine) (list (catch 'ok) (try 'fine))]
      [`(catch ,C-reason ,C-err) C3]
      [(ok fine) (list (catch 'ok) (try 'fine))]
      [#t (same? reason C-reason)]
      [#t (eq? C-reason C-err)]
      ;; catch or try between try and use
      [,T6 (try (bug origin) 'wrong)]
      [(ok fine) (list (catch 'ok) (try 'fine))]
      [`(catch ,T-reason ,T-err) T6]
      [(ok fine) (list (catch 'ok) (try 'fine))]
      [#t (same? reason T-reason)]
      [#t (eq? T-err
            (match (effective origin)
              [throw T-err]
              [raise T-reason]))]
      ;; no exception in catch or try acts like begin
      [,ls '()]
      [,add! (lambda (x) (set! ls (cons x ls)))]
      [(3 2 1) (catch (add! 1) (add! 2) (add! 3) ls)]
      [(6 5 4 3 2 1) (try (add! 4) (add! 5) (add! 6) ls)]
      ;; nest (catch (try e))
      [,C4 (catch (try (bug origin)))]
      [`(catch ,r4 ,T7-err) C4]
      [#t (same? reason r4)]
      [#t (same? reason
            (match (effective origin)
              [throw (match T7-err [`(catch ,r ,@T7-err) r])]
              [raise T7-err]))]
      ;; nest (try (catch e))
      [,T8 (try (catch (bug origin)))]
      [`(catch ,r8 ,@r8) T8]
      [#t (same? reason r8)]
      )
     'ok))
  (check 'raise equal?)
  (check 'throw equal?)
  (check 'crash condition-equal?)
  (check 'out equal?)
  (let-values ([(c1 c2) (catch (values 'a 'b))]
               [(t1 t2) (try (values 'c 'd))]
               [() (catch (values))]
               [() (try (values))])
    (match-let*
     ([a c1]
      [b c2]
      [c t1]
      [d t2])
     'ok))
  (match-let*
   ([3 (call/cc (lambda (k) (try (k 3))))]
    [4 (call/cc (lambda (k) (catch (k 4))))])
   'ok)
  )

(mat make-fault ()
  (match-let*
   ([,e0 (make-fault 'flak)]
    [`(catch flak) e0]
    [`(catch flak ,@e0) e0]
    [(no-raise ,e1) (list 'no-raise (make-fault 'cold e0))]
    [`(catch cold) e1]
    [`(catch cold ,@e1) e1]
    [`(&fault-condition
       [reason cold]
       [inner* (`(&fault-condition [reason flak] [inner* ()] [k ,k0]))]
       [k ,k1])
     e1]
    [#t (#%$continuation? k0)]
    [#t (#%$continuation? k1)]
    [ok
     (call/cc
      (lambda (return)
        (define (my-exit-reason->english x)
          ;; should call exit-reason->english on reason within &fault-condition
          (match x
            [cold (return 'ok)]
            [,_ (swish-exit-reason->english x)]))
        (parameterize ([current-exit-reason->english my-exit-reason->english])
          (exit-reason->english e1)
          'nope)))])
   'ok))

(mat make-fault/no-cc ()
  (match-let*
   ([,e0 (make-fault/no-cc 'flak)]
    [`(catch flak) e0]
    [`(catch flak ,@e0) e0]
    [(no-raise ,e1) (list 'no-raise (make-fault/no-cc 'cold e0))]
    [`(catch cold) e1]
    [`(catch cold ,@e1) e1]
    [`(&fault-condition
       [reason cold]
       [inner* (`(&fault-condition [reason flak] [inner* ()] [k ,k0]))]
       [k ,k1])
     e1]
    [#f (#%$continuation? k0)]
    [#f (#%$continuation? k1)]
    [no-cc k0]
    [no-cc k1]
    [ok
     (call/cc
      (lambda (return)
        (define (my-exit-reason->english x)
          ;; should call exit-reason->english on reason within &fault-condition
          (match x
            [cold (return 'ok)]
            [,_ (swish-exit-reason->english x)]))
        (parameterize ([current-exit-reason->english my-exit-reason->english])
          (exit-reason->english e1)
          'nope)))])
   'ok))

(isolate-mat record-writers ()
  ;; mon
  (match-let*
   ([,pid (spawn&link values)]
    [,m (monitor pid)]
    [,expected (format "#<monitor ~s ~s>" self pid)]
    [,@expected (format "~s" m)])
   'ok)
  ;; pcb
  (match-let*
   ([,pid (spawn&link (lambda () (receive)))]
    [,expected0 (format "#<process ~a>" (global-process-id pid))]
    [,@expected0 (format "~s" pid)]
    [#t (register 'coverage pid)]
    [,expected1 (format "#<process ~a coverage>" (global-process-id pid))]
    [,@expected1 (format "~s" pid)]
    [#t (unregister 'coverage)]
    [,@expected0 (format "~s" pid)])
   'ok)
  ;; &fault-condition
  (match-let*
   ([,c (try (throw 'ball))]
    ["#<fault ball>" (format "~s" c)])
   'ok)
  ;; EXIT-msg
  (match-let*
   ([,_ (process-trap-exit #t)]
    [,pid (spawn (lambda () (throw 'long)))]
    [#t (link pid)]
    [,expected (format "#<EXIT ~s long>" pid)]
    [,@expected
     (receive (after 100 'wrong)
       [,x
        (guard (match x [`(EXIT ,@pid long) #t] [,_ #f]))
        (format "~s" x)])])
   (process-trap-exit #f))
  ;; DOWN-msg
  (match-let*
   ([,pid (spawn (lambda () (throw 'home)))]
    [,m (monitor pid)]
    [,expected (format "#<DOWN ~s home>" pid)]
    [,@expected
     (receive (after 100 'wrong)
       [,x
        (guard (match x [`(DOWN ,@m ,@pid home) #t] [,_ #f]))
        (format "~s" x)])])
   'ok)
  ;; console-event-handler treatment of native exceptions
  (let ([os (open-output-string)])
    (define test-file
      (write-test-file "make-exception.ss"
        (lambda ()
          (printf ";; explicit line breaks for predictable source information\n")
          (printf "(define (fail-nontail n)\n")
          (printf "  (if (zero? n)\n")
          (printf "      (cdr n)\n")
          (printf "      (+ n (fail-nontail (- n 1)))))\n"))))
    (define common
      '(""
        "Date:"
        "Timestamp: [0-9]+"
        "Event: #<compound condition: Exception in cdr: 0 is not a pair>"))
    (define stack-info
      '("Condition: Exception in cdr: 0 is not a pair"
        "Stack:"
        "#<continuation in fail-nontail> at line 5, char 12 of"
        "  n: 1"
        "#<continuation in fail-nontail> at line 5, char 12 of"
        "  n: 2"
        "#<continuation in fail-nontail> at line 5, char 12 of"
        "  n: 3"))
    (parameterize ([compile-file-message #f])
      (compile-and-load test-file #t #f))
    (parameterize ([console-error-port os])
      (let ([c (guard (c [else c]) (fail-nontail 3))])
        ;; first call gets stack since we've not seen it yet
        (console-event-handler c)
        (fprintf os "=== second call to console-event-handler\n")
        ;; second call omits stack since we've seen it already
        (console-event-handler c)
        (fprintf os "=== after reset-console-event-handler\n")
        (reset-console-event-handler)
        ;; stack is back after reset
        (console-event-handler c)))
    (match-regexps
     `(,@common
       ,@stack-info
       seek
       "=== second call to console-event-handler"
       ,@common
       ""
       "=== after reset-console-event-handler"
       ,@common
       ,@stack-info)
     (split (get-output-string os) #\newline))))

(isolate-mat cyclic-event ()
  ;; Handle cyclic structures in console-event-handler.
  (script-test #f '()
    (parameterize ([print-graph #t])
      (format "~{~s\n~}\n"
        '((event-mgr:notify
           (let f ([n 3])
             (if (= n 0)
                 (make-fault '#0=(a b c . #0#))
                 (list n (f (- n 1))))))
          (receive (after 100 (exit))))))
    `(seek
      "Date:"
      "Timestamp:"
      ,(pregexp-quote "Event: (3 (2 (1 #<fault #0=(a b c . #0#)>)))")
      "Stack:"
      "#<continuation in f>"
      "  n: 1"
      "#<continuation in f>"
      "  n: 2"
      "#<continuation in f>"
      "  n: 3")))

(isolate-mat dbg ()
  (script-test #f '()
    (format "~{~s\n~}"
      '((define (fail who)
          (fprintf (console-error-port) "Failed for ~s\n" who)
          (exit 1))
        (printf ";; nothing spawned, so nothing to debug\n")
        (dbg)
        (printf ";; no process 404, so nothing to debug\n")
        (dbg 404)
        (printf ";; no continuation condition for spawned process, so nothing to debug\n")
        (define pid0 (spawn (lambda () (printf "started\n") (raise 'issue))))
        (define m (monitor pid0))
        (receive (after 100 (fail 'pid0))
          [`(DOWN ,_ ,@pid0 ,_) (dbg)])
        (printf ";; now have a continuation condition, so something to dbg\n")
        (define pid1 (spawn (lambda () (cdr 123))))
        (define m (monitor pid1))
        (receive (after 100 (fail 'pid1))
          [`(DOWN ,_ ,@pid1 ,_) (dbg)])
        (printf ";; add another one\n")
        (define pid2 (spawn (lambda () (list (cdr (reverse 'transcriptase))))))
        (define m (monitor pid2))
        (receive (after 100 (fail 'pid2))
          [`(DOWN ,_ ,@pid2 ,_) (dbg)])
        (printf "debug pid2\n")
        (parameterize ([console-input-port (open-input-string "s i show-frames 2\nq\n")])
          (dbg (process-id pid2)))
        (printf "debug pid1\n")
        (parameterize ([console-input-port (open-input-string "s i show-frames 1\nq\n")])
          (dbg (process-id pid1)))
        (printf "done\n")
        (exit)))
    '(";; nothing spawned, so nothing to debug"
      ";; no process 404, so nothing to debug"
      "Nothing to debug."
      ";; no continuation condition for spawned process, so nothing to debug"
      "started"
      ";; now have a continuation condition, so something to dbg"
      "^ *[0-9]+: exited with reason #<compound condition: Exception in cdr: 123 is not a pair>, spawned [0-9]+"
      ";; add another one"
      "^ *[0-9]+: exited with reason #<compound condition: Exception in cdr: 123 is not a pair>, spawned [0-9]+"
      "^ *[0-9]+: exited with reason #<compound condition: Exception in reverse: transcriptase is not a proper list>, spawned [0-9]+"
      "^debug pid2"
      "^debug>.*Exception in reverse: transcriptase is not a proper list"
      "^debug>.*#<system continuation in reverse> +: +0: #<system continuation in reverse>"
      " +1: #<continuation>"
      seek
      "^debug pid1"
      "^debug>.*Exception in cdr: 123 is not a pair"
      "^debug>.*#<continuation> +: +0: #<continuation>"
      "^#<continuation> +: "
      "^debug> "
      "done")))

;; once in a blue moon this will catch a regression in $receive
;; that shows up as curious gen-server behavior; typically need
;; to save it to a separate mat file and run it repeatedly
(isolate-mat tempt-fate (settings (timeout 120000))
  (define server
    (spawn&link
     (lambda ()
       (let lp ()
         (match (receive [,x x])
           [(message ,from ,cookie)
            (send from `(reply ,cookie))
            (lp)]
           [quit (void)])))))
  (define (do-call server)
    (let ([m (monitor self)])
      (send server `(message ,self ,m))
      (receive [(reply ,@m) 'ok])))
  (do ([i 1000000 (fx- i 1)]) ((fx= i 0))
    ;; jostle tick count
    (let spin ([n (random 1000)])
      (when (fx> n 0)
        (spin (fx- n 1))))
    (do-call server)))

(isolate-mat keyboard-interrupt ()
  (define test self)
  (define runner
    (spawn&link
     (lambda ()
       (define count 0)
       (keyboard-interrupt-handler
        (lambda ()
          (set! count (+ count 1))
          (send test `(got ,count))))
       (let lp () (when (fx< count 3) (lp))))))
  (define innocent-bystander
    (spawn&link
     (lambda ()
       (keyboard-interrupt-handler
        (lambda () (throw 'unexpected)))
       (let lp () (lp)))))
  (match-let*
   ([ok (receive (after 500 (keyboard-interrupt runner) 'ok))]
    [(got 1) (receive [,x x])]
    [nothing (receive (after 100 'nothing) [,x x])]
    [ok (receive (after 500 (keyboard-interrupt runner) 'ok))]
    [(got 2) (receive [,x x])]
    [nothing (receive (after 100 'nothing) [,x x])]
    [ok (receive (after 500 (keyboard-interrupt runner) 'ok))]
    [(got 3) (receive [,x x])]
    [nothing (receive (after 100 'nothing) [,x x])]
    [,m (monitor runner)]
    [`(DOWN ,@m ,@runner normal) (receive [,x x])])
   'ok))

;; make sure $receive resumes where it left off in scanning the queue
(isolate-mat message-order ()
  (define test self)
  (define target 'circle)
  (define p
    (spawn
     (lambda ()
       (let f ()
         (receive
          [(,@target ,payload) (send test `(target-acquired ,payload))]
          [#(,x) (send test `(got ,x)) (f)]))
       (send test 'moving-on)
       (receive
        [(,@target ,payload)
         (send test `(subsequent-receive ,payload))]))))
  ;; first message remains in p's queue since target does not match
  (send p '(square first-message))
  (send p '#(1))
  (send p 'unclaimed) ;; not matched, remains in queue
  (send p '#(2))
  (send p '#(3))
  (match-let*
   ([(got 1) (receive [,x x])]
    [(got 2) (receive [,x x])]
    [(got 3) (receive [,x x])]
    ;; p made it through its queue and blocks waiting for a matching message
    [#(waiting-indefinitely ,_)
     (with-process-details p
       (lambda (id name spawned state)
         state))]
    ;; change the target so first message could also match
    [,_ (set! target 'square)]
    ;; send p some matching messages
    [,_ (send p (list target 'new-message))]
    [,_ (send p (list target 'incorrect))]
    ;; p wakes to find the new message. Since it is resuming a receive that
    ;; blocked, it should not reconsider messages it has already scanned.
    [(target-acquired new-message) (receive [,x x])]
    [moving-on (receive [,x x])]
    ;; p's new call to receive restarts at head of queue
    [(subsequent-receive first-message) (receive [,x x])])
   'ok))
