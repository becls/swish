;;; Copyright 2023 Beckman Coulter, Inc.
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
 (swish mat)
 (swish parallel)
 (swish testing)
 )

(define (check-leftovers)
  (let lp ([acc '()])
    (receive
     (after 0
       (unless (null? acc)
         (throw `#(leftover-messages ,(reverse acc))))
       'ok)
     [,msg
      (lp (cons msg acc))])))

(define-syntax parallel-mat
  (syntax-rules ()
    [(_ name settings e0 e1 ...)
     (isolate-mat name settings
       (begin
         ;; A bit of paranoia here. The gatekeeper may be necessary
         ;; for the tests that use eval, especially if the test is
         ;; spawning multiple processes.
         (gatekeeper:start&link)
         (let ()
           e0 e1 ...
           (check-leftovers))))]))

(define-syntax texp
  (syntax-rules ()
    [(_ e)
     (let* ([start (erlang:now)]
            [ret e]
            [end (erlang:now)])
       ;;(printf "t => ~a, ret => ~a\n" (- end start) ret)
       (list (- end start) ret))]))

(parallel-mat syntax ()
  (assert-syntax-error (parallel) "invalid syntax")
  (assert-syntax-error (parallel '123) "invalid syntax")
  (assert-syntax-error (parallel ([start-limit 1 2])) "invalid syntax")
  (assert-syntax-error (parallel ([start-limit])) "invalid syntax")
  (assert-syntax-error (parallel (lambda (x) x) '(1 2 3)) "invalid syntax")

  (assert-syntax-error (parallel!) "invalid syntax")
  (assert-syntax-error (parallel! '123) "invalid syntax")
  (assert-syntax-error (parallel! ([start-limit 1 2])) "invalid syntax")
  (assert-syntax-error (parallel! ([start-limit])) "invalid syntax")
  (assert-syntax-error (parallel! (lambda (x) x) '(1 2 3)) "invalid syntax")
  'ok)

(parallel-mat check-options ()
  (match-let*
   ([`(catch #(bad-arg start-limit 0))
     (try (parallel:options [start-limit 0]))]
    [`(catch #(bad-arg start-limit -1))
     (try (parallel:options [start-limit -1]))]
    [`(catch #(bad-arg start-limit whenever))
     (try (parallel:options [start-limit 'whenever]))]
    [`(catch #(bad-arg order -1))
     (try (parallel:options [order -1]))]
    [`(catch #(bad-arg order however))
     (try (parallel:options [order 'however]))]
    [`(catch #(bad-arg timeout -1))
     (try (parallel:options [timeout -1]))]
    [`(catch #(bad-arg timeout whenever))
     (try (parallel:options [timeout 'whenever]))])
   'ok))

(parallel-mat errors ()
  (define (check1 expected-regexp who args)
    ;;(printf "Expr: ~s\n" (apply list who args))
    (let ([actual (exit-reason->english (try (eval (apply list who args))))])
      (match (pregexp-match expected-regexp actual)
        [#f
         (throw `#(mismatch
                   (expression => ,exp)
                   (expected => ,expected-regexp)
                   (actual => ,actual)))]
        [,_ #t])))
  (define (check expected-regexp who args)
    (check1 expected-regexp who args)
    (check1 expected-regexp who (cons '(parallel:options) args)))

  (define num-args "^Exception: incorrect number of arguments( [0-9]+)? to")
  (define no-proc "noproc is not a procedure")
  (define no-list "is not a proper list")
  (define no-vec "is not a vector")
  (define list-len "differ in length")
  (define vec-len "lengths of input vectors .* differ")
  (define bad-opt "is not of type #<parallel:options>")

  (for-each
   (lambda (who)
     (check1 bad-opt who '(,#f 1 2 3))
     (check1 bad-opt who '(,12 1 2 3)))
   '(parallel parallel!))

  (for-each
   (lambda (who)
     (check num-args who '())
     (check1 num-args who '('noproc))
     (check1 num-args who '((parallel:options) +))
     (check1 no-proc who '((parallel:options) 'noproc))
     (check no-proc who '('noproc 1))
     (check no-list who '(+ 1))
     (check no-list who '(+ '(1 . 2)))
     (check list-len who '(+ '(1 2 3) '(4 5)))
     (check no-list who '(+ '(1 2 . 3) '(4 5)))
     (check no-list who '(+ '(1 2 3) '(4 . 5)))
     (check no-list who '(+ '#0=(1 2 3 . #0#))))
   '(parallel:for-each parallel:map))

  (for-each
   (lambda (who)
     (check num-args who '())
     (check1 num-args who '('noproc))
     (check1 num-args who '((parallel:options) +))
     (check1 no-proc who '((parallel:options) 'noproc))
     (check no-proc who '('noproc 1))
     (check no-vec who '(+ 1))
     (check no-vec who '(+ '(1 . 2)))
     (check vec-len who '(+ '#(1 2 3) '#(4 5))))
   '(parallel:vector-map))
  'ok)

(parallel-mat equiv ()
  ;; Confirm that parallel constructs behave similar to Chez Scheme's
  ;; functions
  (define (assert-same expected . actuals)
    (for-all
     (lambda (actual)
       (unless (equal? expected actual)
         (throw `#(mismatch (expected => ,expected) (actual => ,actual))))
       #t)
     actuals))
  ;; map
  (assert-same
   (map 1+ '())
   (parallel:map 1+ '())
   (parallel:map (parallel:options) 1+ '())
   (parallel ())
   (parallel:execute '()))
  (assert-same
   (map 1+ '(1 2 3))
   (parallel:map 1+ '(1 2 3))
   (parallel:map (parallel:options) 1+ '(1 2 3))
   (parallel () (1+ 1) (1+ 2) (1+ 3))
   (parallel:execute
    (list
     (lambda () (1+ 1))
     (lambda () (1+ 2))
     (lambda () (1+ 3)))))
  (assert-same
   (map cons '(1 2 3) '(4 5 6))
   (parallel:map cons '(1 2 3) '(4 5 6))
   (parallel:map (parallel:options) cons '(1 2 3) '(4 5 6))
   (parallel () (cons 1 4) (cons 2 5) (cons 3 6))
   (parallel:execute
    (list
     (lambda () (cons 1 4))
     (lambda () (cons 2 5))
     (lambda () (cons 3 6)))))
  ;; vector-map
  (assert-same
   (vector-map 1+ '#())
   (parallel:vector-map 1+ '#())
   (parallel:vector-map (parallel:options) 1+ '#()))
  (assert-same
   (vector-map 1+ '#(1 2 3))
   (parallel:vector-map 1+ '#(1 2 3))
   (parallel:vector-map (parallel:options) 1+ '#(1 2 3)))
  (assert-same
   (vector-map cons '#(1 2 3) '#(4 5 6))
   (parallel:vector-map cons '#(1 2 3) '#(4 5 6))
   (parallel:vector-map (parallel:options) cons '#(1 2 3) '#(4 5 6)))
  ;; for-each
  (let ()
    (define test<
      (lambda (x y)
        (cond
         [(fixnum? x) (< x y)]
         [(pair? x) (< (car x) (car y))]
         [else (bad-arg 'test< x)])))
    (define (make-shared)
      (let ([ls '()])
        (case-lambda
         [() (sort test< ls)]
         [(x)
          (with-interrupts-disabled
           (set! ls (cons x ls)))]
         [(x y)
          (with-interrupts-disabled
           (set! ls (cons (cons x y) ls)))])))
    (define (test proc . ls)
      (let ([shared (make-shared)])
        (apply proc shared ls)
        (shared)))
    (assert-same
     (test for-each '())
     (test parallel:for-each '())
     (parallel:execute '())
     (let ([shared (make-shared)])
       (parallel! ())
       (shared))
     (let ([shared (make-shared)])
       (parallel:execute! '())
       (shared)))
    (assert-same
     (test for-each '(1 2 3))
     (test parallel:for-each '(1 2 3))
     (let ([shared (make-shared)])
       (parallel! ()
         (shared 1)
         (shared 2)
         (shared 3))
       (shared))
     (let ([shared (make-shared)])
       (parallel:execute!
        (list
         (lambda () (shared 1))
         (lambda () (shared 2))
         (lambda () (shared 3))))
       (shared)))
    (assert-same
     (test for-each '(1 2 3) '(4 5 6))
     (test parallel:for-each '(1 2 3) '(4 5 6))
     (let ([shared (make-shared)])
       (parallel! ()
         (shared (cons 1 4))
         (shared (cons 2 5))
         (shared (cons 3 6)))
       (shared))
     (let ([shared (make-shared)])
       (parallel:execute!
        (list
         (lambda () (shared (cons 1 4)))
         (lambda () (shared (cons 2 5)))
         (lambda () (shared (cons 3 6)))))
       (shared)))
    ;; syntax
    (assert-same
     (parallel () 1 2 3)
     (let ([shared (make-shared)])
       (parallel! ()
         (shared 1)
         (shared 2)
         (shared 3))
       (shared)))
    (assert-same
     (parallel ([order 'left]) 1 2 3)
     (let ([shared (make-shared)])
       (parallel! ([order 'left])
         (shared 1)
         (shared 2)
         (shared 3))
       (shared)))
    (let ([opts (parallel:options [order 'right])])
      (assert-same
       (parallel ,opts 1 2 3)
       (let ([shared (make-shared)])
         (parallel! ,opts
           (shared 1)
           (shared 2)
           (shared 3))
         (shared))))))

(parallel-mat concurrent ()
  ;; The time to run these in parallel should be close to the time to
  ;; run the slowest expression and less than the sum of all the
  ;; times.
  (match-let*
   ([(,t (1 2 3 4)) (guard (< 999 t 1111))
     (texp
      (parallel ()
        (receive (after 1000 1))
        (receive (after 100 2))
        (receive (after 10 3))
        (receive (after 1 4))))]
    [(,t (1 2 3 4)) (guard (< 999 t 1111))
     (texp
      (parallel:map
       (lambda (t v)
         (receive (after t v)))
       '(1000 100 10 1)
       '(1 2 3 4)))])
   'ok))

(parallel-mat start-limit ()
  ;; For each call to parallel:execute, use a shared counter to count
  ;; the number of active processes running before the current process
  ;; completes. That counter should never excede the start-limit.
  (define (make-f)
    (define running 0)
    (define (inc!)
      (with-interrupts-disabled
       (set! running (fx+ running 1))))
    (define (dec!)
      (with-interrupts-disabled
       (let ([prior running])
         (set! running (fx- running 1))
         prior)))
    (lambda ()
      (inc!)
      (receive (after 100 (dec!)))))
  (define (do-test limit)
    (let* ([f (make-f)]
           [nprocs
            (parallel:execute
             (parallel:options [start-limit limit])
             (make-list 10 f))]
           [most (apply max nprocs)])
      (assert (<= most limit))))

  (parallel:map
   (lambda (i) (do-test (+ i 1)))
   (iota 10))
  'ok)

(parallel-mat order ()
  ;; Force start-limit to 1. Each worker sleeps a reasonable time,
  ;; then returns the current timestamp. The final list can be
  ;; compared with a sorted version of the list for equality.
  (define (do-func order)
    (define (f)
      (receive (after 100 (erlang:now))))
    (parallel:execute (parallel:options [start-limit 1] [order order])
      (make-list 10 f)))
  (define (do-effect order)
    (define me self)
    (define (make-f id)
      (lambda ()
        (receive
         (after 100
           (send me (cons id (erlang:now)))))))
    (define (get-msgs n)
      (if (= n 0)
          '()
          (receive
           [,msg
            (guard (pair? msg))
            (cons msg (get-msgs (- n 1)))])))
    (parallel:execute! (parallel:options [start-limit 1] [order order])
      (map make-f (iota 10)))
    ;; sort by id, then strip the id off
    (map cdr
      (sort
       (lambda (x y) (< (car x) (car y)))
       (get-msgs 10))))
  (define (do-test run-sample)
    (match-let*
     ([(,left ,right ,random)
       (parallel:map run-sample '(left right random))]
      [,@left (sort < left)]
      [,@right (sort > right)])
     (assert (not (equal? random (sort < random))))
     (assert (not (equal? random (sort > random))))))

  (parameterize ([random-seed 2609306995])
    (parallel:for-each do-test (list do-func do-effect))))

(parallel-mat cleanup ()
  (define me self)

  (define-syntactic-monad $ child-delay child-trap-exit)

  ($ define (make-error . ids)
     (lambda (id)
       (send me `#(child ,self ,id))
       (process-trap-exit child-trap-exit)
       (receive
        (after child-delay
          (when (member id ids) (throw 'fish))
          id))))

  (define (wait-for-start ids)
    ;; wait specified children to start and put the message back into
    ;; the queue for later.
    (cond
     [(null? ids) 'ok]
     [else
      (receive
       [,(msg <= #(child ,_ ,i))
        (guard (member i ids))
        (send me msg)
        (wait-for-start (remove i ids))])]))

  (define (snoop ls)
    (let lp ([ls ls] [acc '()])
      (receive (after 0 (reverse acc))
        [#(child ,pid ,i)
         (guard (equal? i (car ls)))
         (let ([m (monitor pid)])
           (receive
            [`(DOWN ,@m ,_ ,reason)
             (lp (cdr ls) (cons reason acc))]))])))

  (match-let*
   ([,ls (iota 5)]
    [,child-delay 0]
    [,child-trap-exit #f]
    ;; Success
    [,@ls (parallel:map ($ make-error) ls)]
    [#t (equal? (snoop ls) (make-list 5 'normal))]
    ;; Single error
    [`(catch fish) (try (parallel:map ($ make-error () 3) ls))]
    [(normal normal normal fish normal) (snoop ls)]

    ;; When parallel:map has a failure, propagate the reason.
    [`(catch timeout)
     (try (parallel:map (parallel:options [timeout 10])
            ($ make-error ([child-delay 1000]))
            ls))]
    [#t (equal? (snoop ls) (make-list 5 'timeout))]

    ;; When the caller of parallel:map is trap-exit and killed by its
    ;; parent, propagate the reason.
    [,pid (spawn
           (lambda ()
             (process-trap-exit #t)
             (parallel:map
              ($ make-error ([child-delay 1000]))
              ls)))]
    [ok (wait-for-start ls)]
    [,_ (kill pid 'teardown)]
    [,m (monitor pid)]
    [teardown (receive [`(DOWN ,@m ,_ ,reason) reason])]
    [#t (equal? (snoop ls) (make-list 5 'teardown))]

    ;; The caller of parallel:map can be killed. The children will be
    ;; killed with the same reason.
    [,pid (spawn
           (lambda ()
             (parallel:map
              ($ make-error ([child-delay 1000]))
              ls)))]
    [ok (wait-for-start ls)]
    [,_ (kill pid 'teardown)]
    [,m (monitor pid)]
    [teardown (receive [`(DOWN ,@m ,_ ,reason) reason])]
    [#t (equal? (snoop ls) (make-list 5 'teardown))]

    ;; When the children are marked trap-exit, we expect that they
    ;; will receive the kill message, but will complete normally. We
    ;; expect the caller to die for the reason given.
    [,pid (spawn
           (lambda ()
             (process-trap-exit #t)
             (parallel:map
              ($ make-error
                 ([child-delay 1000]
                  [child-trap-exit #t]))
              ls)))]
    [ok (wait-for-start ls)]
    [,_ (kill pid 'teardown)]
    [,m (monitor pid)]
    [teardown (receive [`(DOWN ,@m ,_ ,reason) reason])]
    [#t (equal? (snoop ls) (make-list 5 'normal))])
   'ok))

(parallel-mat logging ()
  (define (get-events timeout)
    (receive (after timeout '())
      [,x (cons x (get-events timeout))]))

  (start-silent-event-mgr)
  (capture-events)
  (match-let*
   (;; No workers. Verify supervisor is the current process, there is
    ;; both a reasonable child-start and child-end message.
    [() (parallel ())]
    [(`(<child-start>
        [supervisor ,sup]
        [pid ,pid1]
        [name "parallel:0"]
        [restart-type watch-only]
        [shutdown #f]
        [type worker])
      `(<child-end>
        [pid ,@pid1]
        [killed 0]
        [reason normal]
        [details #f]))
     (guard (eq? sup self))
     (get-events 10)]
    ;; Three workers. Verify we only get one child-start/child-end
    ;; pair.
    [(1 2 3) (parallel () 1 2 3)]
    [(`(<child-start> [pid ,pid1] [name "parallel:3"])
      `(<child-end>
        [pid ,@pid1]
        [killed 0]
        [reason normal]
        [details #f]))
     (get-events 10)]
    ;; Four workers, single failure. Verify the failure is logged.
    [`(catch ball) (try (parallel () 1 2 3 (throw 'ball)))]
    [(`(<child-start> [pid ,pid1] [name "parallel:4"])
      `(<child-end>
        [pid ,@pid1]
        [killed 0]
        [reason ball]
        [details `(catch ball)]))
     (get-events 10)]
    ;; Two workers. Kill the caller process. Verify the log contains
    ;; the reason the kernel was killed.
    [,pid (spawn
           (lambda ()
             (process-trap-exit #t)
             (parallel ()
               (receive)
               (receive))))]
    [ok (receive (after 10 'ok))]
    [,_ (kill pid 'teardown)]
    [(`(<child-start> [pid ,pid1] [name "parallel:2"])
      `(<child-end>
        [pid ,@pid1]
        [killed 1]
        [reason teardown]
        [details `(catch teardown)]))
     (get-events 10)])
   'ok))

(parallel-mat rogue ()
  (match-let*
   ([(1 2)
     (parallel ()
       1
       ;; child attempts to fool the parallel kernel by sending it a
       ;; normal EXIT message.
       (let* ([gramps (process-parent)]
              [m (monitor (spawn (lambda () (link gramps))))])
         (receive
          [`(DOWN ,@m ,_ ,_) 2])))]
    [(1 2)
     (parallel ()
       1
       ;; child attempts to fool the parallel kernel by sending it an
       ;; error EXIT message.
       (let* ([gramps (process-parent)]
              [m (monitor (spawn (lambda () (link gramps) (throw 'boom))))])
         (receive
          [`(DOWN ,@m ,_ ,_) 2])))]
    [(1 2)
     (parallel ()
       1
       (begin
         ;; child attempts to fool the parallel kernel by sending it
         ;; a nonsensical message.
         (send (process-parent) 'hey-look-over-there!)
         2))]
    ;; verify the nonsensical message did not make it back to the
    ;; calling process.
    [ok (receive (after 0 'ok) [,x x])])
   'ok))
