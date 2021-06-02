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

#!chezscheme
(library (swish erlang)
  (export
   DOWN
   EXIT
   add-finalizer
   arg-check
   bad-arg
   catch
   complete-io
   console-event-handler
   dbg
   define-match-extension
   define-tuple
   demonitor
   demonitor&flush
   dump-stack
   erlang:now
   get-registered
   global-process-id
   inherited-parameters
   keyboard-interrupt
   kill
   limit-stack
   limit-stack?
   link
   make-fault
   make-fault/no-cc
   make-inherited-parameter
   make-process-parameter
   match
   match-define
   match-let*
   monitor
   monitor?
   on-exit
   pps
   print-process-state
   process-id
   process-name
   process-parent
   process-trap-exit
   process?
   profile-me
   ps-fold-left
   receive
   register
   reset-console-event-handler
   self
   send
   spawn
   spawn&link
   throw
   try
   unlink
   unregister
   wait-for-io
   walk-stack
   walk-stack-max-depth
   whereis
   with-process-details
   )
  (import
   (chezscheme)
   (swish internal)
   (swish meta)
   (swish osi)
   )
  ;; Procedures starting with @ must be called with interrupts disabled.

  (define-syntax (arg-check x)
    (syntax-case x ()
      [(k $who [$arg pred ...] ...)
       #'(let ([who $who])
           (let ([arg $arg])
             (unless (and (pred arg) ...)
               ;; count on arg-check indicates test coverage of bad-arg case
               (profile-me-as k)
               (bad-arg who arg)))
           ...
           (void))]))

  (define-syntax on-exit
    (syntax-rules ()
      [(_ finally b1 b2 ...)
       (dynamic-wind
         void
         (lambda () b1 b2 ...)
         (lambda () finally))]))

  (define-syntax no-interrupts
    (syntax-rules ()
      [(_ body ...)
       (let ([x (begin (disable-interrupts) body ...)])
         (enable-interrupts)
         x)]))

  (define-syntax (receive x)
    (syntax-case x ()
      [(_ (after timeout t1 t2 ...) (pattern b1 b2 ...) ...)
       (eq? (datum after) 'after)
       #`(receive-after
          (lambda (x) (or (match-pattern x pattern b1 b2 ...) ...))
          #,(find-source x)
          timeout
          (lambda () t1 t2 ...))]
      [(_ (until time t1 t2 ...) (pattern b1 b2 ...) ...)
       (eq? (datum until) 'until)
       #`(receive-until
          (lambda (x) (or (match-pattern x pattern b1 b2 ...) ...))
          #,(find-source x)
          time
          (lambda () t1 t2 ...))]
      [(_ (pattern b1 b2 ...) ...)
       #`($receive
          (lambda (x) (or (match-pattern x pattern b1 b2 ...) ...))
          #,(find-source x)
          #f
          #f)]))

  (define-syntax self
    (identifier-syntax
     (#3%$top-level-value '#{self lgnnu3lheosakvgyylzmvq5uw-0})))

  (define (set-self! x)
    (#3%$set-top-level-value! '#{self lgnnu3lheosakvgyylzmvq5uw-0} x))

  (define-record-type q
    (nongenerative)
    (fields
     (mutable prev)
     (mutable next))
    (protocol
     (lambda (new)
       (lambda ()
         (new #f #f)))))

  (define-record-type msg
    (nongenerative)
    (sealed #t)
    (fields
     (immutable contents))
    (parent q)
    (protocol
     (lambda (new)
       (lambda (contents)
         ((new) contents)))))

  (define-record-type mon
    (nongenerative)
    (sealed #t)
    (fields
     (immutable origin)
     (immutable target)))

  (define-record-type pcb
    (nongenerative)
    (sealed #t)
    (fields
     (immutable id)
     (immutable create-time)
     (mutable parameters)
     (mutable name)
     (mutable cont)
     (mutable sic)
     (mutable winders)
     (mutable exception-state)
     (mutable inbox)
     (mutable precedence)
     (mutable flags)
     (mutable links)
     (mutable monitors)
     (mutable src)
     )
    (parent q)
    (protocol
     (lambda (new)
       (lambda (id cont)
         ((new) id (erlang:now) (make-weak-eq-hashtable)
          #f cont 0 '() #f (make-queue) 0 0 '() '() #f)))))

  (define (pcb-sleeping? p)
    (fxlogbit? 0 (pcb-flags p)))

  (define (pcb-sleeping?-set! p x)
    (pcb-flags-set! p
      (if x
          (fxlogbit1 0 (pcb-flags p))
          (fxlogbit0 0 (pcb-flags p)))))

  (define (pcb-trap-exit p)
    (fxlogbit? 1 (pcb-flags p)))

  (define (pcb-trap-exit-set! p x)
    (pcb-flags-set! p
      (if x
          (fxlogbit1 1 (pcb-flags p))
          (fxlogbit0 1 (pcb-flags p)))))

  (define (pcb-blocked-io? p)
    (fxlogbit? 2 (pcb-flags p)))

  (define (pcb-blocked-io?-set! p x)
    (pcb-flags-set! p
      (if x
          (fxlogbit1 2 (pcb-flags p))
          (fxlogbit0 2 (pcb-flags p)))))

  (define (pcb-interrupt? p)
    (fxlogbit? 3 (pcb-flags p)))

  (define (pcb-interrupt?-set! p x)
    (pcb-flags-set! p
      (if x
          (fxlogbit1 3 (pcb-flags p))
          (fxlogbit0 3 (pcb-flags p)))))

  (define erlang:now osi_get_time)

  (define (panic event)
    (on-exit (osi_exit 80)
      (console-event-handler event)))

  (define (@kill p raw-reason)
    (define reason (unwrap-fault-condition raw-reason))
    (define extended-reason
      (cond
       [(pcb-cont p) =>
        (lambda (k)
          (if (memq reason '(normal shutdown))
              raw-reason
              (make-fault-condition k reason (list raw-reason))))]
       [else raw-reason]))
    (when (eq? p event-loop-process)
      (panic `#(event-loop-process-terminated ,reason)))
    (when (eq? p finalizer-process)
      (panic `#(finalizer-process-terminated ,reason)))
    (when (enqueued? p)
      (@remove-q p))
    (pcb-cont-set! p #f)
    (pcb-winders-set! p '())
    (pcb-exception-state-set! p extended-reason)
    (pcb-inbox-set! p #f)
    (pcb-flags-set! p 0)
    (pcb-src-set! p #f)
    (pcb-parameters-set! p #f)
    (let ([name (pcb-name p)])
      (when name
        (pcb-name-set! p #f)
        (eq-hashtable-delete! registrar name)))
    (let ([links (pcb-links p)])
      (pcb-links-set! p '())
      (@remove-links links p)
      (@kill-linked links p reason extended-reason))
    (let ([monitors (pcb-monitors p)])
      (pcb-monitors-set! p '())
      (for-each
       (lambda (m)
         (let ([origin (mon-origin m)])
           (cond
            [(eq? origin p)
             (@remove-monitor m (mon-target m))]
            [else
             (@send origin (make-DOWN-msg m p extended-reason))
             (@remove-monitor m origin)])))
       monitors)))

  (define (alive? p)
    (pcb-inbox p))

  (define (@remove-links froms to)
    (unless (null? froms)
      (let ([from (car froms)])
        (@remove-link from to)
        (@remove-links (cdr froms) to))))

  (define (@remove-link from to)
    (pcb-links-set! from (remq to (pcb-links from))))

  (define (@remove-monitor m p)
    (pcb-monitors-set! p (remq m (pcb-monitors p))))

  ($import-internal
   make-fault
   make-fault/no-cc
   &fault-condition fault-condition-reason fault-condition? make-fault-condition
   EXIT-msg EXIT-msg? EXIT-msg-pid EXIT-msg-reason make-EXIT-msg
   DOWN-msg DOWN-msg? DOWN-msg-monitor DOWN-msg-pid DOWN-msg-reason make-DOWN-msg)

  (define (unwrap-fault-condition r)
    (if (fault-condition? r)
        (fault-condition-reason r)
        r))

  (define (->fault-condition reason)
    (if (fault-condition? reason)
        reason
        (make-fault-condition #f reason '())))

  (define (->EXIT reason)
    `#(EXIT ,(unwrap-fault-condition reason)))

  (define-syntax try
    (syntax-rules ()
      [(_ e1 e2 ...)
       ($trap (lambda () e1 e2 ...) ->fault-condition)]))

  ;; This binding serves a dual purpose: it provides backwards
  ;; compatibility for older code and it provides a binding for
  ;; the define-match-extension used in newer code.
  (define-syntax catch
    (syntax-rules ()
      [(_ e1 e2 ...)
       ($trap (lambda () e1 e2 ...) ->EXIT)]))

  (define ($trap thunk ->reason)
    (call/1cc
     (lambda (return)
       (with-exception-handler
        (lambda (r)
          (return (->reason r)))
        thunk))))

  ($import-internal throw)

  (define (bad-arg who arg)
    (throw `#(bad-arg ,who ,arg)))

  (define (kill p raw-reason)
    (unless (pcb? p)
      (bad-arg 'kill p))
    (no-interrupts
     (when (alive? p)
       (let ([reason (unwrap-fault-condition raw-reason)])
         (cond
          [(eq? reason 'kill) (@kill p 'killed)]
          [(pcb-trap-exit p) (@send p (make-EXIT-msg self raw-reason))]
          [(not (eq? reason 'normal)) (@kill p raw-reason)]))
       (unless (alive? self)
         (yield #f 0))))
    #t)

  (define (keyboard-interrupt p)
    (unless (pcb? p)
      (bad-arg 'keyboard-interrupt p))
    (if (eq? p self)
        ((keyboard-interrupt-handler))
        (no-interrupts
         (when (alive? p)
           (pcb-interrupt?-set! p #t)
           (cond
            [(pcb-sleeping? p)
             (pcb-sleeping?-set! p #f)
             (@enqueue p run-queue 0)]
            [(pcb-blocked-io? p) (void)]
            [(enqueued? p) (void)]
            [else (@enqueue p run-queue 0)])))))

  (define process-trap-exit
    (case-lambda
     [() (pcb-trap-exit self)]
     [(x) (pcb-trap-exit-set! self x)]))

  (define (@link p1 p2)
    (define (add-link from to)
      (pcb-links-set! from (cons to (pcb-links from))))
    (unless (memq p2 (pcb-links p1))
      (add-link p1 p2)
      (add-link p2 p1)))

  (define (link p)
    (unless (pcb? p)
      (bad-arg 'link p))
    (unless (eq? self p)
      (no-interrupts
       (cond
        [(alive? p) (@link p self)]
        [(pcb-trap-exit self)
         (@send self (make-EXIT-msg p (pcb-exception-state p)))]
        [else
         (let ([r (pcb-exception-state p)])
           (unless (eq? (unwrap-fault-condition r) 'normal)
             (@kill self r)
             (yield #f 0)))])))
    #t)

  (define (unlink p)
    (unless (pcb? p)
      (bad-arg 'unlink p))
    (no-interrupts
     (@remove-link self p)
     (@remove-link p self))
    #t)

  (define (monitor? m) (mon? m))

  (define (monitor p)
    (define (add-monitor m p)
      (pcb-monitors-set! p (cons m (pcb-monitors p))))
    (unless (pcb? p)
      (bad-arg 'monitor p))
    (let ([m (make-mon self p)])
      (no-interrupts
       (if (alive? p)
           (begin
             (add-monitor m self)
             (add-monitor m p))
           (@send self (make-DOWN-msg m p (pcb-exception-state p)))))
      m))

  (define (demonitor m)
    (unless (and (mon? m) (eq? (mon-origin m) self))
      (bad-arg 'demonitor m))
    (no-interrupts
     (@remove-monitor m self)
     (@remove-monitor m (mon-target m)))
    #t)

  (define (demonitor&flush m)
    (demonitor m)
    (receive (until 0 #t)
      [`(DOWN ,@m ,_ ,_) #t]))

  (define (make-queue)
    (let ([q (make-q)])
      (q-prev-set! q q)
      (q-next-set! q q)
      q))

  (define (queue-empty? queue)
    (eq? (q-next queue) queue))

  (define enqueued? q-prev)

  (define (@enqueue process queue precedence)
    (when (enqueued? process)
      (@remove-q process))
    (pcb-precedence-set! process precedence)
    (let find ([next queue])
      (let ([prev (q-prev next)])
        (if (or (eq? prev queue) (<= (pcb-precedence prev) precedence))
            (@insert-q process next)
            (find prev)))))

  (define last-process-id 0)

  (define (@make-process cont)
    (let ([id (+ last-process-id 1)])
      (set! last-process-id id)
      (let ([process (make-pcb id cont)])
        (eq-hashtable-set! process-table process 0)
        process)))

  (define pps
    (case-lambda
     [() (pps (current-output-port))]
     [(op)
      (unless (output-port? op)
        (bad-arg 'pps op))
      (display-string "Processes:\n" op)
      (dump-process-table op (lambda (p) #t))]))

  (define (dump-process-table op pred)
    (parameterize ([print-length 3] [print-level 6])
      (ps-fold-left < #f
        (lambda (_ p)
          (when (pred p)
            (print-process p op))))))

  (define (exception-state-continuation p)
    (let ([e (pcb-exception-state p)])
      (and (continuation-condition? e)
           e)))

  (define dbg
    (case-lambda
     [()
      (dump-process-table (current-output-port)
        exception-state-continuation)]
     [(who)
      (define (find-process-to-debug base p)
        (if (eqv? (process-id p) who)
            (exception-state-continuation p)
            base))
      (debug-condition (ps-fold-left < #f find-process-to-debug))
      (debug)]))

  (define (ps-fold-left id<? base f)
    (let* ([v (vector-sort (lambda (x y) (id<? (pcb-id x) (pcb-id y)))
                (no-interrupts (hashtable-keys process-table)))]
           [end (vector-length v)])
      ;; fixed evaluation order in case f has side effects
      (do ([i 0 (fx+ i 1)]
           [base base (f base (vector-ref v i))])
          ((= i end) base))))

  (define dump-stack
    (let ()
      (define (source-path src)
        (and (source-object? src)
             (let ([sfd (source-object-sfd src)])
               (call-with-values
                 (lambda () (locate-source sfd (source-object-bfp src) #t))
                 (case-lambda
                  [()
                   (format " at offset ~a of ~a" (source-object-bfp src)
                     (source-file-descriptor-path sfd))]
                  [(path line char)
                   (format " at line ~a, char ~a of ~a" line char path)])))))
      (case-lambda
       [() (dump-stack (current-output-port))]
       [(op) (call/cc (lambda (k) (dump-stack k op 'default)))]
       [(k op max-depth)
        (define (dump-frame name source proc-source vars)
          (display name op)
          (cond
           [(source-path source) => (lambda (where) (display where op))]
           [(source-path proc-source) =>
            (lambda (where) (fprintf op " in procedure~a" where))])
          (newline op)
          (for-each
           (lambda (var)
             (fprintf op "  ~s: ~s\n" (car var) (cdr var)))
           (or vars '())))
        (define (next-frame frame base depth next) (next base))
        (define (truncated base depth)
          (fprintf op "Stack dump truncated due to max-depth = ~s.\n" depth))
        (unless (output-port? op) (bad-arg 'dump-stack op))
        (parameterize ([print-level 3] [print-length 6] [print-gensym #f] [print-extended-identifiers #t])
          (walk-stack k (void) dump-frame next-frame
            'dump-stack max-depth truncated))])))

  (define ($limit-stack thunk source)
    ;; thwart cp0 and ensure source is live on the stack
    ((call-with-values thunk $limit-stack-receiver) source))

  (define $limit-stack-receiver
    (case-lambda
     [(x) (lambda (source) x)]
     [xs (lambda (source) (apply values xs))]))

  (define-syntax (limit-stack x)
    (syntax-case x ()
      [(ls e0 e1 ...)
       #`($limit-stack (lambda () e0 e1 ...)
          #,(find-source #'ls))]))

  (define (limit-stack? k)
    (and (#3%$continuation? k)
         (eq? (#3%$continuation-return-code k)
           (#3%$closure-code $limit-stack))))

  (define (do-frame cont handle-frame)
    (let ([description (format "~s" (cont 'value))]
          [source (cont 'source-object)]
          [proc-source ((cont 'code) 'source-object)]
          [vars
           (do ([i (fx1- (cont 'length)) (fx1- i)]
                [vars '()
                  (let ([var (cont 'ref i)])
                    (cons (cons (or (var 'name) i) ((var 'ref) 'value))
                      vars))])
               ((fx< i 0) vars))])
      (handle-frame description source proc-source vars)))

  (define walk-stack-max-depth
    (make-parameter 10
      (lambda (v)
        (arg-check 'walk-stack-max-depth
          [v fixnum? fxnonnegative?])
        v)))

  (define walk-stack
    (case-lambda
     [(k base handle-frame combine)
      (walk-stack k base handle-frame combine 'walk-stack 'default
        (lambda (base depth) base))]
     [(k base handle-frame combine who max-depth truncated)
      (define in (if (symbol? who) who 'walk-stack))
      (arg-check in
        [handle-frame procedure?]
        [who symbol?]
        [combine procedure?]
        [truncated procedure?])
      (let ([obey-limit? (eq? max-depth 'default)]
            [max-depth
             (match max-depth
               [default (walk-stack-max-depth)]
               [,n (guard (and (fixnum? n) (positive? n))) n]
               [#f (most-positive-fixnum)]
               [,_ (bad-arg in max-depth)])])
        (let loop ([cont (inspect/object k)] [depth 0] [base base])
          (cond
           [(not (eq? (cont 'type) 'continuation)) base]
           [(fx= depth max-depth) (truncated base depth)]
           [else
            ;; force evaluation order in case do-frame has side effects
            (let ([frame (do-frame cont handle-frame)])
              (combine frame base depth
                (if (and obey-limit? (limit-stack? (cont 'value)))
                    (lambda (base) base)
                    (lambda (base)
                      (loop (cont 'link) (+ depth 1) base)))))])))]))

  (define (process? p) (pcb? p))

  (define (print-process p op)
    (with-process-details p
     (lambda (id name spawned state)
       (fprintf op " ~6d: " id)
       (when name
         (display name op)
         (write-char #\space op))
       (print-process-state state op)
       (fprintf op ", spawned ~d\n" spawned))))

  (define (with-process-details p k)
    (arg-check 'with-process-details [p pcb?] [k procedure?])
    (let-values
        ([(name precedence sleeping? blocked-io? enqueued? completed? src)
          (with-interrupts-disabled
           (values (pcb-name p) (pcb-precedence p) (pcb-sleeping? p)
             (pcb-blocked-io? p) (enqueued? p) (not (alive? p)) (pcb-src p)))])
      (k (pcb-id p) name (pcb-create-time p)
        (cond
         [(eq? self p) 'running]
         [sleeping?
          `#(sleep-up-to
             ,(/ (max (- precedence (erlang:now)) 0) 1000.0)
             ,src)]
         [blocked-io? `#(waiting-for ,src)]
         [enqueued? 'ready]
         [completed? `#(exited ,(pcb-exception-state p))]
         [else `#(waiting-indefinitely ,src)]))))

  (define (print-src src op)
    (when src
      (match-let* ([#(,at ,offset ,file) src])
        (fprintf op " ~a char ~d of ~a" at offset file))))

  (define (print-process-state state op)
    (match state
      [running (display-string "running" op)]
      [#(sleep-up-to ,seconds ,src)
       (fprintf op "waiting for up to ~as" seconds)
       (print-src src op)]
      [#(waiting-for ,src)
       (fprintf op "waiting for ~a" src)]
      [ready (display-string "ready to run" op)]
      [#(exited ,reason)
       (fprintf op "exited with reason ~s" (unwrap-fault-condition reason))]
      [#(waiting-indefinitely ,src)
       (display-string "waiting indefinitely" op)
       (print-src src op)]))

  (define (global-process-id pid)
    ($import-internal get-session-id)
    (unless (pcb? pid)
      (bad-arg 'global-process-id pid))
    (format "~@[~d~]:~d" (get-session-id) (pcb-id pid)))

  (define process-id
    (case-lambda
     [() (pcb-id self)]
     [(p)
      (unless (pcb? p)
        (bad-arg 'process-id p))
      (pcb-id p)]))

  (define process-name
    (case-lambda
     [() (pcb-name self)]
     [(p)
      (unless (pcb? p)
        (bad-arg 'process-name p))
      (pcb-name p)]))

  (define (process-parent)
    (let ([x (weak-parent)])
      (and (weak-pair? x)
           (let ([x (car x)])
             (and (not (bwp-object? x)) x)))))

  (define (inherit-parameters thunk)
    (define parent self)
    (define inherited (inherited-parameters))
    (define vals (map (lambda (p) (p)) inherited))
    (lambda ()
      (weak-parent (weak-cons parent '()))
      (set! parent #f)
      (for-each (lambda (p v) (p v)) inherited vals)
      (set! inherited '())
      (set! vals '())
      (thunk)))

  (define (spawn thunk)
    (unless (procedure? thunk)
      (bad-arg 'spawn thunk))
    (let ([thunk (inherit-parameters thunk)])
      (no-interrupts
       (let ([p (@make-process (@thunk->cont thunk))])
         (@enqueue p run-queue 0)
         p))))

  (define (spawn&link thunk)
    (unless (procedure? thunk)
      (bad-arg 'spawn&link thunk))
    (let ([thunk (inherit-parameters thunk)])
      (no-interrupts
       (let ([p (@make-process (@thunk->cont thunk))])
         (@link p self)
         (@enqueue p run-queue 0)
         p))))

  (define (@send p x)
    (let ([inbox (pcb-inbox p)])
      (when inbox
        (@insert-q (make-msg x) inbox)
        (cond
         [(pcb-sleeping? p)
          (pcb-sleeping?-set! p #f)
          (@enqueue p run-queue 0)]
         [(pcb-blocked-io? p) (void)]
         [(enqueued? p) (void)]
         [else (@enqueue p run-queue 0)]))))

  (define (send p x)
    (cond
     [(pcb? p) (no-interrupts (@send p x))]
     [(symbol? p)
      (let ([dest (whereis p)])
        (unless dest
          (bad-arg 'send p))
        (no-interrupts (@send dest x)))]
     [else (bad-arg 'send p)]))

  (define (receive-after matcher src timeout timeout-handler)
    (cond
     [(and (or (fixnum? timeout) (bignum? timeout)) (>= timeout 0))
      ($receive matcher src (+ (erlang:now) timeout) timeout-handler)]
     [(eq? timeout 'infinity) ($receive matcher src #f #f)]
     [else (throw `#(timeout-value ,timeout ,src))]))

  (define (receive-until matcher src time timeout-handler)
    (cond
     [(and (or (fixnum? time) (bignum? time)) (>= time 0))
      ($receive matcher src time timeout-handler)]
     [(eq? time 'infinity) ($receive matcher src #f #f)]
     [else (throw `#(timeout-value ,time ,src))]))

  (define ($receive matcher src waketime timeout-handler)
    (disable-interrupts)
    (let find ([prev (pcb-inbox self)])
      (let ([msg (q-next prev)])
        (cond
         [(eq? (pcb-inbox self) msg)
          (cond
           [(not waketime)
            (pcb-src-set! self src)
            (@yield-preserving-interrupts #f 0)
            (pcb-src-set! self #f)
            (find prev)]
           [(< (erlang:now) waketime)
            (pcb-src-set! self src)
            (pcb-sleeping?-set! self #t)
            (@yield-preserving-interrupts sleep-queue waketime)
            (pcb-src-set! self #f)
            (find prev)]
           [else
            (enable-interrupts)
            (timeout-handler)])]
         [else
          (enable-interrupts)
          (cond
           [(matcher (msg-contents msg)) =>
            (lambda (run)
              (no-interrupts (@remove-q msg))
              (run))]
           [else
            (disable-interrupts)
            (find msg)])]))))

  (define process-default-ticks 1000)

  (define quantum-nanoseconds 1000000) ;; 1 millisecond

  (define (@insert-q x next)
    (let ([prev (q-prev next)])
      (q-next-set! prev x)
      (q-prev-set! x prev)
      (q-next-set! x next)
      (q-prev-set! next x)))

  (define (@remove-q x)
    (let ([prev (q-prev x)] [next (q-next x)])
      (q-next-set! prev next)
      (q-prev-set! next prev)
      (q-prev-set! x #f)
      (q-next-set! x #f))
    x)

  (define (@event-check)
    (unless (queue-empty? sleep-queue)
      (let ([rt (erlang:now)])
        (let wake ([p (q-next sleep-queue)])
          (when (and (not (eq? sleep-queue p)) (<= (pcb-precedence p) rt))
            (let ([next (q-next p)])
              (pcb-sleeping?-set! p #f)
              (@enqueue p run-queue 0)
              (wake next)))))))

  (define (@system-sleep-time)
    (cond
     [(not (queue-empty? run-queue)) 0]
     [(queue-empty? sleep-queue) #x1FFFFFFF]
     [else
      (min (max (- (pcb-precedence (q-next sleep-queue)) (erlang:now)) 0)
           #x1FFFFFFF)]))

  (define (yield queue precedence)
    (@yield queue precedence (disable-interrupts)))

  (define (@yield-preserving-interrupts queue precedence)
    ;; If this process is interrupted, @yield should invoke the keyboard
    ;; interrupt handler with interrupts enabled. Since we're called with
    ;; interrupts disabled, we maintain this by disabling interrupts if
    ;; @yield returns after it enables interrupts.
    (disable-interrupts)
    (@yield queue precedence (enable-interrupts))
    (disable-interrupts))

  ;; called with interrupts disabled, but may enable interrupts after restarting
  ;; the new process
  (define (@yield queue precedence disable-count)
    (@event-check)
    (when (alive? self)
      (pcb-winders-set! self (#%$current-winders))
      (pcb-exception-state-set! self (current-exception-state)))
    (#%$current-winders '())

    ;; snap the continuation
    (call/1cc
     (lambda (k)
       (when (alive? self)
         (pcb-cont-set! self k)
         (cond
          [queue (@enqueue self queue precedence)]
          [(enqueued? self) (@remove-q self)]))

       ;; context switch
       (pcb-sic-set! self disable-count)
       (let ([p (q-next run-queue)])
         (when (eq? p run-queue)
           (panic 'run-queue-empty))
         (set-self! (@remove-q p)))

       ;; adjust system interrupt counter for the new process
       (let loop ([next-sic (pcb-sic self)])
         (unless (fx= next-sic disable-count)
           (cond
            [(fx> next-sic disable-count)
             (disable-interrupts)
             (loop (fx- next-sic 1))]
            [else
             (enable-interrupts)
             (loop (fx+ next-sic 1))])))

       ;; Restart the process
       ((pcb-cont self) (void))))

    ;; Restart point
    (#%$current-winders (pcb-winders self))
    (current-exception-state (pcb-exception-state self))
    (pcb-cont-set! self #f)            ;; drop ref
    (pcb-winders-set! self '())        ;; drop ref
    (pcb-exception-state-set! self #f) ;; drop ref
    (osi_set_quantum quantum-nanoseconds)
    (set-timer process-default-ticks)
    (if (pcb-interrupt? self)
        (begin
          (pcb-interrupt?-set! self #f)
          (enable-interrupts)
          ((keyboard-interrupt-handler)))
        (enable-interrupts)))

  (define @thunk->cont
    (let ([return #f])
      (lambda (thunk)
        (let ([winders (#%$current-winders)])
          (#%$current-winders '())
          (let ([k (call/1cc
                    (lambda (k)
                      ;; Don't close over k, or the new process will
                      ;; keep the current continuation alive.
                      (set! return k)
                      (#%$current-stack-link #%$null-continuation)
                      (let ([reason
                             (call/cc
                              (lambda (done)
                                (call/1cc return)
                                (current-exception-state
                                 (create-exception-state done))
                                (pcb-cont-set! self #f) ;; drop ref
                                (osi_set_quantum quantum-nanoseconds)
                                (set-timer process-default-ticks)
                                (enable-interrupts)
                                (exit-handler
                                 (case-lambda
                                  [() (raise 'normal)]
                                  [(x . args) (throw x)]))
                                (reset-handler
                                 (lambda () (done (make-fault 'reset))))
                                (thunk)
                                'normal))])
                        ;; Process finished
                        (disable-interrupts)
                        (@kill self reason)
                        (yield #f 0))))])
            (set! return #f)
            (#%$current-winders winders)
            k)))))

  (define (@kill-linked links p reason raw-reason)
    (unless (null? links)
      (let ([linked (car links)])
        (cond
         [(not (alive? linked))]
         [(pcb-trap-exit linked) (@send linked (make-EXIT-msg p raw-reason))]
         [(not (eq? reason 'normal)) (@kill linked raw-reason)]))
      (@kill-linked (cdr links) p reason raw-reason)))

  (define (whereis name)
    (unless (symbol? name)
      (bad-arg 'whereis name))
    (no-interrupts (eq-hashtable-ref registrar name #f)))

  (define (get-registered)
    (vector->list (no-interrupts (hashtable-keys registrar))))

  (define (register name p)
    (unless (symbol? name)
      (bad-arg 'register name))
    (unless (pcb? p)
      (bad-arg 'register p))
    (with-interrupts-disabled
     (cond
      [(not (alive? p)) (throw `#(process-dead ,p))]
      [(pcb-name p) =>
       (lambda (name) (throw `#(process-already-registered ,name)))]
      [(eq-hashtable-ref registrar name #f) =>
       (lambda (pid) (throw `#(name-already-registered ,pid)))]
      [else
       (pcb-name-set! p name)
       (eq-hashtable-set! registrar name p)
       #t])))

  (define (unregister name)
    (unless (symbol? name)
      (bad-arg 'unregister name))
    (with-interrupts-disabled
     (let ([p (eq-hashtable-ref registrar name #f)])
       (unless p
         (bad-arg 'unregister name))
       (pcb-name-set! p #f)
       (eq-hashtable-delete! registrar name)
       #t)))

  (define finalizers '())

  (define (add-finalizer finalizer)
    (unless (procedure? finalizer)
      (bad-arg 'add-finalizer finalizer))
    (set! finalizers (cons finalizer finalizers)))

  (define (run-finalizers ls)
    (unless (null? ls)
      ((car ls))
      (run-finalizers (cdr ls))))

  (define (finalizer-loop)
    (receive
     [,_
      (let pump ()
        (receive (until 0 'ok)
          [,_ (pump)]))])
    (run-finalizers finalizers)
    (finalizer-loop))

  (define (@event-loop)
    (run-callbacks (osi_get_callbacks (@system-sleep-time)))
    (yield run-queue 0)
    (@event-loop))

  (define (run-callbacks ls)
    (cond
     [(null? ls) (void)]
     [else
      (let ([x (car ls)])
        (run-callbacks (cdr ls))
        (apply (car x) (cdr x)))]))

  (define process-table (make-weak-eq-hashtable))

  (define run-queue (make-queue))
  (define sleep-queue (make-queue))

  (define (wait-for-io name)
    (pcb-blocked-io?-set! self #t)
    (pcb-src-set! self name)
    (yield #f 0)
    (void))

  (define (complete-io p)
    (unless (pcb? p)
      (bad-arg 'complete-io p))
    (no-interrupts
     (when (pcb-blocked-io? p)
       (pcb-blocked-io?-set! p #f)
       (pcb-src-set! p #f)
       (@enqueue p run-queue 0))))

  (define registrar (make-eq-hashtable))

  (define event-loop-process #f)
  (define finalizer-process #f)

  (define (console-event-handler event)
    (with-interrupts-disabled
     (parameterize ([print-graph #t])
       (let ([op (console-error-port)]
             [ht (or (event-condition-table) (make-eq-hashtable))])
         (event-condition-table ht)
         (fprintf op "\nDate: ~a\n" (date-and-time))
         (fprintf op "Timestamp: ~a\n" (erlang:now))
         (fprintf op "Event: ~s\n" event)
         (let* ([keys (hashtable-keys ht)]
                [end (vector-length keys)])
           (do ([i 0 (fx1+ i)]) ((fx= i end))
             (let ([c (vector-ref keys i)])
               (unless (eq? (eq-hashtable-ref ht c #f) 'dumped)
                 (eq-hashtable-set! ht c 'dumped)
                 (unless (fault-condition? c)
                   (fprintf op "Condition: ")
                   (display-condition c op)
                   (newline op))
                 (cond
                  [(and (continuation-condition? c) (condition-continuation c)) =>
                   (lambda (k)
                     (fprintf op "Stack:\n")
                     (dump-stack k op 'default))])))))
         (newline op)
         (flush-output-port op)))))

  (define make-process-parameter
    (case-lambda
     [(initial filter)
      (unless (procedure? filter)
        (bad-arg 'make-process-parameter filter))
      (let ([initial (filter initial)])
        (rec process-parameter
          (case-lambda
           [() (#3%eq-hashtable-ref (pcb-parameters self)
                 process-parameter initial)]
           [(u) (#3%eq-hashtable-set! (pcb-parameters self)
                  process-parameter (filter u))])))]
     [(initial)
      (rec process-parameter
        (case-lambda
         [() (#3%eq-hashtable-ref (pcb-parameters self)
               process-parameter initial)]
         [(u) (#3%eq-hashtable-set! (pcb-parameters self)
                process-parameter u)]))]))

  (define inherited-parameters
    (make-parameter '()
      (lambda (ps)
        (unless (and (list? ps) (andmap procedure? ps))
          (bad-arg 'inherited-parameters ps))
        ps)))

  (define (add-inherited p)
    (no-interrupts
     (inherited-parameters (cons p (inherited-parameters))))
    p)

  (define make-inherited-parameter
    (case-lambda
     [(initial filter)
      ;; To reduce overhead in inherit-parameters, the internal inherited
      ;; parameter does not call the filter. Instead, we return a wrapper that
      ;; calls the provided filter when user code sets the parameter. The
      ;; inherit-parameters code then propagates these filtered values into
      ;; spawned processes.
      (unless (procedure? filter)
        (bad-arg 'make-inherited-parameter filter))
      (let ([no-filter-param (make-process-parameter (filter initial))])
        (add-inherited no-filter-param)
        (case-lambda
         [() (no-filter-param)]
         [(v) (no-filter-param (filter v))]))]
     [(initial)
      (add-inherited (make-process-parameter initial))]))

  ;; to get better names for match continuations than native or
  (define-syntax match-or
    (syntax-rules ()
      [(_) #f]
      [(_ e) e]
      [(_ e0 e1 ...)
       (let ([matched-pattern e0]) ;; could we get actual source info for these?
         (if matched-pattern matched-pattern (match-or e1 ...)))]))

  (define-syntax (match x)
    (syntax-case x ()
      [(_ exp (pattern (guard g) b1 b2 ...))
       (eq? (datum guard) 'guard)
       #`(match-let* ([pattern (guard g) exp]) b1 b2 ...)]
      [(_ exp (pattern b1 b2 ...))
       #`(match-let* ([pattern exp]) b1 b2 ...)]
      [(_ exp (pattern b1 b2 ...) ...)
       #`(let ([v exp])
           ((match-or (match-pattern v pattern b1 b2 ...) ...
              (bad-match v #,(find-source x)))))]))

  (define-syntax match-pattern
    (syntax-rules ()
      [(_ e pat (guard g) b1 b2 ...)
       (eq? (datum guard) 'guard)
       (match-one e pat fail-false (and g (lambda () b1 b2 ...)))]
      [(_ e pat b1 b2 ...)
       (match-one e pat fail-false (lambda () b1 b2 ...))]))

  (define-syntax (match-let* x)
    (syntax-case x ()
      [(_ () b1 b2 ...)
       #'(let () b1 b2 ...)]
      [(_ ([pattern exp] . rest) b1 b2 ...)
       #`(let ([v exp])
           (let-syntax ([fail
                         (syntax-rules ()
                           [(__) (bad-match v #,(find-source #'pattern))])])
             (match-one v pattern fail (match-let* rest b1 b2 ...))))]
      [(_ ([pattern (guard g) exp] . rest) b1 b2 ...)
       (eq? (datum guard) 'guard)
       #`(let ([v exp])
           (let-syntax ([fail
                         (syntax-rules ()
                           [(__) (bad-match v #,(find-source #'pattern))])])
             (match-one v pattern fail
               (if g
                   (match-let* rest b1 b2 ...)
                   (bad-match v #,(find-source #'g))))))]))

  (define-syntax fail-false
    (syntax-rules ()
      [(_) #f]))

  (define (bad-match v src)
    (throw `#(bad-match ,v ,src)))

  (define extension)
  (define extensions)

  (define-syntax (define-match-extension x)
    (syntax-case x ()
      [(_ type handle-object-expr)
       #'(define-match-extension type handle-object-expr
           (lambda x
             (errorf 'define-match-extension
               "no handle-field procedure provided for ~s" 'type)))]
      [(_ type handle-object-expr handle-field-expr)
       #'(begin
           (define-syntax handle-object
             (make-compile-time-value handle-object-expr))
           (define-syntax handle-field
             (make-compile-time-value handle-field-expr))
           (define-property type extensions
             #'(extension handle-object handle-field)))]))

  (meta define (get-rtd lookup id)
    (let ([hit (lookup id)])
      (and (list? hit)
           (apply
            (case-lambda
             [(ignore1 rtd . ignore2)
              (and (record-type-descriptor? rtd) rtd)]
             [other #f])
            hit))))

  (meta define (get-extended lookup type)
    (define (reject-options options context)
      (syntax-case options ()
        [() (void)]
        [_ (pretty-syntax-violation "invalid match pattern" context)]))
    (cond
     [(not (identifier? type)) 'bad-pattern]
     [(lookup type #'extensions) =>
      (lambda (x)
        (syntax-case x (extension)
          [(extension handle-object handle-field)
           (values
            (lookup #'handle-object)
            (lookup #'handle-field))]))]
     [(lookup type #'fields) =>
      ;; tuple
      (lambda (fields)
        (values
         (lambda (v pattern)
           ;; handle-object
           (syntax-case pattern (quasiquote)
             [`(type spec ...)
              #`((guard (type is? #,v))
                 ;; could convert to sub-match, but this is easier
                 (handle-fields (#,v type) spec ...))]))
         ;; handle-field
         (lambda (input fld var options context)
           (reject-options options context)
           (and (memq (syntax->datum fld) fields)
                (syntax-case input ()
                  [(v type)
                   #`((bind #,var (type no-check #,fld v)))])))))]
     [(get-rtd lookup type) =>
      ;; native record
      (lambda (rtd)
        (values
         ;; handle-object
         (lambda (v pattern)
           (syntax-case pattern (quasiquote)
             [`(type spec ...)
              #`((guard ((#3%record-predicate '#,rtd) #,v))
                 (handle-fields #,v spec ...))]))
         ;; handle-field
         (lambda (v fld var options context)
           (reject-options options context)
           (and (guard (c [else #f])
                  (csv7:record-field-accessible? rtd (syntax->datum fld)))
                #`((bind #,var
                     ((#3%csv7:record-field-accessor '#,rtd '#,fld) #,v)))))))]
     [else #f]))

  ;; fail is currently propagated unchanged
  (meta define (match-help lookup x nested?)
    (define (bad-pattern x)
      (syntax-case x ()
        [(e pat . rest)
         (pretty-syntax-violation "invalid match pattern" #'pat)]))
    (define (bad-syntax context msg detail)
      (syntax-case context ()
        [(e pat . rest)
         (pretty-syntax-violation msg #'pat detail)]))
    (define reject-duplicate
      (let ([bound-vars '()])
        (define (duplicate? id)
          (lambda (b) (bound-identifier=? b id)))
        (lambda (id)
          (if (ormap (duplicate? id) bound-vars)
              (syntax-error id "duplicate pattern variable")
              (set! bound-vars (cons id bound-vars))))))
    (define-syntax bind-help
      (syntax-rules ()
        [(_ v e expr)
         (if nested?
             #`(let ([v e]) #,expr)
             #`(begin (define v e) #,expr))]))
    (define-syntax bind-var
      (syntax-rules ()
        [(_ v e expr)
         (let ()
           (reject-duplicate #'v)
           (bind-help v e expr))]))
    (define-syntax fresh-var
      (syntax-rules ()
        [(_ v e expr)
         (with-temporaries (v)
           (bind-help v e expr))]))
    (define-syntax test
      (syntax-rules ()
        [(_ pred body fail)
         (if nested?
             #`(if pred body (fail))
             (with-temporaries (tmp)
               #`(begin (define tmp (unless pred (fail))) body)))]))
    (define (generate context object? handle-field fail body ir)
      (let f ([ir ir])
        (syntax-case ir ()
          [#f (bad-pattern x)]
          [() body]
          [((bind v e) . more)
           (eq? (datum bind) 'bind)
           (bind-var v e (f #'more))]
          [((guard g) . more)
           (eq? (datum guard) 'guard)
           (test g #,(f #'more) #,fail)]
          [((sub-match v pattern))
           (and object? (eq? (datum sub-match) 'sub-match))
           ;; sub-match pattern does not allow guard
           (convert #`(v pattern #,fail #,body))]
          [((handle-fields v field-spec ...))
           (and object? (eq? (datum handle-fields) 'handle-fields))
           (let ()
             (define (do-field field dest-var options body)
               ;; We don't check whether handler emits a bind for dest-var.
               ;; If the code is in a library, the compiler will complain for us.
               (generate context #f #f fail body
                 (or (handle-field #'v field dest-var options context)
                     (bad-syntax x "unknown field" field))))
             (let match-extended-help ([specs #'(field-spec ...)])
               (syntax-case specs (unquote unquote-splicing)
                 [() body]
                 [((unquote field) . rest)
                  (if (identifier? #'field)
                      (do-field #'field #'field '()
                        (match-extended-help #'rest))
                      (bad-pattern x))]
                 [((unquote-splicing var) . rest)
                  (if (identifier? #'var)
                      (with-temporaries (tmp)
                        (do-field #'var #'tmp '()
                          (test (equal? tmp var)
                            #,(match-extended-help #'rest)
                            #,fail)))
                      (bad-pattern x))]
                 [([field pattern option ...] . rest)
                  (with-temporaries (tmp)
                    (do-field #'field #'tmp #'(option ...)
                      (convert #`(tmp pattern #,fail #,(match-extended-help #'rest)))))]
                 [other (bad-pattern x)])))]
          [oops
           (pretty-syntax-violation
            (if object?
                "invalid handle-object output"
                "invalid handle-field output")
            context #'oops 'define-match-extension)])))
    (define (convert x)
      (syntax-case x (unquote unquote-splicing quasiquote)
        [(e (unquote (v <= pattern)) fail body)
         (and (identifier? #'v) (eq? (datum <=) '<=))
         (bind-var v e (convert #'(v pattern fail body)))]
        [(e (unquote v) fail body)
         (cond
          [(eq? (datum v) '_) #'body]
          [(identifier? #'v) (bind-var v e #'body)]
          [else (bad-pattern x)])]
        [(e (unquote-splicing var) fail body)
         (if (identifier? #'var)
             (test (equal? e var) body fail)
             (bad-pattern x))]
        [(e (quasiquote (type spec ...)) fail body)
         (call-with-values
           (lambda () (get-extended lookup #'type))
           (case-lambda
            [(fail)
             (cond
              [(eq? fail 'bad-pattern) (bad-pattern x)]
              [else (bad-syntax x "unknown type" #'type)])]
            [(handle-object handle-field)
             ;; extract pattern, don't rebuild or we'll get wrong source info
             (let ([pattern (syntax-case x () [(_ pat . _) #'pat])])
               (fresh-var v e
                 (generate pattern
                   #t handle-field #'fail #'body
                   (handle-object #'v pattern))))]))]
        [(e lit fail body)
         (let ([x (datum lit)])
           (or (symbol? x) (number? x) (boolean? x) (char? x)))
         (fresh-var v e (test (eqv? v 'lit) body fail))]
        [(e s fail body)
         (string? (datum s))
         (fresh-var v e (test (and (string? v) (#3%string=? v s)) body fail))]
        [(e eof fail body)
         (eof-object? (datum eof))
         (fresh-var v e (test (eof-object? v) body fail))]
        [(e bv fail body)
         (bytevector? (datum bv))
         (fresh-var v e (test (and (bytevector? v) (#3%bytevector=? v bv)) body fail))]
        [(e () fail body)
         (fresh-var v e (test (null? v) body fail))]
        [(e (first . rest) fail body)
         (fresh-var v e
           (test (pair? v)
             #,(convert
                #`((#3%car v) first fail
                   #,(convert #'((#3%cdr v) rest fail body))))
             fail))]
        [(e #(element ...) fail body)
         (let ([len (length (datum (element ...)))])
           (fresh-var v e
             (test (and (vector? v)
                        (#3%fx= (#3%vector-length v) #,len))
               #,(let lp ([i 0] [elt* #'(element ...)])
                   (if (= i len)
                       #'body
                       (convert
                        #`((#3%vector-ref v #,i)
                           #,(car elt*) fail #,(lp (+ i 1) (cdr elt*))))))
               fail)))]
        [_ (bad-pattern x)]))
    (convert x))

  (define-syntax (match-one x)
    (syntax-case x ()
      [(_ e pattern fail body)
       (lambda (lookup)
         (match-help lookup #'(e pattern fail body) #t))]))

  (define-syntax (match-define x)
    (lambda (lookup)
      (syntax-case x ()
        [(_ pattern e)
         #`(begin
             (define v e)
             (define (fail) (pariah (bad-match v #,(find-source #'pattern))))
             #,(match-help lookup
                 #'(v pattern fail (begin)) #f))])))

  (meta define (generate-name prefix fn)
    (if (not prefix) fn (compound-id fn prefix fn)))

  (meta define (get-binding-names bindings)
    (syntax-case bindings ()
      [((fn fv) . rest)
       (cons #'fn (get-binding-names #'rest))]
      [() '()]))

  (meta define (remove-binding f bindings)
    (syntax-case bindings ()
      [((fn fv) . rest)
       (if (syntax-datum-eq? #'fn f)
           #'rest
           #`((fn fv) #,@(remove-binding f #'rest)))]))

  (meta define (find-index fn fields index)
    (let ([f (scar fields)])
      (if (syntax-datum-eq? f fn)
          index
          (find-index fn (scdr fields) (+ index 1)))))

  (meta define (find-binding f bindings)
    (syntax-case bindings ()
      [((fn fv) . rest)
       (if (syntax-datum-eq? #'fn f)
           #'fv
           (find-binding f #'rest))]
      [() #f]))

  (define-syntax (define-tuple x)
    (syntax-case x ()
      [(_ name field ...)
       (and (identifier? #'name)
            (valid-fields? x #'(field ...) #f '(make copy copy* is?)))
       #'(begin
           (define-syntax (name x)
             (define (handle-open x expr prefix field-names)
               (define (make-accessor fn)
                 (let ([new-name (generate-name prefix fn)])
                   #`(define-syntax #,new-name (identifier-syntax (name no-check #,fn tmp)))))
               (if (not (valid-fields? x field-names '(field ...) '()))
                   (syntax-case x ())
                   #`(begin
                       (define tmp
                         (let ([val #,expr])
                           (unless (name is? val)
                             (throw `#(bad-tuple name ,val ,#,(find-source x))))
                           val))
                       #,@(map make-accessor (syntax->list field-names)))))
             (define (handle-copy x e bindings mode)
               #`(let ([src #,e])
                   #,(case mode
                       [copy
                        #`(unless (name is? src)
                            (throw `#(bad-tuple name ,src ,#,(find-source x))))]
                       [copy*
                        (handle-open x #'src #f (get-binding-names bindings))])
                   (vector 'name #,@(copy-tuple #'(field ...) 1 bindings))))
             (define (valid-bindings? bindings)
               (valid-fields? x (get-binding-names bindings) '(field ...) '()))
             (define (make-tuple fields bindings)
               (if (snull? fields)
                   '()
                   (let* ([f (scar fields)]
                          [v (find-binding f bindings)])
                     (unless v
                       (syntax-error x
                         (format "missing field ~a in" (syntax->datum f))))
                     (cons v
                       (make-tuple (scdr fields) (remove-binding f bindings))))))
             (define (copy-tuple fields index bindings)
               (if (snull? fields)
                   '()
                   (let* ([f (scar fields)]
                          [v (find-binding f bindings)])
                     (if v
                         (cons v (copy-tuple (scdr fields) (+ index 1)
                                   (remove-binding f bindings)))
                         (cons #`(#3%vector-ref src #,(datum->syntax f index))
                           (copy-tuple (scdr fields) (+ index 1) bindings))))))

             (syntax-case x ()
               [(_name make . bindings)
                (and (eq? (datum make) 'make)
                     (valid-bindings? #'bindings))
                #`(vector 'name #,@(make-tuple #'(field ...) #'bindings))]
               [(name copy e . bindings)
                (and (eq? (datum copy) 'copy)
                     (valid-bindings? #'bindings))
                (handle-copy x #'e #'bindings 'copy)]
               [(name copy* e . bindings)
                (and (eq? (datum copy*) 'copy*)
                     (valid-bindings? #'bindings))
                (handle-copy x #'e #'bindings 'copy*)]
               [(name open expr prefix field-names)
                (and (eq? (datum open) 'open) (identifier? #'prefix))
                (handle-open x #'expr #'prefix #'field-names)]
               [(name open expr field-names)
                (eq? (datum open) 'open)
                (handle-open x #'expr #f #'field-names)]
               [(name is? . args)
                (eq? (datum is?) 'is?)
                (let ([is?
                       #'(lambda (x)
                           (and (vector? x)
                                (#3%fx= (#3%vector-length x) (length '(name field ...)))
                                (eq? (#3%vector-ref x 0) 'name)))])
                  (syntax-case #'args ()
                    [() is?]
                    [(e) #`(#,is? e)]
                    [else (syntax-case x ())]))]
               [(name fn e)
                (syntax-datum-eq? #'fn #'field)
                (with-syntax ([getter (replace-source x #'(name fn))])
                  #`(getter e))]
               ...
               [(name fn)
                (syntax-datum-eq? #'fn #'field)
                #`(lambda (x)
                    (unless (name is? x)
                      (throw `#(bad-tuple name ,x ,#,(find-source x))))
                    (#3%vector-ref x #,(find-index #'fn #'(field ...) 1)))]
               ...
               [(name no-check fn e)
                (and (eq? (datum no-check) 'no-check)
                     (syntax-datum-eq? #'fn #'field))
                #`(#3%vector-ref e #,(find-index #'fn #'(field ...) 1))]
               ...
               [(name fn)
                (identifier? #'fn)
                (syntax-violation #f "unknown field" x #'fn)]
               [(name fn expr)
                (identifier? #'fn)
                (syntax-violation #f "unknown field" x #'fn)]
               ))
           (define-property name fields '(field ...)))]))

  (define (legacy-EXIT? x)
    (and (vector? x)
         (#3%fx= (#3%vector-length x) 2)
         (eq? (#3%vector-ref x 0) 'EXIT)))

  (define-syntax direct-&fault-condition-ref
    (syntax-rules ()
      [(_ field x)
       ((#3%csv7:record-field-accessor (record-type-descriptor &fault-condition) field) x)]))

  (define-syntax exit-reason (syntax-rules ()))
  (define-match-extension exit-reason
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(exit-reason always-match? r e)
         (with-temporaries (is-fault? tmp.reason)
           #`((bind is-fault? (fault-condition? #,v))
              (guard (or always-match? is-fault? (legacy-EXIT? #,v)))
              (bind tmp.reason
                (if is-fault?
                    (direct-&fault-condition-ref 'reason #,v)
                    (if always-match?
                        (if (legacy-EXIT? #,v)
                            (#3%vector-ref #,v 1)
                            #,v)
                        (#3%vector-ref #,v 1))))
              (handle-fields (#,v is-fault? tmp.reason) [reason r] [err e])))]))
    (lambda (input fld var options context)
      (syntax-case input ()
        [(v is-fault? tmp.reason)
         (case (syntax->datum fld)
           [(reason) #`((bind #,var tmp.reason))]
           [(err) #`((bind #,var (if (and is-fault? (direct-&fault-condition-ref 'k v)) v tmp.reason)))]
           [else (pretty-syntax-violation "unknown field" fld)])])))

  (define-match-extension catch
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(catch r)
         (with-temporaries (tmp)
           #`((sub-match #,v `(exit-reason #f r ,_))))]
        [`(catch r e)
         (with-temporaries (tmp)
           #`((sub-match #,v `(exit-reason #f r e))))])))

  (define-syntax DOWN (syntax-rules ()))
  (define-match-extension DOWN
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(DOWN m p r)
         #`((sub-match #,v `(DOWN-msg [monitor m] [pid p] [reason `(exit-reason #t r ,_)])))]
        [`(DOWN m p r e)
         #`((sub-match #,v `(DOWN-msg [monitor m] [pid p] [reason `(exit-reason #t r e)])))])))

  (define-syntax EXIT (syntax-rules ()))
  (define-match-extension EXIT
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(EXIT p r)
         #`((sub-match #,v `(EXIT-msg [pid p] [reason `(exit-reason #t r ,_)])))]
        [`(EXIT p r e)
         #`((sub-match #,v `(EXIT-msg [pid p] [reason `(exit-reason #t r e)])))])))

  (define-syntax redefine
    (syntax-rules ()
      [(_ var e) (#%$set-top-level-value! 'var e)]))

  (define event-condition-table (make-parameter #f))
  (define (reset-console-event-handler) (event-condition-table #f))

  (define (add-event-condition! c)
    (let ([ht (event-condition-table)])
      (when (and ht (not (eq-hashtable-ref ht c #f)))
        (eq-hashtable-set! ht c #t))))

  (define weak-parent (make-process-parameter #f))

  (record-writer (record-type-descriptor mon)
    (lambda (r p wr)
      (display-string "#<monitor " p)
      (wr (mon-origin r) p)
      (display-string " " p)
      (wr (mon-target r) p)
      (write-char #\> p)))

  (record-writer (record-type-descriptor pcb)
    (lambda (r p wr)
      (display-string "#<process " p)
      (display-string (global-process-id r) p)
      (let ([name (pcb-name r)])
        (when name
          (write-char #\space p)
          (wr name p)))
      (write-char #\> p)))

  ;; support equal-hash on mon and pcb records so folks don't get burned
  ;; by the default record-hash-procedure when using functional hash tables
  (record-type-hash-procedure (record-type-descriptor mon)
    (lambda (m hash)
      (match-define `(mon ,origin ,target) m)
      (+ (pcb-id origin)
         (ash (pcb-id target) (quotient (fixnum-width) 2)))))

  (record-type-hash-procedure (record-type-descriptor pcb)
    (lambda (pcb hash)
      (pcb-id pcb)))

  (record-writer (csv7:record-type-descriptor
                  (condition (make-error) (make-warning)))
    (lambda (x p wr)
      (display-string "#<compound condition: " p)
      (display-condition x p)
      (write-char #\> p)
      (add-event-condition! x)))

  (record-writer (record-type-descriptor &fault-condition)
    (lambda (r p wr)
      (display-string "#<fault " p)
      (wr (fault-condition-reason r) p)
      (write-char #\> p)
      (add-event-condition! r)))

  (record-writer (record-type-descriptor EXIT-msg)
    (lambda (r p wr)
      (display-string "#<EXIT " p)
      (wr (EXIT-msg-pid r) p)
      (display-string " " p)
      (wr (unwrap-fault-condition (EXIT-msg-reason r)) p)
      (write-char #\> p)))

  (record-writer (record-type-descriptor DOWN-msg)
    (lambda (r p wr)
      (display-string "#<DOWN " p)
      (wr (DOWN-msg-pid r) p)
      (display-string " " p)
      (wr (unwrap-fault-condition (DOWN-msg-reason r)) p)
      (write-char #\> p)))

  (disable-interrupts)
  (set-self! (@make-process #f))
  (set! event-loop-process
    (spawn
     (lambda ()
       (disable-interrupts)
       (@event-loop))))
  (set! finalizer-process (spawn finalizer-loop))
  (timer-interrupt-handler
   (lambda ()
     (if (osi_is_quantum_over)
         (yield run-queue 0)
         (set-timer process-default-ticks))))
  (redefine collect
    (let ([system-collect (#%$top-level-value 'collect)])
      (lambda args
        (apply system-collect args)
        (send finalizer-process 'go))))

  ;; Redefine Chez Scheme parameters
  (redefine command-line
    (make-process-parameter (command-line)
      (lambda (x)
        (unless (and (list? x) (andmap string? x))
          (bad-arg 'command-line x))
        x)))
  (redefine command-line-arguments
    (make-process-parameter (command-line-arguments)
      (lambda (x)
        (unless (and (list? x) (andmap string? x))
          (bad-arg 'command-line-arguments x))
        x)))
  (redefine custom-port-buffer-size
    (make-process-parameter 1024
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'custom-port-buffer-size x))
        x)))
  (redefine exit-handler
    (make-process-parameter (exit-handler)
      (lambda (x)
        (unless (procedure? x)
          (bad-arg 'exit-handler x))
        x)))
  (redefine keyboard-interrupt-handler
    (make-process-parameter (keyboard-interrupt-handler)
      (lambda (x)
        (unless (procedure? x)
          (bad-arg 'keyboard-interrupt-handler x))
        x)))
  (redefine pretty-initial-indent
    (make-process-parameter 0
      (lambda (x)
        (unless (and (fixnum? x) (fx>= x 0))
          (bad-arg 'pretty-initial-indent x))
        x)))
  (redefine pretty-line-length
    (make-process-parameter 75
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'pretty-line-length x))
        x)))
  (redefine pretty-maximum-lines
    (make-process-parameter #f
      (lambda (x)
        (unless (or (not x) (and (fixnum? x) (fx>= x 0)))
          (bad-arg 'pretty-maximum-lines x))
        x)))
  (redefine pretty-one-line-limit
    (make-process-parameter 60
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'pretty-one-line-limit x))
        x)))
  (redefine pretty-standard-indent
    (make-process-parameter 1
      (lambda (x)
        (unless (and (fixnum? x) (fx>= x 0))
          (bad-arg 'pretty-standard-indent x))
        x)))
  (redefine print-brackets (make-process-parameter #t (lambda (x) (and x #t))))
  (redefine print-char-name (make-process-parameter #f (lambda (x) (and x #t))))
  (redefine print-gensym
    (make-process-parameter #t
      (lambda (x) (if (memq x '(pretty pretty/suffix)) x (and x #t)))))
  (redefine print-graph (make-process-parameter #f (lambda (x) (and x #t))))
  (redefine print-length
    (make-process-parameter #f
      (lambda (x)
        (unless (or (not x) (and (fixnum? x) (fx>= x 0)))
          (bad-arg 'print-length x))
        x)))
  (redefine print-level
    (make-process-parameter #f
      (lambda (x)
        (unless (or (not x) (and (fixnum? x) (fx>= x 0)))
          (bad-arg 'print-level x))
        x)))
  (redefine print-precision
    (make-process-parameter #f
      (lambda (x)
        (unless (or (not x) (and (or (fixnum? x) (bignum? x)) (> x 0)))
          (bad-arg 'print-precision x))
        x)))
  (redefine print-radix
    (make-process-parameter 10
      (lambda (x)
        (unless (and (fixnum? x) (fx<= 2 x 36))
          (bad-arg 'print-radix x))
        x)))
  (redefine print-record (make-process-parameter #t (lambda (x) (and x #t))))
  (redefine print-unicode (make-process-parameter #t (lambda (x) (and x #t))))
  (redefine print-vector-length
    (make-process-parameter #f (lambda (x) (and x #t))))
  (redefine reset-handler
    (make-process-parameter (reset-handler)
      (lambda (x)
        (unless (procedure? x)
          (bad-arg 'reset-handler x))
        x)))
  (redefine waiter-prompt-and-read
    (make-process-parameter (waiter-prompt-and-read)
      (lambda (x)
        (unless (procedure? x)
          (bad-arg 'waiter-prompt-and-read x))
        x)))
  (redefine waiter-prompt-string
    (make-process-parameter (waiter-prompt-string)
      (lambda (x)
        (unless (string? x)
          (bad-arg 'waiter-prompt-string x))
        x)))

  (osi_set_quantum quantum-nanoseconds)
  (set-timer process-default-ticks)
  (enable-interrupts))
