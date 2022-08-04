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

(import
 (chezscheme)
 (swish mat)
 (swish pregexp)
 (swish script-testing)
 )

(define (stop server)
  (cond
   [(whereis server) => (lambda (pid) (kill pid 'kill))]
   [else 'ok]))

(mat coerce ()
  (define (check-condition c message check-stack)
    (match-let*
     ([,s (coerce c)]
      [,obj (json:string->object s)]
      [,@message (json:ref obj 'message 'nope)])
     (check-stack (json:ref obj 'stacks 'nope))))
  (match-let*
   (["foo" (coerce "foo")]
    [0 (coerce 0)]
    [,n (guard (= n (most-positive-fixnum))) ; fixnum
     (coerce (most-positive-fixnum))]
    [,n (guard (= n (+ (most-positive-fixnum) 1))) ; small bignum
     (coerce (+ (most-positive-fixnum) 1))]
    [3.14159 (coerce 3.14159)]          ; flonum
    [#f (coerce #f)]
    [,n (guard (= n (expt 2 129)))      ; large real
     (coerce (expt 2 129))]
    [#vu8(1 2 3) (coerce '#vu8(1 2 3))]
    ["bar" (coerce 'bar)]
    ["gensym" (coerce (gensym "gensym"))]
    [,pid (spawn (lambda () 'ok))]
    [,expected (global-process-id pid)]
    [,@expected (coerce pid)]
    ["Mon, 2 Feb 2015 01:02:03 +0000" (coerce (make-date 0 3 2 1 2 2 2015 0))]
    [ok (check-condition
         (condition
          (make-error)
          (make-message-condition "condition without a stack"))
         "Exception: condition without a stack."
         (lambda (stacks) (match stacks [() 'ok])))]
    [ok (check-condition
         (match (catch (car 12))
           [#(EXIT ,reason) reason])
         "Exception in car: 12 is not a pair."
         (lambda (stacks)
           (match-let*
            ([(,stack0 . ,_) stacks]
             [#t (json:object? stack0)]
             [10 (json:ref stack0 'truncated 'wrong)]
             [,n (guard (> n 10)) (json:ref stack0 'depth #f)]
             [(,f0 . ,_) (json:ref stack0 'frames '())]
             [,s (json:ref f0 'description 'bad)]
             [#t (starts-with? s "#<system continuation in")])
            'ok)))]
    [ok (check-condition
         (try
          (eval ;; avoid getting source info
           '(let loop1 ([n 3] [m 1])
              (if (< n m)
                  (match m)
                  (list n m (loop1 (- n 1) (+ m 1)))))))
         "Pattern match failed: 3."
         (lambda (stacks)
           (define (check-frame f depth desc free)
             (match-let*
              ([,@desc (json:ref f 'description 'nope)]
               [,@depth (json:ref f 'depth 'nope)]
               ["stack-frame" (json:ref f 'type 'nope)]
               [none (json:ref f 'procedure-source 'none)]
               [,@free
                (and free
                     (map (lambda (v)
                            (list (json:ref v 'name "") (json:ref v 'value "")))
                       (sort (lambda (v0 v1)
                               (string<? (json:ref v0 'name "") (json:ref v1 'name "")))
                         (json:ref f 'free 'nope))))])
              'ok))
           (match-let*
            ([(,stack0) stacks]
             [#t (json:object? stack0)]
             [10 (json:ref stack0 'truncated 'wrong)]
             [,n (guard (> n 10)) (json:ref stack0 'depth #f)]
             [(,f0 ,f1 ,f2 ,f3 . ,_) (json:ref stack0 'frames 0)]
             [ok (check-frame f0 0 "#<continuation in loop1>" '())]
             [ok (check-frame f1 1 "#<continuation in loop1>" '(("m" "2") ("n" "2")))]
             [ok (check-frame f2 2 "#<continuation in loop1>" '(("m" "1") ("n" "3")))]
             [ok (check-frame f3 3 "#<system continuation in dynamic-wind>" #f)])
            'ok)))]
    ;; Other cases
    ["7.0+1.0i" (coerce 7.0+1.0i)]      ; complex
    ["#(<point> 1 2)"                   ; tuple
     (let ()
       (define-tuple <point> x y)
       (coerce (<point> make [x 1] [y 2])))]
    ["{\"bar\":[1,\"two\",{\"zip\":true}],\"foo\":123}"
     (coerce
      (json:make-object
       [foo 123]
       [bar (list 1 "two" (json:make-object [zip #t]))]))]
    ["#0=(1 . #0#)"
     (coerce '#0=(1 . #0#))]
    [ok (check-condition
         (make-irritants-condition '#1=(1 . #1#))
         "Exception occurred with condition components:\n  0. &irritants: #0=(1 . #0#)."
         (lambda (stacks) (match stacks [() 'ok])))])
   'ok))

(isolate-mat log-handler ()
  (process-trap-exit #t)
  (parameterize ([console-error-port (open-output-string)])
    (on-exit (for-each stop '(log-db event-mgr))
      (match-let*
       ([#(ok ,log-db)
         (parameterize ([log-file ":memory:"])
           (start-event-mgr)
           (log-db:start&link))]
        [#(error bad-setup)
         (log-db:setup
          (list
           (<event-logger> make
             [setup (lambda () (raise 'bad-setup))]
             [log (lambda (x) x)])))]
        [ignore
         (log-db:setup
          (list
           (<event-logger> make
             [setup void]
             [log (lambda (x)
                    (when (eq? x 'fail)
                      (raise 'bad-logger)))])))])
       (event-mgr:notify 'fail)
       (receive (after 1000 (throw 'timeout))
         [`(EXIT ,@log-db bad-logger)
          'ok])))))

(isolate-mat handler-already-set ()
  (process-trap-exit #t)
  (parameterize ([console-error-port (open-output-string)])
    (on-exit (for-each stop '(log-db event-mgr))
      (match-let*
       ([#(ok ,log-db)
         (parameterize ([log-file ":memory:"])
           (start-event-mgr)
           (log-db:start&link))]
        [ok (event-mgr:set-log-handler (lambda (event) (void)) self)]
        [#(error log-handler-already-set) (log-db:setup '())])
       'ok))))

(isolate-mat log-handler-endures ()
  (define me self)
  (define strikes 0)
  (define (make-logger who)
    (lambda (x)
      (match x
        [(crash ,@who) (raise `(fail ,who))]
        [,_ (send me (cons who x))])))
  (process-trap-exit #t)
  (parameterize ([console-error-port (open-output-string)])
    (on-exit (for-each stop '(log-db event-mgr))
      (match-let*
       ([#(ok ,log-db)
         (parameterize ([log-file ":memory:"])
           (start-event-mgr)
           (log-db:start&link))]
        [ignore
         (log-db:setup
          (list
           (log-db:event-logger
            [setup void]
            [log (make-logger 'logger-1)]
            [tolerate-fault? (lambda (e) #t)])
           ;; legacy logger
           (<event-logger> make
             [setup void]
             [log (make-logger 'logger-2)])
           (log-db:event-logger
            [setup void]
            [log (make-logger 'logger-3)]
            [tolerate-fault?
             (lambda (e)
               (set! strikes (+ strikes 1))
               (< strikes 3))])))]
        [(logger-1 . `(<system-attributes>)) (receive [,x x])]
        [(logger-2 . `(<system-attributes>)) (receive [,x x])]
        [(logger-3 . `(<system-attributes>)) (receive [,x x])]
        [nothing (receive (until 0 'nothing) [,x x])]
        [ok (event-mgr:notify 'safe)]
        [(logger-1 . safe) (receive [,x x])]
        [(logger-2 . safe) (receive [,x x])]
        [(logger-3 . safe) (receive [,x x])]
        [nothing (receive (until 0 'nothing) [,x x])]
        ;; logger-1 shielded from error, so log-db survives
        [ok (event-mgr:notify '(crash logger-1))]
        ;; crash in logger-1 so we never get to logger-2 or logger-3
        [nothing (receive (until 0 'nothing) [,x x])]
        ;; logger-3 tolerates strike 1, so log-db survives
        [ok (event-mgr:notify '(crash logger-3))]
        [(logger-1 crash logger-3) (receive [,x x])]
        [(logger-2 crash logger-3) (receive [,x x])]
        [nothing (receive (until 0 'nothing) [,x x])]
        [1 strikes]
        ;; logger-3 tolerates strike 2, so log-db survives
        [ok (event-mgr:notify '(crash logger-3))]
        [(logger-1 crash logger-3) (receive [,x x])]
        [(logger-2 crash logger-3) (receive [,x x])]
        [nothing (receive (until 0 'nothing) [,x x])]
        [2 strikes]
        ;; logger-2 is NOT shielded from error, so log goes down
        [ok (event-mgr:notify '(crash logger-2))]
        [(logger-1 crash logger-2) (receive [,x x])]
        [`(EXIT ,@log-db (fail logger-2)) (receive [,x x])]
        [nothing (receive (until 0 'nothing) [,x x])]
        [2 strikes])
       'ok))))

(isolate-mat prune-trigger ()
  (process-trap-exit #t)
  (on-exit (for-each stop '(log-db event-mgr))
    (match-let*
     ([#(ok ,log-db)
       (parameterize ([log-file ":memory:"])
         (start-event-mgr)
         (log-db:start&link))]
      [,max-days 1]
      [,logger (make-swish-event-logger max-days 10)]
      [`(<event-logger> ,setup ,log) logger]
      [ignore (log-db:setup (list logger))]
      [,now (erlang:now)]
      [,msg (<child-end> make
              [timestamp now]
              [pid self]
              [killed 0]
              [reason "arbitrary"]
              [details #f])]
      [,_ (log msg)]
      [,_ (log (<child-end> copy* msg
                 [timestamp (- now (* max-days 24 60 60 1000) 1)]
                 [reason "ancient"]))]
      [(#("ancient") #("arbitrary"))
       (transaction log-db
         (execute "select reason from child_end order by timestamp asc"))]
      [,_ (log (<child-end> copy* msg
                 [timestamp (+ now 1000)]
                 [reason "future"]))]
      [(#("arbitrary") #("future"))
       (transaction log-db
         (execute "select reason from child_end order by timestamp asc"))])
     'ok)))

(isolate-mat create ()
  (define db-file (path-combine (output-dir) (uuid->string (osi_make_uuid))))
  (process-trap-exit #t)
  (on-exit (for-each stop '(log-db event-mgr))
    (match-let*
     ([#f (file-exists? db-file)]
      [#(ok ,log-db)
       (parameterize ([log-file db-file])
         (start-event-mgr)
         (log-db:start&link))]
      [#t (file-exists? db-file)]
      [ignore (log-db:setup (list swish-event-logger))]
      [,id
       (guard (string? id))
       (log-db:get-instance-id)])
     'ok)))

(isolate-mat migrate ()
  (define startup (erlang:now))
  (define db-file "file::memory:?cache=shared")
  (define db-handle ;; hold so it's still available to log-db after db:stop
    (sqlite:open db-file
      (logor SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)))
  (define (fix-timestamps s)
    ;; We need to update the timestamps so that we don't have dates more than 90
    ;; days in the past lest the automatic pruner removes the entries.  The
    ;; timestamps occur in the same million milliseconds, so it's an easy string
    ;; replace.
    (pregexp-replace* (re "\\b1572453(?=\\d{6}\\b)") s
      (number->string (- (quotient startup 1000000) 1))))
  (process-trap-exit #t)
  (on-exit (begin (for-each stop '(log-db event-mgr)) (sqlite:close db-handle))
    (match-let*
     ([,restore (fix-timestamps (utf8->string (read-file "src/swish/migration-test.sql")))]
      [#(ok ,db) (db:start&link #f db-file 'create)]
      [,_ (transaction db
            (for-each execute (remq "" (split restore #\newline))))]
      [,orig-child
       (transaction db
         (execute "select * from child order by rowid"))]
      [,orig-gen-server-debug
       (transaction db
         (execute "select * from gen_server_debug order by rowid"))]
      [,orig-gen-server-terminating
       (transaction db
         (execute "select * from gen_server_terminating order by rowid"))]
      [,orig-http-request
       (transaction db
         (execute "select * from http_request order by rowid"))]
      [,http-join-1
       "select c.duration, h.path from http_request h inner join child c on h.pid = c.id order by h.rowid asc"]
      [,orig-http-join-1-results (transaction db (execute http-join-1))]
      [,orig-statistics
       (transaction db
         ;; several columns were dropped and some were added in 90f445b0c67339f27f61bc813b942a81c8fa1bf8
         ;; statements column was added in e4086f042fe2d6e7be560d8485f989857421b993
         (execute "select timestamp, date, reason, bytes_allocated, osi_bytes_used, sqlite_memory,
                        sqlite_memory_highwater, ports, databases, 0 as statements, listeners,
                        0 as watchers, cpu, real, bytes, gc_count, gc_cpu, gc_real, gc_bytes
                 from statistics order by rowid"))]
      [,orig-supervisor-error-primary
       (transaction db
         (execute "select timestamp, supervisor, error_context, child_pid, child_name from supervisor_error order by rowid"))]
      [,orig-supervisor-error-reason
       (transaction db
         (execute "select reason from supervisor_error order by rowid"))]
      [,orig-system-attributes
       (transaction db
         (execute "select * from system_attributes order by rowid"))]
      [,orig-system-attributes
       (transaction db
         (execute "select * from system_attributes order by rowid"))]
      [,orig-version
       (transaction db
         (execute "select * from version order by rowid"))]
      [,supervisor-query1
       "select c.name, c.start, c.duration, c.killed, s.error_context from child c inner join supervisor_error s on c.supervisor = s.supervisor order by c.name desc"]
      [,supervisor-query1-result (transaction db (execute supervisor-query1))]
      [,supervisor-query2
       "select c.name, c.start, c.duration, c.killed, s.error_context from child c inner join supervisor_error s on c.id = s.child_pid order by c.name desc"]
      [,supervisor-query2-result (transaction db (execute supervisor-query2))]
      [,gs-debug-query
       (string-append
        "select d.type, d.message, d.reply, cc.name, cs.name from gen_server_debug d "
        "inner join child cc on cc.id = d.client "
        "inner join child cs on cs.id = d.server "
        "order by d.rowid asc")]
      [,gs-debug-query-result (transaction db (execute gs-debug-query))]
      [,_ (db:stop db)]
      [#(ok ,log-db)
       (parameterize ([log-file db-file])
         (start-event-mgr)
         (log-db:start&link))]
      ;; migrate
      [ignore (log-db:setup (list swish-event-logger))]
      ;; check data by selecting only what was present in original schema
      [,@orig-child
       (transaction 'log-db
         (execute "select rowid, 0+ltrim(id,':'), name, 0+ltrim(supervisor,':'), restart_type, type, shutdown, start, duration, killed, reason from child order by rowid"))]
      [,@orig-gen-server-debug
       (transaction 'log-db
         (execute "select timestamp, duration, type, 0+ltrim(client,':'), 0+ltrim(server,':'), message, state, reply from gen_server_debug order by rowid"))]
      [,@orig-gen-server-terminating
       (transaction 'log-db
         (execute "select timestamp, name, last_message, state, reason from gen_server_terminating order by rowid"))]
      [,@orig-http-request
       (transaction 'log-db
         (execute "select timestamp, 0+ltrim(pid,':'), host, method, path, header, params from http_request order by rowid"))]
      [,@orig-statistics
       (transaction 'log-db
         (execute "select timestamp, date, reason, bytes_allocated, osi_bytes_used, sqlite_memory,
                         sqlite_memory_highwater,
                         json_extract(foreign_handles, '$.osi-ports') as ports,
                         json_extract(foreign_handles, '$.databases') as databases,
                         json_extract(foreign_handles, '$.statements') as statements,
                         json_extract(foreign_handles, '$.tcp-listeners') as listeners,
                         json_extract(foreign_handles, '$.path-watchers') as watchers,
                         cpu, real, bytes, gc_count, gc_cpu, gc_real, gc_bytes from statistics order by rowid"))]
      [,@orig-supervisor-error-primary
       (transaction 'log-db
         (execute "select timestamp, 0+ltrim(supervisor,':'), error_context, 0+ltrim(child_pid,':'), child_name from supervisor_error order by rowid"))]
      [(#(,reason1) #(,reason2)) orig-supervisor-error-reason]
      [#(error ,reason1) (read (open-input-string reason1))]
      [(#(,@reason1) #(,@reason2))
       (transaction 'log-db
         (execute "select ifnull(json_extract(details, '$.message'), reason) from supervisor_error order by rowid"))]
      [,@orig-system-attributes
       (transaction 'log-db
         ;; N.B. filter out entry added when we did log-db:setup during the test
         (execute "select timestamp, date,
                           json_extract(software_info, '$.swish.version') as software_version,
                           computer_name from system_attributes
                    where timestamp < ?
                    order by rowid"
           startup))]
      [(#("instance" ,instance-id) #("swish" ,original-schema)) orig-version]
      [(#("instance" ,@instance-id) #("swish" ,new-schema))
       (transaction 'log-db
         (execute "select name, version from version order by rowid"))]
      [,@supervisor-query1-result (transaction 'log-db (execute supervisor-query1))]
      [,@supervisor-query2-result (transaction 'log-db (execute supervisor-query2))]
      [,@gs-debug-query-result (transaction 'log-db (execute gs-debug-query))]
      [,@orig-http-join-1-results (transaction 'log-db (execute http-join-1))]
      )
     'ok)))

(isolate-mat bad-args ()
  (match-let*
   ([`(catch #(bad-arg stack->json 234)) (try (stack->json 234))]
    [,empty-stack (json:make-object [type "stack"])]
    [`(catch #(bad-arg stack->json port))
     (try (stack->json 'port empty-stack))]
    [`(catch #(bad-arg json-stack->string deck))
     (try (json-stack->string 'deck))]
    [,ip (open-input-string "")]
    [`(catch #(bad-arg json-stack->string ,@ip))
     (try (json-stack->string ip empty-stack))]
    [,op (let-values ([(op get) (open-bytevector-output-port)]) op)]
    [`(catch #(bad-arg json-stack->string ,@op))
     (try (json-stack->string op empty-stack))]
    [`(catch #(bad-arg json-stack->string 123))
     (try (json-stack->string (open-output-string) 123))]
    [`(catch #(bad-arg log-db:start&link "bad options"))
     (try (log-db:start&link "bad options"))]
    [`(catch #(bad-arg create-prune-on-insert-trigger -1))
     (try (create-prune-on-insert-trigger 'table 'column -1 10))]
    [`(catch #(bad-arg create-prune-on-insert-trigger 0))
     (try (create-prune-on-insert-trigger 'table 'column 44 0))]
    [`(catch #(bad-arg make-swish-event-logger -1))
     (try (make-swish-event-logger -1 10))]
    [`(catch #(bad-arg make-swish-event-logger 0))
     (try (make-swish-event-logger 44 0))]
    [`(catch #(bad-arg setup "xyz"))
     (try (log-db:event-logger [setup "xyz"] [log values]))]
    [`(catch #(bad-arg setup "xyz"))
     (try (log-db:event-logger [setup "xyz"] [log values]))]
    [`(catch #(bad-arg log "rhythm"))
     (try (log-db:event-logger [setup void] [log "rhythm"]))]
    [`(catch #(bad-arg tolerate-fault? "andreas"))
     (try (log-db:event-logger
           [setup void]
           [log values]
           [tolerate-fault? "andreas"]))]
    )
   'ok))
