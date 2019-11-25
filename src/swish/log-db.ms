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
 (swish script-testing)
 )

(define (stop server)
  (cond
   [(whereis server) => (lambda (pid) (kill pid 'kill))]
   [else 'ok]))

(mat coerce ()
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
    [,n (guard (and (integer? n) (> n 0)))
      (coerce (spawn (lambda () 'ok)))]
    ["Mon, 2 Feb 2015 01:02:03 +0000" (coerce (make-date 0 3 2 1 2 2 2015 0))]
    [#(error "Exception: condition without a stack.")
     (read
      (open-input-string
       (coerce (condition
                (make-error)
                (make-message-condition "condition without a stack")))))]
    [#(error "Exception in car: 12 is not a pair." ,stack)
     (guard (starts-with? stack "#<system continuation in"))
     (read
      (open-input-string
       (coerce
        (match (catch (car 12))
          [#(EXIT ,reason) reason]))))]
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
    ["#(error \"Exception occurred with condition components:\\n  0. &irritants: #0=(1 . #0#).\")"
     (coerce (make-irritants-condition '#1=(1 . #1#)))])
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
       (receive (after 1000 (raise 'timeout))
         [#(EXIT ,@log-db bad-logger)
          'ok])))))

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
  (process-trap-exit #t)
  (on-exit (begin (for-each stop '(log-db event-mgr)) (sqlite:close db-handle))
    (match-let*
     ([,restore (utf8->string (read-file "src/swish/migration-test.sql"))]
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
      [,_ (db:stop db)]
      [#(ok ,log-db)
       (parameterize ([log-file db-file])
         (start-event-mgr)
         (log-db:start&link))])
     ;; migrate
     (log-db:setup (list swish-event-logger))
     ;; check data by selecting only what was present in original schema
     (match-let*
      ([,@orig-child
        (transaction 'log-db
          (execute "select id, name, supervisor, restart_type, type, shutdown, start, duration, killed, reason from child order by rowid"))]
       [,@orig-gen-server-debug
        (transaction 'log-db
          (execute "select timestamp, duration, type, client, server, message, state, reply from gen_server_debug order by rowid"))]
       [,@orig-gen-server-terminating
        (transaction 'log-db
          (execute "select timestamp, name, last_message, state, reason from gen_server_terminating order by rowid"))]
       [,@orig-http-request
        (transaction 'log-db
          (execute "select timestamp, pid, host, method, path, header, params from http_request order by rowid"))]
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
          (execute "select timestamp, supervisor, error_context, child_pid, child_name from supervisor_error order by rowid"))]
       [(#(,reason)) orig-supervisor-error-reason]
       [(#(,@reason))
        (transaction 'log-db
          (execute "select reason from supervisor_error order by rowid"))]
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
          (execute "select name, version from version order by rowid"))])
      'ok))))

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
     (try (json-stack->string (open-output-string) 123))])
   'ok))
