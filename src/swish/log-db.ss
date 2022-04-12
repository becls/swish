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
(library (swish log-db)
  (export
   $migrate-pid-columns
   <event-logger>
   coerce
   create-prune-on-insert-trigger
   create-table
   define-simple-events
   json-stack->string
   log-db:event-logger
   log-db:get-instance-id
   log-db:setup
   log-db:start&link
   log-db:version
   make-swish-event-logger
   stack->json
   swish-event-logger
   )
  (import
   (chezscheme)
   (swish app-io)
   (swish db)
   (swish erlang)
   (swish errors)
   (swish event-mgr)
   (swish events)
   (swish internal)
   (swish io)
   (swish json)
   (swish options)
   (swish osi)
   (swish software-info)
   (swish string-utils)
   )

  (define-tuple <event-logger> setup log)

  (define (succumb event) #f)

  (define-options log-db:event-logger
    (required
     [setup (must-be (procedure/arity? #b1))]
     [log (must-be (procedure/arity? #b10))])
    (optional
     [tolerate-fault?
      (default succumb)
      (must-be (procedure/arity? #b10))]))

  (define log-db:start&link
    (case-lambda
     [() (log-db:start&link (db:options))]
     [(options)
      (arg-check 'log-db:start&link [options (db:options is?)])
      (db:start&link 'log-db
        (make-directory-path (log-file))
        'create
        options)]))

  (define (->event-logger x)
    (match x
      [`(<log-db:event-logger>) x]
      [`(<event-logger> ,setup ,log)
       (log-db:event-logger [setup setup] [log log])]))

  (define (log-db:setup loggers)
    (define event-loggers (map ->event-logger loggers))
    (module (log-event endure-logger-fault?)
      (define (->vec f) (list->vector (map f event-loggers)))
      (define log* (->vec (log-db:event-logger log)))
      (define endure* (->vec (log-db:event-logger tolerate-fault?)))
      (define tolerate-fault? succumb)
      (define (endure-logger-fault? e) (tolerate-fault? e))
      (define (log-event e)
        (set! tolerate-fault? succumb)
        (vector-for-each
         (lambda (log endure?)
           (set! tolerate-fault? endure?)
           (log e))
         log* endure*)))
    (match (db:transaction 'log-db (lambda () (setup-db event-loggers)))
      [#(ok ,_)
       (match (event-mgr:set-log-handler
               log-event
               (whereis 'log-db)
               endure-logger-fault?)
         [ok
          (event-mgr:flush-buffer)
          (match (get-uname)
            [`(<uname> ,system ,release ,version ,machine)
             (system-detail <system-attributes>
               [date (current-date)]
               [software-info (software-info)]
               [machine-type (symbol->string (machine-type))]
               [computer-name (osi_get_hostname)]
               [os-pid (osi_get_pid)]
               [os-system system]
               [os-release release]
               [os-version version]
               [os-machine machine]
               [os-total-memory (osi_get_total_memory)])])
          (db:expire-cache 'log-db)
          'ignore]
         [,error error])]
      [,error error]))

  (define log-db:version
    (case-lambda
     [(name)
      (match (execute "select version from version where name=? limit 1"
               (symbol->string name))
        [(#(,version)) version]
        [() #f])]
     [(name version)
      (execute "insert or replace into version (name, version) values (?, ?)"
        (symbol->string name) version)]))

  (define log-db:instance-id #f)

  (define (log-db:get-instance-id)
    log-db:instance-id)

  (define (create-instance-id!)
    (let ([id (uuid->string (osi_make_uuid))])
      (log-db:version 'instance id)
      (set! log-db:instance-id id)))

  (define (setup-db event-loggers)
    (create-table version
      (name text primary key)
      (version text))
    (match (log-db:version 'instance)
      [#f (create-instance-id!)]
      [,id (set! log-db:instance-id id)])
    (for-each (lambda (l) ((log-db:event-logger setup l))) event-loggers)
    'ok)

  (meta define (make-sql-name x)
    (let ([ip (open-input-string (symbol->string (syntax->datum x)))]
          [op (open-output-string)])
      (write-char #\[ op)
      (let lp ()
        (let ([c (read-char ip)])
          (cond
           [(eof-object? c)]
           [(or (char<=? #\A c #\Z)
                (char<=? #\a c #\z)
                (char<=? #\0 c #\9)
                (char=? c #\_))
            (write-char c op)
            (lp)]
           [(char=? c #\-)
            (write-char #\_ op)
            (lp)]
           [else (lp)])))
      (write-char #\] op)
      (get-output-string op)))

  (define-syntax define-simple-events
    (syntax-rules ()
      [(_ create handle (name clause ...) ...)
       (andmap identifier? #'(name ...))
       (begin
         (define (create)
           (create-table name clause ...)
           ...)
         (define (handle r)
           (cond
            [(name is? r) (insert-table r name clause ...)]
            ...
            [else #f])))]))

  (define-syntax (create-table x)
    (define (create-table-clause clause)
      (syntax-case clause ()
        [(field type . inline)
         (identifier? #'field)
         (join
          (cons*
           (make-sql-name #'field)
           (datum type)
           (datum inline))
          #\space)]))
    (syntax-case x ()
      [(k name clause ...)
       #`(execute
          #,(datum->syntax #'k
              (format "create table if not exists ~a(~a)"
                (make-sql-name #'name)
                (join (map create-table-clause #'(clause ...)) ", "))))]))

  (define-syntax insert-table
    (syntax-rules ()
      [(_ r name [field type . inline] ...)
       (log-sql (insert name ([field ,(name no-check field r)] ...)))]))

  (define (coerce x)
    (cond
     [(string? x) x]
     [(fixnum? x) x]
     [(and (bignum? x) (< (integer-length x) 64)) x]
     [(flonum? x) x]
     [(eq? x #f) x]
     [(real? x) (inexact x)]
     [(bytevector? x) x]
     [(symbol? x) (symbol->string x)]
     [(process? x) (global-process-id x)]
     [(date? x) (format-rfc2822 x)]
     [(condition? x)
      (parameterize ([print-graph #t] [print-level 3] [print-length 6])
        (let ([op (open-output-string)])
          (json:write-object op #f json:write
            [message (exit-reason->english x)]
            [stacks (map stack->json (exit-reason->stacks x))])
          (get-output-string op)))]
     [(json:object? x) (json:object->string x)]
     [else (parameterize ([print-graph #t]) (format "~s" x))]))

  (define stack->json
    (case-lambda
     [(k) (stack->json k 'default)]
     [(k max-depth)
      (define who 'stack->json)
      (define (set-source! obj field x)
        (when (source-object? x)
          (let ([sfd (source-object-sfd x)])
            (json:set! obj field
              (json:make-object
               [bfp (source-object-bfp x)]
               [efp (source-object-efp x)]
               [path (source-file-descriptor-path sfd)]
               [checksum (source-file-descriptor-checksum sfd)])))))
      (define (var->json var)
        (json:make-object
         [name (format "~s" (car var))]
         [value (format "~s" (cdr var))]))
      (define obj (inspect/object k))
      (unless (eq? 'continuation (obj 'type))
        (bad-arg who k))
      (parameterize ([print-graph #t])
        (let ([stack (json:make-object [type "stack"] [depth (obj 'depth)])])
          (json:set! stack 'frames
            (walk-stack k '()
              (lambda (description source proc-source free)
                (let ([frame
                       (json:make-object
                        [type "stack-frame"]
                        [description description])])
                  (set-source! frame 'source source)
                  (set-source! frame 'procedure-source proc-source)
                  (when free (json:set! frame 'free (map var->json free)))
                  frame))
              (lambda (frame base depth next)
                (json:set! frame 'depth depth)
                (cons frame (next base)))
              who
              max-depth
              (lambda (base depth)
                (json:set! stack 'truncated depth)
                base)))
          stack))]))

  (define json-stack->string
    (let ()
      (define who 'json-stack->string)
      (define ($json-stack->string op x)
        (define (dump-src prefix)
          (lambda (src)
            (fprintf op "~@[ ~a~] at offset ~a of ~a" prefix
              (json:ref src 'bfp "?")
              (json:ref src 'path "?"))))
        (define (dump-frame x)
          (fprintf op "~a" (json:ref x 'description "?"))
          (cond
           [(json:ref x 'source #f) => (dump-src #f)]
           [(json:ref x 'procedure-source #f) => (dump-src "in procedure")])
          (newline op)
          (for-each
           (lambda (free)
             (fprintf op "  ~a: ~a\n"
               (json:ref free 'name "?")
               (json:ref free 'value "?")))
           (json:ref x 'free '())))
        (unless (and (json:object? x) (equal? "stack" (json:ref x 'type #f)))
          (bad-arg who x))
        (for-each dump-frame (json:ref x 'frames '()))
        (cond
         [(json:ref x 'truncated #f) =>
          (lambda (max-depth)
            (fprintf op "Stack dump truncated due to max-depth = ~a.\n"
              max-depth))]))
      (case-lambda
       [(op x)
        (arg-check who [op output-port? textual-port?])
        ($json-stack->string op x)]
       [(x)
        (let-values ([(op get) (open-string-output-port)])
          ($json-stack->string op x)
          (get))])))

  (define-syntax (log-sql x)
    (syntax-case x ()
      [(k sql)
       (let-values ([(query args) (parse-sql #'sql make-sql-name)])
         (with-syntax ([query (datum->syntax-object #'k query)]
                       [(arg ...) args])
           #'(db:log 'log-db query (coerce arg) ...)))]))

  (define (change-column-type table old-type new-type columns convert-column)
    (define (get-column-specs table)
      (map (lambda (x)
             (match x
               [#(,cid ,name ,type ,notnull ,dflt_value ,pk ,hidden)
                ;; we don't handle complicated cases
                (match-let* ([#f dflt_value] [0 pk] [0 notnull] [0 hidden])
                  (list name type))]))
        (execute (format "pragma table_xinfo([~a])" table))))
    (define (create-table name col-specs)
      (format "create table [~a] (~:{[~a] ~a~:^, ~})" name col-specs))
    (define (change-type col-specs)
      (map (lambda (x)
             (match x
               [(,column ,type)
                (guard (member column columns))
                (assert (equal? type old-type))
                (list column new-type)]
               [,x x]))
        col-specs))
    (define (convert col-specs)
      (map (lambda (x)
             (match x
               [(,column ,_)
                (guard (member column columns))
                (convert-column column)]
               [(,col ,type) (format "[~a]" col)]))
        col-specs))
    (let ([col-specs (get-column-specs table)])
      (execute (format "alter table [~a] rename to [~a_orig]" table table))
      (execute (create-table table (change-type col-specs)))
      (execute (format "insert into [~a] select ~{~a~^, ~} from [~a_orig] order by rowid"
                 table (convert col-specs) table))
      (execute (format "drop table [~a_orig]" table))))

  (define ($migrate-pid-columns table . columns)
    (change-column-type table "INTEGER" "TEXT" columns
      (lambda (column)
        (format "case when [~a] is null then null else ':' || [~a] end"
          column column))))

  (define (check-prune-args who prune-max-days prune-limit)
    (arg-check who
      [prune-max-days fixnum? fxnonnegative?]
      [prune-limit fixnum? fxpositive?]))

  (define (create-prune-on-insert-trigger table column prune-max-days prune-limit)
    (check-prune-args 'create-prune-on-insert-trigger prune-max-days prune-limit)
    (execute
     (format
      (ct:join #\newline
        "create temporary trigger prune_~a after insert on ~:*~a"
        "begin"
        "  delete from ~:*~a"
        "  where rowid in"
        "   (select rowid from ~:*~a where ~a < new.~:*~a - ~d limit ~d);"
        "end")
      table column (* prune-max-days 24 60 60 1000) prune-limit)))

  (module (make-swish-event-logger swish-event-logger)
    (define schema-name 'swish)
    (define schema-version "2021-10-01")

    (define-simple-events create-simple-tables log-simple-event
      (<child-end>
       (timestamp integer)
       (pid text)
       (killed integer)
       (reason text)
       (details text))
      (<child-start>
       (timestamp integer)
       (supervisor text)
       (pid text)
       (name text)
       (restart-type text)
       (type text)
       (shutdown integer))
      (<gen-server-debug>
       (timestamp integer)
       (duration integer)
       (type integer)
       (client text)
       (server text)
       (message text)
       (state text)
       (reply text))
      (<gen-server-terminating>
       (timestamp integer)
       (name text)
       (pid text)
       (last-message text)
       (state text)
       (reason text)
       (details text))
      (<http-request>
       (timestamp integer)
       (pid text)
       (host text)
       (method text)
       (path text)
       (header text)
       (params text))
      (<statistics>
       (timestamp integer)
       (date text)
       (reason text)
       (bytes-allocated integer)
       (current-memory-bytes integer)
       (maximum-memory-bytes integer)
       (osi-bytes-used integer)
       (sqlite-memory integer)
       (sqlite-memory-highwater integer)
       (foreign-handles text)
       (cpu real)
       (real real)
       (bytes integer)
       (gc-count integer)
       (gc-cpu real)
       (gc-real real)
       (gc-bytes integer)
       (os-free-memory integer))
      (<supervisor-error>
       (timestamp integer)
       (supervisor text)
       (error-context text)
       (reason text)
       (details text)
       (child-pid text)
       (child-name text))
      (<system-attributes>
       (timestamp integer)
       (date text)
       (software-info text)
       (machine-type text)
       (computer-name text)
       (os-pid integer)
       (os-system text)
       (os-release text)
       (os-version text)
       (os-machine text)
       (os-total-memory integer))
      (<transaction-retry>
       (timestamp integer)
       (database text)
       (duration integer)
       (count integer)
       (sql text))
      )

    (define (init-db prune-max-days prune-limit)

      (define-syntax create-prune-on-insert-triggers
        (syntax-rules ()
          [(_ (table column) ...)
           (begin
             (create-prune-on-insert-trigger 'table 'column
               prune-max-days prune-limit)
             ...)]))

      (define (create-index name sql)
        (execute (format "create index if not exists ~a on ~a" name sql)))

      (create-simple-tables)

      ;; session-id
      (match-let* ([(#(,id)) (execute "select max(rowid) from system_attributes")])
        ($import-internal set-session-id!)
        (set-session-id! (+ (or id 0) 1)))

      (execute "drop view if exists child")
      (execute "create view child as select T1.rowid as rowid, T1.pid as id, T1.name, T1.supervisor, T1.restart_type, T1.type, T1.shutdown, T1.timestamp as start, T2.timestamp - T1.timestamp as duration, T2.killed, T2.reason, T2.details from child_start T1 left outer join child_end T2 on T1.pid=T2.pid")

      (create-prune-on-insert-triggers
       (child_end timestamp)
       (child_start timestamp)
       (gen_server_debug timestamp)
       (gen_server_terminating timestamp)
       (http_request timestamp)
       (statistics timestamp)
       (supervisor_error timestamp)
       (system_attributes timestamp)
       (transaction_retry timestamp)
       )

      ;; indexes
      (create-index 'child_start_pid
        "child_start(pid)")
      (create-index 'child_start_timestamp
        "child_start(timestamp)")
      (create-index 'child_end_pid
        "child_end(pid)")
      (create-index 'child_end_timestamp
        "child_end(timestamp)")
      (create-index 'gen_server_debug_timestamp
        "gen_server_debug(timestamp)")
      (create-index 'gen_server_terminating_timestamp
        "gen_server_terminating(timestamp)")
      (create-index 'http_request_timestamp
        "http_request(timestamp)")
      (create-index 'statistics_timestamp
        "statistics(timestamp)")
      (create-index 'supervisor_error_timestamp
        "supervisor_error(timestamp)")
      (create-index 'system_attributes_timestamp
        "system_attributes(timestamp)")
      (create-index 'transaction_retry_timestamp
        "transaction_retry(timestamp)")
      'ok)

    (define (upgrade-db)
      (match (log-db:version schema-name)
        [,@schema-version 'ok]
        ["2021-09-18"
         (execute "alter table statistics add column os_free_memory integer default null")
         (execute "alter table system_attributes add column os_total_memory integer default null")
         (log-db:version schema-name "2021-10-01")
         (upgrade-db)]
        ["2020-10-01"
         (execute "alter table statistics add column current_memory_bytes integer default null")
         (execute "alter table statistics add column maximum_memory_bytes integer default null")
         (execute "alter table system_attributes add column os_pid integer default null")
         (log-db:version schema-name "2021-09-18")
         (upgrade-db)]
        ["2020-09-01"
         (execute "alter table system_attributes add column os_system text default null")
         (execute "alter table system_attributes add column os_release text default null")
         (execute "alter table system_attributes add column os_version text default null")
         (execute "alter table system_attributes add column os_machine text default null")
         (log-db:version schema-name "2020-10-01")
         (upgrade-db)]
        ["2019-10-18"
         (execute "drop view if exists child")
         ($migrate-pid-columns "child_start" "pid" "supervisor")
         ($migrate-pid-columns "child_end" "pid")
         ($migrate-pid-columns "http_request" "pid")
         ($migrate-pid-columns "supervisor_error" "child_pid" "supervisor")
         ($migrate-pid-columns "gen_server_debug" "client" "server")
         (execute "alter table gen_server_terminating add column pid text default null")
         (log-db:version schema-name "2020-09-01")
         (upgrade-db)]
        ["2019-06-26"
         (define chunk-size 1000)
         (define op (open-output-string))
         (define (convert reason)
           (match (catch (read (open-input-string reason)))
             [#(error ,r)
              (json:write-object op #f json:write [message r])
              (values "exception" (get-output-string op))]
             [#(error ,r ,s)
              (json:write-object op #f json:write [message r] [preformatted-stack s])
              (values "exception" (get-output-string op))]
             [#(EXIT ,_)
              (json:write-object op #f json:write [message reason])
              (values "exception" (get-output-string op))]
             [,r
              (let ([message (exit-reason->english r)])
                (if (string=? message reason)
                    (values reason #f)
                    (begin
                      (json:write-object op #f json:write [message message])
                      (values reason (get-output-string op)))))]))
         (define (update-reason&details table)
           (define (fetch offset)
             (execute (format "select rowid, reason from ~a order by rowid limit ? offset ?" table)
               chunk-size offset))
           (let lp ([offset 0])
             (let ([rows (fetch offset)])
               (unless (null? rows)
                 (let cvt ([offset offset] [rows rows])
                   (match rows
                     [() (lp offset)]
                     [(#(,rowid ,reason) . ,rows)
                      (let-values ([(reason details) (convert reason)])
                        (execute (format "update ~a set reason = ?, details = ? where rowid = ?" table)
                          reason details rowid))
                      (cvt (+ offset 1) rows)]))))))
         (execute "alter table child_end add column details text")
         (execute "alter table gen_server_terminating add column details text")
         (execute "alter table supervisor_error add column details text")
         (update-reason&details "child_end")
         (update-reason&details "gen_server_terminating")
         (update-reason&details "supervisor_error")
         (log-db:version schema-name "2019-10-18")
         (upgrade-db)]
        ["2019-05-24"
         (execute "alter table statistics rename to statistics_orig")
         (execute "create table statistics(timestamp integer, date text, reason text, bytes_allocated integer, osi_bytes_used integer, sqlite_memory integer, sqlite_memory_highwater integer, foreign_handles text, cpu real, real real, bytes integer, gc_count integer, gc_cpu real, gc_real real, gc_bytes integer)")
         (execute "insert into statistics select timestamp, date, reason, bytes_allocated, osi_bytes_used, sqlite_memory, sqlite_memory_highwater,json_object('databases', databases, 'statements', statements, 'tcp-listeners', listeners, 'osi-ports', ports, 'path-watchers', watchers),cpu, real, bytes, gc_count, gc_cpu, gc_real, gc_bytes from statistics_orig order by rowid")
         (execute "drop table statistics_orig")
         (log-db:version schema-name "2019-06-26")
         (upgrade-db)]
        ["2018-09-25"
         (execute "alter table system_attributes rename to system_attributes_orig")
         (execute "create table system_attributes (timestamp integer, date text, software_info text, machine_type text, computer_name text)")
         (execute "insert into system_attributes select timestamp, date, json_object('swish',json_object('product-name','Swish','version',software_version)), ?, computer_name from system_attributes_orig order by rowid"
           (symbol->string (machine-type)))
         (execute "drop table system_attributes_orig")
         (log-db:version schema-name "2019-05-24")
         (upgrade-db)]
        ["2018-03-02"
         (execute "alter table statistics rename to statistics_orig")
         (execute "create table statistics(timestamp integer, date text, reason text, bytes_allocated integer, osi_bytes_used integer, sqlite_memory integer, sqlite_memory_highwater integer, databases integer, statements integer, listeners integer, ports integer, watchers integer, cpu real, real real, bytes integer, gc_count integer, gc_cpu real, gc_real real, gc_bytes integer)")
         (execute "insert into statistics select timestamp, date, reason, bytes_allocated, osi_bytes_used, sqlite_memory, sqlite_memory_highwater, databases, 0, listeners, ports, watchers, cpu, real, bytes, gc_count, gc_cpu, gc_real, gc_bytes from statistics_orig order by rowid")
         (execute "drop table statistics_orig")
         (log-db:version schema-name "2018-09-25")
         (upgrade-db)]
        ["l2icz69tb6toyr48uf90nlbm3"
         (execute "alter table statistics rename to statistics_orig")
         (execute "create table statistics(timestamp integer, date text, reason text, bytes_allocated integer, osi_bytes_used integer, sqlite_memory integer, sqlite_memory_highwater integer, databases integer, listeners integer, ports integer, watchers integer, cpu real, real real, bytes integer, gc_count integer, gc_cpu real, gc_real real, gc_bytes integer)")
         (execute "insert into statistics select timestamp, date, reason, bytes_allocated, osi_bytes_used, sqlite_memory, sqlite_memory_highwater, databases, listeners, ports, 0, cpu, real, bytes, gc_count, gc_cpu, gc_real, gc_bytes from statistics_orig order by rowid")
         (execute "drop table statistics_orig")
         (log-db:version schema-name "2018-03-02")
         (upgrade-db)]
        [#f
         (log-db:version schema-name schema-version)
         (upgrade-db)]
        [,version (throw `#(unsupported-db-version ,schema-name ,version))]))

    (define make-swish-event-logger
      (case-lambda
       [() (make-swish-event-logger 90 10)]
       [(prune-max-days prune-limit)
        (check-prune-args 'make-swish-event-logger prune-max-days prune-limit)
        (<event-logger> make
          [setup
           (lambda ()
             (upgrade-db)
             (init-db prune-max-days prune-limit))]
          [log log-simple-event])]))

    (define swish-event-logger (make-swish-event-logger)))

  )
