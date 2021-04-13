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
(library (swish db)
  (export
   SQLITE_OPEN_CREATE
   SQLITE_OPEN_READONLY
   SQLITE_OPEN_READWRITE
   SQLITE_STATUS_MEMORY_USED
   bindings-count
   bindings?
   columns
   database-count
   database-create-time
   database-filename
   database?
   db:expire-cache
   db:filename
   db:log
   db:options
   db:start&link
   db:stop
   db:transaction
   execute
   execute-sql
   lazy-execute
   parse-sql
   print-bindings
   print-databases
   print-statements
   sqlite:bind
   sqlite:bulk-execute
   sqlite:clear-bindings
   sqlite:close
   sqlite:columns
   sqlite:execute
   sqlite:expanded-sql
   sqlite:finalize
   sqlite:get-bindings
   sqlite:interrupt
   sqlite:last-insert-rowid
   sqlite:marshal-bindings
   sqlite:open
   sqlite:prepare
   sqlite:sql
   sqlite:step
   sqlite:unmarshal-bindings
   statement-count
   statement-create-time
   statement-database
   statement-sql
   statement?
   transaction
   with-db
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish event-mgr)
   (swish events)
   (swish gen-server)
   (swish io)
   (swish options)
   (swish osi)
   (swish queue)
   (swish string-utils)
   )

  (define (special-filename? filename)
    (or (= (string-length filename) 0)
        (char=? (string-ref filename 0) #\:)
        (starts-with? filename "file:")))

  (define (init-wal db)
    (match (execute-sql db "pragma journal_mode=wal")
      [(#("wal")) 'ok]
      [(#(,mode)) (throw `#(bad-journal-mode ,mode))]))

  (define (default-db-init filename mode db)
    (when (and (eq? mode 'create)
               (not (special-filename? filename)))
      (init-wal db)))

  (define-options db:options
    (optional
     [init
      (default default-db-init)
      (must-be (procedure/arity? #b1000))]
     [cache-timeout
      (default (* 5 60 1000))
      (must-be fixnum? fxnonnegative?)]
     [commit-delay
      (default 0)
      (must-be fixnum? fxnonnegative?)]
     [commit-limit
      (default 10000)
      (must-be fixnum? fxpositive?)]))

  (define db:start&link
    (case-lambda
     [(name filename mode)
      (db:start&link name filename mode (db:options))]
     [(name filename mode arg)
      (gen-server:start&link name filename mode
        (match arg
          [`(<db:options>) arg]
          [,db-init
           (guard (procedure? db-init))
           (db:options
            [init
             (lambda (filename mode db)
               (db-init db))])]
          [,_ (bad-arg 'db:start&link arg)]))]))

  (define (db:stop who)
    (gen-server:call who 'stop 'infinity))

  (define (db:filename who)
    (gen-server:call who 'filename))

  (define (db:log who sql . bindings)
    (gen-server:cast who
      (<log> make
        [sql sql]
        [mbindings (sqlite:marshal-bindings-no-check bindings)])))

  (define (db:transaction who f)
    (gen-server:call who `#(transaction ,f) 'infinity))

  (define (lazy-execute sql . bindings)
    (unless (statement-cache)
      (throw `#(invalid-context lazy-execute)))
    ($lazy-execute sql bindings))

  (define (execute sql . bindings)
    (unless (statement-cache)
      (throw `#(invalid-context execute)))
    ($execute sql bindings))

  (define (columns sql)
    (unless (statement-cache)
      (throw `#(invalid-context columns)))
    (sqlite:columns (get-statement sql)))

  (define-syntax transaction
    (syntax-rules ()
      [(_ db body1 body2 ...) ($transaction db (lambda () body1 body2 ...))]))

  (define ($transaction db thunk)
    (match (db:transaction db thunk)
      [#(ok ,result) result]
      [#(error ,reason) (throw reason)]))

  (define (db:expire-cache who)
    (transaction who
      (remove-dead-entries (statement-cache)
        (lambda (key)
          (not (member key '("BEGIN IMMEDIATE" "COMMIT")))))))

  (define-state-tuple <db-state>
    filename db cache queue worker commit-delay commit-limit log-timeout)

  (define-tuple <log> sql mbindings)

  (define current-database (make-process-parameter #f))
  (define statement-cache (make-process-parameter #f))

  (define SQLITE_OPEN_READONLY 1)
  (define SQLITE_OPEN_READWRITE 2)
  (define SQLITE_OPEN_CREATE 4)

  (define (init filename mode options)
    (match-define `(<db:options> ,init ,cache-timeout ,commit-delay ,commit-limit) options)
    (process-trap-exit #t)
    (let ([db (sqlite:open filename
                (match mode
                  [open SQLITE_OPEN_READWRITE]
                  [create
                   (logor SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)]))])
      (match (try (init filename mode db))
        [`(catch ,reason ,e)
         (sqlite:close db)
         (raise e)]
        [,_ (void)])
      `#(ok ,(<db-state> make
               [filename filename]
               [db db]
               [cache (make-cache cache-timeout)]
               [queue queue:empty]
               [worker #f]
               [commit-delay commit-delay]
               [commit-limit commit-limit]
               [log-timeout #f]))))

  (define (terminate reason state)
    (let* ([x (try (flush (update state)))]
           [y (try
               (vector-for-each (lambda (e) (sqlite:finalize (entry-stmt e)))
                 (hashtable-values (cache-ht ($state cache))))
               (finalize-lazy-objects ($state cache))
               (sqlite:close ($state db)))])
      (match x
        [`(catch ,_ ,e) (raise e)]
        [,_ (match y
              [`(catch ,_ ,e) (raise e)]
              [,_ 'ok])])))

  (define (handle-call msg from state)
    (match msg
      [#(transaction ,f)
       (no-reply
        ($state copy* [queue (queue:add `#(transaction ,f ,from) queue)]))]
      [filename `#(reply ,($state filename) ,state ,(get-timeout state))]
      [stop `#(stop normal stopped ,state)]))

  (define (handle-cast msg state)
    (match msg
      [`(<log>)
       (no-reply/delay
        ($state copy*
          [queue (queue:add msg queue)]
          [commit-delay commit-delay]
          [log-timeout (or log-timeout (+ (erlang:now) commit-delay))]))]))

  (define (handle-info msg state)
    ($state open [cache queue worker])
    (match msg
      [timeout
       (when (queue:empty? queue)
         (remove-dead-entries cache (lambda (key) #f)))
       (no-reply state)]
      [`(DOWN ,_ ,@worker ,reason ,e)
       (let ([state ($state copy [worker #f])])
         (if (eq? reason 'normal)
             (no-reply state)
             `#(stop ,e ,state)))]
      [`(DOWN ,_ ,_ ,_ ,_) (no-reply state)]
      [`(EXIT ,pid ,_ ,e)
       ;; The DOWN message will process the worker.
       (if (eq? pid worker)
           (no-reply state)
           `#(stop ,e ,state))]))

  (define (no-reply state)
    (let ([state (update state)])
      `#(no-reply ,state ,(get-timeout state))))

  (define (no-reply/delay state)
    (if (fxzero? ($state commit-delay))
        (no-reply state)
        `#(no-reply ,state ,(get-timeout state))))

  (define (get-timeout state)
    ($state open [cache log-timeout worker])
    (cond
     [worker 'infinity]
     [log-timeout]
     [(cache-waketime cache)]
     [else 'infinity]))

  (define (update state)
    (match-let* ([`(<db-state> ,queue ,worker) state])
      (if (or worker (queue:empty? queue))
          state
          (let-values ([(queue work) (get-work queue state)])
            (let ([worker (spawn work)])
              (monitor worker)
              ($state copy
                [queue queue]
                [log-timeout (if (queue:empty? queue) #f ($state log-timeout))]
                [worker worker]))))))

  (define-syntax with-values
    (syntax-rules ()
      [(_ expr consumer)
       (call-with-values (lambda () expr) consumer)]))

  (define (get-work queue state)
    (let ([head (queue:get queue)])
      (match head
        [`(<log>)
         (with-values (get-logs queue 0 ($state commit-limit))
           (lambda (logs queue count)
             (values queue
               (make-log-worker (cons head logs) count state))))]
        [#(transaction ,f ,from)
         (values (queue:drop queue)
           (make-transaction-worker f from state))])))

  (define (get-logs queue count commit-limit)
    (let ([queue (queue:drop queue)]
          [count (+ count 1)])
      (if (or (queue:empty? queue) (>= count commit-limit))
          (values '() queue count)
          (let ([head (queue:get queue)])
            (if (<log> is? head)
                (with-values (get-logs queue count commit-limit)
                  (lambda (logs queue count)
                    (values (cons head logs) queue count)))
                (values '() queue count))))))

  (define (setup-worker db cache)
    (current-database db)
    (statement-cache cache))

  (define (make-transaction-worker f from state)
    (match-let* ([`(<db-state> ,db ,cache) state])
      (lambda ()
        (setup-worker db cache)
        (execute-with-retry-on-busy "BEGIN IMMEDIATE")
        (match (try (limit-stack (f)))
          [`(catch ,reason ,e)
           (finalize-lazy-objects cache)
           (execute-with-retry-on-busy "ROLLBACK")
           (gen-server:reply from `#(error ,e))]
          [,result
           (finalize-lazy-objects cache)
           (execute-with-retry-on-busy "COMMIT")
           (gen-server:reply from `#(ok ,result))]))))

  (define (make-log-worker logs count state)
    (match-let* ([`(<db-state> ,db ,cache) state])
      (lambda ()
        (setup-worker db cache)
        (let ([vstmt (make-vector count)]
              [vbind (make-vector count)])
          (do ([ls logs (cdr ls)]
               [i 0 (+ i 1)])
              ((null? ls))
            (match-let* ([`(<log> ,sql ,mbindings) (car ls)])
              (vector-set! vstmt i (get-statement sql))
              (vector-set! vbind i mbindings)))
          (execute-with-retry-on-busy "BEGIN IMMEDIATE")
          (sqlite:bulk-execute vstmt vbind)
          (execute-with-retry-on-busy "COMMIT")
          (vector-for-each sqlite:unmarshal-bindings vbind)))))

  (define (shutdown pid)
    ;; The worker may contain misbehaving code. Use the
    ;; shutdown protocol similar to supervisors.
    (kill pid 'shutdown)
    (receive
     (after 1000
       (kill pid 'kill)
       (receive
        [`(DOWN ,_ ,@pid ,_ ,e)
         (raise e)]))
     [`(DOWN ,_ ,@pid ,reason ,e)
      (unless (memq reason '(normal shutdown))
        (raise e))]))

  (define (stop-worker pid db)
    ;; The C thread may be working on a slow or never-ending
    ;; query. Interrupt and allow a brief time for context
    ;; switches.
    (define busy? (sqlite:interrupt db))
    (receive (after (if busy? 100 0) (shutdown pid))
      [`(DOWN ,_ ,@pid ,reason ,e)
       (unless (eq? reason 'normal)
         (raise e))]))

  (define (flush state)
    (let ([pid ($state worker)])
      (when pid
        ;; Allow the worker enough time to complete default
        ;; commit-limit inserts.
        (receive (after 1000 (stop-worker pid ($state db)))
          [`(DOWN ,_ ,@pid ,reason ,e)
           (if (eq? reason 'normal)
               (flush (update ($state copy [worker #f])))
               (raise e))]))))

  (define ($execute sql bindings)
    (sqlite:execute (get-statement sql) bindings))

  (define (execute-with-retry-on-busy sql)
    ;; Use with BEGIN IMMEDIATE, COMMIT, and ROLLBACK
    (define sleep-times '(2 3 6 11 16 21 26 26 26 51 51 . #0=(101 . #0#)))
    (define (bits-set? rc bits) (equal? bits (bitwise-and rc bits)))
    (define (attempt stmt count sleep-times)
      (unless (< count 500)
        (throw `#(db-retry-failed ,sql ,count)))
      (match (try (sqlite:execute stmt '()))
        [`(catch #(db-error ,_ (,_ ,sqlite_rc . ,_) ,_))
         (guard
          (let ([rc (- (- sqlite_rc) 6000000)])
            (or (bits-set? rc 5)    ;; SQLITE_BUSY
                (bits-set? rc 6)))) ;; SQLITE_LOCKED
         (match sleep-times
           [(,t . ,rest)
            (receive (after t (attempt stmt (+ count 1) rest)))])]
        [`(catch ,reason ,e) (raise e)]
        [,_ count]))
    (let* ([stmt (get-statement sql)]
           [start-time (erlang:now)]
           [count (attempt stmt 0 sleep-times)])
      (when (> count 0)
        (let ([end-time (erlang:now)])
          (event-mgr:notify
           (<transaction-retry> make
             [timestamp start-time]
             [database (database-filename (current-database))]
             [duration (- end-time start-time)]
             [count count]
             [sql sql]))))))

  ;; Cache

  (define-record-type cache
    (nongenerative)
    (sealed #t)
    (fields
     (immutable ht)
     (immutable expire-timeout)
     (mutable waketime)
     (mutable lazy-objects))
    (protocol
     (lambda (new)
       (lambda (expire-timeout)
         (new (make-hashtable string-hash string=?) expire-timeout #f '())))))

  (define-record-type entry
    (nongenerative)
    (sealed #t)
    (fields
     (immutable stmt)
     (mutable timestamp))
    (protocol
     (lambda (new)
       (lambda (stmt)
         (new stmt (erlang:now))))))

  (define (get-statement sql)
    (match (statement-cache)
      [,(cache <= `(cache ,expire-timeout ,ht ,waketime))
       (cond
        [(hashtable-ref ht sql #f) =>
         (lambda (entry)
           (entry-timestamp-set! entry (erlang:now))
           (entry-stmt entry))]
        [else
         (let ([stmt (sqlite:prepare (current-database) sql)])
           (hashtable-set! ht sql (make-entry stmt))
           (unless waketime
             (cache-waketime-set! cache (+ (erlang:now) expire-timeout)))
           stmt)])]))

  (define (finalize-lazy-objects cache)
    (for-each
     (lambda (x)
       (match x
         [`(statement) (sqlite:finalize x)]
         [`(bindings) (sqlite:unmarshal-bindings x)]))
     (cache-lazy-objects cache))
    (cache-lazy-objects-set! cache '()))

  (define (remove-dead-entries the-cache expire-now?)
    (match-let*
     ([`(cache ,expire-timeout ,ht) the-cache]
      [,dead (- (erlang:now) expire-timeout)]
      [,oldest #f])
     (let-values ([(keys vals) (hashtable-entries ht)])
       (vector-for-each
        (lambda (key val)
          (let ([timestamp (entry-timestamp val)])
            (cond
             [(or (<= timestamp dead) (expire-now? key))
              (hashtable-delete! ht key)
              (sqlite:finalize (entry-stmt val))]
             [(or (not oldest) (< timestamp oldest))
              (set! oldest timestamp)])))
        keys vals))
     (cache-waketime-set! the-cache (and oldest (+ oldest expire-timeout)))))

  ;; Low-level SQLite interface

  (define-record-type database
    (nongenerative)
    (sealed #t)
    (fields
     (immutable filename)
     (immutable create-time)
     (mutable handle)))

  (define databases
    (make-foreign-handle-guardian 'databases
      database-handle
      database-handle-set!
      database-create-time
      (lambda (db) (sqlite:close db))
      (lambda (op db handle)
        (fprintf op "  ~d: ~a opened ~d\n"
          handle
          (database-filename db)
          (database-create-time db)))))

  (define database-count (foreign-handle-count 'databases))
  (define print-databases (foreign-handle-print 'databases))

  (define (@make-database filename create-time handle)
    (let ([db (make-database filename create-time handle)])
      (databases db handle)))

  (define-record-type statement
    (nongenerative)
    (sealed #t)
    (fields
     (immutable database)
     (immutable sql)
     (immutable create-time)
     (mutable handle)))

  (define statements
    (make-foreign-handle-guardian 'statements
      statement-handle
      statement-handle-set!
      statement-create-time
      (lambda (s) (sqlite:finalize s))
      (lambda (op s handle)
        (fprintf op "  ~d: ~d ~a prepared ~d\n"
          handle
          (database-handle (statement-database s))
          (statement-sql s)
          (statement-create-time s)))))

  (define statement-count (foreign-handle-count 'statements))
  (define print-statements (foreign-handle-print 'statements))

  (define (@make-statement database sql create-time handle)
    (let ([s (make-statement database sql create-time handle)])
      (statements s handle)))

  (define-record-type bindings
    (nongenerative)
    (sealed #t)
    (fields
     (mutable handle)))

  (define bindings-guardian
    (make-foreign-handle-guardian 'bindings
      bindings-handle
      bindings-handle-set!
      (lambda (b) 0)
      (lambda (b) (sqlite:unmarshal-bindings b))
      (lambda (op b handle)
        (fprintf op "  ~d: ~s\n" handle (sqlite:get-bindings b)))))

  (define bindings-count (foreign-handle-count 'bindings))
  (define print-bindings (foreign-handle-print 'bindings))

  (define (@make-bindings handle)
    (let ([b (make-bindings handle)])
      (bindings-guardian b handle)))

  (define (sqlite:marshal-bindings-no-check bindings)
    (if (or (null? bindings) (eq? bindings '#()))
        #f
        (with-interrupts-disabled
         (@make-bindings (osi_marshal_bindings bindings)))))

  (define (sqlite:marshal-bindings bindings)
    (arg-check 'sqlite:marshal-bindings
      [bindings (lambda (x) (or (list? x) (vector? x)))])
    (sqlite:marshal-bindings-no-check bindings))

  (define (sqlite:unmarshal-bindings mbindings)
    (when mbindings
      (with-interrupts-disabled
       (let ([handle (bindings-handle mbindings)])
         (when handle
           (match (osi_unmarshal_bindings* handle)
             [#t (bindings-guardian mbindings #f)]))))))

  (define (sqlite:get-bindings mbindings)
    (if (not mbindings)
        '#()
        (with-interrupts-disabled
         (let ([handle (bindings-handle mbindings)])
           (and handle (osi_get_bindings handle))))))

  (define (db-error who error detail)
    (throw `#(db-error ,who ,error ,detail)))

  (define-syntax with-db
    (syntax-rules ()
      [(_ [db filename flags] body1 body2 ...)
       (let ([db (sqlite:open filename flags)])
         (on-exit (sqlite:close db)
           body1 body2 ...))]))

  (define (sqlite:open filename flags)
    (define result)
    (with-interrupts-disabled
     (match (osi_open_database* filename flags
              (let ([p self])
                (lambda (r)
                  (if (pair? r)
                      (set! result r)
                      (set! result (@make-database filename (erlang:now) r)))
                  (complete-io p))))
       [#t
        (wait-for-io filename)
        (if (not (pair? result))
            result
            (db-error 'open result filename))]
       [,error
        (db-error 'open error filename)])))

  (define (sqlite:close db)
    (define result)
    (with-interrupts-disabled
     (let ([handle (database-handle db)])
       (when handle
         (match (osi_close_database* handle
                  (let ([p self])
                    (lambda (r)
                      (when (pair? r)
                        (databases db handle))
                      (set! result r)
                      (complete-io p))))
           [#t
            (databases db #f)
            (wait-for-io (database-filename db))
            (when (pair? result)
              (db-error 'close result db))]
           [,error
            (db-error 'close error db)])))))

  (define (sqlite:prepare db sql)
    (define result)
    (with-interrupts-disabled
     (match (osi_prepare_statement* (database-handle db) sql
              (let ([p self])
                (lambda (r)
                  (if (pair? r)
                      (set! result r)
                      (set! result (@make-statement db sql (erlang:now) r)))
                  (complete-io p))))
       [#t
        (wait-for-io (database-filename db))
        (if (pair? result)
            (db-error 'prepare result sql)
            result)]
       [,error (db-error 'prepare result sql)])))

  (define (sqlite:finalize stmt)
    (with-interrupts-disabled
     (let ([handle (statement-handle stmt)])
       (when handle
         (match (osi_finalize_statement* handle)
           [#t (statements stmt #f)]
           [,error (db-error 'finalize error stmt)])))))

  (define (sqlite:bind stmt bindings)
    (osi_reset_statement* (statement-handle stmt))
    (do ([i 1 (+ i 1)] [ls bindings (cdr ls)])
        ((null? ls))
      (osi_bind_statement (statement-handle stmt) i (car ls))))

  (define (sqlite:bind-mbindings stmt mbindings)
    (match stmt
      [`(statement [handle ,stmt-handle])
       (osi_reset_statement* stmt-handle)
       (when mbindings
         (osi_bind_statement_bindings stmt-handle (bindings-handle mbindings)))]))

  (define (sqlite:clear-bindings stmt)
    (osi_clear_statement_bindings (statement-handle stmt)))

  (define (sqlite:interrupt db)
    (osi_interrupt_database (database-handle db)))

  (define (sqlite:last-insert-rowid db)
    (osi_get_last_insert_rowid (database-handle db)))

  (define (sqlite:step stmt)
    (define result)
    (with-interrupts-disabled
     (osi_step_statement (statement-handle stmt)
       (let ([p self])
         (lambda (r)
           (#%$keep-live stmt)
           (set! result r)
           (complete-io p))))
     (wait-for-io (database-filename (statement-database stmt)))
     (when (pair? result)
       (db-error 'step result (statement-sql stmt)))
     result))

  (define (sqlite:execute stmt bindings)
    (define mbindings
      (if (bindings? bindings)
          bindings
          (sqlite:marshal-bindings bindings)))
    (define unmarshal? (not (eq? bindings mbindings)))
    (on-exit
     (begin
       (osi_reset_statement* (statement-handle stmt))
       (sqlite:clear-bindings stmt)
       (when unmarshal?
         (sqlite:unmarshal-bindings mbindings)))
     (sqlite:bind-mbindings stmt mbindings)
     (let lp ()
       (let ([row (sqlite:step stmt)])
         (if row
             (cons row (lp))
             '())))))

  (define NULL 0)
  (define (get-bindings-handle bindings)
    (if (not bindings)
        NULL
        (bindings-handle bindings)))

  (define (sqlite:bulk-execute stmts mbindings)
    (define result)
    (with-interrupts-disabled
     (let ([stmt-handles (vector-map statement-handle stmts)]
           [bind-handles (vector-map get-bindings-handle mbindings)])
       (osi_bulk_execute stmt-handles bind-handles
         (let ([p self])
           (lambda (r)
             (#%$keep-live stmts)
             (#%$keep-live mbindings)
             (set! result r)
             (complete-io p))))
       (let ([db-name (statement-database (vector-ref stmts 0))])
         (wait-for-io (database-filename db-name))
         (when (pair? result)
           (db-error 'bulk-execute result db-name))
         result))))

  (define (execute-sql db sql . bindings)
    (let ([stmt (sqlite:prepare db sql)])
      (on-exit (sqlite:finalize stmt)
        (sqlite:execute stmt bindings))))

  (define (sqlite:columns stmt)
    (osi_get_statement_columns (statement-handle stmt)))

  (define (sqlite:sql stmt)
    (statement-sql stmt))

  (define (sqlite:expanded-sql stmt)
    (osi_get_statement_expanded_sql (statement-handle stmt)))

  (define ($lazy-execute sql bindings)
    (let* ([cache (statement-cache)]
           [stmt (sqlite:prepare (current-database) sql)]
           [mbindings (sqlite:marshal-bindings bindings)])
      (cache-lazy-objects-set! cache
        (let ([objects (cache-lazy-objects cache)])
          (cons stmt (if mbindings (cons mbindings objects) objects))))
      (sqlite:bind-mbindings stmt mbindings)
      (lambda () (sqlite:step stmt))))

  (define parse-sql
    (case-lambda
     [(x) (parse-sql x (lambda (id) id))]
     [(x symbol->sql)
      (define (stringify x)
        (syntax-case x (unquote)
          [(unquote _) "?"]
          [_
           (let ([v (syntax-object->datum x)])
             (cond
              [(symbol? v) (symbol->sql v)]
              [(string? v) v]
              [else (syntax-error x "invalid SQL term")]))]))
      (define (collect-args x)
        (syntax-case x (unquote)
          [() '()]
          [((unquote e) . rest) (cons #'e (collect-args #'rest))]
          [(_ . rest) (collect-args #'rest)]))
      (syntax-case x ()
        [(insert table ([column e1 e2 ...] ...))
         (and (eq? (datum insert) 'insert)
              (identifier? #'table)
              (for-all identifier? #'(column ...)))
         (values
          (format "insert into ~a(~a) values(~a)"
            (stringify #'table)
            (join (map stringify #'(column ...)) ", ")
            (join (map (lambda (args) (join (map stringify args) #\space))
                    #'((e1 e2 ...) ...)) ", "))
          (fold-right
           (lambda (x ls) (append (collect-args x) ls))
           '()
           #'((e1 e2 ...) ...)))]
        [(update table ([column e1 e2 ...] ...) where ...)
         (and (eq? (datum update) 'update)
              (identifier? #'table)
              (for-all identifier? #'(column ...)))
         (values
          (join
           (cons
            (format "update ~a set ~a"
              (stringify #'table)
              (join
               (map (lambda (x)
                      (syntax-case x ()
                        [(column e1 e2 ...)
                         (format "~a=~a" (datum column)
                           (join (map stringify #'(e1 e2 ...)) #\space))]))
                 #'((column e1 e2 ...) ...))
               ", "))
            (map stringify #'(where ...)))
           #\space)
          (fold-right
           (lambda (x ls) (append (collect-args x) ls))
           (collect-args #'(where ...))
           #'((e1 e2 ...) ...)))]
        [(delete table where ...)
         (and (eq? (datum delete) 'delete)
              (identifier? #'table))
         (values
          (join
           (cons
            (format "delete from ~a" (stringify #'table))
            (map stringify #'(where ...)))
           #\space)
          (collect-args #'(where ...)))])]))

  (define SQLITE_STATUS_MEMORY_USED 0)

  (record-writer (record-type-descriptor database)
    (lambda (r p wr)
      (display-string "#<database " p)
      (wr (database-filename r) p)
      (write-char #\> p)))

  (record-writer (record-type-descriptor statement)
    (lambda (r p wr)
      (display-string "#<statement " p)
      (wr (statement-database r) p)
      (display-string " " p)
      (wr (statement-sql r) p)
      (write-char #\> p)))

  (record-writer (record-type-descriptor bindings)
    (lambda (r p wr)
      (cond
       [(sqlite:get-bindings r) =>
        (lambda (bindings)
          (display-string "#<bindings" p)
          (let ([vlen (vector-length bindings)]
                [limit (print-length)])
            (do ([i 0 (fx+ i 1)])
                ((cond
                  [(fx= i vlen)]
                  [(and limit (fx= i limit))
                   (display-string " ..." p)]
                  [else #f]))
              (display-string " " p)
              (wr (vector-ref bindings i) p)))
          (display-string ">" p))]
       [else (display-string "#<bindings>" p)])))

  )
