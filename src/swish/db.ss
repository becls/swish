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
   columns
   database-count
   database-create-time
   database-filename
   database?
   db:filename
   db:log
   db:start&link
   db:stop
   db:transaction
   execute
   execute-sql
   lazy-execute
   parse-sql
   print-databases
   print-statements
   sqlite:bind
   sqlite:clear-bindings
   sqlite:close
   sqlite:columns
   sqlite:execute
   sqlite:expanded-sql
   sqlite:finalize
   sqlite:interrupt
   sqlite:last-insert-rowid
   sqlite:open
   sqlite:prepare
   sqlite:sql
   sqlite:step
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
      [(#(,mode)) (raise `#(bad-journal-mode ,mode))]))

  (define db:start&link
    (case-lambda
     [(name filename mode)
      (db:start&link name filename mode
        (if (and (eq? mode 'create)
                 (not (special-filename? filename)))
            init-wal
            values))]
     [(name filename mode db-init)
      (gen-server:start&link name filename mode db-init)]))

  (define (db:stop who)
    (gen-server:call who 'stop 'infinity))

  (define (db:filename who)
    (gen-server:call who 'filename))

  (define (db:log who sql . bindings)
    (gen-server:cast who `#(log ,sql ,bindings)))

  (define (db:transaction who f)
    (gen-server:call who `#(transaction ,f) 'infinity))

  (define (lazy-execute sql . bindings)
    (unless (statement-cache)
      (raise `#(invalid-context lazy-execute)))
    ($lazy-execute sql bindings))

  (define (execute sql . bindings)
    (unless (statement-cache)
      (raise `#(invalid-context execute)))
    ($execute sql bindings))

  (define (columns sql)
    (unless (statement-cache)
      (raise `#(invalid-context columns)))
    (sqlite:columns (get-statement sql)))

  (define-syntax transaction
    (syntax-rules ()
      [(_ db body1 body2 ...) ($transaction db (lambda () body1 body2 ...))]))

  (define ($transaction db thunk)
    (match (db:transaction db thunk)
      [#(ok ,result) result]
      [#(error ,reason) (raise reason)]))

  (define commit-threshold 10000)

  (define-state-tuple <db-state> filename db cache queue worker)

  (define current-database (make-process-parameter #f))
  (define statement-cache (make-process-parameter #f))

  (define SQLITE_OPEN_READONLY 1)
  (define SQLITE_OPEN_READWRITE 2)
  (define SQLITE_OPEN_CREATE 4)

  (define (init filename mode db-init)
    (process-trap-exit #t)
    (let ([db (sqlite:open filename
                (match mode
                  [open SQLITE_OPEN_READWRITE]
                  [create
                   (logor SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)]))])
      (match (catch (db-init db))
        [#(EXIT ,reason)
         (sqlite:close db)
         (raise reason)]
        [,_ (void)])
      `#(ok ,(<db-state> make
               [filename filename]
               [db db]
               [cache (make-cache)]
               [queue queue:empty]
               [worker #f]))))

  (define (terminate reason state)
    (catch (flush state))
    (vector-for-each (lambda (e) (sqlite:finalize (entry-stmt e)))
      (hashtable-values (cache-ht ($state cache))))
    (finalize-lazy-statements ($state cache))
    (sqlite:close ($state db))
    'ok)

  (define (handle-call msg from state)
    (match msg
      [#(transaction ,f)
       (no-reply
        ($state copy* [queue (queue:add `#(transaction ,f ,from) queue)]))]
      [filename `#(reply ,($state filename) ,state ,(get-timeout state))]
      [stop `#(stop normal stopped ,(flush state))]))

  (define (handle-cast msg state)
    (match msg
      [#(log ,sql ,bindings)
       (no-reply ($state copy* [queue (queue:add msg queue)]))]))

  (define (handle-info msg state)
    (let ([pid ($state worker)])
      (match msg
        [timeout
         (remove-dead-entries ($state cache))
         (no-reply state)]
        [#(EXIT ,@pid normal) (no-reply ($state copy [worker #f]))]
        [#(EXIT ,_pid ,reason) `#(stop ,reason ,($state copy [worker #f]))])))

  (define (no-reply state)
    (let ([state (update state)])
      `#(no-reply ,state ,(get-timeout state))))

  (define (get-timeout state)
    (cond
     [($state worker) 'infinity]
     [(cache-waketime ($state cache)) =>
      (lambda (waketime) (max (- waketime (erlang:now)) 0))]
     [else 'infinity]))

  (define (update state)
    (match-let* ([`(<db-state> ,queue ,worker) state])
      (if (or worker (queue:empty? queue))
          state
          (let-values ([(work queue) (get-work queue state)])
            ($state copy [queue queue] [worker (spawn&link work)])))))

  (define (get-work queue state)
    (let ([head (queue:get queue)])
      (match head
        [#(log ,_ ,_)
         (let-values ([(logs queue) (get-related queue 0)])
           (values (make-worker (cons head logs) state) queue))]
        [#(transaction ,_ ,_)
         (values (make-worker head state) (queue:drop queue))])))

  (define (get-related queue count)
    (let ([queue (queue:drop queue)]
          [count (+ count 1)])
      (if (or (queue:empty? queue) (>= count commit-threshold))
          (values '() queue)
          (let ([head (queue:get queue)])
            (match head
              [#(log ,_ ,_)
               (let-values ([(logs queue) (get-related queue count)])
                 (values (cons head logs) queue))]
              [,_ (values '() queue)])))))

  (define (make-worker x state)
    (match-let* ([`(<db-state> ,db ,cache) state])
      (lambda ()
        (current-database db)
        (statement-cache cache)
        (execute-with-retry-on-busy "BEGIN IMMEDIATE")
        (match x
          [#(transaction ,f ,from)
           (match (catch (f))
             [#(EXIT ,reason)
              (finalize-lazy-statements cache)
              (execute-with-retry-on-busy "ROLLBACK")
              (gen-server:reply from `#(error ,reason))]
             [,result
              (finalize-lazy-statements cache)
              (execute-with-retry-on-busy "COMMIT")
              (gen-server:reply from `#(ok ,result))])]
          [,logs
           (for-each
            (lambda (x)
              (match-let* ([#(log ,sql ,bindings) x])
                ($execute sql bindings)))
            logs)
           (execute-with-retry-on-busy "COMMIT")]))))

  (define (flush state)
    (cond
     [($state worker) =>
      (lambda (pid)
        (receive
         [#(EXIT ,@pid normal) (flush (update ($state copy [worker #f])))]
         [#(EXIT ,@pid ,reason) (raise reason)]))]
     [else state]))

  (define ($execute sql bindings)
    (sqlite:execute (get-statement sql) bindings))

  (define (execute-with-retry-on-busy sql)
    ;; Use with BEGIN IMMEDIATE, COMMIT, and ROLLBACK
    (define sleep-times '(2 3 6 11 16 21 26 26 26 51 51 . #0=(101 . #0#)))
    (define (attempt stmt count sleep-times)
      (unless (< count 500)
        (raise `#(db-retry-failed ,sql ,count)))
      (match (catch (sqlite:execute stmt '()))
        [#(EXIT #(db-error ,_ (,_ . #x20000005) ,detail)) ; SQLITE_BUSY
         (match sleep-times
           [(,t . ,rest)
            (receive (after t (attempt stmt (+ count 1) rest)))])]
        [#(EXIT ,reason) (raise reason)]
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

  (define cache-timeout (* 5 60 1000))

  (define-record-type cache
    (nongenerative)
    (fields
     (immutable ht)
     (mutable waketime)
     (mutable lazy-statements))
    (protocol
     (lambda (new)
       (lambda ()
         (new (make-hashtable string-hash string=?) #f '())))))

  (define-record-type entry
    (nongenerative)
    (fields
     (immutable stmt)
     (mutable timestamp))
    (protocol
     (lambda (new)
       (lambda (stmt)
         (new stmt (erlang:now))))))

  (define (get-statement sql)
    (let* ([cache (statement-cache)]
           [ht (cache-ht cache)])
      (cond
       [(hashtable-ref ht sql #f) =>
        (lambda (entry)
          (entry-timestamp-set! entry (erlang:now))
          (entry-stmt entry))]
       [else
        (let ([stmt (sqlite:prepare (current-database) sql)])
          (hashtable-set! ht sql (make-entry stmt))
          (unless (cache-waketime cache)
            (cache-waketime-set! cache (+ (erlang:now) cache-timeout)))
          stmt)])))

  (define (finalize-lazy-statements cache)
    (for-each sqlite:finalize (cache-lazy-statements cache))
    (cache-lazy-statements-set! cache '()))

  (define (remove-dead-entries cache)
    (let ([dead (- (erlang:now) cache-timeout)]
          [ht (cache-ht cache)]
          [oldest #f])
      (let-values ([(keys vals) (hashtable-entries ht)])
        (vector-for-each
         (lambda (key val)
           (let ([timestamp (entry-timestamp val)])
             (cond
              [(<= timestamp dead)
               (hashtable-delete! ht key)
               (sqlite:finalize (entry-stmt val))]
              [(or (not oldest) (< timestamp oldest))
               (set! oldest timestamp)])))
         keys vals))
      (cache-waketime-set! cache (and oldest (+ oldest cache-timeout)))))

  ;; Low-level SQLite interface

  (define-record-type database
    (nongenerative)
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

  (define (db-error who error detail)
    (raise `#(db-error ,who ,error ,detail)))

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
    (sqlite:bind stmt bindings)
    (on-exit (osi_reset_statement* (statement-handle stmt))
      (let lp ()
        (let ([row (sqlite:step stmt)])
          (if row
              (cons row (lp))
              '())))))

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
           [stmt (sqlite:prepare (current-database) sql)])
      (cache-lazy-statements-set! cache
        (cons stmt (cache-lazy-statements cache)))
      (sqlite:bind stmt bindings)
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

  (define SQLITE_STATUS_MEMORY_USED 0))
