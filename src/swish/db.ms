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

(import
 (chezscheme)
 (swish app-io)
 (swish db)
 (swish erlang)
 (swish io)
 (swish mat)
 (swish osi)
 (swish testing)
 )

(define (stop-event-mgr)
  (cond
   [(whereis 'event-mgr) => (lambda (pid) (kill pid 'kill))]
   [else 'ok]))

(define-syntax db-mat
  (syntax-rules ()
    [(_ name settings e0 e1 ...)
     (isolate-mat name settings
       (stop-event-mgr)
       (start-silent-event-mgr)
       (let () e0 e1 ...))]))

(define-syntax (echo-sql x)
  (syntax-case x ()
    [(k sql)
     (let-values ([(query args) (parse-sql #'sql)])
       (with-syntax ([query (datum->syntax-object #'k query)]
                     [(arg ...) args])
         #'(list query arg ...)))]))

(define (make-integers max-bits)
  (define (ones i ls)
    (if (< i max-bits)
        (ones (+ i 1)
          (let ([x (bitwise-arithmetic-shift 1 i)])
            (cons* x (bitwise-not x) ls)))
        ls))
  (define (twos i j ls)
    (if (< i max-bits)
        (if (< j i)
            (let ([x (bitwise-ior
                      (bitwise-arithmetic-shift 1 i)
                      (bitwise-arithmetic-shift 1 j))])
              (cons* x (bitwise-not x) (twos i (+ j 1) ls)))
            (twos (+ i 1) 0 ls))
        ls))
  (twos 1 0 (ones 0 '(0 -1))))

(define (make-doubles)
  (define B 10)
  (define P 18)
  (define emin -306)
  (define emax 308)

  (define (type1 i x y ls)
    (if (> i P)
        ls
        (type1 (+ i 1) x (/ y B) (cons (+ x y) ls))))

  (define (type2 i x y ls)
    (if (> i P)
        ls
        (let ([x (+ x y)] [y (/ y B)])
          (type2 (+ i 1) x y (cons x ls)))))

  (define (type4 ls)
    (fold-left (lambda (ls x) (cons (* (- B 1) x) ls)) ls ls))

  (define (shift e fractions ls)
    (if (> e emax)
        ls
        (shift (+ e 1) fractions
          (fold-left (let ([s (expt B e)])
                       (lambda (ls x) (cons (exact->inexact (* x s)) ls)))
            ls fractions))))

  (define (remdup ls)
    (if (null? ls)
        '()
        (let loop ([x (car ls)] [ls (cdr ls)] [acc '()])
          (if (null? ls)
              (cons x acc)
              (let ([y (car ls)])
                (if (eqv? x y)
                    (loop x (cdr ls) acc)
                    (loop y (cdr ls) (cons x acc))))))))

  (let ([positives
         (remdup
          (sort fl>
            (shift emin
              (type4 (type2 1 0 (/ B) (type1 3 (/ B) (expt B -3) '())))
              '(0.0))))])
    (fold-left (lambda (ls x) (cons (fl- x) ls))
      positives positives)))

(define (make-strings)
  (define next-char
    (let ([seed 3])
      (lambda ()
        (let ([x (modulo (+ seed 43) #x110000)])
          (set! seed x)
          (if (<= #xD800 x #xDFFF)
              (next-char)
              (integer->char x))))))
  (define (populate s i)
    (when (< i (string-length s))
      (string-set! s i (next-char))
      (populate s (+ i 1))))
  (let loop ([n 1493] [ls '()])
    (if (< n 0)
        ls
        (let ([s (make-string n)])
          (populate s 0)
          (loop (- n 1) (cons s ls))))))

(define (make-blobs)
  (define next-u8
    (let ([seed 3])
      (lambda ()
        (let ([x (modulo (+ seed 43) 256)])
          (set! seed x)
          x))))
  (define (populate bv i)
    (when (< i (bytevector-length bv))
      (bytevector-u8-set! bv i (next-u8))
      (populate bv (+ i 1))))
  (let loop ([n 256] [ls '()])
    (if (< n 0)
        ls
        (let ([bv (make-bytevector n)])
          (populate bv 0)
          (loop (- n 1) (cons bv ls))))))

(print-unicode #f)

(db-mat marshal ()
  (define goodies
    (append '(#f) (make-integers 63) (make-doubles) (make-strings)
      (make-blobs)))
  (match-let* ([#(ok ,db) (db:start&link #f ":memory:" 'create)])
    (transaction db
      (execute "create table data(x)")
      (for-each (lambda (x) (execute "insert into data(x) values(?)" x))
        goodies))
    (transaction db
      (define (check x result)
        (match result
          [(#(,@x)) 'ok]
          [,_ (errorf 'check "~s didn't match: ~s\n" x result)]))
      (fold-left
       (lambda (id x)
         (check x (execute "select x from data where rowid=?" id))
         (+ id 1))
       1 goodies))
    (db:stop db)))

(db-mat marshal-bulk ()
  (define goodies
    (append '(#f) (make-integers 63) (make-doubles) (make-strings)
      (make-blobs)))
  (with-db [db ":memory:" SQLITE_OPEN_READWRITE]
    (execute-sql db "create table data(x)")
    (let ([stmt (sqlite:prepare db "insert into data(x) values(?)")])
      (on-exit (sqlite:finalize stmt)
        (let* ([rows (length goodies)]
               [stmts (make-vector rows stmt)]
               [bindings (list->vector
                          (map
                           (lambda (x)
                             (sqlite:marshal-bindings (list x)))
                           goodies))])
          (on-exit (vector-for-each sqlite:unmarshal-bindings bindings)
            (execute-sql db "BEGIN IMMEDIATE")
            (sqlite:bulk-execute stmts bindings)
            (execute-sql db "COMMIT")))
        (let ()
          (define (check x result)
            (match result
              [(#(,@x)) 'ok]
              [,_ (errorf 'check "~s didn't match: ~s\n" x result)]))
          (fold-left
           (lambda (id x)
             (check x (execute-sql db "select x from data where rowid=?" id))
             (+ id 1))
           1 goodies))))))

;; wrong number of bindings for statement
(db-mat bulk-execute ()
  (with-db [db ":memory:" SQLITE_OPEN_READWRITE]
    (execute-sql db "create table data(x)")
    (let ([stmt (sqlite:prepare db "insert into data(x) values(?)")])
      (on-exit (sqlite:finalize stmt)
        (let ([stmts (vector stmt)]
              [bindings (vector (sqlite:marshal-bindings (list 1 2)))])
          (on-exit (vector-for-each sqlite:unmarshal-bindings bindings)
            (execute-sql db "BEGIN IMMEDIATE")
            (match-let*
             ([#(EXIT #(db-error bulk-execute ,_ ,_))
               (catch (sqlite:bulk-execute stmts bindings))])
             (execute-sql db "COMMIT"))))))))

;; empty vectors to bulk-execute
(mat bulk-execute-empty ()
  (match (try (sqlite:bulk-execute '#() '#()))
    [`(catch #(osi-error osi_bulk_execute ,_ ,_))
     'ok]))

;; mismatched vectors to bulk-execute
(db-mat bulk-execute-mismatch ()
  (with-db [db ":memory:" SQLITE_OPEN_READWRITE]
    (execute-sql db "create table data(x integer unique)")
    (let ([stmt (sqlite:prepare db "insert into data(x) values(?)")]
          [bind (sqlite:marshal-bindings (list 4))])
      (on-exit (begin (sqlite:finalize stmt) (sqlite:unmarshal-bindings bind))
        (let ([stmts (make-vector 24 stmt)]
              [bindings (make-vector 7 bind)])
          (execute-sql db "BEGIN IMMEDIATE")
          (match-let*
           ([#(EXIT #(osi-error osi_bulk_execute ,_ ,_))
             (catch (sqlite:bulk-execute stmts bindings))])
           (execute-sql db "COMMIT")))))))

;; violate a constraint during insert
(db-mat bulk-execute-violate-constraint ()
  (with-db [db ":memory:" SQLITE_OPEN_READWRITE]
    (execute-sql db "create table data(x integer unique)")
    (let ([stmt (sqlite:prepare db "insert into data(x) values(?)")]
          [bind (sqlite:marshal-bindings (list 4))])
      (on-exit (begin (sqlite:finalize stmt) (sqlite:unmarshal-bindings bind))
        (execute-sql db "insert into data(x) values(3)")
        (let ([stmts (make-vector 7 stmt)]
              [bindings (make-vector 7 bind)])
          (execute-sql db "BEGIN IMMEDIATE")
          (match-let*
           ([#(EXIT #(db-error bulk-execute ,_ ,_))
             (catch (sqlite:bulk-execute stmts bindings))]
            [() (execute-sql db "COMMIT")]
            [(#(3) #(4))
             (execute-sql db "select x from data order by x asc")])
           'ok))))))

(db-mat m1 ()
  (match-let*
   ([#(ok ,db) (db:start&link #f ":memory:" 'create)]
    [":memory:" (db:filename db)]
    [created-tables
     (transaction db
       (execute "CREATE TABLE table1 (col1, col2, col3)")
       (execute "CREATE TABLE table2 (col1, col2, col3)")
       (execute "CREATE TABLE table3 (col1, col2, col3)")
       'created-tables)])
   ;; Test that the rollback statement gets reset; otherwise, the drop
   ;; table statement will get a table locked error.
   (match-let*
    ([#(error boom)
      (db:transaction db
        (lambda ()
          (execute "create table temp.foo(x)")
          (raise 'boom)))])
    (transaction db (execute "create table temp.foo(x)"))
    (transaction db (execute "drop table temp.foo")))
   ;; Once more, this time with throw instead of raise
   (match-let*
    ([#(error `(catch pie))
      (db:transaction db
        (lambda ()
          (execute "create table temp.bar(x)")
          (throw 'pie)))])
    (transaction db (execute "create table temp.bar(x)"))
    (transaction db (execute "drop table temp.bar")))
   (do ([i 0 (+ i 1)]) ((= i 150))
     (db:log db "INSERT INTO table1 (col1, col2, col3) VALUES(?,?,?)"
       i i i))
   (receive (after 1000 (void)))
   (do ([i 0 (+ i 1)]) ((= i 150))
     (db:log db "INSERT INTO table2 (col1, col2, col3) VALUES(?,?,?)"
       i i i))
   (match-let*
    ([inserted-rows
      (transaction db
        (do ([i 0 (+ i 1)]) ((= i 150))
          (execute "INSERT INTO table3 (col1, col2, col3) VALUES(?,?,?)"
            i i i))
        'inserted-rows)]
     [selected-rows-lazy
      (transaction db
        (let ([thunk (lazy-execute "SELECT col1 FROM table1")])
          (do ([i 0 (+ i 1)]) ((= i 10))
            (assert (thunk))))
        'selected-rows-lazy)]
     [#("col1" "col2" "Column3")
      (transaction db
        (columns "SELECT col1,col2,col3 AS Column3 FROM table1"))]
     [#(EXIT failed-transaction)
      (catch (transaction db (throw 'failed-transaction)))])
    ;; trigger the handle-info timeout path
    (send db 'timeout)
    (db:stop db))))

(db-mat errors ()
  (capture-events)
  (process-trap-exit #t)
  (match-let*
   ([#(EXIT #(invalid-context lazy-execute)) (catch (lazy-execute "SELECT 1"))]
    [#(EXIT #(invalid-context execute)) (catch (execute "SELECT 1"))]
    [#(EXIT #(invalid-context columns)) (catch (columns "SELECT 1"))]
    [#(EXIT #(db-error open ,_ "*"))
     (catch (sqlite:open "*" SQLITE_OPEN_READWRITE))]
    [#(EXIT #(bad-arg sqlite:marshal-bindings 1))
     (catch (sqlite:marshal-bindings 1))]
    [#(EXIT ,reason)
     (catch (sqlite:unmarshal-bindings 1))]
    [(,_ ,_)
     ;; make coverage does not attribute the exception to bindings-handle.
     (pregexp-match "Exception( in bindings-handle|): 1 is not of type #<record type bindings>."
       (exit-reason->english reason))]
    [#(EXIT ,reason)
     (catch (sqlite:get-bindings 'ties))]
    [(,_ ,_)
     ;; make coverage does not attribute the exception to bindings-handle.
     (pregexp-match "Exception( in bindings-handle|): ties is not of type #<record type bindings>."
       (exit-reason->english reason))]
    ;; check int64 error during sqlite:marshal-bindings
    [,too-large (ash 1 64)]
    [,expected-error
     (format "Exception in Sinteger64_value: ~a is out of range." too-large)]
    [#(EXIT ,reason)
     (catch (sqlite:marshal-bindings (list too-large)))]
    [,@expected-error (exit-reason->english reason)]
    [#(EXIT ,reason)
     (catch (sqlite:marshal-bindings (vector too-large)))]
    [,@expected-error (exit-reason->english reason)]
    ;; check int64 error during sqlite:bind
    [,stmts-before (statement-count)]
    [#(EXIT ,reason)
     (with-db [db ":memory:" SQLITE_OPEN_READWRITE]
       (execute-sql db "create table data(x)")
       (let ([s (sqlite:prepare db "insert into data(x) values(?)")])
         (on-exit (sqlite:finalize s)
           (assert (= (statement-count) (+ stmts-before 1)))
           (catch (sqlite:bind s (list too-large))))))]
    [,@stmts-before (statement-count)]
    [,name (gensym)]
    [,_
     (db:start&link name ":memory:" 'open
       (lambda (db)
         (assert (database? db))
         (raise 'make-db-init-fail)))]
    [#f (whereis name)])
   (receive (after 1000 (throw 'timeout))
     [`(EXIT ,pid make-db-init-fail)
      'ok])
   (match-let*
    ;; Original failure mode:
    ;;  1. db:stop does a gen-server:call to db with timeout 'infinity
    ;;  2. db's handle-call calls flush
    ;;  3. error during flush of bad db:log raises exception, killing worker
    ;;  4. flush receives EXIT message for failed worker and raises exception
    ;;  5. gen-server calls db's terminate, but db state still has worker set
    ;;  6. terminate calls flush with same worker pid
    ;;  7. flush waits indefinitely to receive pid EXIT that was already
    ;;     received in step 4
    ([#(ok ,db) (db:start&link #f ":memory:" 'open)]
     [,_ (db:log db "insert into nosuchtable(x) values(3)")]
     [stopped (db:stop db)])
    (receive (after 1000 (throw 'timeout))
      [`(<gen-server-terminating>
         [pid ,@db]
         [reason #(db-error prepare ,_ ,_)]
         [details `(catch #(db-error prepare ,_ ,_))])
       'ok]))))

(mat statements ()
  (let ([db (sqlite:open ":memory:" SQLITE_OPEN_READWRITE)])
    (on-exit (sqlite:close db)
      (execute-sql db "create table foo(x, y)")
      (let ([s (sqlite:prepare db "select * from foo where x=? or y=? order by rowid asc")])
        (on-exit (sqlite:finalize s)
          (for-each
           (lambda (x)
             (for-each
              (lambda (y)
                (execute-sql db "insert into foo(x, y) values(?, ?)" x y))
              '(0 1)))
           '(0 1))
          (match-let*
           ([4 (catch (sqlite:last-insert-rowid db))]
            ["select * from foo where x=? or y=? order by rowid asc" (sqlite:sql s)]
            [ok (receive (after 1 'ok))] ;; force later timestamp for db2 and s2 for foreign-handle printing
            [(2 2)
             (catch (let ([op (open-output-string)]
                          [db2 (sqlite:open ":memory:" SQLITE_OPEN_READWRITE)])
                      (on-exit (sqlite:close db2)
                        (let ([s2 (sqlite:prepare db2 "create table foo(x)")])
                          (on-exit (sqlite:finalize s2)
                            (print-databases op)
                            (match-regexps
                             (map
                              (lambda (db)
                                (format "[0-9]+: :memory: opened ~s" (database-create-time db)))
                              (list db db2))
                             (split (get-output-string op) #\newline))
                            (print-statements op)
                            (match-regexps
                             (map
                              (lambda (s)
                                (format "[0-9]+: [0-9]+ ~a prepared [0-9]+"
                                  (pregexp-quote (statement-sql s))))
                              (list s s2))
                             (split (get-output-string op) #\newline))
                            (list (database-count) (statement-count)))))))]
            [#("x" "y") (catch (sqlite:columns s))]
            ["select * from foo where x=NULL or y=NULL order by rowid asc"
             (catch (sqlite:expanded-sql s))]
            ["select * from foo where x=0 or y=1 order by rowid asc"
             (catch (sqlite:bind s '(0 1)) (sqlite:expanded-sql s))]
            [#(0 0) (catch (sqlite:step s))]
            [#(0 1) (catch (sqlite:step s))]
            [#(1 1) (catch (sqlite:step s))]
            [#f (catch (sqlite:step s))]
            ["select * from foo where x=NULL or y=NULL order by rowid asc"
             (catch (sqlite:clear-bindings s) (sqlite:expanded-sql s))]
            [#(0 1) (catch (sqlite:bind s '(1 1)) (sqlite:step s))]
            [#(EXIT #(db-error step (sqlite3_step -6000009 . "interrupted") ,_))
             (catch (sqlite:interrupt db) (sqlite:step s))]
            [#(EXIT #(db-error prepare ,_ ,_))
             (catch (sqlite:prepare db "insert"))]
            ;; Close the database
            [ok (catch (sqlite:close db) 'ok)]
            [#(EXIT #(osi-error osi_bind_statement osi_bind_statement ,_))
             (catch (sqlite:bind s '(0 1)))]
            [#(EXIT #(osi-error osi_clear_statement_bindings osi_clear_statement_bindings ,_))
             (catch (sqlite:clear-bindings s))]
            [#(EXIT #(osi-error osi_get_statement_columns osi_get_statement_columns ,_))
             (catch (sqlite:columns s))]
            [#(EXIT #(osi-error osi_get_statement_expanded_sql osi_get_statement_expanded_sql ,_))
             (catch (sqlite:expanded-sql s))]
            [#(EXIT #(osi-error osi_step_statement osi_step_statement ,_))
             (catch (sqlite:step s))])
           'ok))))))

(mat bindings ()
  (let ([count (bindings-count)]
        [vals '#(#f 1 2.34 "text" #vu8(1 2 3 4))])
    (let ([b1 (sqlite:marshal-bindings (vector->list vals))]
          [b2 (sqlite:marshal-bindings vals)])
      (on-exit (begin (sqlite:unmarshal-bindings b1)
                      (sqlite:unmarshal-bindings b2))
        (let ([op (open-output-string)])
          (print-bindings op)
          (let ([pat (format "  [0-9]+: ~a" (pregexp-quote (format "~s" vals)))])
            (match-regexps `(seek ,pat seek ,pat)
              (split (get-output-string op) #\newline)))
          (assert (= (bindings-count) (+ count 2))))
        (match-let*
         ([,@vals (sqlite:get-bindings b1)]
          [,@vals (sqlite:get-bindings b2)])
         'ok)))))

(mat invalid-bindings ()
  (let ([count (bindings-count)])
    (match-let*
     ([`(catch #(osi-error osi_marshal_bindings ,_ ,_))
       (try (sqlite:marshal-bindings '(#f 1 2.34 symbol "text" #vu8(1 2 3 4))))]
      [`(catch #(osi-error osi_marshal_bindings ,_ ,_))
       (try (sqlite:marshal-bindings '#(#f 1 2.34 symbol "text" #vu8(1 2 3 4))))]
      [,@count (bindings-count)])
     'ok)))

(mat expand-sql ()
  (match-let*
   ([("insert into table1(col1, col2, col3) values(?, ?, ?)" 1 1 1)
     (let ([i 1])
       (echo-sql (insert table1 ([col1 ,i] [col2 ,i] [col3 ,i]))))]
    [("update table1 set col1=1 where rowid = 1")
     (echo-sql (update table1 ([col1 "1"]) where rowid = "1"))]
    [("update table1 set col1=1, col2=1 where rowid = 1")
     (echo-sql (update table1 ([col1 "1"] [col2 "1"]) where rowid = "1"))]
    [("update table1 set col1=?, col2=? where id = ?" 1 2 3)
     (let ([col1 1] [col2 2] [id 3])
       (echo-sql (update table1 ([col1 ,col1] [col2 ,col2]) where id = ,id)))]
    [("delete from table1")
     (echo-sql (delete table1))]
    [("delete from table1 where id > 10")
     (echo-sql (delete table1 where id > "10"))])
   (assert-syntax-error
    (echo-sql (insert table1 ([col1 1])))
    "invalid SQL term")
   (assert-syntax-error
    (echo-sql (update table1 ([col1 1]) where rowid = "1"))
    "invalid SQL term")
   (assert-syntax-error
    (echo-sql (delete table1 where rowid = 1))
    "invalid SQL term")
   'ok))

(db-mat db-guardian ()
  (let ([g (make-guardian)])
    (g (sqlite:open ":memory:" (+ SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)))
    (gc)
    (assert (handle-gone? (g)))))

(db-mat db-open ()
  (match-let*
   ([,filename (path-combine (data-dir) "test-db.db3")]
    [#(ok ,file) (db:start&link #f filename 'create)]
    [#(ok ,tmp) (db:start&link #f "" 'create)]
    [#(ok ,mem) (db:start&link #f ":memory:" 'create)]
    [#(ok ,shared1) (db:start&link #f "file::memory:?cache=shared" 'create)]
    [#(ok ,shared2) (db:start&link #f "file::memory:?cache=shared" 'create)]
    [#(ok ,init)
     (db:start&link #f ":memory:" 'create
       (let ([me self])
         (lambda (db)
           (send me `#(init ,(execute-sql db "pragma journal_mode"))))))]
    [#(init (#("memory"))) (receive (after 1000 (throw 'timeout)) [,x x])])

   ;; Verify in-memory using shared cache connects to same data
   (transaction shared1
     (execute "CREATE TABLE table1 (col1, col2, col3)")
     (execute "INSERT INTO table1 (col1, col2, col3) VALUES(?,?,?)" 1 2 3))
   (match-let*
    ([(#(1 2 3))
      (transaction shared2 (execute "SELECT col1, col2, col3 FROM table1"))])

    (db:stop file)
    (catch (remove-file filename)))))

(db-mat retry ()
  (capture-events)
  (match-let*
   ([,db-uri "file::memory:?cache=shared"]
    [#(ok ,shared1) (db:start&link #f db-uri 'create)]
    [#(ok ,shared2) (db:start&link #f db-uri 'create)]
    [()
     (transaction shared1
       (execute "CREATE TABLE table1 (x, y)"))]
    [,me self]
    [,pid
     (spawn&link
      (lambda ()
        (transaction shared1
          (execute "INSERT INTO table1 (x, y) VALUES(?,?)" 1 2)
          (send me 'go-ahead)
          (receive (after 1000 'ok)))))]
    [ok (receive (after 1000 (throw 'timeout))
          [go-ahead 'ok])]
    [(#(1 2))
     (transaction shared2
       (execute "SELECT x, y FROM table1"))])
   (receive (after 2000 (throw 'timeout))
     [`(<transaction-retry> [database ,@db-uri] ,count)
      (guard (> count 0))
      'ok])))

(isolate-mat crash ()
  ;; Explicitly not using db-mat form because we want to build an
  ;; event-mgr that dumps output to the console.
  (stop-event-mgr)
  (start-event-mgr)
  (capture-events)
  (process-trap-exit #t)
  (match-let*
   ([,op (open-output-string)]
    [,_ (console-error-port op)]
    [,_ (console-output-port op)]
    [#(ok ,db) (db:start&link #f ":memory:" 'create)]
    [,me self]
    [,pid
     (spawn
      (lambda ()
        (transaction db
          (send me `(in-transaction . ,self))
          ;; block while test populates db queue
          (receive [permission 'ok]))))]
    [,waiting-pid (receive [(in-transaction . ,waiting-pid) waiting-pid])]
    ;; no such table
    [,sql "insert into test(x) values(?)"]
    [,data '(42 3.7 "asdf" #f #vu8(7 6 5))]
    [,_ (for-each (lambda (x) (db:log db sql x)) data)]
    [,_ (send db 'crash)]
    ;; unblock the transaction
    [,_ (send waiting-pid 'permission)])
   (receive [`(EXIT ,@db #(db-error prepare ,_ ,_)) 'ok])
   (event-mgr:flush-buffer)
   (receive [`(<gen-server-terminating> [pid ,@db] ,state) 'ok])
   (let ([output (get-output-string op)])
     ;; look for data passed to db:log in the stack dump
     (for-each
      (lambda (datum)
        (assert
         (pregexp-match
          (parameterize ([print-level 0])
            (string-append
             (pregexp-quote "#(<log>") "\\s+"
             (format "(#[0-9]=~a|#[0-9]#)" (pregexp-quote sql))
             "\\s+"
             (pregexp-quote (format "#<bindings ~s>" datum))
             (pregexp-quote ")")))
          output)))
      data))))

(db-mat record-writers ()
  ;; database, statement, and bindings
  (match-let*
   ([,db (sqlite:open ":memory:" SQLITE_OPEN_READONLY)]
    [,stmt (sqlite:prepare db "select 123")]
    [,_ (sqlite:finalize stmt)]
    [,_ (sqlite:close db)]
    [,db-expected (format "#<database ~s>" ":memory:")]
    [,@db-expected (format "~s" db)]
    [,stmt-expected (format "#<statement ~a ~s>" db-expected "select 123")]
    [,@stmt-expected (format "~s" stmt)]
    [,mbindings (sqlite:marshal-bindings '(#vu8(1 2 3 4 5 6 7) 123 #f 98.6 "test"))]
    ["#<bindings #vu8(1 2 3 4 5 6 7) 123 #f 98.6 \"test\">"
     (format "~s" mbindings)]
    ["#<bindings #vu8(1 2 3 4 5 6 7) 123 #f 98.6 \"test\">"
     (parameterize ([print-level 2]) (format "~s" mbindings))]
    ["#<bindings #vu8(1 2 ...) 123 ...>"
     (parameterize ([print-length 2]) (format "~s" mbindings))]
    [,_ (sqlite:unmarshal-bindings mbindings)]
    ["#<bindings>" (format "~s" mbindings)]
    )
   'ok))
