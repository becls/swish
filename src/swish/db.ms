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

(define (assert-stmt-counts before)
  (let ([after (statement-count)])
    (unless (= before after)
      (errorf 'assert-stmt-counts
        "statement counts didn't match, before ~a and after ~a\n"
        before after))))

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

(db-mat bulk-execute-null-case ()
  (with-db [db ":memory:" SQLITE_OPEN_READWRITE]
    (execute-sql db "create table data(x)")
    (let ([stmt (sqlite:prepare db "insert into data(x) values(10)")])
      (on-exit (sqlite:finalize stmt)
        (let ([stmts (vector stmt)]
              [bindings (vector (sqlite:marshal-bindings '#()))])
          (on-exit (vector-for-each sqlite:unmarshal-bindings bindings)
            (match-let*
             ([() (execute-sql db "BEGIN IMMEDIATE")]
              [#t (sqlite:bulk-execute stmts bindings)]
              [() (execute-sql db "COMMIT")]
              [(#(10)) (execute-sql db "select x from data")])
             'ok))))))
  (match-let*
   ([#(ok ,db) (db:start&link #f ":memory:" 'create)]
    [ok (transaction db (execute "CREATE TABLE data(x)") 'ok)]
    [ok (db:log db "insert into data(x) values(?)" 1)]
    [ok (db:log db "insert into data(x) values(?)" 2)]
    [ok (db:log db "insert into data(x) values(3)")]
    [ok (db:log db "insert into data(x) values(?)" 4)]
    [ok (db:log db "insert into data(x) values(5)")]
    [ok (db:log db "insert into data(x) values(6)")]
    [(1 2 3 4 5 6)
     (map (lambda (x) (match x [#(,data) data]))
       (transaction db
         (execute "select x from data order by rowid asc")))])
   (db:stop db)))

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
     [selected-rows-lazy/w-bindings
      (transaction db
        (let ([thunk (lazy-execute "SELECT col1 FROM table1 where ? < ?" 0 1)])
          (do ([i 0 (+ i 1)]) ((= i 10))
            (assert (thunk))))
        'selected-rows-lazy/w-bindings)]
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
     (with-db [db ":memory:" SQLITE_OPEN_READWRITE]
       (execute-sql db "create table data(x)")
       (let ([s (sqlite:prepare db "insert into data(x) values(?)")])
         (on-exit (sqlite:finalize s)
           (catch (sqlite:execute s (list too-large))))))]
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
           (assert-stmt-counts (+ stmts-before 1))
           (catch (sqlite:bind s (list too-large))))))]
    [,@stmts-before (statement-count)]
    [#(EXIT #(bad-arg db:start&link "options"))
     (catch (db:start&link #f ":memory:" 'open "options"))]
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
            ;; vector for bindings
            [(#(0 1) #(1 1)) (sqlite:execute s '#(4 1))]
            ;; marshaled bindings for bindings
            [,bound (sqlite:marshal-bindings '(1 3))]
            [(#(1 0) #(1 1)) (sqlite:execute s bound)]
            ["select * from foo where x=NULL or y=NULL order by rowid asc"
             (sqlite:expanded-sql s)]
            [,_ (sqlite:unmarshal-bindings bound)]
            ["select * from foo where x=NULL or y=NULL order by rowid asc"
             (catch (sqlite:clear-bindings s) (sqlite:expanded-sql s))]
            [#(0 1) (catch (sqlite:bind s '(1 1)) (sqlite:step s))]
            [#(EXIT #(db-error step (sqlite3_step -6000009 . "interrupted") ,_))
             (catch (sqlite:interrupt db) (sqlite:step s))]
            [#(EXIT #(db-error prepare ,_ ,_))
             (catch (sqlite:prepare db "insert"))]
            ;; too many arguments for statement: list case
            [#(EXIT #(osi-error osi_bind_statement sqlite3_bind_int64 (-6000025 . "column index out of range")))
             (catch (sqlite:bind s '(1 2 3 4)))]
            ;; omit some arguments for statement: list case
            ["select * from foo where x=7 or y=NULL order by rowid asc"
             (catch
              (sqlite:clear-bindings s)
              (sqlite:bind s '(7))
              (sqlite:expanded-sql s))]
            ;; too many arguments for statement: marshaled bindings case
            [,mbindings (sqlite:marshal-bindings '(1 2 3 4))]
            [#(EXIT #(osi-error osi_bind_statement_bindings sqlite3_bind_int64 (-6000025 . "column index out of range")))
             (on-exit (sqlite:unmarshal-bindings mbindings)
               (catch (sqlite:execute s mbindings)))]
            ;; omit some arguments for statement: marshaled bindings case
            [,mbindings (sqlite:marshal-bindings '(11))]
            [() ;; no rows with null y
             (on-exit (sqlite:unmarshal-bindings mbindings)
               (sqlite:clear-bindings s)
               (sqlite:execute s mbindings))]
            ["select * from foo where x=NULL or y=NULL order by rowid asc"
             (sqlite:expanded-sql s)]
            ;; mbindings can be collected after sqlite:execute returns
            [,g (make-guardian)]
            [(#(0 0) #(0 1))
             (on-exit (gc)
               (let ([mbindings (sqlite:marshal-bindings '(0 7))])
                 (g mbindings)
                 (sqlite:execute s mbindings)))]
            [#t (bindings? (g))]
            ;; clear-bindings works
            ["select * from foo where x=NULL or y=NULL order by rowid asc"
             (begin
               (sqlite:clear-bindings s)
               (sqlite:expanded-sql s))]
            ;; don't crash if mbindings unmarshaled after sqlite:execute
            [,mbindings (sqlite:marshal-bindings '("text" "string"))]
            [() (sqlite:execute s mbindings)]
            ["select * from foo where x=NULL or y=NULL order by rowid asc"
             (begin
               (sqlite:unmarshal-bindings mbindings)
               (sqlite:expanded-sql s))]
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
          [b2 (sqlite:marshal-bindings vals)]
          [b3 (sqlite:marshal-bindings '())]
          [b4 (sqlite:marshal-bindings '#())])
      (on-exit (begin (sqlite:unmarshal-bindings b1)
                      (sqlite:unmarshal-bindings b2)
                      (sqlite:unmarshal-bindings b3)
                      (sqlite:unmarshal-bindings b4))
        (let ([op (open-output-string)])
          (print-bindings op)
          (let ([pat (format "  [0-9]+: ~a" (pregexp-quote (format "~s" vals)))])
            (match-regexps `(seek ,pat seek ,pat)
              (split (get-output-string op) #\newline)))
          ;; we don't allocate records to marshal empty bindings
          (assert (= (bindings-count) (+ count 2))))
        (match-let*
         ([,@vals (sqlite:get-bindings b1)]
          [,@vals (sqlite:get-bindings b2)]
          [#() (sqlite:get-bindings b3)]
          [#() (sqlite:get-bindings b4)])
         'ok)))))

(mat raw-bindings ()
  (match-let*
   ([,vals '#(#f 1 2.34 "text" #vu8(1 2 3 4))]
    [,p1 (osi_marshal_bindings (vector->list vals))]
    [,p2 (osi_marshal_bindings vals)]
    [,p3 (osi_marshal_bindings '())]
    [,p4 (osi_marshal_bindings '#())])
   (on-exit (begin (osi_unmarshal_bindings p1)
                   (osi_unmarshal_bindings p2)
                   (osi_unmarshal_bindings p3)
                   (osi_unmarshal_bindings p4))
     (match-let*
      ([,@vals (osi_get_bindings p1)]
       [,@vals (osi_get_bindings p2)]
       [#() (osi_get_bindings p3)]
       [#() (osi_get_bindings p4)])
      'ok))))

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
   ([,filename
     (make-directory-path
      (path-combine (data-dir) "test-db.db3"))]
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

(define (do-transaction trans-proc)
  (match-let*
   ([,me self]
    [,stmts-before (statement-count)]
    [#(ok ,db) (db:start&link #f ":memory:" 'create)])
   (spawn
    (lambda ()
      ;; Fire up a transaction that may not complete, with a live
      ;; statement.
      (transaction db
        (trans-proc (lambda () (send me 'ready))))))
   (receive [ready 'ok])
   ;; Give the transaction some time to start executing
   (receive (after 10 'ok))
   (unlink db)
   (db:stop db)
   (assert-stmt-counts stmts-before)))

(db-mat terminate-live-statement ()
  (do-transaction
   (lambda (ready)
     (ready)
     (execute "SELECT 1")
     (receive))))

(db-mat terminate-live-statement-trap-exit ()
  (do-transaction
   (lambda (ready)
     (process-trap-exit #t)
     (ready)
     (execute "SELECT 1")
     (receive))))

(db-mat transaction-link ()
  (do-transaction
   (lambda (ready)
     (link (process-parent))
     (ready)
     (execute "SELECT 1")
     (receive))))

(db-mat transaction-link-error ()
  (do-transaction
   (lambda (ready)
     (link (process-parent))
     (ready)
     (execute "SELECT 1")
     (throw 'boom!))))

(db-mat transaction-unlink ()
  (do-transaction
   (lambda (ready)
     (unlink (process-parent))
     (ready)
     (execute "SELECT 1")
     (receive))))

(db-mat transaction-unlink-trap-exit ()
  (do-transaction
   (lambda (ready)
     (process-trap-exit #t)
     (unlink (process-parent))
     (ready)
     (execute "SELECT 1")
     (receive))))

(db-mat trap-exit-succeed-after-shutdown ()
  (do-transaction
   (lambda (ready)
     (process-trap-exit #t)
     (ready)
     (execute "SELECT 1")
     (receive [`(EXIT ,_ shutdown) 'done]))))

(db-mat trap-exit-succeed-after-shutdown-with-busy ()
  (do-transaction
   (lambda (ready)
     (process-trap-exit #t)
     (ready)
     (try
      (execute
       (string-append
        "with recursive ints(n) as\n"
        " ( select 1 union all select n + 1 from ints )\n"
        "select max(n) from ints")))
     (receive [`(EXIT ,_ shutdown) 'done]))))

(db-mat terminate-live-trap-exit-alternate-error ()
  (match-let*
   ([,me self]
    [,stmts-before (statement-count)]
    [#(ok ,db) (db:start&link #f ":memory:" 'create)])
   (spawn
    (lambda ()
      (transaction db
        (process-trap-exit #t)
        (send me 'ready)
        (execute "SELECT 1")
        (receive [`(EXIT ,_ shutdown) (execute "ROLLBACK")]))))
   (receive [ready 'ok])
   (spawn
    (lambda ()
      (transaction db
        (execute "SELECT 1"))
      (send me 'second-transaction-completed)))
   (receive (after 10 'ok))

   (unlink db)
   (db:stop db)
   (receive (after 10 'ok)
     [second-transaction-completed
      (throw 'transaction-should-not-have-completed)])
   (assert-stmt-counts stmts-before)))

(db-mat terminate-while-busy ()
  (do-transaction
   (lambda (ready)
     (ready)
     (execute
      (string-append
       "with recursive ints(n) as\n"
       " ( select 1 union all select n + 1 from ints )\n"
       "select max(n) from ints")))))

(db-mat terminate-while-busy-extended ()
  ;; Query runs forever. Gets interrupted. Foolishly, someone caught
  ;; the error and tried to rollback the transaction and causes a
  ;; failure outside of user-code.
  (do-transaction
   (lambda (ready)
     (ready)
     (try
      (execute
       (string-append
        "with recursive ints(n) as\n"
        " ( select 1 union all select n + 1 from ints )\n"
        "select max(n) from ints")))
     (execute "ROLLBACK"))))

(db-mat terminate-bad-transaction ()
  (match-let*
   ([,me self]
    [,stmts-before (statement-count)]
    [#(ok ,db) (db:start&link #f ":memory:" 'create)]
    [,_ (process-trap-exit #t)]
    [,pid (spawn&link
           (lambda ()
             ;; Fire up a transaction that jumps the gun by doing the
             ;; rollback that the gen-server will do when the transaction
             ;; crashes.
             (transaction db
               (execute "ROLLBACK")
               (send me 'crashing)
               (throw 'crashed))))])
   (receive [crashing 'ok])
   (receive
    [`(EXIT ,@db #(db-error step (sqlite3_step ,_ . ,_) "ROLLBACK")) 'ok])
   (receive
    [`(EXIT ,@pid #(#(db-error step (sqlite3_step ,_ . ,_) "ROLLBACK")
                    #(gen-server call ,_)))
     'ok])
   (assert-stmt-counts stmts-before)))

(db-mat errant-messages ()
  (match-let*
   ([#(ok ,db) (db:start&link #f ":memory:" 'create)]
    ;; This is more for coverage than an actual test. It is unlikely
    ;; that we get an errant DOWN message, but if we do, the db
    ;; gen-server should survive.
    [,m (monitor (spawn (lambda () (throw 'boom!))))]
    [,msg (receive [,(msg <= `(DOWN ,@m ,_ ,_ ,_)) msg])]
    [,_ (send db msg)]
    [(#(1)) (transaction db (execute "SELECT 1"))]
    [,m (monitor (spawn (lambda () 'done)))]
    [,msg (receive [,(msg <= `(DOWN ,@m ,_ ,_ ,_)) msg])]
    [,_ (send db msg)]
    [(#(1)) (transaction db (execute "SELECT 1"))]
    ;; The db gen-server should ignore EXIT message from worker.
    [#t (transaction db (kill db 'howdy))]
    [(#(1)) (transaction db (execute "SELECT 1"))]
    ;; If the db gen-server receives an EXIT message from a process
    ;; other than worker, it should terminate.
    [,me self]
    [,_ (unlink db)]
    [,_ (spawn (lambda () (kill db 'bye) (send me 'ready)))]
    [ok (receive [ready 'ok])]
    [,m (monitor db)])
   (receive (after 100 (throw 'timeout))
     [`(DOWN ,@m ,@db bye) 'ok])))

(db-mat flush-queue ()
  ;; check that we flush lazy transactions on terminate
  (match-let*
   ([,op (open-output-string)]
    [,_ (console-error-port op)]
    [,_ (console-output-port op)]
    [#(ok ,db1)
     (db:start&link #f "file::memory:?cache=shared" 'create
       (db:options
        [commit-delay 5000]))]
    [#(ok ,db2)
     (db:start&link #f "file::memory:?cache=shared" 'open)]
    [ok
     (transaction db1
       (execute "create table data(x)")
       'ok)]
    [ok (db:log db1 "insert into data(x) values(?)" 1)]
    [() (transaction db2 (execute "select * from data"))]
    [stopped (db:stop db1)]
    [(#(1)) (transaction db2 (execute "select * from data"))])
   'ok))

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

(db-mat check-options ()
  (match-let*
   ([,wrong-arity (lambda (x) x)]
    [`(catch #(bad-arg init ,@wrong-arity))
     (try (db:options [init wrong-arity]))]
    [`(catch #(bad-arg init bogus))
     (try (db:options [init 'bogus]))]
    [`(catch #(bad-arg cache-timeout -1))
     (try (db:options [cache-timeout -1]))]
    [`(catch #(bad-arg cache-timeout whenever))
     (try (db:options [cache-timeout 'whenever]))]
    [`(catch #(bad-arg commit-delay -3))
     (try (db:options [commit-delay -3]))]
    [`(catch #(bad-arg commit-delay none))
     (try (db:options [commit-delay 'none]))]
    [`(catch #(bad-arg commit-limit 3.4))
     (try (db:options [commit-limit 3.4]))])
   'ok))

(define (julian->timestamp j)
  ;; based on example at https://sqlite.org/lang_datefunc.html
  (exact (round (* (- j 2440587.5) 86400 1000))))

(db-mat commit-delay ()
  (match-let*
   ([,op (open-output-string)]
    [,_ (console-error-port op)]
    [,_ (console-output-port op)]
    [,delay-ms 250]
    [,expected '(1 3 7 25)]
    [#(ok ,db)
     (db:start&link #f ":memory:" 'create
       (db:options
        [commit-delay delay-ms]
        [commit-limit 10000]))]
    [ok
     (transaction db
       (execute "create table log(id,queued,actual)")
       'ok)]
    [,sql "insert into log(id,queued,actual) values(?,?,julianday('now'))"]
    [,_ (gen-server:debug db '(message) '())]
    [,rev-actual '()]
    [,me self]
    [,_ (event-mgr:add-handler
         (let ([log 0])
           (define-tuple <log> sql mbindings)
           (lambda (x)
             (match x
               [`(<gen-server-debug> [type 2] [message `(<log>)])
                (set! log (+ log 1))]
               [`(<gen-server-debug> [type 3] [message `(DOWN ,_ ,worker ,r)])
                (set! rev-actual (cons log rev-actual))
                (set! log 0)
                (send me 'worker-finished)]
               [#(,@me waiting) (send me 'in-sync)]
               [,_ (void)]))))]
    [ok
     (let insert ([i 0] [rows expected])
       (match rows
         [() 'ok]
         [(,n . ,rows)
          (do ([j 0 (fx+ j 1)]) ((fx= j n))
            (db:log db sql (fx+ i j) (erlang:now)))
          ;; wait for the commit-delay
          (receive (after delay-ms 'ok))
          ;; now wait for the worker to complete
          (receive (after 2000 (throw 'timeout-waiting-for-worker))
            [worker-finished 'ok])
          (insert (fx+ i n) rows)]))]
    [,_ (event-mgr:notify `#(,self waiting))]
    [ok (receive [in-sync 'ok])]
    [,@expected (reverse rev-actual)])
   (define (print-row id ts1 ts2 delta)
     (when (getenv "DEBUG")
       (printf "~3a ~13a ~13a ~5a\n" id ts1 ts2 delta)))
   (transaction db
     (print-row "id" "queued" "actual" "delta")
     (for-each
      (lambda (v)
        (match v
          [#(,id ,ts1 ,julian)
           (let ([ts2 (julian->timestamp julian)])
             (print-row id ts1 ts2 (- ts2 ts1))
             (assert (<= delay-ms (- ts2 ts1) (* 2 delay-ms))))]))
      (execute "select id, queued, actual from log order by rowid asc")))))

(db-mat cache-timeout ()
  (match-let*
   ([,op (open-output-string)]
    [,_ (console-error-port op)]
    [,_ (console-output-port op)]
    [#(ok ,db)
     (db:start&link #f ":memory:" 'create
       (db:options
        [cache-timeout 0]))]
    [,stmts-before (statement-count)]
    [ok
     (transaction db
       (execute "create table data(x)")
       (execute "insert into data(x) values(?)" 1)
       (execute "insert into data(x) values(?)" 2)
       (execute "insert into data(x) values(?)" 3)
       'ok)]
    [ok (db:log db "insert into data(x) values(4)")]
    [ok (db:log db "insert into data(x) values(?)" 5)]
    [(#(1) #(2) #(3) #(4) #(5))
     (transaction db
       (execute "select x from data order by rowid asc"))]
    [ok (receive (after 10 'ok))]
    [,@stmts-before (statement-count)])
   'ok))

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

(db-mat expire-cache ()
  (match-let*
   ([#(ok ,db) (db:start&link #f ":memory:" 'create)]
    [,stmts-before (statement-count)])
   (transaction db
     (execute "CREATE TABLE table1 (col1, col2, col3)")
     (execute "INSERT INTO table1 (col1, col2, col3) VALUES(?,?,?)" 1 2 3)
     (execute "INSERT INTO table1 (col1, col2, col3) VALUES(?,?,?)" 4 5 6))
   ;; BEGIN IMMEDIATE, COMMIT, CREATE, INSERT
   (assert-stmt-counts (+ stmts-before 4))
   (db:expire-cache db)
   ;; Only BEGIN IMMEDIATE, COMMIT remain.
   (assert-stmt-counts (+ stmts-before 2))
   (db:stop db)))
