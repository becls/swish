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
              (cons* x (bitwise-not x) ls))
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

(isolate-mat marshal ()
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

(isolate-mat m1 ()
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
      (catch (transaction db (raise 'failed-transaction)))])
    (db:stop db))))

(mat errors ()
  (match-let*
   ([#(EXIT #(invalid-context lazy-execute)) (catch (lazy-execute "SELECT 1"))]
    [#(EXIT #(invalid-context execute)) (catch (execute "SELECT 1"))]
    [#(EXIT #(invalid-context columns)) (catch (columns "SELECT 1"))]
    [#(EXIT #(db-error open ,_ "*"))
     (catch (sqlite:open "*" SQLITE_OPEN_READWRITE))])
   'ok))

(mat statements ()
  (let ([db (sqlite:open ":memory:" SQLITE_OPEN_READWRITE)])
    (on-exit (sqlite:close db)
      (execute-sql db "create table foo(x, y)")
      (let ([s (sqlite:prepare db "select * from foo where x=? or y=?")])
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
            ["select * from foo where x=? or y=?" (sqlite:sql s)]
            [(2 2)
             (catch (let ([op (open-output-string)]
                          [db2 (sqlite:open ":memory:" SQLITE_OPEN_READWRITE)])
                      (on-exit (sqlite:close db2)
                        (let ([s2 (sqlite:prepare db2 "create table foo(x)")])
                          (on-exit (sqlite:finalize s2)
                            (print-databases op)
                            (print-statements op)
                            (list (database-count) (statement-count)))))))]
            [#("x" "y") (catch (sqlite:columns s))]
            ["select * from foo where x=NULL or y=NULL"
             (catch (sqlite:expanded-sql s))]
            ["select * from foo where x=0 or y=1"
             (catch (sqlite:bind s '(0 1)) (sqlite:expanded-sql s))]
            [#(0 0) (catch (sqlite:step s))]
            [#(0 1) (catch (sqlite:step s))]
            [#(1 1) (catch (sqlite:step s))]
            [#f (catch (sqlite:step s))]
            ["select * from foo where x=NULL or y=NULL"
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

(isolate-mat db-guardian ()
  (let ([g (make-guardian)])
    (g (sqlite:open ":memory:" (+ SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)))
    (gc)
    (assert (handle-gone? (g)))))

(isolate-mat db-open ()
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
    [#(init (#("memory"))) (receive (after 1000 (raise 'timeout)) [,x x])])

   ;; Verify in-memory using shared cache connects to same data
   (transaction shared1
     (execute "CREATE TABLE table1 (col1, col2, col3)")
     (execute "INSERT INTO table1 (col1, col2, col3) VALUES(?,?,?)" 1 2 3))
   (match-let*
    ([(#(1 2 3))
      (transaction shared2 (execute "SELECT col1, col2, col3 FROM table1"))])

    (db:stop file)
    (catch (remove-file filename)))))
