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

(http:include "components.ss")

;; String manipulation

(define (stringify x) (format "~a" x))

(define (trim-whitespace s)
  (define (find-start s i len)
    (cond
     [(eqv? i len) ""]
     [(char-whitespace? (string-ref s i)) (find-start s (fx+ i 1) len)]
     [else (find-end s (fx- len 1) i len)]))
  (define (find-end s i start len)
    (cond
     [(eqv? i start)
      (if (eqv? len 1)
          s
          (string (string-ref s i)))]
     [(char-whitespace? (string-ref s i)) (find-end s (fx- i 1) start len)]
     [else
      (if (and (eqv? start 0) (eqv? (fx- len 1) i))
          s
          (substring s start (fx+ i 1)))]))
  (find-start s 0 (string-length s)))

;; HTTP/HTML responses

(define-syntax respond
  (syntax-rules ()
    [(_ c1 c2 ...)
     (hosted-page "Log DB Query"
       (list
        (css-include "css/query-db.css")
        (js-include "js/jquery-1.4.4.min.js")
        (js-include "js/query-db.js"))
       c1 c2 ...)]))

(define (respond:error reason sql)
  (respond
   (match reason
     [#(db-query-failed empty-query ,sql)
      (section "No query given" `(p ,sql))]
     [#(db-query-failed not-a-query ,sql)
      (section "Not a SELECT or EXPLAIN statement" `(p ,sql))]
     [,_
      (section "Query failed" `(p ,(exit-reason->english reason)))])))

;; Home page

(define (do-home db last-sql)
  (define (table-info master-row)
    (match master-row
      [#(,table-name)
       (cons
        (string->symbol table-name)
        (map column-info
          (execute-sql db (format "pragma table_info(~s)" table-name))))]))
  (define (column-info table-info)
    (match table-info
      [#(,_ ,name ,type ,_ ,_ ,_)
       (cons (string->symbol name) type)]))
  (let ([db-tables
         (map table-info
           (execute-sql db "select tbl_name from SQLITE_MASTER where type in (?, ?) order by tbl_name" "table" "view"))])
    (respond
     `(div
       (p "Please enter a SELECT or EXPLAIN statement in "
         (a (@ (href "http://www.sqlite.org/lang_select.html")) "SQLite syntax") ". ")
       (p (i "Note: LIMIT and OFFSET clauses are not allowed."))
       (form (@ (method "get") (class "schema"))
         (input (@ (name "limit") (class "hidden") (value 100)))
         (input (@ (name "offset") (class "hidden") (value 0)))
         (p (textarea (@ (id "sql") (name "sql") (class "sql"))
              ,(or last-sql "")))
         (p (button (@ (type "submit")) "Run Query"))))
     (section "Schema"
       (schema->html db-tables)))))

(define (schema->html db-tables)
  (define (db-table->tr table)
    (match table
      [(,name . ,columns)
       (subsection (stringify name)
         `(table ,@(map column->tr columns)))]))
  (define (column->tr column-type)
    (match column-type
      [(,column . ,type)
       `(tr (td ,(stringify column))
          (td ,type))]))
  `(div (@ (class "schema"))
     ,@(map db-table->tr db-tables)))

;; Running a query

(define (do-query db sql limit offset)
  (define (check-request)
    (cond
     [(string=? sql "") (raise `#(db-query-failed empty-query ,sql))]
     [(or (starts-with-ci? sql "select ")
          (starts-with-ci? sql "with ")
          (starts-with-ci? sql "explain "))
      (if (and limit offset)
          'ok
          (raise `#(db-query-failed missing-limit-offset ,limit ,offset ,sql)))]
     [else (raise `#(db-query-failed not-a-query ,sql))]))
  (define (home-link last-sql)
    `(a (@ (href ,(format "query-db?lastSql=~a"
                    (http:percent-encode last-sql))))
       "Return to query page"))
  (define (nav-form where from-offset enabled?)
    `(form (@ (name "query") (method "get"))
       (textarea (@ (name "sql") (class "hidden")) ,sql)
       (input (@ (name "limit") (class "hidden") (value ,(stringify limit))))
       (input (@ (name "offset") (class "hidden") (value ,(stringify from-offset))))
       (button ,(if enabled?
                    '(@ (type "submit"))
                    '(@ (type "submit") (disabled)))
         ,(stringify where))))
  (define (get-results next-row f)
    (let lp ([results '()])
      (match (next-row)
        [#f (reverse results)]
        [,row (lp (cons (f row) results))])))
  (define (row->tr row)
    `(tr ,@(map value->td (vector->list row))))
  (define (value->td v)
    `(td ,(cond
           [(bytevector? v) `(i "Binary data")]
           [(not v) "<null>"]
           [else (stringify v)])))

  (check-request)
  (match-let*
   ([,stmt (sqlite:prepare db (format "~a limit ? offset ?" sql))]
    [,_ (sqlite:bind stmt (list limit offset))]
    [,results (get-results (lambda () (sqlite:step stmt)) row->tr)]
    [,count (length results)])
   (if (= count 0)
       (respond (section "Query finished" `(p ,(home-link sql))))
       (respond
        `(table
          (tr (@ (style "text-align: center;"))
            (td (@ (class "navigation"))
              ,(nav-form "Previous Page" (max 0 (- offset limit)) (> offset 0)))
            (td (@ (class "navigation"))
              (form (@ (id "rowForm") (method "get"))
                (textarea (@ (name "sql") (class "hidden")) ,sql)
                (input (@ (name "limit") (class "hidden") (value ,(stringify limit))))
                (button (@ (id "offsetButton") (type "submit")) "Go to row")
                (p (input (@ (id "offsetInput") (name "offset") (class "offset"))))))
            (td (@ (class "navigation"))
              ,(nav-form "Next Page" (+ offset limit) (= count limit)))
            (td (@ (class "link"))
              ,(home-link sql))))
        (section (format "Rows ~d to ~d" (+ offset 1) (+ offset count))
          `(table
            (tr ,@(map (lambda (column)
                         `(th ,(stringify column)))
                    (vector->list (sqlite:columns stmt))))
            ,@results))))))

;; Dispatching requests

(define (dispatch)
  (define (string-param name)
    (let ([value (find-param name)])
      (and value (trim-whitespace value))))
  (define (integer-param name min-value)
    (let* ([string-value (find-param name)]
           [number-value (and string-value (string->number string-value))])
      (and string-value
           (if (and (integer? number-value) (>= number-value min-value))
               number-value
               (raise `#(bad-integer-param ,name ,min-value ,number-value))))))
  (let ([sql (string-param "sql")]
        [last-sql (string-param "lastSql")]
        [limit (integer-param "limit" 0)]
        [offset (integer-param "offset" 0)])
    (with-db [db (log-path) SQLITE_OPEN_READONLY]
      (if sql
          (match (catch (do-query db sql limit offset))
            [#(EXIT ,reason) (respond:error reason sql)]
            [,value value])
          (do-home db last-sql)))))

(dispatch)
