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

(http:include "display-query.ss")

;; HTTP/HTML responses

(define (get-page-name) "Log DB")

(define (respond:error reason sql)
  (respond
   (match reason
     [#(db-query-failed empty-query ,sql)
      (section "No query given" `(p ,sql))]
     [#(db-query-failed not-a-query ,sql)
      (section "Not a SELECT or EXPLAIN statement" `(p ,sql))]
     [,_ (section "Query failed" `(p ,(exit-reason->english reason)))])))

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
     `(div (@ (class "enter-query"))
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

;; Running a query

(define (check-run-query db sql limit offset)
  (define (check-request)
    (cond
     [(string=? sql "") (throw `#(db-query-failed empty-query ,sql))]
     [(or (starts-with-ci? sql "select ")
          (starts-with-ci? sql "with ")
          (starts-with-ci? sql "explain ")
          (starts-with-ci? sql "select* "))
      (if (and limit offset)
          'ok
          (throw `#(db-query-failed missing-limit-offset ,limit ,offset ,sql)))]
     [else (throw `#(db-query-failed not-a-query ,sql))]))
  (check-request)
  (do-query db sql limit offset "" (lambda x x)))

(define (edit-query last-sql)
  `(form (@ (method "get"))
     (textarea (@ (name "lastSql") (class "hidden")) ,last-sql)
     (button (@ (type "submit"))
       ;; work around vertical alignment issue
       (span (@ (style "height: 1em; display: inline-block;")) "🖉")
       (span " Edit Query"))))

;; Dispatching requests

(define (dispatch)
  (let ([sql (string-param "sql" params)]
        [last-sql (string-param "lastSql" params)]
        [limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)])
    (with-db [db (log-file) SQLITE_OPEN_READONLY]
      (if sql
          (match (try (check-run-query db sql limit offset))
            [`(catch ,reason) (respond:error reason sql)]
            [,value value])
          (do-home db last-sql)))))

(dispatch)
