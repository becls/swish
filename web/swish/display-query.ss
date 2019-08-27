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

;; HTML responses
(define-syntax respond
  (syntax-rules ()
    [(_ c1 c2 ...)
     (hosted-page (get-page-name)
       (list (css-include "css/query-db.css")
         (js-include "js/jquery-1.4.4.min.js")
         (js-include "js/query-db.js"))
       c1 c2 ...)]))

(define (string-param name params)
  (let ([value (http:find-param name params)])
    (and value (trim-whitespace value))))

(define (integer-param name min-value params)
  (let* ([string-value (http:find-param name params)]
         [number-value (and string-value (string->number string-value))])
    (and string-value
         (if (and (integer? number-value) (>= number-value min-value))
             number-value
             (raise `#(bad-integer-param ,name ,min-value ,number-value))))))

(define (stringify x)
  (cond
   [(string? x) x]
   [(symbol? x) (symbol->string x)]
   [else (format "~a" x)]))

;; Running a query
(define (do-query db sql limit offset type f . bindings)
  (define (nav-form where from-offset enabled? new-sql)
    `(form (@ (name "query") (method "get"))
       (textarea (@ (name "sql") (class "hidden")) ,new-sql)
       (input (@ (name "limit") (class "hidden") (value ,(stringify limit))))
       (input (@ (name "offset") (class "hidden") (value ,(stringify from-offset))))
       (input (@ (name "type") (class "hidden") (value ,(stringify type))))
       (button ,(if enabled?
                    '(@ (type "submit"))
                    '(@ (type "submit") (disabled)))
         ,(stringify where))))
  (define (get-results next-row func)
    (let lp ([results '()])
      (match (next-row)
        [#f (reverse results)]
        [,row (lp (cons (func row) results))])))
  (define (row->tr row)
    `(tr ,@(map value->td (vector->list row))))
  (define (value->td v)
    `(td ,(cond
           [(bytevector? v) `(i "Binary data")]
           [(not v) "<null>"]
           [else (stringify v)])))

  (define (remove-limit-offset str)
    (stringify
     (match (pregexp-match (re "^((?:.|\\n)*?) limit \\d+ offset \\d+$") str)
       [(,full ,match) match]
       [(,no-limit) no-limit])))

  (let ([stmt (sqlite:prepare db (format "~a limit ? offset ?" sql))])
    (on-exit (sqlite:finalize stmt)
      (sqlite:bind stmt (append bindings (list limit offset)))
      (let* ([no-limit (remove-limit-offset (sqlite:expanded-sql stmt))]
             [results (get-results (lambda () (sqlite:step stmt)) row->tr)]
             [count (length results)]
             [flag (or (string-param "flag" params) "")])
        (if (= count 0)
            (respond  (section "Query finished" `(p "Query was:") `(p ,no-limit)
                        `(p ,(home-link no-limit))))
            (respond
             `(table
               (tr (@ (style "text-align: center;"))
                 (td (@ (class "navigation"))
                   ,(nav-form "Previous Page" (max 0 (- offset limit)) (> offset 0) no-limit))
                 (td (@ (class "navigation"))
                   (form (@ (id "rowForm") (method "get"))
                     (textarea (@ (name "sql") (class "hidden")) ,no-limit)
                     (input (@ (name "limit") (class "hidden") (value ,(stringify limit))))
                     (input (@ (name "type") (class "hidden") (value ,(stringify type))))
                     (button (@ (id "offsetButton") (type "submit")) "Go to row")
                     (p (input (@ (id "offsetInput") (name "offset") (class "offset"))))))
                 (td (@ (class "navigation"))
                   ,(nav-form "Next Page" (+ offset limit) (= count limit) no-limit))
                 (td (@ (class "link"))
                   ,(home-link no-limit))))
             `(p (@ (style "text-align: center; color: Red; size: +10; font-weight: bold")),flag)
             (section (format "Rows ~d to ~d" (+ offset 1) (+ offset count))
               (match (cons (sqlite:columns stmt) (sqlite:execute stmt '()))
                 [(,cols . ,rows) (data->html-table 1 cols rows f)]))))))))

(define (make-td c r)
  (let* ([text (format "~a" r)]
         [len (string-length text)]
         [text (if (starts-with-ci? text "(a (")
                   r
                   text)])
    (cond
     [(< len 64)
      `(td (@ (class "narrow")) ,text)]
     [(< len 256)
      `(td (@ (class "normal")) ,text)]
     [(< len 512)
      `(td (@ (class "wide")) ,text)]
     [else
      (let ([id (symbol->string (gensym))])
        `(td (@ (class "extra-wide"))
           (div (@ (class ,(format "elide ~a" c)) (word-break "break-all"))
             (input (@ (class "elide") (id ,id) (type "checkbox") (checked "yes")))
             (label (@ (for ,id) (class "elide")) ,text))))])))

(define (data->html-table border columns rows f)
  (let ([columns (vector->list columns)])
    `(div (@ (class "dataCont"))
       (table (@ (class "dataTable"))
         (tbody
          (tr ,@(map (lambda (c) `(th  ,c)) columns))
          ,@(map
             (lambda (row)
               `(tr ,@(map make-td columns (apply f (vector->list row)))))
             rows))))))

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
