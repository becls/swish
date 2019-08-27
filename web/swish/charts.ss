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

(define (load-packages names)
  `(script
    ,(format "google.load('visualization', '1', {packages:[~a]})"
       (join (map (lambda (x) (format "'~a'" x)) names) #\,))))

(define sanitize-column-name
  (let ([regexp (re "^json_extract\\(foreign_handles, '\\$.([^']*)'\\)")])
    (lambda (col)
      (match (pregexp-match regexp col)
        [(,_ ,col) col]
        [,col col]))))

(define (make-annotated-time-line div-id columns rows)
  ;; packages: annotatedtimeline
  `(script
    ,(let ([op (open-output-string)]
           [fname (format "draw_~a" div-id)])
       (fprintf op "function ~a() {\n" fname)
       (fprintf op "var data = new google.visualization.DataTable();\n")
       (fprintf op "data.addColumn('datetime', 'Date');\n")
       (for-each
        (lambda (col)
          (fprintf op "data.addColumn('number', '~a');\n" (sanitize-column-name col)))
        (cddr columns))
       (fprintf op "data.addRows([~a]);\n"
         (join
          (map
           (lambda (row)
             (let ([date (vector-ref row 0)]
                   [vals (cddr (vector->list row))])
               (format "[new Date(~a)~{,~:[0~;~:*~a~]~}]" date vals)))
           rows)
          #\,))
       (fprintf op "var chart = new google.visualization.AnnotatedTimeLine(document.getElementById('~a'));\n" div-id)
       (fprintf op "chart.draw(data, {});\n")
       (fprintf op "}\n")
       (fprintf op "google.setOnLoadCallback(~a);\n" fname)
       (get-output-string op))))

(define (fill-gaps rows)
  (define (make-row row f reason)
    (let ([r (make-vector (vector-length row) 0)])
      (vector-set! r 0 (f (vector-ref row 0)))
      (vector-set! r 1 reason)
      r))
  (fold-left
   (lambda (rows row)
     (match (vector-ref row 1)
       ["startup" (cons* (make-row row 1- "startup") row rows)]
       ["shutdown" (cons* row (make-row row 1+ "shutdown") rows)]
       [,r1
        (if (null? rows)
            (cons row '())
            (let* ([next (car rows)]
                   [t1 (vector-ref row 0)]
                   [t2 (vector-ref next 0)]
                   [r2 (vector-ref next 1)])
              (cond
               [(equal? r2 "startup")
                (cons* row (make-row row 1+ "crash") rows)]
               [(and (> (- t2 t1) 305000)
                     (not (and (equal? r1 "suspend") (equal? r2 "resume"))))
                (cons* row (make-row row 1+ "shutdown")
                  (make-row next 1- "startup") rows)]
               [else (cons row rows)])))]))
   '()
   rows))

(define (memory-chart db chart-columns limit)
  (let* ([sql (format "SELECT CAST(timestamp AS INTEGER) as timestamp,reason,~a FROM statistics WHERE (timestamp/1000) > CAST(strftime('%s','now',?) AS INTEGER) ORDER BY timestamp DESC" (join chart-columns #\,))]
         [stmt (sqlite:prepare db sql)])
    (on-exit (sqlite:finalize stmt)
      (make-annotated-time-line "memory_chart"
        (vector->list (sqlite:columns stmt))
        (fill-gaps (sqlite:execute stmt (list limit)))))))

(define standard-charts
  '(("memory" "bytes_allocated" "osi_bytes_used")
    ("time" "cpu" "gc_real")
    ("sqlite" "sqlite_memory" "sqlite_memory_highwater")
    ("bytes_allocated" "bytes_allocated")
    ("osi_bytes_used" "osi_bytes_used")
    ("cpu" "cpu")
    ("real" "real")
    ("bytes" "bytes")
    ("gc_count" "gc_count")
    ("gc_real" "gc_real")
    ("gc_bytes" "gc_bytes")))

(define (self-link chart limit text)
  (link (format "?c=~a&limit=~a"
          (http:percent-encode chart)
          (http:percent-encode limit)) text))

(with-db [db (log-file) SQLITE_OPEN_READONLY]
  (let* ([valid-charts
          (append standard-charts
            (map
             (lambda (row)
               (match row
                 [#(,key)
                  `(,key ,(format "json_extract(foreign_handles, '$.~a')" key))]))
             (execute-sql db
               "select key from (select distinct key from json_each(foreign_handles), (select distinct foreign_handles from statistics)) order by key")))]
         [chart (cond
                 [(find-param "c") => (lambda (x) x)]
                 [else (caar valid-charts)])]
         [cols (cond
                [(assoc chart valid-charts) => cdr]
                [else (raise `#(invalid-chart ,chart))])]
         [limit (or (find-param "limit") "-7 days")])
    (hosted-page "Charts"
      (list
       (js-include "https://www.google.com/jsapi")
       (load-packages '("annotatedtimeline"))
       (memory-chart db cols limit)
       (css-include "css/charts.css"))
      `(div (@ (style "width:700px; height:300px; margin:10px auto 15px;"))
         (h3 ,(format "Viewing chart: ~a" chart)) (div (@ (id "memory_chart") (style "width:700px; height:250px;"))))

      (panel "Build a new Chart"
        (column "standard multi"
          (apply section "Chart Types"
            (map
             (lambda (chart)
               (let ([c (car chart)])
                 `(p ,(self-link c limit c))))
             valid-charts)))
        (column "narrow"
          (apply section "Chart Limits"
            (map
             (lambda (limit text)
               `(p ,(self-link chart limit text)))
             '("-7 days" "-14 days" "-1 month" "-2 months" "-3 months")
             '("1 week" "2 weeks" "1 month" "2 months" "3 months"))))))))
