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

(define (write-comma i op)
  (unless (= i 0)
    (write-char #\, op)))

(define (write-row v op)
  (write-char #\[ op)
  (do ([i 0 (+ i 1)] [n (vector-length v)]) [(= i n)]
    (let ([x (vector-ref v i)])
      (write-comma i op)
      (cond
       [(eq? x #f) (put-string op "null")]
       [(bytevector? x)
        (write-char #\[ op)
        (do ([i 0 (+ i 1)] [n (bytevector-length x)]) [(= i n)]
          (write-comma i op)
          (json:write op (bytevector-u8-ref x i)))
        (write-char #\] op)]
       [else (json:write op x)])))
  (write-char #\] op))

(define (meta op)
  (json:write op
    (json:make-object
     [instance (log-db:get-instance-id)]
     [computer-name (osi_get_hostname)]
     [software-info (software-info)]))
  'ok)

(define (doit op)
  (let ([query (find-param "sql")])
    (unless query (throw "Missing sql"))
    (unless (or (starts-with-ci? query "select ")
                (starts-with-ci? query "with ")
                (starts-with-ci? query "explain "))
      (throw "Query must start with select, with, or explain."))
    (with-db [db (log-file) SQLITE_OPEN_READONLY]
      (let ([stmt (sqlite:prepare db query)])
        (on-exit (sqlite:finalize stmt)
          (put-string op "{\"instance\":\"")
          (put-string op (log-db:get-instance-id))
          (put-string op "\",\"columns\":")
          (write-row (sqlite:columns stmt) op)
          (put-string op ",\"rows\":[")
          (let lp ([i 0])
            (cond
             [(sqlite:step stmt) =>
              (lambda (row)
                (write-comma i op)
                (write-row row op)
                (cond
                 [(< (port-position op) 10000000) (lp (+ i 1))]
                 [else
                  (put-string op "],\"limit\":")
                  (json:write op (+ i 1))
                  (write-char #\} op)
                  'ok]))]
             [else
              (put-string op "]}")
              'ok])))))))

(http:respond conn 200 '(("Access-Control-Allow-Origin" . "*")
                         ("Access-Control-Max-Age" . "86400")
                         ("Content-Type" . "application/json"))
  (let-values ([(op get) (open-bytevector-output-port (make-utf8-transcoder))])
    (match (try (if (find-param "meta")
                    (meta op)
                    (doit op)))
      [ok (get)]
      [`(catch ,reason)
       (json:object->bytevector
        (json:make-object [error (exit-reason->english reason)]))])))
