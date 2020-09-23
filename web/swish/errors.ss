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

;;HTTP/HTML responses
(define (get-page-name)
  (match (get-param "type")
    ["child" "Child Errors"]
    ["gen-server" "Gen-Server Errors"]
    ["supervisor" "Supervisor Errors"]
    [,_ (throw "invalid-type")]))

(define (edit-query last-sql)
  (void))

(define (nice-duration x)
  (let* ([milliseconds (remainder x 1000)]
         [x (quotient x 1000)]
         [seconds (remainder x 60)]
         [x (quotient x 60)]
         [minutes (remainder x 60)]
         [x (quotient x 60)]
         [hours x])
    (if (> hours 0)
        (format "~d:~2,'0d:~2,'0d.~3,'0d" hours minutes seconds milliseconds)
        (format "~2,'0d:~2,'0d.~3,'0d" minutes seconds milliseconds))))

(define (get-message-and-stacks details)
  (match (try (json:string->object details))
    [`(catch ,_) (values "" "")]
    [,obj
     (values
      (json:ref obj 'message "")
      (or (json:ref obj 'preformatted-stack #f)
          (format "~{~a~^~80,,,'-<~>\n~}"
            (map json-stack->string
              (json:ref obj 'stacks '())))))]))

(define (dispatch)
  (let ([limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)]
        [child-sql "SELECT id, name, supervisor, restart_type, type, shutdown, datetime(start/1000,'unixepoch','localtime') as start, duration, killed, reason, details as message, NULL as stacks FROM child WHERE message IS NOT NULL ORDER BY rowid DESC"]
        [gen-sql "SELECT datetime(timestamp/1000,'unixepoch','localtime') as timestamp, pid, name, last_message, state, reason, details as message, NULL as stacks  FROM gen_server_terminating ORDER BY ROWID DESC"]
        [super-sql "SELECT datetime(timestamp/1000,'unixepoch','localtime') as timestamp, supervisor, error_context, reason, child_pid, child_name, details as message, NULL as stacks FROM supervisor_error ORDER BY ROWID DESC"]
        [sql (string-param "sql" params)]
        [child-func  (lambda (id name supervisor restart-type type shutdown start duration killed reason details _ignore)
                       (let-values ([(message stacks) (get-message-and-stacks details)])
                         (list id name supervisor restart-type type shutdown start (nice-duration duration)
                           (if (eqv? killed 1) "Y" "n") reason message stacks)))]
        [gen-func  (lambda (timestamp pid name last-message state reason details _ignore)
                     (let-values ([(message stacks) (get-message-and-stacks details)])
                       (list timestamp pid name last-message state reason message stacks)))]
        [super-func  (lambda (timestamp supervisor error-context reason child-pid child-name details _ignore)
                       (let-values ([(message stacks) (get-message-and-stacks details)])
                         (list timestamp supervisor error-context
                           reason
                           (or child-pid "None")
                           child-name
                           message
                           stacks)))]
        [type (get-param "type")])

    (define (previous-sql-valid? sql)
      (and (string? sql) (not (string=? sql ""))))

    (let ([sql (if (previous-sql-valid? sql)
                   sql
                   (match type
                     ["child" child-sql]
                     ["gen-server" gen-sql]
                     ["supervisor" super-sql]))]
          [func (match type
                  ["child" child-func]
                  ["gen-server" gen-func]
                  ["supervisor" super-func])])

      (with-db [db (log-file) SQLITE_OPEN_READONLY]
        (do-query db sql limit offset type func)))))

(print-graph #t)
(dispatch)
