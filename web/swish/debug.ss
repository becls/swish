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

(define (uptime)
  (let* ([x (real-time)]
         [milliseconds (remainder x 1000)]
         [x (quotient x 1000)]
         [seconds (remainder x 60)]
         [x (quotient x 60)]
         [minutes (remainder x 60)]
         [x (quotient x 60)]
         [hours (remainder x 24)]
         [x (quotient x 24)]
         [days x])
    (format "~a day~:p, ~a hour~:p, ~a minute~:p, ~a second~:p"
      days hours minutes seconds)))

(define next-id
  (let ([n (erlang:now)])
    (lambda ()
      (set! n (+ n 1))
      (format "~36r" n))))

(define (show-state state spawned)
  (define op (open-output-string))
  (define (format-stacks k*)
    (define (cons-formatted k rest)
      (dump-stack k op 'default)
      (cons (get-output-string op) rest))
    (fold-right cons-formatted '() k*))
  (match state
    [#(exited ,reason)
     (let ([why `(begin "\"" ,(exit-reason->english reason) "\"")])
       `(begin
          "exited with reason "
          ,(match (format-stacks (exit-reason->stacks reason))
             [() `(begin ,why ,spawned)]
             [,k*
              (let ([id (next-id)])
                `(begin
                   (input (@ (id ,id) (type "checkbox") (class "collapsoid")))
                   (label (@ (for ,id) (class "reason")) ,why)
                   (span ,spawned)
                   (label (@ (for ,id) (class "stackdump"))
                     ,@(map
                        (let ([checked (unless (null? (cdr k*)) '(checked "true"))])
                          (lambda (k)
                            (let ([sid (next-id)])
                              `(begin
                                 (input (@ (id ,sid) (type "checkbox") (class "stack") ,checked))
                                 (label (@ (for ,sid) (class "stack")) (span) ,k)))))
                        k*))))])))]
    [,_
     (print-process-state state op)
     (display-string spawned op)
     (get-output-string op)]))

(define (process->html p)
  (with-process-details p
    (lambda (id name spawned state)
      `(begin
         ,(format "~6d: ~@[~a ~]" id name)
         ,(show-state state (format ", spawned ~a\n" spawned))))))

(when (find-param "collect")
  (collect (collect-maximum-generation)))

(print-graph #t)

;; Allow some control over the print-bindings aspect of print-foreign-handles.
(print-length
 (cond
  [(http:find-param 'print-length params) => string->number]
  [else #f]))

(hosted-page "Debug" '()
  `(style
    "input.collapsoid { display: none; }"
    "label.reason { display: inline; color: blue; }"
    "input.collapsoid:checked + label.reason + span + label.stackdump { display: block; margin-left: 5em; }"
    "input.collapsoid + label.reason + span + label.stackdump { display: none; }"
    "label.stack { display: flex; }"
    "input.stack { display: none; }"
    "input.stack + label span { min-width: 1em; display: inline-block; }"
    "input.stack + label span::before { content: \"▼\"; }"
    "input.stack:checked + label span::before { content: \"▶\"; }"
    "input.stack:checked + label.stack { text-overflow: ellipsis; white-space: pre; max-height: 1.25em; line-height: 1.25; overflow: hidden; }"
    )
  `(pre
    ,(let-values ([(op get) (open-string-output-port)])
       (let ([now (current-time)])
         (fprintf op "Date: ~a [~d]\n"
           (format-rfc2822 (time-utc->date now))
           (+ (* (time-second now) 1000)
              (quotient (time-nanosecond now) 1000000))))
       (fprintf op "Uptime: ~a\n" (uptime))
       (newline op)
       (let ([free (osi_get_free_memory)]
             [total (osi_get_total_memory)])
         (fprintf op "  Free memory: ~16:D (~,1f%)\n" free (* 100 (/ free total)))
         (fprintf op "  Phys memory: ~16:D\n" total))
       (newline op)
       (let ([current (current-memory-bytes)]
             [occupied (bytes-allocated)])
         (fprintf op "  Scheme bytes: ~15:D / ~:D (~,1f% occupied)\n"
           occupied current
           (* 100 (/ occupied current))))
       (fprintf op "       C bytes: ~15:D\n" (osi_get_bytes_used))
       (match (osi_get_sqlite_status* SQLITE_STATUS_MEMORY_USED #f)
         [#(,current ,highwater)
          (fprintf op "   SQLite size: ~15:D\n" current)
          (fprintf op "   SQLite high: ~15:D\n" highwater)]
         [,err
          (fprintf op "SQLite status error: ~a\n" err)])
       (newline op)
       (display-statistics op)
       (newline op)
       (get))
    (begin
      "Processes:\n"
      ,@(ps-fold-left > '()
          (lambda (rest p)
            (cons (process->html p) rest))))
    ,(let-values ([(op get) (open-string-output-port)])
       (print-foreign-handles op)
       (get))))
