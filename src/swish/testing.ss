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

(library (swish testing)
  (export
   <os-result>
   capture-events
   delete-tree
   gc
   handle-gone?
   isolate-mat
   match-prefix
   match-regexps
   process-alive?
   run-os-process
   sleep-ms
   start-event-mgr
   start-silent-event-mgr
   system-mat
   )
  (import
   (chezscheme)
   (swish app)
   (swish app-io)
   (swish erlang)
   (swish event-mgr)
   (swish gatekeeper)
   (swish gen-server)
   (swish io)
   (swish log-db)
   (swish mat)
   (swish osi)
   (swish pregexp)
   (swish string-utils)
   (swish supervisor)
   (swish watcher)
   )

  (define (sleep-ms t) (receive (after t 'ok)))

  (define (gc)
    (collect (collect-maximum-generation))
    (sleep-ms 10))

  (define (handle-gone? x)
    (and (record? x)
         (not ((csv7:record-field-accessor (record-rtd x) 'handle) x))))

  (define (start-event-mgr)
    (unless (whereis 'event-mgr)
      (let* ([caller self]
             [pid (spawn
                   (lambda ()
                     (event-mgr:start&link)
                     (send caller 'ready)
                     (receive)))])
        (receive (after 1000 (raise 'timeout-starting-event-mgr))
          [ready 'ok]))))

  (define (start-silent-event-mgr)
    (start-event-mgr)
    (event-mgr:set-log-handler (lambda (x) x) (spawn (lambda () (receive))))
    (event-mgr:flush-buffer))

  (define (capture-events)
    (let ([me self])
      (event-mgr:add-handler (lambda (event) (send me event)))))

  (define ($isolate-mat thunk)
    (let* ([pid (spawn thunk)]
           [m (monitor pid)])
      (receive (after 60000
                 (kill pid 'kill)
                 (raise 'timeout))
        [#(DOWN ,@m ,@pid normal) (void)]
        [#(DOWN ,@m ,@pid ,reason) (raise reason)])))

  (define-syntax isolate-mat
    (syntax-rules ()
      [(_ name tags e1 e2 ...)
       (mat name tags ($isolate-mat (lambda () e1 e2 ...)))]))

  (define (match-prefix lines pattern)
    (match lines
      [() (raise `#(pattern-not-found ,pattern))]
      [(,line . ,rest)
       (if (starts-with? line pattern)
           line
           (match-prefix rest pattern))]))

  (define (process-alive? x timeout)
    (let ([m (monitor x)])
      (demonitor m)
      (receive
       (after timeout #t)
       [#(DOWN ,@m ,@x ,_) #f])))

  (define (boot-system)
    (log-file (path-combine (data-dir) "TestLog.db3"))
    (match (init-main-sup)
      [#(ok ,pid) pid]))

  (define (shutdown-system)
    (cond
     [(whereis 'main-sup) =>
      (lambda (pid)
        (monitor pid)
        (receive (after 60000 (raise 'main-sup-still-running))
          [#(DOWN ,_ ,@pid ,_) 'ok]))]))

  (define-syntax system-mat
    (syntax-rules ()
      [(_ name tags e1 e2 ...)
       (mat name tags ($system-mat (lambda () (boot-system) e1 e2 ...)))]))

  (define ($system-mat thunk)
    (let* ([pid (spawn thunk)]
           [m (monitor pid)])
      (on-exit (shutdown-system)
        (receive (after 300000 (kill pid 'shutdown) (raise 'timeout))
          [#(DOWN ,_ ,@pid normal) 'ok]
          [#(DOWN ,_ ,@pid ,reason) (raise reason)]))))

  (define-tuple <os-result> stdout stderr exit-status)

  (define (run-os-process command args write-stdin timeout redirected)
    (let-values
        ([(to-stdin from-stdout from-stderr os-pid)
          (spawn-os-process command args self)])
      (let ([to-stdin (binary->utf8 to-stdin)]
            [from-stdout (binary->utf8 from-stdout)]
            [from-stderr (binary->utf8 from-stderr)])
        (define (spawn-handler pid tag ip)
          (define lines '())
          (define (spawn-drain handle-input)
            (spawn&link
             (lambda ()
               (handle-input)
               (send pid `#(,tag ,os-pid ,(reverse lines))))))
          (define (collect-lines)
            (let ([line (get-line ip)])
              (unless (eof-object? line)
                (set! lines (cons line lines))
                (collect-lines))))
          (define (print)
            (let ([c (read-char ip)])
              (unless (eof-object? c)
                (write-char c)
                (flush-output-port)
                (print))))
          (spawn-drain (if (memq tag redirected) print collect-lines)))
        (spawn-handler self 'stdout from-stdout)
        (spawn-handler self 'stderr from-stderr)
        (on-exit (begin (close-output-port to-stdin)
                        (close-input-port from-stdout)
                        (close-input-port from-stderr))
          (write-stdin to-stdin)
          (receive
           (after timeout
             (osi_kill* os-pid 15)
             (raise
              `#(os-process-timeout
                 #(stdout ,(receive (after 10 '()) [#(stdout ,@os-pid ,lines) lines]))
                 #(stderr ,(receive (after 10 '()) [#(stderr ,@os-pid ,lines) lines])))))
           [#(<process-terminated> ,@os-pid ,exit-status ,_)
            (<os-result> make
              [stdout (receive [#(stdout ,@os-pid ,lines) lines])]
              [stderr (receive [#(stderr ,@os-pid ,lines) lines])]
              [exit-status exit-status])])))))

  (define (match-regexps patterns ls)
    (let check ([patterns patterns] [lines ls])
      (match patterns
        [() lines]
        [(,pattern . ,patterns)
         (let search ([re (pregexp pattern)] [lines lines])
           (match lines
             [() (raise `#(pattern-not-found ,pattern ,ls))]
             [(,line . ,lines)
              (if (pregexp-match re line)
                  (check patterns lines)
                  (search re lines))]))])))

  (define (delete-tree path)
    (if (file-directory? path)
        (or (delete-directory path)
            (begin
              (for-each (lambda (p) (delete-tree (path-combine path p)))
                (directory-list path))
              (receive (after 10 'ok))
              (delete-directory path)))
        (delete-file path)))
  )
