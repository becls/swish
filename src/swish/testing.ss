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
   $run-test-spec
   <os-result>
   <test-spec>
   assert-syntax-error
   capture-events
   delete-tree
   gc
   handle-gone?
   isolate-mat
   match-prefix
   match-regexps
   may-have-mats?
   process-alive?
   run-os-process
   run-test-spec
   scheme-exe
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
   (swish json)
   (swish log-db)
   (swish mat)
   (swish osi)
   (swish pregexp)
   (swish profile)
   (swish software-info)
   (swish string-utils)
   (swish supervisor)
   (swish watcher)
   )

  (profile:exclude)

  (define scheme-exe
    (or (getenv (if (memq (machine-type) '(a6nt i3nt ta6nt ti3nt)) "SCHEME_WIN" "SCHEME"))
        "scheme"))

  (define-syntax assert-syntax-error
    (syntax-rules ()
      [(_ e expected) ($assert-syntax-error 'e expected #f)]
      [(_ e expected match-form) ($assert-syntax-error 'e expected match-form)]))

  (define ($assert-syntax-error e expected match-form)
    (define (matches? msg)
      (cond
       [(string? expected) (string=? msg expected)]
       [(procedure? expected) (expected msg)]
       [(list? expected) (pregexp-match expected msg)]))
    (guard
     (x
      [(and (syntax-violation? x) (matches? (condition-message x)))
       (when match-form
         (match-form e (syntax-violation-form x)))
       'ok])
     (eval e)
     (errorf 'assert-syntax-error "failed to raise syntax error: ~s" e)))

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
    (match ($init-main-sup)
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
    (parameterize ([console-error-port (open-output-string)])
      (let* ([pid (spawn thunk)]
             [m (monitor pid)])
        (on-exit (shutdown-system)
          (receive (after 300000 (kill pid 'shutdown) (raise 'timeout))
            [#(DOWN ,_ ,@pid normal) 'ok]
            [#(DOWN ,_ ,@pid ,reason) (raise reason)])))))

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
                 #(stdout ,(receive (after 100 '()) [#(stdout ,@os-pid ,lines) lines]))
                 #(stderr ,(receive (after 100 '()) [#(stderr ,@os-pid ,lines) lines])))))
           [#(process-terminated ,@os-pid ,exit-status ,_)
            (<os-result> make
              [stdout (receive [#(stdout ,@os-pid ,lines) lines])]
              [stderr (receive [#(stderr ,@os-pid ,lines) lines])]
              [exit-status exit-status])])))))

  (define (match-regexps patterns ls)
    (let check ([patterns patterns] [remaining-lines ls])
      (match patterns
        [() remaining-lines]
        [(seek ,pattern . ,patterns)
         (let search ([re (pregexp pattern)] [lines remaining-lines])
           (match lines
             [() (raise `#(pattern-not-found seek ,pattern ,remaining-lines))]
             [(,line . ,lines)
              (if (pregexp-match re line)
                  (check patterns lines)
                  (search re lines))]))]
        [(,pattern . ,patterns)
         (match remaining-lines
           [(,line . ,lines)
            (guard (pregexp-match pattern line))
            (check patterns lines)]
           [,_ (raise `#(pattern-not-found ,pattern ,remaining-lines))])])))

  (define (delete-tree path)
    (if (file-directory? path)
        (or (delete-directory path)
            (begin
              (for-each (lambda (p) (delete-tree (path-combine path p)))
                (directory-list path))
              (receive (after 10 'ok))
              (delete-directory path)))
        (delete-file path)))

  (define-tuple <test-spec> test-file test-run report-file tests incl-tags excl-tags profile progress src-dirs lib-dirs)

  (define (run-test-spec swish trspec)
    (<test-spec> open trspec [test-file lib-dirs])
    (define (write-stdin op)
      (fprintf op "~{~s\n~}"
        `((library-directories ',lib-dirs)
          (import (swish testing))
          ($run-test-spec ',trspec)))
      (flush-output-port op))
    (and (may-have-mats? test-file)
         (match (run-os-process swish '("-q") write-stdin 'infinity '(stdout stderr))
           [`(<os-result> ,exit-status)
            (case exit-status
              [(0) 'ran]
              [(1) 'fail]
              [(2) 'skip]
              [(3) 'no-tests]
              [else 'fail])])))

  ;; expects to run in a separate OS process.
  ;; expects test-file to contain tests since file has been screened by may-have-mats?.
  (define ($run-test-spec trspec)
    (<test-spec> open trspec [test-file test-run report-file tests incl-tags excl-tags profile progress src-dirs lib-dirs])
    (reset-handler (lambda () (printf "\nTest Failed: ~a\n" test-file) (exit 1)))
    (base-dir (cd))
    (library-directories lib-dirs)
    (source-directories src-dirs)
    (when profile
      (profile:prepare)
      (match (profile:start profile profile #t)
        [#(ok ,_) #t]
        [#(error ,reason) (raise 'profile-failed-to-start)])
      (when (string=? "ms" (path-extension test-file))
        (let () (profile:exclude test-file) (void))))
    (match (catch (load-mats test-file profile))
      [#(EXIT ,reason) (raise reason)]
      [none (exit 3)]
      [ok
       (let ([mo-op (open-file-to-append report-file)])
         (on-exit
          (begin
            (when profile (profile:save))
            (close-port mo-op))
          (match ($run-mats tests test-file incl-tags excl-tags mo-op progress)
            [(,_ (fail ,fail) ,_)
             (guard (> fail 0))
             (exit 1)]
            [(,_ ,_ (skip ,skip))
             (guard (> skip 0))
             (exit 2)]
            [((pass ,pass) (fail 0) (skip 0))
             (if (> pass 0) (exit 0) (exit 3))])))]))

  (define eof-mats-token 'mats)

  (define (may-have-mats? test-file)
    (match (path-extension test-file)
      ["ms" #t]
      [,_
       (match (catch (has-eof-mat? test-file))
         [#(EXIT ,reason) #f]
         [,verdict verdict])]))

  (define (has-eof-mat? test-file)
    (let ([ip (open-file-to-read test-file)])
      (on-exit (close-port ip)
        (let lp ()
          (let ([x (read ip)])
            (if (eof-object? x)
                (eq? (read ip) eof-mats-token)
                (lp)))))))

  (define (load-mats test-file profile)
    (with-sfd-source-offset test-file
      (lambda (ip sfd source-offset)
        (define (do-mats fp)
          (let-values ([(obj fp) (get-datum/annotations ip sfd fp)])
            (cond
             [(eof-object? obj) 'ok]
             [else
              (eval obj)
              (do-mats fp)])))
        (define (do-source fp)
          (let ([deferred (get-datum/annotations-all ip sfd fp)])
            (do-eof-mats (port-position ip) deferred)))
        (define (do-eof-mats fp deferred)
          (call-with-values
            (lambda () (catch (get-datum/annotations ip sfd fp)))
            (case-lambda
             [(err) 'none]
             [(obj fp)
              (match (and (annotation? obj) (annotation-expression obj))
                [,@eof-mats-token
                 (let ([dirs (cons (path-parent test-file) (source-directories))])
                   (define (help-load eval)
                     ;; Save profile so we don't lose counts due to recompiling
                     ;; the deferred expressions if tests call load-this multiple
                     ;; times, e.g., to run #!eof mats at the end of a swish script.
                     (when profile (profile:save))
                     (parameterize ([source-directories dirs])
                       (for-each eval (reverse deferred))
                       (void)))
                   (define (expose-eval to-expose)
                     (lambda (x)
                       (eval
                        (match (annotation-expression x)
                          [(,library ,name ,exports ,imports . ,rest)
                           (guard (eq? 'library (annotation-expression library)))
                           `(,library ,name ,exports ,imports
                              ,@(map (lambda (x) `(export ,x)) to-expose)
                              ,@rest)]
                          [,_ x]))))
                   (set-top-level-value! 'load-this
                     (lambda () (help-load eval)))
                   (set-top-level-value! 'load-this-exposing
                     (lambda (to-expose) (help-load (expose-eval to-expose)))))
                 (do-mats fp)]
                [,_ 'none])])))
        (on-exit (close-port ip)
          (match (path-extension test-file)
            ["ms" (do-mats source-offset)]
            [,_ (do-source source-offset)])))))

  )
