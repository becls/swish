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

(include "swish/script-testing.ss")
(import
 (chezscheme)
 (swish erlang)
 (swish log-db)
 (swish mat)
 (swish script-testing)
 (swish testing)
 )

(system-mat m1 ()
  'ok)

(mat try-import ()
  (define (check-library-path lib)
    (match lib
      [(swish . ,_) 'ok]
      [(chezscheme) 'ok]
      [(scheme) 'ok]))
  ;; Check that the naming convention expected by (try-import) in app.ss
  ;; is compatible with (import (swish imports)).
  (for-each check-library-path
    (library-requirements '(swish imports))))

;; make sure we review changes to the set of libraries in swish-core.library
(mat core-library ()
  (define enumerate-libs-expr
    `(lambda (libs)
       (let ([sorted
              (sort string<?
                (map (lambda (lib) (format "~s" lib)) libs))])
         (do ([i 0 (+ i 1)] [ls sorted (cdr ls)] [sep "" "\n"]) ((null? ls))
           (printf "~a~a ~a" sep i (car ls))))))
  (match-let*
   ([,expected
     (let ([os (open-output-string)])
       (parameterize ([current-output-port os])
         ((eval enumerate-libs-expr)
          '((chezscheme)
            (swish app-core)
            (swish erlang)
            (swish errors)
            (swish io)
            (swish osi)
            (swish meta))))
       (split (get-output-string os) #\newline))]
    [`(<os-result> [stdout ,actual])
     ;; load swish-core.library into stock scheme and look at (library-list)
     (run-os-process scheme-exe '("-q")
       (lambda (op)
         (for-each (lambda (x) (pretty-print x op))
           `((load ,(path-combine (prereq-path) "swish-core.library"))
             (,enumerate-libs-expr
              (remp (lambda (x)
                      (or (eq? (car x) 'rnrs)
                          (member x '((chezscheme csv7) (scheme) (scheme csv7)))))
                (library-list)))
             (exit)))
         (flush-output-port op))
       1000
       '())]
    [,@expected actual])
   'ok))

(mat app-exception-handler ()
  (define (try who val)
    (call/cc
     (lambda (k)
       (let ([os (open-output-string)])
         (parameterize ([app:name who]
                        [console-error-port os]
                        [reset-handler
                         (lambda ()
                           (k (get-output-string os)))])
           (app-exception-handler val))
         "failed to invoke reset-handler"))))
  (match-let*
   (["Exception occurred with non-condition value foo\n"
     ;; when app:name is not set, we get default exception handler
     (try #f 'foo)]
    ["bummer: foo.\n"
     ;; when app:name is set, we get app-exception-handler
     (try "bummer" 'foo)]
    ["oops: Invalid datum: \"flobble\".\n"
     (try "oops" '#(invalid-datum "flobble"))]
    ["dang: HTTP handler failed: Exception in cdr: 2 is not a pair.\n"
     ;; nested exit-reason->english with native exception
     (try "dang" `#(http-handler-failed ,(guard (c [else c]) (cdr 2))))]
    ["dang: HTTP handler failed: Exception in cdr: 2 is not a pair.\n"
     ;; nested exit-reason->english with non-condition
     (try "dang" `#(http-handler-failed ,(guard (c [else c]) (cdr 2))))]
    ["rats: HTTP handler failed: flak.\n"
     ;; nested exit-reason->english with caught non-condition
     (try "rats" `#(http-handler-failed ,(catch 'flak)))]
    ["wow: (this happened).\n"
     ;; current-exit-reason->english unchanged
     (try "wow" '(this happened))]
    ["dude: my bad.\n"
     ;; vague current-exit-reason->english
     (parameterize ([current-exit-reason->english (lambda (x) "my bad")])
       (try "dude" '(this happened)))]
    ["wow: Exception occurred with non-condition value (this happened).\n"
     ;; failure in current-exit-reason->english falls back to display-condition
     (parameterize ([current-exit-reason->english
                     (lambda args
                       (raise "internal error"))])
       (try "wow" '(this happened)))]
    ["hey: 123 is not a string.\n"
     ;; native Error condition
     (try "hey" (guard (c [else c]) (string-upcase 123)))]
    ["engine: overheating.\n"
     ;; native Warning condition
     (try "engine"
       (condition (guard (c [else c]) (warning #f "overheating"))))]
    ["my: message.\n"
     ;; native message condition
     (try "my" (make-message-condition "message"))]
    )
   'ok))

(isolate-mat application-shutdown ()
  ;; Make sure app:shutdown doesn't do a (receive).
  ;; If a supervisor malfunctions while we're at the repl,
  ;; we'd like to be able to investigate.
  (match
   (catch
    (test-os-process swish-exe '()
      (format "~{~s\n~}\n"
        '((define repl self)
          (define (bad-sup:start&link)
            (define (init)
              (process-trap-exit #t)
              `#(ok #f))
            (define (terminate reason state)
              (spawn (lambda () (receive (after 10 (send repl 'locking-up)))))
              (receive [disarm 'ok]))
            (define (handle-call msg from state) (match msg))
            (define (handle-cast msg state) (match msg))
            (define (handle-info msg state) (match msg))
            (gen-server:start&link 'bad-sup))
          (app-sup-spec
           `(#(bad-sup ,bad-sup:start&link permanent infinity supervisor)))
          (app:start)
          (app:shutdown 2)
          ;; simulate repl interaction after surprising
          ;; non-exit of app:shutdown
          (receive (after 1000 (osi_exit 3))
            [locking-up 'ok])
          (printf "armed and ready\n")
          (send (whereis 'bad-sup) 'disarm)
          (receive (after 1000 (osi_exit 4)))))
      '("expected non-zero exit")))
   [#(EXIT `(<os-process-failed> [exit-status 2] ,stdout))
    (match-regexps '(seek "armed and ready") stdout)]))
