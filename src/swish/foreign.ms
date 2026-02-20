;;; Copyright 2019 Beckman Coulter, Inc.
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
 (swish foreign)
 (swish mat)
 (swish pregexp)
 (swish script-testing)
 (swish testing)
 )

(define swish-build-src (path-combine "src" "swish" "swish-build"))

(define (swish-build out src . extra-args)
  (script-test swish-build-src
    `("-vv" "-o" ,out ,src "-L" ,(output-dir) ,@extra-args)
    "" '()))

(define shlib-repl
  (write-test-file "shlib-repl"
    (lambda ()
      (for-each pretty-print
        `((library-directories (cons ,(output-dir) (library-directories)))
          (waiter-prompt-string "")
          (new-cafe))))))

(define (run-script exprs output)
  (script-test shlib-repl '()
    (format "(reset-handler (lambda () (app:shutdown 1)))\n~{~s\n~}#!eof\n" exprs)
    output))

(define (run-repl shlib-imports shlib-test-expr* write-config success)
  (let ([tmp-cd (path-combine (output-dir) "try-repl-tmp-dir")])
    (make-directory tmp-cd)
    (on-exit (delete-tree tmp-cd)
      (if write-config
          (with-output-to-file (path-combine tmp-cd ".config") write-config)
          (delete-file ".config"))
      (parameterize ([current-directory tmp-cd])
        (test-os-process swish-exe `("-q")
          (format "~{~s\n~}"
            `((library-directories '(".."))
              ,shlib-imports
              ,@shlib-test-expr*
              (exit)))
          success)))))

(define shlibtest.so
  (let ([shlib
         (cond
          [windows? "shlibtest.dll"]
          [else
           (case (machine-type)
             [(a6osx arm64osx ta6osx tarm64osx) "shlibtest.dylib"]
             [else "shlibtest.so"])])])
    ;; deliberately left this in src/swish, not in build/mat-prereq/bin,
    ;; so we don't happen to find shared library just because it's near
    ;; one of our binaries or is in the path
    (parameterize ([cd (path-combine (base-dir) "src" "swish")])
      (match (find (lambda (fn) (equal? fn shlib))
               (map car (list-directory (cd))))
        [,fn (guard (string? fn)) (get-real-path fn)]
        [,_
         (errorf #f "could not find ~s; run 'make -C swish mat-prereq' first"
           shlib)]))))

(define (config-file-name app-name)
  (string-append (path-root (path-last app-name)) ".config"))

(define (write-empty-object) (printf "{}\n"))

(define (with-temporary-app-config thunk)
  (parameterize ([app:config (json:make-object)])
    (thunk)))

;; provide absolute path to shared library
(define (write-valid-config)
  (with-temporary-app-config
   (lambda ()
     (provide-shared-object 'shlibtest shlibtest.so)
     (json:write (current-output-port) (app:config) 0))))

(define (get-relative-path base sub)
  (assert (starts-with? sub base))
  (do ([ls '() (cons ".." ls)]
       [sub sub (path-parent sub)])
      ((equal? base sub)
       (apply path-combine ls))))

(define git-toplevel (cd))

;; expects current-output-port to be a file port writing to
;; a path that starts with git-toplevel
(define (write-relative-config)
  (with-temporary-app-config
   (lambda ()
     (provide-shared-object 'shlibtest
       ;; path relative to output-dir
       (path-combine
        (get-relative-path git-toplevel
          (path-parent (port-name (current-output-port))))
        "src" "swish" (path-last shlibtest.so)))
     (json:write (current-output-port) (app:config) 0))))

(define (write-config-shlib-not-found)
  (with-temporary-app-config
   (lambda ()
     ;; file does not exist
     (provide-shared-object 'shlibtest "this-file-does-not-exist")
     (json:write (current-output-port) (app:config) 0))))

(define bogus-shared-object
  (write-test-file "bogus-shared-object"
    (lambda ()
      (display "BOGUS"))))

(define (write-config-bad-shlib)
  (with-temporary-app-config
   (lambda ()
     ;; file exists, but is not a shared library
     (provide-shared-object 'shlibtest bogus-shared-object)
     (json:write (current-output-port) (app:config) 0))))

(define (write-malformed-config)
  (printf "{\"malformed\"\n"))

(define check-error
  (make-process-parameter
   (lambda (failure stderr)
     (match-regexps `(,failure) stderr))))

(define-syntax expect
  (syntax-rules ()
    [(_ failure expr)
     (match (catch expr)
       [#(EXIT `(<os-process-failed> ,stderr))
        ((check-error) failure stderr)])]))

(define (default-exception failure)
  (pregexp-quote
   (format-app-exception "shlib-repl" failure)))

(define (format-app-exception app-name failure)
  (format "~a: ~a" (path-last (path-root app-name)) (exit-reason->english failure)))

(define (app-exception app-name failure)
  (pregexp-quote (format-app-exception app-name failure)))

(define empty-config-file app-exception)

(define no-config-file app-exception)

(define no-base-config-file app-exception)

(define (malformed-config-file app-name)
  (let* ([config-file (path-combine (output-dir) (config-file-name app-name))]
         [reason `#(invalid-config-file ,config-file #(json:unexpected |object value| #!eof 13 "bytevector"))])
    (app-exception app-name (exit-reason->english reason))))

(define (bad-shared-object app-name caught)
  (match caught
    [#(EXIT ,reason)
     (let ([reason `#(cannot-load-shared-object shlibtest ,reason)])
       (format-app-exception app-name reason))]))

(define (shlib-not-found app-name)
  (bad-shared-object app-name
    (catch (get-real-path (path-combine (output-dir) "this-file-does-not-exist")))))

(define (bad-shlib app-name)
  (bad-shared-object app-name
    (catch (load-shared-object (path-combine (output-dir) "bogus-shared-object")))))

(define-tuple <shlib-test> shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure)

(define (setup-shlib-test mode write-base-config write-app-config)
  (define shlib-imports
    (let ([imports '((scheme) (swish imports) (test-rso))])
      (case mode
        [(explicit-init main-init)
         `(import (lib-config) ,@imports)]
        [(no-init config-file base-config-file hook-handler)
         `(import ,@imports)]
        [else (match mode)])))
  (define shlib-test-expr*
    ;; Using a list of exprs here instead of (begin ...) so that we can rely on
    ;; run-script fully expanding and executing each of the expressions in order.
    ;; Otherwise, we'd be relying on the expander's require-invoke collector, and
    ;; that can give different results for (begin e1 e2) vs. (let () e1 e2).
    `((begin
        ,(match mode
           [explicit-init '(void)]
           [no-init '(void)]
           [main-init '(configure)]
           [config-file '(void)]
           [base-config-file '(void)]
           [hook-handler '(void)])
        (call-it pretty-print
          `((before ,before-rso) (after ,after-rso) (sq ,(square 9))))
        (receive (after 10 'ok)))))
  (define test-prog1.ss
    (write-test-file "test-prog1.ss"
      (lambda ()
        (pretty-print shlib-imports)
        (for-each pretty-print shlib-test-expr*))))
  (define test-rso.ss
    (write-test-file "test-rso.ss"
      (lambda ()
        (pretty-print
         `(library (test-rso)
            (export after-rso before-rso call-it square)
            (import (lib-config) (scheme) (swish foreign))
            (define before-rso (foreign-entry? "square"))
            (define _setup_
              ,(case mode
                 [(explicit-init) '(configure)]
                 [(no-init main-init config-file) '(void)]
                 [(base-config-file)
                  ;; reset app:path so require-shared-object uses base config
                  '(let() (import (swish app-core)) (app:path #f))]
                 [(hook-handler) '(void)]
                 [else (match mode)]))
            (define _rso_
              ,(case mode
                 [(explicit-init no-init main-init config-file base-config-file)
                  '(require-shared-object 'shlibtest)]
                 [(hook-handler)
                  '(require-shared-object 'shlibtest
                     (lambda (path key dict)
                       (define (show key)
                         (printf "key: ~a = ~a\n" key (hashtable-ref dict key "NOT FOUND")))
                       (printf "handler called\n")
                       (printf "path = ~a\n" path)
                       (show 'file)
                       (load-shared-object path)))]
                 [else (match mode)]))
            (define after-rso (foreign-entry? "square"))
            (define square
              (and after-rso (foreign-procedure "square" (int) int)))
            (define call-it
              (foreign-procedure "call_it" (ptr ptr) ptr)))))))
  (define lib-config.ss
    (write-test-file "lib-config.ss"
      (lambda ()
        (pretty-print
         `(library (lib-config)
            (export configure)
            (import (scheme) (swish foreign))
            (define (configure)
              (provide-shared-object 'shlibtest ,shlibtest.so)))))))
  (define linked (fix-exe (path-combine (output-dir) "shlib-linked")))
  (define stand-alone (fix-exe (path-combine (output-dir) "shlib-stand-alone")))
  (define stand-alone-rtlib1 (fix-exe (path-combine (output-dir) "shlib-stand-alone-rtlib1")))
  (define stand-alone-rtlib2 (fix-exe (path-combine (output-dir) "shlib-stand-alone-rtlib2")))
  (define rso-rtlib (path-combine (output-dir) "rso-rtlib"))
  (define success
    (list
     (format "~s" "EROFS")
     (pregexp-quote "((before #f) (after #t) (sq 81))")))
  (define failure `#(unknown-shared-object shlibtest))
  (define (do-config app-name writer)
    (define filename (config-file-name app-name))
    (if writer
        (write-test-file filename writer)
        (delete-file (path-combine (output-dir) filename))))
  (define (delete-compiled filename)
    (let ([root (path-combine (output-dir) (path-root filename))])
      (delete-file (string-append root ".so"))
      (delete-file (string-append root ".wpo"))))
  (let ([base-config (path-combine (output-dir) ".config")])
    (if write-base-config
        (with-output-to-file base-config write-base-config 'replace)
        (delete-file base-config)))
  (ensure-dlls)
  (do-config "shlib-repl" write-app-config)
  (do-config linked write-app-config)
  (do-config stand-alone write-app-config)
  (do-config stand-alone-rtlib1 write-app-config)
  (do-config stand-alone-rtlib2 write-app-config)
  (for-each delete-compiled '("test-rso.ss" "lib-config.ss" "test-prog1.ss" "rso-rtlib"))
  (swish-build linked test-prog1.ss)
  (swish-build stand-alone test-prog1.ss "-b" "petite")
  (swish-build stand-alone-rtlib1 test-prog1.ss "-b" "petite" "--rtlib" "swish")
  ;; now build rso-rtlib as a library ...
  (swish-build rso-rtlib test-rso.ss "--library")
  ;; ... and include it as --rtlib in stand-alone-rtlib2
  ;; which should be okay because (test-rso) depends on (swish foreign), so
  ;;  1. wpo will include require-shared-object and friends in the boot file
  ;;  2. boot file will invoke-library on (test-rso) after it does the equivalent
  ;;     work of invoke-library on the inlined (swish foreign)
  (swish-build stand-alone-rtlib2 test-prog1.ss "-b" "petite" "--rtlib" rso-rtlib)
  (<shlib-test> make
    [shlib-imports shlib-imports]
    [shlib-test-expr* shlib-test-expr*]
    [linked linked]
    [stand-alone stand-alone]
    [stand-alone-rtlib1 stand-alone-rtlib1]
    [stand-alone-rtlib2 stand-alone-rtlib2]
    [success success]
    [failure failure]))

;; Calling (configure) explicitly within (test-rso) library should work fine.
(isolate-mat explicit-init ()
  (define init (setup-shlib-test 'explicit-init #f #f))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  ;; run via script
  (run-script (cons shlib-imports shlib-test-expr*) success)
  ;; run from compiled stand-alone application
  (test-os-process stand-alone '() "" success)
  ;; run from compiled stand-alone application that includes swish as rtlib
  (test-os-process stand-alone-rtlib1 '() "" success)
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (test-os-process stand-alone-rtlib2 '() "" success)
  ;; run from compiled linked application
  (test-os-process swish-exe `(,linked) "" success))

;; With no help from the config file and no initialization,
;; (test-rso) can't find the required shared library.
(isolate-mat no-init-empty-config ()
  (define init (setup-shlib-test 'no-init write-empty-object write-empty-object))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  ;; run via script
  (expect (default-exception failure)
    (run-script (cons shlib-imports shlib-test-expr*) '()))
  ;; run from compiled stand-alone application
  (expect (empty-config-file stand-alone failure)
    (test-os-process stand-alone '() "" '()))
  ;; run from compiled stand-alone application that includes swish as rtlib
  (expect (empty-config-file stand-alone-rtlib1 failure)
    (test-os-process stand-alone-rtlib1 '() "" '()))
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (expect (empty-config-file stand-alone-rtlib2 failure)
    (test-os-process stand-alone-rtlib2 '() "" '()))
  ;; run from compiled linked application
  (expect (empty-config-file linked failure)
    (test-os-process swish-exe `(,linked) "" '())))

;; Calling (configure) within the body of the compiled program is too late.
;; It needs to happen before the test-rso library is initialized.
(isolate-mat main-init-empty-config ()
  (define init (setup-shlib-test 'main-init write-empty-object write-empty-object))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  ;; run via script (effectively doing explicit initialization at the repl), so it works
  (run-script (cons shlib-imports shlib-test-expr*) success)
  ;; run from compiled stand-alone application
  (expect (empty-config-file stand-alone failure)
    (test-os-process stand-alone '() "" '()))
  ;; run from compiled stand-alone application that includes swish as rtlib
  (expect (empty-config-file stand-alone-rtlib1 failure)
    (test-os-process stand-alone-rtlib1 '() "" '()))
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (expect (empty-config-file stand-alone-rtlib2 failure)
    (test-os-process stand-alone-rtlib2 '() "" '()))
  ;; run from compiled linked application
  (expect (empty-config-file linked failure)
    (test-os-process swish-exe `(,linked) "" '())))

;; With no config file found and no explicit init,
;; (test-rso) can't find the required shared library.
(isolate-mat config-file-no-config ()
  (define init (setup-shlib-test 'config-file #f #f))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  ;; run via script
  (expect (default-exception failure)
    (run-script (cons shlib-imports shlib-test-expr*) '()))
  ;; run from compiled stand-alone application
  (expect (no-config-file stand-alone failure)
    (test-os-process stand-alone '() "" '()))
  ;; run from compiled stand-alone application that includes swish as rtlib
  (expect (no-config-file stand-alone-rtlib1 failure)
    (test-os-process stand-alone-rtlib1 '() "" '()))
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (expect (no-config-file stand-alone-rtlib2 failure)
    (test-os-process stand-alone-rtlib2 '() "" '()))
  ;; run from compiled linked application
  (expect (no-config-file linked failure)
    (test-os-process swish-exe `(,linked) "" '())))

;; (test-rso) finds the require shared library because
;; application config file exists and provides absolute path
(isolate-mat config-file-app-config ()
  (define init (setup-shlib-test 'config-file #f write-valid-config))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  ;; run via script
  (run-script (cons shlib-imports shlib-test-expr*) success)
  ;; run from compiled stand-alone application
  (test-os-process stand-alone '() "" success)
  ;; run from compiled stand-alone application that includes swish as rtlib
  (test-os-process stand-alone-rtlib1 '() "" success)
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (test-os-process stand-alone-rtlib2 '() "" success)
  ;; run from compiled linked application
  (test-os-process swish-exe `(,linked) "" success)
  ;; run from swish repl
  (run-repl shlib-imports shlib-test-expr* write-valid-config success))

;; (test-rso) finds the require shared library because
;; application config file exists and provides path relative
;; to the application config file
(isolate-mat config-file-app-config-relative ()
  (define init (setup-shlib-test 'config-file #f write-relative-config))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  (define dir (path-combine (output-dir) "temporary-sub-directory"))
  (make-directory dir)
  (on-exit (delete-directory dir)
    (parameterize ([current-directory dir])
      ;; run from a subdirectory that does not contain a config file
      (match (list-directory ".") [() 'ok])
      ;; run via script
      (run-script (cons shlib-imports shlib-test-expr*) success)
      ;; run from compiled stand-alone application
      (test-os-process stand-alone '() "" success)
      ;; run from compiled stand-alone application that includes swish as rtlib
      (test-os-process stand-alone-rtlib1 '() "" success)
      ;; run from compiled stand-alone application that includes test-rso as rtlib
      (test-os-process stand-alone-rtlib2 '() "" success)
      ;; run from compiled linked application
      (test-os-process swish-exe `(,linked) "" success)
      ;; run from swish repl
      (run-repl shlib-imports shlib-test-expr* write-relative-config success))))

;; (test-rso) finds the require shared library via the base config
;; because we explicitly reset app:path and base config provides
;; the location
(isolate-mat config-file-base-config ()
  (define init (setup-shlib-test 'base-config-file write-valid-config #f))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  (parameterize ([current-directory (output-dir)])
    ;; run via script
    (run-script (cons shlib-imports shlib-test-expr*) success)
    ;; run from compiled stand-alone application
    (test-os-process stand-alone '() "" success)
    ;; run from compiled stand-alone application that includes swish as rtlib
    (test-os-process stand-alone-rtlib1 '() "" success)
    ;; run from compiled stand-alone application that includes test-rso as rtlib
    (test-os-process stand-alone-rtlib2 '() "" success)
    ;; run from compiled linked application
    (test-os-process swish-exe `(,linked) "" success))
  (delete-file (path-combine (output-dir) ".config"))
  (parameterize ([current-directory (output-dir)])
    (expect (default-exception failure)
      ;; run via script
      (run-script (cons shlib-imports shlib-test-expr*) '()))
    ;; run from compiled stand-alone application
    (expect (no-base-config-file stand-alone failure)
      (test-os-process stand-alone '() "" '()))
    ;; run from compiled stand-alone application that includes swish as rtlib
    (expect (no-base-config-file stand-alone-rtlib1 failure)
      (test-os-process stand-alone-rtlib1 '() "" '()))
    ;; run from compiled stand-alone application that includes test-rso as rtlib
    (expect (no-base-config-file stand-alone-rtlib2 failure)
      (test-os-process stand-alone-rtlib2 '() "" '()))
    ;; run from compiled linked application
    (expect (no-base-config-file linked failure)
      (test-os-process swish-exe `(,linked) "" '()))))

;; EOF while reading config file
(isolate-mat config-file-malformed-app-config ()
  (define init (setup-shlib-test 'config-file #f write-malformed-config))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  ;; run via script
  (expect (malformed-config-file shlib-repl)
    (run-script (cons shlib-imports shlib-test-expr*) '()))
  ;; run from compiled stand-alone application
  (expect (malformed-config-file stand-alone)
    (test-os-process stand-alone '() "" '()))
  ;; run from compiled stand-alone application that includes swish as rtlib
  (expect (malformed-config-file stand-alone-rtlib1)
    (test-os-process stand-alone-rtlib1 '() "" '()))
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (expect (malformed-config-file stand-alone-rtlib2)
    (test-os-process stand-alone-rtlib2 '() "" '()))
  ;; run from compiled linked application
  (expect (malformed-config-file linked)
    (test-os-process swish-exe `(,linked) "" '())))

;; config file points to non-existent file
(isolate-mat config-file-app-config-shlib-not-found ()
  (define init (setup-shlib-test 'config-file #f write-config-shlib-not-found))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  (check-error
   (lambda (failure stderr)
     (assert (equal? (split failure #\newline) stderr))))
  ;; run via script
  (expect (shlib-not-found shlib-repl)
    (run-script (cons shlib-imports shlib-test-expr*) '()))
  ;; run from compiled stand-alone application
  (expect (shlib-not-found stand-alone)
    (test-os-process stand-alone '() "" '()))
  ;; run from compiled stand-alone application that includes swish as rtlib
  (expect (shlib-not-found stand-alone-rtlib1)
    (test-os-process stand-alone-rtlib1 '() "" '()))
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (expect (shlib-not-found stand-alone-rtlib2)
    (test-os-process stand-alone-rtlib2 '() "" '()))
  ;; run from compiled linked application
  (expect (shlib-not-found linked)
    (test-os-process swish-exe `(,linked) "" '())))

;; config file points to file that exists, but is not a shared library
(isolate-mat config-file-app-config-bad-shlib ()
  (define init (setup-shlib-test 'config-file #f write-config-bad-shlib))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  (define (massage-result err)
    (meta-cond
     [(memq (machine-type) '(a6osx arm64osx ta6osx tarm64osx))
      (match err
        [(,err1 . ,_)
         ;; MacOS dlerror(3) may include unpredictable extra lines after
         ;; "... no suitable image found.  Did find:"
         (match (pregexp-match "^(.*: no suitable image found).*" err1)
           [(,_ ,hit) `(,hit)]
           [#f `(,err1)])]
        [,_ err])]
     [else err]))
  (define (verbose-equal? a b)
    (or (equal? a b)
        (begin
          (pretty-print `(equal? ',a ',b))
          #f)))
  (check-error
   (lambda (failure stderr)
     (assert
      (verbose-equal?
       (massage-result (split failure #\newline))
       (massage-result stderr)))))
  ;; run via script
  (expect (bad-shlib shlib-repl)
    (run-script (cons shlib-imports shlib-test-expr*) '()))
  ;; run from compiled stand-alone application
  (expect (bad-shlib stand-alone)
    (test-os-process stand-alone '() "" '()))
  ;; run from compiled stand-alone application that includes swish as rtlib
  (expect (bad-shlib stand-alone-rtlib1)
    (test-os-process stand-alone-rtlib1 '() "" '()))
  ;; run from compiled stand-alone application that includes test-rso as rtlib
  (expect (bad-shlib stand-alone-rtlib2)
    (test-os-process stand-alone-rtlib2 '() "" '()))
  ;; run from compiled linked application
  (expect (bad-shlib linked)
    (test-os-process swish-exe `(,linked) "" '())))

;; test optional handler argument to require-shared-object
(isolate-mat config-file-hook-handler ()
  (define init (setup-shlib-test 'hook-handler #f write-valid-config))
  (<shlib-test> open init [shlib-imports shlib-test-expr* linked stand-alone stand-alone-rtlib1 stand-alone-rtlib2 success failure])
  (let ([success
         (list*
          "handler called"
          (pregexp-quote (format "path = ~a" shlibtest.so))
          (pregexp-quote (format "key: file = ~a" shlibtest.so))
          success)])
    (parameterize ([current-directory (output-dir)])
      ;; run via script
      (run-script (cons shlib-imports shlib-test-expr*) success)
      ;; run from compiled stand-alone application
      (test-os-process stand-alone '() "" success)
      ;; run from compiled stand-alone application that includes swish as rtlib
      (test-os-process stand-alone-rtlib1 '() "" success)
      ;; run from compiled stand-alone application that includes test-rso as rtlib
      (test-os-process stand-alone-rtlib2 '() "" success)
      ;; run from compiled linked application
      (test-os-process swish-exe `(,linked) "" success))))

(mat bad-args ()
  (match-let*
   ([#(EXIT #(bad-arg provide-shared-object "thing1"))
     (catch (provide-shared-object "thing1" "string"))]
    [#(EXIT #(bad-arg provide-shared-object thing2))
     (catch (provide-shared-object 'symbol 'thing2))]
    [#(EXIT #(bad-arg require-shared-object "thing1"))
     (catch (require-shared-object "thing1"))]
    [#(EXIT #(bad-arg require-shared-object thing2))
     (catch (require-shared-object 'symbol 'thing2))])
   'ok))
