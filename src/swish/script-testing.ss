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

(library (swish script-testing)
  (export
   <os-process-failed>
   fix-exe
   output-dir
   prereq-path
   script-test
   swish-exe
   test-os-process
   write-test-file
   )
  (import
   (scheme)
   (swish imports)
   (swish profile)
   (swish testing)
   )

  (define (output-dir)
    (path-combine (tmp-dir) "mat-output"))

  (define (write-test-file filename thunk)
    (let* ([full-name (path-combine (output-dir) filename)]
           [op (open-file-to-replace (make-directory-path full-name))])
      (on-exit (close-port op)
        (parameterize ([current-output-port op])
          (thunk)))
      full-name))

  (define (prereq-path)
    (path-combine (base-dir) "build" "mat-prereq" "lib" "swish.x.y.z" "arch"))

  (define exe-suffix
    (if (file-exists? (path-combine (prereq-path) "swish.exe"))
        ".exe"
        ""))

  (define (fix-exe name)
    (format "~a~a" name exe-suffix))

  (define swish-exe
    (get-real-path (path-combine (prereq-path) (fix-exe "swish"))))

  (define-tuple <os-process-failed> command args stdout stderr exit-status)

  (define (test-os-process command args for-stdin patterns)
    (define (write-stdin op)
      (display for-stdin op)
      (newline op)
      (flush-output-port op))
    (match (run-os-process command args write-stdin 10000 '())
      [`(<os-result> ,stdout ,stderr ,exit-status)
       (unless (eqv? exit-status 0)
         (raise
          (<os-process-failed> make
            [command command]
            [args args]
            [stdout stdout]
            [stderr stderr]
            [exit-status exit-status])))
       (match-regexps patterns (append stdout stderr))]))

  (define (script-test script-file args for-stdin patterns)
    (cond
     [(whereis 'profiler)
      (let ([tmp-file (string-append (profile:filename) ".sub-process")])
        (on-exit
         (when (file-exists? tmp-file)
           (profile:merge tmp-file)
           (delete-file tmp-file))
         (test-os-process swish-exe '("-q" "--")
           (format "~{~s\n~}~a"
             `((load ,(path-combine (prereq-path) "lib" "swish" "profile.so"))
               (import (swish profile))
               ;; return void so repl doesn't print #(ok #<process 4 profiler>)
               ;; which could break tests that expect specific patterns
               (begin
                 (profile:prepare)
                 (profile:start #f ,tmp-file #t)
                 ;; when profiling, mat-prereq still builds .so files but in a different directory
                 (library-extensions (append (library-extensions) '((".ss" . ".so"))))
                 (reset-handler (lambda () (exit 1)))
                 (void))
               (on-exit (profile:save)
                 ;; call the (scheme-start) installed by app.ss to
                 ;; mimic initial application startup
                 (apply (scheme-start) ',script-file ',args)))
             for-stdin)
           patterns)))]
     [else (test-os-process swish-exe `(,script-file ,@args) for-stdin patterns)]))

  )
