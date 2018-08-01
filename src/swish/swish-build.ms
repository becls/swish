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

(import
 (chezscheme)
 (swish testing)
 )

(define (prereq-path)
  (path-combine (base-dir) "build" "mat-prereq" "lib" "swish.x.y.z" "arch"))

(define (output-dir)
  (path-combine (tmp-dir) "mat-output"))

(define exe-suffix
  (if (file-exists? (path-combine (prereq-path) "swish.exe"))
      ".exe"
      ""))

(define (fix-exe name)
  (format "~a~a" name exe-suffix))

(define swish-exe
  (get-real-path (path-combine (prereq-path) (fix-exe "swish"))))
(define swish-build
  (get-real-path (path-combine (prereq-path) "swish-build")))

(define (input-file basename)
  (path-combine (output-dir) (format "~a.ss" basename)))

(define (output-file basename)
  (path-combine (output-dir) (fix-exe basename)))

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
     (match-regexps patterns stdout)]))

(define (swish-build-test args patterns)
  (test-os-process swish-exe `(,swish-build ,@args) "" patterns))

(define (write-example basename exprs)
  (let ([op (open-file-to-replace
             (make-directory-path (input-file basename)))])
    (on-exit (close-port op)
      (for-each (lambda (expr) (write expr op)) exprs))))

(define (build-example basename args)
  (swish-build-test
   `("-o" ,(output-file basename)
     ,@args
     ,(path-combine (output-dir) (format "~a.ss" basename)))
   '()))

(define (run-fat basename args patterns)
  (test-os-process (output-file basename) args "" patterns))

(define (run-thin basename args patterns)
  (test-os-process swish-exe `(,(output-file basename) ,@args) "" patterns))

(isolate-mat help ()
  (swish-build-test '("--help")
    '("Usage:"
      "write output"
      "source filename")))

(define hello-fat
  `((import
     (scheme)
     (swish imports))
    (swish-start
     (lambda (cmd-line)
       (printf "Hello, World!\n")))))

(isolate-mat fat1 ()
  ;; fat with petite only
  (write-example "fat1" hello-fat)
  (build-example "fat1" '("-b" "petite"))
  (run-fat "fat1" '() '("Hello, World!")))

(isolate-mat fat2 ()
  ;; fat with petite and scheme
  (write-example "fat2" hello-fat)
  (build-example "fat2" '("-b" "petite" "-b" "scheme"))
  (run-fat "fat2" '() '("Hello, World!")))

(isolate-mat fat3 ()
  ;; fat with scheme only
  (write-example "fat3" hello-fat)
  (build-example "fat3" '("-b" "scheme"))
  (run-fat "fat3" '() '("Hello, World!")))

(isolate-mat thin ()
  (write-example "thin"
    `((import
       (scheme)
       (swish imports))
      (printf "Hello, World!\n")))
  (build-example "thin" '())
  (run-thin "thin" '() '("Hello, World!")))

(isolate-mat import-nonexistent-lib-fat ()
  ;; Error case
  (write-example "nolib-fat"
    `((import
       (lib-does-not-exist)
       (scheme)
       (swish imports))
      (swish-start
       (lambda (cmd-line)
         (printf "abc: ~a\n" abc)))))
  (match (catch (build-example "nolib-fat" `("-b" "petite")))
    [#(EXIT `(<os-process-failed>
              [stderr ("Exception: library (lib-does-not-exist) not found")]))
     'ok]))

(isolate-mat import-nonexistent-lib-thin ()
  ;; Error case
  (write-example "nolib-thin"
    `((import
       (lib-does-not-exist)
       (scheme)
       (swish imports))
      (printf "abc: ~a\n" abc)))
  (match (catch (build-example "nolib-thin" `()))
    [#(EXIT `(<os-process-failed>
              [stderr ("Exception: library (lib-does-not-exist) not found")]))
     'ok]))

(isolate-mat libdir-fat ()
  ;; Create a library whose name does not match the subdirectory name,
  ;; and thus we must add the path to library-directories.
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "libdir-fat"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (swish-start
       (lambda (cmd-line)
         (printf "abc: ~a\n" abc)))))
  (build-example "libdir-fat"
    `("-b" "petite"
      "-L" ,(path-combine (output-dir) "libdir")))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (run-fat "libdir-fat" '() '("abc: 123")))

(isolate-mat libdir-thin ()
  ;; Create a library whose name does not match the subdirectory name,
  ;; and thus we must add the path to library-directories.
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "libdir-thin"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (printf "abc: ~a\n" abc)))
  (build-example "libdir-thin"
    `("-L" ,(path-combine (output-dir) "libdir")))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (run-thin "libdir-thin" '() '("abc: 123")))

(isolate-mat libs-visible-fat-bad ()
  ;; Error case where abc was not referenced so lib-abc was not
  ;; included in the boot file. Swish-build should succeed, but the
  ;; resulting executable should fail to locate the library.
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "libsvisbad-fat"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (swish-start
       (lambda (cmd-line)
         (printf "abc: ~a\n"
           (top-level-value 'abc (environment '(lib-abc))))))))
  (build-example "libsvisbad-fat"
    `("-b" "petite"
      "-L" ,(path-combine (output-dir) "libdir")
      "--libs-visible"))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (match (catch (run-fat "libsvisbad-fat" '() '()))
    [#(EXIT `(<os-process-failed>
              [stdout ()]
              [stderr ("Exception: library (lib-abc) not found")]))
     'ok]))

(isolate-mat libs-visible-thin-bad ()
  ;; Error case where abc was not referenced so lib-abc was not
  ;; included in the boot file. Swish-build should succeed, but the
  ;; resulting executable should fail to locate the library.
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "libsvisbad-thin"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (printf "abc: ~a\n"
        (top-level-value 'abc (environment '(lib-abc))))))
  (build-example "libsvisbad-thin"
    `("-L" ,(path-combine (output-dir) "libdir")
      "--libs-visible"))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (match (catch (run-thin "libsvisbad-thin" '() '()))
    [#(EXIT `(<os-process-failed>
              [stdout ()]
              [stderr ("Exception: library (lib-abc) not found")]))
     'ok]))

(isolate-mat libs-visible-fat ()
  ;; Success case
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "libsvis-fat"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (swish-start
       (lambda (cmd-line)
         abc                            ; must reference the export
         (printf "abc: ~a\n"
           (top-level-value 'abc (environment '(lib-abc))))))))
  (build-example "libsvis-fat"
    `("-b" "petite"
      "-L" ,(path-combine (output-dir) "libdir")
      "--libs-visible"))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (run-fat "libsvis-fat" '() '("abc: 123")))

(isolate-mat libs-visible-thin ()
  ;; Success case
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "libsvis-thin"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (begin
        abc                             ; must reference the export
        (printf "abc: ~a\n"
          (top-level-value 'abc (environment '(lib-abc)))))))
  (build-example "libsvis-thin"
    `("-L" ,(path-combine (output-dir) "libdir")
      "--libs-visible"))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (run-thin "libsvis-thin" '() '("abc: 123")))

(isolate-mat rtlibs-fat ()
  ;; Success case where abc was not referenced, yet lib-abc was
  ;; included in the boot file due to explicit --rtlib.
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "rtlibs-fat"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (swish-start
       (lambda (cmd-line)
         (printf "abc: ~a\n"
           (top-level-value 'abc (environment '(lib-abc))))))))

  (parameterize ([compile-file-message #f])
    (compile-file (path-combine (output-dir) "libdir/lib-abc")))
  (build-example "rtlibs-fat"
    `("-b" "petite"
      "-L" ,(path-combine (output-dir) "libdir")
      "--rtlib" ,(path-combine (output-dir) "libdir" "lib-abc.so")))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (run-fat "rtlibs-fat" '() '("abc: 123")))

(isolate-mat rtlibs-thin ()
  ;; Success case where abc was not referenced, yet lib-abc was
  ;; included in the boot file due to explicit --rtlib.
  (write-example "libdir/lib-abc"
    `((library (lib-abc)
        (export abc)
        (import (chezscheme))
        (define abc 123))))
  (write-example "rtlibs-thin"
    `((import
       (lib-abc)
       (scheme)
       (swish imports))
      (printf "abc: ~a\n"
        (top-level-value 'abc (environment '(lib-abc))))))

  (parameterize ([compile-file-message #f])
    (compile-file (path-combine (output-dir) "libdir/lib-abc")))
  (build-example "rtlibs-thin"
    `("-L" ,(path-combine (output-dir) "libdir")
      "--rtlib" ,(path-combine (output-dir) "libdir" "lib-abc.so")))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (run-thin "rtlibs-thin" '() '("abc: 123")))

(isolate-mat rtlibs-swish-fat ()
  (write-example "rtlibs-swish-fat"
    `((import
       (scheme)
       (swish imports))
      (swish-start
       (lambda (cmd-line)
         (printf "abc: ~a\n"
           (top-level-value 'software-product-name (environment '(swish software-info))))))))

  ;; Error case: swish library is not visible
  (build-example "rtlibs-swish-fat" `("-b" "petite"))
  (match (catch (run-fat "rtlibs-swish-fat" '()
                  (list (format "abc: ~a" software-product-name))))
    [#(EXIT `(<os-process-failed>
              [stderr ("Exception in visit: library (swish software-info) is not visible")]))
     'ok])

  ;; Success case
  (build-example "rtlibs-swish-fat"
    `("-b" "petite"
      "--rtlib" "swish"))
  (run-fat "rtlibs-swish-fat" '()
    (list (format "abc: ~a" software-product-name))))

(isolate-mat non-numeric-exit ()
  ;; Attempt to call exit with a non-numeric exit code and is
  ;; confusing enough that we output a message with the
  ;; console-event-handler.
  (write-example "non-numeric-exit"
    `((import
       (scheme)
       (swish imports))
      (exit 'foo)))
  (build-example "non-numeric-exit" '())
  (match (catch (run-thin "non-numeric-exit" '() '()))
    [#(EXIT `(<os-process-failed>
              [exit-status 1]
              [stdout ,stdout]
              [stderr ()]))
     (match-regexps '("app:shutdown due to \\(exit foo\\)") stdout)]))

(isolate-mat compile-library ()
  (define scheme (or (getenv "SCHEME") "scheme"))
  (define source (input-file "clib"))
  (define compiled (string-append (path-root source) ".so"))
  (define wpo (string-append (path-root source) ".wpo"))
  (write-example "clib"
    `((library (clib)
        (export x y)
        (import (scheme))
        (define w "hidden")
        (define x 123)
        (define y "not"))))
  (swish-build-test `("-o" ,compiled "--library" ,source) '())
  ;; expected outputs exist
  (assert (regular-file? compiled))
  (assert (regular-file? wpo))
  ;; see if it worked, making sure (import (clib))
  ;; can't just fall back on the source
  (assert (delete-file source))
  (test-os-process scheme `("-q" ,compiled)
    (format "~{~s\n~}" `((import (clib)) (printf "y: ~a x: ~a\n" y x) #!eof))
    `("^y: not x: 123$")))

(isolate-mat missing-libs ()
  (define scheme (or (getenv "SCHEME") "scheme"))
  (define source (input-file "miss"))
  (define compiled (string-append (path-root source) ".so"))
  (define wpo (string-append (path-root source) ".wpo"))
  (define lib-compiled (path-combine (output-dir) "lib.so"))
  (write-example "miss"
    `((library (miss)
        (export xyz)
        (import (scheme))
        (define xyz "PDQ"))))
  (write-example "lib"
    `((library (lib)
        (export a)
        (import (miss) (scheme))
        (define a (list xyz "Bach")))))
  (write-example "app"
    `((import (lib) (scheme))
      (printf "got:~{ ~a~}\n" a)))
  (swish-build-test `("-o" ,compiled "--library" ,source) '())
  ;; ensure we have only the compiled library, no source or wpo
  (assert (delete-file source))
  (assert (delete-file wpo))
  ;; thin
  (swish-build-test `("-L" ,(output-dir) "-o" ,(output-file "thin-ml") ,(input-file "app"))
    (list (format "^~a$" (pregexp-quote "Added without whole-program optimization: (miss)"))))
  (run-thin "thin-ml" '() '("^got: PDQ Bach$"))
  ;; fat
  (write-example "app"
    `((import (lib) (scheme) (swish imports))
      (swish-start
       (lambda (cmdline)
         (printf "got:~{ ~a~}\n" a)))))
  (swish-build-test `("-L" ,(output-dir) "-o" ,(output-file "fat-ml") ,(input-file "app") "-b" "petite")
    (list (format "^~a$" (pregexp-quote "Added without whole-program optimization: (miss)"))))
  (run-fat "fat-ml" '() '("^got: PDQ Bach$"))
  ;; library
  (swish-build-test
   `("-L" ,(output-dir) "--library" "-o" ,lib-compiled ,(input-file "lib"))
    (list (format "^~a$" (pregexp-quote "These libraries must be provided at run time: (miss)"))))
  ;; delete lib.ss, but leave miss.so so we can check that the
  ;; compiled library works so long as (miss) can be found
  (assert (delete-file (input-file "lib")))
  (test-os-process scheme `("-q" "--import-notify" "--libdirs" ,(output-dir) ,lib-compiled)
    (format "~{~s\n~}"
      `((import (lib))
        (pretty-print a)
        #!eof))
    (list
     "^import: found object file \".*miss\\.so\""
     "^import: loading object file \".*miss\\.so\""
     (pregexp-quote (format "~s" '("PDQ" "Bach")))))
  ;; now delete miss.so so using compiled library fails
  (assert (delete-file compiled))
  (test-os-process scheme `("-q" "--libdirs" ,(output-dir) ,lib-compiled)
    (format "~{~s\n~}" `((reset-handler exit) (console-error-port (console-output-port)) (import (lib))))
    (list "^Exception: library \\(miss\\) not found")))
