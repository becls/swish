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

(include "swish/script-testing.ss")
(import
 (chezscheme)
 (swish script-testing)
 (swish testing)
 )

(define swish-build
  (get-real-path (path-combine (prereq-path) "swish-build")))

(define (input-file basename)
  (path-combine (output-dir) (format "~a.ss" basename)))

(define (output-file basename)
  (path-combine (output-dir) (fix-exe basename)))

(define swish-build-src (path-combine "src" "swish" "swish-build"))

(define (swish-build-test args patterns)
  (script-test swish-build-src args "" patterns))

(define (write-example basename exprs)
  (write-test-file (string-append basename ".ss")
    (lambda () (for-each write exprs))))

(define (build-example basename args)
  (swish-build-test
   `("-o" ,(output-file basename)
     ,@args
     ,(path-combine (output-dir) (format "~a.ss" basename)))
   '()))

(define (run-script filename args patterns)
  (test-os-process swish-exe `(,filename ,@args) "" patterns))

(define (run-fat basename args patterns)
  (test-os-process (output-file basename) args "" patterns))

(define (run-thin basename args patterns)
  (test-os-process swish-exe `(,(output-file basename) ,@args) "" patterns))

(isolate-mat installation ()
  ;; We use the relative path to make the coverage report tidier.
  ;; This test makes sure we're still testing what we installed.
  (assert
   (equal?
    (read-file (path-combine (prereq-path) "swish-build"))
    (read-file swish-build-src))))

(isolate-mat help ()
  (swish-build-test '("--help")
    '("Usage:"
      "write output"
      "source filename"))
  (swish-build-test '("-h" "details")
    '("To compile a library"
      "To create a stand-alone application"))
  (swish-build-test '("-h" "examples")
    '("Examples:"
      "swish-build -o foo -b petite foo.ss")))

(define hello-fat
  `((import
     (scheme)
     (swish imports))
    (printf "Hello, World!\n")))

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

(isolate-mat command-line-arguments ()
  ;; command-line thin and fat
  (write-example "cl"
    '((import
       (scheme)
       (swish imports))
      (match (command-line)
        [(,argv0 . ,args)
         (printf "~{~a~^ ~}\n"
           (cons (path-root (path-last argv0)) args))])))
  (build-example "cl" '())
  (run-thin "cl" '() '("^cl$"))
  (run-thin "cl" '("p" "q") '("^cl p q$"))
  (run-thin "cl" '("--verbose" "-q") '("^cl --verbose -q$"))
  (build-example "cl" '("-b" "petite"))
  (run-fat "cl" '() '("^cl$"))
  (run-fat "cl" '("--version") '("^cl --version$"))
  (run-fat "cl" '("--help" "-h") '("^cl --help -h$"))
  ;; command-line-arguments thin and fat
  (write-example "cla"
    '((import
       (scheme)
       (swish imports))
      (printf "~a~{ ~a~}\n" (app:name) (command-line-arguments))))
  (build-example "cla" '())
  (run-thin "cla" '() '("^cla$"))
  (run-thin "cla" '("a" "b" "c") '("^cla a b c$"))
  (run-thin "cla" '("--" "help") '("^cla -- help$"))
  (build-example "cla" '("-b" "petite"))
  (run-fat "cla" '() '("^cla$"))
  (run-fat "cla" '("3" "x" "y" "z") '("^cla 3 x y z$"))
  (run-fat "cla" '("-h" "--help") '("^cla -h --help$")))

(isolate-mat import-nonexistent-lib-fat ()
  ;; Error case
  (write-example "nolib-fat"
    `((import
       (lib-does-not-exist)
       (scheme)
       (swish imports))
      (printf "abc: ~a\n" abc)))
  (match (catch (build-example "nolib-fat" `("-b" "petite")))
    [#(EXIT `(<os-process-failed>
              [stderr ("swish-build: library (lib-does-not-exist) not found.")]))
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
              [stderr ("swish-build: library (lib-does-not-exist) not found.")]))
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
      (printf "abc: ~a\n" abc)))
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
      (printf "abc: ~a\n"
        (top-level-value 'abc (environment '(lib-abc))))))
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
              [stderr ("libsvisbad-fat: library (lib-abc) not found.")]))
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
              [stderr ("libsvisbad-thin: library (lib-abc) not found.")]))
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
      abc                            ; must reference the export
      (printf "abc: ~a\n"
        (top-level-value 'abc (environment '(lib-abc))))))
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
      (printf "abc: ~a\n"
        (top-level-value 'abc (environment '(lib-abc))))))

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
      (printf "abc: ~a\n"
        (top-level-value 'software-product-name (environment '(swish software-info))))))

  ;; Error case: swish library is not visible
  (build-example "rtlibs-swish-fat" `("-b" "petite"))
  (match (catch (run-fat "rtlibs-swish-fat" '()
                  (list (format "abc: ~a" software-product-name))))
    [#(EXIT `(<os-process-failed>
              [stderr ("rtlibs-swish-fat: library (swish software-info) is not visible.")]))
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
     (match-regexps '("application shutdown due to \\(exit foo\\)") stdout)]))

(isolate-mat compile-library ()
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
  (test-os-process scheme-exe `("-q" ,compiled)
    (format "~{~s\n~}" `((import (clib)) (printf "y: ~a x: ~a\n" y x) #!eof))
    `("^y: not x: 123$")))

(isolate-mat missing-libs ()
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
  (test-os-process scheme-exe `("-q" "--import-notify" "--libdirs" ,(output-dir) ,lib-compiled)
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
  (test-os-process scheme-exe `("-q" "--libdirs" ,(output-dir) ,lib-compiled)
    (format "~{~s\n~}" `((reset-handler exit) (console-error-port (console-output-port)) (import (lib))))
    (list "^Exception: library \\(miss\\) not found")))

(isolate-mat claim-exceptions ()
  ;; run one source example three different ways
  (define broken-echo
    (write-example "broken-echo"
      `((printf "~{~a~^ ~}\n" (command-line-arguments))
        (when (member "crash" (command-line-arguments))
          (cdr 17)))))
  ;; run as swish script
  (run-script broken-echo '("1" "2" "345") '("^1 2 345$"))
  (match (catch (run-script broken-echo '("make" "it" "crash") '()))
    [#(EXIT `(<os-process-failed>
              [stdout ("make it crash")]
              [stderr ("broken-echo: 17 is not a pair.")]))
     'ok])
  ;; run as thin app
  (build-example "broken-echo" '())
  (run-thin "broken-echo" '("42" "27") '("^42 27$"))
  (match (catch (run-thin "broken-echo" '("crash" "bang" "boom") '()))
    [#(EXIT `(<os-process-failed>
              [stdout ("crash bang boom")]
              [stderr ("broken-echo: 17 is not a pair.")]))
     'ok])
  (assert (delete-file (output-file "broken-echo")))
  ;; run as fat app
  (build-example "broken-echo" '("-b" "petite"))
  (run-fat "broken-echo" '("2" "4" "6") '("^2 4 6$"))
  (match (catch (run-fat "broken-echo" '("crash" "bang" "boom") '()))
    [#(EXIT `(<os-process-failed>
              [stdout ("crash bang boom")]
              [stderr ("broken-echo: 17 is not a pair.")]))
     'ok])
  )

(isolate-mat hash-bang ()
  ;; run one source example three different ways
  (define howdy
    (write-test-file "howdy.ss"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (for-each pretty-print
          ;; look ma, no import
          '((display (join '("howdy" "folks") #\space))
            (newline))))))
  (set-file-mode howdy #o755)
  ;; run as /usr/bin/env script
  (test-os-process (fix-exe "bash") '()
    (string-append
     "PATH=./build/mat-prereq/lib/swish.x.y.z/arch:${PATH}\n"
     "./data/tmp/mat-output/howdy.ss\n"
     "exit\n")
    '("howdy folks"))
  ;; run as swish script
  (run-script howdy '() '("^howdy folks$"))
  ;; run as thin app
  (build-example "howdy" '())
  (run-thin "howdy" '() '("^howdy folks$"))
  (assert (delete-file (output-file "howdy")))
  ;; run as fat app
  (build-example "howdy" '("-b" "petite"))
  (run-fat "howdy" '() '("^howdy folks$"))
  )

(isolate-mat crash-bang ()
  ;; run one source example three different ways, with nonzero exit code
  (define crash
    (write-test-file "crash.ss"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (pretty-print '(exit 3)))))
  (set-file-mode crash #o755)
  ;; run as /usr/bin/env script
  (test-os-process (fix-exe "bash") '()
    (string-append
     "PATH=./build/mat-prereq/lib/swish.x.y.z/arch:${PATH}\n"
     "./data/tmp/mat-output/crash.ss\n"
     "echo $?\n"
     "exit\n")
    '("3"))
  ;; run as swish script
  (match (catch (run-script crash '() '()))
    [#(EXIT `(<os-process-failed> [exit-status 3]))
     'ok])
  ;; run as thin app
  (build-example "crash" '())
  (match (catch (run-thin "crash" '() '()))
    [#(EXIT `(<os-process-failed> [exit-status 3]))
     'ok])
  (assert (delete-file (output-file "crash")))
  ;; run as fat app
  (build-example "crash" '("-b" "petite"))
  (match (catch (run-fat "crash" '() '()))
    [#(EXIT `(<os-process-failed> [exit-status 3]))
     'ok])
  )

(isolate-mat scheme-start ()
  ;; make sure we've restored swish's scheme-start, especially in fat app case
  (define bounce
    (write-test-file "bounce.ss"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (for-each pretty-print
          '((printf "Hello from ")
            ((scheme-start) "--version"))))))
  (define expected-output '("Hello from Swish Version"))
  (set-file-mode bounce #o755)
  ;; run as /usr/bin/env script
  (test-os-process (fix-exe "bash") '()
    (string-append
     "PATH=./build/mat-prereq/lib/swish.x.y.z/arch:${PATH}\n"
     "./data/tmp/mat-output/bounce.ss\n"
     "exit\n")
    expected-output)
  ;; run as swish script
  (run-script bounce '() expected-output)
  ;; run as thin app
  (build-example "bounce" '())
  (run-thin "bounce" '() expected-output)
  (assert (delete-file (output-file "bounce")))
  ;; run as fat app
  (build-example "bounce" '("-b" "petite" "--rtlib" "swish"))
  (run-fat "bounce" '() expected-output)
  )

(isolate-mat error-reporting ()
  (define bad
    (write-test-file "bad.ss"
      (lambda ()
        (pretty-print '(let x 3)))))
  (define dubious
    (write-test-file "dubious.ss"
      (lambda ()
        (for-each pretty-print
          `((define (foo a b) b)
            (foo 1 2 3 4 5))))))
  (define incomplete
    (write-test-file "incomplete.ss"
      (lambda ()
        (printf "(let ([x 3]"))))
  (define bad-patterns
    (list (format "swish-build: invalid syntax.*~a" (pregexp-quote bad))))
  (define dubious-patterns
    (list
     (format "swish-build: possible incorrect argument count.*~a"
       (pregexp-quote dubious))))
  (define incomplete-patterns
    (list
     (format "swish-build: unexpected end-of-file reading list.*~a"
       (pregexp-quote incomplete))))
  ;; compile-time error thin
  (match (catch (build-example "bad" '()))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps bad-patterns stderr)])
  ;; compile-time error thin
  (match (catch (build-example "bad" '("-b" "petite")))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps bad-patterns stderr)])
  ;; compile-time warning thin
  (match (catch (build-example "dubious" '()))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps dubious-patterns stderr)])
  ;; compile-time warning thin
  (match (catch (build-example "dubious" '("-b" "petite")))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps dubious-patterns stderr)])
  ;; reader error thin
  (match (catch (build-example "incomplete" '()))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps incomplete-patterns stderr)])
  ;; reader error thin
  (match (catch (build-example "incomplete" '("-b" "petite")))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps incomplete-patterns stderr)])
  )

(isolate-mat source-directories ()
  (define file1
    (write-test-file "nested/under/here.ss"
      (lambda ()
        (pretty-print '(define x 123)))))
  (define file2
    (write-test-file "test-sd.ss"
      (lambda ()
        (for-each pretty-print
          `((include "here.ss")
            (pretty-print x))))))
  ;; fails to build if source directories not specified
  (match (catch (build-example "test-sd" '()))
    [#(EXIT
       `(<os-process-failed>
         [stderr
          ("swish-build: failed for here.ss: no such file or directory.")]))
     'ok])
  (build-example "test-sd" `("-s" ,(path-parent file1)))
  (run-thin "test-sd" '() '("123")))
