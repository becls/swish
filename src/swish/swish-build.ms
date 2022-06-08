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

(define build-example
  (case-lambda
   [(basename args) (build-example basename args '())]
   [(basename args patterns)
    (swish-build-test
     `("-o" ,(output-file basename)
       ,@args
       ,(path-combine (output-dir) (format "~a.ss" basename)))
     patterns)]))

(define (run-script filename args patterns)
  (test-os-process swish-exe `(,filename ,@args) "" patterns))

(define (run-fat basename args patterns)
  (ensure-dlls)
  (test-os-process (output-file basename) args "" patterns))

(define (run-thin basename args patterns)
  (test-os-process swish-exe `(,(output-file basename) ,@args) "" patterns))

(define (run-bash script args expected-output)
  (test-os-process (fix-exe "bash") '()
    (string-append
     "PATH=./build/mat-prereq/lib/swish.x.y.z/arch:${PATH}\n"
     (format "./data/tmp/mat-output/~a~{ ~a~}\n" script args)
     "exit\n")
    expected-output))

(isolate-mat installation ()
  ;; We use the relative path to make the coverage report tidier.
  ;; This test makes sure we're still testing what we installed.
  (assert
   (equal?
    (read-file (path-combine (prereq-path) "swish-build"))
    (read-file swish-build-src))))

(isolate-mat help ()
  (swish-build-test '()
    '("Usage: swish-build .*"
      "   or: swish-build .*"))
  (swish-build-test '("--help")
    '("Usage:"
      seek
      "write output"
      seek
      "source filename"))
  (swish-build-test '("-h" "details")
    '(seek
      "To compile a library"
      seek
      "To create a stand-alone program"))
  (swish-build-test '("-h" "examples")
    '(seek
      "Examples:"
      seek
      "swish-build -o foo -b petite foo.ss")))

(isolate-mat version ()
  (swish-build-test '("--version")
    '("swish-build Version (?:\\d+\\.){2}\\d+ \\([[:xdigit:]dirty-]+\\)")))

(isolate-mat info ()
  (swish-build-test '("--info")
    (map pregexp-quote
      (split
       (trim-whitespace
        (json:object->string
         (json:make-object
          [software-info (software-info)]
          [uname
           (match (get-uname)
             [`(<uname> ,system ,release ,version ,machine)
              (json:make-object
               [os-machine machine]
               [os-release release]
               [os-system system]
               [os-version version])])])
         0))
       #\newline))))

(isolate-mat required-arguments ()
  (define testfile
    (write-example "triv"
      '((import (scheme)) (printf "okay\n"))))
  (match (catch (swish-build-test `("-o" "gee") '()))
    [#(EXIT `(<os-process-failed> [stderr ("swish-build: requires a single source filename.")]))
     'ok])
  (match (catch (swish-build-test `(,testfile) '()))
    [#(EXIT `(<os-process-failed> [stderr ("swish-build: requires output filename.")]))
     'ok]))

(isolate-mat errors ()
  (define empty (write-example "empty" '()))
  (define non-lib (write-example "non-lib" '((define x 1) (pretty-print x))))
  (define tl-prog (write-example "tl-prog" '((top-level-program (import (scheme)) (newline)))))
  (define mixed
    (write-example "mixed"
      ;; okay to compile a file containing more than one library, but can't mix in a top-level-program
      '((library (A) (export a) (import (scheme)) (define a 1))
        (top-level-program (import (scheme)) (newline)))))
  (define mixed2
    (write-example "mixed2"
      ;; okay to compile a file containing more than one library, but can't mix in a top-level expression
      '((library (A) (export a) (import (scheme)) (define a 1))
        (write "hello"))))
  (define-syntax expect-errors
    (syntax-rules ()
      [(_ [patterns expr] ...)
       (begin
         (match (try expr)
           [`(catch `(<os-process-failed> ,stderr))
            (match-regexps `patterns stderr)])
         ...)]))
  (expect-errors
   [("swish-build: unexpected eof in fasl file.*")
    (swish-build-test `("--library" "-o" "fail" ,empty) '())]
   [("swish-build: expected .*library form, but encountered.*")
    (swish-build-test `("--library" "-o" "fail" ,non-lib) '())]
   [("swish-build: found program while reading library.*")
    (swish-build-test `("--library" "-o" "fail" ,tl-prog) '())]
   [("swish-build: found program while reading library.*")
    (swish-build-test `("--library" "-o" "fail" ,mixed) '())]
   [("swish-build: expected .*library form, but encountered.*")
    (swish-build-test `("--library" "-o" "fail" ,mixed2) '())]
   [("swish-build: invalid context for top-level-program form.*")
    (swish-build-test `("-o" "fail" ,tl-prog) '())]
   [("swish-build: invalid context for top-level-program form.*")
    (swish-build-test `("-o" "fail" ,tl-prog "-c") '())]
   [("swish-build: invalid context for top-level-program form.*")
    (swish-build-test `("-o" "fail" ,tl-prog "-b" "petite") '())]
   )
  ;; empty program or component is okay; silly, but okay
  (match-let*
   ([() (swish-build-test `("-c" "-o" "empty" ,empty) '())]
    [() (swish-build-test `("-o" "empty" ,empty) '())]
    [() (swish-build-test `("-o" "empty" ,empty "-b" "petite") '())])
   'ok)
  )

(define hello-fat
  `((import
     (scheme)
     (swish imports))
    (printf "Hello, World!\n")))

(isolate-mat fat1 ()
  (define wpo-file (string-append (path-root (input-file "fat1")) ".wpo"))
  ;; fat with petite only
  (write-example "fat1" hello-fat)
  (delete-file wpo-file)
  (build-example "fat1" '("-b" "petite"))
  (match (get-stat wpo-file)
    [`(<stat>) (throw "wpo file remains")]
    [,_ 'ok])
  (run-fat "fat1" '() '("Hello, World!"))
  ;; run fat app via PATH search, not via relative or absolute path
  (test-os-process (fix-exe "bash") '()
    (string-append
     "PATH=\"${PWD}/data/tmp/mat-output/:${PATH}\"\n"
     "type -a fat1\n"
     "fat1\n"
     "exit\n")
    '("^fat1 is .*/data/tmp/mat-output/fat1.*"
      "^Hello, World!"))
  )

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

(isolate-mat fat-ext ()
  ;; fat with petite, where output filename contains an extension
  (write-example "fat.extension" hello-fat)
  (build-example "fat.extension" '("-b" "petite"))
  (run-fat "fat.extension" '() '("Hello, World!"))
  (write-example "fat.exe" hello-fat)
  (build-example "fat.exe" '("-b" "petite"))
  (run-fat "fat.exe" '() '("Hello, World!")))

(isolate-mat thin ()
  (write-example "thin"
    `((import
       (scheme)
       (swish imports))
      (printf "Hello, World!\n")))
  (build-example "thin" '())
  (run-thin "thin" '() '("Hello, World!")))

(isolate-mat top-level-program ()
  (define tlp
    (write-example "tlp"
      ;; swish-build wraps body in a top-level-program with default imports
      ;; top-level-program allows us to mix definitions and expressions
      `((printf "before\n")
        (define x 123)
        (printf "after: ~a\n" x))))
  ;; Run as a script. This does not go through swish-build, so it is not wrapped
  ;; as a top-level program. Instead, swish calls load which reads and evaluates
  ;; each source expression from the file in turn. Here that has the same effect
  ;; as running the top-level program.
  (run-script tlp '() '("before" "after: 123"))
  ;; Run as a linked application.
  (build-example "tlp" '())
  (run-thin "tlp" '() '("before" "after: 123"))
  ;; Run as a stand-alone application.
  (build-example "tlp" '("-b" "petite"))
  (run-fat "tlp" '() '("before" "after: 123")))

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
    `("-b" "petite" "-vv"
      "--rtlib" ,(path-combine (output-dir) "libdir" "lib-abc.so")
      "-L" ,(path-combine (output-dir) "libdir"))
    '(seek
      "^making stand-alone program boot file"
      "^-b .*petite.boot"
      "^--rtlib .*swish-core.library"
      "^--rtlib .*lib-abc.so"
      ".*rtlibs-fat.ss"))

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
    `("-vv" "--rtlib" ,(path-combine (output-dir) "libdir" "lib-abc.so")
      "-L" ,(path-combine (output-dir) "libdir"))
    '(seek
      "^making linked program"
      "^--rtlib .*lib-abc.so"
      ".*rtlibs-thin.ss"))

  ;; Ensure source file and object file are not around after
  ;; compile-time.
  (delete-tree (path-combine (output-dir) "libdir"))

  (run-thin "rtlibs-thin" '() '("abc: 123")))

(isolate-mat rtlibs-swish-fat ()
  (write-example "rtlibs-swish-fat"
    `((import
       (scheme)
       (swish imports))
      ;; try to get something from a library that is not in swish-core.library
      (printf "A: ~a\n" (json:object->string '(1 2 3)))
      (printf "B: ~a\n" ((top-level-value 'json:object->string (environment '(swish json))) '(4 5 6)))))

  ;; Error case: swish library is not visible
  (build-example "rtlibs-swish-fat" `("-b" "petite"))
  (match (catch (run-fat "rtlibs-swish-fat" '() '()))
    [#(EXIT `(<os-process-failed>
              [stdout ("A: [1,2,3]")]
              [stderr (,err-msg)]))
     (assert
      (or (string=? err-msg "rtlibs-swish-fat: library (swish json) is not visible.")
          ;; 9.5.3 provides a more precise error message
          (string=? err-msg "rtlibs-swish-fat: attempt to import invisible library (swish json).")))
     'ok])

  ;; Success case
  (build-example "rtlibs-swish-fat"
    `("--rtlib" "swish"
      "-b" "petite"))
  (run-fat "rtlibs-swish-fat" '()
    (map pregexp-quote '("A: [1,2,3]" "B: [4,5,6]"))))

(isolate-mat find-rtlib ()
  (define prog
    (write-example "rtlib-prog"
      `((import (rtlib-l1))
        (printf "abc: ~a\n" abc))))
  (define prog-compiled (output-file "rtlib-prog"))
  (define lib
    (write-example "rtlib-l1"
      `((library (rtlib-l1)
          (export abc)
          (import (scheme))
          (define abc "def")))))
  (define libname "rtlib-l1.library")
  (define libdir (path-combine (output-dir) "find-rtlib" "under" "here"))
  ;; compile library under a subdirectory
  (define lib-compiled (make-directory-path (path-combine libdir libname)))
  (swish-build-test `("-o" ,lib-compiled "--library" ,lib) '())
  ;; delete any secondary artifacts from compiling library
  (delete-file lib)
  (delete-file (pregexp-replace "\\.ss$" lib ".so"))
  (delete-file (pregexp-replace "\\.ss$" lib ".wpo"))
  ;; confirm that we can't build prog without --rtlib
  (match (catch (swish-build-test `("-o" ,prog-compiled ,prog) '()))
    [#(EXIT `(<os-process-failed>
              [stderr ("swish-build: library (rtlib-l1) not found.")]))
     'ok])
  ;; confirm that we can't build prog despite --rtlib without -L
  (let ([err (format "swish-build: cannot resolve --rtlib ~s." libname)])
    (match (catch (swish-build-test `("-o" ,prog-compiled ,prog "--rtlib" ,libname) '()))
      [#(EXIT `(<os-process-failed> [stderr (,@err)]))
       'ok]))
  ;; confirm that --rtlib can be located given a suitable -L
  (swish-build-test `("-o" ,prog-compiled ,prog "--rtlib" ,libname "-L" ,libdir) '())
  (run-thin "rtlib-prog" '() '("abc: def")))

(isolate-mat rtlibs-output ()
  (define rto-lib
    (write-example "rto-lib"
      `((library (rto-lib)
          (export x y)
          (import (scheme))
          (define x 1)
          (define y 2)))))
  (define rto-library (string-append rto-lib ".library"))
  (define rto-main
    (write-example "rto-main"
      `((begin
          (import (rto-lib) (scheme))
          (printf "~a ~a\n" x y)
          (waiter-prompt-string "")
          (new-cafe)))))
  (define rto-out (output-file "rto-main"))
  (swish-build-test `("-o" ,rto-library "--library" ,rto-lib) '())
  ;; thin app omits petite.boot, swish-core.library, and swish.library despite --rtlib swish
  (swish-build-test
   `("-vv" "-o" ,rto-out ,rto-main "--rtlib" ,rto-library "swish")
   `(seek
     ,(format "compiling ~a" (pregexp-quote rto-main))
     "making linked program from:"
     ,(format "--rtlib ~a" (pregexp-quote rto-library))
     ,(format "        ~a" (pregexp-quote rto-main))))
  (test-os-process swish-exe `(,rto-out)
    (string-append
     "(json:object->string '(3 4 5))"
     "(exit)")
    `("1 2" ,(pregexp-quote "[3,4,5]")))
  (delete-file rto-out)
  ;; fat app includes petite.boot, swish-core.library, and swish.library
  (swish-build-test
   `("-vv" "-o" ,rto-out ,rto-main "--rtlib" ,rto-library "swish" "-b" "petite")
   `(seek
     ,(format "compiling ~a" (pregexp-quote rto-main))
     "making stand-alone program boot file .* from:"
     ,(format "-b      .*~a" (pregexp-quote "petite.boot"))
     ,(format "--rtlib ~a"   (pregexp-quote rto-library))
     ,(format "--rtlib .*~a" (pregexp-quote "swish-core.library"))
     ,(format "--rtlib .*~a" (pregexp-quote "swish.library"))
     ,(format "        ~a"   (pregexp-quote rto-main))))
  (test-os-process rto-out '()
    (string-append
     "(import (swish json))"
     "(json:object->string '(7 8))"
     "(exit)")
    `("1 2" ,(pregexp-quote "[7,8]")))
  )

(isolate-mat normal-exit ()
  (define normal-exit (write-example "normal-exit" `((exit))))
  ;; run as swish script
  (run-script normal-exit '() '())
  ;; run as thin app
  (build-example "normal-exit" '())
  (run-thin "normal-exit" '() '())
  ;; run as fat app
  (assert (delete-file (output-file "normal-exit")))
  (build-example "normal-exit" '("-b" "petite"))
  (run-fat "normal-exit" '() '()))

(isolate-mat non-numeric-exit ()
  (define-syntax assert-error
    (syntax-rules ()
      [(_ expr)
       (match (catch expr)
         [#(EXIT `(<os-process-failed>
                   [exit-status 1]
                   [stdout ()]
                   [stderr ,stderr]))
          (match-regexps '(seek "application shutdown due to \\(exit foo\\)") stderr)])]))
  ;; Attempt to call exit with a non-numeric exit code is
  ;; confusing enough that we output a message with the
  ;; console-event-handler.
  (define non-numeric-exit
    (write-example "non-numeric-exit"
      `((import
         (scheme)
         (swish imports))
        (exit 'foo))))
  ;; run as swish script
  (assert-error (run-script non-numeric-exit '() '()))
  ;; run as thin app
  (build-example "non-numeric-exit" '())
  (assert-error (run-thin "non-numeric-exit" '() '()))
  ;; run as fat app
  (assert (delete-file (output-file "non-numeric-exit")))
  (build-example "non-numeric-exit" '("-b" "petite"))
  (assert-error (run-fat "non-numeric-exit" '() '())))

(isolate-mat exit-handler ()
  ;; script may hook the exit handler to perform some cleanup on shutdown
  (define hook-exit
    (write-example "hook-exit"
      `((let ([eh (exit-handler)])
          (exit-handler
           (lambda args
             (printf "cleaning up\n")
             (apply eh args))))
        (printf "starting\n"))))
  (define expected-output '("^starting" "^cleaning up"))
  ;; run as swish script
  (run-script hook-exit '() expected-output)
  ;; run as thin app
  (build-example "hook-exit" '())
  (run-thin "hook-exit" '() expected-output)
  ;; run as fat app
  (assert (delete-file (output-file "hook-exit")))
  (build-example "hook-exit" '("-b" "petite"))
  (run-fat "hook-exit" '() expected-output))

(isolate-mat application-shutdown ()
  (define event "Event: #\\(<child-end> [0-9]+ #<process [0-9]+:[0-9]+> 1 (normal|shutdown)")
  (define expected-output
    `(seek "Date:" "Timestamp:" ,event
       seek "Date:" "Timestamp:" ,event
       seek "Date:" "Timestamp:" ,event))
  (define app-shutdown
    (write-test-file "app-shutdown.ss"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (for-each pretty-print
          `((log-file ":memory:")
            (match (command-line-arguments)
              [("normal") (app:start)]
              [("abnormal")
               (begin
                 (app:start)
                 (kill (whereis 'application) 'shutdown)
                 (receive))]
              [("specific")
               (begin
                 (app:start)
                 (app:shutdown 42)
                 (receive))]))))))
  (set-file-mode app-shutdown #o755)
  ;; run as /usr/bin/env script
  (run-bash "app-shutdown.ss" '("normal") expected-output)
  (match (catch (run-bash "app-shutdown.ss" '("abnormal") '()))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 2]))
     (match-regexps expected-output stderr)])
  (match (catch (run-bash "app-shutdown.ss" '("specific") '()))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 42]))
     (match-regexps expected-output stderr)])
  ;; run as swish script
  (run-script app-shutdown '("normal") expected-output)
  (match (catch (run-script app-shutdown '("abnormal") expected-output))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 2]))
     (match-regexps expected-output stderr)])
  (match (catch (run-script app-shutdown '("specific") expected-output))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 42]))
     (match-regexps expected-output stderr)])
  ;; run as thin app
  (build-example "app-shutdown" '())
  (run-thin "app-shutdown" '("normal") expected-output)
  (match (catch (run-thin "app-shutdown" '("abnormal") '()))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 2]))
     (match-regexps expected-output stderr)])
  (match (catch (run-thin "app-shutdown" '("specific") '()))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 42]))
     (match-regexps expected-output stderr)])
  (assert (delete-file (output-file "app-shutdown")))
  ;; run as fat app
  (build-example "app-shutdown" '("--rtlib" "swish" "-b" "petite"))
  (run-fat "app-shutdown" '("normal") expected-output)
  (match (catch (run-fat "app-shutdown" '("abnormal") '()))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 2]))
     (match-regexps expected-output stderr)])
  (match (catch (run-fat "app-shutdown" '("specific") '()))
    [#(EXIT `(<os-process-failed> ,stderr [exit-status 42]))
     (match-regexps expected-output stderr)]))

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

(isolate-mat compile-component ()
  (define lib1
    (write-test-file "lib1.ss" ;; will need to be rtlib
      (lambda ()
        (pretty-print
         '(library (lib1)
            (export crash-component say)
            (import (scheme))
            (define table
              (begin
                (printf "creating lookup table\n")
                (make-hashtable equal-hash equal?)))
            (define (say x)
              (hashtable-update! table x
                (lambda (prev)
                  (printf "~@[redundant: ~]~s\n" prev x)
                  x)
                #f))
            (define crash-component (make-parameter #f)))))))
  (define lib2
    (write-test-file "lib2.ss"
      (lambda ()
        (pretty-print
         '(library (lib2)
            (export a b)
            (import (scheme))
            (define a "this is a")
            (define b '(this is b)))))))
  (define prog
    (write-test-file "prog.ss"
      (lambda ()
        (for-each pretty-print
          `((import (lib1) (lib2) (scheme))
            (say a)
            (say b)
            (say b)
            (say a)
            (for-each load (command-line-arguments))
            (printf "done\n"))))))
  (define comp1
    (write-test-file "comp1.ss"
      (lambda ()
        (for-each pretty-print
          '((import (lib1) (lib2) (scheme))
            (say "comp.ss")
            (when (crash-component) (raise 'cain))
            (say a)
            (say b))))))
  (define enable-crash
    (write-test-file "crash.ss"
      (lambda ()
        (pretty-print
         `(let ()
            (import (lib1))
            (crash-component #t)
            (printf "expect crash\n"))))))
  (define lib1.library (path-combine (output-dir) "lib1.library"))
  (define comp1.out (path-combine (output-dir) "comp1"))
  (define comp2.out (path-combine (output-dir) "comp2"))
  (define comp3.out (path-combine (output-dir) "comp3"))
  (define prog1 (output-file "prog1"))
  (define prog2 (output-file "prog2"))
  (define prog3 (output-file "prog3"))
  (define (->regexp p) (format "^~a" (pregexp-quote p)))
  (define common-prefix
    '("creating lookup table"
      "\"this is a\""
      "(this is b)"
      "redundant: (this is b)"
      "redundant: \"this is a\""))
  (define duplicate-suffix
    `("creating lookup table" ;; duplicated!
      "\"comp.ss\""
      "\"this is a\""
      "(this is b)"
      "done"))
  (define !duplicated!
    `(,@common-prefix
      ,@duplicate-suffix))

  (swish-build-test `("-o" ,lib1.library "--library" ,lib1) '())
  (swish-build-test `("-o" ,prog1 ,prog "-L" ,(output-dir)) '())
  (swish-build-test `("-o" ,prog2 ,prog "-L" ,(output-dir) "--libs-visible") '())
  (swish-build-test `("-o" ,prog3 ,prog "-L" ,(output-dir) "--rtlib" ,lib1.library) '())
  (swish-build-test `("-c" "-o" ,comp1.out ,comp1 "-L" ,(output-dir)) '())
  (swish-build-test `("-c" "-o" ,comp2.out ,comp1 "-L" ,(output-dir) "--rtlib" ,lib1.library) '())
  (swish-build-test `("-c" "-o" ,comp3.out ,comp1 "-L" ,(output-dir) "--libs-visible") '())

  ;; baseline: no components loaded
  (run-script prog1 '() (map ->regexp `(,@common-prefix "done")))

  ;; table created twice since prog1 and comp1 duplicate the code for lib1.ss
  (run-script prog1 `(,comp1.out) (map ->regexp !duplicated!))

  ;; table created twice since prog2 and comp1 duplicate the code for lib1.ss
  (run-script prog2 `(,comp1.out) (map ->regexp !duplicated!))

  ;; table created twice since prog3 and comp1 duplicate the code for lib1.ss
  (run-script prog3 `(,comp1.out) (map ->regexp !duplicated!))

  ;; comp2 can't find say when loaded by prog1:
  ;;  - comp2 compiled with --rtlib lib1.library, so it doesn't include code for lib1
  ;;  - prog1 was compiled without --libs-visible, so its copy of lib1 isn't visible
  (match (catch (run-script prog1 `(,comp2.out) '()))
    [#(EXIT `(<os-process-failed>
              [stdout ,@common-prefix]
              [stderr (,err-msg)]))
     (assert
       (or (string=? err-msg "prog1: variable say is not bound.")
           ;; 9.5.3 change in library-manager error messages
           (string=? err-msg "prog1: attempt to invoke invisible library (lib1).")))
     'ok])

  ;; table not duplicated when prog2 is loaded and say is visible
  (run-script prog2 `(,comp2.out)
    (map ->regexp
      `(,@common-prefix
        "\"comp.ss\""
        "redundant: \"this is a\""
        "redundant: (this is b)"
        "done")))

  ;; table not duplicated when prog3 is loaded and say is visible
  (run-script prog3 `(,comp2.out)
    (map ->regexp
      `(,@common-prefix
        "\"comp.ss\""
        "redundant: \"this is a\""
        "redundant: (this is b)"
        "done")))

  ;; same, but load comp2 twice
  (run-script prog2 `(,comp2.out ,comp2.out)
    (map ->regexp
      `(,@common-prefix
        "\"comp.ss\""
        "redundant: \"this is a\""
        "redundant: (this is b)"
        "redundant: \"comp.ss\""
        "redundant: \"this is a\""
        "redundant: (this is b)"
        "done")))

  ;; lib1 not visible in prog1, so crash.ss can't arm crash-component
  (match (catch (run-script prog1 `(,enable-crash) '()))
    [#(EXIT `(<os-process-failed>
              [stdout ,@common-prefix]
              [stderr (,err-msg)]))
     (assert
      (or (string=? err-msg "prog1: library (lib1) is not visible.")
          ;; 9.5.3 provides more precise error message
          (string=? err-msg "prog1: attempt to import invisible library (lib1).")))
     'ok])

  ;; expected crash does not happen since comp1 duplicates unarmed (crash-component)
  (run-script prog2 `(,enable-crash ,comp1.out)
    (map ->regexp `(,@common-prefix "expect crash" ,@duplicate-suffix)))

  ;; expected crash does not happen since comp1 duplicates unarmed (crash-component)
  (run-script prog3 `(,enable-crash ,comp1.out)
    (map ->regexp `(,@common-prefix "expect crash" ,@duplicate-suffix)))

  ;; when loading component, report crash via app-exception-handler
  ;; no duplication of lib1, so crash.ss can arm (crash-component)
  (let ([armed-output `(,@common-prefix "expect crash" "\"comp.ss\"")])
    (match (catch (run-script prog2 `(,enable-crash ,comp2.out) '()))
      [#(EXIT `(<os-process-failed>
                [stdout ,@armed-output]
                [stderr ("prog2: cain.")]))
       'ok])
    (match (catch (run-script prog3 `(,enable-crash ,comp2.out) '()))
      [#(EXIT `(<os-process-failed>
                [stdout ,@armed-output]
                [stderr ("prog3: cain.")]))
       'ok]))

  ;; test -c with --libs-visible
  (let ()
    (define comp-common
      '("^creating lookup table"
        "\"comp\\.ss\""
        "\"this is a\""
        "\\(this is b\\))"))
    (define (make-test-input component)
      (format "~{~s\n~}"
        `((reset-handler (lambda () (exit 1)))
          (import-notify #t)
          (load ,component)
          (import (lib1))
          (say 'again)
          (say 'again)
          (exit))))
    (delete-file lib1)
    (delete-file (string-append (path-root lib1) ".so"))
    (delete-file (string-append (path-root lib1) ".wpo"))
    ;; can't import lib1 by loading comp1, since no --libs-visible
    (match (catch (test-os-process swish-exe '("-q") (make-test-input comp1.out) '()))
      [#(EXIT `(<os-process-failed> ,stdout ,stderr))
       (match-regexps comp-common stdout)
       (match-regexps
        ;; 9.5.3 provides more precise error message
        '("^(Exception in visit: library \\(lib1\\) is not visible|Exception: attempt to import invisible library \\(lib1\\)\\.)")
        stderr)])
    ;; can import lib1 by loading comp3, since it was compiled with --libs-visible
    (test-os-process swish-exe '("-q") (make-test-input comp3.out)
      (append comp-common '("again" "redundant: again"))))
  )

(isolate-mat missing-libs ()
  (define source (input-file "miss"))
  (define compiled (string-append (path-root source) ".so"))
  (define wpo (string-append (path-root source) ".wpo"))
  (define lib-compiled (path-combine (output-dir) "lib.so"))
  ;; make tests less sensitive to changes in wrap-text line breaks,
  ;; e.g., if run from directory with long path name
  (with-script-test-parameters ([help-wrap-width (most-positive-fixnum)])
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
    (swish-build-test `("-v" "-L" ,(output-dir) "-o" ,(output-file "thin-ml") ,(input-file "app"))
      `(seek ,(format "^~a$" (pregexp-quote "Added without whole-program optimization: (miss)"))))
    (run-thin "thin-ml" '() '("^got: PDQ Bach$"))
    ;; fat
    (swish-build-test `("-v" "-L" ,(output-dir) "-o" ,(output-file "fat-ml") ,(input-file "app") "-b" "petite")
      `(seek ,(format "^~a$" (pregexp-quote "Added without whole-program optimization: (miss)"))))
    (run-fat "fat-ml" '() '("^got: PDQ Bach$"))
    ;; library
    (swish-build-test
     `("-L" ,(output-dir) "--library" "-vv" "-o" ,lib-compiled ,(input-file "lib"))
     '(seek
       "^These libraries must be provided at run time:"
       ;; could be .so or .sop file depending on whether or not we're profiling
       ".*miss\\.sop?: \\(miss\\)"))
    ;; delete lib.ss, but leave miss.so so we can check that the
    ;; compiled library works so long as (miss) can be found
    (assert (delete-file (input-file "lib")))
    (test-os-process scheme-exe `("-q" "--import-notify" "--libdirs" ,(output-dir) ,lib-compiled)
      (format "~{~s\n~}"
        `((import (lib))
          (pretty-print a)
          #!eof))
      `(seek
        "^import: found object file \".*miss\\.so\""
        seek
        ;; after Chez Scheme commit b911ed47190727b0e1d6a88c0e473d1757accdcd
        ;; we get "visiting" here instead of "loading"
        "^import: (load|visit)ing object file \".*miss\\.so\""
        seek ;; skip new "attempting to 'revisit' previously 'visited' ..." message
        ,(pregexp-quote (format "~s" '("PDQ" "Bach")))))
    ;; now delete miss.so so using compiled library fails
    (assert (delete-file compiled))
    (test-os-process scheme-exe `("-q" "--libdirs" ,(output-dir) ,lib-compiled)
      (format "~{~s\n~}" `((reset-handler exit) (import (lib))))
      (list "^Exception: library \\(miss\\) not found"))))

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
  (run-bash "howdy.ss" '() '("howdy folks"))
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
  (match (catch (run-bash "crash.ss" '() '()))
    [#(EXIT `(<os-process-failed> [exit-status 3]))
     'ok])
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

;; app:path is not the same as osi_get_executable_path
(isolate-mat app-path ()
  (define source
    (write-test-file "test-app-path.ss"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (pretty-print '(printf "~a\n" (app:path))))))
  (define expect-source (list (pregexp-quote source)))
  (define expect-binary (list (pregexp-quote (output-file "test-app-path"))))
  (set-file-mode source #o755)
  ;; run as /usr/bin/env script
  (run-bash "test-app-path.ss" '() expect-source)
  ;; run as swish script
  (run-script source '() expect-source)
  ;; run as thin app
  (build-example "test-app-path" '())
  (run-thin "test-app-path" '() expect-binary)
  (assert (delete-file (output-file "test-app-path")))
  ;; run as fat app
  (build-example "test-app-path" '("-b" "petite"))
  (run-fat "test-app-path" '() expect-binary))

(isolate-mat swish-start ()
  (define buggy-script
    (write-example "buggy-script"
      `((printf "The ~a got~{ ~a~}.\n" (app:name) (command-line-arguments))
        bug)))
  (define set-prompt
    (write-example "set-prompt"
      `((waiter-prompt-string "#")
        (printf "prompted\n"))))
  (define outer-script-input "(* 6 4)\n(swish-start)\n")
  (define outer-script
    (write-example "outer-script"
      `((printf "~a got args:~{ ~a~}.\n" (app:name) (command-line-arguments))
        (swish-start "-q" "--" ,set-prompt) ;; reads outer-script-input
        (parameterize ([console-input-port
                        (open-input-string
                         (format "(swish-start ~s~{ \"~s\"~})" ,buggy-script '(9 10 11)))])
          (swish-start "-q"))
        (printf "back to ~a\n" (app:name))
        unbound-variable)))
  (define bounce
    (write-test-file "bounce.ss"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (pretty-print
         `(begin
            (printf "Hello from ")
            (match-let*
             ([ok (begin (swish-start "--version") 'ok)]
              [ok (begin (swish-start "--help") 'ok)]
              ;; hook console early so (swish-start "-q") repl uses our string input port
              [ok (begin (hook-console-input) 'ok)]
              [5 (parameterize ([console-input-port
                                 (open-input-string
                                  "(pretty-print 12345) (exit (+ 2 3))")])
                   (swish-start "-q"))]
              [()
               (call-with-values
                 (lambda ()
                   (parameterize ([console-input-port (open-input-string "7\n")])
                     (swish-start)))
                 list)]
              [(1 2 3)
               (call-with-values
                 (lambda ()
                   (parameterize ([console-input-port (open-input-string "(exit 1 2 3)\n")])
                     (swish-start "-q")))
                 list)]
              [strategy
               (call/cc
                (lambda (bail)
                  (parameterize ([exit-handler bail]
                                 [reset-handler (lambda () (exit 'strategy))]
                                 [console-input-port (open-input-string ,outer-script-input)])
                    (swish-start ,outer-script "noodle" "soup"))))])
             (printf "~a\n" (app:name))
             (printf "okay\n")))))))
  (define (expected-output who extra-stderr)
    `(;; ==stdin==
      ;; swish-start --version
      "^Hello from Swish Version"
      ;; swish-start --help
      ,(format "^Usage: ~a *.*--verbose.*--version" who)
      seek
      "^ *-q *suppress startup message and prompt string"
      seek
      ;; swish-start -q
      "^12345"
      seek
      ;; swish-start
      "^Swish Version "
      "7"
      "> "
      ;; swish-start file ...
      "^outer-script got args: noodle soup\\."
      ;;   swish-start -q -- arg ...
      "^prompted"
      "^# 24"
      ;;     swish-start
      "^# Swish Version"
      "^>> "
      ;; eof
      "^# "
      ;; swish-start -q
      ;;   swish-start buggy-script
      "^The buggy-script got 9 10 11\\."
      "^back to outer-script"
      ;; app:name
      "^bounce"
      "^okay"
      ;; ==stderr==
      ,@extra-stderr ;; for swish --verbose
      "^buggy-script: variable bug is not bound\\."
      "^outer-script: variable unbound-variable is not bound\\."
      ))
  (test-os-process swish-exe `("--verbose" ,bounce) ""
    (expected-output "swish"
      '("^trying .*swish\\.boot\\.*opened"
        "^version and machine type check"
        seek ;; may try more than one entry in SCHEMEHEAPDIRS
        "^trying .*scheme.boot\\.*opened"
        "^version and machine type check"
        seek ;; may try more than one entry in SCHEMEHEAPDIRS
        "^trying .*petite.boot\\.*opened"
        "^version and machine type check")))
  (set-file-mode bounce #o755)
  ;; run as /usr/bin/env script
  (run-bash "bounce.ss" '() (expected-output "swish" '()))
  ;; run as swish script
  (run-script bounce '() (expected-output "swish" '()))
  ;; run as thin app
  (build-example "bounce" '())
  (run-thin "bounce" '() (expected-output "swish" '()))
  (assert (delete-file (output-file "bounce")))
  ;; run as fat app
  (build-example "bounce" '("--rtlib" "swish" "-b" "petite"))
  (run-fat "bounce" '() (expected-output "bounce" '()))
  )

;; (swish-start <script> arg ...) should return, not exit
(isolate-mat swish-start2 ()
  (define script2
    (write-test-file "script2"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (for-each pretty-print
          `((printf "script2 app:name = ~a, args =~{ ~a~}.\n"
              (app:name) (command-line-arguments))
            27)))))
  (define script1
    (write-test-file "script1"
      (lambda ()
        (printf "#! /usr/bin/env swish\n")
        (for-each pretty-print
          `((printf "script1 before: app:name = ~a, args =~{ ~a~}.\n"
              (app:name) (command-line-arguments))
            (printf "script1 calling script2\n")
            (printf "result = ~s\n" (swish-start ,script2 "apple" "cider"))
            (printf "script1 after: app:name = ~a, args =~{ ~a~}.\n"
              (app:name) (command-line-arguments)))))))
  (define script1.ss (string-append script1 ".ss"))
  (define args '("ginger" "mace" "nutmeg"))
  (define expected-output
    (let ([arg-str (format "~{ ~a~}" args)])
      `(,(format "^script1 before: app:name = script1, args =~a" arg-str)
        "^script1 calling script2"
        "^script2 app:name = script2, args = apple cider\\.$"
        "^result = #<void>$"
        ,(format "^script1 after: app:name = script1, args =~a" arg-str))))
  (set-file-mode script1 #o755)
  (set-file-mode script2 #o755)
  ;; run as /usr/bin/env script
  (run-bash "script1" args expected-output)
  ;; run as thin app
  (rename-path script1 script1.ss)
  (build-example "script1" '())
  (run-thin "script1" args expected-output)
  (assert (delete-file (output-file "script1")))
  ;; run as fat app
  (build-example "script1" '("-b" "petite"))
  (run-fat "script1" args expected-output))

(isolate-mat try-import ()
  ;; combined into one big mat so we build fewer executables
  (define (ref-export var)
    (lambda ()
      (pretty-print `(pretty-print ,var))))
  (define core-script
    (write-test-file "try-import-core-script"
      ;; reference an export that is present in swish-core.library
      ;; *without* explicitly importing swish libraries
      (ref-export 'DIRENT_FILE)))
  (define run-core (list (path-last core-script)))
  (define non-core-script
    (write-test-file "try-import-non-core-script"
      ;; reference display-usage, which is not present in swish-core.library,
      ;; but is provided by (swish cli) which is imported where swish-start is
      ;; defined; again, no explicit import of swish libraries
      (ref-export 'display-usage)))
  (define run-non-core (list (path-last non-core-script)))
  (define importer.ss-full-path
    (write-test-file "try-import.ss"
      (lambda ()
        ;; no explicit swish imports, but swish-start script and repl
        ;; cases do try-import so script we load can work
        (printf "#! /usr/bin/env swish\n")
        (pretty-print
         `(begin ;; cd so we can use relative path to keep Windows happy
            (cd (path-parent (app:path)))
            (apply swish-start (command-line-arguments)))))))
  (define importer.ss (path-last importer.ss-full-path))
  (define importer (path-root importer.ss))
  (define expected-output-core
    (list (format "~s" DIRENT_FILE)))
  (define expected-output-non-core
    (list (pregexp-quote (format "~s" display-usage))))
  (define repl-input-core
    (format "~{~s\n~}"
      '((reset-handler exit)
        DIRENT_FILE
        (printf "okay\n")
        (exit))))
  (define repl-input-non-core
    (format "~{~s\n~}"
      '((reset-handler exit)
        display-usage
        (printf "okay\n")
        (exit))))
  (set-file-mode importer.ss-full-path #o755)
  ;; run as /usr/bin/env script
  ;; core and non-core okay since we have all of swish
  (run-bash importer.ss run-core expected-output-core)
  (run-bash importer.ss run-non-core expected-output-non-core)
  ;; run as swish script
  ;; core and non-core okay since we have all of swish
  (run-script importer.ss-full-path run-core expected-output-core)
  (run-script importer.ss-full-path run-non-core expected-output-non-core)
  ;; run as thin app
  ;; core and non-core okay since we have all of swish
  (build-example importer '())
  (run-thin importer run-core expected-output-core)
  ;; run-core via fat app: should work even without --libs-visible
  ;; because we're going after an export present in core libraries
  (assert (delete-file (output-file importer)))
  (build-example importer '("-b" "petite"))
  (run-fat importer run-core expected-output-core)
  ;; run-non-core via fat app: should fail since libs are not visible
  (match (catch (run-fat importer run-non-core expected-output-non-core))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps
      '("try-import-non-core-script: variable display-usage is not bound")
      stderr)])
  ;; repl-input-core in repl without --libs-visible should pass
  (test-os-process (output-file importer) '("-q") repl-input-core
    expected-output-core)
  ;; repl-input-non-core in repl without --libs-visible should fail
  (test-os-process (output-file importer) '("-q") repl-input-non-core
    `(,(format "~a: variable display-usage is not bound" (path-root importer.ss))))
  ;; rebuild importer with --libs-visible
  (assert (delete-file (output-file importer)))
  (build-example importer '("-b" "petite" "--libs-visible"))
  ;; run-core as fat app: should (still) pass with --libs-visible
  (run-fat importer run-core expected-output-core)
  ;; run-non-core as fat app: should (now) pass due to --libs-visible
  (run-fat importer run-non-core expected-output-non-core)
  ;; repl-input-core in repl with --libs-visible should (still) pass
  (test-os-process (output-file importer) '("-q") repl-input-core
    (append expected-output-core '("^okay")))
  ;; repl-input-non-core in repl with --libs-visible should now pass
  (test-os-process (output-file importer) '("-q") repl-input-non-core
    (append expected-output-non-core '("^okay"))))

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
  (define duplicate-body
    '((define register 'file)
      (pretty-print register)))
  (define duplicate
    (write-test-file "duplicate.ss"
      (lambda ()
        (printf ";; comment to move error to line 2\n")
        (for-each pretty-print duplicate-body))))
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
  (define duplicate-patterns
    ;; make sure we don't have stray #<annotation> showing up in the error message
    (list
     (format "swish-build: multiple definitions for register in body ~a near line 2, char 1 of ~a"
       (pregexp-quote (format "~s" `(top-level-program (import (scheme) (swish imports)) ,@duplicate-body)))
       (pregexp-quote duplicate))))
  (define incomplete-patterns
    (list
     (format "swish-build: unexpected end-of-file reading list.*~a"
       (pregexp-quote incomplete))))
  ;; compile-time error thin
  (match (catch (build-example "bad" '()))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps bad-patterns stderr)])
  ;; compile-time error fat
  (match (catch (build-example "bad" '("-b" "petite")))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps bad-patterns stderr)])
  ;; compile-time error thin
  (match (catch (build-example "duplicate" '()))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps duplicate-patterns stderr)])
  ;; compile-time error fat
  (match (catch (build-example "duplicate" '("-b" "petite")))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps duplicate-patterns stderr)])
  ;; compile-time warning thin
  (match (catch (build-example "dubious" '()))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps dubious-patterns stderr)])
  ;; compile-time warning fat
  (match (catch (build-example "dubious" '("-b" "petite")))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps dubious-patterns stderr)])
  ;; reader error thin
  (match (catch (build-example "incomplete" '()))
    [#(EXIT `(<os-process-failed> ,stderr))
     (match-regexps incomplete-patterns stderr)])
  ;; reader error fat
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

(isolate-mat source-file-switch ()
  (define lib1
    (write-test-file "lib1.ss"
      (lambda ()
        (pretty-print
         '(library (lib1)
            (export what)
            (import (scheme))
            (define what 'day))))))
  (define lib2
    (write-test-file "lib2.ss"
      (lambda ()
        (pretty-print
         '(library (lib2)
            (export now)
            (import (scheme))
            (define now 'joy))))))
  (define file1
    (write-test-file "file1.ss"
      (lambda ()
        (for-each pretty-print
          '((import (lib1) (lib2) (scheme))
            (printf "happy ~s\n" what)
            (printf "great ~s\n" now))))))
  (define lib1.out (string-append lib1 ".out"))
  (define lib2.out (string-append lib2 ".out"))
  (define test (output-file "test"))
  (swish-build-test `("-o" ,lib1.out "--library" ,lib1) '())
  (swish-build-test `("-o" ,lib2.out "--library" ,lib2) '())
  ;; confirm that lib1 is not available via the default
  ;; library-directories we'll be using in this test
  (match-let*
   ([#(EXIT ,reason) (catch (begin (eval '(import (lib1))) "expected import to fail"))]
    [,os (open-output-string)]
    [,_ (display-condition reason os)]
    ["Exception: library (lib1) not found" (get-output-string os)])
   'ok)
  (assert (not (regular-file? "lib2.ss")))
  ;; oops, user meant file1 to be source, but --rtlib consumes all args (up to next option)
  (match (catch (swish-build-test `("-o" ,test "--rtlib" ,lib1.out ,lib2.out ,file1) '()))
    [#(EXIT `(<os-process-failed> [stderr ("swish-build: requires a single source filename.")]))
     'ok])
  ;; we can use -- to prevent --rtlib from grabbing too many arguments
  (swish-build-test `("-v" "-o" ,test "--rtlib" ,lib1.out ,lib2.out "--" ,file1)
    `(,(format "^compiling ~a" (pregexp-quote file1))))
  (run-thin "test" '() '("happy day" "great joy"))
  (delete-file test)
  ;; we still allow just one filename: multiple files after --
  (match (catch (swish-build-test `("-o" ,test "--rtlib" ,lib1.out ,lib2.out "--" ,file1 ,lib1) '()))
    [#(EXIT `(<os-process-failed> [stderr ("swish-build: requires a single source filename.")]))
     'ok])
  ;; we still allow just one filename: both explicit-source and source-file specified
  (match (catch (swish-build-test `("-o" ,test ,lib1 "--rtlib" ,lib1.out ,lib2.out "--" ,file1) '()))
    [#(EXIT `(<os-process-failed> [stderr ("swish-build: requires a single source filename.")]))
     'ok])
  ;; okay if -- is empty
  (swish-build-test `(,file1 "-o" ,test "--rtlib" ,lib1.out ,lib2.out "--") '())
  (run-thin "test" '() '("happy day" "great joy"))
  (delete-file test)
  )

;; Regression test for bug where swish-build deletes the -o output-filename if
;; source-filename extension matches an entry in library-extensions for which
;; output-filename is the corresponding object file, e.g. "foo.ss" ->
;; "foo.so".
(isolate-mat filename-regression ()
  (define lib.ss
    (write-test-file "lib.ss"
      (lambda ()
        (pretty-print
         '(library (lib)
            (export x)
            (import (scheme))
            (define x 11))))))
  (define lib.so (string-append (path-root lib.ss) ".so"))
  (define prog.ss
    (write-test-file "prog.ss"
      (lambda ()
        (pretty-print '(printf "okay\n")))))
  (define prog.so (string-append (path-root prog.ss) ".so"))
  ;; library case
  (delete-file lib.so)
  (swish-build-test `(,lib.ss "-o" ,lib.so "--library") '())
  (assert (file-exists? lib.so))
  ;; component case
  (delete-file prog.so)
  (swish-build-test `(,prog.ss "-o" ,prog.so "-c") '())
  (assert (file-exists? prog.so))
  ;; linked-program case
  (delete-file prog.so)
  (swish-build-test `(,prog.ss "-o" ,prog.so) '())
  (assert (file-exists? prog.so))
  ;; stand-alone program case
  (delete-file prog.so)
  (swish-build-test `(,prog.ss "-o" ,prog.so "-b" "petite") '())
  (assert (file-exists? prog.so)))
