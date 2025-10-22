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
 (swish mat)
 (swish profile)
 (swish script-testing)
 (swish testing)
 )

(define swish-test-src (path-combine "src" "swish" "swish-test"))

(define (swish-test . args)
  (let ([os (open-output-string)])
    (for-each (lambda (arg) (display arg os)) args)
    ;; not using filenames that contain spaces
    (remove "" (split (get-output-string os) #\space))))

(define (zero-exit args expected)
  (script-test swish-test-src args "" expected))

(define (nonzero-exit args expected-stdout . expected-stderr)
  (match-let*
   ([#(EXIT `(<os-process-failed> ,stdout ,stderr ,exit-status))
     (catch (zero-exit args '()))]
    ;; we get a non-zero exit code if any test fails
    ;; or if we skip tests that were otherwise selected
    [#f (= exit-status 0)]
    [,_ (match-regexps expected-stdout stdout)]
    [,_ (match-regexps expected-stderr stderr)])
   'ok))

(define (path-relative path)
  (substring path (+ 1 (string-length (cd))) (string-length path)))

(isolate-mat installation ()
  ;; We use the relative path to make the coverage report tidier.
  ;; This test makes sure we're still testing what we installed.
  (assert
   (equal?
    (read-file (path-combine (prereq-path) "swish-test"))
    (read-file swish-test-src))))

(isolate-mat help ()
  (zero-exit (swish-test)
    '("Usage:"))
  (zero-exit (swish-test "--help")
    '("Usage:"
      seek
      "<spec> .* directory .* suite"
      seek
      "progress <mode>"))
  (zero-exit (swish-test "-h all")
    '("Usage:"
      seek
      "<spec> .* directory .* suite"
      seek
      "progress <mode>"
      seek
      "By default,"
      seek
      "Examples:"))
  (nonzero-exit (swish-test "--help me-obi-wan")
    '()
    "swish-test: unrecognized help section: me-obi-wan")
  )

(isolate-mat version ()
  (zero-exit (swish-test "--version")
    '("swish-test Version (?:\\d+\\.){2}\\d+(?:-\\d+)?(?:-dirty)? \\([[:xdigit:]dirty-]+\\)")))

(isolate-mat check-options ()
  (nonzero-exit (swish-test "--progress pilgrim")
    '() "invalid --progress pilgrim")
  (nonzero-exit (swish-test "--coverage foo")
    '() "--coverage requires --load-profile or --save-profile")
  ;; check for required options before complaining about missing file
  (nonzero-exit (swish-test "--load-profile file-that-does-not-exist")
    '() "--load-profile requires --save-profile or --coverage")
  (nonzero-exit (swish-test "--report foo.html")
    '()
    (pregexp-quote
     "--report requires --harvest or --load-results \
      or a set of tests to run (<file> ...)"))
  (nonzero-exit (swish-test "--harvest . --progress none .")
    '() "--harvest conflicts with <file> ...")
  (nonzero-exit (swish-test "--load-results foo.json .")
    '() "--load-results conflicts with <file> ...")
  (nonzero-exit (swish-test "--rerun skip --load-results foo.json .")
    '() "--rerun conflicts with --load-results")
  (nonzero-exit (swish-test "--rerun pass --harvest .")
    '() "--rerun conflicts with --harvest")
  (nonzero-exit (swish-test "--rerun sitcom .")
    '() "invalid --rerun type sitcom")
  ;; using \t as separator instead of space since swish-test helper
  ;; splits on space
  (nonzero-exit (swish-test "--rerun (pass\tfail\tsurprise) .")
    '() "invalid --rerun type surprise")
  (nonzero-exit (swish-test "--test-run amok")
    '() "invalid --test-run UUID amok"))

(isolate-mat load-this ()
  (write-test-file "test-load-this.ss"
    (lambda ()
      (display "#!chezscheme\n")
      (for-each pretty-print
        '((define a 1)
          (define-syntax b (identifier-syntax 2))))
      (display "#!eof mats\n")
      (for-each pretty-print
        '((import (swish mat) (swish testing))
          (load-this)
          (mat t1 () (assert (= a 1)))
          (mat t2 () (assert (= b 2)))))))
  (script-test swish-test-src
    `("--progress" "test"
      ,(path-combine (output-dir) "test-load-this.ss"))
    ""
    '(seek "Tests run: 2   Pass: 2   Fail: 0   Skip: 0")))

(isolate-mat load-this-exposing ()
  (write-test-file "test-exposing.ss"
    ;; - explicitly exported variable
    ;; - exposed variable
    ;; - exposed macro
    ;; - no access to un-exported, un-exposed variable
    (lambda ()
      (display "#!chezscheme\n")
      (pretty-print
       '(library (test-exposing)
          (export a)
          (import (scheme))
          (define a 123)
          (define b 404)
          (define-syntax c (identifier-syntax 9000))
          (define d 'enied)
          (define-syntax e (identifier-syntax "vaded"))
          "non-definition form"))
      (display "#!eof mats\n")
      (for-each pretty-print
        '((import (swish mat) (swish testing))
          (load-this-exposing '(b c))
          (import (test-exposing))
          (mat exported () (assert (= a 123)))
          (mat exposed-variable () (assert (= b 404)))
          (mat exposed-macro () (assert (= c 9000)))
          (mat invisible-variable ()
            (match-let*
             ([#(EXIT ,reason) (catch d)]
              ["Exception: variable d is not bound." (exit-reason->english reason)])
             'ok))
          (mat invisible-macro ()
            (match-let*
             ([#(EXIT ,reason) (catch e)]
              ["Exception: variable e is not bound." (exit-reason->english reason)])
             'ok))))))
  (write-test-file "test-reexpose.ss"
    ;; - load-this-exposing adds export for variable already exported
    (lambda ()
      (display "#!chezscheme\n")
      (pretty-print
       '(library (test-exposing)
          (export a)
          (import (scheme))
          (define a 123)
          (define b 404)
          "non-definition form"))
      (display "#!eof mats\n")
      (for-each pretty-print
        '((import (swish mat) (swish testing))
          ;; re-expose a, which is already exported
          (load-this-exposing '(a b))
          (import (test-exposing))
          (mat exported () (= a 123))
          (mat exposed-variable () (= b 404))))))
  (script-test swish-test-src
    `("--progress" "test"
      ,(path-combine (output-dir) "test-exposing.ss")
      ,(path-combine (output-dir) "test-reexpose.ss"))
    ""
    '(seek
      "Tests run: 5   Pass: 5   Fail: 0   Skip: 0"
      seek
      "Tests run: 2   Pass: 2   Fail: 0   Skip: 0")))

(isolate-mat swish-test-repl ()
  (define tmp-prof (path-combine (output-dir) "test-repl.prof"))
  (write-test-file "test-repl.ss"
    (lambda ()
      (for-each pretty-print
        '((define (loaded? L) (and (member L (library-list)) #t))
          (assert (andmap loaded? '((swish mat) (swish profile) (swish testing))))
          (assert (not (app:name)))
          (assert (not (app:path)))
          (match (path-last (app:config-filename))
            [".config" 'ok])
          (printf "okay\n")))))
  (with-output-to-file tmp-prof void 'replace)
  (script-test swish-test-src `("--repl" ,(path-combine (output-dir) "test-repl.ss"))
    (format "~{~s\n~}"
      `((profile:start ,tmp-prof ,tmp-prof #t)
        (for-each pretty-print (sort string<? (map symbol->string (library-exports '(swish mat)))))
        (profile:save)
        (exit)))
    `(,(format "~a Version ~a" (software-product-name 'swish) (software-version 'swish))
      "okay" seek "add-mat" "mat" seek "run-mats" seek "summarize"))
  (script-test swish-test-src
    `("--repl" "--load-profile" "foo" "bar" "--save-profile" "chowder" "-s" "luke" "--progress" "ion" "--tag" "RFID" "--not" "really")
    (format "~{~s\n~}"
      `((profile:start ,tmp-prof ,tmp-prof #t)
        (pretty-print (source-directories))
        (profile:save)
        (exit)))
    `(seek
      "Ignoring --tag, --not, --progress, --load-profile, --save-profile in --repl mode"
      seek
      "(\"luke\" \"\\.\")"))
  (when (whereis 'profiler)
    (profile:merge tmp-prof))
  )

(isolate-mat select-tests ()
  (define subdir (uuid->string (osi_make_uuid)))
  (define test-file
    (write-test-file (path-combine subdir "test-specified.ms")
      (lambda ()
        (for-each pretty-print
          '((import (swish mat))
            (mat a1 (triv) (assert #t))
            (mat a2 (triv) (assert #t))
            (mat a3 (broken) (assert #f))
            (mat a4 () (assert (or #f #t)))
            (mat a5 (silly) (assert "ion")))))))
  ;; run entire suite, yet skip all tests
  (nonzero-exit
   (swish-test "--progress test --tag NONE " test-file)
   '(seek
     "SKIP *a1"
     "SKIP *a2"
     "SKIP *a3"
     "SKIP *a4"
     "SKIP *a5"
     "[-]+"
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 5"))
  ;; run test selected by name, yet skip all tests
  (nonzero-exit
   (swish-test "--progress test --tag NONE " test-file " a1 a5")
   '(seek
     "SKIP *a1"
     "SKIP *a5"
     "[-]+"
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 2"))
  ;; run tests selected by tag, all tests run pass
  (nonzero-exit
   (swish-test "--progress test --tag triv " test-file)
   '(seek
     "pass *a1"
     "pass *a2"
     "SKIP *a3"
     "SKIP *a4"
     "SKIP *a5"
     "[-]+"
     "Tests run: 2   Pass: 2   Fail: 0   Skip: 3"))
  ;; run tests excluding by tag
  (nonzero-exit
   (swish-test "--progress test --not broken " test-file)
   '(seek
     "pass *a1"
     "pass *a2"
     "SKIP *a3"
     "pass *a4"
     "pass *a5"
     "[-]+"
     "Tests run: 4   Pass: 4   Fail: 0   Skip: 1"))
  ;; tags are additive
  (nonzero-exit
   (swish-test "--progress test --tag triv --tag broken " test-file)
   '(seek
     "pass *a1"
     "pass *a2"
     "FAIL *a3"
     "SKIP *a4"
     "SKIP *a5"
     "[-]+"
     "Tests run: 3   Pass: 2   Fail: 1   Skip: 2"))
  ;; rerun skipped tests
  (zero-exit
   (swish-test "--rerun skip " (string-append test-file ".mo"))
   '(seek
     "pass *a4"
     "pass *a5"
     "[-]+"
     "Tests run: 2   Pass: 2   Fail: 0   Skip: 0"))
  ;; exclusion tags are additive
  (nonzero-exit
   (swish-test "--progress test --not triv --not silly " test-file)
   '(seek
     "SKIP *a1"
     "SKIP *a2"
     "FAIL *a3"
     "pass *a4"
     "SKIP *a5"
     "[-]+"
     "Tests run: 2   Pass: 1   Fail: 1   Skip: 3"))
  ;; rerun tests that passed or failed
  (nonzero-exit
   ;; using \t here to work around the split on spaces in swish-test helper
   (swish-test "--rerun (pass\tfail) " (string-append test-file ".mo"))
   '(seek
     "FAIL *a3"
     "pass *a4"
     "[-]+"
     "Tests run: 2   Pass: 1   Fail: 1   Skip: 0"))
  ;; rerun tests that failed
  (nonzero-exit
   (swish-test "--rerun fail " (string-append test-file ".mo"))
   '(seek
     "FAIL *a3"
     "[-]+"
     "Tests run: 1   Pass: 0   Fail: 1   Skip: 0"))
  ;; filter first by tags and then by exclusion tags
  ;; order on command-line doesn't matter
  (nonzero-exit
   (swish-test "--progress test --tag silly --not broken --tag triv --tag broken --tag triv " test-file)
   '(seek
     "pass *a1"
     "pass *a2"
     "SKIP *a3" ;; since --not broken
     "SKIP *a4" ;; since it doesn't match any included tags
     "pass *a5"
     "[-]+"
     "Tests run: 3   Pass: 3   Fail: 0   Skip: 2"))
  ;; rerun tests that were skipped, but respect the --not broken tag constraint
  (nonzero-exit
   (swish-test "--rerun skip --not broken " (string-append test-file ".mo"))
   '(seek
     "SKIP *a3"
     "pass *a4"
     "[-]+"
     "Tests run: 1   Pass: 1   Fail: 0   Skip: 1"))
  ;; rerun skipped tests in a specified directory
  (nonzero-exit
   (swish-test "--rerun skip " (path-parent test-file))
   '(seek
     "FAIL *a3"
     "[-]+"
     "Tests run: 1   Pass: 0   Fail: 1   Skip: 0"))
  ;; if any test fails, suite summary shows fail
  (nonzero-exit
   (swish-test "--progress suite --tag broken --tag silly " test-file)
   (list (format "~a *fail" (pregexp-quote test-file))))
  ;; if no test fails, suite summary shows skipped
  (nonzero-exit
   (swish-test "--progress suite --tag triv --tag silly " test-file)
   (list (format "~a *pass *\\(skipped 2\\)" (pregexp-quote test-file))))
  ;; failure exit code if specified test not found
  (nonzero-exit
   (swish-test "--progress test " test-file " this-test-does-not-exist")
   '(seek "*** Some test suite did not complete ***")
   "Exception in run-mat: mat this-test-does-not-exist is not defined")
  ;; if all *specified* tests pass, show pass, exit code zero
  (zero-exit
   (swish-test "--progress suite " test-file " a1 a2 a4")
   (list (format "~a *pass$" (pregexp-quote test-file)))))

(isolate-mat test-dir ()
  (define test-dir0 "subdir0")
  (define test-dir1 (path-combine test-dir0 "subdir1"))
  (define test-dir2 (path-combine test-dir0 "subdir2"))
  (define file1
    (write-test-file (path-combine test-dir1 "file1.ms")
      (lambda ()
        (for-each pretty-print
          '((import (swish mat))
            (mat t1 () (assert #t))
            (mat t2 () (assert #t)))))))
  (define file2
    (write-test-file (path-combine test-dir1 "file2.ms")
      (lambda ()
        (for-each pretty-print
          '((import (swish mat))
            (mat a1 () (assert #t))
            (mat a2 () (assert #t)))))))
  (define file3
    (write-test-file (path-combine test-dir2 "file3.ms")
      (lambda ()
        (for-each pretty-print
          '((import (swish mat))
            (mat a1 () (assert #t))
            (mat a2 () (assert #t))
            (mat bad () (assert #f)))))))
  (nonzero-exit
   (swish-test "--progress test " (path-combine (output-dir) test-dir0))
   (list
    (pregexp-quote file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *pass *t2"
    'seek
    (pregexp-quote file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *a1"
    " *pass *a2"
    'seek
    (pregexp-quote file3)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *a1"
    " *pass *a2"
    " *FAIL *bad"))
  (nonzero-exit
   (swish-test "--progress suite " (path-combine (output-dir) test-dir0))
   (list
    (format "~a *pass$" (pregexp-quote file1))
    (format "~a *pass$" (pregexp-quote file2))
    (format "~a *fail$" (pregexp-quote file3))))
  ;; test --harvest directory processing
  (let ([combined.json (path-combine (output-dir) "combined.json")])
    (nonzero-exit
     (swish-test "--report " combined.json " --harvest "
       (path-combine (output-dir) test-dir0))
     '("Tests run: 7   Pass: 6   Fail: 1   Skip: 0"))))

(isolate-mat spec-processing ()
  (define common-name-file
    (write-test-file "looks-like-filename.ms"
      (lambda ()
        (for-each pretty-print
          '((import (swish mat))
            (mat t1 () (assert #t))
            (mat t2 () (assert #f)))))))
  (define common-name (string->symbol common-name-file))
  (define test-file
    (write-test-file "test-spec-processing.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat))
            (mat ,common-name () (assert #t))
            (mat a1 () (assert #t))
            (mat a2 () (assert #f))
            (mat a3 () (assert #f)))))))
  (nonzero-exit
   ;; common-name interpreted as a suite since there is a file of that name
   (swish-test "--progress test " test-file " " common-name)
   (list
    'seek
    ;; suites appear in command-line order
    (pregexp-quote test-file)
    "[-]+" " Result * Test name * Message *" "[-]+"
    (format " *pass *~a" (pregexp-quote (symbol->string common-name)))
    " *pass *a1"
    " *FAIL *a2"
    " *FAIL *a3"
    "[-]+"
    "Tests run: 4   Pass: 2   Fail: 2   Skip: 0"
    'seek
    (pregexp-quote common-name-file)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *FAIL *t2"
    "[-]+"
    "Tests run: 2   Pass: 1   Fail: 1   Skip: 0"))
  (zero-exit
   ;; common-name explicitly identified as a test
   ;; zero exit means:
   ;;  1. we didn't run common-name as a suite
   ;;  2. we ran only the test named common-name
   (swish-test "--progress test " test-file " -t " common-name)
   (list
    'seek
    (pregexp-quote test-file)
    "[-]+" " Result * Test name * Message *" "[-]+"
    (format " *pass *~a" (pregexp-quote (symbol->string common-name)))
    "[-]+"
    "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"))
  (nonzero-exit
   ;; -t expects a value
   (swish-test test-file " -t")
   '()
   "swish-test: option expects value: -t")
  (nonzero-exit
   ;; bad spec
   (swish-test "does-not-exist-as-file-or-directory")
   '()
   "swish-test: expected directory or suite: \"does-not-exist-as-file-or-directory\"")
  )

(isolate-mat test-order ()
  (define test-setup
    (write-test-file "test-setup.ss"
      (lambda ()
        (for-each pretty-print
          '((import (swish mat))
            ;; side-effect so we can confirm tests ran in specified order vs.
            ;; results reported in a particular order
            (define tested
              (let ([mats '()])
                (case-lambda
                 [() (reverse mats)]
                 [(x) (set! mats (cons x mats))]))))))))
  (define test-file1
    (write-test-file "test-file1.ms"
      (lambda ()
        (for-each pretty-print
          `((include ,test-setup)
            (mat t1 () (tested 't1))
            (mat t2 () (tested 't2) (assert #f)) ;; failing test shouldn't affect order
            (mat t3 () (tested 't3))
            (mat t123 () (match (tested) [(t1 t2 t3) 'ok]))
            (mat t132 () (match (tested) [(t1 t3 t2) 'ok]))
            (mat t321 () (match (tested) [(t3 t2 t1) 'ok]))
            (mat t112233 () (match (tested) [(t1 t1 t2 t2 t3 t3) 'ok]))
            (mat t312213 () (match (tested) [(t3 t1 t2 t2 t1 t3) 'ok])))))))
  (define test-file2
    (write-test-file "test-file2.ms"
      (lambda ()
        (for-each pretty-print
          `((include ,test-setup)
            (mat a1 () (tested 'a1) (assert #f)) ;; failing test shouldn't affect order
            (mat a2 () (tested 'a2))
            (mat a3 () (tested 'a3) (assert #f)) ;; failing test shouldn't affect order
            (mat a123 () (match (tested) [(a1 a2 a3) 'ok]))
            (mat a321 () (match (tested) [(a3 a2 a1) 'ok]))
            (mat a232 () (match (tested) [(a2 a3 a2) 'ok]))
            (mat a11 () (match (tested) [(a1 a1) 'ok]))
            (mat a33321 () (match (tested) [(a3 a3 a3 a2 a1) 'ok])))))))
  ;; suites run in order they appear in file if not specified on command line
  (nonzero-exit
   (swish-test "--progress test " test-file1)
   (list
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *FAIL *t2"
    " *pass *t3"
    " *pass *t123"
    " *FAIL *t132"
    " *FAIL *t321"
    " *FAIL *t112233"
    " *FAIL *t312213"
    "[-]+"
    "Tests run: 8   Pass: 3   Fail: 5   Skip: 0"))
  ;; run a subset of the tests in the order they appear within the file
  (nonzero-exit
   (swish-test "--progress test " test-file1 " t1 t2 t3 t123")
   (list
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *FAIL *t2"
    " *pass *t3"
    " *pass *t123"
    "[-]+"
    "Tests run: 4   Pass: 3   Fail: 1   Skip: 0"))
  ;; run a subset of the tests in an order that differs from that within the file
  (nonzero-exit
   (swish-test "--progress test " test-file1 " t1 t3 t2 t132")
   (list
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *pass *t3"
    " *FAIL *t2"
    " *pass *t132"
    "[-]+"
    "Tests run: 4   Pass: 3   Fail: 1   Skip: 0"))
  ;; run a subset of the tests with duplicates
  (nonzero-exit
   (swish-test "--progress test " test-file1 " t1 t1 t2 t2 t3 t3 t112233")
   (list
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *pass *t1"
    " *FAIL *t2"
    " *FAIL *t2"
    " *pass *t3"
    " *pass *t3"
    " *pass *t112233"
    "[-]+"
    "Tests run: 7   Pass: 5   Fail: 2   Skip: 0"))
  ;; run a different subset of the tests with duplicates
  (nonzero-exit
   (swish-test "--progress test " test-file1 " t3 t1 t2 t2 t1 t3 t312213")
   (list
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t3"
    " *pass *t1"
    " *FAIL *t2"
    " *FAIL *t2"
    " *pass *t1"
    " *pass *t3"
    " *pass *t312213"
    "[-]+"
    "Tests run: 7   Pass: 5   Fail: 2   Skip: 0"))
  ;; run a subset of tests from one suite with all tests from second suite
  (nonzero-exit
   (swish-test "--progress test " test-file1 " t321 t3 t2 t1 t321 " test-file2)
   (list
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *FAIL *t321"
    " *pass *t3"
    " *FAIL *t2"
    " *pass *t1"
    " *pass *t321"
    "[-]+"
    "Tests run: 5   Pass: 3   Fail: 2   Skip: 0"
    'seek
    (pregexp-quote test-file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *FAIL *a1"
    " *pass *a2"
    " *FAIL *a3"
    " *pass *a123"
    " *FAIL *a321"
    " *FAIL *a232"
    " *FAIL *a11"
    " *FAIL *a33321"
    "[-]+"
    "Tests run: 8   Pass: 2   Fail: 6   Skip: 0"))
  ;; same as above, but flip order of suites
  (nonzero-exit
   (swish-test "--progress test " test-file2 " " test-file1 " t321 t3 t2 t1 t321")
   (list
    'seek
    (pregexp-quote test-file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *FAIL *a1"
    " *pass *a2"
    " *FAIL *a3"
    " *pass *a123"
    " *FAIL *a321"
    " *FAIL *a232"
    " *FAIL *a11"
    " *FAIL *a33321"
    "[-]+"
    "Tests run: 8   Pass: 2   Fail: 6   Skip: 0"
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *FAIL *t321"
    " *pass *t3"
    " *FAIL *t2"
    " *pass *t1"
    " *pass *t321"
    "[-]+"
    "Tests run: 5   Pass: 3   Fail: 2   Skip: 0"))
  ;; run subsets of each suite
  (nonzero-exit
   (swish-test "--progress test " test-file2 " a1 a1 a11 " test-file1 " t1 t3 t2 t132")
   (list
    'seek
    (pregexp-quote test-file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *FAIL *a1"
    " *FAIL *a1"
    " *pass *a11"
    "[-]+"
    "Tests run: 3   Pass: 1   Fail: 2   Skip: 0"
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *pass *t3"
    " *FAIL *t2"
    " *pass *t132"
    "[-]+"
    "Tests run: 4   Pass: 3   Fail: 1   Skip: 0"))
  ;; different subsets of each suite, with suites repeated;
  ;; also hit case where we run full suite after subset of same suite
  (nonzero-exit
   (swish-test "--progress test "
     test-file2 " a3 a3 a3 a2 a1 a33321 "
     test-file1 " t1 t3 t2 t132 "
     test-file1 " "
     test-file2 " a2 a3 a2 a232 ")
   (list
    'seek
    (pregexp-quote test-file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *FAIL *a3"
    " *FAIL *a3"
    " *FAIL *a3"
    " *pass *a2"
    " *FAIL *a1"
    " *pass *a33321"
    "[-]+"
    "Tests run: 6   Pass: 2   Fail: 4   Skip: 0"
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *pass *t3"
    " *FAIL *t2"
    " *pass *t132"
    "[-]+"
    "Tests run: 4   Pass: 3   Fail: 1   Skip: 0"
    'seek
    (pregexp-quote test-file1)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *t1"
    " *FAIL *t2"
    " *pass *t3"
    " *pass *t123"
    " *FAIL *t132"
    " *FAIL *t321"
    " *FAIL *t112233"
    " *FAIL *t312213"
    "[-]+"
    "Tests run: 8   Pass: 3   Fail: 5   Skip: 0"
    'seek
    (pregexp-quote test-file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *a2"
    " *FAIL *a3"
    " *pass *a2"
    " *pass *a232"
    "[-]+"
    "Tests run: 4   Pass: 3   Fail: 1   Skip: 0"))
  ;; run subset of suite after full suite
  (nonzero-exit
   (swish-test "--progress test "
     test-file2 " "
     test-file2 " a2 a3 a2 a232 ")
   (list
    'seek
    (pregexp-quote test-file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *FAIL *a1"
    " *pass *a2"
    " *FAIL *a3"
    " *pass *a123"
    " *FAIL *a321"
    " *FAIL *a232"
    " *FAIL *a11"
    " *FAIL *a33321"
    "[-]+"
    "Tests run: 8   Pass: 2   Fail: 6   Skip: 0"
    'seek
    (pregexp-quote test-file2)
    "[-]+" " Result * Test name * Message *" "[-]+"
    " *pass *a2"
    " *FAIL *a3"
    " *pass *a2"
    " *pass *a232"
    "[-]+"
    "Tests run: 4   Pass: 3   Fail: 1   Skip: 0")))

(isolate-mat report-results ()
  (define report-file (path-combine (output-dir) "report.html"))
  (define report-file1 (path-combine (output-dir) "report1.html"))
  (define report-file2 (path-combine (output-dir) "report2.html"))
  (define report-file3 (path-combine (output-dir) "report3.html"))
  (define report-file4 (path-combine (output-dir) "report4.html"))
  (define test-file
    (write-test-file "test-report.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat))
            (mat TEST1 (good) (assert #t))
            (mat TEST2 (tag1 tag2) (assert #f))
            (mat TEST3 () (cdr 17)))))))
  (define incomplete
    ;; create incomplete test suite output
    (write-test-file "incomplete.ms.mo"
      (lambda ()
        (let ([op (current-output-port)])
          ;; keep in sync w/ write-meta-data in mat.ss to avoid export
          (json:write-object op #f json:write
            [_type_ "meta-kv"]
            [key "completed"]
            [value #f])
          (json:write-object op #f json:write
            [_type_ "meta-kv"]
            [key "test-file"]
            [value "incomplete.ms"])))))
  (define test-file2 (write-test-file "test-report2.ms" void))
  (define test-file3
    (write-test-file "test-report3.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat))
            ;; raise error with a stack to dump
            (define (bug n)
              (if (= n 0)
                  (errorf 'bug "didn't handle zero")
                  (cons n (bug (- n 1)))))
            (mat zero ()
              (assert (list? (bug 10)))))))))
  (define test-file4
    (write-test-file "test-report4.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat))
            ;; check stack depth reporting
            (define (flub n what)
              (if (= n 0)
                  (throw what)
                  (cons n (flub (- n 1) what))))
            (mat hangry () (throw 'tantrum))
            (mat deep () (flub 100 'deep))
            (mat shallow () (flub 1 "a fit")))))))
  (define all-pass
    (write-test-file "all-pass.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat))
            (mat one () (assert #t))
            (mat two () (assert 'yes)))))))
  (define (check data test-name type tags message check-stack)
    (match-let*
     ([,results (mat-data-results data)]
      [,suite-start (json:ref (mat-data-meta-data data) 'timestamp #f)]
      [,mr (find (lambda (mr) (equal? test-name (mat-result-test mr))) results)]
      [#t (mat-result? mr)]
      [,@type (mat-result-type mr)]
      [,@tags (mat-result-tags mr)]
      [,@message (mat-result-message mr)]
      [#t (or (not check-stack)
              (check-stack
               (map json-stack->string (mat-result-stacks mr))))])
     (when suite-start
       (let ([mat-start-time (json:ref (mat-result-meta-data mr) 'start-time #f)]
             [mat-end-time (json:ref (mat-result-meta-data mr) 'end-time #f)])
         (assert (<= suite-start mat-start-time mat-end-time))))
     'ok))
  (define mo-path1 (string-append test-file ".mo"))
  (define mo-path2 (string-append test-file2 ".mo"))
  (define mo-path3 (string-append test-file3 ".mo"))
  (define mo-path4 (string-append test-file4 ".mo"))
  (define mo-all-pass (string-append all-pass ".mo"))
  (define (get-data content)
    (match-let* ([(,_ ,data) (pregexp-match "<script[^>]*>var data = (.*);</script><script[^>]*>\n  function escapeHtml" content)])
      (json:string->object data)))
  (define (results-of-type t obj)
    (filter (lambda (r) (eq? t (mat-result-type r)))
      (mat-data-results obj)))
  (define (find-test name obj)
    (find (lambda (r) (eq? name (mat-result-test r)))
      (mat-data-results obj)))
  ;; case where all tests pass
  (zero-exit (swish-test "--progress none " all-pass) '())
  ;; Tests like the following failed on ta6nt when we switched to the Github
  ;; windows-2025 image. The tests pass on the corresponding non-threaded build,
  ;; a6nt. Bob found that this test passed on ta6nt if we remove "--progress
  ;; none", which suggests a bug on ta6nt when a spawned Swish OS process exits
  ;; having written nothing to stdout (which is connected to a pipe). Since
  ;; threaded builds like ta6nt are not supported, we removed them from our
  ;; github test actions.
  (zero-exit (swish-test "--progress none --harvest " mo-all-pass) '())
  ;; case where some tests fail
  (nonzero-exit
   (swish-test "--progress none --report " report-file " " test-file)
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file)]
    [,content (utf8->string (read-file report-file))]
    [(,obj) (get-data content)]
    [,test1-result (find-test 'TEST1 obj)]
    [,test2-result (find-test 'TEST2 obj)]
    [,test3-result (find-test 'TEST3 obj)]
    ["pass" (json:ref test1-result 'type #f)]
    ["fail" (json:ref test2-result 'type #f)]
    [(,_) (pregexp-match "Exception: failed assertion #f at line 3,.*" (json:ref test2-result 'message #f))]
    ["fail" (json:ref test3-result 'type #f)]
    ["Exception in cdr: 17 is not a pair." (json:ref test3-result 'message #f)])
   'ok)
  ;; check .mo results file
  (let ([data (load-results mo-path1)])
    (check data 'TEST1 'pass '(good) "" #f)
    (check data 'TEST2 'fail '(tag1 tag2)
      (format "Exception: failed assertion #f at line 3, char 32 of ~a." test-file)
      #f)
    (check data 'TEST3 'fail '() "Exception in cdr: 17 is not a pair." #f)
    (match-let*
     ([,meta (mat-data-meta-data data)]
      ;; (meta-data completed)
      [#t (json:ref meta 'completed #f)]
      ;; (meta-data software-info)
      [,ignore-swish-revision
       (lambda (x)
         (json:object->string
          (let ([copy (json:string->object (json:object->string x))])
            (json:set! copy '(swish revision) "ignored")
            copy)))]
      [,expected (ignore-swish-revision (software-info))]
      [,@expected (ignore-swish-revision (json:ref meta 'software-info #f))]
      ;; (meta-data date)
      [,date-regexp  "([^ ]+ [^ ]+ [^ ]+ [^ ]+) ([^:]+):([^:]+)"]
      [(,_ ,exp-day ,exp-hour ,exp-min)
       (pregexp-match date-regexp (format-rfc2822 (current-date)))]
      [(,_ ,act-day ,act-hour ,act-min)
       (pregexp-match date-regexp (json:ref meta 'date ""))]
      [#t (or (string=? exp-day act-day)
              (string=? act-hour "23"))]
      ;; (meta-data timestamp)
      [,then (json:ref meta 'timestamp (expt 2 64))]
      [,now (erlang:now)]
      [#t (< then now)]
      ;; (meta-data test-file)
      [,test-file (path-last test-file)]
      [,@test-file (path-last (json:ref meta 'test-file "fail"))]
      ;; (meta-data test-run)
      [,test-run (json:ref meta 'test-run "fail")]
      [ok (and (string->uuid test-run) 'ok)]
      ;; (meta-data hostname)
      [,hostname (osi_get_hostname)]
      [,@hostname (json:ref meta 'hostname #f)]
      ;; (meta-data machine-type)
      [,arch (machine-type)]
      [,@arch (string->symbol (json:ref meta 'machine-type ""))])
     'ok))
  ;; specify explicit uuid
  (let ([test-file
         (write-test-file "explicit-uuid.ms"
           (lambda ()
             (for-each pretty-print
               `((import (swish mat))
                 (mat good () (assert #t))
                 (mat bad () (assert #f))))))]
        [explicit.json (path-combine (output-dir) "explicit.json")]
        [uuid (uuid->string (osi_make_uuid))])
    (nonzero-exit
     (swish-test "--test-run " (format "~(~a~)" uuid) " --report " explicit.json " " test-file)
     '(seek
       "Tests run: 2   Pass: 1   Fail: 1   Skip: 0"))
    (match-let*
     ([(,file1) (json:bytevector->object (read-file explicit.json))]
      [,@uuid (json:ref file1 '(meta-data test-run) #f)])
     'ok))
  ;; check stack field
  (nonzero-exit
   (swish-test test-file3)
   '(seek "Exception in bug: didn't handle zero"))
  (let ([data (load-results mo-path3)])
    (check data 'zero 'fail '() "Exception in bug: didn't handle zero."
      (lambda (actual-formatted)
        (equal? actual-formatted
          (list
           ;; JSON stack data provides offset, not line and char
           (format "~:{#<continuation in bug> at offset 106 of ~a\n  n: ~a\n~}Stack dump truncated due to max-depth = 10.\n"
             (map (lambda (n) (list test-file3 (+ n 1)))
               (iota 10))))))))
  ;; check stack dump
  (match-let*
   ([ok (nonzero-exit (swish-test test-file4)
          '(seek "Tests run: 3   Pass: 0   Fail: 3   Skip: 0"))]
    [,data (load-results mo-path4)]
    [,max-depth 10]
    [,where (format "#<continuation in flub> at offset 78 of ~a" test-file4)]
    [,trunc-prefix "Stack dump truncated due to max-depth = "]
    [,trunc (format "~a~a." trunc-prefix max-depth)])
   (check data 'hangry 'fail '() "tantrum"
     (lambda (actual-formatted)
       (match-let*
        ([(,str) actual-formatted]
         [(,first . ,_) (split str #\newline)]
         [(,_) (pregexp-match "#<continuation> at offset 116 of .*test-report4.ms"
                 first)])
        #t)))
   (check data 'deep 'fail '() "deep"
     (lambda (actual-formatted)
       (match-let* ([(,str) actual-formatted])
         (equal? (split str #\newline)
           (let expect ([n max-depth] [ls (list trunc "")])
             (if (= n 0)
                 ls
                 (expect (- n 1)
                   `(,where ,(format "  n: ~s" n) ,@ls))))))))
   (match-let* ([,results (mat-data-results data)]
                [,mr (find (lambda (mr) (eq? 'deep (mat-result-test mr))) results)]
                [(,stack0) (mat-result-stacks mr)]
                [,@max-depth (json:ref stack0 'truncated 'wrong)]
                [(,fr0 . ,_) (json:ref stack0 'frames '())]
                [,depth (json:ref stack0 'depth "missing")])
     (assert (> depth 100)))
   (check data 'shallow 'fail '() "a fit"
     (lambda (actual-formatted)
       (match-let* ([(,str) actual-formatted]
                    [,lines (split str #\newline)])
         (and (match-regexps
               `(,(pregexp-quote where)
                 "  n: 1"
                 ,(pregexp-quote (format "#<continuation> at offset 181 of ~a" test-file4)))
               lines)
              #t)))))
  ;; all pass, but some skipped so nonzero exit
  (nonzero-exit
   (swish-test "--progress none --tag good --report " report-file1 " " test-file)
   '())
  ;; same exit status when we harvest those results
  (nonzero-exit (swish-test "--progress none --harvest " mo-path1) '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file1)]
    [,content (utf8->string (read-file report-file1))]
    [(,obj) (get-data content)]
    [1 (length (results-of-type 'pass obj))]
    [2 (length (results-of-type 'skip obj))]
    [0 (length (results-of-type 'fail obj))])
   'ok)
  ;; all pass, some skipped, but test specified explicitly so zero exit
  (zero-exit
   (swish-test "--progress none --tag good --report " report-file2 " " test-file " TEST1")
   '())
  ;; same exit code when we --harvest those results [questionable but consistent]
  (zero-exit (swish-test "--progress none --harvest " mo-path1) '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file2)]
    [,content (utf8->string (read-file report-file2))]
    [(,obj) (get-data content)]
    [1 (length (results-of-type 'pass obj))]
    [0 (length (results-of-type 'skip obj))]
    [0 (length (results-of-type 'fail obj))])
   'ok)
  ;; some skipped so nonzero exit, no results for test-file2
  (nonzero-exit
   (swish-test "--progress none --tag good --report " report-file3 " " test-file " " test-file2)
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file3)]
    [,content (utf8->string (read-file report-file3))]
    [(,obj1 ,obj2) (get-data content)]
    ["test-report.ms" (path-last (json:ref obj1 '(meta-data test-file) #f))]
    ["test-report2.ms" (path-last (json:ref obj2 '(meta-data test-file) #f))]
    [1 (length (results-of-type 'pass obj1))]
    [2 (length (results-of-type 'skip obj1))]
    [0 (length (results-of-type 'fail obj1))]
    [,test1-result (find-test 'TEST1 obj1)]
    ["pass" (json:ref test1-result 'type #f)]
    [() (json:ref obj2 'results #f)])
   'ok)
  ;; check summary
  (match-let*
   ([(1 0 2 2 3)
     (call-with-values
       (lambda () (summarize (list mo-path1 mo-path2 incomplete)))
       list)])
   'ok)
  ;; tests fail, some skipped
  (nonzero-exit
   (swish-test "--progress none --not good --report " report-file4 " " test-file)
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file4)]
    [,content (utf8->string (read-file report-file4))]
    [(,obj) (get-data content)]
    [0 (length (results-of-type 'pass obj))]
    [1 (length (results-of-type 'skip obj))]
    [2 (length (results-of-type 'fail obj))])
   'ok)
  ;; check summary
  (match-let*
   ([(0 2 1 2 2)
     (call-with-values
       (lambda () (summarize (list mo-path1 mo-path2)))
       list)])
   'ok)
  (let ([aggregate.json (path-combine (output-dir) "aggregate.json")]
        [aggregate.html (path-combine (output-dir) "aggregate.html")]
        [alt.html (path-combine (output-dir) "alt.html")]
        [summary-output
         '("Tests run: 3   Pass: 1   Fail: 2   Skip: 1"
           seek
           "Some test suite did not complete")])
    ;; re-run test-file for harvest tests, skipping 1
    (nonzero-exit
     (swish-test "--progress none --not tag1 " test-file)
     '())
    ;; --harvest alone prints summary and incomplete notice
    (nonzero-exit
     (swish-test "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete)
     summary-output)
    ;; --harvest for report also prints summary and incomplete notice
    (nonzero-exit
     (swish-test
      "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete
      " --report " aggregate.json)
     summary-output)
    ;; --load-results alone prints summary and incomplete notice
    (nonzero-exit
     (swish-test "--load-results " aggregate.json)
     summary-output)
    (let ([expected
           `(,(string-append (pregexp-quote test-file) ".* fail")
             ,(string-append (pregexp-quote test-file2) ".* no tests")
             ,(string-append (pregexp-quote test-file3) ".* fail")
             ,(string-append (pregexp-quote "incomplete.ms") ".* no tests")
             ,@summary-output)])
      ;; --progress suite with --harvest
      (nonzero-exit
       (swish-test
        "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete " "
        "--progress " "suite")
       expected)
      ;; --progress suite with --load-results
      (nonzero-exit
       (swish-test
        "--progress " "suite "
        "--load-results " aggregate.json)
       expected))
    (let ([expected
           `(,(string-append (pregexp-quote test-file) ".* fail")
             " SKIP .*TEST2"
             " FAIL .*TEST3"
             ,(string-append (pregexp-quote test-file2) ".* no tests")
             ,(string-append (pregexp-quote test-file3) ".* fail")
             " FAIL .*zero"
             ,(string-append (pregexp-quote "incomplete.ms") ".* no tests")
             ,@summary-output)])
      ;; --progress suite-verbose with --harvest
      (nonzero-exit
       (swish-test
        "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete " "
        "--progress " "suite-verbose")
       expected)
      ;; --progress suite-verbose with --load-results
      (nonzero-exit
       (swish-test
        "--progress " "suite-verbose "
        "--load-results " aggregate.json)
       expected))
    (let ([expected
           `(,(pregexp-quote test-file)
             seek
             " pass .*TEST1"
             " SKIP .*TEST2"
             " FAIL .*TEST3"
             seek
             "Tests run: 2   Pass: 1   Fail: 1   Skip: 1"
             seek
             ,(pregexp-quote test-file2)
             seek
             "Tests run: 0   Pass: 0   Fail: 0   Skip: 0"
             seek
             ,(pregexp-quote test-file3)
             seek
             " FAIL .*zero"
             seek
             "Tests run: 1   Pass: 0   Fail: 1   Skip: 0"
             seek
             ,(pregexp-quote "incomplete.ms")
             seek
             "Tests run: 0   Pass: 0   Fail: 0   Skip: 0"
             seek
             ,@summary-output)])
      ;; --progress test with --harvest
      (nonzero-exit
       (swish-test
        "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete " "
        "--progress " "test")
       expected)
      ;; --progress test with --load-results
      (nonzero-exit
       (swish-test
        "--progress " "test "
        "--load-results " aggregate.json)
       expected))
    (let ([expected '(seek "Some test suite did not complete")])
      ;; --progress none with --harvest
      (nonzero-exit
       (swish-test
        "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete " "
        "--progress " "none")
       expected)
      ;; --progress none with --load-results
      (nonzero-exit
       (swish-test
        "--progress " "none "
        "--load-results " aggregate.json)
       expected))
    ;; --load-results and --report generating HTML output prints same summary
    (nonzero-exit
     (swish-test "--load-results " aggregate.json " --report " aggregate.html)
     (cons "see file:.*aggregate.html" summary-output))
    ;; can generate same HTML report via --harvest and --report
    (nonzero-exit
     (swish-test
      "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete
      " --report " alt.html)
     (cons "see file:.*alt.html" summary-output))
    ;; test --info with --harvest
    (let* ([data (load-results mo-path2)] ;; get actual info
           [info (json:ref data '(meta-data software-info) 'oops)]
           [fake-file (string-append mo-path2 ".fake")]
           [fake-info #f])
      (define (fmt info key)
        (pregexp-quote
         (format "~a ~a (~a)"
           (json:ref info `(,key product-name) "?")
           (json:ref info `(,key version) "?")
           (json:ref info `(,key revision) "?"))))
      ;; add fake info as if test results generated w/ multiple software
      ;; revisions; base it on test-report2 which contains no results so
      ;; it doesn't change expected summary-output
      (let ([ip (open-file-to-read mo-path2)]
            [op (open-file-to-replace fake-file)])
        (on-exit (begin (close-port ip) (close-port op))
          (let f ()
            (let ([x (json:read ip)])
              (unless (eof-object? x)
                (cond
                 [(equal? "software-info" (json:ref x 'key #f))
                  (set! fake-info (json:ref x 'value #f))
                  (json:set! fake-info '(chezscheme revision) (make-string 40 #\0))
                  (json:set! fake-info '(swish revision) (make-string 40 #\f))
                  (json:write op x 0)]
                 [else (json:write op x 0)])
                (f))))))
      (on-exit (delete-file fake-file)
        (nonzero-exit
         (swish-test
          "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete
          " --info")
         `(,@summary-output
           "^$"
           ,(fmt info 'chezscheme)
           ,(fmt info 'swish)))
        (nonzero-exit
         (swish-test
          "--harvest " mo-path1 " " mo-path2 " " mo-path3 " " incomplete
          " " fake-file
          " --info")
         `(,@summary-output
           "^$"
           ,(fmt fake-info 'chezscheme)
           ,(fmt info 'chezscheme)
           ,(fmt info 'swish)
           ,(fmt fake-info 'swish)))))
    ;; check the HTML report output
    (match-let*
     ([,expected (read-file aggregate.html)]
      [,_ ;; we get the same HTML output both ways
       (assert (equal? expected (read-file alt.html)))]
      [,content (utf8->string expected)]
      [,mat-data* (get-data content)]
      [,find-suite
       (lambda (test-file)
         (find (lambda (x)
                 (equal? test-file (json:ref (mat-data-meta-data x) 'test-file #f)))
           mat-data*))]
      [,mo1 (find-suite test-file)]
      [,test1-result (find-test 'TEST1 mo1)]
      [,test2-result (find-test 'TEST2 mo1)]
      [,test3-result (find-test 'TEST3 mo1)]
      ["pass" (json:ref test1-result 'type #f)]
      ["skip" (json:ref test2-result 'type #f)]
      ["fail" (json:ref test3-result 'type #f)]
      [,mo2 (find-suite test-file2)]
      [() (mat-data-results mo2)]
      [,mo3 (find-suite test-file3)]
      [,zero-result (find-test 'zero mo3)]
      ["fail" (json:ref zero-result 'type #f)]
      ["Exception in bug: didn't handle zero." (json:ref zero-result 'message #f)]
      [(,_)
       (pregexp-match "continuation in bug.*at offset 106"
         (format "~{~a\n~}"
           (map json-stack->string
             (json:ref zero-result 'stacks "no stack"))))]
      [,mo4 (find-suite "incomplete.ms")]
      [#f (json:ref mo4 '(meta-data completed) "bupkis")]
      [() (mat-data-results mo4)])
     'ok))
  ;; try harvesting from the wrong place
  (nonzero-exit
   (swish-test "--harvest " mo-path1 " " report-file1)
   '()
   "swish-test: cannot load mat results from.*report1.html: Unexpected input at position")
  ;; try loading results from the wrong place
  (nonzero-exit
   (swish-test "--load-results " report-file1)
   '()
   "swish-test: cannot load results from.*report1.html: Unexpected input at position")
  )

(isolate-mat annotation ()
  (define anno.ms
    (write-test-file "anno.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat))
            (define (f n)
              (let lp ([n n] [ls '()])
                (mat:add-annotation! (json:make-object [n n] [ls ls]))
                (if (= n 0)
                    ls
                    (lp (- n 1) (cons n ls)))))
            (mat one ()
              (mat:add-annotation! "before")
              (assert (equal? '(1 2 3 4) (f 4)))
              (mat:add-annotation! "after")))))))
  (define anno.ms.mo (string-append anno.ms ".mo"))
  (define (make-ref field) (lambda (o) (json:ref o field #f)))
  (zero-exit
   (swish-test anno.ms)
   '(seek "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"))
  (match-let*
   ([,data (load-results anno.ms.mo)]
    [(,result) (mat-data-results data)]
    [,annotations (json:ref (mat-result-meta-data result) 'annotations #f)]
    ;; check value field
    [("before" ,a4 ,a3 ,a2 ,a1 ,a0 "after") (map (make-ref 'value) annotations)]
    [(4 3 2 1 0) (map (make-ref 'n) (list a4 a3 a2 a1 a0))]
    [() (json:ref a4 'ls #f)]
    [(4) (json:ref a3 'ls #f)]
    [(3 4) (json:ref a2 'ls #f)]
    [(2 3 4) (json:ref a1 'ls #f)]
    [(1 2 3 4) (json:ref a0 'ls #f)])
   ;; check timestamp field; timestamps are non-decreasing since
   ;; we record annotations in the order they were added
   (assert (apply <= (map (make-ref 'timestamp) annotations)))))

(isolate-mat incomplete ()
  (define abort.ms
    (write-test-file "abort.ms"
      (lambda ()
        ;; call exit instead of abort to clean up and prevent apparent leaks
        (pretty-print '(exit 1)))))
  (define no-tests.ms (write-test-file "no-tests.ms" void))
  (define pass.ms
    (write-test-file "pass.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat))
            (mat good () (assert #t)))))))
  (nonzero-exit
   ;; complain if test suite does not complete
   (swish-test abort.ms)
   `("Some test suite did not complete"))
  (nonzero-exit
   ;; complain if test suite does not complete
   (swish-test abort.ms " " no-tests.ms)
   `(seek
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 0"
     seek
     "Some test suite did not complete"
     seek
     "No tests found in"
     "no-tests.ms"))
  (nonzero-exit
   ;; complain if test suite does not complete
   (swish-test abort.ms " " no-tests.ms " " pass.ms)
   `(seek
     "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"
     seek
     "Some test suite did not complete"
     seek
     "No tests found in"
     "no-tests.ms"))
  ;; check summary
  (match-let*
   ([(1 0 0 2 3)
     (call-with-values
       (lambda ()
         (summarize
          (map (lambda (fn) (string-append fn ".mo"))
            (list abort.ms pass.ms no-tests.ms))))
       list)])
   'ok)
  ;; check available meta-data for incomplete .mo
  (match-let*
   ([,abort-result (load-results (string-append abort.ms ".mo"))]
    [() (mat-data-results abort-result)]
    [,abort-meta (mat-data-meta-data abort-result)]
    ;; (meta-data completed)
    [#f (json:ref abort-meta 'completed "oops")]
    ;; (meta-data test-file)
    [,expected (path-last abort.ms)]
    [,@expected (path-last (json:ref abort-meta 'test-file ""))]
    ;; (meta-data hostname)
    [,hostname (osi_get_hostname)]
    [,@hostname (json:ref abort-meta 'hostname #f)]
    ;; (meta-data machine-type)
    [,arch (symbol->string (machine-type))]
    [,@arch (json:ref abort-meta 'machine-type #f)])
   'ok)
  )

(isolate-mat file-extension ()
  (define test-file (write-test-file "no-tests.ms" void))
  (define source-file (write-test-file "no-tests.ss" void))
  (nonzero-exit
   ;; complain if .ms file contains no tests
   (swish-test test-file)
   `(seek
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 0"
     seek
     "No tests found in"
     ,(pregexp-quote test-file)))
  (zero-exit
   ;; don't complain if non-.ms file contains no tests
   (swish-test source-file)
   '())
  (nonzero-exit
   (swish-test "--report foobar")
   '()
   "cannot determine output format from --report foobar, expected .json, .html, or .htm extension")
  )

;; We shut down the profiler temporarily in some places to avoid interfering
;; with the profile:start call in swish-test; this means the coverage report
;; understates the actual coverage for swish-test.
(define-syntax without-profiler
  (syntax-rules ()
    [(_ e0 e1 ...)
     (let* ([pid (whereis 'profiler)]
            [pfile (and pid (profile:filename))])
       (when pid (profile:stop))
       (on-exit
        (let ([pid (whereis 'profiler)])
          (when pid (profile:stop))
          (when pfile
            (match (profile:start pfile pfile #t)
              [#(ok ,pid) (void)])))
        e0 e1 ...))]))

(isolate-mat profile ()
  (define src-file "profile-code.ss")
  (define source-file
    (write-test-file src-file
      (lambda ()
        (display "#!chezscheme\n")
        (display "(define (fac n)\n")
        (display "  (if (= n 0)\n")
        (display "      1\n")
        (display "      (* n (fac (- n 1)))))\n")
        (display "(define (unused)\n")
        (display "  (when (fly? 'pigs)\n")
        (display "    (unused)))\n")
        (display "#!eof mats\n")
        (for-each pretty-print
          '((import (swish mat) (swish testing))
            ;; re-expose a, which is already exported
            (load-this)
            (mat test0 () (assert (odd? (fac 0))))
            (mat test5 () (assert (= 120 (fac 5)))))))))
  (define source-file-rel (path-relative source-file))
  (define test-file
    (write-test-file "profile-code.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat) (swish profile) (swish testing))
            (load ,source-file-rel)
            (mat a1 ()
              (assert (odd? (fac 0)))
              (assert (even? (fac 10)))))))))
  (define profile-file (path-combine (output-dir) "prof.dat"))
  (define profile1-file (path-combine (output-dir) "prof1.dat"))
  (define profile2-file (path-combine (output-dir) "prof2.dat"))
  (define aggregate-file (path-combine (output-dir) "agg.dat"))
  (define coverage-file (path-combine (output-dir) "cov" "summary.html"))
  (define coverage1-file (path-combine (output-dir) "cov1" "summary.html"))
  (define (check-count count elt s)
    (pregexp-match (format "<span title=\"~a\"[^>]*>~a</span>" count (pregexp-quote elt)) s))
  (define (summary-row filename hits sites coverage max-count)
    (format ".*~a.*<td>~a</td><td>~a</td>.*~a%.*<td>~a</td>"
      (pregexp-quote (pregexp-replace* "\\\\" filename "/")) hits sites coverage max-count))
  ;; --save-profile overwrites, accumulates only with explicit --load-profile
  (do ([n 0 (+ n 1)]) ((= n 2))
    (without-profiler
     (zero-exit
      (swish-test "--save-profile " profile-file " --progress suite " test-file)
      (list (format "~a *pass" (pregexp-quote test-file))))
     (zero-exit
      (swish-test "--load-profile " profile-file " --coverage " coverage-file)
      '(seek ;; needed for make coverage, due to "Skipping ..."
        "see file:.*cov/summary.html"
        "Tests run: 0   Pass: 0   Fail: 0   Skip: 0")))
    (match-let*
     ([#t (file-regular? coverage-file)]
      [,cov (utf8->string (read-file coverage-file))]
      [(,_) (pregexp-match "Overall 74% coverage with 17 of 23 sites covered" cov)]
      [(,_) (pregexp-match (summary-row src-file 17 23 74 12) cov)]
      [,ss.html (path-combine (path-parent coverage-file) (string-append src-file ".html"))]
      [#t (file-regular? ss.html)]
      [,ss (utf8->string (read-file ss.html))]
      [(,_) (check-count 2 "1" ss)]  ;; base case
      [(,_) (check-count 10 "-" ss)] ;; recursive call
      [(,_) (check-count 0 "unused" ss)])
     'ok))
  ;; --save-profile combined with --coverage for a specific #!eof mat
  (without-profiler
   (zero-exit
    (swish-test "--save-profile " profile1-file " --coverage " coverage1-file " " source-file-rel " test0")
    '(seek
      "see file:.*cov1/summary.html"
      "Tests run: 1   Pass: 1   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage1-file)]
    [,cov1 (utf8->string (read-file coverage1-file))]
    [(,_) (pregexp-match "Overall 45% coverage with 18 of 40 sites covered" cov1)]
    [(,_) (pregexp-match (summary-row src-file 18 40 45 1) cov1)]
    [,ss.html (path-combine (path-parent coverage1-file) (string-append src-file ".html"))]
    [#t (file-regular? ss.html)]
    [,ss (utf8->string (read-file ss.html))]
    [(,_) (check-count 1 "1" ss)] ;; base case
    [(,_) (check-count 0 "-" ss)] ;; recursive call
    [(,_) (check-count 0 "unused" ss)]
    [(,_) (check-count 1 "odd?" ss)] ;; ran test0
    [(,_) (check-count 0 "120" ss)]  ;; did not run test5
    )
   'ok)
  ;; --save-profile followed by separate --coverage for a different #!eof mat
  (without-profiler
   (zero-exit
    (swish-test "--save-profile " profile2-file " " source-file-rel " test5")
    '(seek
      " pass *test5"
      seek
      "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"))
   (zero-exit
    (swish-test "--load-profile " profile2-file " --coverage " coverage1-file)
    '(seek ;; needed for make coverage, due to "Skipping ..."
      "see file:.*cov1/summary.html"
      "Tests run: 0   Pass: 0   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage1-file)]
    [,cov1 (utf8->string (read-file coverage1-file))]
    [(,_) (pregexp-match "Overall 70% coverage with 28 of 40 sites covered" cov1)]
    [(,_) (pregexp-match (summary-row src-file 28 40 70 6) cov1)]
    [,ss.html (path-combine (path-parent coverage1-file) (string-append src-file ".html"))]
    [#t (file-regular? ss.html)]
    [,ss (utf8->string (read-file ss.html))]
    [(,_) (check-count 1 "1" ss)] ;; base case
    [(,_) (check-count 5 "-" ss)] ;; recursive call
    [(,_) (check-count 0 "unused" ss)]
    [(,_) (check-count 0 "odd?" ss)] ;; did not run test0
    [(,_) (check-count 1 "120" ss)]  ;; ran test5
    )
   'ok)
  ;; --load-profile multiple profiles via --coverage
  (zero-exit
   (swish-test "--load-profile " profile1-file " " profile2-file " --coverage " coverage1-file)
   '(seek ;; needed for make coverage, due to "Skipping ..."
     "see file:.*cov1/summary.html"
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 0"))
  (match-let*
   ([#t (file-regular? coverage1-file)]
    [,cov1 (utf8->string (read-file coverage1-file))]
    [(,_) (pregexp-match "Overall 85% coverage with 34 of 40 sites covered" cov1)]
    [(,_) (pregexp-match (summary-row src-file 34 40 85 7) cov1)]
    [,ss.html (path-combine (path-parent coverage1-file) (string-append src-file ".html"))]
    [#t (file-regular? ss.html)]
    [,ss (utf8->string (read-file ss.html))]
    [(,_) (check-count 2 "1" ss)] ;; base case
    [(,_) (check-count 5 "-" ss)] ;; recursive call
    [(,_) (check-count 0 "unused" ss)]
    [(,_) (check-count 1 "odd?" ss)] ;; ran test0
    [(,_) (check-count 1 "120" ss)]  ;; ran test5
    )
   'ok)
  ;; add to profile via --load-profile and --save-profile and save a coverage report
  (without-profiler
   (zero-exit
    (swish-test
     "--load-profile " profile-file
     " --save-profile " profile-file
     " --coverage " coverage-file
     " " test-file " " source-file-rel)
    '(seek
      " pass *a1"
      "[-]+"
      "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"
      seek
      " pass *test0"
      " pass *test5"
      "[-]+"
      "Tests run: 2   Pass: 2   Fail: 0   Skip: 0"
      seek
      "see file:.*cov/summary.html"
      "Tests run: 3   Pass: 3   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage-file)]
    [,cov (utf8->string (read-file coverage-file))]
    [(,_) (pregexp-match "Overall 85% coverage with 34 of 40 sites covered" cov)]
    [(,_) (pregexp-match (summary-row src-file 34 40 85 31) cov)]
    [,ss.html (path-combine (path-parent coverage-file) (string-append src-file ".html"))]
    [#t (file-regular? ss.html)]
    [,ss (utf8->string (read-file ss.html))]
    [(,_) (check-count (+ (* 2 2)  2) "1" ss)] ;; base case
    [(,_) (check-count (+ (* 2 10) 5) "-" ss)] ;; recursive call
    [(,_) (check-count 0 "unused" ss)])
   'ok)
  ;; merge profile data via --load-profile and --save-profile
  (without-profiler
   (zero-exit
    (swish-test
     "--load-profile " profile-file " " profile1-file " " profile2-file
     " --save-profile " profile-file
     " --coverage " coverage-file)
    '(seek ;; needed for make coverage, due to "Skipping ..."
      "see file:.*cov/summary.html"
      "Tests run: 0   Pass: 0   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage-file)]
    [,cov (utf8->string (read-file coverage-file))]
    [(,_) (pregexp-match "Overall 85% coverage with 34 of 40 sites covered" cov)]
    [(,_) (pregexp-match (summary-row src-file 34 40 85 (+ 31 7)) cov)]
    [,ss.html (path-combine (path-parent coverage-file) (string-append src-file ".html"))]
    [#t (file-regular? ss.html)]
    [,ss (utf8->string (read-file ss.html))]
    [(,_) (check-count (+ (* 2 2)  (* 2 2)) "1" ss)] ;; base case
    [(,_) (check-count (+ (* 2 10) (* 2 5)) "-" ss)] ;; recursive call
    [(,_) (check-count 0 "unused" ss)])
   'ok)
  )

(isolate-mat library ()
  (define lib-dir "util")
  (define lib-file
    (write-test-file (path-combine lib-dir "help.ss")
      (lambda ()
        (pretty-print
         `(library (help)
            (export ob)
            (import (scheme))
            (define ob 1))))))
  (define src-file
    (write-test-file "lib-src.ss"
      (lambda ()
        (for-each pretty-print
          `((import (,(string->symbol (path-root (path-last lib-file)))))
            (import (scheme))
            (define (guess x)
              (if (equal? x ob)
                  'yes
                  'no)))))))
  (define test-file
    (write-test-file "lib-test.ms"
      (lambda ()
        (for-each pretty-print
          `((import (swish mat) (swish profile) (swish testing))
            (load ,src-file)
            (mat b9 ()
              (match-let*
               ([yes (guess 1)]
                [no (guess 0)])
               'ok)))))))
  (zero-exit
   (swish-test "-L " (path-combine (output-dir) lib-dir) " " test-file)
   '(seek
     " pass *b9"
     seek
     "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"))
  (nonzero-exit
   (swish-test test-file)
   `(seek ,(pregexp-quote (format "Test Failed: ~a" test-file)))
   (pregexp-quote "Exception: library (help) not found")))
