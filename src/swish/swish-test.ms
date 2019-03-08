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
      "progress <mode>"
      "<spec> .* directory .* suite"))
  (zero-exit (swish-test "-h all")
    '("Usage:"
      "progress <mode>"
      "<spec> .* directory .* suite"
      "By default,"
      "Examples:"))
  (nonzero-exit (swish-test "--help me-obi-wan")
    '()
    "swish-test: unrecognized help section: me-obi-wan")
  )

(isolate-mat check-options ()
  (nonzero-exit (swish-test "--coverage foo")
    '() "--coverage requires --load-profile or --save-profile")
  ;; check for required options before complaining about missing file
  (nonzero-exit (swish-test "--load-profile file-that-does-not-exist")
    '() "--load-profile requires --save-profile or --coverage"))

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
    '("Tests run: 2   Pass: 2   Fail: 0   Skip: 0")))

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
    '("Tests run: 5   Pass: 5   Fail: 0   Skip: 0"
      "Tests run: 2   Pass: 2   Fail: 0   Skip: 0")))

(isolate-mat swish-test-repl ()
  (define tmp-prof (path-combine (output-dir) "test-repl.prof"))
  (write-test-file "test-repl.ss"
    (lambda ()
      (for-each pretty-print
        '((define (loaded? L) (and (member L (library-list)) #t))
          (assert (andmap loaded? '((swish mat) (swish profile) (swish testing))))
          (printf "okay\n")))))
  (with-output-to-file tmp-prof void 'replace)
  (script-test swish-test-src `("--repl" ,(path-combine (output-dir) "test-repl.ss"))
    (format "~{~s\n~}"
      '((profile:start ,tmp-prof ,tmp-prof #t)
        (for-each pretty-print (sort string<? (map symbol->string (library-exports '(swish mat)))))
        (profile:save)
        (exit)))
    `("" ,(swish-version) "okay" "add-mat" "mat" "run-mats" "summarize"))
  (script-test swish-test-src
    `("--repl" "--load-profile" "foo" "bar" "--save-profile" "chowder" "-s" "luke" "--progress" "ion" "--tag" "RFID" "--not" "really")
    (format "~{~s\n~}"
      `((profile:start ,tmp-prof ,tmp-prof #t)
        (pretty-print (source-directories))
        (profile:save)
        (exit)))
    `("Ignoring --tag, --not, --load-profile, --save-profile, --progress in --repl mode"
      "(\"luke\" \"\\.\")"))
  (when (whereis 'profiler)
    (profile:merge tmp-prof))
  )

(isolate-mat select-tests ()
  (define test-file
    (write-test-file "test-specified.ms"
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
   '("SKIP *a1"
     "SKIP *a2"
     "SKIP *a3"
     "SKIP *a4"
     "SKIP *a5"
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 5"))
  ;; run test selected by name, yet skip all tests
  (nonzero-exit
   (swish-test "--progress test --tag NONE " test-file " a1 a5")
   '("SKIP *a1"
     "SKIP *a5"
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 2"))
  ;; run tests selected by tag, all tests run pass
  (nonzero-exit
   (swish-test "--progress test --tag triv " test-file)
   '("pass *a1"
     "pass *a2"
     "SKIP *a3"
     "SKIP *a4"
     "SKIP *a5"
     "Tests run: 2   Pass: 2   Fail: 0   Skip: 3"))
  ;; run tests excluding by tag
  (nonzero-exit
   (swish-test "--progress test --not broken " test-file)
   '("pass *a1"
     "pass *a2"
     "SKIP *a3"
     "pass *a4"
     "pass *a5"
     "Tests run: 4   Pass: 4   Fail: 0   Skip: 1"))
  ;; tags are additive
  (nonzero-exit
   (swish-test "--progress test --tag triv --tag broken " test-file)
   '("pass *a1"
     "pass *a2"
     "FAIL *a3"
     "SKIP *a4"
     "SKIP *a5"
     "Tests run: 3   Pass: 2   Fail: 1   Skip: 2"))
  ;; exclusion tags are additive
  (nonzero-exit
   (swish-test "--progress test --not triv --not silly " test-file)
   '("SKIP *a1"
     "SKIP *a2"
     "FAIL *a3"
     "pass *a4"
     "SKIP *a5"
     "Tests run: 2   Pass: 1   Fail: 1   Skip: 3"))
  ;; filter first by tags and then by exclusion tags
  ;; order on command-line doesn't matter
  (nonzero-exit
   (swish-test "--progress test --tag silly --not broken --tag triv --tag broken --tag triv " test-file)
   '("pass *a1"
     "pass *a2"
     "SKIP *a3" ;; since --not broken
     "SKIP *a4" ;; since it doesn't match any included tags
     "pass *a5"
     "Tests run: 3   Pass: 3   Fail: 0   Skip: 2"))
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
   '("Exception in run-mat: mat this-test-does-not-exist is not defined"))
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
    " *pass *t1"
    " *pass *t2"
    (pregexp-quote file2)
    " *pass *a1"
    " *pass *a2"
    (pregexp-quote file3)
    " *pass *a1"
    " *pass *a2"
    " *FAIL *bad"))
  (nonzero-exit
   (swish-test "--progress suite " (path-combine (output-dir) test-dir0))
   (list
    (format "~a *pass$" (pregexp-quote file1))
    (format "~a *pass$" (pregexp-quote file2))
    (format "~a *fail$" (pregexp-quote file3)))))

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
    ;; suites appear in sorted order
    (pregexp-quote common-name-file)
    " *pass *t1"
    " *FAIL *t2"
    "Tests run: 2   Pass: 1   Fail: 1   Skip: 0"
    (pregexp-quote test-file)
    (format " *pass *~a" (pregexp-quote (symbol->string common-name)))
    " *pass *a1"
    " *FAIL *a2"
    " *FAIL *a3"
    "Tests run: 4   Pass: 2   Fail: 2   Skip: 0"))
  (zero-exit
   ;; common-name explicitly identified as a test
   ;; zero exit means:
   ;;  1. we didn't run common-name as a suite
   ;;  2. we ran only the test named common-name
   (swish-test "--progress test " test-file " -t " common-name)
   (list
    (pregexp-quote test-file)
    (format " *pass *~a" (pregexp-quote (symbol->string common-name)))
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
  (define test-file2 (write-test-file "test-report2.ms" void))
  (define (check results test-name type tags message)
    (match-let*
     ([,mr (find (lambda (mr) (equal? test-name (mat-result-test mr))) results)]
      [#t (mat-result? mr)]
      [,@type (mat-result-type mr)]
      [,@tags (mat-result-tags mr)]
      [,@message (mat-result-message mr)])
     'ok))
  (nonzero-exit
   (swish-test "--progress none --report " report-file " " test-file)
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file)]
    [,content (utf8->string (read-file report-file))]
    [(,_ ,rest) (pregexp-match ".*<tr>[<td/>]*<td>TEST1</td><td>.*PASS(.*)" content)]
    [(,_ ,rest) (pregexp-match ".*<tr>[<td/>]*<td>TEST2</td><td>.*Exception: failed assertion #f at line 3(.*)" rest)]
    [(,_) (pregexp-match ".*<tr>[<td/>]*<td>TEST3</td><td>.*Exception in cdr: 17 is not a pair" rest)])
   'ok)
  ;; check .mo results file
  (match-let*
   ([,mo-path (string-append test-file ".mo")]
    [,results (load-results mo-path)])
   (check results 'TEST1 'pass '(good) "")
   (check results 'TEST2 'fail '(tag1 tag2)
     (format "Exception: failed assertion #f at line 3, char 32 of ~a" test-file))
   (check results 'TEST3 'fail '() "Exception in cdr: 17 is not a pair")
   'ok)
  ;; all pass, but some skipped so nonzero exit
  (nonzero-exit
   (swish-test "--progress none --tag good --report " report-file1 " " test-file)
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file1)]
    [,content (utf8->string (read-file report-file1))]
    [(,_) (pregexp-match (pregexp-quote "passed 1 tests (skipped 2)") content)])
   'ok)
  ;; all pass, some skipped, but test specified explicitly so zero exit
  (zero-exit
   (swish-test "--progress none --tag good --report " report-file2 " " test-file " TEST1")
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file2)]
    [,content (utf8->string (read-file report-file2))]
    [(,_) (pregexp-match (pregexp-quote "PASSED all 1 tests") content)])
   'ok)
  ;; some skipped so nonzero exit, no results for test-file2
  (nonzero-exit
   (swish-test "--progress none --tag good --report " report-file3 " " test-file " " test-file2)
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file3)]
    [,content (utf8->string (read-file report-file3))]
    [(,_ ,rest)
     (pregexp-match (string-append (pregexp-quote "passed 1 tests (skipped 2)") "(.*)") content)]
    [(,_ ,rest) (pregexp-match ".*<tr>[<td/>]*<td>TEST1</td><td>.*PASS(.*)" rest)]
    [(,_) (pregexp-match ".*<tr><td>.*test-report2.ms</td><td></td><td>.*no results" rest)])
   'ok)
  ;; tests fail, some skipped
  (nonzero-exit
   (swish-test "--progress none --not good --report " report-file4 " " test-file)
   '())
  ;; check HTML report output
  (match-let*
   ([#t (file-exists? report-file4)]
    [,content (utf8->string (read-file report-file4))]
    [(,_) (pregexp-match (pregexp-quote "Failed 2 of 2 tests (skipped 1)") content)])
   'ok)
  )

(isolate-mat file-extension ()
  (define test-file (write-test-file "no-tests.ms" void))
  (define source-file (write-test-file "no-tests.ss" void))
  (nonzero-exit
   ;; complain if .ms file contains no tests
   (swish-test test-file)
   `("Tests run: 0   Pass: 0   Fail: 0   Skip: 0"
     "No tests found in"
     ,(pregexp-quote test-file)))
  (zero-exit
   ;; don't complain if non-.ms file contains no tests
   (swish-test source-file)
   '()))

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
  (define source-file
    (write-test-file "profile-code.ss"
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
      '("see file:.*cov/summary.html"
        "Tests run: 0   Pass: 0   Fail: 0   Skip: 0")))
    (match-let*
     ([#t (file-regular? coverage-file)]
      [,cov (utf8->string (read-file coverage-file))]
      [(,_) (pregexp-match "Overall 74% coverage with 17 of 23 sites covered" cov)]
      [(,_) (pregexp-match (summary-row source-file-rel 17 23 74 12) cov)]
      [,ss.html (path-combine (path-parent coverage-file) (string-append source-file-rel ".html"))]
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
    '("see file:.*cov1/summary.html"
      "Tests run: 1   Pass: 1   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage1-file)]
    [,cov1 (utf8->string (read-file coverage1-file))]
    [(,_) (pregexp-match "Overall 45% coverage with 18 of 40 sites covered" cov1)]
    [(,_) (pregexp-match (summary-row source-file-rel 18 40 45 1) cov1)]
    [,ss.html (path-combine (path-parent coverage1-file) (string-append source-file-rel ".html"))]
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
    '(" pass *test5"
      "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"))
   (zero-exit
    (swish-test "--load-profile " profile2-file " --coverage " coverage1-file)
    '("see file:.*cov1/summary.html"
      "Tests run: 0   Pass: 0   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage1-file)]
    [,cov1 (utf8->string (read-file coverage1-file))]
    [(,_) (pregexp-match "Overall 70% coverage with 28 of 40 sites covered" cov1)]
    [(,_) (pregexp-match (summary-row source-file-rel 28 40 70 6) cov1)]
    [,ss.html (path-combine (path-parent coverage1-file) (string-append source-file-rel ".html"))]
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
   '("see file:.*cov1/summary.html"
     "Tests run: 0   Pass: 0   Fail: 0   Skip: 0"))
  (match-let*
   ([#t (file-regular? coverage1-file)]
    [,cov1 (utf8->string (read-file coverage1-file))]
    [(,_) (pregexp-match "Overall 85% coverage with 34 of 40 sites covered" cov1)]
    [(,_) (pregexp-match (summary-row source-file-rel 34 40 85 7) cov1)]
    [,ss.html (path-combine (path-parent coverage1-file) (string-append source-file-rel ".html"))]
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
    '(" pass *a1"
      "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"
      " pass *test0"
      " pass *test5"
      "Tests run: 2   Pass: 2   Fail: 0   Skip: 0"
      "see file:.*cov/summary.html"
      "Tests run: 3   Pass: 3   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage-file)]
    [,cov (utf8->string (read-file coverage-file))]
    [(,_) (pregexp-match "Overall 85% coverage with 34 of 40 sites covered" cov)]
    [(,_) (pregexp-match (summary-row source-file-rel 34 40 85 31) cov)]
    [,ss.html (path-combine (path-parent coverage-file) (string-append source-file-rel ".html"))]
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
    '("see file:.*cov/summary.html"
      "Tests run: 0   Pass: 0   Fail: 0   Skip: 0")))
  (match-let*
   ([#t (file-regular? coverage-file)]
    [,cov (utf8->string (read-file coverage-file))]
    [(,_) (pregexp-match "Overall 85% coverage with 34 of 40 sites covered" cov)]
    [(,_) (pregexp-match (summary-row source-file-rel 34 40 85 (+ 31 7)) cov)]
    [,ss.html (path-combine (path-parent coverage-file) (string-append source-file-rel ".html"))]
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
   '(" pass *b9"
     "Tests run: 1   Pass: 1   Fail: 0   Skip: 0"))
  (nonzero-exit
   (swish-test test-file)
   (map pregexp-quote
     (list
      "Exception: library (help) not found"
      (format "Test Failed: ~a" test-file)))))
