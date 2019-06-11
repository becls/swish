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

(library (swish mat)
  (export
   $run-mats
   add-mat
   for-each-mat
   load-results
   mat
   mat-data
   mat-data-meta-data
   mat-data-report-file
   mat-data-results
   mat-data?
   mat-result
   mat-result-message
   mat-result-sstats
   mat-result-tags
   mat-result-test
   mat-result-test-file
   mat-result-type
   mat-result?
   meta-data
   meta-data-key
   meta-data-test-file
   meta-data-value
   meta-data?
   run-mat
   run-mats
   run-mats-to-file
   summarize
   summarize-results
   write-meta-data
   )
  (import (chezscheme))

  (define mats (make-parameter '()))

  (define-record-type (%mat make-mat mat?)
    (nongenerative)
    (fields
     (immutable name)
     (immutable tags)
     (immutable test)))

  (define-record-type mat-data
    (nongenerative)
    (fields
     (immutable report-file)
     (immutable meta-data)
     (immutable results)))

  (define-record-type meta-data
    (nongenerative)
    (fields
     (immutable test-file)
     (immutable key)
     (immutable value)))

  (define-record-type mat-result
    (nongenerative)
    (fields
     (immutable test-file)
     (immutable test)
     (immutable tags)
     (immutable type)
     (immutable message)
     (immutable sstats)))

  (define-syntax mat
    (syntax-rules ()
      [(_ name (tag ...) e1 e2 ...)
       (add-mat 'name '(tag ...) (lambda () e1 e2 ...))]))

  (define (all-mats) (reverse (mats)))

  (define (find-mat name)
    (find (lambda (m) (eq? name (%mat-name m))) (mats)))

  (define (add-mat name tags test)
    (if (find-mat name)
        (errorf 'add-mat "mat ~a is already defined." name)
        (mats (cons (make-mat name tags test) (mats)))))

  (define run-mat
    (case-lambda
     [(mat/name reporter) (run-mat mat/name reporter '() '())]
     [(mat/name reporter incl-tags excl-tags)
      (cond
       [(mat? mat/name) (do-run-mat mat/name reporter incl-tags excl-tags)]
       [(find-mat mat/name) => (lambda (mat) (do-run-mat mat reporter incl-tags excl-tags))]
       [else (errorf 'run-mat "mat ~a is not defined." mat/name)])]))

  (define (skip? mat incl-tags excl-tags)
    (or (and (pair? incl-tags) (not (present (%mat-tags mat) incl-tags)))
        (and (pair? excl-tags) (present (%mat-tags mat) excl-tags))))

  (define (do-run-mat mat reporter incl-tags excl-tags)
    (let* ([skip (and (skip? mat incl-tags excl-tags) 'skip)]
           [before (statistics)]
           [result
            (or skip
                (guard (e [else (cons 'fail e)])
                  ((%mat-test mat))
                  'pass))])
      (reporter (%mat-name mat) (%mat-tags mat) result
        (sstats-difference (statistics) before))))

  (define (present tags tag-list)
    (ormap (lambda (tag) (memq tag tag-list)) tags))

  (define (for-each-mat procedure)
    (for-each (lambda (mat) (procedure (%mat-name mat) (%mat-tags mat)))
      (all-mats)))

  (define-syntax run-mats
    (syntax-rules ()
      [(_) ($run-mats #f)]
      [(_ name1 name2 ...) ($run-mats '(name1 name2 ...))]))

  (define (result-type x)
    (cond
     [(eq? x 'pass) 'pass]
     [(eq? x 'skip) 'skip]
     [(and (pair? x) (eq? (car x) 'fail)) 'fail]
     [else #f]))

  (define (extract x)
    (if (condition? x)
        (let-values ([(op get) (open-string-output-port)])
          (display-condition x op)
          (get))
        (format "~s" x)))

  (define (print-col col1 col2 col3)
    (printf " ~8a ~14a ~a\n" col1 col2 col3))

  (define $run-mats
    (case-lambda
     [(mat/names) ($run-mats mat/names #f '() '() #f 'test) (void)]
     [(mat/names test-file incl-tags excl-tags mo-op progress)
      (let-values ([(update-tally! get-tally) (make-tally-reporter)])
        (define progress-reporter (if (eq? progress 'test) (make-progress-reporter progress) NOP))
        (define write-summary (if mo-op (make-write-summary mo-op) NOP))
        (define (reporter name tags result sstats)
          (define r
            (case (result-type result)
              [(pass skip) (make-mat-result test-file name tags result "" sstats)]
              [(fail) (make-mat-result test-file name tags 'fail (extract (cdr result)) sstats)]
              [else (errorf '$run-mats "unknown result ~s" result)]))
          (update-tally! r)
          (progress-reporter r)
          (write-summary r))
        (define (sep) (display "-------------------------------------------------------------------------\n"))
        (case progress
          [none (void)]
          [suite (printf "~40a" test-file)]
          [test
           (when test-file (printf "~a\n" test-file))
           (sep)
           (print-col "Result" "Test name" "Message")
           (sep)])
        (flush-output-port)
        (for-each
         (lambda (mat/name)
           (run-mat mat/name reporter incl-tags excl-tags))
         (or mat/names (all-mats)))
        (let-values ([(pass fail skip) (get-tally)])
          (case progress
            [none (void)]
            [suite
             (cond
              [(= pass fail skip 0) (printf "no tests\n")]
              [(> fail 0) (printf "fail\n")]
              [(> pass 0) (printf "pass~[~:;     (skipped ~s)~]\n" skip skip)]
              [else (printf "skipped\n")])]
            [test
             (sep)
             (printf "Tests run: ~s   Pass: ~s   Fail: ~s   Skip: ~s\n\n"
               (+ pass fail) pass fail skip)])
          (flush-output-port)
          `((pass ,pass) (fail ,fail) (skip ,skip))))]))

  (define (NOP r) (void))

  (define (make-tally-reporter)
    (define pass 0)
    (define fail 0)
    (define skip 0)
    (values
     (lambda (r)
       (case (mat-result-type r)
         [pass (set! pass (+ pass 1))]
         [fail (set! fail (+ fail 1))]
         [skip (set! skip (+ skip 1))]
         [else (errorf 'tally-reporter "unknown result type in ~s" r)]))
     (lambda () (values pass fail skip))))

  (define (make-progress-reporter progress)
    (lambda (r)
      (case (mat-result-type r)
        [pass (print-col "pass" (mat-result-test r) "")]
        [fail (print-col "FAIL" (mat-result-test r) (mat-result-message r))]
        [skip (print-col "SKIP" (mat-result-test r) "")]
        [else (errorf 'test-progress-reporter "unknown result ~s" r)])
      (flush-output-port)))

  (define sstats (statistics))
  (define sstats-rtd (record-rtd sstats))
  (define time-rtd (record-rtd (sstats-real sstats)))
  (define default-record-writer (record-writer sstats-rtd))
  ;; make adapter so we can use parameterize syntax for dynamic-wind
  (define (make-param record-io-param key)
    (case-lambda
     [() (record-io-param key)]
     [(v) (record-io-param key v)]))
  (define time-writer (make-param record-writer time-rtd))
  (define (make-record-reader rtd) (make-param record-reader (record-type-name rtd)))
  (define time-reader (make-record-reader time-rtd))
  (define sstats-reader (make-record-reader sstats-rtd))
  (define mat-data-rtd (record-type-descriptor mat-data))
  (define mat-data-reader (make-record-reader mat-data-rtd))
  (define mat-result-rtd (record-type-descriptor mat-result))
  (define mat-result-reader (make-record-reader mat-result-rtd))
  (define meta-data-rtd (record-type-descriptor meta-data))
  (define meta-data-reader (make-record-reader meta-data-rtd))

  (define (write-meta-data op test-file key value)
    (parameterize ([print-gensym #f])
      (fprintf op "~s\n" (make-meta-data test-file key value))))

  (define (make-write-summary op)
    (lambda (r)
      ;; sstats already uses default-record-writer
      (parameterize ([print-gensym #f] [time-writer default-record-writer])
        (fprintf op "~s\n" r))))

  (define (run-mats-to-file filename)
    ($run-mats-to-file filename (all-mats) '() '()))

  (define ($run-mats-to-file filename mat/names incl-tags excl-tags)
    (call-with-output-file filename
      (lambda (op)
        (define reporter (make-write-summary op))
        (for-each
         (lambda (mat/name)
           (run-mat mat/name reporter incl-tags excl-tags))
         (or mat/names (all-mats))))
      'replace))

  (define (load-results filename)
    (parameterize ([mat-data-reader mat-data-rtd]
                   [mat-result-reader mat-result-rtd]
                   [meta-data-reader meta-data-rtd]
                   [sstats-reader sstats-rtd]
                   [time-reader time-rtd])
      (call-with-input-file filename
        (lambda (ip)
          (let lp ([meta-data '()] [results '()])
            (let ([x (read ip)])
              (cond
               [(eof-object? x)
                (make-mat-data filename (reverse meta-data) (reverse results))]
               [(meta-data? x) (lp (cons x meta-data) results)]
               [(mat-result? x) (lp meta-data (cons x results))]
               [else (raise `#(unexpected-mat-result ,x))])))))))

  (define (summarize-results results*)
    (let-values ([(update-tally! get-tally) (make-tally-reporter)])
      (for-each
       (lambda (data)
         (for-each update-tally! (mat-data-results data)))
       results*)
      (get-tally)))

  (define (summarize files)
    (summarize-results (map load-results files)))

  )
