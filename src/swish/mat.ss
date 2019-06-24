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
   $init-mat-output-file
   $run-mats
   add-mat
   for-each-mat
   load-results
   mat
   mat-data-meta-data
   mat-data-report-file
   mat-data-results
   mat-data?
   mat-result-message
   mat-result-stack
   mat-result-tags
   mat-result-test
   mat-result-type
   mat-result?
   run-mat
   run-mats
   run-mats-to-file
   summarize
   summarize-results
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish io)
   (swish json)
   (swish meta)
   (swish osi)
   (swish software-info)
   (swish string-utils))

  (define mats (make-parameter '()))

  (define-record-type (%mat make-mat mat?)
    (nongenerative)
    (fields
     (immutable name)
     (immutable tags)
     (immutable test)))

  (define-syntax (define-json x)
    (syntax-case x ()
      [(_ type field ...)
       (let* ([transform-help
               (lambda (field clauses)
                 (let ([clauses (collect-clauses x clauses '(-> <-))])
                   (with-syntax ([(-> out) (or (find-clause '-> clauses) #'(-> values))]
                                 [(<- in) (or (find-clause '<- clauses) #'(<- values))])
                     (list field #'out #'in (compound-id field #'type "-" field)))))]
              [transform-field
               (lambda (f)
                 (syntax-case f ()
                   [field
                    (identifier? #'field)
                    (transform-help #'field '())]
                   [(field clause ...)
                    (transform-help #'field #'(clause ...))]))])
         (with-syntax ([pred? (compound-id #'type #'type "?")]
                       [maker (compound-id #'type "make-" #'type)]
                       [((field -> <- getter) ...)
                        (map transform-field #'(field ...))])
           #`(begin
               (define (maker field ...)
                 (json:make-object
                  [_type_ (symbol->string 'type)]
                  [field (-> field)] ...))
               (define (pred? r)
                 (and (json:object? r)
                      (equal? (symbol->string 'type) (json:ref r '_type_ #f))))
               (define (getter r)
                 (unless (pred? r) (bad-arg 'getter r))
                 (<- (json:ref r 'field #f)))
               ...)))]))

  (define-json mat-result
    test-file
    [test (-> symbol->string) (<- string->symbol)]
    [tags
     (-> (lambda (x) (map symbol->string x)))
     (<- (lambda (x) (map string->symbol x)))]
    [type (-> symbol->string) (<- string->symbol)]
    message
    stack
    [sstats
     (->
      (lambda (sstats)
        (json:make-object
         [cpu (time-duration (sstats-cpu sstats))]
         [real (time-duration (sstats-real sstats))]
         [bytes (sstats-bytes sstats)]
         [gc-count (sstats-gc-count sstats)]
         [gc-cpu (time-duration (sstats-gc-cpu sstats))]
         [gc-real (time-duration (sstats-gc-real sstats))]
         [gc-bytes (sstats-gc-bytes sstats)])))])

  (define (time-duration t)
    (+ (time-second t)
       (/ (time-nanosecond t) 1000000000.0)))

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

  (define (stack x)
    (if (continuation-condition? x)
        (let-values ([(op get) (open-string-output-port)])
          (dump-stack (condition-continuation x) op 'default)
          (get))
        ""))

  (define (print-col col1 col2 col3)
    (printf " ~8a ~14a ~a\n" col1 col2 col3))

  (define ($init-mat-output-file report-file test-file uuid)
    (let ([op (open-file-to-replace report-file)])
      (on-exit (close-port op)
        (write-meta-data op 'completed #f)
        (write-meta-data op 'test-file test-file)
        (write-meta-data op 'hostname (osi_get_hostname))
        (write-meta-data op 'machine-type (symbol->string (machine-type)))
        (write-meta-data op 'test-run uuid))))

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
              [(pass skip) (make-mat-result test-file name tags result "" "" sstats)]
              [(fail) (make-mat-result test-file name tags 'fail (extract (cdr result)) (stack (cdr result)) sstats)]
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
        (when mo-op
          ;; record revision information after loading mats, but before running mats
          (write-meta-data mo-op 'software-info (software-info))
          (write-meta-data mo-op 'date (format-rfc2822 (current-date)))
          (write-meta-data mo-op 'timestamp (erlang:now)))
        (for-each
         (lambda (mat/name)
           (run-mat mat/name reporter incl-tags excl-tags))
         (or mat/names (all-mats)))
        (when mo-op (write-meta-data mo-op 'completed #t))
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

  (define-json meta-kv key value)

  (define (write-meta-data op key value)
    (json:write-object op 0 json:write
      [_type_ "meta-kv"]
      [key (symbol->string key)]
      [value value]))

  (define (make-write-summary op)
    (lambda (r)
      (json:write op r 0)))

  (define (run-mats-to-file filename)
    (define test-file (string-append "to " filename))
    ($init-mat-output-file filename test-file (uuid->string (osi_make_uuid)))
    (let ([mo-op (open-file-to-append filename)])
      (on-exit (close-port mo-op)
        ($run-mats #f test-file '() '() mo-op 'test))))

  (define-json mat-data report-file meta-data results)

  (define (load-results filename)
    (define meta-data (json:make-object))
    (define obj (make-mat-data filename meta-data '()))
    (let ([ip (open-file-to-read filename)])
      (let rd ()
        (let ([r (json:read ip)])
          (unless (eof-object? r)
            (match (json:ref r '_type_ #f)
              ["mat-result"
               (json:update! obj 'results (lambda (old) (cons r old)) #f)]
              ["meta-kv"
               (json:set! meta-data (string->symbol (meta-kv-key r)) (meta-kv-value r))])
            (rd)))))
    obj)

  (define (summarize-results results*)
    (let-values ([(update-tally! get-tally) (make-tally-reporter)]
                 [(completed) 0])
      (for-each
       (lambda (data)
         (for-each update-tally! (mat-data-results data))
         (when (json:ref data '(meta-data completed) #f)
           (set! completed (+ completed 1))))
       results*)
      (let-values ([(pass fail skip) (get-tally)])
        (values pass fail skip completed (length results*)))))

  (define (summarize files)
    (summarize-results (map load-results files)))

  )
