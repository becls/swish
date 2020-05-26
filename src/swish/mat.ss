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
   mat-result-meta-data
   mat-result-sstats
   mat-result-stacks
   mat-result-tags
   mat-result-test
   mat-result-test-file
   mat-result-type
   mat-result?
   mat:add-annotation!
   run-mat
   run-mats
   run-mats-to-file
   summarize
   summarize-results
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish errors)
   (swish io)
   (swish json)
   (swish log-db)
   (swish meta)
   (swish osi)
   (swish software-info)
   (swish string-utils))

  (define mats (make-parameter '()))

  (define-record-type (%mat make-mat mat?)
    (nongenerative)
    (sealed #t)
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
    meta-data
    stacks
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
       (add-mat 'name '(tag ...) (lambda () e1 e2 ... (void)))]))

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
    (parameterize ([mat-annotations '()])
      (let* ([skip (and (skip? mat incl-tags excl-tags) 'skip)]
             [before (statistics)]
             [result
              (or skip
                  (let ([ignore #f])
                    (guard (e [else (list 'fail e ignore)])
                      (call/cc
                       (lambda (k)
                         (set! ignore k)
                         (mat-start-time (erlang:now))
                         ((%mat-test mat))))
                      'pass)))])
        (mat-end-time (erlang:now))
        (reporter (%mat-name mat) (%mat-tags mat) result
          (sstats-difference (statistics) before)))))

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

  (define (print-col col1 col2 col3)
    (printf " ~8a ~14a ~a\n" col1 col2 col3))

  (define ($init-mat-output-file report-file test-file uuid)
    (let ([op (open-file-to-replace report-file)])
      (on-exit (close-port op)
        (write-meta-data op 'completed #f)
        (write-meta-data op 'test-file test-file)
        (write-meta-data op 'hostname (osi_get_hostname))
        (write-meta-data op 'machine-type (symbol->string (machine-type)))
        (write-meta-data op 'test-run uuid)
        (write-meta-data op 'uname
          (match (get-uname)
            [`(<uname> ,system ,release ,version ,machine)
             (json:make-object
              [os-machine machine]
              [os-release release]
              [os-system system]
              [os-version version])]))))
    (void))

  (define mat-annotations (make-parameter #f))
  (define mat-start-time (make-parameter #f))
  (define mat-end-time (make-parameter #f))

  (define-syntax (mat:add-annotation! x)
    (syntax-case x ()
      [(_ expr)
       #`(add-annotation! #,(find-source x) expr)]))

  (define (add-annotation! src datum)
    (let ([timestamp (erlang:now)])
      (with-interrupts-disabled
       (let ([anno (mat-annotations)])
         (unless anno
           (errorf 'mat:add-annotation! "must be called only while running a mat"))
         (mat-annotations
          (cons
           (json:make-object
            [src
             (match src
               [#(at ,bfp ,path) (json:make-object [bfp bfp] [path path])]
               [#f #f])]
            [timestamp timestamp]
            [value datum])
           anno))))))

  (define-json result-meta-data start-time end-time annotations)

  (define (get-result-meta-data)
    (make-result-meta-data
     (mat-start-time)
     (mat-end-time)
     (reverse (mat-annotations))))

  (define $run-mats
    (case-lambda
     [(mat/names) ($run-mats mat/names #f '() '() #f 'test) (void)]
     [(mat/names test-file incl-tags excl-tags mo-op progress)
      (let-values ([(update-tally! get-tally) (make-tally-reporter)]
                   [(write-header write-result write-tally) (progress-reporter progress)])
        (define write-summary (if mo-op (make-write-summary mo-op) NOP))
        (define (reporter name tags result sstats)
          (define r
            (case (result-type result)
              [(pass skip) (make-mat-result test-file name tags result "" (get-result-meta-data) '() sstats)]
              [(fail)
               (match (cdr result)
                 [(,e ,ignore)
                  (make-mat-result test-file name tags 'fail
                    (exit-reason->english e)
                    (get-result-meta-data)
                    (map stack->json (remq ignore (exit-reason->stacks e)))
                    sstats)])]
              [else (errorf '$run-mats "unknown result ~s" result)]))
          (update-tally! r)
          (write-result r)
          (write-summary r))
        (write-header test-file)
        (flush-output-port)
        (when mo-op
          ;; record revision information after loading mats, but before running mats
          (write-meta-data mo-op 'software-info (software-info))
          (write-meta-data mo-op 'date (format-rfc2822 (current-date)))
          (write-meta-data mo-op 'timestamp (erlang:now))
          (flush-output-port mo-op))
        (for-each
         (lambda (mat/name)
           (run-mat mat/name reporter incl-tags excl-tags))
         (or mat/names (all-mats)))
        (when mo-op
          (write-meta-data mo-op 'completed #t)
          (flush-output-port mo-op))
        (let-values ([(pass fail skip) (get-tally)])
          (write-tally pass fail skip)
          (flush-output-port)
          `((pass ,pass) (fail ,fail) (skip ,skip))))]))

  (define (NOP . _) (void))

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

  (define (sep) (display "-------------------------------------------------------------------------\n"))
  (define (write-totals pass fail skip)
    (printf "Tests run: ~s   Pass: ~s   Fail: ~s   Skip: ~s\n\n"
      (+ pass fail) pass fail skip))
  (define (progress-reporter progress)
    (case progress
      [(none summary) (values NOP NOP NOP)]
      [(suite)
       (values
        ;; write-header
        (lambda (test-file)
          (printf "~39a " test-file))
        ;; write-result
        NOP
        ;; write-tally
        (lambda (pass fail skip)
          (cond
           [(= pass fail skip 0) (printf "no tests\n")]
           [(> fail 0) (printf "fail\n")]
           [(> pass 0) (printf "pass~[~:;     (skipped ~s)~]\n" skip skip)]
           [else (printf "skipped\n")])))]
      [(test)
       (values
        ;; write-header
        (lambda (test-file)
          (when test-file (printf "~a\n" test-file))
          (sep)
          (print-col "Result" "Test name" "Message")
          (sep))
        ;; write-result
        (lambda (r)
          (case (mat-result-type r)
            [pass (print-col "pass" (mat-result-test r) "")]
            [fail (print-col "FAIL" (mat-result-test r) (mat-result-message r))]
            [skip (print-col "SKIP" (mat-result-test r) "")]
            [else (errorf 'test-progress-reporter "unknown result ~s" r)])
          (flush-output-port))
        ;; write-tally
        (lambda (pass fail skip)
          (sep)
          (write-totals pass fail skip)))]
      [else (match progress)]))

  (define-json meta-kv key value)

  (define (write-meta-data op key value)
    (json:write-object op 0 json:write
      [_type_ "meta-kv"]
      [key (symbol->string key)]
      [value value]))

  (define (make-write-summary op)
    (lambda (r)
      (json:write op r 0)
      (flush-output-port op)))

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
    (json:set! obj 'results (reverse (json:ref obj 'results '())))
    obj)

  (define summarize-results
    (case-lambda
     [(results*) (summarize-results results* 'none)]
     [(results* progress) (summarize-results results* progress #t)]
     [(results* progress warn-incomplete?)
      (let-values ([(update-total-tally! get-total-tally) (make-tally-reporter)]
                   [(write-header write-result write-tally) (progress-reporter progress)])
        (define completed 0)
        (for-each
         (lambda (data)
           (let-values ([(update-tally! get-tally) (make-tally-reporter)])
             (define (do-result mr)
               (update-tally! mr)
               (update-total-tally! mr)
               (write-result mr))
             (write-header (assert (json:ref (mat-data-meta-data data) 'test-file #f)))
             (for-each do-result (mat-data-results data))
             (let-values ([(pass fail skip) (get-tally)])
               (write-tally pass fail skip))
             (flush-output-port)
             (when (json:ref data '(meta-data completed) #f)
               (set! completed (+ completed 1)))))
         results*)
        (let-values ([(pass fail skip) (get-total-tally)])
          (match progress
            [none (void)]
            [,_ (write-totals pass fail skip)])
          (let ([attempted (length results*)])
            (when (and warn-incomplete? (not (= attempted completed)))
              (printf "*** Some test suite~p did not complete ***\n\n" (- attempted completed)))
            (values pass fail skip completed attempted))))]))

  (define (summarize files)
    (summarize-results (map load-results files) 'none #f))

  )
