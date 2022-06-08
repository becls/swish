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

(import
 (chezscheme)
 (swish erlang)
 (swish log-db)
 (swish mat)
 (swish script-testing)
 (swish testing)
 )

(parameterize ([library-extensions '((".ss" . ".so"))])
  (eval '(import (swish internal-testing))))

(system-mat m1 ()
  'ok)

(mat try-import ()
  (define (check-library-path lib)
    (match lib
      [(swish . ,_) 'ok]
      [(chezscheme) 'ok]
      [(scheme) 'ok]))
  ;; Check that the naming convention expected by (try-import) in app.ss
  ;; is compatible with (import (swish imports)).
  (for-each check-library-path
    (library-requirements '(swish imports)
      (library-requirements-options import))))

;; make sure we review changes to the set of libraries in swish-core.library
(mat core-library ()
  (define enumerate-libs-expr
    `(lambda (libs)
       (let ([sorted
              (sort string<?
                (map (lambda (lib) (format "~s" lib)) libs))])
         (do ([i 0 (+ i 1)] [ls sorted (cdr ls)] [sep "" "\n"]) ((null? ls))
           (printf "~a~a ~a" sep i (car ls))))))
  (match-let*
   ([,expected
     (let ([os (open-output-string)])
       (parameterize ([current-output-port os])
         ((eval enumerate-libs-expr)
          '((chezscheme)
            (swish app-core)
            (swish erlang)
            (swish errors)
            (swish internal)
            (swish io)
            (swish osi)
            (swish meta))))
       (split (get-output-string os) #\newline))]
    [`(<os-result> [stdout ,@expected] [exit-status 0])
     ;; load swish-core.library into stock scheme and look at (library-list)
     (run-os-process scheme-exe '("-q")
       (lambda (op)
         (for-each (lambda (x) (pretty-print x op))
           `((load ,(path-combine (prereq-path) "swish-core.library"))
             (,enumerate-libs-expr
              (remp (lambda (x)
                      (or (eq? (car x) 'rnrs)
                          (member x '((chezscheme csv7) (scheme) (scheme csv7)))))
                (library-list)))
             (exit)))
         (flush-output-port op))
       #f
       '())])
   'ok))

(mat app-exception-handler ()
  (define (unwrap-debug-condition)
    (find (lambda (x) (match x [`(&fault-condition) x] [,_ #f]))
      (simple-conditions (debug-condition))))
  (define (go who val)
    (call/cc
     (lambda (k)
       (let ([os (open-output-string)])
         (parameterize ([app:name who]
                        [console-error-port os]
                        [reset-handler
                         (lambda ()
                           (assert
                            (or (eq? val (debug-condition))
                                (eq? val (unwrap-debug-condition))))
                           (k (get-output-string os)))])
           (app-exception-handler val))
         "failed to invoke reset-handler"))))
  (define (repl-as who . inputs)
    (let ([os (open-output-string)])
      (parameterize ([app:name who]
                     [console-input-port
                      (open-input-string (format "~{~a\n~}" inputs))]
                     [console-output-port os]
                     [console-error-port os]
                     [pretty-one-line-limit 1024])
        (new-cafe)
        (split (get-output-string os) #\newline))))
  (match-let*
   (["Exception: foo.\n"
     ;; when app:name is not set, default exception handler prints "Exception:"
     (go #f 'foo)]
    ["bummer: foo.\n"
     ;; when app:name is set, we get app-exception-handler
     (go "bummer" 'foo)]
    ["oops: Invalid datum: \"flobble\".\n"
     (go "oops" '#(invalid-datum "flobble"))]
    ["dang: HTTP handler failed: Exception in cdr: 2 is not a pair.\n"
     ;; nested exit-reason->english with native exception
     (go "dang" `#(http-handler-failed ,(guard (c [else c]) (cdr 2))))]
    ["dang: HTTP handler failed: Exception in cdr: 2 is not a pair.\n"
     ;; nested exit-reason->english with non-condition
     (go "dang" `#(http-handler-failed ,(guard (c [else c]) (cdr 2))))]
    ["rats: HTTP handler failed: flak.\n"
     ;; nested exit-reason->english with caught non-condition
     (go "rats" `#(http-handler-failed ,(catch 'flak)))]
    ["wow: (this happened).\n"
     ;; current-exit-reason->english unchanged
     (go "wow" '(this happened))]
    ["dude: my bad.\n"
     ;; vague current-exit-reason->english
     (parameterize ([current-exit-reason->english (lambda (x) "my bad")])
       (go "dude" '(this happened)))]
    ["wow: Exception occurred with non-condition value (this happened).\n"
     ;; failure in current-exit-reason->english falls back to display-condition
     (parameterize ([current-exit-reason->english
                     (lambda args
                       (throw "internal error"))])
       (go "wow" '(this happened)))]
    ["hey: 123 is not a string.\n"
     ;; native Error condition
     (go "hey" (guard (c [else c]) (string-upcase 123)))]
    ["ahem: 123 is not a string.\n"
     ;; catch of native Error condition
     (go "ahem" (catch (string-upcase 123)))]
    ["uh: 123 is not a string.\n"
     ;; try of native Error condition
     (go "uh" (try (string-upcase 123)))]
    [()
     ;; anonymous: native Warning condition
     (match-regexps
      '("Warning: overheating."
        "^$")
      (repl-as #f "(warning #f \"overheating\")"))]
    [()
     ;; named: native Warning condition
     (match-regexps
      '("engine: overheating."
        "^$")
      (repl-as "engine" "(warning #f \"overheating\")"))]
    ["my: message.\n"
     ;; native message condition
     (go "my" (make-message-condition "message"))])
   'ok)
  (match-let*
   ([,script
     (format "~{~s\n~}"
       '((define x (catch (cdr 0)))
         x
         (match x [`(catch ,r ,e) (set! last-err e) (raise e)])))]
    [()
     ;; anonymous: raise err from catch
     (match-regexps
      '("#\\(EXIT #<compound condition.*>\\)"
        "Exception in cdr: 0 is not a pair."
        "^$")
      (repl-as #f script))]
    [#t (eq? (debug-condition) last-err)]
    [()
     ;; named: raise err from catch
     (match-regexps
      '("#\\(EXIT #<compound condition.*>\\)"
        "my-app: 0 is not a pair."
        "^$")
      (repl-as "my-app" script))]
    [#t (eq? (debug-condition) last-err)])
   'ok)
  (match-let*
   ([,script
     (format "~{~s\n~}"
       '((define x (catch (cdr 1)))
         x
         (match x [`(catch ,r ,e) (set! last-err e) (throw e)])))]
    [()
     ;; anonymous: err from catch
     (match-regexps
      '("#\\(EXIT #<compound condition.*>\\)"
        "Exception in cdr: 1 is not a pair."
        "^$")
      (repl-as #f script))]
    [#t
     (eq?
      (match (unwrap-debug-condition) [`(&fault-condition ,reason) reason])
      last-err)]
    [()
     ;; named: err from catch
     (match-regexps
      '("#\\(EXIT #<compound condition.*>\\)"
        "my-app: 1 is not a pair."
        "^$")
      (repl-as "my-app" script))]
    [#t
     (eq?
      (match (unwrap-debug-condition) [`(&fault-condition ,reason) reason])
      last-err)]
    [(,k0 ,k1) (exit-reason->stacks (unwrap-debug-condition))]
    [,@k0 (condition-continuation (debug-condition))])
   'ok)
  (match-let*
   ([,script
     (format "~{~s\n~}"
       '((define x (try (car 2)))
         x
         (match x [`(catch ,r ,e) (set! last-err e) (raise e)])))]
    [()
     ;; anonymous: raise err from try
     (match-regexps
      '("#<fault #<compound condition.*>>"
        "Exception in car: 2 is not a pair."
        "^$")
      (repl-as #f script))]
    [#t (eq? (debug-condition) last-err)]
    [()
     ;; named: raise err from try
     (match-regexps
      '("#<fault #<compound condition.*>>"
        "my-app: 2 is not a pair."
        "^$")
      (repl-as "my-app" script))]
    [#t (eq? (debug-condition) last-err)]
    [(,k0) (exit-reason->stacks last-err)]
    [,@k0 (condition-continuation (debug-condition))])
   'ok)
  (match-let*
   ([,script
     (format "~{~s\n~}"
       '((define x (try (throw 'overboard)))
         x
         (match x [`(catch ,r ,e) (set! last-err e) (raise e)])))]
    [()
     ;; anonymous: raise thrown from try
     (match-regexps
      '("#<fault overboard>"
        "Exception: overboard."
        "^$")
      (repl-as #f script))]
    [#t (eq? (unwrap-debug-condition) last-err)]
    [()
     ;; named: raise thrown from try
     (match-regexps
      '("#<fault overboard>"
        "my-app: overboard."
        "^$")
      (repl-as "my-app" script))]
    [#t (eq? (unwrap-debug-condition) last-err)]
    [(,k0) (exit-reason->stacks last-err)]
    [,@k0 (condition-continuation (debug-condition))])
   'ok)
  (match-let*
   ([,script
     (format "~{~s\n~}"
       '((define x (try (length 3)))
         x
         (match x [`(catch ,r ,e) (set! last-err e) (throw e)])))]
    [()
     ;; anonymous: throw err from try
     (match-regexps
      '("#<fault #<compound condition.*>>"
        "Exception in length: 3 is not a proper list."
        "^$")
      (repl-as #f script))]
    [#t
     (eq?
      (match (unwrap-debug-condition)
        [`(&fault-condition ,reason [inner* ()]) reason])
      last-err)]
    [()
     ;; named: throw err from try
     (match-regexps
      '("#<fault #<compound condition.*>>"
        "my-app: 3 is not a proper list."
        "^$")
      (repl-as "my-app" script))]
    [#t
     (eq?
      (match (unwrap-debug-condition)
        [`(&fault-condition ,reason [inner* ()]) reason])
      last-err)]
    [(,k0 ,k1) (exit-reason->stacks (unwrap-debug-condition))]
    [,@k0 (condition-continuation (debug-condition))])
   'ok)
  (match-let*
   ([()
     ;; anonymous: cyclic structure warning shouldn't prevent repl from printing result
     (match-regexps
      '("Warning in pretty-print: cycle detected; proceeding with \\(print-graph #t\\)."
        "#0=\\(123 . #0#\\)"
        "^$")
      (repl-as #f "'#3=(123 . #3#)"))]
    [()
     ;; named: cyclic structure warning shouldn't prevent repl from printing result
     (match-regexps
      '("yelp: cycle detected; proceeding with \\(print-graph #t\\)."
        "#0=\\(123 . #0#\\)"
        "^$")
      (repl-as "yelp" "'#3=(123 . #3#)"))]
    [()
     ;; anonymous: err with non-condition, unrecognized error
     (match-regexps
      '("Exception: rare." "^$")
      (repl-as #f "(throw 'rare)"))]
    [()
     ;; named: err with non-condition, unrecognized error
     (match-regexps
      '("quite: rare." "^$")
      (repl-as "quite" "(throw 'rare)"))]
    [()
     ;; anonymous: raise cyclic structure
     (match-regexps
      `(,(pregexp-quote "Exception: (1 (1 (1 (...)))).") "^$")
      (repl-as #f "(raise '#0=(1 #0#))"))]
    [()
     ;; named: raise cyclic structure
     (match-regexps
      `(,(pregexp-quote "hmm: (2 (2 (2 (...)))).") "^$")
      (repl-as "hmm" "(raise '#0=(2 #0#))"))]
    [()
     ;; anonymous: err cyclic structure
     (match-regexps
      `(,(pregexp-quote "Exception: (3 (3 (3 (...)))).") "^$")
      (repl-as #f "(throw '#0=(3 #0#))"))]
    [()
     ;; named: err cyclic structure
     (match-regexps
      `(,(pregexp-quote "well: (4 (4 (4 (...)))).") "^$")
      (repl-as "well" "(throw '#0=(4 #0#))"))]
    [()
     ;; anonymous: raise cyclic structure
     (match-regexps
      `(,(pregexp-quote "Exception: (1 2 3 1 2 3 ...).") "^$")
      (repl-as #f "(raise '#0=(1 2 3 . #0#))"))]
    [()
     ;; named: raise cyclic structure
     (match-regexps
      `(,(pregexp-quote "hmm: (1 2 3 1 2 3 ...).") "^$")
      (repl-as "hmm" "(raise '#0=(1 2 3 . #0#))"))]
    [()
     ;; anonymous: err cyclic structure
     (match-regexps
      `(,(pregexp-quote "Exception: (4 3 2 4 3 2 ...).") "^$")
      (repl-as #f "(throw '#0=(4 3 2 . #0#))"))]
    [()
     ;; named: err cyclic structure
     (match-regexps
      `(,(pregexp-quote "well: (4 3 2 4 3 2 ...).") "^$")
      (repl-as "well" "(throw '#0=(4 3 2 . #0#))"))]
    )
   'ok))

(isolate-mat application-shutdown ()
  ;; Make sure app:shutdown doesn't do a (receive).
  ;; If a supervisor malfunctions while we're at the repl,
  ;; we'd like to be able to investigate.
  (match
   (catch
    ;; switch to script-test for better profile coverage
    (script-test #f '()
      (format "~{~s\n~}\n"
        '((define repl self)
          (define (bad-sup:start&link)
            (define (init)
              (process-trap-exit #t)
              `#(ok #f))
            (define (terminate reason state)
              (spawn (lambda () (receive (after 10 (send repl 'locking-up)))))
              (receive [disarm 'ok])
              (cond
               [(whereis 'profiler) =>
                (lambda (pid)
                  (gen-server:call pid 'save 'infinity))]))
            (define (handle-call msg from state) (match msg))
            (define (handle-cast msg state) (match msg))
            (define (handle-info msg state) (match msg))
            (gen-server:start&link 'bad-sup))
          (app-sup-spec
           `(#(bad-sup ,bad-sup:start&link permanent infinity supervisor)))
          (log-file ":memory:")
          (app:start)
          (app:shutdown 2)
          ;; simulate repl interaction after surprising
          ;; non-exit of app:shutdown
          (receive (after 1000 (osi_exit 3))
            [locking-up 'ok])
          (printf "armed and ready\n")
          (send (whereis 'bad-sup) 'disarm)
          ;; Travis ta6osx build occasionally takes > 5s of real time
          (receive (after 10000 (osi_exit 4)))))
      '("expected non-zero exit")))
   [#(EXIT `(<os-process-failed> [exit-status 2] ,stdout))
    (match-regexps '(seek "armed and ready") stdout)]))

(isolate-mat application-shutdown2 ()
  ;; Take care when closing input port if we call app:shutdown more than once.
  (match
   (catch
    (script-test #f '()
      (format "~{~s\n~}\n"
        '((log-file ":memory:")
          (app:start)
          ;; simulate winning race to exit-process with an earlier app:shutdown
          (#%$set-top-level-value! '$console-input-port 'bogus)
          (app:shutdown 1)
          (receive (after 100 (osi_exit 3)))))
      '("expected non-zero exit")))
   [#(EXIT `(<os-process-failed> [exit-status 1] ,stdout ,stderr))
    (match-regexps '("ok") stdout)
    (match-regexps
     '(seek #0="Event: #\\(<child-end> .*\\)" seek #0# seek #0# "") stderr)]
   [#(EXIT `(<os-process-failed> [exit-status 3] ,stderr))
    (printf "~{~a\n~}" stderr)
    (throw 'matched-wrong-case)]))

(isolate-mat application-shutdown3 ()
  ;; Test for regression (unexpected return from reset handler) when a script:
  ;;  1. calls app:start, and
  ;;  2. causes an exception in the main process
  (define script
    (write-test-file "script"
      (lambda ()
        (printf "~{~s\n~}\n"
        '((log-file ":memory:")
          (app:start)
          (cdr 7))))))
  (match (catch (script-test script '() "" '("expected non-zero exit")))
   [#(EXIT `(<os-process-failed> ,stdout ,stderr))
    (match-regexps '() stdout)
    (match-regexps
     '("^script: 7 is not a pair[.]$"
       ""
       "^Date: "
       "^Timestamp: "
       #0="Event: #\\(<child-end> .*\\)"
       seek #0#
       seek #0# "")
     stderr)]))

(isolate-mat application-shutdown4 ()
  ;; Test for regression where application doesn't shut down when we
  ;;  1. start a cafe,
  ;;  2. call app:start, and
  ;;  3. cause an exception in a process linked to the main process
  (match
   (catch
    (script-test #f '()
      (format "~{~s\n~}\n"
        '((log-file ":memory:")
          (app:start)
          (spawn&link (lambda () (raise 'issue)))))
      '("expected non-zero exit")))
   [#(EXIT `(<os-process-failed> [exit-status 2] ,stdout ,stderr))
    (match-regexps '("ok") stdout)
    (match-regexps
     '("^Exception: issue[.]$"
       ""
       "^Date: "
       "^Timestamp: "
       #0="Event: #\\(<child-end> .*\\)"
       seek #0#
       seek #0# "")
     stderr)]))

(meta-cond
 [(memq (machine-type) '(i3nt ti3nt a6nt ta6nt))
  ;; test manually on Windows
  (void)]
 [else
  (isolate-mat default-signal-handlers ()
    (define (test signum)
      (define-values (to-stdin from-stdout from-stderr os-pid)
        (match (osi_get_argv)
          [#()
           (parameterize ([cd "src"])
             (spawn-os-process scheme-exe
               '("--eedisable" "--program" "repl.ss" "-q" "--")
               self))]
          [,_
           (spawn-os-process (osi_get_executable_path) '("-q" "--") self)]))
      (let ([to-stdin (binary->utf8 to-stdin)]
            [from-stdout (binary->utf8 from-stdout)]
            [from-stderr (binary->utf8 from-stderr)]
            [me self])
        (define (dump p)
          (let ([line (get-line p)])
            (unless (eof-object? line)
              (send me (cons p line))
              (dump p))))
        (spawn&link (lambda () (dump from-stderr)))
        (spawn&link (lambda () (dump from-stdout)))
        (pretty-print
         ;; single expression to avoid gatekeeper interaction after app:start
         `(begin
            (log-file ":memory:")
            (app:start)
            (printf "started\n")
            (flush-output-port)
            (receive (after 1000) (exit 3)))
         to-stdin)
        (flush-output-port to-stdin)
        (match-let*
         (["started"
           (receive (after 1500 'bad-start)
             [(,@from-stdout . ,msg) msg])]
          [,! (osi_kill os-pid signum)]
          [,expected-exit-code 0]
          [,expected-signal 0]
          [ok
           (receive (after 1500 'fail)
             [#(process-terminated ,@os-pid ,@expected-exit-code ,@expected-signal)
              'ok])])
         (match-regexps
          '(seek
            "Date: "
            "Timestamp: "
            "Event: #\\(<child-end>.*normal #f\\)"
            seek
            "Date: "
            "Timestamp: "
            "Event: #\\(<child-end>.*shutdown #f\\)"
            seek
            "Date: "
            "Timestamp: "
            "Event: #\\(<child-end>.*shutdown #f\\)")
          (let f ()
            (receive (after 0 '())
              [(,@from-stderr . ,msg) (cons msg (f))])))
         'ok)))
    (test SIGINT)
    (test SIGTERM))])

(mat app-config ()
  (define (fake-path . dirs)
    (apply path-combine (tmp-dir) dirs))
  (define (with-fake-app path thunk)
    (match path
      [(,first . ,rest)
       (on-exit (delete-tree (path-combine (tmp-dir) first))
         (let ([fake-app (make-directory-path (apply fake-path path))])
           (on-exit (delete-file fake-app)
             (with-output-to-file fake-app void)
             (parameterize ([app:path fake-app])
               (thunk)))))]))
  (define (with-fake-config path write-config thunk)
    (with-fake-app path
      (lambda ()
        (call-with-output-file (make-directory-path (app:config-filename))
          write-config 'replace)
        (app:config #f) ;; force reload
        (thunk))))
  (unless (directory? (tmp-dir))
    (make-directory (tmp-dir)))
  (parameterize ([base-dir (tmp-dir)])
    (match-let*
     ([#f (regular-file? (app:config-filename))]
      ;; reject non-hashtable
      [#(EXIT #(bad-arg app:config rubbish)) (catch (app:config 'rubbish))]
      [,unspecified (void)]
      [,obj (json:make-object [foo "bar"] [baz '(1 2 3)])]
      ;; hashtable okay
      [,@unspecified (app:config obj)]
      [,obj (app:config)]
      ;; read process-parameter unaffected by our change
      [#f (let ([me self])
            (spawn (lambda () (send me (app:config))))
            (receive [,cfg (eq? cfg (app:config))]))]
      [(1 2 3) (json:ref (app:config) 'baz #f)]
      ;; #f okay
      [,@unspecified (app:config #f)]
      [#f (json:ref (app:config) 'foo #f)]
      ;; we're unaffected by process-parameter change
      [#f (begin
            (spawn (lambda () (app:config obj)))
            (json:ref (app:config) 'foo #f))]
      ;; check config path when no app:path set
      [,expected (path-combine (base-dir) ".config")]
      [,@expected (parameterize ([app:path #f])
                    (app:config-filename))]
      ;; check config path when app:path ends with bin directory
      [,expected (fake-path "usr" "lib" "frozzle" "config")]
      [,@expected
       (with-fake-app `("usr" "bin" ,(fix-exe "frozzle"))
         app:config-filename)]
      ;; check config path when app:path does not end w/ bin directory
      [,expected (fake-path "flib" "blab" "frozzle.config")]
      [,@expected
       (with-fake-app `("flib" "blab" ,(fix-exe "frozzle"))
         app:config-filename)]
      ;; read config from file
      [,data (json:make-object
              [powder "keg"]
              [floss (json:make-object [sauce "moss"])]
              [glass '(3 4 5)])]
      [,expected (json:object->string data)]
      [,@expected
       (with-fake-config `("flim" "flam" ,(fix-exe "blott"))
         (lambda (op) (json:write op data 0))
         (lambda () (json:object->string (app:config))))]
      ;; malformed config file
      [,expected (fake-path "usr" "local" "lib" "zorp" "config")]
      [#(EXIT #(invalid-config-file ,expected unexpected-eof))
       (catch
        (with-fake-config `("usr" "local" "bin" ,(fix-exe "zorp"))
          (lambda (op) (display "{" op)) ;; incomplete
          app:config))]
      ;; config file contains non-dictionary at top level
      [,expected (fake-path "usr" "local" "lib" "zorp" "config")]
      [#(EXIT #(invalid-config-file ,expected expected-dictionary))
       (catch
        (with-fake-config `("usr" "local" "bin" ,(fix-exe "zorp"))
          (lambda (op) (json:write op '("apple" "pear" "banana") 0))
          app:config))]
      ;; config file deleted before we reload, so config is empty
      [,expected (fake-path "Applications" "zap.config")]
      ["got default"
       (with-fake-config `("Applications" ,(fix-exe "zap"))
         (lambda (op)
           (json:write op (json:make-object [flip "pers"])))
         (lambda ()
           ;; confirm config written
           (match-let*
            (["pers" (json:ref (app:config) 'flip #f)])
            'ok)
           ;; now delete and force reload
           (assert (delete-file (app:config-filename)))
           (app:config #f)
           (json:ref (app:config) 'flip "got default")))]
      ;; found directory instead of file at app:config-filename
      ;; as happens if we start a swish repl in ${HOME}
      ["nope"
       (with-fake-app `("home" "on" "the" ,(fix-exe "range"))
         (lambda ()
           (make-directory (app:config-filename))
           (app:config #f)
           (assert (directory? (app:config-filename)))
           (json:ref (app:config) 'grail "nope")))]
      )
     'ok)))

(mat base-dir ()
  (match-let*
   ([#(EXIT ,reason) (catch (directory? 'tofu))]
    [,expected-reason (exit-reason->english reason)]
    [#(EXIT ,actual-reason) (catch (base-dir 'tofu))]
    [,@expected-reason (exit-reason->english actual-reason)]
    [#(EXIT ,_) (catch (base-dir "no-such-directory-exists"))]
    [,dir (path-combine (output-dir) "tmp-test-base-dir")])
   (on-exit (delete-tree dir)
     (make-directory-path (path-combine dir "ignore-file"))
     (parameterize ([base-dir dir])
       (assert (starts-with? (data-dir) dir))
       (assert (equal? (data-dir) (path-parent (log-file))))
       (assert (starts-with? (tmp-dir) (data-dir)))))
   'ok))

(isolate-mat app-params ()
  (match-let*
   ([#(EXIT #(bad-arg app:name bad)) (catch (app:name 'bad))]
    [#(EXIT #(bad-arg app:path (wrong))) (catch (app:path '(wrong)))]
    [,_ (app:name "path/to/the-application.extension")]
    ["the-application" (app:name)]
    [#(EXIT ,reason) (catch (app:path "this-does-not-exist"))]
    [,_ (app:path "src/swish/app.ms")]
    [,path (app:path)]
    [#t (path-absolute? path)]
    [,@path (get-real-path "src/swish/app.ms")])
   'ok))

(mat repl-level ()
  (define script
    (write-test-file "script"
      (lambda ()
        (printf "~{~s\n~}\n"
          '((printf "repl level is ~s\n" (repl-level))
            (swish-start "-q" "--")
            (printf "back to ~s\n" (repl-level)))))))
  (match-let*
   ([("repl level is 1"
      "now at level 2"
      "reached 3"
      "back to 2"
      "and now 1")
     (script-test #f '()
       (format "~{~s\n~}"
         `((printf "repl level is ~s\n" (repl-level))
           (swish-start "-q" "--")
           (printf "now at level ~s\n" (repl-level))
           (swish-start "-q" "--")
           (printf "reached ~s\n" (repl-level))
           (exit)
           (printf "back to ~s\n" (repl-level))
           (exit)
           (printf "and now ~s\n" (repl-level))
           (exit)))
       '())]
    [("repl level is 0"
      "now at 1"
      "back to 0")
     (script-test script '()
       (format "~{~s\n~}"
         `((printf "now at ~s\n" (repl-level))
           (exit)))
       '())]
    [`(catch #(bad-arg repl-level "expert")) (try (repl-level "expert"))]
    [`(catch #(bad-arg repl-level -3)) (try (repl-level -3))]
    [`(catch #(bad-arg repl-level 2.0)) (try (repl-level 2.0))])
   'ok))

(mat coverage ()
  (define script
    (write-test-file "script"
      (lambda ()
        ;; hit else case in print-banner
        (pretty-print '(swish-start "--version"))
        ;; hit help case in run
        (pretty-print
         `(let ([os (open-output-string)])
            (parameterize ([current-output-port os])
              (swish-start "--help"))
            (match (split (get-output-string os) #\space)
              [("Usage:" ,(fix-exe "swish") . ,',_) 'ok])))
        ;; hit null files case in run
        (pretty-print '(swish-start)))))
  (match-let*
   ([,expected
     (format "~a Version ~a"
       (software-product-name)
       (software-version))]
    [,prompt (if (whereis 'profiler) ">> " "> ")]
    [(,@expected ,@expected ,@prompt)
     (script-test script '() "(exit)" '())])
   'ok))
