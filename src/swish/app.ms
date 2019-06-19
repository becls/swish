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
    (library-requirements '(swish imports))))

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
       1000
       '())])
   'ok))

(mat app-exception-handler ()
  (define (try who val)
    (call/cc
     (lambda (k)
       (let ([os (open-output-string)])
         (parameterize ([app:name who]
                        [console-error-port os]
                        [reset-handler
                         (lambda ()
                           (assert (eq? val (debug-condition)))
                           (k (get-output-string os)))])
           (app-exception-handler val))
         "failed to invoke reset-handler"))))
  (match-let*
   (["Exception occurred with non-condition value foo\n"
     ;; when app:name is not set, we get default exception handler
     (try #f 'foo)]
    ["bummer: foo.\n"
     ;; when app:name is set, we get app-exception-handler
     (try "bummer" 'foo)]
    ["oops: Invalid datum: \"flobble\".\n"
     (try "oops" '#(invalid-datum "flobble"))]
    ["dang: HTTP handler failed: Exception in cdr: 2 is not a pair.\n"
     ;; nested exit-reason->english with native exception
     (try "dang" `#(http-handler-failed ,(guard (c [else c]) (cdr 2))))]
    ["dang: HTTP handler failed: Exception in cdr: 2 is not a pair.\n"
     ;; nested exit-reason->english with non-condition
     (try "dang" `#(http-handler-failed ,(guard (c [else c]) (cdr 2))))]
    ["rats: HTTP handler failed: flak.\n"
     ;; nested exit-reason->english with caught non-condition
     (try "rats" `#(http-handler-failed ,(catch 'flak)))]
    ["wow: (this happened).\n"
     ;; current-exit-reason->english unchanged
     (try "wow" '(this happened))]
    ["dude: my bad.\n"
     ;; vague current-exit-reason->english
     (parameterize ([current-exit-reason->english (lambda (x) "my bad")])
       (try "dude" '(this happened)))]
    ["wow: Exception occurred with non-condition value (this happened).\n"
     ;; failure in current-exit-reason->english falls back to display-condition
     (parameterize ([current-exit-reason->english
                     (lambda args
                       (raise "internal error"))])
       (try "wow" '(this happened)))]
    ["hey: 123 is not a string.\n"
     ;; native Error condition
     (try "hey" (guard (c [else c]) (string-upcase 123)))]
    ["engine: overheating.\n"
     ;; native Warning condition
     (try "engine"
       (condition (guard (c [else c]) (warning #f "overheating"))))]
    ["my: message.\n"
     ;; native message condition
     (try "my" (make-message-condition "message"))]
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
          (app:start)
          (app:shutdown 2)
          ;; simulate repl interaction after surprising
          ;; non-exit of app:shutdown
          (receive (after 1000 (osi_exit 3))
            [locking-up 'ok])
          (printf "armed and ready\n")
          (send (whereis 'bad-sup) 'disarm)
          (receive (after 1000 (osi_exit 4)))))
      '("expected non-zero exit")))
   [#(EXIT `(<os-process-failed> [exit-status 2] ,stdout))
    (match-regexps '(seek "armed and ready") stdout)]))

(isolate-mat application-shutdown2 ()
  ;; Take care when closing input port if we call app:shutdown more than once.
  (match
   (catch
    (script-test #f '()
      (format "~{~s\n~}\n"
        '((app:start)
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
    (raise 'matched-wrong-case)]))

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
            "Event: #\\(<child-end>.*normal\\)"
            seek
            "Date: "
            "Timestamp: "
            "Event: #\\(<child-end>.*shutdown\\)"
            seek
            "Date: "
            "Timestamp: "
            "Event: #\\(<child-end>.*shutdown\\)")
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
       (assert (starts-with? (tmp-dir) (data-dir)))
       (assert (starts-with? (web-dir) dir))))
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
