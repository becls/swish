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

#!chezscheme
(library (swish profile)
  (export
   profile:dump-html
   profile:exclude
   profile:filename
   profile:merge
   profile:prepare
   profile:save
   profile:start
   profile:start&link
   profile:stop
   )
  (import
   (chezscheme)
   (swish app-io)
   (swish erlang)
   (swish gen-server)
   (swish html)
   (swish io)
   (swish meta)
   (swish pregexp)
   )

  (define-state-tuple <profile-state> ht filename context waketime)

  (define (profile:prepare)
    (library-extensions
     (map (lambda (x) (match x [(,ss . ,so) (cons ss (string-append so "p"))]))
       (library-extensions)))
    (compile-imported-libraries #t)
    (compile-profile #t)
    (compile-interpret-simple #f)
    (cp0-effort-limit 0)
    (run-cp0 (lambda (f x) x)))

  (define (source-object-bfp=? x y)
    (= (source-object-bfp x) (source-object-bfp y)))

  (define (sfd=? x y)
    (or (eq? x y)
        (and (source-file-descriptor? x)
             (source-file-descriptor? y)
             (string=? (source-file-descriptor-path x)
               (source-file-descriptor-path y))
             (= (source-file-descriptor-checksum x)
                (source-file-descriptor-checksum y)))))

  (define (find-src x)
    (if (source-object? x)
        x
        (and (vector? x) (fx> (vector-length x) 1)
             (find-src (vector-ref x 1)))))

  (define (add-filedata sfd-table items)
    ;; item = (record . count)
    (define (add-item item)
      (let ([source (find-src (car item))])
        (when source
          (let* ([sfd (source-object-sfd source)]
                 [source-table
                  (or (hashtable-ref sfd-table sfd #f)
                      (let ([ht (make-hashtable source-object-bfp
                                  source-object-bfp=?)])
                        (hashtable-set! sfd-table sfd ht)
                        ht))])
            (hashtable-update! source-table source
              (lambda (count) (+ count (cdr item)))
              0)))))
    (for-each add-item items))

  ;; Test code must be able to call profile:exclude whether or not we have
  ;; started the profiling gen-server, so we manage this state outside the
  ;; gen-server.  We record this state in the file when creating a profile.
  ;; We restore this state when loading or merging a profile.
  ;; Since we create and restore profiles in separate OS processes, it's okay
  ;; for profile:dump-html to modify this state.
  (define-syntax (profile:exclude x)
    (syntax-case x ()
      [(pem) #`(module () (add-profile-exclusion #,(find-source x)))]
      [(pem path) #`(module () (add-profile-exclusion path))]
      [(pem path range) #`(module () (add-profile-exclusion `#(at ,range ,path)))]))

  (define-tuple <profile-config> excluded-paths excluded-ranges source-directories)
  (define-tuple <profile-data> context entries)

  (define profile-exclusions (make-hashtable string-hash string=?))
  (define source-dirs (make-hashtable string-hash string=?))
  (define (current-source-dirs) (vector->list (hashtable-keys source-dirs)))
  (define (add-source-dir! dir) (hashtable-set! source-dirs dir #t))
  (define (current-profile-config)
    (for-each add-source-dir! (source-directories))
    (for-each add-source-dir! (map car (library-directories)))
    (let-values ([(keys vals) (hashtable-entries profile-exclusions)])
      (<profile-config> make
        [excluded-paths keys]
        [excluded-ranges vals]
        [source-directories (hashtable-keys source-dirs)])))
  (define profile-included-regexp #f)
  (define profile-excluded-regexp #f)
  (define (profile-excluded? path fp)
    (cond
     [(and profile-included-regexp (pregexp-match profile-included-regexp path)) #f]
     [(hashtable-ref profile-exclusions path #f) => (lambda (excluded) (>= fp excluded))]
     [profile-excluded-regexp (pregexp-match profile-excluded-regexp path)]
     [else #f]))
  (define (profile-exclude! path range) (hashtable-set! profile-exclusions path range))
  (define (add-profile-exclusion src)
    (match src
      [#(at ,bfp ,path) (profile-exclude! path bfp)]
      [,path (guard (string? path)) (profile-exclude! path 0)]
      [#f (void)]))

  (define (globs->regexp patterns)
    (define (convert-pattern pattern os)
      (let ([ip (open-input-string pattern)])
        (do ([c (read-char ip) (read-char ip)])
            ((eof-object? c))
          (case c
            [(#\*)
             (case (peek-char ip)
               [#\* (read-char ip) (display ".*" os)]
               [else (display "(|[^/\\\\]*)" os)])]
            [(#\?) (display "[^/\\\\]" os)]
            [(#\\)
             (let ([escaped (read-char ip)])
               (when (eof-object? escaped)
                 (error 'globs->regexp "expected character after \\ escape"))
               (write-char #\\ os)
               (write-char escaped os))]
            [(#\. #\| #\\ #\( #\))
             (write-char #\\ os)
             (write-char c os)]
            [#\[
             (let ([next (read-char ip)])
               (when (eof-object? next)
                 (error 'globs->regexp "empty [ expression"))
               (write-char c os)
               (case next
                 [#\! (write-char #\^ os)]
                 [else (write-char next os)]))]
            [else (write-char c os)]))))
    (match patterns
      [() #f]
      [(,pattern . ,more)
       (let ([os (open-output-string)])
         (write-char #\^ os)
         (when (pair? more) (write-char #\( os))
         (do ([patterns patterns (cdr patterns)]
              [sep "" "|"])
             ((null? patterns))
           (display sep os)
           (convert-pattern (car patterns) os))
         (when (pair? more) (write-char #\) os))
         (write-char #\$ os)
         (get-output-string os))]))

  (define (profile:filename)
    (gen-server:call 'profiler 'get-filename 'infinity))

  (define (profile-load ht filename)
    ($profile-load filename (lambda (items) (add-filedata ht items)))
    ht)

  (define ($profile-load filename add!)
    (let ([ip (open-binary-file-to-read filename)])
      (on-exit (close-port ip)
        (let lp ()
          (let ([x (fasl-read ip)])
            (unless (eof-object? x)
              (match x
                [`(<profile-data> ,entries) (add! entries)]
                [`(<profile-config> ,excluded-paths ,excluded-ranges ,source-directories)
                 (vector-for-each profile-exclude! excluded-paths excluded-ranges)
                 (vector-for-each add-source-dir! source-directories)])
              (lp)))))))

  (define (source-table->list source-table)
    (let-values ([(keys vals) (hashtable-entries source-table)])
      (let lp ([i 0] [ls '()])
        (if (= i (vector-length keys))
            ls
            (lp (+ i 1)
              (cons
               (cons (vector-ref keys i) (vector-ref vals i))
               ls))))))

  (define (profile-save filename context sfd-table)
    (let ([op (open-binary-file-to-replace (make-directory-path filename))])
      (on-exit (close-port op)
        (fasl-write (current-profile-config) op)
        (let-values ([(keys vals) (hashtable-entries sfd-table)])
          (vector-for-each
           (lambda (sfd source-table)
             (fasl-write (<profile-data> make [context context] [entries (source-table->list source-table)]) op))
           keys vals))
        'ok)))

  (define (profile-update state)
    (<profile-state> open state [context filename ht])
    (collect (collect-maximum-generation))
    (add-filedata ht
      (with-interrupts-disabled
       (let ([data (profile-dump)])
         (profile-clear)
         data)))
    (profile-save filename context ht))

  (define (resolve path)
    (if (path-absolute? path)
        path
        (match (path-parent path)
          ["" (path-combine (cd) path)]
          [,dir (path-combine (get-real-path dir) (path-last path))])))

  (define (reply msg state)
    `#(reply ,msg ,state ,($state waketime)))

  (define (no-reply state)
    `#(no-reply ,state ,($state waketime)))

  (define (next-waketime) (+ (erlang:now) 60000))

  (define (unwrap x)
    (match x
      [#(ok ,result) result]
      [#(error ,reason) (raise reason)]))

  (define (init input-fn output-fn force-start?)
    (cond
     [(and (not force-start?) (null? (profile-dump)))
      'ignore]
     [else
      ;; Trap exits so that terminate is called when the system is
      ;; going down.
      (process-trap-exit #t)
      ;; get an absolute path since (cd) may change between load and save
      (let* ([filename (resolve output-fn)]
             [ht (make-hashtable source-file-descriptor-checksum sfd=?)]
             [state (<profile-state> make
                      [ht ht]
                      [filename filename]
                      [context #f]
                      [waketime (next-waketime)])])
        (when input-fn (profile-load ht input-fn))
        `#(ok ,state ,($state waketime)))]))
  (define (terminate reason state)
    (profile-update state))
  (define (handle-call msg from state)
    (match msg
      [save
       (profile-update state)
       (reply 'ok ($state copy [waketime (next-waketime)]))]
      [get-filename
       (reply ($state filename) state)]
      [(merge . ,paths)
       (let ([ht ($state ht)])
         (match (catch (fold-left profile-load ht paths))
           [#(EXIT ,reason) (reply `#(error ,reason) state)]
           [,ht (reply '#(ok ok) ($state copy [ht ht] [waketime 0]))]))]
      [stop `#(stop normal stopped ,state)]))
  (define (handle-cast msg state) (match msg))
  (define (handle-info msg state)
    (match msg
      [timeout
       (match (catch (profile-update state))
         [ok
          (no-reply ($state copy [waketime (next-waketime)]))]
         [#(EXIT #(io-error ,_ ,_ ,_))
          (no-reply ($state copy [waketime (+ (erlang:now) 1000)]))]
         [#(EXIT ,reason) (raise reason)])]))
  (define (default-filename) (path-combine (data-dir) "server.profile"))
  (define (profile:start&link input-fn output-fn force-start?)
    (gen-server:start&link 'profiler input-fn output-fn force-start?))
  (define (profile:start input-fn output-fn force-start?)
    (gen-server:start 'profiler input-fn output-fn force-start?))
  (define (profile:stop)
    (gen-server:call 'profiler 'stop))

  (define (profile:save)
    (gen-server:call 'profiler 'save 'infinity))

  (define (profile:merge . paths)
    (unwrap (gen-server:call 'profiler `(merge ,@paths) 'infinity)))

  (define sfd-sig
    (let ([ht (make-eq-hashtable)])
      (lambda (sfd)
        (or (eq-hashtable-ref ht sfd #f)
            (let ([x (cons (source-file-descriptor-checksum sfd)
                       (sfd-source-path sfd get-real-path))])
              (eq-hashtable-set! ht sfd x)
              x)))))

  (define (sfd-sig-hash sfd) (equal-hash (sfd-sig sfd)))
  (define (sfd-sig=? x y)
    (or (eq? x y)
        (equal? (sfd-sig x) (sfd-sig y))))
  (define (sfd-source-path sfd convert)
    (let ([ip (open-source-file sfd)])
      (if ip
          (on-exit (close-port ip)
            (convert (port-name ip)))
          (source-file-descriptor-path sfd))))

  (define (profile:dump-html profile-in output-fn include-globs exclude-globs)
    (define inputs (if (list? profile-in) profile-in (list profile-in)))
    (define (list-of-strings? x) (and (list? x) (andmap string? x)))
    (define (load-profiles)
      (define table (make-hashtable sfd-sig-hash sfd-sig=?))
      (fluid-let ([profile-included-regexp (globs->regexp include-globs)]
                  [profile-excluded-regexp (globs->regexp exclude-globs)])
        (for-each
         (lambda (profile-fn)
           (match (catch ($profile-load profile-fn (make-insert-filedata table)))
             [#(EXIT ,reason)
              (let () (import (swish errors)) (printf "REASON: ~a\n" (exit-reason->english reason)))
              (errorf 'profile:dump-html "cannot load profile data from ~a" profile-fn)]
             [,_ (void)]))
         inputs))
      (let-values ([(keys vals) (hashtable-entries table)])
        (vector->list
         (vector-sort (lambda (a b) (string<? (car a) (car b)))
           (vector-map (lambda (k v) (cons (sfd-source-path k values) v))
             keys vals)))))
    (unless (list-of-strings? inputs) (bad-arg 'profile:dump-html profile-in))
    (unless (string? output-fn) (bad-arg 'profile:dump-html output-fn))
    (unless (list-of-strings? include-globs) (bad-arg 'profile:dump-html include-globs))
    (unless (list-of-strings? exclude-globs) (bad-arg 'profile:dump-html exclude-globs))
    (let ([results (load-profiles)]
          [op (open-file-to-replace (make-directory-path output-fn))])
      (fprintf op "<!DOCTYPE html>\n")
      (fprintf op "<html>\n")
      (html->string op
        `(head
          (meta (@ (charset "UTF-8")))
          (title "Test Coverage")
          (style
            "td { text-align: right; }"
            "td:first-child { text-align: left; }")))
      (fprintf op "<body style='font-family:monospace;'>\n")
      (let-values ([(hits sites percentage) (summarize-coverage results)])
        (fprintf op
          "<h2>Overall ~a% coverage with ~a of ~a sites covered.\n</h2>"
          percentage hits sites))
      (fprintf op "<table style=\"font-size: 1em;\">\n")
      (output-row op "filename" "hits" "sites" "coverage" "max-count")
      (parameterize ([source-directories (current-source-dirs)])
        (let ([root (path-parent (get-real-path output-fn))])
          (for-each
           (lambda (entry)
             ;; entry = (sfd . skiplist | #f)
             (match entry
               [(,name . ,sl)
                (match (and sl (sl))
                  [,data
                   (guard (pair? data))
                   ;; non-blocking i/o OK: profile:dump-html runs from dedicated OS process, not interested in concurrency
                   (let* ([ip (open-input-file name)]
                          [file-op (open-file-to-replace (make-directory-path (path-combine root (string-append name ".html"))))])
                     (on-exit (close-port file-op)
                       (annotate (pregexp-replace* "\\\\" name "/") ip data op file-op)))]
                  [,_ (void)])]
               [#f (void)]))
           results)))
      (fprintf op "</table>\n")
      (fprintf op "</body></html>\n")
      (close-output-port op)
      #t))

  (define (annotate name ip data summary-op file-op)
    ;; data = ((source . count) ...) in order of ascending bfp
    (let  ([max-count (do ([ls data (cdr ls)] [n 1 (max (cdar ls) n)])
                          ((null? ls) n))]
           [hits (fold-left (lambda (hit p) (if (= (cdr p) 0) hit (+ hit 1)))
                   0 data)]
           [sites (length data)])
      (when (> sites 0)
        (let ([h-name (html-encode (stringify name))])
          (output-row summary-op
            (format "<a href=\"~a.html\">~a</a>" h-name h-name)
            hits
            sites
            (round (* (/ hits sites) 100))
            max-count)))
      (emit-header (sanitize name) file-op)
      (let loop ([fp 0] [ls data] [stack '()])
        (cond
         [(and (pair? stack) (= (car stack) fp))
          (display "</span>" file-op)
          (loop fp ls (cdr stack))]
         [(and (pair? ls) (= (source-object-bfp (caar ls)) fp))
          (let ([count (cdar ls)])
            (fprintf file-op "<span title=\"~:d\" style=\"background-color: ~a\">"
              count (colorize count max-count)))
          (loop fp (cdr ls) (cons (source-object-efp (caar ls)) stack))]
         [else
          (let ([c (read-char ip)])
            (unless (eof-object? c)
              (print-char c file-op)
              (loop (+ fp 1) ls stack)))]))
      (emit-trailer file-op)
      (close-input-port ip)))

  (define (colorize x max-count)
    (cond
     [(= x 0) "#C0C0C0"]         ;; gray
     [(= max-count 1) "#FFFF80"] ;; yellow
     [else
      ;; from red to yellow on a log scale
      (let ([s (number->string
                (+ (inexact->exact
                    (round (* (/ (log x) (log max-count)) #x7F)))
                   #x80)
                16)])
        (string-append "#FF" s "80"))]))

  (define (sanitize s)
    (do ([i 0 (fx+ i 1)] [n (string-length s)] [op (open-output-string)])
        ((fx= i n) (get-output-string op))
      (print-char (string-ref s i) op)))

  (define (print-char c op)
    (case c
      [(#\") (display "&quot;" op)]
      [(#\<) (display "&lt;" op)]
      [(#\>) (display "&gt;" op)]
      [(#\&) (display "&amp;" op)]
      [else (write-char c op)]))

  (define (stringify x)
    (if (string? x)
        x
        (format "~s" x)))

  (define (html-encode s)
    (let-values ([(op get) (open-string-output-port)])
      (let ([len (string-length s)])
        (do ((i 0 (+ i 1))) ((= i len))
          (let ([c (string-ref s i)])
            (print-char c op)))
        (get))))

  (define (emit-header name op)
    (fprintf op
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
<title>~a</title>
<style type=\"text/css\">
<!--
SPAN
{
 border-width: 1px;
               border-top-width: 0;
               border-bottom-width: 0;
               border-style: solid;
               }
-->
</style>
</head>
<body>
<h1>~a</h1>

<pre>
" name name))

  (define (emit-trailer op)
    (display "
</pre>
</body>
</html>" op))

  (define (output-row op c1 c2 c3 c4 c5)
    (fprintf op
      "<tr><td>~a</td><td>~a</td><td>~a</td><td>~a</td><td>~a</td></tr>\n"
      c1
      (html-encode (stringify c2))
      (html-encode (stringify c3))
      (cond
       [(fixnum? c4)
        (format "<span style='color:~a'>~d%</span>"
          (cond
           [(<= c4 50) "#FF0000"]
           [(< c4 80) "#FF8800"]
           [else "#007F00"])
          c4)]
       [else
        (html-encode (stringify c4))])
      (html-encode (stringify c5))))

  (define (summarize-coverage dump)
    (let lp ([hits 0] [sites 0] [rest dump])
      (if (eq? '() rest)
          (values hits sites (if (= sites 0) 100 (round (* 100 (/ hits sites)))))
          (let* ([sl (cdr (car rest))]
                 [data (if sl (sl) '())]
                 [new-hits
                  (fold-left (lambda (hit p) (if (= (cdr p) 0) hit (+ hit 1)))
                    0 data)]
                 [new-sites (length data)])
            (lp (+ hits new-hits) (+ sites new-sites) (cdr rest))))))

  (define (make-insert-filedata table)
    (define okay (make-eq-hashtable))
    (define (invalid-sfd? sfd)
      (cond
       [(eq-hashtable-ref okay sfd #f) #f]
       [(path-absolute? (source-file-descriptor-path sfd))
        "absolute path"]
       [else
        (let ([ip (open-source-file sfd)])
          (cond
           [ip (close-port ip) (eq-hashtable-set! okay sfd #t) #f]
           [else "file not found (or file modified)"]))]))
    (define (insert-filedata item)
      ;; item = (crecord . count)
      (cond
       [(find-src (car item)) =>
        (lambda (source)
          (let ([sfd (source-object-sfd source)]
                [count (cdr item)])
            (define (add-it sl)
              (sl source
                (lambda (x y insert update)
                  (if (source-bfp=? x y)
                      (update + count)
                      (insert count))))
              sl)
            (unless (profile-excluded? (source-file-descriptor-path sfd) (source-object-bfp source))
              (hashtable-update! table sfd
                (lambda (sl)
                  (cond
                   [sl (add-it sl)]
                   [(invalid-sfd? sfd) =>
                    (lambda (reason)
                      (unless (hashtable-contains? okay sfd)
                        (hashtable-set! okay sfd #f)
                        (printf "Skipping ~a: ~a\n"
                          (source-file-descriptor-path sfd)
                          reason))
                      #f)]
                   [else
                    (add-it (make-skiplist source-bfp<?))]))
                #f))))]))
    (lambda (items)
      (parameterize ([source-directories (current-source-dirs)])
        (for-each insert-filedata items))))

  (define (source-bfp<? x y)
    (< (source-object-bfp x) (source-object-bfp y)))

  (define (source-bfp=? x y)
    (and (source-object? x) (source-object? y) (= (source-object-bfp x) (source-object-bfp y))))

  (define make-skiplist
    (case-lambda
     [(key<) (make-skiplist 1000000 key<)]
     [(size key<)
      (let* ([p-inv 5]
             [max-forward
              (max (inexact->exact (round (/ (log size) (log p-inv)))) 1)]
             [first-forward 2]
             [last-forward (+ first-forward max-forward -1)]
             [maxsize (+ last-forward 1)]
             [first (make-vector maxsize #f)]
             [update (make-vector maxsize #f)])

                                        ; key
        (define (item-key x) (vector-ref x 0))
        (define (set-key! x y) (vector-set! x 0 y))

                                        ; value
        (define (item-value x) (vector-ref x 1))
        (define (set-value! x y) (vector-set! x 1 y))

                                        ; forward
        (define (item-forward x i) (vector-ref x i))
        (define (set-forward! x i y) (vector-set! x i y))

        (define (item-top x) (fx- (vector-length x) 1))

        (define random-size
          (let* ([p-denom (expt 2 28)]
                 [p-num (fx/ p-denom p-inv)])
            (lambda ()
              (let loop ([size (fx+ first-forward 1)])
                (cond
                 [(fx= size maxsize) maxsize]
                 [(fx< (random p-denom) p-num) (loop (fx+ size 1))]
                 [else size])))))

        (define (make-item key value)
          (let ([item (make-vector (random-size) #f)])
            (set-key! item key)
            (set-value! item value)
            item))

        (case-lambda
         [(key f)
          (let find ([x first] [i last-forward])
            (let ([z (item-forward x i)])
              (cond
               [(and z (not (key< key (item-key z))))
                (find z i)]
               [(fx> i first-forward)
                (vector-set! update i x)
                (find x (fx- i 1))]
               [else
                (vector-set! update i x)
                (f (item-key x) key
                  (lambda (value)
                    (let ([y (make-item key value)])
                      (let loop ([i (item-top y)])
                        (when (fx>= i first-forward)
                          (let ([x (vector-ref update i)])
                            (set-forward! y i (item-forward x i))
                            (set-forward! x i y))
                          (loop (fx- i 1))))))
                  (lambda (f value)
                    (set-value! x (f (item-value x) value))))])))]
         [()
          (let loop ([x (item-forward first first-forward)])
            (if x
                (cons (cons (item-key x) (item-value x))
                  (loop (item-forward x first-forward)))
                '()))]))]))

  (profile:exclude "swish/mat.ss")
  (profile:exclude "src/swish/script-testing.ss")

  )

#!eof mats

(load-this-exposing '(globs->regexp))

(import
 (swish mat)
 (swish profile)
 (swish testing))

(mat glob ()
  (match-let*
   ([,regexp (globs->regexp '("foo*.ss" "*.ms" "bar**/*.[ch]" "que\\?" "mi?s"))]
    [,check (lambda (str) (if (pregexp-match regexp str) 'hit 'miss))]
    [hit (check "foolish.ss")]
    [hit (check "foo.ss")]
    [hit (check "fob.ms")]
    [hit (check "bar.ms")]
    [hit (check "bar/bla.c")]
    [hit (check "bar/bla.h")]
    [hit (check "bar/.c")]
    [hit (check "bar/.h")]
    [hit (check "barbeque/snail.c")]
    [hit (check "bar/b/e/q/u/e/snail.c")]
    [hit (check "barrel/of/monkeys.h")]
    [hit (check "bar\\none/foo.c")]
    [hit (check "que?")]
    [hit (check "miss")]
    [hit (check "mits")]
    [miss (check "mites")]
    [miss (check "quey")]
    [miss (check "fo.ss")]
    [miss (check "foo/lish.ss")]
    [miss (check "foo\\lish.ss")]
    [miss (check "foolish.sh")]
    [miss (check "fob.ss")]
    [miss (check "key/fob.ms")]
    [miss (check "bar.msi")]
    [miss (check "bar/bla.o")]
    [miss (check "ba/rbla.h")]
    [#f (globs->regexp '())]
    [#(EXIT ,reason) (catch (globs->regexp '("foo\\")))]
    ["Exception in globs->regexp: expected character after \\ escape." (exit-reason->english reason)]
    [#(EXIT ,reason) (catch (globs->regexp '("[")))]
    ["Exception in globs->regexp: empty [ expression." (exit-reason->english reason)])
   'ok)
  (match-let*
   ([,regexp (globs->regexp '("[a-c]z"))]
    [,check (lambda (str) (if (pregexp-match regexp str) 'hit 'miss))]
    [hit (check "az")]
    [hit (check "bz")]
    [hit (check "cz")]
    [miss (check "dz")]
    [miss (check "ez")]
    [miss (check "fz")]
    [miss (check "gz")])
   'ok)
  (match-let*
   ([,regexp (globs->regexp '("[!d-f]z"))]
    [,check (lambda (str) (if (pregexp-match regexp str) 'hit 'miss))]
    [hit (check "az")]
    [hit (check "bz")]
    [hit (check "cz")]
    [miss (check "dz")]
    [miss (check "ez")]
    [miss (check "fz")]
    [hit (check "gz")])
   'ok)
  )
