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

#!chezscheme
(library (swish http)
  (export
   <request>
   http:add-file-server
   http:add-server
   http:call-url-handler
   http:call-with-form
   http:call-with-ports
   http:compose-url-handlers
   http:configure-file-server
   http:configure-server
   http:enable-dynamic-pages
   http:eval-dynamic-page
   http:find-header
   http:find-param
   http:get-content-length
   http:get-header
   http:get-param
   http:get-port-number
   http:make-default-file-url-handler
   http:make-default-media-type-handler
   http:options
   http:percent-encode
   http:read-header
   http:read-status
   http:respond
   http:respond-file
   http:switch-protocol
   http:url-handler
   http:write-header
   http:write-status
   )
  (import
   (chezscheme)
   (swish app)
   (swish app-io)
   (swish erlang)
   (swish event-mgr)
   (swish events)
   (swish gen-server)
   (swish ht)
   (swish html)
   (swish io)
   (swish json)
   (swish meta)
   (swish options)
   (swish osi)
   (swish pregexp)
   (swish string-utils)
   (swish supervisor)
   (swish watcher)
   )

  (define-syntax define-process-parameter-options
    (syntax-rules ()
      [(_ options-name install-params [param kv ...] ...)
       (begin
         (define param (make-process-parameter #f)) ...
         (define-options options-name
           (optional [param kv ...] ...))
         (define (install-params options)
           (param (options-name param options)) ...))]))

  (define-process-parameter-options http:options install-options
    [file-search-extensions
     (default '(".html"))
     (must-be list? (lambda (ls) (andmap string? ls)))]
    [file-transform
     (default (lambda (path) #f))
     (must-be procedure?)]
    [header-limit
     (default 1048576)
     (must-be fixnum? fxpositive?)]
    [media-type-handler
     (default (lambda (ext) #f))
     (must-be procedure?)]
    [request-limit
     (default 8192)
     (must-be fixnum? fxpositive?)]
    [request-timeout
     (default 30000)
     (must-be fixnum? fxpositive?)]
    [response-timeout
     (default 60000)
     (must-be fixnum? fxpositive?)]
    )

  (define-syntax http:add-file-server
    (syntax-rules ()
      [(_ arg ...)
       (app-sup-spec
        (append (app-sup-spec) (http:configure-file-server arg ...)))]))

  (define http:configure-file-server
    (case-lambda
     [(name port dir)
      (http:configure-file-server name port dir (http:options))]
     [(name port dir options)
      (let ([mth (http:make-default-media-type-handler dir)])
        (http:configure-server name port
          (http:make-default-file-url-handler dir)
          (http:options copy options
            [media-type-handler mth])))]))

  (define-syntax http:add-server
    (syntax-rules ()
      [(_ arg ...)
       (app-sup-spec
        (append (app-sup-spec) (http:configure-server arg ...)))]))

  (define http:configure-server
    (case-lambda
     [(name port url-handler)
      (http:configure-server name port url-handler (http:options))]
     [(name port url-handler options)
      (arg-check 'http:configure-server
        [name (lambda (x) (or (eq? x #f) (symbol? x)))]
        [port (lambda (x) (and (fixnum? x) (fx<= 0 x 65535)))]
        [url-handler procedure?]
        [options (http:options is?)])
      `(#(,(gensym "http-sup")
          ,(lambda ()
             (supervisor:start&link #f 'one-for-one 10 10000
               `(#(http-listener
                   ,(lambda ()
                      (http-listener:start&link name self port url-handler
                        options))
                   permanent 1000 worker))))
          permanent infinity supervisor))]))

  (define-syntax (http:url-handler x)
    (syntax-case x ()
      [(k body1 body2 ...)
       (with-implicit (k conn request header params)
         #'(lambda (conn request header params)
             body1 body2 ...))]))

  (define (http:compose-url-handlers handlers)
    (http:url-handler
     (let lp ([handlers handlers])
       (match handlers
         [() #f]
         [(,h . ,handlers)
          (or (http:call-url-handler h)
              (if (conn:output-ready? conn)
                  (lp handlers)
                  (throw 'http-side-effecting-handler)))]))))

  (define-syntax (http:call-url-handler x)
    (syntax-case x ()
      [(k handler)
       (with-implicit (k conn request header params)
         #'(handler conn request header params))]))

  (define (http-listener:start&link name sup init-port url-handler options)
    (define-state-tuple <http-listener> tcp-listener cache-manager pid->op)
    (define (init)
      (process-trap-exit #t)
      (match-let* ([#(ok ,cache-mgr) (cache-mgr:start&link sup options)])
        `#(ok ,(<http-listener> make
                 [tcp-listener (listen-tcp "::" init-port self)]
                 [cache-manager cache-mgr]
                 [pid->op (ht:make process-id eq? process?)]))))
    (define (terminate reason state)
      ;; Ignore the pid->op table. In the rare event that this
      ;; gen-server fails, the dispatchers can continue
      ;; processing. The garbage collector will clean up if necessary.
      (close-tcp-listener ($state tcp-listener)))
    (define (handle-call msg from state)
      (match msg
        [get-port-number
         `#(reply ,(listener-port-number ($state tcp-listener)) ,state)]))
    (define (handle-cast msg state) (match msg))
    (define (handle-info msg state)
      (match msg
        [#(accept-tcp ,_ ,ip ,op)
         (match-let*
          ([,cache-mgr ($state cache-manager)]
           [#(ok ,pid)
            (watcher:start-child sup (gensym "http-dispatcher") 1000
              (lambda ()
                `#(ok ,(spawn&link
                        (lambda ()
                          (cache-manager cache-mgr)
                          (install-options options)
                          (match-let*
                           ([#(ok ,conn)
                             (conn:start&link cache-mgr ip op options)])
                           (http:handle-input conn url-handler options)))))))])
          (monitor pid)
          `#(no-reply ,($state copy* [pid->op (ht:set pid->op pid op)])))]
        [#(accept-tcp-failed ,_ ,_ ,_)
         `#(stop ,msg ,state)]
        [`(DOWN ,_ ,pid ,_)
         (cond
          [(ht:ref ($state pid->op) pid #f) =>
           (lambda (op)
             (force-close-output-port op)
             `#(no-reply ,($state copy* [pid->op (ht:delete pid->op pid)])))]
          [else
           `#(no-reply ,state)])]))
    (gen-server:start&link name))

  (define (http:get-port-number listener)
    (gen-server:call listener 'get-port-number))

  (define (conn:start&link cache-mgr ip op options)
    (define-state-tuple <conn>
      input                     ; ready | #(advance ipos) | close
      output                    ; ready | dirty
      )
    (define host (port-name ip))

    (define (internal-server-error state)
      (match ($state output)
        [ready
         (respond 500 '()
           (html->bytevector
            `(html5
              (head
               (meta (@ (charset "UTF-8")))
               (title "Internal server error"))
              (body
               (h1 "The server was unable to complete the request.")))))
         ($state copy [input 'close] [output 'dirty])]
        [dirty
         ($state copy [input 'close])]))

    (define (advance-input state)
      (match ($state input)
        [#(advance ,ipos)
         (when ipos
           (let ([remaining (- ipos (port-position ip))])
             (when (< remaining 0)
               (internal-server-error state)
               (throw 'http-input-violation))
             (do ([n remaining (fx- n 1)] [b #f (get-u8 ip)])
                 ((fx= n 0)
                  (when (eof-object? b)
                    (throw 'unexpected-eof))))))
         ($state copy [input 'ready])]
        [,_ state]))

    (define (respond status header content)
      (http:write-status op status)
      (http:write-header op
        (let ([header (add-cache-control "no-cache" header)])
          (if content
              (add-content-length (bytevector-length content) header)
              header)))
      (when content
        (put-bytevector op content))
      (flush-output-port op)
      #t)

    (define (respond-file status header filename)
      (let ([port (open-file-port filename O_RDONLY 0)])
        (on-exit (close-osi-port port)
          (let* ([n (get-file-size port)]
                 [bufsize (min n (ash 1 18))]
                 [buffer (make-bytevector bufsize)])
            (http:write-status op status)
            (http:write-header op
              (add-content-length n
                (add-cache-control "max-age=3600" ; 1 hour
                  (add-content-type filename header))))
            (let lp ([fp 0])
              (when (< fp n)
                (let ([count (read-osi-port port buffer 0 bufsize fp)])
                  (when (eqv? count 0)
                    (throw 'unexpected-eof))
                  (put-bytevector op buffer 0 count)
                  (lp (+ fp count))))))
          (flush-output-port op)
          #t)))

    (define (init)
      ;; Important for the conn not to trap exits, since it may get
      ;; killed when a timeout occurs.
      (cache-manager cache-mgr)
      (install-options options)
      `#(ok ,(<conn> make
               [input 'ready]
               [output 'dirty])))
    (define (terminate reason state) 'ok)
    (define (handle-call msg from state)
      (match msg
        [detach-ports
         (advance-input state)
         `#(stop normal #(ok ,ip ,op) detached)]
        [get-request
         (let ([state (advance-input state)])
           (match (<conn> output state)
             [ready
              `#(reply #(error http-output-violation)
                  ,($state copy [input 'close]))]
             [dirty
              (match (<conn> input state)
                [close `#(reply #(ok #f) ,state)]
                [ready
                 (let ([x (read-line ip (request-limit))])
                   (cond
                    [(eof-object? x)
                     `#(reply #(ok #f) ,($state copy [input 'close]))]
                    [(http:parse-request x) =>
                     (lambda (request)
                       (define header (http:read-header ip (header-limit)))
                       (define ipos
                         (cond
                          [(http:get-content-length header) =>
                           (lambda (len) (+ (port-position ip) len))]
                          [else #f]))
                       `#(reply #(ok ,(<request> copy request
                                        [host host]
                                        [header header]))
                           ,($state copy
                              [input (if (keep-alive? header)
                                         `#(advance ,ipos)
                                         'close)]
                              [output 'ready])))]
                    [else (throw `#(http-unhandled-input ,x))]))])]))]
        [output-ready?
         `#(reply
            #(ok ,(match ($state output)
                    [ready #t]
                    [dirty #f]))
            ,state)]
        [#(respond ,status ,header ,content)
         (match ($state output)
           [ready
            `#(reply #(ok ,(respond status header content))
                ,($state copy [output 'dirty]))]
           [dirty
            `#(reply #(error http-output-violation) ,state)])]
        [#(respond-file ,status ,header ,filename)
         (match ($state output)
           [ready
            `#(reply #(ok ,(respond-file status header filename))
                ,($state copy [output 'dirty]))]
           [dirty
            `#(reply #(error http-output-violation) ,state)])]
        [#(call-with-ports ,f)
         (let* ([opos (port-position op)]
                [x (try (f ip op))]
                [output? (not (= opos (port-position op)))]
                [state (if output? ($state copy [output 'dirty]) state)])
           (flush-output-port op)
           (match x
             [`(catch ,_ ,e) `#(reply #(error ,e) ,state)]
             [,result `#(reply #(ok ,result) ,state)]))]
        [internal-server-error
         `#(no-reply ,(internal-server-error state))]))
    (define (handle-cast msg state) (match msg))
    (define (handle-info msg state) (match msg))
    (gen-server:start&link #f))

  (define (conn:detach-ports who)
    (match (gen-server:call who 'detach-ports)
      [#(ok ,ip ,op) (values ip op)]))

  (define (unwrap x)
    (match x
      [#(error ,reason) (raise reason)]
      [#(ok ,result) result]))

  (define (conn:get-request who)
    (match (try (gen-server:call who 'get-request (request-timeout)))
      [`(catch #(timeout ,_)) (raise 'http-request-timeout)]
      [,result (unwrap result)]))

  (define (conn:output-ready? who)
    (unwrap (gen-server:call who 'output-ready?)))

  (define (conn:internal-server-error who)
    ;; This is a call rather than cast to ensure the connection gets
    ;; an opportunity to send a response if appropriate. This uses a
    ;; short timeout in case the original failure was a timeout
    ;; waiting for the connection.
    (catch (gen-server:call who 'internal-server-error 100))
    'ok)

  (define http:call-with-ports
    (case-lambda
     [(conn f)
      (unwrap (gen-server:call conn `#(call-with-ports ,f)))]
     [(conn f timeout)
      (unwrap (gen-server:call conn `#(call-with-ports ,f) timeout))]))

  (define (body-allowed? status)
    (not (or (<= 100 status 199) (= status 204) (= status 304))))

  (define http:respond
    (case-lambda
     [(conn status header)
      (unwrap
       (gen-server:call conn `#(respond ,status ,header #f) (response-timeout)))]
     [(conn status header content)
      (http:respond conn status header content (response-timeout))]
     [(conn status header content timeout)
      (unless (bytevector? content)
        (bad-arg 'http:respond content))
      (let ([allow-body? (body-allowed? status)])
        (when (and (not allow-body?) (> (bytevector-length content) 0))
          (throw `#(http-no-response-body-allowed ,status)))
        (unwrap
         (gen-server:call conn
           `#(respond ,status ,header ,(and allow-body? content))
           timeout)))]))

  (define http:respond-file
    (case-lambda
     [(conn status header filename)
      (http:respond-file conn status header filename (response-timeout))]
     [(conn status header filename timeout)
      (unwrap
       (gen-server:call conn `#(respond-file ,status ,header ,filename)
         timeout))]))

  (define http:call-with-form
    (case-lambda
     [(conn header content-limit file-limit files f)
      (http:call-with-form conn header content-limit file-limit files f
        (request-timeout))]
     [(conn header content-limit file-limit files f timeout)
      (arg-check 'http:call-with-form
        [header json:object?]
        [content-limit (lambda (x) (and (fixnum? x) (fx>= x 0)))]
        [file-limit (lambda (x) (and (fixnum? x) (fx>= x 0)))]
        [files (lambda (ls) (and list? (andmap symbol? ls)))]
        [f procedure?]
        [timeout (lambda (x) (and (fixnum? x) (fx> x 0)))])
      (let* ([len (http:get-content-length header)]
             [type (or (http:find-header 'content-type header) "none")]
             [data
              (cond
               [(not len) (json:make-object)]
               [(multipart/form-data-boundary type) =>
                (lambda (boundary)
                  (http:call-with-ports conn
                    (lambda (ip op)
                      (parse-multipart/form-data ip
                        (string-append "--" boundary)
                        content-limit
                        file-limit
                        files))
                    timeout))]
               [(starts-with? type "application/x-www-form-urlencoded")
                (when (> len content-limit)
                  (throw 'http-content-limit-exceeded))
                (http:call-with-ports conn
                  (lambda (ip op)
                    (let ([content (make-bytevector len)])
                      (get-chunk! ip content len)
                      (parse-encoded-kv content 0 len)))
                  timeout)]
               [else (json:make-object)])])
        (on-exit (delete-tmp-files data)
          (f data)))]))

  (define (http-cache:start&link name sup root-dir options)
    ;; The cache maintains a hash table mapping a path to a handler.
    ;; It lazily loads watchers for the web directory and all
    ;; subdirectories.  When anything other than mime-types changes,
    ;; the watchers are cleared.  It uses separate processes to
    ;; interpret the files and a cookie to determine if the result is
    ;; valid to be placed in the cache.
    (define-state-tuple <http-cache>
      cookie     ; integer
      mime-types ; #f | file-extension -> media-type
      pages      ; path -> #(loaded ,handler) | #(loading ,path ,pid ,waiters)
      watchers   ; (path-watcher ...)
      )
    (define cache-timeout (* 5 60 1000))
    (define empty-ht (ht:make string-ci-hash string-ci=? string?))
    (define (read-mime-types)
      (let ([ip (open-file-to-read (path-combine root-dir "mime-types"))])
        (on-exit (close-port ip)
          (let lp ([ht empty-ht])
            (match (read ip)
              [,eof (guard (eof-object? eof)) ht]
              [(,ext . ,media-type)
               (guard (and (string? ext) (string? media-type)))
               (lp (ht:set ht ext media-type))]
              [,x (throw `#(http-invalid-media-type ,x))])))))
    (define (update-mime-types state)
      (if ($state mime-types)
          state
          (let ([state (load-watchers state)])
            ($state copy [mime-types (read-mime-types)]))))
    (define (url->abs-paths path) ;; path starts with "/"
      (let ([path (path-combine root-dir
                    (substring path 1 (string-length path)))]
            [search-extensions (file-search-extensions)])
        (cond
         [(directory-separator? (string-ref path (- (string-length path) 1)))
          (values
           (map
            (lambda (ext) (string-append path "index" ext))
            search-extensions)
           #f)]
         [(string=? (path-extension path) "")
          (values
           (map
            (lambda (ext) (string-append path ext))
            search-extensions)
           (map
            (lambda (ext) (string-append path "/index" ext))
            search-extensions))]
         [else (values (list path) #f)])))
    (define (lookup-handler pages paths)
      (exists (lambda (path) (ht:ref pages path #f)) paths))
    (define (make-static-file-handler path)
      (http:url-handler
       (http:respond-file conn 200 '() path)))
    (define (start-evaluator abs-path cookie eval-file)
      (let ([me self])
        (match-let*
         ([#(ok ,pid)
           (watcher:start-child sup (gensym "http-evaluator")
             1000
             (lambda ()
               `#(ok ,(spawn&link
                       (lambda ()
                         (send me `#(load-finished ,self ,abs-path ,cookie
                                      ,(eval-file root-dir abs-path))))))))])
         (link pid)
         pid)))
    (define (init)
      (process-trap-exit #t)
      (install-options options)
      `#(ok ,(<http-cache> make
               [cookie 0]
               [mime-types #f]
               [pages empty-ht]
               [watchers '()])))
    (define (terminate reason state)
      (for-each close-path-watcher ($state watchers)))
    (define (handle-call msg from state)
      (match msg
        [#(get-handler ,request)
         (<request> open request (method path original-path))
         (let-values ([(paths redirect) (url->abs-paths path)])
           (match (lookup-handler ($state pages) paths)
             [#(loaded ,handler)
              `#(reply #(ok ,handler) ,state ,cache-timeout)]
             [#(loading ,abs-path ,pid ,waiters)
              `#(no-reply
                 ,($state copy* [pages (ht:set pages abs-path
                                         `#(loading ,abs-path ,pid
                                             ,(cons from waiters)))])
                 ,cache-timeout)]
             [#f
              (let* ([state (load-watchers state)]
                     [abs-path (find regular-file? paths)])
                (cond
                 [(not abs-path)
                  `#(reply
                     #(ok ,(and redirect
                                (exists
                                 (lambda (fn)
                                   (and (regular-file? fn)
                                        (http:url-handler
                                         (http:respond conn 302
                                           `(("Location" . ,(string-append original-path "/")))
                                           '#vu8()))))
                                 redirect)))
                     ,state ,cache-timeout)]
                 [((file-transform) abs-path) =>
                  (lambda (eval-file)
                    (let ([pid (start-evaluator abs-path ($state cookie)
                                 eval-file)])
                      `#(no-reply
                         ,($state copy* [pages (ht:set pages abs-path
                                                 `#(loading ,abs-path ,pid
                                                     ,(list from)))])
                         ,cache-timeout)))]
                 [(eq? method 'GET)
                  (let ([h (make-static-file-handler abs-path)])
                    `#(reply #(ok ,h)
                        ,($state copy* [pages (ht:set pages abs-path
                                                `#(loaded ,h))])
                        ,cache-timeout))]
                 [else
                  `#(reply
                     #(ok ,(http:url-handler
                            (not-found conn)
                            (throw `#(http-invalid-method ,method ,path))))
                     ,state
                     ,cache-timeout)]))]))]
        [#(get-media-type ,ext)
         (let ([state (update-mime-types state)])
           (match (ht:ref ($state mime-types) ext #f)
             [#f `#(reply #(error not-found) ,state ,cache-timeout)]
             [,type `#(reply #(ok ,type) ,state ,cache-timeout)]))]))
    (define (handle-cast msg state) (match msg))
    (define (load-watchers state)
      (define (watch path watchers)
        (fold-left
         (lambda (watchers x)
           (match x
             [(,fn . ,@DIRENT_DIR) (watch (path-combine path fn) watchers)]
             [,_ watchers]))
         (cons (watch-path path self) watchers)
         (list-directory path)))
      (if (null? ($state watchers))
          ($state copy [watchers (watch root-dir '())])
          state))
    (define (clear-cache state)
      ($state copy*
        [cookie (cond
                 [(null? watchers) cookie]
                 [else
                  (for-each close-path-watcher watchers)
                  (+ cookie 1)])]
        [mime-types #f]
        [pages
         (ht:fold pages
           (lambda (key val acc)
             (match val
               [#(loaded ,_) acc]
               [,_ (ht:set acc key val)]))
           empty-ht)]
        [watchers '()]))
    (define (handle-info msg state)
      (match msg
        [timeout `#(stop normal ,state)]
        [#(path-changed ,path ,filename ,_)
         `#(no-reply
            ,(if (and (string=? path root-dir)
                      (string=? filename "mime-types"))
                 ($state copy [mime-types #f])
                 (clear-cache state))
            ,cache-timeout)]
        [#(load-finished ,pid ,abs-path ,cookie ,handler)
         (match (ht:ref ($state pages) abs-path #f)
           [#(loading ,abs-path ,@pid ,waiters)
            (for-each
             (lambda (w)
               (gen-server:reply w `#(ok ,handler)))
             waiters)
            `#(no-reply ,(if (eqv? ($state cookie) cookie)
                             ($state copy*
                               [pages (ht:set pages abs-path
                                        `#(loaded ,handler))])
                             ($state copy*
                               [pages (ht:delete pages abs-path)]))
                ,cache-timeout)]
           [,_ `#(no-reply ,state ,cache-timeout)])]
        [`(EXIT ,_ normal) `#(no-reply ,state ,cache-timeout)]
        [`(EXIT ,pid ,reason)
         `#(no-reply
            ,(call/cc
              (lambda (return)
                (ht:fold ($state pages)
                  (lambda (key val acc)
                    (match val
                      [#(loading ,_ ,@pid ,waiters)
                       (for-each
                        (lambda (w)
                          (gen-server:reply w `#(error ,reason)))
                        waiters)
                       (return ($state copy* [pages (ht:delete pages key)]))]
                      [,_ acc]))
                  state)))
            ,cache-timeout)]))
    (gen-server:start&link name))

  (define (http-cache:get-media-type who extension)
    (gen-server:call who `#(get-media-type ,extension)))

  (define (http-cache:get-handler who request)
    (match (gen-server:call who `#(get-handler ,request) 'infinity)
      [#(error ,reason) (raise reason)]
      [#(ok ,result) result]))

  (define cache-manager (make-process-parameter #f))

  (define (cache-mgr:start&link sup options)
    (define-state-tuple <cache-mgr> path->pid pid->path)
    (define (init)
      `#(ok ,(<cache-mgr> make
               [path->pid (ht:make string-hash string=? string?)]
               [pid->path (ht:make process-id eq? process?)])))
    (define (terminate reason state) 'ok)
    (define (handle-call msg from state)
      (match msg
        [#(get-cache ,path)
         (cond
          [(ht:ref ($state path->pid) path #f) =>
           (lambda (pid) `#(reply #(ok ,pid) ,state))]
          [else
           (match-let*
            ([#(ok ,pid)
              (watcher:start-child sup (gensym "http-cache") 1000
                (lambda () (http-cache:start&link #f sup path options)))])
            (monitor pid)
            `#(reply #(ok ,pid)
                ,($state copy*
                   [path->pid (ht:set path->pid path pid)]
                   [pid->path (ht:set pid->path pid path)])))])]))
    (define (handle-cast msg state) (match msg))
    (define (handle-info msg state)
      (match msg
        [`(DOWN ,_ ,pid ,_)
         (cond
          [(ht:ref ($state pid->path) pid #f) =>
           (lambda (path)
             `#(no-reply
                ,($state copy*
                   [path->pid (ht:delete path->pid path)]
                   [pid->path (ht:delete pid->path pid)])))]
          [else
           `#(no-reply ,state)])]))
    (gen-server:start&link #f))

  (define (cache-mgr:get-cache who path)
    (match (gen-server:call who `#(get-cache ,path))
      [#(error ,reason) (raise reason)]
      [#(ok ,result) result]))

  (define (http:make-default-file-url-handler dir)
    (http:url-handler
     (let ([pid (cache-mgr:get-cache (cache-manager) dir)])
       (cond
        [(http-cache:get-handler pid request) =>
         (lambda (handler)
           (handler conn request header params))]
        [else #f]))))

  (define (http:make-default-media-type-handler dir)
    (lambda (ext)
      (let ([pid (cache-mgr:get-cache (cache-manager) dir)])
        (match (http-cache:get-media-type pid ext)
          [#(error not-found) #f]
          [#(ok ,type) type]))))

  (define (read-line ip limit)
    (let ([x (lookahead-u8 ip)])
      (if (eof-object? x)
          x
          (let-values ([(op get) (open-bytevector-output-port)])
            (let lp ([n 0])
              (when (fx> n limit)
                (throw 'http-input-limit-exceeded))
              (let ([x (get-u8 ip)])
                (cond
                 [(eof-object? x) (get)]
                 [(eqv? x (char->integer #\newline)) (get)]
                 [(eqv? x (char->integer #\return))
                  (when (eqv? (lookahead-u8 ip) (char->integer #\newline))
                    (get-u8 ip))
                  (get)]
                 [else
                  (put-u8 op x)
                  (lp (fx+ n 1))])))))))

  (define (bv-match-positions bv sep n)
    (let ([limit (bytevector-length bv)])
      (let lp ([e 0] [n n])
        (cond
         [(or (fx= e limit) (fx= n 0)) '()]
         [(fx= (bytevector-u8-ref bv e) sep)
          (cons e (lp (fx+ e 1) (fx- n 1)))]
         [else (lp (fx+ e 1) n)]))))

  (define (bv-next-non-lws bv i)
    (or (and (fx< i (bytevector-length bv))
             (let ([c (bytevector-u8-ref bv i)])
               (cond
                [(or (fx= c (char->integer #\space))
                     (fx= c (char->integer #\tab)))
                 (bv-next-non-lws bv (fx+ i 1))]
                [else i])))
        i))

  (define (bv-extract-string bv start end)
    (let* ([len (fx- end start)]
           [buf (make-bytevector len)])
      (bytevector-copy! bv start buf 0 len)
      (utf8->string buf)))

  (define (parse-encoded-string bv i end stop-char)
    (let-values ([(op get) (open-bytevector-output-port)])
      (let lp ([i i])
        (if (fx>= i end)
            (values i (utf8->string (get)))
            (let ([c (bytevector-u8-ref bv i)])
              (cond
               [(fx= c (char->integer stop-char))
                (values i (utf8->string (get)))]
               [(fx= c (char->integer #\+))
                (put-u8 op (char->integer #\space))
                (lp (fx+ i 1))]
               [(and (fx= c (char->integer #\%)) (decode bv i end)) =>
                (lambda (h)
                  (put-u8 op h)
                  (lp (fx+ i 3)))]
               [else
                (put-u8 op c)
                (lp (fx+ i 1))]))))))

  (define (parse-encoded-kv bv i end)
    (let ([ht (json:make-object)])
      (let lp ([i i])
        (if (fx>= i end)
            ht
            (let*-values
                ([(stop key) (parse-encoded-string bv i end #\=)]
                 [(stop val) (parse-encoded-string bv (fx+ stop 1) end #\&)])
              (json:set! ht (string->symbol key) val)
              (lp (fx+ stop 1)))))))

  (define (decode bv i end)
    (and (fx< (fx+ i 2) end)
         (let ([high (decode-hex (bytevector-u8-ref bv (fx+ i 1)))]
               [low (decode-hex (bytevector-u8-ref bv (fx+ i 2)))])
           (and high low
                (fx+ (fx* high 16) low)))))

  (define (decode-hex c)
    (cond
     [(fx<= (char->integer #\0) c (char->integer #\9))
      (fx- c (char->integer #\0))]
     [(fx<= (char->integer #\A) c (char->integer #\F))
      (fx- c (fx- (char->integer #\A) 10))]
     [(fx<= (char->integer #\a) c (char->integer #\f))
      (fx- c (fx- (char->integer #\a) 10))]
     [else #f]))

  (define-tuple <request> method original-path path params host header)

  (define (http:parse-request bv)
    (match (bv-match-positions bv (char->integer #\space) 2)
      [(,s1 ,s2)
       (cond
        [(string=? "HTTP/1.1"
           (bv-extract-string bv (fx+ s2 1) (bytevector-length bv)))
         (let-values ([(s3 path) (parse-encoded-string bv (fx+ s1 1) s2 #\?)])
           (<request> make
             [method (string->symbol (bv-extract-string bv 0 s1))]
             [original-path path]
             [path path]
             [params (parse-encoded-kv bv (fx+ s3 1) s2)]
             [host #f]
             [header #f]))]
        [else #f])]
      [,_ #f]))

  (define (http:read-header ip limit)
    (let ([hdr (json:make-object)])
      (let lp ([limit limit])
        (match (read-line ip limit)
          [#vu8() hdr]
          [,line
           (match (bv-match-positions line (char->integer #\:) 1)
             [(,colon)
              (let* ([len (bytevector-length line)]
                     [k (bv-extract-string line 0 colon)]
                     [v (bv-extract-string line
                          (bv-next-non-lws line (fx+ colon 1))
                          len)])
                (json:update! hdr (string->symbol (string-downcase k))
                  (lambda (old)
                    (if old
                        (string-append old "," v)
                        v))
                  #f)
                (lp (fx- limit len)))]
             [,_ (throw 'http-invalid-header)])]))))

  (define (multipart/form-data-boundary type)
    (match (pregexp-match (re "^multipart/form-data;\\s*boundary=(.+)$") type)
      [(,_ ,boundary) boundary]
      [#f #f]))

  (define (get-chunk! ip bv n)
    (let ([count (get-bytevector-n! ip bv 0 n)])
      (when (or (eof-object? count) (not (= count n)))
        (throw 'unexpected-eof))))

  (define (http:get-content-length header)
    (cond
     [(http:find-header 'content-length header) =>
      (lambda (x)
        (unless (pregexp-match (re "^[0-9]{1,20}$") x)
          (throw `#(http-invalid-content-length ,x)))
        (string->number x))]
     [else #f]))

  (define (http:handle-input conn url-handler options)
    (let ([request (conn:get-request conn)])
      (when request
        (match (http:process-handler conn url-handler request)
          [#(switch-protocol ,proc)
           (guard (procedure? proc))
           (let-values ([(ip op) (conn:detach-ports conn)])
             (proc ip op))]
          [#t
           (http:handle-input conn url-handler options)]
          [,other
           (raise `#(bad-return-value ,other))]))))

  (define (http:switch-protocol proc)
    `#(switch-protocol ,proc))

  (define (http:read-status ip limit)
    (let ([x (read-line ip limit)])
      (and (bytevector? x)
           (match (bv-match-positions x (char->integer #\space) 2)
             [(8)
              (string->number (bv-extract-string x 9 (bytevector-length x)))]
             [(8 ,e)
              (string->number (bv-extract-string x 9 e))]
             [,_ #f]))))

  (define (http:write-status op status)
    (unless (and (fixnum? status) (fx<= 100 status 599))
      (bad-arg 'http:write-status status))
    (put-bytevector op (string->utf8 (format "HTTP/1.1 ~d \r\n" status))))

  (define (http:write-header op header)
    (put-bytevector op
      (call-with-bytevector-output-port
       (lambda (p)
         (for-each
          (lambda (kv)
            (match kv
              [(,key . ,val)
               (guard (string? key))
               (fprintf p "~a: ~a\r\n" key val)]
              [,_ (bad-arg 'http:write-header header)]))
          header)
         (display-string "\r\n" p))
       (make-utf8-transcoder))))

  (define (find-alist-val name ls pred)
    (cond
     [(assp (lambda (key) (pred key name)) ls) => cdr]
     [else #f]))

  (define (find-header-alist name header)
    (find-alist-val name header string-ci=?))

  (define (add-content-type filename header)
    (cond
     [(find-header-alist "Content-Type" header) header]
     [((media-type-handler) (path-extension filename)) =>
      (lambda (type) (cons (cons "Content-Type" type) header))]
     [else header]))

  (define (add-content-length n header)
    (cons (cons "Content-Length" n) header))

  (define (add-cache-control value header)
    (if (find-header-alist "Cache-Control" header)
        header
        (cons (cons "Cache-Control" value) header)))

  (define (keep-alive? header)
    (let ([c (http:find-header 'connection header)])
      (not (and c (string-ci=? c "close")))))

  (define (internal-find-header who name header)
    (let ([key (cond
                [(symbol? name) name]
                [(string? name) (string->symbol (string-downcase name))]
                [else (bad-arg who name)])])
      (json:ref header key #f)))

  (define (http:find-header name header)
    (internal-find-header 'http:find-header name header))

  (define (http:get-header name header)
    (or (internal-find-header 'http:get-header name header)
        (throw `#(http-invalid-header ,name))))

  (define (internal-find-param who name params)
    (let ([key (cond
                [(symbol? name) name]
                [(string? name) (string->symbol name)]
                [else (bad-arg who name)])])
      (json:ref params key #f)))

  (define (http:find-param name params)
    (internal-find-param 'http:find-param name params))

  (define (http:get-param name params)
    (or (internal-find-param 'http:get-param name params)
        (throw `#(http-invalid-param ,name))))

  (define (http:enable-dynamic-pages path)
    (and (string-ci=? (path-extension path) "ss")
         http:eval-dynamic-page))

  (define (http:eval-dynamic-page root-dir abs-path)
    (do-eval (read-bytevector abs-path (read-file abs-path))
      root-dir
      (path-parent abs-path)))

  (define (do-eval exprs root-dir path)
    (define (http:path-root path fn)
      (and (string? fn)
           (if (starts-with? fn "/")    ; rooted
               root-dir
               path)))
    (define (http:include-help k fn path)
      (define (fail)
        (syntax-error k (format "invalid path ~s in" fn)))
      (let* ([root (or (http:path-root path fn) (fail))]
             [fn (path-combine root fn)]
             [path (path-parent fn)])
        (datum->syntax k
          (wrap-3D-include path
            (read-bytevector fn (read-file fn))))))
    (define (wrap-3D-include path exprs)
      `(let-syntax ([http:include
                     (lambda (x)
                       (syntax-case x ()
                         [(k fn) (',http:include-help #'k (datum fn) ,path)]))])
         ,@exprs))
    (eval
     `(http:url-handler
       (define-syntax find-param
         (syntax-rules ()
           [(_ key) (http:find-param key params)]))
       (define-syntax get-param
         (syntax-rules ()
           [(_ key) (http:get-param key params)]))
       ,(wrap-3D-include path exprs))))

  (define (validate-path path)
    (and (string=? (path-first path) "/")
         (let lp ([path (path-rest path)])
           (let ([first (path-first path)])
             (cond
              [(string=? first "") #t]
              [(string=? first "..") #f]
              [else (lp (path-rest path))])))))

  (define (not-found conn)
    (http:respond conn 404 '()
      (html->bytevector
       `(html5
         (head
          (meta (@ (charset "UTF-8")))
          (title "Page not found"))
         (body
          (h1 "This is not the web page you are looking for."))))))

  (define (http:process-handler conn url-handler request)
    (<request> open request [host method path header params])
    (system-detail <http-request>
      [pid self]
      [host host]
      [method method]
      [path path]
      [header header]
      [params params])
    (cond
     [(not (validate-path path))
      (not-found conn)
      (raise `#(http-invalid-path ,path))]
     [(match (try (limit-stack (url-handler conn request header params)))
        [`(catch ,_ ,e)
         (conn:internal-server-error conn)
         (raise e)]
        [,result result])]
     [else
      (not-found conn)
      (raise `#(http-file-not-found ,path))]))

  (define (http:percent-encode s)
    (define (encode bv i op)
      (when (fx> (bytevector-length bv) i)
        (let ([c (bytevector-u8-ref bv i)])
          (cond
           [(or (fx<= (char->integer #\A) c (char->integer #\Z))
                (fx<= (char->integer #\a) c (char->integer #\z))
                (fx<= (char->integer #\0) c (char->integer #\9))
                (fx= c (char->integer #\-))
                (fx= c (char->integer #\_))
                (fx= c (char->integer #\.))
                (fx= c (char->integer #\~)))
            (write-char (integer->char c) op)
            (encode bv (fx+ i 1) op)]
           [else
            (fprintf op "%~2,'0X" c)
            (encode bv (fx+ i 1) op)]))))
    (let-values ([(op get) (open-string-output-port)])
      (encode (string->utf8 s) 0 op)
      (get)))

  (define (next-u8 ip)
    (let ([x (get-u8 ip)])
      (when (eof-object? x)
        (throw 'unexpected-eof))
      x))

  (define (make-bit-sink)
    (make-custom-binary-output-port "bit-sink port"
      (lambda (bv start n) n) #f #f #f))

  (define (parse-multipart/form-data ip boundary content-limit file-limit files)
    (define result (json:make-object))
    (define copy-until-match
      (make-copy-until-match
       (string->utf8 (string-append "\r\n" boundary))))
    (define (parse-end climit flimit)
      (let* ([x1 (next-u8 ip)] [x2 (next-u8 ip)])
        (cond
         [(and (eqv? x1 (char->integer #\return))
               (eqv? x2 (char->integer #\newline)))
          (parse-next climit flimit)]
         [(and (eqv? x1 (char->integer #\-))
               (eqv? x2 (char->integer #\-))
               (eqv? (next-u8 ip) (char->integer #\return))
               (eqv? (next-u8 ip) (char->integer #\newline)))
          result]
         [else (throw 'http-invalid-multipart-boundary)])))
    (define (parse-next climit flimit)
      (define header (http:read-header ip (header-limit)))
      (define data
        (parse-form-data-disposition
         (http:get-header 'content-disposition header)))
      (define name (find-alist-val "name" data string=?))
      (cond
       [(find-alist-val "filename" data string=?)
        (let ([key (string->symbol name)])
          (cond
           [(and (memq key files) (not (json:ref result key #f)))
            (parse-file key climit flimit)]
           [else
            (let ([n (copy-until-match ip (make-bit-sink) flimit
                       'http-file-upload-limit-exceeded)])
              (parse-end climit (- flimit n)))]))]
       [else
        (let-values ([(op get) (open-bytevector-output-port)])
          (let* ([n (copy-until-match ip op climit
                      'http-content-limit-exceeded)]
                 [content (get)])
            (json:set! result (string->symbol name) (utf8->string content))
            (parse-end (- climit n) flimit)))]))
    (define (parse-file key climit flimit)
      (let* ([fn (make-directory-path
                  (path-combine (tmp-dir)
                    (format "~36r.tmp"
                      (bytevector-uint-ref (osi_make_uuid) 0 'little 16))))]
             [op (parameterize ([custom-port-buffer-size (ash 1 16)])
                   (open-binary-file-to-write fn))])
        (match (try
                (let ([n (copy-until-match ip op flimit
                           'http-file-upload-limit-exceeded)])
                  (close-port op)
                  (json:set! result key
                    (json:make-object
                     [type "file"]
                     [filename fn]))
                  (parse-end climit (- flimit n))))
          [`(catch ,_ ,e)
           (force-close-output-port op)
           (delete-file fn)
           (raise e)]
          [,params params])))
    ;; The boundary occurs first.
    (do ([i 0 (fx+ i 1)] [bv (string->utf8 boundary)])
        [(= i (bytevector-length bv))]
      (unless (eqv? (next-u8 ip) (bytevector-u8-ref bv i))
        (throw 'http-invalid-multipart-boundary)))
    (parse-end content-limit file-limit))

  (define (parse-form-data-disposition d)
    (match (pregexp-match (re "^form-data;\\s*(.*)$") d)
      [(,_ ,params)
       (let lp ([params params])
         (if params
             (match (pregexp-match (re "^([^=]+)=\"([^\"]*)\"(?:;\\s*(.*))?$")
                      params)
               [(,_ ,key ,val ,params)
                (cons (cons key val) (lp params))]
               [#f (throw `#(http-invalid-content-disposition ,d))])
             '()))]
      [#f (throw `#(http-invalid-content-disposition ,d))]))

  (define (delete-tmp-files params)
    (vector-for-each
     (lambda (x)
       (cond
        [(and (json:object? x)
              (equal? (json:ref x 'type #f) "file")
              (json:ref x 'filename #f))
         => delete-file]))
     (hashtable-values params)))

  ;; Knuth Morris Pratt
  (define (make-copy-until-match pattern)
    (define pattern-length (bytevector-length pattern))
    (define partial (make-fxvector pattern-length))
    (define (copy-until-match ip op limit err)
      (define buffer (make-bytevector pattern-length))
      (define (populate-buffer i pos limit)
        (cond
         [(fx< i pattern-length)
          (let ([x (next-u8 ip)])
            (bytevector-u8-set! buffer i x)
            (populate-buffer (fx+ i 1) (update-pos x pos)
              (update-limit limit)))]
         [(fx< pos pattern-length) (copy 0 pos limit)]))
      (define (copy i pos limit)
        (let ([x (next-u8 ip)])
          (put-u8 op (bytevector-u8-ref buffer i))
          (bytevector-u8-set! buffer i x)
          (let ([pos (update-pos x pos)])
            (when (fx< pos pattern-length)
              (copy (next-index i) pos (update-limit limit))))))
      (define (next-index i) (fxmodulo (fx+ i 1) pattern-length))
      (define (update-pos x pos)
        (if (and (fx> pos -1) (not (fx= (bytevector-u8-ref pattern pos) x)))
            (update-pos x (fxvector-ref partial pos))
            (fx+ pos 1)))
      (define (update-limit limit)
        (and limit
             (if (fx> limit 0)
                 (fx- limit 1)
                 (throw err))))
      (let ([pos (port-position ip)])
        (populate-buffer 0 0 limit)
        (- (port-position ip) pos pattern-length)))
    ;; Build partial match table
    (fxvector-set! partial 0 -1)
    (let build ([pos 1] [cnd -1])
      (if (fx< pos pattern-length)
          (let update ([x (bytevector-u8-ref pattern (fx- pos 1))] [cnd cnd])
            (if (and (fx> cnd -1)
                     (not (fx= x (bytevector-u8-ref pattern cnd))))
                (update x (fxvector-ref partial cnd))
                (let ([cnd (fx+ cnd 1)])
                  (if (fx= (bytevector-u8-ref pattern pos)
                           (bytevector-u8-ref pattern cnd))
                      (fxvector-set! partial pos (fxvector-ref partial cnd))
                      (fxvector-set! partial pos cnd))
                  (build (fx+ pos 1) cnd))))
          copy-until-match))))
