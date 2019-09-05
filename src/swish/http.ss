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
   http-content-limit
   http-header-limit
   http-port-number
   http-request-limit
   http-sup:start&link
   http:find-header
   http:find-param
   http:get-content-length
   http:get-header
   http:get-param
   http:get-port-number
   http:handle-input
   http:percent-encode
   http:read-header
   http:read-status
   http:respond
   http:respond-file
   http:write-header
   http:write-status
   )
  (import
   (chezscheme)
   (swish app-io)
   (swish erlang)
   (swish event-mgr)
   (swish events)
   (swish gen-server)
   (swish ht)
   (swish html)
   (swish io)
   (swish osi)
   (swish pregexp)
   (swish string-utils)
   (swish supervisor)
   (swish watcher)
   )

  (define http-port-number
    (make-parameter #f
      (lambda (x)
        (unless (or (not x) (and (fixnum? x) (fx<= 0 x 65535)))
          (bad-arg 'http-port-number x))
        x)))

  (define http-request-limit
    (make-parameter 4096
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'http-request-limit x))
        x)))

  (define http-header-limit
    (make-parameter 1048576
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'http-header-limit x))
        x)))

  (define http-content-limit
    (make-parameter 4194304
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'http-content-limit x))
        x)))

  (define (http-sup:start&link)
    (if (not (http-port-number))
        'ignore
        (supervisor:start&link 'http-sup 'one-for-one 10 10000
          `(#(http-cache ,http-cache:start&link permanent 1000 worker)
            #(http-listener ,http-listener:start&link permanent 1000 worker)))))

  (define (http:get-port-number)
    (gen-server:call 'http-listener 'get-port-number))

  (define (http-listener:start&link)
    (define (init)
      (process-trap-exit #t)
      `#(ok ,(listen-tcp "::" (http-port-number) self)))
    (define (terminate reason state) (close-tcp-listener state))
    (define (handle-call msg from state)
      (match msg
        [get-port-number `#(reply ,(listener-port-number state) ,state)]))
    (define (handle-cast msg state) (match msg))
    (define (handle-info msg state)
      (match msg
        [#(accept-tcp ,_ ,ip ,op)
         (match-let*
          ([#(ok ,pid)
            (watcher:start-child 'http-sup (gensym "http-connection") 1000
              (lambda ()
                `#(ok ,(spawn&link
                        (lambda ()
                          (on-exit (force-close-output-port op)
                            (http:handle-input ip op)))))))])
          `#(no-reply ,state))]
        [#(accept-tcp-failed ,l ,who ,errno)
         (raise `#(accept-tcp-failed ,l ,who ,errno))]))
    (gen-server:start&link 'http-listener))

  (define (http-cache:get-content-type extension)
    (gen-server:call 'http-cache `#(get-content-type ,extension)))

  (define (http-cache:get-handler method path)
    (match (gen-server:call 'http-cache `#(get-handler ,method ,path) 'infinity)
      [#(error ,reason) (raise reason)]
      [#(ok ,result) result]))

  (define (http-cache:start&link)
    ;; The cache maintains a hash table mapping a path to a handler.
    ;; It lazily loads watchers for the web directory and all
    ;; subdirectories.  When anything other than mime-types changes,
    ;; the watchers are cleared.  It uses separate processes to
    ;; interpret the files and a cookie to determine if the result is
    ;; valid to be placed in the cache.
    (define-state-tuple <http-cache>
      cookie     ; integer
      mime-types ; #f | file-extension -> content-type
      pages      ; path -> #(loaded ,handler) | #(loading ,path ,pid ,waiters)
      watchers   ; (path-watcher ...)
      )
    (define empty-ht (ht:make string-ci-hash string-ci=? string?))
    (define (read-mime-types)
      (let ([ip (open-file-to-read (path-combine (web-dir) "mime-types"))])
        (on-exit (close-port ip)
          (let lp ([ht empty-ht])
            (match (read ip)
              [,eof (guard (eof-object? eof)) ht]
              [(,ext . ,content-type)
               (guard (and (string? ext) (string? content-type)))
               (lp (ht:set ht ext content-type))]
              [,x (raise `#(invalid-mime-type ,x))])))))
    (define (update-mime-types state)
      (if ($state mime-types)
          state
          (let ([state (load-watchers state)])
            ($state copy [mime-types (read-mime-types)]))))
    (define (url->abs-paths path) ;; path starts with "/"
      (let ([path (path-combine (web-dir)
                    (substring path 1 (string-length path)))])
        (cond
         [(directory-separator? (string-ref path (- (string-length path) 1)))
          (list (string-append path "index.ss"))]
         [(string=? (path-extension path) "")
          (list path
            (string-append path ".ss")
            (path-combine path "index.ss"))]
         [else (list path)])))
    (define (lookup-handler pages paths)
      (exists (lambda (path) (ht:ref pages path #f)) paths))
    (define (make-static-file-handler path)
      (lambda (ip op request header params)
        (http:respond-file op 200 '() path)))
    (define (start-interpreter abs-path cookie)
      (let ([me self])
        (match-let*
         ([#(ok ,pid)
           (watcher:start-child 'http-sup (gensym "http-interpreter")
             1000
             (lambda ()
               `#(ok ,(spawn&link
                       (lambda ()
                         (send me `#(load-finished ,self ,abs-path ,cookie
                                      ,(do-eval-file abs-path))))))))])
         (link pid)
         pid)))
    (define (init)
      (process-trap-exit #t)
      `#(ok ,(<http-cache> make
               [cookie 0]
               [mime-types #f]
               [pages empty-ht]
               [watchers '()])))
    (define (terminate reason state)
      (for-each close-path-watcher ($state watchers)))
    (define (handle-call msg from state)
      (match msg
        [#(get-handler ,method ,path)
         (let ([paths (url->abs-paths path)])
           (match (lookup-handler ($state pages) paths)
             [#(loaded ,handler)
              `#(reply #(ok ,handler) ,state)]
             [#(loading ,abs-path ,pid ,waiters)
              `#(no-reply
                 ,($state copy* [pages (ht:set pages abs-path
                                         `#(loading ,abs-path ,pid
                                             ,(cons from waiters)))]))]
             [#f
              (let* ([state (load-watchers state)]
                     [abs-path (find regular-file? paths)])
                (cond
                 [(not abs-path)
                  `#(reply #(ok #f) ,state)]
                 [(string-ci=? (path-extension abs-path) "ss")
                  (let ([pid (start-interpreter abs-path ($state cookie))])
                    `#(no-reply
                       ,($state copy* [pages (ht:set pages abs-path
                                               `#(loading ,abs-path ,pid
                                                   ,(list from)))])))]
                 [(eq? method 'GET)
                  (let ([h (make-static-file-handler abs-path)])
                    `#(reply #(ok ,h)
                        ,($state copy* [pages (ht:set pages abs-path
                                                `#(loaded ,h))])))]
                 [else
                  `#(reply
                     #(ok ,(lambda (ip op request header params)
                             (not-found op)
                             (raise `#(invalid-http-method ,method ,path))))
                     ,state)]))]))]
        [#(get-content-type ,ext)
         (let ([state (update-mime-types state)])
           (match (ht:ref ($state mime-types) ext #f)
             [#f `#(reply #(error not-found) ,state)]
             [,type `#(reply #(ok ,type) ,state)]))]))
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
          ($state copy [watchers (watch (web-dir) '())])
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
        [#(path-changed ,path ,filename ,_)
         `#(no-reply
            ,(if (and (string=? path (web-dir))
                      (string=? filename "mime-types"))
                 ($state copy [mime-types #f])
                 (clear-cache state)))]
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
                               [pages (ht:delete pages abs-path)])))]
           [,_ `#(no-reply ,state)])]
        [#(EXIT ,_ normal) `#(no-reply ,state)]
        [#(EXIT ,pid ,reason)
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
                  state))))]))
    (gen-server:start&link 'http-cache))

  (define (read-line ip limit)
    (let ([x (lookahead-u8 ip)])
      (if (eof-object? x)
          x
          (let-values ([(op get) (open-bytevector-output-port)])
            (let lp ([n 0])
              (when (fx> n limit)
                (raise 'input-limit-exceeded))
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
      (let lp ([s 0] [e 0] [n n])
        (cond
         [(or (fx= e limit) (fx= n 0)) '()]
         [(fx= (bytevector-u8-ref bv e) sep)
          (cons e (lp (fx+ e 1) (fx+ e 1) (fx- n 1)))]
         [else (lp s (fx+ e 1) n)]))))

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
    (if (fx>= i end)
        '()
        (let*-values
            ([(stop key) (parse-encoded-string bv i end #\=)]
             [(stop val) (parse-encoded-string bv (fx+ stop 1) end #\&)])
          (cons (cons key val) (parse-encoded-kv bv (fx+ stop 1) end)))))

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

  (define-tuple <request> method path query)

  (define (http:parse-request bv)
    (match (bv-match-positions bv (char->integer #\space) 2)
      [(,s1 ,s2)
       (cond
        [(string=? "HTTP/1.1"
           (bv-extract-string bv (fx+ s2 1) (bytevector-length bv)))
         (let-values ([(s3 path) (parse-encoded-string bv (fx+ s1 1) s2 #\?)])
           (<request> make
             [method (string->symbol (bv-extract-string bv 0 s1))]
             [path path]
             [query (parse-encoded-kv bv (fx+ s3 1) s2)]))]
        [else #f])]
      [,_ #f]))

  (define (http:read-header ip limit)
    (match (read-line ip limit)
      [#vu8() '()]
      [,line
       (match (bv-match-positions line (char->integer #\:) 1)
         [(,colon)
          (cons (cons (bv-extract-string line 0 colon)
                  (bv-extract-string line (bv-next-non-lws line (fx+ colon 1))
                    (bytevector-length line)))
            (http:read-header ip (fx- limit (bytevector-length line))))]
         [,_ (raise 'invalid-header)])]))

  (define (multipart/form-data-boundary type)
    (match (pregexp-match (re "^multipart/form-data;\\s*boundary=(.+)$") type)
      [(,_ ,boundary) boundary]
      [#f #f]))

  (define (get-chunk! ip bv n)
    (let ([count (get-bytevector-n! ip bv 0 n)])
      (when (or (eof-object? count) (not (= count n)))
        (raise 'unexpected-eof))))

  (define (advance! ip ipos op opos)
    (when ipos
      (let ([remaining (- ipos (port-position ip))])
        (when (< remaining 0)
          (when (= opos (port-position op))
            (internal-server-error op))
          (raise 'http-violation))
        (do ([n remaining (fx- n 1)] [b #f (get-u8 ip)])
            ((fx= n 0)
             (when (eof-object? b)
               (raise 'unexpected-eof)))))))

  (define (http:get-content-length header)
    (cond
     [(http:find-header "Content-Length" header) =>
      (lambda (x)
        (unless (pregexp-match (re "^[0-9]+$") x)
          (raise `#(invalid-content-length ,x)))
        (string->number x))]
     [else #f]))

  (define (http:get-content-params&pos ip header)
    (cond
     [(http:get-content-length header) =>
      (lambda (len)
        (let ([type (or (http:find-header "Content-Type" header) "none")]
              [pos (+ (port-position ip) len)])
          (cond
           [(multipart/form-data-boundary type) =>
            (lambda (boundary)
              (values
               (parse-multipart/form-data ip (string-append "--" boundary))
               pos))]
           [(starts-with? type "application/x-www-form-urlencoded")
            (when (> len (http-content-limit))
              (raise 'content-limit-exceeded))
            (let ([content (make-bytevector len)])
              (get-chunk! ip content len)
              (values (parse-encoded-kv content 0 len) pos))]
           [else (values '() pos)])))]
     [else (values '() #f)]))

  (define (http:handle-input ip op)
    (let ([x (read-line ip (http-request-limit))])
      (cond
       [(eof-object? x) (raise 'normal)]
       [(http:parse-request x) =>
        (lambda (request)
          (match request
            [`(<request> ,query)
             (define header (http:read-header ip (http-header-limit)))
             (let-values ([(params ipos)
                           (http:get-content-params&pos ip header)])
               (define opos (port-position op))
               (on-exit (delete-tmp-files params)
                 (http:file-handler ip op request header
                   (append query params)))
               (when (keep-alive? header)
                 (advance! ip ipos op opos)
                 (http:handle-input ip op)))]))]
       [else
        (raise `#(unhandled-input ,x))])))

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

  (define (http:respond op status header content)
    (unless (bytevector? content)
      (bad-arg 'http:respond content))
    (http:write-status op status)
    (http:write-header op
      (add-content-length (bytevector-length content)
        (add-cache-control "no-cache" header)))
    (put-bytevector op content)
    (flush-output-port op))

  (define (add-content-type filename header)
    (if (http:find-header "Content-Type" header)
        header
        (match (http-cache:get-content-type (path-extension filename))
          [#(error not-found) header]
          [#(ok ,content-type)
           (cons (cons "Content-Type" content-type) header)])))

  (define (add-content-length n header)
    (cons (cons "Content-Length" n) header))

  (define (add-cache-control value header)
    (if (http:find-header "Cache-Control" header)
        header
        (cons (cons "Cache-Control" value) header)))

  (define (http:respond-file op status header filename)
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
                  (raise 'unexpected-eof))
                (put-bytevector op buffer 0 count)
                (lp (+ fp count))))))
        (flush-output-port op))))

  (define (keep-alive? header)
    (let ([c (http:find-header "Connection" header)])
      (not (and c (string-ci=? c "close")))))

  (define (internal-find-header who name header)
    (unless (string? name)
      (bad-arg who name))
    (cond
     [(assp (lambda (key) (string-ci=? key name)) header) => cdr]
     [else #f]))

  (define (http:find-header name header)
    (internal-find-header 'http:find-header name header))

  (define (http:get-header name header)
    (or (internal-find-header 'http:get-header name header)
        (raise `#(invalid-header ,name ,header))))

  (define (internal-find-param who name params)
    (unless (string? name)
      (bad-arg who name))
    (cond
     [(assp (lambda (key) (string=? key name)) params) => cdr]
     [else #f]))

  (define (http:find-param name params)
    (internal-find-param 'http:find-param name params))

  (define (http:get-param name params)
    (or (internal-find-param 'http:get-param name params)
        (raise `#(invalid-param ,name ,params))))

  (define (do-eval-file abs-path)
    (do-eval (read-bytevector abs-path (read-file abs-path))
      (path-parent abs-path)))

  (define (do-eval exprs path)
    (define (http:path-root path fn)
      (and (string? fn)
           (if (starts-with? fn "/") ; rooted
               (web-dir)
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
     `(lambda (ip op request header params)
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

  (define (not-found op)
    (http:respond op 404 '()
      (html->bytevector
       `(html5
         (head
          (meta (@ (charset "UTF-8")))
          (title "Page not found"))
         (body
          (h1 "This is not the web page you are looking for."))))))

  (define (internal-server-error op)
    (http:respond op 500 '()
      (html->bytevector
       `(html5
         (head
          (meta (@ (charset "UTF-8")))
          (title "Internal server error"))
         (body
          (h1 "The server was unable to complete the request."))))))

  (define (http:file-handler ip op request header params)
    (<request> open request (method path))
    (system-detail <http-request>
      [pid self]
      [host (port-name ip)]
      [method method]
      [path path]
      [header header]
      [params params])
    (let ([start-pos (port-position op)])
      (match
       (catch
        (cond
         [(not (validate-path path))
          (not-found op)
          (raise `#(invalid-http-path ,path))]
         [(http-cache:get-handler method path) =>
          (lambda (handler)
            (handler ip op request header params)
            (flush-output-port op))]
         [else
          (not-found op)
          (raise `#(http-file-not-found ,path))]))
       [#(EXIT ,reason)
        (when (= start-pos (port-position op))
          (internal-server-error op))
        (raise reason)]
       [,_ (void)])))

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
        (raise 'unexpected-eof))
      x))

  (define (parse-multipart/form-data ip boundary)
    (define copy-until-match
      (make-copy-until-match
       (string->utf8 (string-append "\r\n" boundary))))
    (define (parse-end limit)
      (let* ([x1 (next-u8 ip)] [x2 (next-u8 ip)])
        (cond
         [(and (eqv? x1 (char->integer #\return))
               (eqv? x2 (char->integer #\newline)))
          (parse-next limit)]
         [(and (eqv? x1 (char->integer #\-))
               (eqv? x2 (char->integer #\-))
               (eqv? (next-u8 ip) (char->integer #\return))
               (eqv? (next-u8 ip) (char->integer #\newline)))
          '()]
         [else (raise 'invalid-multipart-boundary)])))
    (define (parse-next limit)
      (define header (http:read-header ip (http-header-limit)))
      (define data
        (parse-form-data-disposition
         (http:get-header "Content-Disposition" header)))
      (define name (http:get-param "name" data))
      (cond
       [(http:find-param "filename" data) (parse-file name limit)]
       [else
        (let-values ([(op get) (open-bytevector-output-port)])
          (copy-until-match ip op limit)
          (let ([content (get)])
            (cons (cons name (utf8->string content))
              (parse-end (- limit (bytevector-length content))))))]))
    (define (parse-file name limit)
      (let* ([fn (make-directory-path
                  (path-combine (tmp-dir)
                    (format "~36r.tmp"
                      (bytevector-uint-ref (osi_make_uuid) 0 'little 16))))]
             [op (parameterize ([custom-port-buffer-size (ash 1 16)])
                   (open-binary-file-to-write fn))])
        (match (catch
                (copy-until-match ip op #f)
                (close-port op)
                (cons (cons name `#(<file> ,fn)) (parse-end limit)))
          [#(EXIT ,reason)
           (force-close-output-port op)
           (delete-file fn)
           (raise reason)]
          [,params params])))
    ;; The boundary occurs first.
    (do ([i 0 (fx+ i 1)] [bv (string->utf8 boundary)])
        [(= i (bytevector-length bv))]
      (unless (eqv? (next-u8 ip) (bytevector-u8-ref bv i))
        (raise 'invalid-multipart-boundary)))
    (parse-end (http-content-limit)))

  (define (parse-form-data-disposition d)
    (match (pregexp-match (re "^form-data;\\s*(.*)$") d)
      [(,_ ,params)
       (let lp ([params params])
         (if params
             (match (pregexp-match (re "^([^=]+)=\"([^\"]*)\"(?:;\\s*(.*))?$")
                      params)
               [(,_ ,key ,val ,params)
                (cons (cons key val) (lp params))]
               [#f (raise `#(invalid-content-disposition ,d))])
             '()))]
      [#f (raise `#(invalid-content-disposition ,d))]))

  (define (delete-tmp-files params)
    (for-each
     (lambda (x)
       (match x
         [(,_ . #(<file> ,fn)) (delete-file fn)]
         [,_ (void)]))
     params))

  ;; Knuth Morris Pratt
  (define (make-copy-until-match pattern)
    (define pattern-length (bytevector-length pattern))
    (define partial (make-fxvector pattern-length))
    (define (copy-until-match ip op limit)
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
                 (raise 'content-limit-exceeded))))
      (populate-buffer 0 0 limit))
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
