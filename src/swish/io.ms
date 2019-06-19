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
 (swish io)
 (swish mat)
 (swish osi)
 (swish script-testing)
 (swish testing)
 )
(define (make-byte-stream prime)
  (let ([next 0])
    (lambda ()
      (set! next (modulo (+ next prime) 256))
      next)))

(define (build-buffer size stream)
  (let ([bv (make-bytevector size)])
    (do ([i 0 (+ i 1)]) ((= i size))
      (bytevector-u8-set! bv i (stream)))
    bv))

(define (start-watchdog timeout reason)
  (spawn&link
   (lambda ()
     (receive (after timeout (raise reason))))))

(define (stop-watchdog x) (unlink x))

(define (get-bv ip len)
  (let* ([me self]
         [reader (spawn&link
                  (lambda ()
                    (let ([x (get-bytevector-n ip len)])
                      (send me `#(read ,self ,x)))))])
    (receive [#(read ,@reader ,x) x])))

(define (writer op)
  (receive
   [flush
    (flush-output-port op)
    (writer op)]
   [#(put-bv ,data ,flush?)
    (put-bytevector op data)
    (when flush? (flush-output-port op))
    (writer op)]))

(define (put-bv who data flush?)
  (send who `#(put-bv ,data ,flush?)))

(define (send/expect ip op data timeout)
  (let ([dog (start-watchdog timeout
               `#(send/expect-timeout ,timeout ,(bytevector-length data)))])
    (on-exit (stop-watchdog dog)
      (let ([w (spawn&link (lambda () (writer op)))])
        (put-bv w data #t)
        (let ([new (get-bv ip (bytevector-length data))])
          (assert (equal? new data)))))))

(define primes '(7 11 13 239 241 251))
(define buffer-sizes '(1 127 256 1024 2048 4096 8192))

(define (check-loopback ip op)
  (for-each
   (lambda (prime)
     (for-each
      (lambda (size)
        (send/expect ip op (build-buffer size (make-byte-stream prime)) 4000))
      buffer-sizes))
   primes))

(define (make-char-stream prime)
  (let ([next 0])
    (lambda ()
      (let lp ([n next])
        (let ([candidate (modulo (+ n prime) #x110000)])
          (if (or (<= candidate #xD7FF) (<= #xE000 candidate))
              (set! next candidate)
              (lp candidate))))
      (integer->char next))))

(isolate-mat tcp ()
  (define pid self)
  (define (run address hostname)
    (let* ([listener (listen-tcp address 0 self)]
           [test-port (listener-port-number listener)])
      (on-exit (close-tcp-listener listener)
        (let-values ([(cip cop) (connect-tcp hostname test-port)])
          (on-exit
           (begin (close-input-port cip) (force-close-output-port cop))
           (receive (after 5000 (raise 'timeout-connecting-tcp))
             [#(accept-tcp ,@listener ,sip ,sop)
              (on-exit (force-close-output-port sop)
                (check-loopback sip cop)
                (check-loopback cip sop))]
             [,(msg <= #(accept-tcp-failed ,_ ,_ ,_)) (raise msg)]))))))
  (run "0.0.0.0" "127.0.0.1")
  (run "127.0.0.1" "127.0.0.1")
  (run "::1" "::1")
  (run "::" "127.0.0.1")
  (run "::" "::1")
  ;; Test the listener guardian
  (let ([g (make-guardian)])
    (g (listen-tcp "::" 0 self))
    (gc)
    (assert (handle-gone? (g)))))

(isolate-mat tcp-bad ()
  (define pid self)
  (define (run hostname)
    (match-let*
     ([#(EXIT #(bad-arg listen-tcp #f)) (catch (listen-tcp #f 0 self))]
      [#(EXIT #(bad-arg listen-tcp #f)) (catch (listen-tcp "::" #f self))]
      [#(EXIT #(listen-tcp-failed ,_ 0 uv_ip4_addr ,_))
       (catch (listen-tcp "0.0" 0 self))]
      [#(EXIT #(listen-tcp-failed ,_ 0 uv_ip6_addr ,_))
       (catch (listen-tcp ":1:2" 0 self))]
      [#(EXIT #(bad-arg close-tcp-listener #f)) (catch (close-tcp-listener #f))]
      [#(EXIT #(bad-arg connect-tcp #f)) (catch (connect-tcp #f 0))]
      [#(EXIT #(bad-arg connect-tcp #f)) (catch (connect-tcp "" #f))]
      [#(EXIT #(io-error "[]:*nope*" uv_getaddrinfo ,_)) (catch (connect-tcp "" "*nope*"))])
     'ok)
    (let* ([listener (listen-tcp "::" 0 self)]
           [test-port (listener-port-number listener)])
      (on-exit (close-tcp-listener listener)
        (let ([op (open-output-string)]
              [l2 (listen-tcp "::" 0 self)])
          (on-exit (close-tcp-listener l2)
            (match-let* ([2 (tcp-listener-count)])
              (print-tcp-listeners op))))
        (match (catch (listen-tcp "::" test-port self))
          [#(EXIT #(listen-tcp-failed "::" ,@test-port uv_listen ,_)) 'ok])
        (receive (after 10 (close-tcp-listener listener)))
        (match (catch (connect-tcp hostname test-port))
          [#(EXIT #(io-error ,_ uv_tcp_connect ,_))
           'ok]))))
  (run "127.0.0.1")
  (run "::1"))

(isolate-mat process-bad ()
  (match-let*
   ([#(EXIT #(io-error "*!*" uv_spawn ,_))
     (catch (spawn-os-process "*!*" '() self))]
    [(osi_spawn . ,_)
     (osi_spawn* "*" '(0) (lambda (os-pid exit-status term-signal) 'ok))]
    [(osi_spawn . ,_)
     (osi_spawn* "*" '("x" . 0) (lambda (os-pid exit-status term-signal) 'ok))])
   'ok))

(isolate-mat files ()
  (define (make-buffers)
    (fold-left
     (lambda (acc prime)
       (fold-left
        (lambda (acc size)
          (cons (build-buffer size (make-byte-stream prime)) acc))
        acc
        primes))
     '()
     buffer-sizes))
  (define (watch-callbacks renames updates)
    ;; macOS does not send update events consistently for new files.
    (let lp ([renames-tbd renames]
             [updates-tbd (if (memq (machine-type) '(a6osx ta6osx))
                              '()
                              updates)]
             [updates-seen (if (memq (machine-type) '(a6osx ta6osx))
                               updates
                               '())])
      (receive
       (after 100
         (unless (and (null? renames-tbd) (null? updates-tbd))
           (raise `#(path-changed-missing ,renames-tbd ,updates-tbd))))
       [#(path-changed ,@test-dir ,filename 1)
        (if (member filename renames-tbd)
            (lp (remove filename renames-tbd) updates-tbd updates-seen)
            (raise `#(unexpected-path-rename ,filename ,renames)))]
       [#(path-changed ,@test-dir ,filename 2)
        (cond
         [(member filename updates-tbd)
          (lp renames-tbd (remove filename updates-tbd)
            (cons filename updates-seen))]
         [(member filename updates-seen)
          (lp renames-tbd updates-tbd updates-seen)]
         [else
          (raise `#(unexpected-path-updated ,filename ,updates))])])))
  (define bullet "\x2022;")
  (define test-dir (string-append bullet "files-test" bullet "/"))
  (match-let*
   ([#(EXIT #(bad-arg open-file bad-type))
     (catch (open-file "foo.bar" 0 0 'bad-type))]
    [#(EXIT #(bad-arg close-path-watcher #f))
     (catch (close-path-watcher #f))]
    [#(EXIT #(io-error "*" uv_fs_realpath ,_))
     (catch (get-real-path "*"))]
    [#(EXIT #(io-error "*" osi_list_directory ,_))
     (catch (list-directory "*"))]
    [#(EXIT #(io-error "*" uv_fs_event_start ,_))
     (catch (watch-path "*" self))]
    [#(EXIT #(io-error "*" osi_unlink ,_))
     (catch (remove-file "*"))]
    [#(EXIT #(io-error "*" osi_remove_directory ,_))
     (catch (remove-directory "*"))]
    [#(EXIT #(io-error "*" osi_chmod ,_))
     (catch (set-file-mode "*" #o777))]
    [,x (guard (and (integer? x) (< x 0))) (get-stat "*" #f)]
    [,uv-handles
     (guard (for-all
             (lambda (x)
               (and (pair? x) (integer? (car x)) (integer? (cdr x))))
             uv-handles))
     (osi_list_uv_handles)]
    [,t (guard (integer? t)) (osi_get_hrtime)]
    ["end of file" (osi_get_error_text UV_EOF)]
    ["SQLITE_ERROR" (osi_get_error_text -6000001)]
    ["Error code 0" (osi_get_error_text 0)]
    [#t (directory? (osi_get_temp_directory))])
   'ok)
  (delete-tree test-dir)
  (make-directory-path test-dir)
  (on-exit (delete-tree test-dir)
    (let ([watcher (watch-path test-dir self)]
          [fn (path-combine test-dir "basic")]
          [fn2 (path-combine test-dir "moved")])
      (define (write-file fn)
        (let ([op (open-file fn (+ O_WRONLY O_CREAT O_TRUNC) #o777
                    'binary-output)])
          (on-exit (force-close-output-port op)
            (put-bytevector op (build-buffer 4096 (make-byte-stream 7)))
            (assert (= (file-position op) 4096))
            (file-position op 0)
            (assert (= (file-position op) 0))
            (put-bytevector op (build-buffer 4096 (make-byte-stream 7))))))
      (let ([op (open-output-string)]
            [w2 (watch-path test-dir (spawn void))])
        (on-exit (close-path-watcher w2)
          (match-let* ([2 (path-watcher-count)])
            (print-path-watchers op))))
      (write-file fn)
      (watch-callbacks '("basic") '("basic"))
      (rename-path fn fn2)
      (assert (not (file-exists? fn)))
      (assert (file-exists? fn2))
      (watch-callbacks '("basic" "moved") '())
      (write-file fn)
      (watch-callbacks '("basic") '("basic"))
      (delete-file fn2)
      (assert (not (file-exists? fn2)))
      (watch-callbacks '("moved") '())
      (delete-file fn)
      (assert (not (file-exists? fn)))
      (watch-callbacks '("basic") '())
      (close-path-watcher watcher))

    (let* ([buffers (make-buffers)]
           [filenames
            (map (lambda (bv) (uuid->string (osi_make_uuid))) buffers)])
      (for-each
       (lambda (data fn)
         (let ([op (open-file (path-combine test-dir fn)
                     (+ O_WRONLY O_CREAT O_TRUNC) #o777 'binary-output)])
           (on-exit (force-close-output-port op)
             (put-bytevector op data))))
       buffers filenames)
      (for-each
       (lambda (data fn)
         (let ([ip (open-file (path-combine test-dir fn)
                     O_RDONLY 0 'binary-input)])
           (on-exit (close-input-port ip)
             (let ([new (get-bytevector-all ip)])
               (assert (equal? new data))))))
       buffers filenames)
      (let ([op (open-output-string)]
            [ports (map (lambda (fn)
                          (open-file (path-combine test-dir fn)
                            O_RDONLY 0 'binary-input)) filenames)])
        (on-exit (for-each close-input-port ports)
          (assert (>= (osi-port-count) (length filenames)))
          (print-osi-ports op)))
      ;; Look for files on disk, if they are one of ours, delete
      ;; it. This will clear out the directory for cleanup.
      (assert
       (null?
        (fold-left
         (lambda (filenames file)
           (cond
            [(find (lambda (x) (string=? x file)) filenames)
             (remove-file (path-combine test-dir file))
             (remove file filenames)]
            [else filenames]))
         filenames
         (directory-list test-dir)))))
    ;; Test the osi-port guardian
    (let ([fn (path-combine test-dir "guardian")]
          [g (make-guardian)])
      (g (open-file-port fn (+ O_WRONLY O_CREAT O_TRUNC) #o777))
      (gc)
      (on-exit (delete-file fn)
        (assert (handle-gone? (g)))))
    ;; Test the path-watcher guardian
    (let ([g (make-guardian)])
      (g (watch-path test-dir self))
      (gc)
      (assert (handle-gone? (g))))))

(isolate-mat read ()
  (read-bytevector "src/swish/io.ms" (read-file "src/swish/io.ms")))

(isolate-mat unicode ()
  (define fn (gensym->unique-string (gensym)))
  (define prime 32783)
  (match (catch (open-file-to-read fn))
    [#(EXIT #(io-error ,@fn uv_fs_open ,_)) 'ok])
  (let ([stream (make-char-stream prime)])
    (let ([op (open-file-to-write fn)])
      (on-exit (force-close-output-port op)
        (do ([n #x88000 (- n 1)]) ((= n 0))
          (write-char (stream) op))))
    (match (catch (open-file-to-write fn))
      [#(EXIT #(io-error ,@fn ,_ ,_)) 'ok])
    (let ([op (open-file-to-append fn)])
      (on-exit (force-close-output-port op)
        (do ([n #x88000 (- n 1)]) ((= n 0))
          (write-char (stream) op)))))
  (let ([stream (make-char-stream prime)]
        [ip (open-file-to-read fn)])
    (on-exit (close-input-port ip)
      (do ([n #x110000 (- n 1)]) ((= n 0))
        (let ([c1 (read-char ip)] [c2 (stream)])
          (unless (eqv? c1 c2)
            (raise `#(wrong-char ,n ,(char->integer c1) ,(char->integer c2))))))))
  (let ([op (open-file-to-replace fn)])
    (on-exit (force-close-output-port op)
      (display "nothing here" op)))
  (let ([ip (open-file fn O_RDONLY 0 'binary-input)])
    (on-exit (close-input-port ip)
      (match (get-string-all (open-utf8-bytevector (get-bytevector-all ip)))
        ["nothing here" 'ok])))
  (remove-file fn))

(isolate-mat binary ()
  (define fn (gensym->unique-string (gensym)))
  (define prime 32783)
  (define nbytes (expt 2 20))
  (define bytes
    (with-interrupts-disabled
     (parameterize ([random-seed prime])
       (let ([bv (make-bytevector nbytes)])
         (do ([i 0 (fx+ i 1)]) ((fx= i nbytes))
           (bytevector-u8-set! bv i (random 256)))
         bv))))
  (match (catch (open-binary-file-to-read fn))
    [#(EXIT #(io-error ,@fn uv_fs_open ,_)) 'ok])
  ;; write and append
  (begin
    (let ([op (open-binary-file-to-write fn)])
      (on-exit (force-close-output-port op)
        (do ([i 0 (fx+ i 1)]) ((fx= i nbytes))
          (put-u8 op (bytevector-u8-ref bytes i)))))
    (match (catch (open-binary-file-to-write fn))
      [#(EXIT #(io-error ,@fn ,_ ,_)) 'ok])
    (let ([op (open-binary-file-to-append fn)])
      (on-exit (force-close-output-port op)
        (do ([i (fx1- nbytes) (fx1- i)]) ((fx< i 0))
          (put-u8 op (bytevector-u8-ref bytes i))))))
  ;; read
  (let ([ip (open-binary-file-to-read fn)])
    (define (check i)
      (let ([b1 (get-u8 ip)] [b2 (bytevector-u8-ref bytes i)])
        (unless (fx= b1 b2)
          (raise `#(wrong-byte ,i ,b1 ,b2)))))
    (on-exit (close-input-port ip)
      (do ([i 0 (fx+ i 1)]) ((fx= i nbytes))
        (check i))
      (do ([i (fx1- nbytes) (fx1- i)]) ((fx< i 0))
        (check i))
      (match (get-u8 ip)
        [#!eof 'ok])))
  ;; replace
  (let ([op (open-binary-file-to-replace fn)])
    (define message "verify truncate on replace")
    (on-exit (force-close-output-port op)
      (put-bytevector op (string->utf8 message)))
    (let ([ip (open-file fn O_RDONLY 0 'binary-input)])
      (on-exit (close-input-port ip)
        (match (get-string-all (open-utf8-bytevector (get-bytevector-all ip)))
          [,@message 'ok]))))
  (remove-file fn))

(isolate-mat get-source-offset ()
  (define (gso expected-offset string)
    (match-let*
     ([,p (open-bytevector-input-port (string->utf8 string))]
      [,initial-offset 0]
      [,@initial-offset (file-position p)]
      [,@expected-offset (get-source-offset p)]
      [,@expected-offset (file-position p)])
     'ok))
  (gso 0 "")
  (gso 0 "no newline")
  (gso 6 "#! foo")
  (gso 7 (string-append "#! foo" "\n" "more"))
  (gso 19 (string-append "#!/usr/bin/env foo" "\n" "more"))
  (gso 0 "#/nope")
  (gso 0 "#!r6rs")
  (gso 0 "!bang"))

(isolate-mat foreign-handle ()
  (define-syntax define-logged
    (syntax-rules ()
      [(_ logger [(f pat ...) body ...] ...)
       (begin
         (define logger
           ;; log textual representation to avoid holding pointers to arguments
           (let ([op (open-output-string)])
             (case-lambda
              [() (split (get-output-string op) #\newline)]
              [(x) (write x op) (newline op) x])))
         (define (f . args)
           (match (logger `(f . ,args))
             [(f pat ...) body ...]))
         ...)]))
  (define-logged logger
    [(make-fake ,info ,create-time ,handle)
     `#(fake ,info ,create-time ,handle)]
    [(fake-info #(fake ,info ,_ ,_))
     info]
    [(fake-create-time #(fake ,_ ,create-time ,_))
     create-time]
    [(fake-handle #(fake ,_ ,_ ,handle))
     handle]
    [(fake-handle-set! ,[x <= #(fake ,_ ,_ ,_)] ,val)
     (vector-set! x 3 val)]
    [(close-fake ,[x <= #(fake ,_ ,_ ,handle)])
     (when handle (fakes x #f))]
    [(print-fake ,op ,[x <= #(fake ,_ ,_ ,handle)] ,@handle)
     (let* ([info (fake-info x)]
            [time (fake-create-time x)])
       (fprintf op "  ~d: ~a created ~d\n" handle info time))])
  (define fakes-id (gensym))
  (define fakes
    (make-foreign-handle-guardian fakes-id
      fake-handle
      fake-handle-set!
      fake-create-time
      close-fake
      print-fake))
  (match-let*
    ([,fake-count (foreign-handle-count fakes-id)]
    [,print-fakes (foreign-handle-print fakes-id)]
    [,os (open-output-string)]
    [0 (fake-count)]
    ["" (begin (print-fakes os) (get-output-string os))]
    [,build-fake
     (lambda (info create-time handle)
       (let ([r (make-fake info create-time handle)])
         (fakes r handle)))]
    [,x1 (build-fake 'info1 3 7000)]
    [,x2 (build-fake 'info2 1 8000)]
    [,x3 (build-fake 'info3 2 9000)]
    [,x4 (build-fake 'info4 2 6000)]
    ;; foreign-guardian made expected calls
    [("(make-fake info1 3 7000)"
      "(fake-handle #(fake info1 3 7000))"
      "(make-fake info2 1 8000)"
      "(fake-handle #(fake info2 1 8000))"
      "(make-fake info3 2 9000)"
      "(fake-handle #(fake info3 2 9000))"
      "(make-fake info4 2 6000)"
      "(fake-handle #(fake info4 2 6000))" "")
     (logger)]
    ;; count works
    [4 (fake-count)]
    ;; print sorts by creation time, then by handle
    [("  8000: info2 created 1"
      "  6000: info4 created 2"
      "  9000: info3 created 2"
      "  7000: info1 created 3"
      "")
     (begin
       (print-fakes os)
       (split (get-output-string os) #\newline))]
    ;; finalize on collection
    [("(close-fake #(fake info3 2 9000))"
      "(fake-handle-set! #(fake info3 2 9000) #f)"
      "")
     (begin
       (logger)
       (set! x3 #f) ;; drop reference
       (gc)
       (assert (andmap vector? (list x1 x2 x4))) ;; live
       (logger))]
    [3 (fake-count)]
    ;; test drop and restore of handles as happens in sqlite:close if osi_close_database* fails
    [#(fake ,_ ,_ ,x1-handle) x1]
    [#(fake ,_ ,_ ,x4-handle) x4]
    ;; drop handle on x4 ...
    [2 (begin (fakes x4 #f) (fake-count))]
    ;; drop handle on x1 ...
    [1 (begin (fakes x1 #f) (fake-count))]
    ;; ... revive handle on x4
    [2 (begin (fakes x4 x4-handle) (fake-count))]
    ;; ... revive handle on x1
    [3 (begin (fakes x1 x1-handle) (fake-count))]
    [("(fake-handle-set! #(fake info4 2 6000) #f)"
      "(fake-handle-set! #(fake info1 3 7000) #f)"
      "(fake-handle #(fake info4 2 #f))"
      "(fake-handle-set! #(fake info4 2 #f) 6000)"
      "(fake-handle #(fake info1 3 #f))"
      "(fake-handle-set! #(fake info1 3 #f) 7000)"
      "")
     (logger)]
    [(,@fakes-id . 3)
     (let ([pr (cons #f #f)])
       (count-foreign-handles pr
         (lambda (pr type count)
           (when (eq? type fakes-id)
             (set-car! pr type)
             (set-cdr! pr count))))
       pr)]
    [("" "osi-ports:" . ,_)
     (begin
       (parameterize ([current-output-port os])
         (print-foreign-handles))
       (match-regexps
        ;; gensym g<nnn> will show up between databases and osi-ports
        `("" "databases:" "" seek
          ,(format "~a:" fakes-id)
          "8000: info2 created 1"
          "  6000: info4 created 2"
          "  7000: info1 created 3")
        (split (get-output-string os) #\newline)))]
    ;; finalize the rest
    [(""
      "(close-fake #(fake info1 3 7000))"
      "(close-fake #(fake info2 1 8000))"
      "(close-fake #(fake info4 2 6000))"
      "(fake-handle-set! #(fake info1 3 7000) #f)"
      "(fake-handle-set! #(fake info2 1 8000) #f)"
      "(fake-handle-set! #(fake info4 2 6000) #f)")
     (begin (logger) (gc) (sort string<? (logger)))]
    [0 (fake-count)]
    ["" (begin (print-fakes os) (get-output-string os))]
    ["" (parameterize ([current-output-port os]) (print-fakes) (get-output-string os))])
   'ok)
  (match-let*
   ([#(EXIT #(type-already-registered osi-ports))
     (catch (make-foreign-handle-guardian 'osi-ports values values values values values))]
    [#(EXIT #(bad-arg make-foreign-handle-guardian "foo"))
     (catch (make-foreign-handle-guardian "foo" values values values values values))]
    [#(EXIT #(bad-arg make-foreign-handle-guardian "bar"))
     (catch (make-foreign-handle-guardian 'foo "bar" values values values values))]
    [#(EXIT #(bad-arg make-foreign-handle-guardian "baz"))
     (catch (make-foreign-handle-guardian 'foo values "baz" values values values))]
    [#(EXIT #(bad-arg make-foreign-handle-guardian "cod"))
     (catch (make-foreign-handle-guardian 'foo values values "cod" values values))]
    [#(EXIT #(bad-arg make-foreign-handle-guardian "dog"))
     (catch (make-foreign-handle-guardian 'foo values values values "dog" values))]
    [#(EXIT #(bad-arg make-foreign-handle-guardian "eel"))
     (catch (make-foreign-handle-guardian 'foo values values values values "eel"))]
    [#(EXIT #(bad-arg foreign-handle-count dracula))
     (catch (foreign-handle-count 'dracula))]
    [#(EXIT #(bad-arg foreign-handle-print radix))
     (catch (foreign-handle-print 'radix))]
    )
   'ok)
  )

(isolate-mat signal-handler ()
  (define (extract-signum s)
    (define pat (re "^  [0-9]+: for signal ([0-9]+) registered [0-9]+"))
    (match-define (,_ ,sig) (pregexp-match pat s))
    (string->number sig))
  (define (->signals output)
    (sort <
      (map extract-signum
        (remq "" (split output #\newline)))))
  (match-let*
   ([#(EXIT #(bad-arg signal-handler sym)) (catch (signal-handler 'sym))]
    [#(EXIT #(bad-arg signal-handler "string")) (catch (signal-handler "string" #f))]
    [#(EXIT #(bad-arg signal-handler 123)) (catch (signal-handler SIGWINCH 123))]
    [#(EXIT #(io-error 1234 uv_signal_start ,_)) (catch (signal-handler 1234 pretty-print))]
    [,handle-it (lambda (n) 'ignored)]
    [ok (begin (signal-handler SIGWINCH handle-it) 'ok)]
    [,@handle-it (signal-handler SIGWINCH)]
    [,trapped-signals ;; in app-core.ss
      (sort <
        (cons SIGWINCH
          (meta-cond
           [(memq (machine-type) '(i3nt ti3nt a6nt ta6nt))
            (list SIGBREAK SIGHUP SIGINT)]
           [else
            (list SIGINT SIGTERM)])))]
    [ok (and (andmap procedure? (map signal-handler trapped-signals)) 'ok)]
    [,@trapped-signals
      (let ([os (open-output-string)])
        (print-signal-handlers os)
        (->signals (get-output-string os)))]
    [,@trapped-signals
     (let ([os (open-output-string)])
       (parameterize ([current-output-port os])
         (print-signal-handlers))
       (->signals (get-output-string os)))]
    [,expected (length trapped-signals)]
    [,@expected (signal-handler-count)]
    [ok (begin (signal-handler SIGWINCH #f) 'ok)]
    [#f (signal-handler SIGWINCH)]
    [,expected (- expected 1)]
    [,@expected (signal-handler-count)])
   'ok)
  (meta-cond
   [(memq (machine-type) '(i3nt ti3nt a6nt ta6nt))
    ;; skip signal-handling tests on Windows
    (void)]
   [else
    (script-test #f '("--")
      (format "~{~s\n~}"
        `((define (ignore signum)
            ;; Bad example: should not do I/O in a signal handler.
            ;; We get away with writing to the default output port.
            (printf "parent ignoring ~s\n" signum))
          (define parent-pid (osi_get_pid))
          (signal-handler SIGCHLD ignore)
          (signal-handler SIGINT ignore)
          (let-values ([(to-stdin from-stdout from-stderr os-pid)
                        (spawn-os-process (osi_get_executable_path) '("-q" "--") self)])
            (let ([to-stdin (binary->utf8 to-stdin)]
                  [from-stdout (binary->utf8 from-stdout)]
                  [from-stderr (binary->utf8 from-stderr)])
              (define (dump p)
                (let ([line (get-line p)])
                  (unless (eof-object? line)
                    (printf "~a\n" line)
                    (dump p))))
              ;; parent re-hooks SIGINT
              (signal-handler SIGINT
                (let ([orig (signal-handler SIGINT)])
                  (lambda (x)
                    (orig x)
                    (printf "parent terminating child via ~s\n" SIGTERM)
                    (osi_kill os-pid SIGTERM))))
              (for-each
               (lambda (x)
                 (pretty-print x to-stdin))
               `((signal-handler SIGTERM
                   (lambda (x)
                     (printf "child caught ~s\n" x)
                     (signal-handler x #f)
                     (printf "child unregistered signal handler and re-raised ~s\n" x)
                     (osi_kill (osi_get_pid) x)))
                 (printf "child killing parent with ~s\n" SIGINT)
                 (begin
                   (osi_kill ,parent-pid SIGINT)
                   (receive))))
              (flush-output-port to-stdin)
              (receive (after 2000 (raise 'fail))
                [#(process-terminated ,',@os-pid ,',exit-status ,',@SIGTERM)
                 'ok])
              (printf "child-output:\n")
              (dump from-stdout)
              (dump from-stderr)
              (printf "done\n")
              (exit)))))
      `(,(format "parent ignoring ~s" SIGINT)
        ,(format "parent terminating child via ~s" SIGTERM)
        ,(format "parent ignoring ~s" SIGCHLD)
        "child-output:"
        ,(format "child killing parent with ~s" SIGINT)
        ,(format "child caught ~s" SIGTERM)
        ,(format "child unregistered signal handler and re-raised ~s" SIGTERM)
        "done"
        ))])
  )

(hook-console-input)
