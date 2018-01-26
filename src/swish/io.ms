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
 (swish erlang)
 (swish mat)
 (swish osi)
 (swish io)
 (swish testing)
 (except (chezscheme) define-record exit sleep)
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
     (receive (after timeout (exit reason))))))

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

(define (delete-tree path)
  (if (file-directory? path)
      (or (delete-directory path)
          (begin
            (for-each (lambda (p) (delete-tree (path-combine path p)))
              (directory-list path))
            (receive (after 10 'ok))
            (delete-directory path)))
      (delete-file path)))

(isolate-mat tcp ()
  (define pid self)
  (define (run hostname)
    (let* ([listener (listen-tcp 0 self)]
           [test-port (listener-port-number listener)])
      (on-exit (close-tcp-listener listener)
        (let-values ([(cip cop) (connect-tcp hostname test-port)])
          (receive (after 5000 (exit 'timeout-connecting-tcp))
            [#(accept-tcp ,sip ,sop)
             (on-exit (force-close-output-port sop)
               (check-loopback sip cop)
               (check-loopback cip sop))]
            [,(msg <= #(accept-tcp-failed ,_ ,_)) (exit msg)])))))
  (run "127.0.0.1")
  (run "::1")
  ;; Test the listener guardian
  (let ([g (make-guardian)])
    (g (listen-tcp 0 self))
    (gc)
    (assert (handle-gone? (g)))))

(isolate-mat tcp-bad ()
  (define pid self)
  (define (run hostname)
    (match-let*
     ([#(EXIT #(bad-arg listen-tcp #f)) (catch (listen-tcp #f self))]
      [#(EXIT #(bad-arg close-tcp-listener #f)) (catch (close-tcp-listener #f))]
      [#(EXIT #(bad-arg connect-tcp #f)) (catch (connect-tcp #f 0))]
      [#(EXIT #(bad-arg connect-tcp #f)) (catch (connect-tcp "" #f))]
      [#(EXIT #(io-error "[]:*nope*" uv_getaddrinfo ,_)) (catch (connect-tcp "" "*nope*"))])
     'ok)
    (let* ([listener (listen-tcp 0 self)]
           [test-port (listener-port-number listener)])
      (on-exit (close-tcp-listener listener)
        (match (catch (listen-tcp test-port self))
          [#(EXIT #(listen-tcp-failed ,@test-port uv_listen ,_)) 'ok])
        (receive (after 10 (close-tcp-listener listener)))
        (match (catch (connect-tcp hostname test-port))
          [#(EXIT #(io-error ,_ uv_tcp_connect ,_))
           'ok]))))
  (run "127.0.0.1")
  (run "::1"))

(isolate-mat process-bad ()
  (match-let*
   ([#(EXIT #(io-error "*!*" uv_spawn ,_))
     (catch (spawn-os-process "*!*" '() self))])
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
             [updates-tbd (if (memq (machine-type) '(a6osx i3osx))
                              '()
                              updates)]
             [updates-seen (if (memq (machine-type) '(a6osx i3osx))
                               updates
                               '())])
      (receive
       (after 100
         (unless (and (null? renames-tbd) (null? updates-tbd))
           (exit `#(path-changed-missing ,renames-tbd ,updates-tbd))))
       [#(path-changed ,@test-dir ,filename 1)
        (if (member filename renames-tbd)
            (lp (remove filename renames-tbd) updates-tbd updates-seen)
            (exit `#(unexpected-path-rename ,filename ,renames)))]
       [#(path-changed ,@test-dir ,filename 2)
        (cond
         [(member filename updates-tbd)
          (lp renames-tbd (remove filename updates-tbd)
            (cons filename updates-seen))]
         [(member filename updates-seen)
          (lp renames-tbd updates-tbd updates-seen)]
         [else
          (exit `#(unexpected-path-updated ,filename ,updates))])])))
  (define bullet "\x2022;")
  (define test-dir (string-append bullet "files-test" bullet "/"))
  (match-let*
   ([#(EXIT #(bad-arg open-file bad-type))
     (catch (open-file "foo.bar" 0 0 'bad-type))]
    [#(EXIT #(bad-arg close-path-watcher #f))
     (catch (close-path-watcher #f))])
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
            (map (lambda (bv) (guid->string (osi_make_guid))) buffers)])
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
  (read-bytevector "swish/io.ms" (read-file "swish/io.ms")))

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
            (exit `#(wrong-char ,n ,(char->integer c1) ,(char->integer c2))))))))
  (let ([op (open-file-to-replace fn)])
    (on-exit (force-close-output-port op)
      (display "nothing here" op)))
  (let ([ip (open-file fn O_RDONLY 0 'binary-input)])
    (on-exit (close-input-port ip)
      (match (get-string-all (open-utf8-bytevector (get-bytevector-all ip)))
        ["nothing here" 'ok])))
  (remove-file fn))

(hook-console-input)
