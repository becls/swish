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
(library (swish io)
  (export
   <stat>
   binary->utf8
   close-osi-port
   close-path-watcher
   close-tcp-listener
   connect-tcp
   directory?
   force-close-output-port
   get-file-size
   get-stat
   hook-console-input
   io-error
   list-directory
   listen-tcp
   listener-port-number
   listener?
   make-directory
   make-directory-path
   make-utf8-transcoder
   open-file
   open-file-port
   open-file-to-append
   open-file-to-read
   open-file-to-replace
   open-file-to-write
   path-combine
   path-watcher-path
   path-watcher?
   read-bytevector
   read-file
   read-osi-port
   regular-file?
   remove-directory
   remove-file
   rename-path
   set-file-mode
   stat-directory?
   stat-regular-file?
   watch-path
   write-osi-port
   )
  (import
   (swish erlang)
   (swish osi)
   (except (chezscheme) define-record exit))

  ;; Procedures starting with @ must be called with interrupts disabled.

  (define-record <stat>
    dev
    mode
    nlink
    uid
    gid
    rdev
    ino
    size
    blksize
    blocks
    flags
    gen
    atime
    mtime
    ctime
    birthtime)

  (define-record-type osi-port
    (nongenerative)
    (fields
     (immutable name)
     (mutable handle)))

  (define (read-osi-port port bv start n fp)
    (define result)
    (with-interrupts-disabled
     (match (osi_read_port* (osi-port-handle port) bv start n fp
              (let ([p self])
                (lambda (r)
                  (#%$keep-live port)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io (osi-port-name port))
        (cond
         [(>= result 0) result]
         [(eqv? result UV_EOF) 0]
         [else (io-error (osi-port-name port) 'osi_read_port result)])]
       [(,who . ,errno) (io-error (osi-port-name port) who errno)])))

  (define (write-osi-port port bv start n fp)
    (define result)
    (with-interrupts-disabled
     (match (osi_write_port* (osi-port-handle port) bv start n fp
              (let ([p self])
                (lambda (r)
                  (#%$keep-live port)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io (osi-port-name port))
        (if (>= result 0)
            result
            (io-error (osi-port-name port) 'osi_write_port result))]
       [(,who . ,errno) (io-error (osi-port-name port) who errno)])))

  (define (close-osi-port port)
    (with-interrupts-disabled
     (let ([handle (osi-port-handle port)])
       (when handle
         (match (osi_close_port* handle
                  (let ([p self])
                    (lambda (result) ;; ignore failures
                      (#%$keep-live port)
                      (complete-io p))))
           [#t
            (osi-port-handle-set! port #f)
            (wait-for-io (osi-port-name port))]
           [(,who . ,errno) (io-error (osi-port-name port) who errno)])))))

  (define (force-close-output-port op)
    (unless (port-closed? op)
      (match (catch (close-output-port op))
        [#(EXIT #(io-error ,_ osi_write_port ,_))
         (clear-output-port op)
         (close-output-port op)]
        [#(EXIT ,reason) (exit reason)]
        [,_ (void)])))

  (define (io-error name who errno)
    (exit `#(io-error ,name ,who ,errno)))

  (define (make-r! port)
    (lambda (bv start n)
      (read-osi-port port bv start n -1)))

  (define (make-w! port)
    (lambda (bv start n)
      (write-osi-port port bv start n -1)))

  (define (make-close port)
    (lambda ()
      (close-osi-port port)))

  (define (make-utf8-transcoder)
    (make-transcoder (utf-8-codec)
      (eol-style none)
      (error-handling-mode raise)))

  (define (binary->utf8 bp)
    (transcoded-port bp (make-utf8-transcoder)))

  (define (make-iport name port close?)
    (make-custom-binary-input-port name (make-r! port) #f #f
      (and close? (make-close port))))

  (define (make-oport name port)
    (make-custom-binary-output-port name (make-w! port) #f #f
      (make-close port)))

  (define (read-bytevector name contents)
    (let* ([ip (open-bytevector-input-port contents)]
           [sfd (make-source-file-descriptor name ip #t)]
           [ip (transcoded-port ip (make-utf8-transcoder))])
      (let f ([offset 0])
        (let-values ([(x offset) (get-datum/annotations ip sfd offset)])
          (if (eof-object? x)
              '()
              (cons x (f offset)))))))

  (define osi-port-guardian (make-guardian))

  (define (@make-osi-port name handle)
    (let ([port (make-osi-port name handle)])
      (osi-port-guardian port)
      port))

  (define (close-dead-osi-ports)
    ;; This procedure runs in the finalizer process.
    (let ([port (osi-port-guardian)])
      (when port
        (close-osi-port port)
        (close-dead-osi-ports))))

  ;; Path watching

  (define-record-type path-watcher
    (nongenerative)
    (fields
     (mutable handle)
     (immutable path)))

  (define path-watcher-guardian (make-guardian))

  (define (close-dead-path-watchers)
    ;; This procedure runs in the finalizer process.
    (let ([w (path-watcher-guardian)])
      (when w
        (close-path-watcher w)
        (close-dead-path-watchers))))

  (define (close-path-watcher watcher)
    (unless (path-watcher? watcher)
      (bad-arg 'close-path-watcher watcher))
    (with-interrupts-disabled
     (let ([handle (path-watcher-handle watcher)])
       (when handle
         (osi_close_path_watcher handle)
         (path-watcher-handle-set! watcher #f)))))

  (define (watch-path path process)
    (unless (string? path)
      (bad-arg 'watch-path path))
    (unless (process? process)
      (bad-arg 'watch-path process))
    (with-interrupts-disabled
     (match (osi_watch_path* path
              (case-lambda
               [(filename events)
                (send process
                  `#(path-changed ,path ,filename ,events))]
               [(errno)
                (send process
                  `#(path-watcher-failed ,path ,errno))]))
       [(,who . ,errno) (exit `#(watch-path-failed ,path ,who ,errno))]
       [,handle
        (let ([w (make-path-watcher handle path)])
          (path-watcher-guardian w)
          w)])))

  ;; Console Ports

  (define (make-console-input)
    ;; no need to register (osi_stdin) with guardian
    (define name "stdin")
    (binary->utf8 (make-iport name (make-osi-port name (osi_stdin)) #f)))

  (define hook-console-input
    (let ([hooked? #f])
      (lambda ()
        (unless hooked?
          (let ([ip (make-console-input)])
            ;; Chez Scheme uses $console-input-port to do smart
            ;; flushing of console I/O.
            (set! hooked? #t)
            (#%$set-top-level-value! '$console-input-port ip)
            (console-input-port ip)
            (current-input-port ip))))))

  ;; File System

  (define path-combine
    (case-lambda
     [(x y)
      (let ([n (string-length x)])
        (cond
         [(eqv? n 0) y]
         [(directory-separator? (string-ref x (fx- n 1)))
          (string-append x y)]
         [else (format "~a~c~a" x (directory-separator) y)]))]
     [(x) x]
     [(x y . rest) (apply path-combine (path-combine x y) rest)]))

  (define make-directory
    (case-lambda
     [(path mode)
      (define result)
      (with-interrupts-disabled
       (match (osi_make_directory* path mode
                (let ([p self])
                  (lambda (r)
                    (set! result r)
                    (complete-io p))))
         [#t
          (wait-for-io path)
          (when (< result 0)
            (io-error path 'osi_make_directory result))]
         [(,who . ,errno) (io-error path who errno)]))]
     [(path) (make-directory path #o777)]))

  (define make-directory-path
    (case-lambda
     [(path mode)
      (let loop ([path path])
        (let ([dir (path-parent path)])
          (unless (or (string=? dir path) (string=? dir ""))
            (unless (directory? dir)
              (loop dir)
              (make-directory dir mode)))))
      path]
     [(path) (make-directory-path path #o777)]))

  (define (directory? path)
    (stat-directory? (get-stat path)))

  (define (stat-directory? x)
    (match x
      [`(<stat> ,mode) (eqv? (bitwise-and mode S_IFMT) S_IFDIR)]
      [,_ #f]))

  (define (regular-file? path)
    (stat-regular-file? (get-stat path)))

  (define (stat-regular-file? x)
    (match x
      [`(<stat> ,mode) (eqv? (bitwise-and mode S_IFMT) S_IFREG)]
      [,_ #f]))

  (define (remove-directory path)
    (define result)
    (with-interrupts-disabled
     (match (osi_remove_directory* path
              (let ([p self])
                (lambda (r)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io path)
        (when (< result 0)
          (io-error path 'osi_remove_directory result))]
       [(,who . ,errno) (io-error path who errno)])))

  (define (remove-file path)
    (define result)
    (with-interrupts-disabled
     (match (osi_unlink* path
              (let ([p self])
                (lambda (r)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io path)
        (when (< result 0)
          (io-error path 'osi_unlink result))]
       [(,who . ,errno) (io-error path who errno)])))

  (define (rename-path path new-path)
    (define result)
    (with-interrupts-disabled
     (match (osi_rename* path new-path
              (let ([p self])
                (lambda (r)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io path)
        (when (< result 0)
          (io-error path 'osi_rename result))]
       [(,who . ,errno) (io-error path who errno)])))

  (define (set-file-mode path mode)
    (define result)
    (with-interrupts-disabled
     (match (osi_chmod* path mode
              (let ([p self])
                (lambda (r)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io path)
        (when (< result 0)
          (io-error path 'osi_chmod result))]
       [(,who . ,errno) (io-error path who errno)])))

  (define (open-file-port name flags mode)
    (define result)
    (with-interrupts-disabled
     (match (osi_open_file* name flags mode
              (let ([p self])
                (lambda (r)
                  (if (pair? r)
                      (set! result r)
                      (set! result (@make-osi-port name r)))
                  (complete-io p))))
       [#t
        (wait-for-io name)
        (match result
          [(,who . ,errno) (io-error name who errno)]
          [,port port])]
       [(,who . ,errno) (io-error name who errno)])))

  (define (get-file-size port)
    (define result)
    (with-interrupts-disabled
     (match (osi_get_file_size* (osi-port-handle port)
              (let ([p self])
                (lambda (r)
                  (#%$keep-live port)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io (osi-port-name port))
        (match result
          [(,who . ,errno) (io-error (osi-port-name port) who errno)]
          [,size size])]
       [(,who . ,errno) (io-error (osi-port-name port) who errno)])))

  (define get-stat
    (case-lambda
     [(path follow?)
      (define result)
      (with-interrupts-disabled
       (match (osi_stat* path follow?
                (let ([p self])
                  (lambda (r)
                    (set! result r)
                    (complete-io p))))
         [#t
          (wait-for-io path)
          result]
         [(,who . ,errno) (io-error path who errno)]))]
     [(path) (get-stat path #t)]))

  (define (list-directory path)
    (define result)
    (with-interrupts-disabled
     (match (osi_list_directory* path
              (let ([p self])
                (lambda (r)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io path)
        (if (number? result)
            (io-error path 'osi_list_directory result)
            result)]
       [(,who . ,errno) (io-error path who errno)])))

  (define-syntax define-export
    (syntax-rules ()
      [(_ name e) (identifier? #'name) (begin (define name e) (export name))]))

  (include "io-constants.ss")

  (define (open-file name flags mode type)
    (unless (memq type '(binary-input binary-output input output))
      (bad-arg 'open-file type))
    (let ([port (open-file-port name flags mode)])
      (define fp 0)
      (define (r! bv start n)
        (let ([x (read-osi-port port bv start n fp)])
          (unless (eof-object? x)
            (set! fp (+ fp x)))
          x))
      (define (w! bv start n) (write-osi-port port bv start n -1))
      (define (gp) fp)
      (define (sp! pos) (set! fp pos))
      (define (close) (close-osi-port port))
      (case type
        [(binary-input)
         (make-custom-binary-input-port name r! gp sp! close)]
        [(binary-output)
         (make-custom-binary-output-port name w! #f #f close)]
        [(input)
         (binary->utf8
          (make-custom-binary-input-port name r! gp sp! close))]
        [(output)
         (binary->utf8
          (make-custom-binary-output-port name w! #f #f close))])))

  (define (open-file-to-read name)
    (open-file name O_RDONLY 0 'input))

  (define (open-file-to-write name)
    (open-file name (+ O_WRONLY O_CREAT O_EXCL) #o777 'output))

  (define (open-file-to-append name)
    (open-file name (+ O_WRONLY O_CREAT O_APPEND) #o777 'output))

  (define (open-file-to-replace name)
    (open-file name (+ O_WRONLY O_CREAT O_TRUNC) #o777 'output))

  (define (read-file name)
    (let ([port (open-file-port name O_RDONLY 0)])
      (on-exit (close-osi-port port)
        (let ([n (get-file-size port)])
          (if (> n 0)
              (let* ([bv (make-bytevector n)]
                     [count (read-osi-port port bv 0 n 0)])
                (unless (eqv? count n)
                  (exit `#(unexpected-eof ,name)))
                bv)
              #vu8())))))

  ;; TCP/IP Ports

  (define-record-type listener
    (nongenerative)
    (fields
     (mutable handle)
     (immutable port-number)))

  (define listener-guardian (make-guardian))

  (define (port-number? x) (and (fixnum? x) (fx<= 0 x 65535)))

  (define (close-dead-listeners)
    ;; This procedure runs in the finalizer process.
    (let ([l (listener-guardian)])
      (when l
        (close-tcp-listener l)
        (close-dead-listeners))))

  (define (listen-tcp port-number process)
    (unless (port-number? port-number)
      (bad-arg 'listen-tcp port-number))
    (unless (process? process)
      (bad-arg 'listen-tcp process))
    (with-interrupts-disabled
     (match (osi_listen_tcp* port-number
              (lambda (r)
                ;; This procedure runs in the event loop.
                (if (pair? r)
                    (send process
                      `#(accept-tcp-failed (car r) (cdr r)))
                    (let* ([name (osi_get_ip_address r)]
                           [port (@make-osi-port name r)])
                      (send process
                        `#(accept-tcp
                           ,(make-iport name port #f)
                           ,(make-oport name port)))))))
       [(,who . ,errno)
        (exit `#(listen-tcp-failed ,port-number ,who ,errno))]
       [,handle
        (let ([l (make-listener handle
                   (osi_get_tcp_listener_port handle))])
          (listener-guardian l)
          l)])))

  (define (close-tcp-listener listener)
    ;; This procedure may run in the finalizer process.
    (unless (listener? listener)
      (bad-arg 'close-tcp-listener listener))
    (with-interrupts-disabled
     (let ([handle (listener-handle listener)])
       (when handle
         (osi_close_tcp_listener handle)
         (listener-handle-set! listener #f)))))

  (define (connect-tcp hostname port-spec)
    (define result
      (begin
        (unless (string? hostname)
          (bad-arg 'connect-tcp hostname))
        (unless (or (port-number? port-spec) (string? port-spec))
          (bad-arg 'connect-tcp port-spec))
        (void)))
    (with-interrupts-disabled
     (match (osi_connect_tcp* hostname
              (if (string? port-spec) port-spec (number->string port-spec))
              (let ([p self])
                (lambda (r)
                  (if (pair? r)
                      (set! result r)
                      (set! result (@make-osi-port (osi_get_ip_address r) r)))
                  (complete-io p))))
       [#t
        (let ([name (format "[~a]:~a" hostname port-spec)])
          (wait-for-io name)
          (match result
            [(,who . ,errno) (io-error name who errno)]
            [,port
             (let ([name (osi-port-name port)])
               (values (make-iport name port #f) (make-oport name port)))]))])))

  (add-finalizer close-dead-osi-ports)
  (add-finalizer close-dead-path-watchers)
  (add-finalizer close-dead-listeners)
  )
