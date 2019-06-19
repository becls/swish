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
(library (swish io)
  (export
   <stat>
   binary->utf8
   close-osi-port
   close-path-watcher
   close-tcp-listener
   connect-tcp
   count-foreign-handles
   directory?
   force-close-output-port
   foreign-handle-count
   foreign-handle-print
   get-datum/annotations-all
   get-file-size
   get-real-path
   get-source-offset
   get-stat
   hook-console-input
   io-error
   list-directory
   listen-tcp
   listener-address
   listener-create-time
   listener-port-number
   listener?
   make-directory
   make-directory-path
   make-foreign-handle-guardian
   make-osi-input-port
   make-osi-output-port
   make-utf8-transcoder
   open-binary-file-to-append
   open-binary-file-to-read
   open-binary-file-to-replace
   open-binary-file-to-write
   open-fd-port
   open-file
   open-file-port
   open-file-to-append
   open-file-to-read
   open-file-to-replace
   open-file-to-write
   open-utf8-bytevector
   osi-port-closed?
   osi-port-count
   osi-port-create-time
   osi-port-name
   osi-port?
   path-combine
   path-watcher-count
   path-watcher-create-time
   path-watcher-path
   path-watcher?
   print-foreign-handles
   print-osi-ports
   print-path-watchers
   print-signal-handlers
   print-tcp-listeners
   read-bytevector
   read-file
   read-osi-port
   regular-file?
   remove-directory
   remove-file
   rename-path
   set-file-mode
   signal-handler
   signal-handler-count
   spawn-os-process
   stat-directory?
   stat-regular-file?
   tcp-listener-count
   watch-path
   with-sfd-source-offset
   write-osi-port
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish osi)
   )

  ;; Procedures starting with @ must be called with interrupts disabled.

  (define-tuple <stat>
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
     (immutable create-time)
     (mutable handle)))

  (define (get-osi-port-handle who p)
    (unless (osi-port? p)
      (bad-arg who p))
    (or (osi-port-handle p)
        (raise `#(osi-port-closed ,who ,p))))

  (define (read-osi-port port bv start n fp)
    (define result)
    (with-interrupts-disabled
     (let retry ()
       (match (osi_read_port* (get-osi-port-handle 'read-osi-port port)
                bv start n fp
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
           [(eqv? result UV_EINTR) (retry)]
           [else (io-error (osi-port-name port) 'osi_read_port result)])]
         [(,who . ,errno) (io-error (osi-port-name port) who errno)]))))

  (define (write-osi-port port bv start n fp)
    (define result)
    (with-interrupts-disabled
     (match (osi_write_port* (get-osi-port-handle 'write-osi-port port)
              bv start n fp
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
    (unless (osi-port? port)
      (bad-arg 'close-osi-port port))
    (with-interrupts-disabled
     (let ([handle (osi-port-handle port)])
       (when handle
         (match (osi_close_port* handle
                  (let ([p self])
                    (lambda (result) ;; ignore failures
                      (#%$keep-live port)
                      (complete-io p))))
           [#t
            (osi-ports port #f)
            (wait-for-io (osi-port-name port))]
           [(,who . ,errno) (io-error (osi-port-name port) who errno)])))))

  (define (force-close-output-port op)
    (unless (port-closed? op)
      (match (catch (close-output-port op))
        [#(EXIT ,_)
         (clear-output-port op)
         (close-output-port op)]
        [,_ (void)])))

  (define (io-error name who errno)
    (raise `#(io-error ,name ,who ,errno)))

  (define (make-utf8-transcoder)
    (make-transcoder (utf-8-codec)
      (eol-style none)
      (error-handling-mode replace)))

  (define (binary->utf8 bp)
    (transcoded-port bp (make-utf8-transcoder)))

  (define (make-iport name port close?)
    (define fp 0)
    (define (r! bv start n)
      (let ([count (read-osi-port port bv start n -1)])
        (set! fp (+ fp count))
        count))
    (define (gp) fp)
    (make-custom-binary-input-port name r! gp #f
      (and close? (lambda () (close-osi-port port)))))

  (define (make-osi-input-port p)
    (unless (osi-port? p)
      (bad-arg 'make-osi-input-port p))
    (make-iport (osi-port-name p) p #t))

  (define (make-oport name port)
    (define fp 0)
    (define (w! bv start n)
      (let ([count (write-osi-port port bv start n -1)])
        (set! fp (+ fp count))
        count))
    (define (gp) fp)
    (define (close) (close-osi-port port))
    (make-custom-binary-output-port name w! gp #f close))

  (define (make-osi-output-port p)
    (unless (osi-port? p)
      (bad-arg 'make-osi-output-port p))
    (make-oport (osi-port-name p) p))

  (define (open-utf8-bytevector bv)
    (binary->utf8 (open-bytevector-input-port bv)))

  (define (with-sfd-source-offset filename handler)
    (let* ([raw-ip (open-file filename O_RDONLY 0 'binary-input)]
           [sfd (make-source-file-descriptor filename raw-ip #t)]
           [source-offset (get-source-offset raw-ip)]
           [ip (binary->utf8 raw-ip)])
      (on-exit (close-port ip)
        (handler ip sfd source-offset))))

  (define (get-datum/annotations-all ip sfd bfp)
    (let f ([bfp bfp])
      (let-values ([(x bfp) (get-datum/annotations ip sfd bfp)])
        (if (eof-object? x)
            '()
            (cons x (f bfp))))))

  (define (read-bytevector name contents)
    (let* ([ip (open-bytevector-input-port contents)]
           [sfd (make-source-file-descriptor name ip #t)]
           [ip (transcoded-port ip (make-utf8-transcoder))])
      (get-datum/annotations-all ip sfd 0)))

  ;; Explicitly skip a #! line at the beginning of the file.
  (define (get-source-offset ip)
    (let ([start (port-position ip)])
      (or (and (eqv? (get-u8 ip) (char->integer #\#))
               (eqv? (get-u8 ip) (char->integer #\!))
               (let ([b (get-u8 ip)])
                 (or (eqv? b (char->integer #\space)) (eqv? b (char->integer #\/))))
               (let lp ()
                 (let ([b (get-u8 ip)])
                   (or (eof-object? b) (eqv? b (char->integer #\newline)) (lp)))))
          (set-port-position! ip start)))
    (port-position ip))

  (define (sorted-cells ht create-time cell->record cell->handle)
    ;; cell is in ht iff its cell->handle is not #f
    (define (cell<? x1 x2)
      (let ([t1 (create-time (cell->record x1))] [t2 (create-time (cell->record x2))])
        (cond
         [(< t1 t2) #t]
         [(> t1 t2) #f]
         [else (< (cell->handle x1) (cell->handle x2))])))
    (let ([v (with-interrupts-disabled (hashtable-cells ht))])
      (vector-sort! cell<? v)
      v))

  (define-record-type %type-reporter
    (nongenerative)
    (fields
     (immutable name)
     (immutable count)
     (immutable print)))

  (define foreign-handle-reporters (make-hashtable symbol-hash eq?))

  (define (add-foreign-reporter name table cell->record cell->handle get-create-time print-one)
    (define (get-count) (hashtable-size table))
    (define print
      (case-lambda
       [() (print (current-output-port))]
       [(op)
        (vector-for-each
         (lambda (x)
           (print-one op (cell->record x) (cell->handle x)))
         (sorted-cells table get-create-time cell->record cell->handle))]))
    (hashtable-update! foreign-handle-reporters name
      (lambda (prev)
        (when prev (raise `#(type-already-registered ,name)))
        (make-%type-reporter name get-count print))
      #f))

  (define (count-foreign-handles obj report-count)
    (vector-for-each
     (lambda (cell)
       (report-count obj (car cell) ((%type-reporter-count (cdr cell)))))
     (hashtable-cells foreign-handle-reporters))
    obj)

  (define (get-type-reporter who type)
    (let ([tr (hashtable-ref foreign-handle-reporters type #f)])
      (if (%type-reporter? tr)
          tr
          (bad-arg who type))))

  (define (foreign-handle-count type)
    (%type-reporter-count
     (get-type-reporter 'foreign-handle-count type)))

  (define (foreign-handle-print type)
    (%type-reporter-print
     (get-type-reporter 'foreign-handle-print type)))

  (define print-foreign-handles
    (case-lambda
     [() (print-foreign-handles (current-output-port))]
     [(op)
      (define (symbol<? a b)
        (string<? (symbol->string a) (symbol->string b)))
      (vector-for-each
       (lambda (cell)
         (fprintf op "\n~a:\n" (car cell))
         ((%type-reporter-print (cdr cell)) op))
       (vector-sort (lambda (a b) (symbol<? (car a) (car b)))
         (hashtable-cells foreign-handle-reporters)))]))

  (define (make-foreign-handle-guardian name get-handle set-handle! get-create-time close-handle print)
    (define guardian (make-guardian))
    (define table (make-weak-eq-hashtable))
    (define (close-dead-handles)
      ;; This procedure runs in the finalizer process.
      (let ([x (guardian)])
        (when x
          (catch (close-handle x))
          (close-dead-handles))))
    (arg-check 'make-foreign-handle-guardian
      [name symbol?]
      [get-handle procedure?]
      [set-handle! procedure?]
      [get-create-time procedure?]
      [close-handle procedure?]
      [print procedure?])
    (add-finalizer close-dead-handles)
    (add-foreign-reporter name table car cdr get-create-time print)
    (lambda (record handle)
      (cond
       [handle
        ;; avoid re-registering with guardian if we're reviving handle because
        ;; close failed and we want to be able to print the handle
        (if (get-handle record)
            (guardian record)
            (set-handle! record handle))
        (eq-hashtable-set! table record handle)]
       [else
        (set-handle! record #f)
        (eq-hashtable-delete! table record)])
      record))

  (define (osi-port-closed? p)
    (unless (osi-port? p)
      (bad-arg 'osi-port-closed? p))
    (not (osi-port-handle p)))

  (define osi-ports
    (make-foreign-handle-guardian 'osi-ports
      osi-port-handle
      osi-port-handle-set!
      osi-port-create-time
      close-osi-port
      (lambda (op p handle)
        (fprintf op "  ~d: ~a opened ~d\n" handle
          (osi-port-name p)
          (osi-port-create-time p)))))

  (define osi-port-count (foreign-handle-count 'osi-ports))
  (define print-osi-ports (foreign-handle-print 'osi-ports))

  (define (@make-osi-port name handle)
    (osi-ports (make-osi-port name (erlang:now) handle) handle))

  ;; Path watching

  (define-record-type path-watcher
    (nongenerative)
    (fields
     (immutable path)
     (immutable create-time)
     (mutable handle)))

  (define (close-path-watcher watcher)
    (unless (path-watcher? watcher)
      (bad-arg 'close-path-watcher watcher))
    (with-interrupts-disabled
     (let ([handle (path-watcher-handle watcher)])
       (when handle
         (osi_close_path_watcher handle)
         (path-watchers watcher #f)))))

  (define path-watchers
    (make-foreign-handle-guardian 'path-watchers
      path-watcher-handle
      path-watcher-handle-set!
      path-watcher-create-time
      close-path-watcher
      (lambda (op p handle)
        (fprintf op "  ~d: ~a opened ~d\n" handle
          (path-watcher-path p)
          (path-watcher-create-time p)))))

  (define path-watcher-count (foreign-handle-count 'path-watchers))
  (define print-path-watchers (foreign-handle-print 'path-watchers))

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
       [(,who . ,errno) (io-error path who errno)]
       [,handle
        (path-watchers (make-path-watcher path (erlang:now) handle) handle)])))

  ;; Console Ports

  (define (make-console-input)
    (binary->utf8 (make-osi-input-port (open-fd-port "stdin-nb" 0 #f))))

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

  (define (open-fd-port name fd close?)
    (unless (string? name)
      (bad-arg 'open-fd-port name))
    (unless (and (fixnum? fd) (fx>= fd 0))
      (bad-arg 'open-fd-port fd))
    (with-interrupts-disabled
     (match (osi_open_fd* fd close?)
       [(,who . ,errno) (io-error name who errno)]
       [,handle (@make-osi-port name handle)])))

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
     (match (osi_get_file_size* (get-osi-port-handle 'get-file-size port)
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

  (define (get-real-path path)
    (define result)
    (with-interrupts-disabled
     (match (osi_get_real_path* path
              (let ([p self])
                (lambda (r)
                  (set! result r)
                  (complete-io p))))
       [#t
        (wait-for-io path)
        (match result
          [(,who . ,errno) (io-error path who errno)]
          [,path path])]
       [(,who . ,errno) (io-error path who errno)])))

  (define get-stat
    (case-lambda
     [(path follow?)
      (define result)
      (with-interrupts-disabled
       (match (osi_get_stat* path follow?
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
    (unless (memq type '(binary-input binary-output binary-append input output append))
      (bad-arg 'open-file type))
    (let ([port (open-file-port name flags mode)])
      (define fp 0)
      (define (r! bv start n)
        (let ([count (read-osi-port port bv start n fp)])
          (set! fp (+ fp count))
          count))
      (define (w! bv start n)
        (let ([count (write-osi-port port bv start n fp)])
          (set! fp (+ fp count))
          count))
      (define (a! bv start n) (write-osi-port port bv start n -1))
      (define (gp) fp)
      (define (sp! pos) (set! fp pos))
      (define (close) (close-osi-port port))
      (let open ([type type])
        (case type
          [(binary-input)
           (make-custom-binary-input-port name r! gp sp! close)]
          [(binary-output)
           (make-custom-binary-output-port name w! gp sp! close)]
          [(binary-append)
           (make-custom-binary-output-port name a! #f #f close)]
          [(input) (binary->utf8 (open 'binary-input))]
          [(output) (binary->utf8 (open 'binary-output))]
          [(append) (binary->utf8 (open 'binary-append))]))))

  (define (open-file-to-read name)
    (open-file name O_RDONLY 0 'input))

  (define (open-binary-file-to-read name)
    (open-file name O_RDONLY 0 'binary-input))

  (define (open-file-to-write name)
    (open-file name (+ O_WRONLY O_CREAT O_EXCL) #o666 'output))

  (define (open-binary-file-to-write name)
    (open-file name (+ O_WRONLY O_CREAT O_EXCL) #o666 'binary-output))

  (define (open-file-to-append name)
    (open-file name (+ O_WRONLY O_CREAT O_APPEND) #o666 'append))

  (define (open-binary-file-to-append name)
    (open-file name (+ O_WRONLY O_CREAT O_APPEND) #o666 'binary-append))

  (define (open-file-to-replace name)
    (open-file name (+ O_WRONLY O_CREAT O_TRUNC) #o666 'output))

  (define (open-binary-file-to-replace name)
    (open-file name (+ O_WRONLY O_CREAT O_TRUNC) #o666 'binary-output))

  (define (read-file name)
    (let ([port (open-file-port name O_RDONLY 0)])
      (on-exit (close-osi-port port)
        (let ([n (get-file-size port)])
          (if (> n 0)
              (let* ([bv (make-bytevector n)]
                     [count (read-osi-port port bv 0 n 0)])
                (unless (eqv? count n)
                  (raise `#(unexpected-eof ,name)))
                bv)
              #vu8())))))

  ;; Process Ports

  (define (spawn-os-process path args process)
    (unless (and (list? args) (for-all string? args))
      (bad-arg 'spawn-os-process args))
    (unless (process? process)
      (bad-arg 'spawn-os-process process))
    (with-interrupts-disabled
     (match (osi_spawn* path args
              (lambda (os-pid exit-status term-signal)
                (send process
                  `#(process-terminated ,os-pid ,exit-status ,term-signal))))
       [#(,to-stdin ,from-stdout ,from-stderr ,os-pid)
        (values
         (let ([name (format "process ~d stdin" os-pid)])
           (make-oport name (@make-osi-port name to-stdin)))
         (let ([name (format "process ~d stdout" os-pid)])
           (make-iport name (@make-osi-port name from-stdout) #t))
         (let ([name (format "process ~d stderr" os-pid)])
           (make-iport name (@make-osi-port name from-stderr) #t))
         os-pid)]
       [(,who . ,errno) (io-error path who errno)])))

  ;; TCP/IP Ports

  (define-record-type listener
    (nongenerative)
    (fields
     (immutable address)
     (immutable port-number)
     (immutable create-time)
     (mutable handle)))

  (define tcp-listeners
    (make-foreign-handle-guardian 'tcp-listeners
      listener-handle
      listener-handle-set!
      listener-create-time
      (lambda (l) (close-tcp-listener l))
      (lambda (op l handle)
        (define (contains-period? s)
          (let lp ([i 0] [n (string-length s)])
            (and (< i n)
                 (or (char=? (string-ref s i) #\.)
                     (lp (+ i 1) n)))))
        (fprintf op "  ~d: " handle)
        (let ([addr (listener-address l)])
          (if (contains-period? addr)
              (display-string addr op)
              (fprintf op "[~a]" addr))
          (fprintf op ":~d opened ~d\n"
            (listener-port-number l)
            (listener-create-time l))))))

  (define tcp-listener-count (foreign-handle-count 'tcp-listeners))
  (define print-tcp-listeners (foreign-handle-print 'tcp-listeners))

  (define (port-number? x) (and (fixnum? x) (fx<= 0 x 65535)))

  (define (listen-tcp address port-number process)
    ;; Keep a weak reference to the listener in cell so that the
    ;; callback can use it.
    (define cell (weak-cons 0 0))
    (unless (string? address)
      (bad-arg 'listen-tcp address))
    (unless (port-number? port-number)
      (bad-arg 'listen-tcp port-number))
    (unless (process? process)
      (bad-arg 'listen-tcp process))
    (with-interrupts-disabled
     (match (osi_listen_tcp* address port-number
              (lambda (r)
                ;; This procedure runs in the event loop.
                (let ([listener (car cell)])
                  (if (listener? listener)
                      (if (pair? r)
                          (send process
                            `#(accept-tcp-failed ,listener ,(car r) ,(cdr r)))
                          (let* ([name (osi_get_ip_address r)]
                                 [port (@make-osi-port name r)])
                            (send process
                              `#(accept-tcp ,listener ,(make-iport name port #f)
                                  ,(make-oport name port)))))
                      (unless (pair? r)
                        (osi_close_port* r 0))))))
       [(,who . ,errno)
        (raise `#(listen-tcp-failed ,address ,port-number ,who ,errno))]
       [,handle
        (let ([l (make-listener address (osi_get_tcp_listener_port handle)
                   (erlang:now) handle)])
          (set-car! cell l)
          (tcp-listeners l handle))])))

  (define (close-tcp-listener listener)
    ;; This procedure may run in the finalizer process.
    (unless (listener? listener)
      (bad-arg 'close-tcp-listener listener))
    (with-interrupts-disabled
     (let ([handle (listener-handle listener)])
       (when handle
         (osi_close_tcp_listener handle)
         (tcp-listeners listener #f)))))

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

  (define-record-type sighandler
    (nongenerative)
    (fields
     (immutable signum)
     (immutable create-time)
     (immutable handle)
     (immutable callback)))

  (define signal-handlers
    (let ([table (make-hashtable values fx=)])
      (define cell->record cdr)
      (define (cell->handle cell) (sighandler-handle (cell->record cell)))
      (add-foreign-reporter 'signal-handlers table cell->record cell->handle
        sighandler-create-time
        (lambda (op s handle)
          (fprintf op "  ~d: for signal ~a registered ~d\n" handle
            (sighandler-signum s)
            (sighandler-create-time s))))
      table))

  (define signal-handler-count (foreign-handle-count 'signal-handlers))
  (define print-signal-handlers (foreign-handle-print 'signal-handlers))

  (define (@signal-handler-callback signum)
    (cond
     [(hashtable-ref signal-handlers signum #f) => sighandler-callback]
     [else #f]))

  (define (@deliver-signal signum)
    (cond
     [(@signal-handler-callback signum) =>
      (lambda (callback) (callback signum))]))

  (define signal-handler
    (case-lambda
     [(signum)
      (arg-check 'signal-handler [signum fixnum? fxpositive?])
      (with-interrupts-disabled
        (@signal-handler-callback signum))]
     [(signum callback)
      (arg-check 'signal-handler
        [signum fixnum? fxpositive?]
        [callback (lambda (cb) (or (not cb) (procedure? cb)))])
      (with-interrupts-disabled
       (let* ([cell (hashtable-cell signal-handlers signum #f)]
              [prev (cdr cell)])
         (define (set-handler! signum handle callback)
           (set-cdr! cell
             (make-sighandler signum (erlang:now) handle callback)))
         (cond
          [(sighandler? prev)
           (if (not callback)
               (@close-sighandler prev)
               (set-handler! signum (sighandler-handle prev) callback))]
          [(procedure? callback)
           (match (osi_start_signal* signum)
             [(,who . ,errno)
              (hashtable-delete! signal-handlers signum)
              (io-error signum who errno)]
             [,handle
              (set-handler! signum handle callback)])])))]))

  (define (@close-sighandler handler)
    (let ([handle (sighandler-handle handler)])
      (match (osi_stop_signal* handle)
        [#t
         (hashtable-delete! signal-handlers (sighandler-signum handler))]
        [(,who . ,errno) (io-error (sighandler-signum handler) who errno)])))

  (set-top-level-value! '@deliver-signal @deliver-signal)
  )
