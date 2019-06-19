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
(library (swish osi)
  (export
   osi_bind_statement
   osi_bind_statement*
   osi_chmod
   osi_chmod*
   osi_clear_statement_bindings
   osi_clear_statement_bindings*
   osi_close_database
   osi_close_database*
   osi_close_path_watcher
   osi_close_port
   osi_close_port*
   osi_close_tcp_listener
   osi_connect_tcp
   osi_connect_tcp*
   osi_exit
   osi_finalize_statement
   osi_finalize_statement*
   osi_get_argv
   osi_get_bytes_used
   osi_get_callbacks
   osi_get_error_text
   osi_get_executable_path
   osi_get_file_size
   osi_get_file_size*
   osi_get_hostname
   osi_get_hostname*
   osi_get_hrtime
   osi_get_ip_address
   osi_get_ip_address*
   osi_get_last_insert_rowid
   osi_get_last_insert_rowid*
   osi_get_pid
   osi_get_real_path
   osi_get_real_path*
   osi_get_sqlite_status
   osi_get_sqlite_status*
   osi_get_stat
   osi_get_stat*
   osi_get_statement_columns
   osi_get_statement_columns*
   osi_get_statement_expanded_sql
   osi_get_statement_expanded_sql*
   osi_get_tcp_listener_port
   osi_get_tcp_listener_port*
   osi_get_temp_directory
   osi_get_temp_directory*
   osi_get_time
   osi_interrupt_database
   osi_is_quantum_over
   osi_kill
   osi_kill*
   osi_list_directory
   osi_list_directory*
   osi_list_uv_handles
   osi_listen_tcp
   osi_listen_tcp*
   osi_make_directory
   osi_make_directory*
   osi_make_uuid
   osi_make_uuid*
   osi_open_database
   osi_open_database*
   osi_open_fd
   osi_open_fd*
   osi_open_file
   osi_open_file*
   osi_prepare_statement
   osi_prepare_statement*
   osi_read_port
   osi_read_port*
   osi_remove_directory
   osi_remove_directory*
   osi_rename
   osi_rename*
   osi_reset_statement
   osi_reset_statement*
   osi_set_quantum
   osi_start_signal
   osi_start_signal*
   osi_stop_signal
   osi_stop_signal*
   osi_spawn
   osi_spawn*
   osi_step_statement
   osi_step_statement*
   osi_unlink
   osi_unlink*
   osi_watch_path
   osi_watch_path*
   osi_write_port
   osi_write_port*
   string->uuid
   uuid->string
   )
  (import (chezscheme))

  (define _init_ ((foreign-procedure "osi_init" () void)))

  (define-syntax fdefine
    (syntax-rules ()
      [(_ name (arg-name arg-type) ... ret-type)
       (define name
         (foreign-procedure (symbol->string 'name) (arg-type ...) ret-type))]))

  (define-syntax (define-osi x)
    (syntax-case x ()
      [(_ name (arg-name arg-type) ...)
       (with-syntax
        ([name*
          (datum->syntax #'name
            (string->symbol
             (string-append (symbol->string (datum name)) "*")))])
        #'(begin
            (define name*
              (foreign-procedure (symbol->string 'name) (arg-type ...) ptr))
            (define (name arg-name ...)
              (let ([x (name* arg-name ...)])
                (if (not (and (pair? x) (symbol? (car x))))
                    x
                    (raise `#(osi-error name ,(car x) ,(cdr x))))))))]))

  ;; System
  (fdefine osi_get_argv ptr)
  (fdefine osi_get_bytes_used size_t)
  (fdefine osi_get_callbacks (timeout unsigned-64) ptr)
  (fdefine osi_get_error_text (err int) string)
  (define-osi osi_get_hostname)
  (fdefine osi_get_hrtime unsigned-64)
  (fdefine osi_get_pid int)
  (fdefine osi_get_time unsigned-64)
  (fdefine osi_is_quantum_over boolean)
  (fdefine osi_list_uv_handles ptr)
  (define-osi osi_make_uuid)
  (fdefine osi_set_quantum (nanoseconds unsigned-64) void)
  (define-osi osi_start_signal (signum int))
  (define-osi osi_stop_signal (handler uptr))

  ;; Ports
  (define-osi osi_read_port (port uptr) (buffer ptr) (start-index size_t) (size unsigned-32) (offset integer-64) (callback ptr))
  (define-osi osi_write_port (port uptr) (buffer ptr) (start-index size_t) (size unsigned-32) (offset integer-64) (callback ptr))
  (define-osi osi_close_port (port uptr) (callback ptr))

  ;; Process
  (fdefine osi_exit (status int) void)
  (define-osi osi_spawn (path string) (args ptr) (callback ptr))
  (define-osi osi_kill (pid int) (signum int))

  ;; File System
  (define-osi osi_open_fd (fd int) (close? boolean))
  (define-osi osi_open_file (path string) (flags int) (mode int) (callback ptr))
  (define-osi osi_get_executable_path)
  (define-osi osi_get_file_size (port uptr) (callback ptr))
  (define-osi osi_get_real_path (path string) (callback ptr))
  (define-osi osi_get_temp_directory)
  (define-osi osi_chmod (path string) (mode int) (callback ptr))
  (define-osi osi_make_directory (path string) (mode int) (callback ptr))
  (define-osi osi_list_directory (path string) (callback ptr))
  (define-osi osi_remove_directory (path string) (callback ptr))
  (define-osi osi_rename (path string) (new-path string) (callback ptr))
  (define-osi osi_get_stat (path string) (follow? boolean) (callback ptr))
  (define-osi osi_unlink (path string) (callback ptr))
  (define-osi osi_watch_path (path string) (callback ptr))
  (fdefine osi_close_path_watcher (watcher uptr) void)

  ;; TCP/IP
  (define-osi osi_connect_tcp (node string) (service string) (callback ptr))
  (define-osi osi_listen_tcp (address string) (port unsigned-16) (callback ptr))
  (fdefine osi_close_tcp_listener (listener uptr) void)
  (define-osi osi_get_tcp_listener_port (listener uptr))
  (define-osi osi_get_ip_address (port uptr))

  (define (uuid->string uuid)
    (unless (and (bytevector? uuid) (= (bytevector-length uuid) 16))
      (raise `#(bad-arg uuid->string ,uuid)))
    (format "~8,'0X-~4,'0X-~4,'0X-~4,'0X-~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X~2,'0X"
      (#3%bytevector-u32-ref uuid 0 'little)
      (#3%bytevector-u16-ref uuid 4 'little)
      (#3%bytevector-u16-ref uuid 6 'little)
      (#3%bytevector-u16-ref uuid 8 'big)
      (#3%bytevector-u8-ref uuid 10)
      (#3%bytevector-u8-ref uuid 11)
      (#3%bytevector-u8-ref uuid 12)
      (#3%bytevector-u8-ref uuid 13)
      (#3%bytevector-u8-ref uuid 14)
      (#3%bytevector-u8-ref uuid 15)))

  (define (string->uuid s)
    (define (err) (raise `#(bad-arg string->uuid ,s)))
    (define (decode-digit c)
      (cond
       [(#3%char<=? #\0 c #\9)
        (#3%fx- (#3%char->integer c) (char->integer #\0))]
       [(#3%char<=? #\A c #\F)
        (#3%fx- (#3%char->integer c) (fx- (char->integer #\A) 10))]
       [(#3%char<=? #\a c #\f)
        (#3%fx- (#3%char->integer c) (fx- (char->integer #\a) 10))]
       [else (err)]))
    (define-syntax decode
      (syntax-rules ()
        [(_ i) (decode-digit (#3%string-ref s i))]))
    (define-syntax build
      (syntax-rules ()
        [(_ i ...)
         (bytevector
          (#3%fx+ (#3%fx* (decode i) 16) (decode (#3%fx+ i 1)))
          ...)]))
    (unless (and (string? s)
                 (#3%fx= (#3%string-length s) 36)
                 (#3%char=? (#3%string-ref s 8) #\-)
                 (#3%char=? (#3%string-ref s 13) #\-)
                 (#3%char=? (#3%string-ref s 18) #\-)
                 (#3%char=? (#3%string-ref s 23) #\-))
      (err))
    (build 6 4 2 0 11 9 16 14 19 21 24 26 28 30 32 34))

  ;; SQLite
  (define-osi osi_open_database (filename string) (flags int) (callback ptr))
  (define-osi osi_close_database (database uptr) (callback ptr))
  (define-osi osi_prepare_statement (database uptr) (sql ptr) (callback ptr))
  (define-osi osi_finalize_statement (statement uptr))
  (define-osi osi_bind_statement (statement uptr) (index int) (datum ptr))
  (define-osi osi_clear_statement_bindings (statement uptr))
  (define-osi osi_get_last_insert_rowid (database uptr))
  (define-osi osi_get_statement_columns (statement uptr))
  (define-osi osi_get_statement_expanded_sql (statement uptr))
  (define-osi osi_reset_statement (statement uptr))
  (define-osi osi_step_statement (statement uptr) (callback ptr))
  (fdefine osi_interrupt_database (database uptr) void)
  (define-osi osi_get_sqlite_status (operation int) (reset? boolean))
  )
