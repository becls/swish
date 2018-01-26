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

(library (swish profile)
  (export
   profile:merge
   profile:save
   profile:start
   profile:start&link
   )
  (import
   (swish app-io)
   (swish erlang)
   (swish gen-server)
   (swish io)
   (swish meta)
   (except (chezscheme) define-record exit))

  (define (source-object-bfp=? x y)
    (= (source-object-bfp x) (source-object-bfp y)))

  (define (sfd=? x y)
    (or (eq? x y)
        (and (string=? (source-file-descriptor-path x)
               (source-file-descriptor-path y))
             (= (source-file-descriptor-checksum x)
                (source-file-descriptor-checksum y)))))

  (define (find-src x)
    (if (source-object? x)
        x
        (and (vector? x) (fx> (vector-length x) 1)
             (find-src (vector-ref x 1)))))

  (define (add-filedata sfd-table item)
    ;; item = (record . count)
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

  (define server-filename (path-combine data-dir "server.profile"))

  (define (profile-load filename ht)
    (let ([ip (open-file filename O_RDONLY #o777 'binary-input)])
      (on-exit (close-port ip)
        (let lp ()
          (let ([ls (fasl-read ip)])
            (unless (eof-object? ls)
              (for-each (lambda (x) (add-filedata ht x)) ls)
              (lp)))))))

  (define (profile-save filename sfd-table)
    (let ([op (open-file (make-directory-path filename)
                (+ O_WRONLY O_CREAT O_TRUNC) #o777 'binary-output)])
      (on-exit (close-port op)
        (let-values ([(keys vals) (hashtable-entries sfd-table)])
          (vector-for-each
           (lambda (sfd source-table)
             (let-values ([(keys vals) (hashtable-entries source-table)])
               (fasl-write
                (let lp ([i 0] [ls '()])
                  (if (= i (vector-length keys))
                      ls
                      (lp (+ i 1)
                        (cons
                         (cons (vector-ref keys i) (vector-ref vals i))
                         ls))))
                op)))
           keys vals))
        'ok)))

  (define (profile-update ht)
    (for-each (lambda (x) (add-filedata ht x))
      (with-interrupts-disabled
       (let ([data (profile-dump)])
         (profile-clear)
         data)))
    (profile-save server-filename ht))

  (define (init)
    (cond
     [(null? (profile-dump))
      'ignore]
     [else
      ;; Trap exits so that terminate is called when the system is
      ;; going down.
      (process-trap-exit #t)
      (let ([ht (make-hashtable source-file-descriptor-checksum sfd=?)])
        (catch (profile-load server-filename ht))
        `#(ok ,ht 60000))]))
  (define (terminate reason ht)
    (profile-update ht)
    'ok)
  (define (handle-call msg from ht)
    (match msg
      [save (profile-update ht) `#(reply ok ,ht 60000)]))
  (define (handle-cast msg ht) (match msg))
  (define (handle-info msg ht)
    (match msg
      [timeout
       (match (catch (profile-update ht))
         [ok `#(no-reply ,ht 60000)]
         [#(EXIT #(io-error ,_ ,_ ,_)) `#(no-reply ,ht 1000)]
         [#(EXIT ,reason) (exit reason)])]))
  (define (profile:start&link)
    (gen-server:start&link 'profiler))
  (define (profile:start)
    (gen-server:start 'profiler))

  (define (profile:save)
    (gen-server:call 'profiler 'save 'infinity))

  (define (profile:merge . paths)
    (let ([ht (make-hashtable source-file-descriptor-checksum sfd=?)])
      (for-each (lambda (path) (profile-load path ht)) paths)
      (profile-save server-filename ht)))
  )
