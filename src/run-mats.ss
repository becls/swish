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
 (swish profile)
 (swish string-utils)
 )

(define (run-suite basename)
  (define (start-profiler)
    (match (profile:start)
      [#(ok ,_) #t]
      [ignore #f]
      [#(error ,reason) (raise 'profile-failed-to-start)]))
  (reset-handler (lambda () (display "\nTest Failed\n") (abort 1)))
  (load (string-append basename ".ms"))
  (let ([profiling? (start-profiler)])
    (on-exit (when profiling? (profile:save))
      (run-mats-to-file (string-append basename ".mo")))))

(define (find-mo-files root)
  (define (find-mo root rel)
    (fold-left
     (lambda (fns x)
       (match x
         [(,fn . ,@DIRENT_FILE)
          (guard (ends-with? fn ".mo"))
          (cons (string-append rel fn) fns)]
         [,_ fns]))
     '()
     (list-directory (string-append root "/" rel))))
  (define (find-subdirs root rel)
    (fold-left
     (lambda (fns x)
       (match x
         [(,fn . ,@DIRENT_DIR) (cons (string-append rel fn "/") fns)]
         [,_ fns]))
     '()
     (list-directory (string-append root "/" rel))))
  (let lp ([files (find-mo root "")] [dirs (find-subdirs root "")])
    (if (null? dirs)
        files
        (lp
         (append files (find-mo root (car dirs)))
         (append (cdr dirs) (find-subdirs root (car dirs)))))))

(define (html-report indir filename)
  (define (stringify x)
    (if (string? x)
        x
        (format "~s" x)))
  (define (html-encode s)
    (let-values ([(op get) (open-string-output-port)])
      (let ([len (string-length s)])
        (do ((i 0 (+ i 1))) ((= i len))
          (let ([c (string-ref s i)])
            (case c
              [(#\") (display "&quot;" op)]
              [(#\&) (display "&amp;" op)]
              [(#\<) (display "&lt;" op)]
              [(#\>) (display "&gt;" op)]
              [else (write-char c op)])))
        (get))))
  (define (output-row op c1 c2 c3)
    (fprintf op "<tr><td>~a</td><td>~a</td><td>~a</td></tr>\n"
      (html-encode (stringify c1))
      (html-encode (stringify c2))
      (if c3
          (html-encode (stringify c3))
          "<p style='color:#007F00;'>PASS</p>")))
  (define (output-result op name tags result)
    (match result
      [pass (output-row op "" name #f)]
      [(fail . ,reason)
       (output-row op "" name reason)]))
  (define (results< x y)
    (match-let* ([(,name1 . ,_) x]
                 [(,name2 . ,_) y])
      (string-ci<? (symbol->string name1) (symbol->string name2))))
  (call-with-output-file filename
    (lambda (op)
      (fprintf op "<html>\n")
      (fprintf op "<body style='font-family:monospace;'>\n")
      (let-values ([(pass fail) (summarize-directory indir)])
        (if (eq? fail 0)
            (fprintf op "<h1>PASSED all ~a tests.</h1>\n" pass)
            (fprintf op
              "<h1>Failed ~a of ~a tests.</h1>\n" fail (+ fail pass))))
      (fprintf op "<table>\n")
      (output-row op "Suite" "Name" "Message")
      (for-each
       (lambda (in-file)
         (output-row op in-file "" "")
         (for-each (lambda (ls) (apply output-result op ls))
           (sort results< (load-results (string-append indir "/" in-file)))))
       (find-mo-files indir))
      (fprintf op "</table>\n")
      (fprintf op "</body></html>\n"))
    'replace))

(define (console-summary indir)
  (let-values ([(pass fail) (summarize-directory indir)])
    (printf "Tests run: ~s   Pass: ~s   Fail: ~s\n\n"
      (+ pass fail) pass fail)))

(define (exit-summary indirs)
  (match indirs
    [() (exit 0)]
    [(,indir . ,rest)
     (let-values ([(pass fail) (summarize-directory indir)])
       (if (> fail 0)
           (exit 1)
           (exit-summary rest)))]))

(define (summarize-directory indir)
  (summarize (map (lambda (x) (string-append indir "/" x))
               (find-mo-files indir))))
