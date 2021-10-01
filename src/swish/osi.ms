;;; Copyright 2019 Beckman Coulter, Inc.
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
 (swish mat)
 (swish script-testing)
 (swish testing))

(isolate-mat exit-with-pending-read ()
  ;; On Unix systems exit blocks if there is an outstanding read on
  ;; stdin. Swish uses _exit, this test ensures it gets called despite
  ;; a pending read on the repl.
  (test-os-process swish-exe '()
    "(spawn (lambda () (receive (after 100 (osi_exit 0)))))"
    '()))

(isolate-mat submit-request ()
  (define test-request
    (write-test-file "test-request.ss"
      (lambda ()
        (for-each pretty-print
          '((require-shared-object 'test-request)
            (define main self)
            (define actual-reps (make-hashtable values =))
            (define (make-work! expected-thread-no cb-name)
              (define (proc thread-no rep)
                (assert (= thread-no expected-thread-no))
                (hashtable-update! actual-reps thread-no
                  (lambda (prev) (cons rep prev))
                  #f)
                (when (zero? rep) (send main 'done)))
              (set-top-level-value! (string->symbol cb-name) proc))
            (define add-work!
              (let ([fp (foreign-procedure "add_work" (int string) ptr)])
                (lambda (iterations thread-no cb-name)
                  (cond
                   [(fp iterations cb-name)
                    (make-work! thread-no cb-name)
                    (hashtable-set! actual-reps thread-no '())
                    #t]
                   [else #f]))))
            (define (run n)
              (define call-now (foreign-procedure "call_now" () int))
              (define start (foreign-procedure "create_threads" () ptr))
              (define stop (foreign-procedure "join_threads" () void))
              (define (expected-reps thread-no) (iota (+ n 1)))
              (assert (= (call-now) UV_EPERM))
              (assert (> n 0))
              (hashtable-clear! actual-reps)
              (printf "add-work\n")
              (let ([nthreads
                     (do ([t 0 (+ t 1)])
                         ;; we want maximal contention, so use
                         ;; same # iterations for each thread
                         ((not (add-work! n t (format "callback-~s" t))) t))])
                (printf "start\n")
                (start)
                (do ([t 0 (+ t 1)]) ((= t nthreads))
                  (receive [done 'ok]))
                (stop)
                (printf "stop\n"))
              (vector-for-each (lambda (cell) (unless (equal? (cdr cell) (expected-reps (car cell))) (raise cell)))
                (hashtable-cells actual-reps))
              (printf "ok\n"))
            (match (command-line-arguments)
              [(,n)
               (run (string->number n))]))))))
  (with-output-to-file (string-append (path-root test-request) ".config")
    (lambda ()
      (parameterize ([app:config (json:make-object)])
        (provide-shared-object 'test-request
          (path-combine (base-dir) "src" "swish"
            (case (machine-type)
              [(a6nt i3nt ta6nt ti3nt) "test-request.dll"]
              [(a6osx ta6osx) "test-request.dylib"]
              [else "test-request.so"])))
        (json:write (current-output-port) (app:config) 0)))
    'replace)
  (script-test test-request '("150") "" '("add-work" "start" "stop" "ok")))

;; test for change in semantics, e.g., units of the underlying uv_get_total_memory
(isolate-mat sanity-check-memory-info ()
  ;; test only on Linux; interface should be the same on other platforms
  (match (get-uname)
    [`(<uname> [system "Linux"])
     (let* ([os-total (osi_get_total_memory)] ;; in bytes
            [os-free (osi_get_free_memory)]   ;; in bytes
            [ip (open-file-to-read "/proc/meminfo")])
       (assert (< os-free os-total))
       (on-exit (close-port ip)
         (let lp ()
           (match (get-line ip)
             [#!eof (throw `reference-memory-info-not-found)]
             [,line
              ;; free memory fluctuates too much to check reliably this way
              (match (pregexp-match (re "^MemTotal: *([0-9]+) kB") line)
                [(,_ ,total-kb)
                 (assert (= os-total (* 1024 (string->number total-kb))))]
                [,_ (lp)])]))))]
    [,_ 'ok]))
