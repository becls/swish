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

#!chezscheme
(library (swish app-core)
  (export
   $exit-process
   $swish-start
   app-exception-handler
   app:name
   app:path
   application:shutdown
   repl-level
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish errors)
   (swish io)
   (swish meta)
   (swish osi))

  (define application-exit-code 2)

  (define app-identity #f)
  (define pid1 #f)

  ;; intended to return short descriptive name of the application if known
  (define app:name
    (make-inherited-parameter #f
      (lambda (x)
        (unless (or (not x) (string? x))
          (bad-arg 'app:name x))
        (when (eq? self pid1) (set! app-identity x))
        (and x (path-root (path-last x))))))

  ;; intended to return full path to the application script or executable, if known
  (define app:path
    (make-inherited-parameter #f
      (lambda (x)
        (unless (or (not x) (string? x))
          (bad-arg 'app:path x))
        (and x (get-real-path x)))))

  (define repl-level
    (make-process-parameter 0
      (lambda (n)
        (arg-check 'repl-level [n fixnum? nonnegative?])
        n)))

  (define (strip-prefix string prefix)
    (define slen (string-length string))
    (define plen (string-length prefix))
    (and (> slen plen)
         (string=? (substring string 0 plen) prefix)
         (substring string plen slen)))

  (define (claim-exception who c)
    (define stderr (console-error-port))
    (define os (open-output-string))
    (define (report msg op)
      (fresh-line op)
      (fprintf op "~@[~a: ~]~a\n" who msg)
      (flush-output-port op))
    (define (flush!)
      (let ([msg (get-output-string os)])
        ;; may fail when writing to stderr, e.g., on broken pipe
        (match (try (report msg stderr))
          [`(catch ,_)
           ;; try once more with a fresh stderr
           (try (report msg (binary->utf8 (standard-error-port (buffer-mode line)))))]
          [,_ (void)])))
    (guard (_ [else (get-output-string os) (display-condition c os)])
      (parameterize ([print-level 3] [print-length 6])
        (cond
         [(match c [`(catch ,_) #t] [,c (not (condition? c))])
          (let ([text (exit-reason->english c)])
            (unless who (display "Exception: " os))
            (display text os))]
         [(not who) (display (exit-reason->english c) os)]
         [else
          (display-condition (condition (make-who-condition #f) c) os)
          (let ([text (get-output-string os)])
            (display (or (strip-prefix text "Warning: ")
                         (strip-prefix text "Exception: ")
                         text)
              os))])))
    ;; add final "." since display-condition does not and exit-reason->english may or may not
    (let ([i (- (port-output-index os) 1)])
      (when (and (> i 0) (not (char=? #\. (string-ref (port-output-buffer os) i))))
        (display "." os)))
    (cond
     [(and (warning? c) (not (serious-condition? c))) (flush!)]
     [else
      (when (and (> (repl-level) 0)
                 (interactive?)
                 (continuation-condition? (debug-condition)))
        (display "\nType (debug) to enter the debugger." os))
      (flush!)
      (reset)]))

  (define (app-exception-handler c)
    (debug-condition c)
    (claim-exception (app:name)
      (match c
        [`(catch ,r) (if (condition? r) r c)]
        [,_ c])))

  (define (int32? x) (and (or (fixnum? x) (bignum? x)) (<= #x-7FFFFFFF x #x7FFFFFFF)))
  (define (->exit-status exit-code)
    (cond
     [(int32? exit-code) exit-code]
     [(eq? exit-code (void)) 0]
     [else
      ($console-event-handler (format "application shutdown due to (exit ~s)" exit-code))
      1]))

  (profile-omit ;; profiler won't have a chance to save data for these due to osi_exit

   (define (exit-process exit-code)
     (trap-signals #f)
     (catch (flush-output-port (console-output-port)))
     (catch (flush-output-port (console-error-port)))
     (let ([p (#%$top-level-value '$console-input-port)])
       ;; convince Chez Scheme to close console-input port
       (#%$set-top-level-value! '$console-input-port #f)
       (when (input-port? p)
         (close-port p)))
     (osi_exit (->exit-status exit-code)))

   (define ($exit-process)
     (exit-process application-exit-code)))

  (define application:shutdown
    (case-lambda
     [() (application:shutdown 0)]
     [(exit-code . _)
      (cond
       [(whereis 'application) =>
        (lambda (p)
          (set! application-exit-code exit-code)
          (kill p 'shutdown))]
       [else (exit-process exit-code)])]))

  (define (quit)
    ;; Spawn a process to avoid deadlock when application:shutdown
    ;; calls exit-process.
    (spawn application:shutdown))

  (define (handle-signal signo)
    (match signo
      [,@SIGHUP
       ;; LSB 3.1.1: return 3 for unimplemented feature, e.g., SIGHUP to reload
       (spawn (lambda () (application:shutdown 3)))]
      [,_ (quit)]))

  (define (trap-signals handler)
    (meta-cond
     [windows?
      (signal-handler SIGBREAK handler)
      (signal-handler SIGHUP handler)
      (signal-handler SIGINT handler)]
     [else
      (signal-handler SIGHUP handler)
      (signal-handler SIGINT handler)
      (signal-handler SIGTERM handler)]))

  (define started? #f)
  (define Charon #f)
  (define ($swish-start stand-alone? args run)
    (let ([who (osi_get_executable_path)])
      (parameterize ([command-line (cons who args)]
                     [command-line-arguments args])
        (cond
         [started? (run)]
         [else
          (set! pid1 self)
          (set! started? #t)
          (base-exception-handler app-exception-handler)
          (random-seed (+ (remainder (erlang:now) (- (ash 1 32) 1)) 1))
          (trap-signals handle-signal)
          (hook-console-input)
          (call/cc
           (lambda (bail)
             (exit-handler
              (lambda args
                (apply application:shutdown args)
                (bail)))
             (reset-handler
              (lambda ()
                (application:shutdown application-exit-code)
                (bail)))
             (when stand-alone?
               (app:name who)
               (app:path who))
             (set! Charon
               (spawn
                (let ([me self])
                  (lambda ()
                    (let ([m (monitor me)])
                      (receive
                       [`(DOWN ,@m ,_ normal) 'ok]
                       [`(DOWN ,@m ,_ ,reason ,e)
                        (app:name app-identity)
                        (on-exit (application:shutdown application-exit-code)
                          (app-exception-handler e))]))))))
             (call-with-values run exit)))
          (receive)]))))

  ;; External entry points are run from the event-loop process
  (set-top-level-value! '$shutdown quit)
  (set-top-level-value! '$suspend void)
  (set-top-level-value! '$resume void)
  )

#!eof mats

(load-this-exposing '(->exit-status))

(import
 (swish app-core)
 (swish mat)
 (swish profile)
 (swish testing)
 )

(mat coverage ()
  (match-let*
   ([27 (->exit-status 27)]
    [#x7ffffff (->exit-status #x7ffffff)]
    [0 (->exit-status (void))]
    [,os (open-output-string)]
    [1 (parameterize ([console-error-port os]) (->exit-status 'other))]
    [,stderr (get-output-string os)])
   (match-regexps '(seek "Event: \"application shutdown due to \\(exit other\\)\"")
     (split stderr #\newline)))
  )
