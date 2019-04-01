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
(library (swish app)
  (export
   app-exception-handler
   app-sup-spec
   app:resume
   app:shutdown
   app:start
   app:suspend
   init-main-sup
   make-swish-sup-spec
   )
  (import
   (chezscheme)
   (swish app-core)
   (swish application)
   (swish cli)
   (swish erlang)
   (swish errors)
   (swish event-mgr)
   (swish gatekeeper)
   (swish http)
   (swish io)
   (swish log-db)
   (swish osi)
   (swish pregexp)
   (swish software-info)
   (swish statistics)
   (swish supervisor)
   )

  (define (app:start) (application:start init-main-sup))

  (alias app:shutdown application:shutdown)

  (alias app:suspend statistics:suspend)

  (alias app:resume statistics:resume)

  (define (init-main-sup)
    ;; When one process at this level crashes, we want the supervisor
    ;; to shutdown which only happens during restart. Thus, each
    ;; process is marked permanent and the restart intensity is 0.
    (supervisor:start&link 'main-sup 'one-for-all 0 1 (app-sup-spec)))

  (define (make-swish-sup-spec loggers)
    `(#(event-mgr ,event-mgr:start&link
         permanent 1000 worker)
      #(log-db ,log-db:start&link
         permanent 10000 worker)
      #(event-mgr-sentry
        ,(lambda ()
           `#(ok ,(spawn&link
                   (lambda ()
                     ;; Unregister event-mgr so that event-mgr:notify
                     ;; no longer sends events to log-db but to the
                     ;; console.  This prevents messages that occur
                     ;; after log-db shuts down from getting lost.
                     (process-trap-exit #t)
                     (receive
                      [#(EXIT ,_ ,_) (event-mgr:unregister)])))))
        permanent 1000 worker)
      #(log-db:setup ,(lambda () (log-db:setup loggers))
         temporary 1000 worker)
      #(statistics ,statistics:start&link
         permanent 1000 worker)
      #(gatekeeper ,gatekeeper:start&link
         permanent 1000 worker)
      #(http-sup ,http-sup:start&link permanent infinity supervisor)))

  (define app-sup-spec
    (make-parameter
     (make-swish-sup-spec (list swish-event-logger))
     (lambda (x)
       (let ([reason (supervisor:validate-start-specs x)])
         (when reason (raise reason)))
       x)))

  (define (set-random-seed)
    (random-seed (+ (remainder (erlang:now) (- (ash 1 32) 1)) 1)))

  (define cli
    (cli-specs
     default-help
     ["verbose" --verbose bool "trace boot file search"]
     ["version" --version bool "print version information"]
     ["quiet" -q bool "suppress startup message and prompt string"]
     ["args" -- (list . "arg") "remaining arguments are files to load"]
     ["files" (list . "file") "execute file with remaining arguments"]))

  (define (run)
    (eval '(import (swish imports)))
    (let* ([opt (parse-command-line-arguments cli)]
           [files (or (opt "files") '())])
      (when (opt "quiet") (waiter-prompt-string ""))
      (cond
       [(opt "help")
        (display-help (path-last (osi_get_executable_path)) cli (opt))
        (values)]
       [(opt "version")
        (printf "~a (~a)\n" (swish-version) (software-revision 'swish))
        (values)]
       [(null? files)                   ; repl
        (app:name #f)
        (app:path #f)
        (let ([filenames (or (opt "args") '())])
          (unless (opt "quiet")
            (printf "\n~a Version ~a\n" software-product-name software-version)
            (flush-output-port))
          (hook-console-input)
          (set-random-seed)
          (for-each load filenames)
          (new-cafe))]
       [else                            ; script
        (let ([script-file (car files)])
          (command-line (command-line-arguments))
          (command-line-arguments (cdr (command-line)))
          (app:name script-file)
          (app:path script-file)
          (set-random-seed)
          ;; use exit handler installed by the script, if any
          (match (catch (load script-file))
            [#(EXIT ,reason)
             (app-exception-handler reason)
             ((exit-handler) 1)]
            [,_ ((exit-handler))]))])))

  (define (int32? x) (and (or (fixnum? x) (bignum? x)) (<= #x-7FFFFFFF x #x7FFFFFFF)))
  (define (->exit-status exit-code)
    (cond
     [(int32? exit-code) exit-code]
     [(eq? exit-code (void)) 0]
     [else
      (console-event-handler (format "application shutdown due to (exit ~s)" exit-code))
      1]))
  (define quit
    (case-lambda
     [() (quit 0)]
     [(exit-code . _)
      (app:shutdown (->exit-status exit-code))
      (receive)]))

  (define swish-start
    (lambda args
      (let* ([argv (osi_get_argv)]
             [who (if (= 0 (vector-length argv)) "scheme" (vector-ref argv 0))])
        (command-line (cons who args))
        (command-line-arguments args)
        (with-exception-handler app-exception-handler
          (lambda ()
            (call-with-values run quit))))))

  (suppress-greeting #t)

  (scheme-start
   (lambda args
     (exit-handler quit)
     (scheme-start swish-start)
     (apply swish-start args)))
  )
