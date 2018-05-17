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
   app-sup-spec
   app:resume
   app:shutdown
   app:start
   app:suspend
   init-main-sup
   make-swish-sup-spec
   swish-start
   )
  (import
   (chezscheme)
   (swish application)
   (swish erlang)
   (swish event-mgr)
   (swish gatekeeper)
   (swish http)
   (swish io)
   (swish log-db)
   (swish osi)
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

  (define-tuple <args> cmd quiet? filenames)

  (define (process-command-line args)
    (define init
      (<args> make
        [cmd 'repl]
        [quiet? #f]
        [filenames '()]))
    (let lp ([args args] [acc init])
      (match args
        [() acc]
        [("--help" . ,_)
         (<args> copy acc [cmd "--help"])]
        [("--version" . ,_)
         (<args> copy acc [cmd "--version"])]
        [("--verbose" . ,rest)
         ;; --verbose must be handled by run.c
         (lp rest acc)]
        [("-q" . ,rest)
         (lp rest (<args> copy acc [cmd 'repl] [quiet? #t]))]
        [("--" . ,filenames)
         (<args> copy acc [cmd 'repl] [filenames filenames])]
        [(,cmd . ,filenames)
         (<args> copy acc [cmd cmd] [filenames filenames])])))

  (define (default-swish-start cmdline)
    (define (run)
      (match (process-command-line cmdline)
        [`(<args> [cmd "--help"])
         (printf "Usage: ~a [option] ... [ file | -- ] [arg] ...\n"
           (path-last (osi_get_executable_path)))
         (printf "Options and arguments:\n")
         (printf " --help      print this help message\n")
         (printf " --verbose   trace boot file search\n")
         (printf " --version   print version information\n")
         (printf " -q          suppress startup message and prompt string\n")
         (printf " --          remaining arguments are files to load\n")
         (printf " file        execute file with remaining arguments\n")
         (values)]
        [`(<args> [cmd "--version"])
         (printf "~a\n" (swish-version))
         (values)]
        [`(<args> [cmd repl] ,quiet? ,filenames)
         (cond
          [quiet?
           (waiter-prompt-string "")]
          [else
           (printf "\n~a Version ~a\n" software-product-name software-version)
           (flush-output-port)])
         (hook-console-input)
         (set-random-seed)
         (for-each load filenames)
         (new-cafe)]
        [`(<args> [cmd ,script-file])
         (command-line cmdline)
         (command-line-arguments (cdr cmdline))
         (set-random-seed)
         (call/cc
          (lambda (return)
            (exit-handler return)
            (load script-file)
            ((exit-handler))))]
        [#f ;; TODO: (IsService) and I do mean TODO
         (console-event-handler `#(software-version ,software-version))
         (app:start)]))
    (define (int32? x) (and (or (fixnum? x) (bignum? x)) (<= #x-7FFFFFFF x #x7FFFFFFF)))
    (eval '(import (swish imports)))
    (call-with-values run
      (case-lambda
       [() (app:shutdown)]
       [(exit-code . _)
        (app:shutdown
         (cond
          [(int32? exit-code) exit-code]
          [(eq? exit-code (void)) 0]
          [else 1]))]))
    (receive))

  (define swish-start
    (make-parameter default-swish-start
      (lambda (x)
        (unless (procedure? x)
          (bad-arg 'swish-start x))
        x)))

  (suppress-greeting #t)

  (scheme-start
   (lambda args
     ((swish-start) args)))

  )
