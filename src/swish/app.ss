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
   $init-main-sup
   app-sup-spec
   app:shutdown
   app:start
   make-swish-sup-spec
   swish-start
   )
  (import
   (chezscheme)
   (swish app-core)
   (swish app-io)
   (swish application)
   (swish cli)
   (swish erlang)
   (swish errors)
   (swish event-mgr)
   (swish gatekeeper)
   (swish io)
   (swish log-db)
   (swish meta)
   (swish osi)
   (swish pregexp)
   (swish software-info)
   (swish statistics)
   (swish supervisor)
   )

  (define (app:start) (application:start $init-main-sup))

  (alias app:shutdown application:shutdown)

  (define ($init-main-sup)
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
                      [`(EXIT ,_ ,_) (event-mgr:unregister)])))))
        permanent 1000 worker)
      #(log-db:setup ,(lambda () (log-db:setup loggers))
         temporary 1000 worker)
      #(statistics ,statistics:start&link
         permanent 1000 worker)
      #(gatekeeper ,gatekeeper:start&link
         permanent 1000 worker)))

  (define app-sup-spec
    (make-parameter
     (make-swish-sup-spec (list swish-event-logger))
     (lambda (x)
       (let ([reason (supervisor:validate-start-specs x)])
         (when reason (throw reason)))
       x)))

  (define cli
    (cli-specs
     default-help
     [verbose --verbose bool "trace boot file search"]
     [version --version bool "print version information"]
     [quiet -q bool "suppress startup message and prompt string"]
     [args -- (list . "arg") "remaining arguments are files to load"]
     [files (list . "file") "execute file with remaining arguments"]))

  (define (try-import)
    ;; Try to import the available swish libraries, since that is convenient for
    ;; interactive use and for quick one-off scripts.
    ;;
    ;; We guard against the possibility that the libraries are present but not
    ;; visible, which can happen if a stand-alone program is compiled without
    ;; --libs-visible and invokes the swish-start exported by this library.
    ;;
    ;; Since a stand-alone program might include a subset of the swish
    ;; libraries, we consult library-list rather than trying to directly import
    ;; (swish imports), which may not be available at run time, as it has no
    ;; exports of its own.
    (for-each
     (lambda (library)
       (match library
         [(swish . ,_) (catch (eval `(import ,library)))]
         [,_ #f]))
     (library-list)))

  (define (print-banner revision)
    (let-values ([(name version)
                  (cond
                   [(software-product-name) =>
                    (lambda (name)
                      (values name (software-version)))]
                   [else
                    (values
                     (software-product-name 'swish)
                     (software-version 'swish))])])
      (printf "~a~@[ Version ~a~]~@[ (~a)~]\n" name version revision)))

  (define (run)
    (let* ([opt (parse-command-line-arguments cli)]
           [files (or (opt 'files) '())])
      (cond
       [(opt 'help)
        (display-help (path-last (osi_get_executable_path)) cli (opt))
        (values)]
       [(opt 'version)
        (print-banner (software-revision))
        (values)]
       [(null? files)                   ; repl
        (let ([filenames (or (opt 'args) '())])
          (unless (opt 'quiet)
            (print-banner #f)
            (flush-output-port))
          (try-import)
          (parameterize ([waiter-prompt-string (if (opt 'quiet) "" ">")]
                         [repl-level (+ (repl-level) 1)])
            (define (trap-CTRL-C handler)
              (meta-cond
               [windows?
                (signal-handler SIGBREAK handler)
                (signal-handler SIGINT handler)]
               [else
                (signal-handler SIGINT handler)]))
            (when (interactive?)
              (trap-CTRL-C
               (let ([p self])
                 (lambda (n) (keyboard-interrupt p)))))
            (for-each load filenames)
            (new-cafe)))]
       [else                            ; script
        (let ([script-file (car files)]
              [cmdline (command-line-arguments)])
          (parameterize ([command-line cmdline]
                         [command-line-arguments (cdr cmdline)]
                         [app:name script-file]
                         [app:path script-file]
                         [app:config #f])
            (try-import)
            ;; use exit handler installed by the script, if any
            (match (try (load script-file))
              [`(catch ,_ ,e)
               (app-exception-handler e)
               (exit 1)]
              [,_ (void)])))])))

  (define (swish-start . args)
    ($swish-start #f args run)))
