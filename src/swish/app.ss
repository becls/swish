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
   app:resume
   app:shutdown
   app:start
   app:suspend
   init-main-sup
   swish-start
   )
  (import
   (chezscheme)
   (main-sup-spec)
   (swish application)
   (swish erlang)
   (swish io)
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
    (supervisor:start&link 'main-sup 'one-for-all 0 1 (main-sup-spec)))

  (define (set-random-seed)
    (random-seed (+ (remainder (erlang:now) (- (ash 1 32) 1)) 1)))

  (define-tuple <args> cmd suppress-greeting? filenames)

  (define (process-command-line args)
    (define init
      (<args> make
        [cmd 'repl]
        [suppress-greeting? #f]
        [filenames '()]))
    (let lp ([args args] [acc init])
      (match args
        [() acc]
        [("--help" . ,_)
         (<args> copy acc [cmd "--help"])]
        [("--version" . ,_)
         (<args> copy acc [cmd "--version"])]
        [("-q" . ,rest)
         (lp rest (<args> copy acc [cmd 'repl] [suppress-greeting? #t]))]
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
         (printf " --version   print version information\n")
         (printf " -q          suppress startup message\n")
         (printf " --          remaining arguments are files to load\n")
         (printf " file        execute file with remaining arguments\n")
         (values)]
        [`(<args> [cmd "--version"])
         (printf "~a\n" (swish-version))
         (values)]
        [`(<args> [cmd repl] ,suppress-greeting? ,filenames)
         (unless suppress-greeting?
           (printf "\n~a Version ~a\n" software-product-name software-version)
           (flush-output-port))
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
    (define (int32? x) (and (integer? x) (exact? x) (<= (integer-length x) 32)))
    (interaction-environment
     (copy-environment (environment '(scheme) '(swish imports)) #t))
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
