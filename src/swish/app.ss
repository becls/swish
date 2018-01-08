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

(library (swish app)
  (export
   app:resume
   app:scheme-start
   app:shutdown
   app:start
   app:suspend
   init-main-sup
   )
  (import
   (main-sup-spec)
   (software-info)
   (swish application)
   (swish erlang)
   (swish io)
   (swish statistics)
   (swish supervisor)
   (except (chezscheme) define-record exit))

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

  (define (app:scheme-start . args)
    (cond
     [#f ;; TODO: (IsService)
      (console-event-handler `#(software-version ,software-version))
      (set-random-seed)
      (app:start)]
     [else
      (printf "\n~a Version ~a\n" software-product-name software-version)
      (flush-output-port)
      (set-random-seed)
      (hook-console-input)
      (app:start)
      (for-each load args)
      (new-cafe)])
    (receive))
  )
