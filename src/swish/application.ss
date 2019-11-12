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

(library (swish application)
  (export
   application:shutdown
   application:start
   )
  (import
   (chezscheme)
   (swish app-core)
   (swish erlang)
   (swish gen-server)
   (swish osi)
   )

  (define (init starter)
    (process-trap-exit #t)
    (match (starter)
      [,(r <= #(ok ,process)) (link process) r]
      [#(error ,reason) `#(stop ,reason)]))

  (define (terminate reason process)
    (when process
      (monitor process)
      (kill process 'shutdown)
      (receive
       [`(DOWN ,_ ,@process ,_) 'ok]))
    ($exit-process))

  (define (handle-call msg from process) (match msg))

  (define (handle-cast msg process) (match msg))

  (define (handle-info msg process)
    (match msg
      [`(EXIT ,p ,reason)
       `#(stop ,reason ,(and (not (eq? p process)) process))]))

  (define (application:start starter)
    (match (gen-server:start 'application starter)
      [#(ok ,_) 'ok]
      [#(error ,reason)
       (console-event-handler `#(application-start-failed ,reason))
       (exit 1)]))

  )
