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

(library (swish watcher)
  (export
   watcher:shutdown-children
   watcher:start&link
   watcher:start-child
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish supervisor)
   )

  (define (watcher:start&link name)
    (supervisor:start&link name 'one-for-one 0 1 '()))

  (define (watcher:start-child watcher name shutdown thunk)
    (supervisor:start-child watcher
      `#(,name ,thunk watch-only ,shutdown worker)))

  (define (watcher:shutdown-children watcher)
    (for-each
     (lambda (x)
       (when (eq? (<child> restart-type x) 'watch-only)
         (supervisor:terminate-child watcher (<child> name x))
         (supervisor:delete-child watcher (<child> name x))))
     (supervisor:get-children watcher))
    (void))
  )
