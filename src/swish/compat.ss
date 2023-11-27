;;; Copyright 2023 Beckman Coulter, Inc.
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
(library (swish compat)
  (export
   no-inline
   )
  (import (scheme))

  (meta-cond
   [(top-level-bound? 'keep-live) (begin)]
   [else
    (define (keep-live x) (#%$keep-live x))
    (export keep-live)])

  (meta-cond
   [(guard (c [else #f]) (eval '#%$app/no-inline) #t)
    (define-syntax no-inline
      (syntax-rules ()
        [(_ proc arg ...)
         (#%$app/no-inline proc arg ...)]))]
   [else
    ;; Before #%$app/no-inline there was no convenient mechanism to prevent
    ;; inlining without further degrading performance, but we didn't need a
    ;; way to enforce this until tests started breaking in Chez Scheme 10.x,
    ;; hence expand to simple application here.
    (define-syntax no-inline
      (syntax-rules ()
        [(_ proc arg ...) (proc arg ...)]))])

  )
