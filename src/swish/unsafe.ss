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

;; This file provides macros that are for internal use only.
;;
;; We could pull these in via a library but there is currently no way to unload
;; a library. In general we can't provide these macros via internal.ss because
;; that breaks "#!eof mats" since they run after we evaluate boot.ss and poison
;; $import-internal.

;; Use with care. Build with UNSAFE_PRIMITIVES=no to disable.
(define-syntax declare-unsafe-primitives
  (if (equal? (getenv "UNSAFE_PRIMITIVES") "no")
      (syntax-rules () [(_ ...) (begin)])
      (syntax-rules ()
        [(_ prim ...)
         (begin
           (define-syntax prim (identifier-syntax ($primitive 3 prim)))
           ...)])))

;; Use with care. Unlike with-interrupts-disabled, no-interrupts is unsafe if
;; body forms can raise an exception.
(define-syntax no-interrupts
  (syntax-rules ()
    [(_ body ...)
     (let ([x (begin (disable-interrupts) body ...)])
       (enable-interrupts)
       x)]))
