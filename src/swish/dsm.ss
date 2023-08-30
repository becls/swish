;;; Copyright (c) 1998-2016 R. Kent Dybvig and Oscar Waddell
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; authors: R. Kent Dybvig and Oscar Waddell

(library (swish dsm)
  (export
   define-syntactic-monad
   )
  (import
   (chezscheme)
   (swish meta)
   )
  (define-syntax define-syntactic-monad
    (syntax-rules ()
      [(_ name formal ...)
       (andmap identifier? #'(name formal ...))
       (define-syntax name
         (lambda (x)
           (define (guard-for-coverage) #t)
           (syntax-case x (case-lambda define lambda let
                            trace-case-lambda trace-define trace-lambda trace-let)
             [(key define (proc-name . more-formals) . body)
              (guard-for-coverage)
              #'(define proc-name (key lambda more-formals  . body))]
             [(key let proc-name ((x e) (... ...)) ([fml arg] (... ...)) . body)
              (guard-for-coverage)
              (with-implicit (key formal ...)
                #'(key (rec proc-name (key lambda (fml (... ...)) . body))
                    ((x e) (... ...)) arg (... ...)))]
             [(key lambda more-formals . body)
              (guard-for-coverage)
              (with-implicit (key lambda formal ...) ;; include lambda here to report correct source for errors
                #'(lambda (formal ... . more-formals) . body))]
             [(key case-lambda (more-formals . body) (... ...))
              (guard-for-coverage)
              (with-implicit (key case-lambda formal ...) ;; see lambda case above
                #'(case-lambda [(formal ... . more-formals) . body] (... ...)))]
             [(key proc ((x e) (... ...)) arg (... ...))
              (andmap identifier? #'(x (... ...)))
              (with-implicit (key formal ...)
                (for-each
                 (lambda (x)
                   (unless (let mem ((ls #'(formal ...)))
                             (and (not (null? ls))
                                  (or (free-identifier=? x (car ls))
                                      (mem (cdr ls)))))
                     (syntax-error x (format "in syntactic monad ~s, unrecognized identifier" 'name))))
                 #'(x (... ...)))
                (with-syntax ([(t (... ...)) (generate-temporaries #'(arg (... ...)))])
                  #'(let ((p proc) (x e) (... ...) (t arg) (... ...))
                      (p formal ... t (... ...)))))]
             [(key trace-define (proc-name . more-formals) . body)
              (guard-for-coverage)
              (with-implicit (key formal ...)
                #'(define proc-name (key trace-lambda proc-name more-formals . body)))]
             [(key trace-let proc-name ((x e) (... ...)) ([fml arg] (... ...)) . body)
              (guard-for-coverage)
              (with-implicit (key formal ...)
                #'(key (rec proc-name (key trace-lambda proc-name (fml (... ...)) . body))
                    ((x e) (... ...)) arg (... ...)))]
             [(key trace-lambda who more-formals . body)
              (guard-for-coverage)
              (with-implicit (key trace-lambda formal ...) ;; see lambda case above
                #'(trace-lambda who (formal ... . more-formals) . body))]
             [(key trace-case-lambda who (more-formals . body) (... ...))
              (guard-for-coverage)
              (with-implicit (key trace-case-lambda formal ...) ;; see lambda case above
                #'(trace-case-lambda who [(formal ... . more-formals) . body] (... ...)))]
             [(key proc)
              (guard-for-coverage)
              #'(key proc ())])))]))
  )
