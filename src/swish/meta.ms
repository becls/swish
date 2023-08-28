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

(import (swish mat) (swish testing))

;; TODO remove if replace-source is accepted upstream
(mat replace-source ()
  (define a 1)
  (define b 2)
  (define no-src (datum->syntax #'a 'a))
  (define (source=? x y)
    (let ([x-ae (syntax->annotation x)]
          [y-ae (syntax->annotation y)])
      (if x-ae
          (and y-ae (equal? (annotation-source x-ae) (annotation-source y-ae)))
          (not y-ae))))
  (define (check-source src-template before after)
    ;; make sure replace-source did something if source was available
    ;; - different source (or no source) to start with
    (assert (or (not (source=? src-template before))
                (not (syntax->annotation before))))
    ;; - same source after
    (assert (source=? src-template after))
    ;; - has source if src-template has source
    (assert (if (syntax->annotation src-template)
                (syntax->annotation after)
                #t)))
  (define (check-id src-template x)
    (let ([after (replace-source src-template x)])
      (check-source src-template x after)
      (assert (free-identifier=? after x))
      (assert (bound-identifier=? after x))))
  (define (check-structured src-template x)
    (let ([after (replace-source src-template x)])
      (check-source src-template x after)
      ;; make sure identifiers within after are unaffected by replace-source
      (syntax-case after ()
        [(_ a-id b-id)
         (begin
           (assert (free-identifier=? #'a #'a-id))
           (assert (bound-identifier=? #'a #'a-id))
           (assert (free-identifier=? #'b #'b-id))
           (assert (bound-identifier=? #'b #'b-id)))])))
  ;; - template has source
  (assert (syntax->annotation #'a))
  ;;   - target is an identifier that already has source information
  (check-id #'a #'b)
  ;;   - target is an identifier with no source information
  (check-id #'a (datum->syntax #'a 'b))
  ;;   - target is a structured form that already has source information
  ;;     - target is a syntax object that has not been unwrapped
  (check-structured #'a #'(foo a b))
  ;;     - target is a list of syntax objects, manually constructed
  (check-structured #'a (list #'foo #'a #'b))
  ;;     - target is a list of syntax objects, built by (syntax x)
  (syntax-case #'(foo b a) ()
    [(_ y x) ;; rearrange for good measure
     (check-structured #'a #'(bar x y))])
  ;;   - target is a structured form with no source information
  (check-structured #'a (datum->syntax #'a '(foo a b)))
  ;; - template has no source
  (assert (not (syntax->annotation no-src)))
  ;;   - target is an identifier that already has source information
  (check-id no-src #'b)
  ;;   - target is an identifier with no source information
  (check-id no-src (datum->syntax #'a 'b))
  ;;   - target is a structured form that already has source information
  ;;     - target is a syntax object that has not been unwrapped
  (check-structured no-src #'(foo a b))
  ;;     - target is a list of syntax objects, manually constructed
  (check-structured no-src (list #'foo #'a #'b))
  ;;     - target is a list of syntax objects, built by (syntax x)
  (syntax-case #'(foo b a) ()
    [(_ y x) ;; rearrange for good measure
     (check-structured no-src #'(bar x y))])
  ;;   - target is a structured form with no source information
  (check-structured no-src (datum->syntax #'a '(foo a b)))

  )
