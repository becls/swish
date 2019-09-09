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

(import
 (chezscheme)
 (swish mat)
 )

(mat coerce ()
  (match-let*
   (["foo" (coerce "foo")]
    [0 (coerce 0)]
    [,n (guard (= n (most-positive-fixnum))) ; fixnum
      (coerce (most-positive-fixnum))]
    [,n (guard (= n (+ (most-positive-fixnum) 1))) ; small bignum
      (coerce (+ (most-positive-fixnum) 1))]
    [3.14159 (coerce 3.14159)]          ; flonum
    [#f (coerce #f)]
    [,n (guard (= n (expt 2 129)))      ; large real
      (coerce (expt 2 129))]
    [#vu8(1 2 3) (coerce '#vu8(1 2 3))]
    ["bar" (coerce 'bar)]
    ["gensym" (coerce (gensym "gensym"))]
    [,n (guard (and (integer? n) (> n 0)))
      (coerce (spawn (lambda () 'ok)))]
    ["Mon, 2 Feb 2015 01:02:03 +0000" (coerce (make-date 0 3 2 1 2 2 2015 0))]
    [#(error "Exception: condition without a stack.")
     (read
      (open-input-string
       (coerce (condition
                (make-error)
                (make-message-condition "condition without a stack")))))]
    [#(error "Exception in car: 12 is not a pair." ,stack)
     (guard (starts-with? stack "#<system continuation in"))
     (read
      (open-input-string
       (coerce
        (match (catch (car 12))
          [#(EXIT ,reason) reason]))))]
    ;; Other cases
    ["7.0+1.0i" (coerce 7.0+1.0i)]      ; complex
    ["#(<point> 1 2)"                   ; tuple
     (let ()
       (define-tuple <point> x y)
       (coerce (<point> make [x 1] [y 2])))]
    ["{\"bar\":[1,\"two\",{\"zip\":true}],\"foo\":123}"
     (coerce
      (json:make-object
       [foo 123]
       [bar (list 1 "two" (json:make-object [zip #t]))]))]
    ["#0=(1 . #0#)"
     (coerce '#0=(1 . #0#))]
    ["#(error \"Exception occurred with condition components:\\n  0. &irritants: #0=(1 . #0#).\")"
     (coerce (make-irritants-condition '#1=(1 . #1#)))])
   'ok))
