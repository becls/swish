;;; Copyright 2017 Beckman Coulter, Inc.
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
 (swish erlang)
 (swish mat)
 (swish queue)
 )

(mat q ()
  (match-let*
   ([#t (queue:empty? queue:empty)]
    [#(EXIT empty) (catch (queue:get queue:empty))]
    [#(EXIT empty) (catch (queue:drop queue:empty))]
    [ok (test-adding-and-dropping queue:add #f)]
    [ok (test-adding-and-dropping queue:add-front #t)])
   'ok))

(define (test-adding-and-dropping adder reverse?)
  (define (add-list q ls)
    (fold-left
     (lambda (q x) (adder x q))
     q
     (if reverse? (reverse ls) ls)))
  (define (drop-list q ls)
    (fold-left
     (lambda (q x)
       (match-let* ([,@x (queue:get q)])
         (queue:drop q)))
     q
     ls))
  (match-let*
   ([,q (add-list queue:empty '(1))]
    [#f (queue:empty? q)]
    [,q (drop-list q '(1))]
    [#t (queue:empty? q)]
    [,q (add-list q '(1 2 3 4 5 6))]
    [1 (queue:get q)]
    [1 (queue:get q)]
    [,q (drop-list q '(1 2 3 4 5 6))]
    [#t (queue:empty? q)])
   'ok))
