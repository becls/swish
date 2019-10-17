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

(library (swish queue)
  (export
   queue:add
   queue:add-front
   queue:drop
   queue:empty
   queue:empty?
   queue:get
   )
  (import
   (chezscheme)
   (swish erlang)
   )

  (define queue:empty '(() . ()))

  (define (queue:empty? q) (eq? q queue:empty))

  (define (queue:add x q)
    (match q
      [(,(in <= (,_)) . ()) `((,x) . ,in)]
      [(,in . ,out) `((,x . ,in) . ,out)]))

  (define (queue:add-front x q)
    (match q
      [(() . ,(out <= (,_))) `(,out . (,x))]
      [(,in . ,out) `(,in . (,x . ,out))]))

  (define (queue:get q)
    (when (queue:empty? q) (throw 'empty))
    (match q
      [(,_ . (,h . ,_)) h]
      [((,h) . ()) h]))

  (define (queue:drop q)
    (when (queue:empty? q) (throw 'empty))
    (match q
      [((,_) . ()) (profile-me) queue:empty]
      [(,r . (,_)) (profile-me) (r2f r)]
      [(,r . (,_ . ,f)) `(,r . ,f)]))

  (define (r2f ls)
    (match ls
      [() (profile-me) queue:empty]
      [,(r <= (,_)) `(() . ,r)]
      [(,x ,y) `((,x) . (,y))]
      [(,x ,y . ,r) `((,x ,y) . ,(reverse r))]))
  )
