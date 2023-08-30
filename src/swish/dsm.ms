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

(import
 (swish dsm)
 (swish erlang)
 (swish mat)
 (swish testing))

(mat dsm-define ()
  ;; okay to have more than one syntactic monad in scope
  (define-syntactic-monad M a b c)
  (define-syntactic-monad G d e)
  (M define (f0) (list 'f0 a b c))
  (M define (f1 x) (list 'f1 a b c x))
  (G define (g0) (vector 'g0 d e))
  (G define (g2 a b) (vector 'g2 d e a b))
  (M define (f-1 . more) (list* 'f-1 a b c more))
  (G define (g-2 x . more) (vector 'g-2 d e x more))
  (define (circumvent-compile-time-warning f k)
    ;; we want to check the run-time exception, not the compile-time warning
    ;; issued when we try to load the mat
    (let ([loc k]) ;; assign to make cp0 give up
      (set! loc f)
      (k loc)))
  (define-syntax wrong-argument-count
    (syntax-rules ()
      [(_ (proc arg ...))
       (match-let*
        ([`(catch ,reason)
          (guard (incorrect-argument-count? reason proc))
          (try
           (circumvent-compile-time-warning proc
             (lambda (f) (f arg ...))))])
        'ok)]))
  ;; try fixed args
  (match-let*
   ([(f0 x y z) (f0 'x 'y 'z)]
    [(f0 3 1 2) (f0 3 1 2)]
    [(f1 p d q "9") (f1 'p 'd 'q "9")]
    [(f1 4 3 2 1) (f1 4 3 2 1)]
    [#(g0 2 4) (g0 2 4)]
    [#(g0 a b) (g0 'a 'b)]
    [#(g2 m N 10 4) (g2 'm 'N 10 4)]
    [#(g2 2 4 6 8) (g2 2 4 6 8)])
   (wrong-argument-count (f0 'a 'b))
   (wrong-argument-count (f0 'a 'b 'c 'D))
   (wrong-argument-count (f1 'a 'b 'c 'D 'E))
   (wrong-argument-count (g0 'd))
   (wrong-argument-count (g0 'd 'e 'F))
   (wrong-argument-count (g2 'd 'e 'a))
   (wrong-argument-count (g2 'd 'e 'a 'b 'c))
   'ok)
  ;; try rest args
  (match-let*
   ([(f-1 "a" "b" "c" 1 2 3) (f-1 "a" "b" "c" 1 2 3)]
    [(f-1 "a1" "b2" "c3") (f-1 "a1" "b2" "c3")]
    [#(g-2 "d1" "e1" "required x" ()) (g-2 "d1" "e1" "required x")]
    [#(g-2 "d1" "e1" "x" (a b)) (g-2 "d1" "e1" "x" 'a 'b)])
   (wrong-argument-count (f-1 'a 'b))
   (wrong-argument-count (g-2 'a 'b))
   'ok))

(mat dsm-lambda ()
  (define-syntactic-monad A b c)
  (define-syntactic-monad D e f g)
  (define a0 (A lambda () (list 'a0 b c)))
  (define a1 (A lambda (x) (list 'a1 b c x)))
  (define d
    (D case-lambda
      [() (vector 'd0 e f g)]
      [(x) (vector 'd1 e f g x)]
      [(x y) (vector 'd2 e f g x y)]
      [(x y . more) (vector 'dmore e f g x y more)]))
  (match-let*
   ([(a0 1 2) (a0 1 2)]
    [(a0 c b) (a0 'c 'b)]
    [(a1 3 4 6) (a1 3 4 6)]
    [(a1 "a" "b" "c") (a1 "a" "b" "c")]
    [#(d0 3 4 5) (d 3 4 5)]
    [#(d1 4 5 6 extra) (d 4 5 6 'extra)]
    [#(d2 5 6 7 even more) (d 5 6 7 'even 'more)]
    [#(dmore 1 2 3 A B (c d e)) (d 1 2 3 'A 'B 'c 'd 'e)])
   'ok))

(mat dsm-let ()
  (define-syntactic-monad P sym num char other)
  (define (part ls)
    (P let rip ([sym '()] [num '()] [char '()] [other '()]) ([ls ls] [i 0])
      (if (null? ls)
          (vector (reverse sym) (reverse num) (reverse char) (reverse other))
          (let ([x (car ls)] [rest (cdr ls)] [next-i (+ i 1)])
            (cond
             [(symbol? x) (P rip ([sym (cons (list i x) sym)]) rest next-i)]
             [(number? x) (P rip ([num (cons (list i x) num)]) rest next-i)]
             [(char? x) (P rip ([char (cons (list i x) char)]) rest next-i)]
             [else (P rip ([other (cons (list i x) other)]) rest next-i)])))))
  (define (no-shadow n)
    (P let lp ([sym 'a] [num 'b] [char 'c] [other 'd]) ([i 0] [ls '()])
      (cond
       [(= i n) (reverse ls)]
       ;; cons should see current value of sym, num, etc.
       [(= (mod i 4) 0) (P lp ([sym 'new-sym]) (+ i 1) (cons sym ls))]
       [(= (mod i 4) 1) (P lp ([num 'new-num]) (+ i 1) (cons num ls))]
       [(= (mod i 4) 2) (P lp ([char 'new-char]) (+ i 1) (cons char ls))]
       [(= (mod i 4) 3) (P lp ([other 'new-other]) (+ i 1) (cons other ls))])))
  (match-let*
   ([#(() () () ()) (part '())]
    [#(((0 a) (3 b))
       ((1 123) (5 -9))
       ((4 #\a) (6 #\z))
       ((2 #f) (7 #t) (8 "str")))
     (part '(a 123 #f b #\a -9 #\z #t "str"))]
    [(a b c d new-sym new-num new-char new-other) (no-shadow 8)])
   'ok))

(mat dsm-call ()
  (define-syntactic-monad FN a b k)
  (define (f a b k x y)
    (k a b x y k))
  (define (g a b k) (+ a b k))
  (match-let*
   ([(1 2 3 4 ,@list) (f 1 2 list 3 4)]
    [(1 2 3 4 ,@list)
     (let ([a 1] [b 2] [k list])
       (FN f () 3 4))]
    [(4 3 ,@vector 2 1)
     (let ([a 4] [b 3] [k list])
       ;; rebinding monad variable doesn't shadow rator: we call list not
       ;; vector, i.e., current value of k not next
       (FN k ([k vector]) 2 1))]
    [(10 9 1 2 ,@list)
     (let ([a 1] [b 2] [k list])
       ;; rebinding monad variable doesn't shadow rands: we see original values
       ;; of a and b where we pass them in as x and y
       (FN f ([b 9] [a 10]) a b))]
    ;; coverage:
    [11 (let ([a 1] [b 7] [k 3]) (FN g))])
   'ok))

(mat dsm-syntax ()
  (define-syntax assert-syntax-error
    (syntax-rules ()
      [(_ message input ...)
       (match-let*
        ([`(catch ,reason)
          (try (expand
                (read-bytevector "buggy"
                  (string->utf8
                   (with-output-to-string
                    (lambda ()
                      (pretty-print
                       '(let ()
                          (define-syntactic-monad M a b c)
                          input ...
                          123))))))))]
         [,msg message]
         ;; make sure source info does not point to swish/dsm.ss
         [,expected (string-append (pregexp-quote msg) " (at|near).* of buggy\\.")]
         [,actual (exit-reason->english reason)])
        (unless (pregexp-match expected actual)
          (printf "expected: ~a at/near ... of buggy.\n" msg)
          (printf "actual:   ~a\n" actual)
          (syntax-error #'message "failed to generate expected error"))
        'ok)]))
  ;; clash between monad var and extra formal
  (assert-syntax-error
   "Exception: invalid parameter list in (lambda (a b c a) a)"
   (M define (bad a) a))
  (assert-syntax-error
   "Exception: invalid parameter list in (lambda (a b c b) a)"
   (M define (bad b) a))
  (assert-syntax-error
   "Exception: invalid parameter list in (lambda (a b c c) a)"
   (M define (bad c) a))
  ;; also caught by trace-define
  (assert-syntax-error
   "Exception: invalid parameter list in (lambda (a b c a) a)"
   (M trace-define (bad a) a))
  ;; variable z is not in monad M
  (assert-syntax-error
   "Exception: in syntactic monad M, unrecognized identifier z"
   (M f ([z 3])))
  (assert-syntax-error
   ;; not an ideal error message
   "Exception: duplicate bound variable a in (let ((p f) (a 3) (a 9)) (p a b c))"
   (M f ([a 3] [a 9])))
  (assert-syntax-error
   ;; not an ideal error message
   "Exception: invalid parameter list in (lambda (a b c a) 3)"
   (M lambda (a) 3))
  )

(mat dsm-trace ()
  (define-syntactic-monad G x y)
  (define os (open-output-string))
  (define f1 (G trace-lambda list3 (z) (list x y z)))
  (define f23
    (G trace-case-lambda jump
      [(a) (G f23 () a #f)]
      [(a b) (if b (+ a x y) (* a x y))]))
  (define (g ls)
    (define x 2)
    (G trace-let F ([y 5]) ([ls ls] [sum 0])
      (cond
       [(null? ls) (+ y sum)]
       [(> x 0) (G F ([x (- x 1)]) ls (+ x sum))]
       [else (G F ([y (car ls)]) (cdr ls) (+ y sum))])))
  (define-syntax expect
    (syntax-rules ()
      [(_ expr str ...)
       (match-let*
        ([,_ (parameterize ([trace-output-port os]) expr)]
         [(str ... "") (split (get-output-string os) #\newline)])
        'ok)]))
  (expect (G f1 ([y 'not] [x 'iting]) 'axis)
    "|(list3 iting not axis)"
    "|(iting not axis)")
  (expect (G f23 ([x 2] [y 3]) 4 #t)
    "|(jump 2 3 4 #t)"
    "|9")
  (expect (G f23 ([x 2] [y 3]) 4)
    "|(jump 2 3 4)"
    "|(jump 2 3 4 #f)"
    "|24")
  (expect (g '(1 2 3))
    "|(F 2 5 (1 2 3) 0)"
    "|(F 1 5 (1 2 3) 2)"
    "|(F 0 5 (1 2 3) 3)"
    "|(F 0 1 (2 3) 8)"
    "|(F 0 2 (3) 9)"
    "|(F 0 3 () 11)"
    "|14"))
