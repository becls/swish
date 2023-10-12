;;; Copyright 2021 Beckman Coulter, Inc.
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
(import
 (chezscheme)
 (swish erlang)
 (swish mat)
 (swish options)
 (swish testing)
 )

(mat syntax ()
  (assert-syntax-error
   (define-options my:options (required signature))
   "invalid syntax")
  (assert-syntax-error
   (define-options my:options (optional signature))
   "invalid syntax")
  (assert-syntax-error
   (define-options (my:options) (required))
   "invalid syntax")
  (assert-syntax-error
   (define-options my:options (required [(my-key)]))
   "invalid field")
  (assert-syntax-error
   (define-options my:options (optional [(my-key)]))
   "invalid field")
  (assert-syntax-error
   (define-options my:options (optional [copy]))
   "invalid field")
  (assert-syntax-error
   (define-options my:options (required [a]) (optional [a]))
   "duplicate field")
  (assert-syntax-error
   (define-options my:options (required [a] [a]))
   "duplicate field")
  (assert-syntax-error
   (define-options my:options (optional [a] [a]))
   "duplicate field")
  ;; no default clause for required options
  (assert-syntax-error
   (define-options my:options (required [xyz (default 9)]))
   "invalid clause")
  (assert-syntax-error
   (define-options my:options (required [xyz (unknown 9)]))
   "invalid clause")
  (assert-syntax-error
   (define-options my:options (optional [xyz (unknown 9)]))
   "invalid clause")
  (assert-syntax-error
   (define-options my:options (optional [xyz (default 1 2 3)]))
   "invalid syntax")
  (assert-syntax-error
   (define-options my:options (optional [xyz (filter 1 2 3)]))
   "invalid syntax")
  (assert-syntax-error
   (define-options my:options (optional [xyz (must-be)]))
   "invalid syntax")
  (assert-syntax-error
   (define-options my:options (optional [xyz (filter)]))
   "invalid syntax")
  ;; must specify required options
  (assert-syntax-error
   (let ()
     (define-options my:options (required [key1]))
     (my:options))
   "missing key1 clause in")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [key1] [key2]) (optional [key3]))
     (my:options [key1 1] [key3 3]))
   "missing key2 clause in")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options [x 1] [x 2]))
   "duplicate clause")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options [x]))
   "invalid syntax")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options [x 1 2 3]))
   "invalid syntax")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options [y not]))
   "invalid clause")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options y))
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options y 123))
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options copy 123 [y not]))
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options copy* 123 [y not]))
   "unknown field")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options copy 123 [x 4 5 6]))
   "invalid syntax")
  (assert-syntax-error
   (let ()
     (define-options my:options (required [x]))
     (my:options copy* 123 [x 4 5 6]))
   "invalid syntax")
  ;; optional means optional
  (match-let*
   ([#t
     (let ()
       (define-options my:options (optional [key1]))
       (my:options is? (my:options)))])
   'ok)
  ;; no namespace pollution
  (assert
   (eval ;; delay expand-time error, if any, til now
    '(let ()
       (define-options foo (optional [x]))
       (define-options bar (optional [y]))
       (define make-record)
       (define make-<foo>)
       (define make-<bar>)
       (match-let*
        ([`(<foo> [x 1]) (foo [x 1])]
         [`(<bar> [y 2]) (bar [y 2])])
        'ok))))
  ;; field accessor syntax error
  (assert-syntax-error
   (let ()
     (define-options foo (optional [x]))
     (foo x 1 2))
   "invalid syntax")

  )

(mat operation ()
  (let ()
    (define-options my:options
      (required [any])
      (optional [num (must-be number?) (default 0)]))
    (define-options other:opts
      (optional [a] [b] [c]))
    (match-let*
     ([,x (my:options [any 3])]
      [,y (my:options [any "seven"] [num 11])]
      ;; direct field accessors
      [3 (my:options any x)]
      ["seven" (my:options any y)]
      [0 (my:options num x)]
      [11 (my:options num y)]
      ;; curried field accessors
      [(3 "seven") (map (my:options any) (list x y))]
      [(0 11) (map (my:options num) (list x y))]
      [(4 101)
       (map (my:options num)
         (list
          (my:options [any #f] [num 4])
          (my:options [any #t] [num 101])))]
      [`(<my:options> [any #(outer)])
       (let ([any 'outer])
         (my:options copy x [any (vector any)]))]
      [`(<my:options> [any #(3)])
       (let ([any 'outer])
         (my:options copy* x [any (vector any)]))]
      [`(<my:options> [any #(3)])
       (let ([any 'outer])
         (my:options copy* x [any (vector any)]))]
      [`(<my:options> [any 0] [num 3])
       (let ([any 'outer] [num 'ber])
         ;; copy* clause bindings shadow the local bindings
         (my:options copy* x [any num] [num any]))]
      [`(<my:options> [any ber] [num 0])
       (let ([any 14] [num 'ber])
         ;; we see local binding for num since no clause for num here
         (my:options copy* x [any num]))]
      [`(<my:options> [any 3] [num 14])
       (let ([any 14] [num 'ber])
         ;; we see local binding for any since no clause for any here
         (my:options copy* x [num any]))]
      [`(<my:options> [any 14] [num 15])
       (let ([any 14] [num 15])
         (my:options copy x [any any] [num num]))]
      ;; option types are distinct
      [,other (other:opts)]
      [#f (my:options is? other)]
      [#t (other:opts is? other)]
      [#t (my:options is? x)]
      [#f (other:opts is? x)]
      ;; field order does not matter
      [`(<other:opts> [a 1] [b 2] [c 3])
       (other:opts [a 1] [b 2] [c 3])]
      [`(<other:opts> [a 1] [b 2] [c 3])
       (other:opts [b 2] [a 1] [c 3])]
      [`(<other:opts> [a 1] [b 2] [c 3])
       (other:opts [b 2] [c 3] [a 1])]
      [`(<other:opts> [a 1] [b 2] [c 3])
       (other:opts [c 3] [b 2] [a 1])])
     'ok))
  (let ()
    (define-options alt:opts
      (optional
       [int (default 0) (must-be integer?)]
       [->ls (filter list)]
       [str->sym
        (default "none")
        (must-be string?)
        (filter string->symbol)]
       [multi
        (default 0)
        (must-be fixnum? even? fxnonnegative?)
        (filter (lambda (n) (* n n)))]))
    (match-let*
     ([,x (alt:opts)]
      ;; single predicate must-be
      [`(catch #(bad-arg int "zero")) (try (alt:opts [int "zero"]))]
      ;; multi-predicate must-be
      [`(catch #(bad-arg multi -8)) (try (alt:opts [multi -8]))]
      ;; apply must-be before filter (no error from string->symbol)
      [`(catch #(bad-arg str->sym 7)) (try (alt:opts [str->sym 7]))]
      ;; apply filters when making options
      [`(<alt:opts> [int 3] [->ls (wrapped)] [str->sym ok] [multi 16])
       (alt:opts [int 3] [->ls 'wrapped] [str->sym "ok"] [multi 4])]
      ;; reminder of filtered result to explain catch that follows
      [none (alt:opts str->sym x)]
      ;; apply must-be on copy
      [`(catch #(bad-arg str->sym none))
       (try (alt:opts copy x))]
      ;; apply filters on copy
      [`(<alt:opts> [int 0] [->ls ((#f))] [str->sym okay] [multi 4])
       (alt:opts copy x [str->sym "okay"] [multi 2])]
      ;; apply must-be on copy*
      [`(catch #(bad-arg str->sym none))
       (try (alt:opts copy* x))]
      ;; apply filter on copy*
      [`(<alt:opts> [int 0] [->ls ((#f))] [str->sym okay] [multi 64])
       (alt:opts copy* x [str->sym "okay"] [multi 8])])
     'ok))
  (let ()
    ;; default is #f if not specified
    ;; no filter or predicate if not specified
    (define-options new:opts (optional [x]))
    (match (new:opts)
      [`(<new:opts> [x #f]) 'ok]
      [`(<new:opts> [x list?]) (new:opts [x list?])]
      [`(<new:opts> [x #!eof]) (new:opts [x #!eof])])
    'ok)
  (let ()
    ;; default expression evaluated iff n is not supplied
    (define count 0)
    (define (next!) (set! count (+ count 1)) count)
    (define-options new:opts
      (optional
       [n (default (next!))]))
    (match-let*
     ([0 count]
      [,x (new:opts [n 1000])]
      [0 count]
      [,y (new:opts)]
      [1 count]
      [,z (new:opts)]
      [2 count]
      [(1000 1 2) (map (new:opts n) (list x y z))]
      ;; copy and copy* supply value, so default not invoked
      [,a (new:opts copy x)]
      [2 count]
      [,a (new:opts copy* x)]
      [2 count])
     'ok))
  ;; check that (foo x) and (foo x obj) can see <foo>-x field
  ;; accessor at top-level
  (match-let*
   ([,env (environment '(scheme) '(swish imports))]
    [ok
     (eval
      '(begin
         (define-options foo (optional [x]))
         (define y (foo [x 1]))
         (and (= 1 ((foo x) y) (foo x y)) 'ok))
      (copy-environment env #t))]
    [ok
     (eval
      '(let ()
         (define-options foo (optional [x]))
         (define y (foo [x 1]))
         (and (= 1 ((foo x) y) (foo x y)) 'ok))
      (copy-environment env #f))]
    [ok
     (eval
      '(begin
         (define-options foo (optional [x]))
         (define x (foo [x 1]))
         (and (= 1 ((foo x) x) (foo x x)) 'ok))
      (copy-environment env #t))]
    [ok
     (eval
      '(let ()
         (define-options foo (optional [x]))
         (define x (foo [x 1]))
         (and (= 1 ((foo x) x) (foo x x)) 'ok))
      (copy-environment env #f))])
   'ok)
  ;; safely reject bad input
  (let ()
    (define-options foo (optional [x]))
    (define-options bar (optional [x]))
    (match-let*
     ([`(catch #(bad-match 3 ,_)) (try (foo x 3))]
      [,ba (bar [x 'elephant])]
      [`(catch #(bad-match ,@ba ,_)) (try (foo x ba))]
      [,z (foo [x 11])]
      [`(catch #(bad-match ,@z ,_)) (try (bar x z))]
      [elephant (bar x ba)]
      [11 (foo x z)])
     'ok))
  ;; fields do not shadow default expression; default expressions are scoped
  ;; as let* of fields in the order they are listed in the define-options
  (let ()
    (define x 11)
    (define-options foo (required [x]) (optional [y (default x)]))
    (define-options bar (optional [spawn (default spawn)]))
    (match-let*
     ([,@spawn (bar spawn (bar))]
      [7 (bar spawn (bar [spawn 7]))]
      [,x 'local]
      [,opt (foo [x 'act])]
      [act (foo y opt)]
      [act (foo x opt)]
      [,opt (foo [x 123] [y 'not])]
      [not (foo y opt)]
      [123 (foo x opt)]
      )
     'ok))
  ;; must-be has access to prior bindings
  (let ()
    (define x 11)
    (define-options foo
      (required [x])
      (optional [y (default 0) (must-be fixnum? (lambda (y) (> y x)))]))
    (match-let*
     ([,a (foo [x -1])]
      [-1 (foo x a)]
      [0 (foo y a)]
      [`(catch #(bad-arg y 0)) (try (foo [x 1]))]
      [`(catch #(bad-arg y not)) (try (foo [x 1] [y 'not]))]
      [`(catch #(bad-arg y 4)) (try (foo [x 5] [y 4]))]
      [,b (foo [x 7] [y 11])]
      [7 (foo x b)]
      [11 (foo y b)]
      )
     'ok))
  )

(mat procedure/arity ()
  (define (take-any . x) x)
  (define (take-one x) x)
  (define (take-five a b c d e) a)
  (define take-one-or-four
    (case-lambda
     [(x) x]
     [(a b c d) a]))
  (define port (open-input-string "port in a storm"))
  ;; curried cases
  (match-let*
   ([,any (procedure/arity? -1)]
    [#t (any take-any)]
    [#t (any take-one)]
    [#t (any take-five)]
    [#t (any take-one-or-four)]
    [#f (any port)]
    [,two+ (procedure/arity? -4)]
    [#t (two+ take-any)]
    [#f (two+ take-one)]
    [#t (two+ take-five)]
    [#t (two+ take-one-or-four)]
    [#f (two+ port)]
    [,one-or-four (procedure/arity? #b10010)]
    [#t (one-or-four take-any)]
    [#t (one-or-four take-one)]
    [#f (one-or-four take-five)]
    [#t (one-or-four take-one-or-four)]
    [#f (one-or-four port)])
   'ok)
  ;; direct case
  (match-let*
   ([#t (procedure/arity? -1 take-any)]
    [#t (procedure/arity? -1 take-one)]
    [#t (procedure/arity? -1 take-five)]
    [#t (procedure/arity? -1 take-one-or-four)]
    [#f (procedure/arity? -1 port)]
    [#t (procedure/arity? -4 take-any)]
    [#f (procedure/arity? -4 take-one)]
    [#t (procedure/arity? -4 take-five)]
    [#t (procedure/arity? -4 take-one-or-four)]
    [#f (procedure/arity? -4 port)]
    [#t (procedure/arity? #b10010 take-any)]
    [#t (procedure/arity? #b10010 take-one)]
    [#f (procedure/arity? #b10010 take-five)]
    [#t (procedure/arity? #b10010 take-one-or-four)]
    [#f (procedure/arity? #b10010 port)])
   'ok)
  )

(mat record-writer ()
  (define-options my:opts (required))
  (define-options other:options (optional [x] [y]))
  (define-options more:options (required [a]) (optional [b]))
  (match-let*
   (["#<my:opts>" (format "~s" (my:opts))]
    ["#<other:options>" (format "~s" (other:options))]
    ["#<other:options>" (format "~s" (other:options [x 11]))]
    ["#<other:options>" (format "~s" (other:options [y not]))]
    ["#<other:options>" (format "~s" (other:options [x 11] [y not]))]
    ["#<more:options>" (format "~s" (more:options [a 'ok]))]
    ["#<more:options>" (format "~s" (more:options [a 'ok] [b "line"]))])
   'ok))
