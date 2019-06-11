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

#!chezscheme
(import
 (chezscheme)
 (swish erlang)
 (swish json)
 (swish mat)
 (swish testing)
 )

(define (key->string x)
  (if (gensym? x)
      (parameterize ([print-gensym #t]) (format "~s" x))
      (symbol->string x)))

(define (cell<? x y)
  (string<? (key->string (car x)) (key->string (car y))))

(define (dump x)
  (cond
   [(json:object? x)
    (vector-map
     (lambda (cell) (cons (car cell) (dump (cdr cell))))
     (vector-sort cell<? (hashtable-cells x)))]
   [(pair? x) (map dump x)]
   [else x]))

(define round-trip
  (case-lambda
   [(x) (round-trip x #f #f)]
   [(x custom-write custom-inflate)
    (define (->string x)
      (if custom-write
          (json:object->string x #f custom-write)
          (json:object->string x)))
    (define (->bytevector x)
      (if custom-write
          (json:object->bytevector x #f custom-write)
          (json:object->bytevector x)))
    (define (string-> x)
      (if custom-inflate
          (json:string->object x custom-inflate)
          (json:string->object x)))
    (define (bytevector-> x)
      (if custom-inflate
          (json:bytevector->object x custom-inflate)
          (json:bytevector->object x)))
    (let ([y (string-> (->string x))])
      (assert (equal? (dump x) (dump y))))
    (let ([y (bytevector-> (->bytevector x))])
      (assert (equal? (dump x) (dump y))))
    'ok]))

(define test-objs
  `(#(#\nul "null")
    #(#t "true")
    #(#f "false")
    #(1 "1")
    #(3.1415 "3.1415")
    #(-1 "-1")
    #(-3.1415 "-3.1415")
    #(0.1 "0.1")
    #(-0.1 "-0.1")
    #(1.2e11 "12.0e10")
    #(1.234e10 "1.234e+10")
    #(-1.234e-10 "-1.234e-10")
    #((1.0 2) "[1.0e0,2]")
    #("" "\"\"")
    #(,(list->string (map integer->char (iota #xD800)))
      ,(let ([op (open-output-string)])
         (write-char #\" op)
         (do ([i 0 (+ i 1)]) [(= i #xD800)]
           (let ([c (integer->char i)])
             (cond
              [(eqv? c #\x08) (write-char #\\ op) (write-char #\b op)]
              [(eqv? c #\x09) (write-char #\\ op) (write-char #\t op)]
              [(eqv? c #\x0C) (write-char #\\ op) (write-char #\f op)]
              [(eqv? c #\x0A) (write-char #\\ op) (write-char #\n op)]
              [(eqv? c #\x0D) (write-char #\\ op) (write-char #\r op)]
              [(char<=? c #\x1F) (fprintf op "\\u~4,'0x" i)]
              [(memv c '(#\\ #\")) (write-char #\\ op) (write-char c op)]
              [else (write-char c op)])))
         (write-char #\" op)
         (get-output-string op)))
    #("\x1D11E;\x1d11f;\x1d120;" "\"\\uD834\\uDD1E\x1D11F;\\ud834\\udd20\"")
    #(,(json:make-object) "{}")
    #(,(json:make-object [foo "bar"]) "{\"foo\":\"bar\"}")
    #(,(json:make-object [foo "bar"] [baz 123])
      "{\"foo\":\"bar\",\"baz\":123}")
    #(() "[]")
    #((()) "[[]]")
    #((1 "foo") "[1,\"foo\"]")

    #(,(json:make-object [foo '(123)]) "{\"foo\":[123]}")
    #(,(json:make-object [,'foo (json:make-object [bar #t])])
      "{\"foo\":{\"bar\":true}}")

    #((-123 "foo" ,(json:make-object [bar '()]) #\nul)
      "[-123,\"foo\",{\"bar\":[]},null]")
    #(,(json:make-object [,(gensym "bar" "unique-name") 456] [#{foo lish} 789])
      "{\"#{foo lish}\":789,\"#{bar unique-name}\":456}")
    #(,(json:make-object [,(string->symbol "#{foo not-gensym") 123])
      "{\"#{foo not-gensym\":123}")
    ))

(mat read ()
  (assert (eof-object? (json:read (open-input-string ""))))
  (assert (eof-object? (json:read (open-input-string "  \t  \n  "))))
  (match-let*
   ([,ip (open-input-string "1  \t  \n  2")]
    [1 (json:read ip)]
    [2 (json:read ip)]
    [,x (guard (eof-object? x)) (json:read ip)])
   'ok))

(mat string->object ()
  (for-each
   (lambda (obj)
     (match-let* ([#(,x ,y) obj])
       (assert (equal? (dump x) (dump (json:string->object y))))))
   test-objs))

(mat bytevector->object ()
  (for-each
   (lambda (obj)
     (match-let* ([#(,x ,y) obj])
       (assert (equal? (dump x) (dump (json:bytevector->object (string->utf8 y)))))))
   test-objs))

(mat round-trip ()
  (for-each
   (lambda (obj)
     (match-let* ([#(,x ,_) obj])
       (round-trip x)))
   test-objs))

(mat whitespace ()
  (match-let*
   ([(1 2 3) (json:string->object " \t\r\n[ \t\r\n1 \t\r\n, \t\r\n2 \t\r\n, \t\r\n3 \t\r\n] \t\r\n")])
   'ok))

(mat write-object ()
  (let-values ([(op get) (open-string-output-port)])
    (match-let*
     ([,by-write
       (let ()
         (json:write op
           (json:make-object
            [bar "rel"]
            [foo "tstep"]
            [ace "rbic"]))
         (get))]
      [,@by-write
       (let ()
         (json:write-object op #f json:write
           [bar "rel"]
           [foo "tstep"]
           [ace "rbic"])
         (get))]
      [,custom-write
       (lambda (op v indent wr)
         (if (vector? v)
             (fprintf op "[~{\"~r\"~^,~}]" (vector->list v))
             (wr op v)))]
      ["{\"abc\":[\"four\",\"five\",\"six\"],\"def\":[7,8,9],\"ghi\":3}"
       (begin
         (json:write-object op #f json:write
           [ghi 3]
           [abc '#(4 5 6) custom-write]
           [def '(7 8 9) custom-write])
         (get))]
      [,g1 (gensym)]
      [,g2 (gensym)]
      [,str
       (begin
         ((eval
           `(lambda (op)
              (json:write-object op #f json:write
                [,g1 123]
                [,g2 456])))
          op)
         (get))]
      [,obj (json:string->object str)]
      [123 (json:ref obj g1 #f)]
      [456 (json:ref obj g2 #f)]
      ["{}"
       (begin
         (json:write-object op #f json:write)
         (get))]
      )
     'ok)))

(mat pretty ()
  (let-values ([(op get) (open-string-output-port)])
    (define-syntax compare-output
      (syntax-rules ()
        [(_ indent [key val] ...)
         (match-let*
          ([,! (json:write op (json:make-object [key val] ...) indent)]
           [,expected (get)]
           [,! (json:write-object op indent json:write [key val] ...)]
           [,@expected (get)])
          expected)]))
    (match-let*
     (["{\n  \"abc\": [\n    \"four\",\n    \"five\",\n    \"six\"\n  ],\n  \"def\": [\n    7,\n    8,\n    9\n  ],\n  \"ghi\": 3\n}\n"
       (begin
         (json:write op
           (json:make-object
            [ghi 3]
            [abc '#(4 5 6)]
            [def '(7 8 9)])
           0
           (lambda (op x indent wr)
             (cond
              [(vector? x)
               (wr op (map (lambda (x) (format "~r" x)) (vector->list x)) indent)
               #t]
              [else #f])))
         (get))]
      ["{\n  \"abc\": [\"four\",\"five\",\"six\"],\n  \"def\": [\n    7,\n    8,\n    9\n  ],\n  \"ghi\": 3\n}\n"
       (begin
         (json:write op
           (json:make-object
            [ghi 3]
            [abc '#(4 5 6)]
            [def '(7 8 9)])
           0
           (lambda (op x indent wr)
             (cond
              [(vector? x)
               ;; Explicitly bypasses pretty mechanisms.
               (fprintf op "[~{\"~r\"~^,~}]" (vector->list x))
               #t]
              [else #f])))
         (get))]
      [,goodbye (lambda (op x indent wr) (wr op "goodbye" indent))]
      ["{\n  \"list\": [\n    1,\n    2,\n    {\n      \"tiki\": \"torch\",\n      \"wiki\": \"goodbye\"\n    }\n  ]\n}\n"
       (begin
         (json:write op
           (json:make-object
            [list (list 1 2 'flubber)])
           0
           (lambda (op x indent wr)
             (match x
               [flubber
                (json:write-object op indent wr
                  [tiki "torch"]
                  [wiki "hello" goodbye])
                #t]
               [,_ #f])))
         (get))]
      ;; ensure that json:write-object matches json:write
      ["{}" (compare-output #f)]
      ["{}" (compare-output 3)]
      ["{}\n" (compare-output 0)]
      ["{\"bar\":2,\"foo\":1}"
       (compare-output #f [foo 1] [bar 2])]
      ["{\n  \"bar\": 2,\n  \"foo\": 1\n}\n"
       (compare-output 0 [foo 1] [bar 2])]
      ["{\n      \"bar\": 2,\n      \"foo\": 1\n    }"
       (compare-output 4 [foo 1] [bar 2])]
      )
     'ok)))

(mat custom ()
  (define-tuple <obj> a b c)
  (define dne (box #t))
  (define (must-ref ht key)
    (let ([hit (json:ref ht key dne)])
      (if (eq? hit dne)
          (errorf 'must-ref "did not find ~s" key)
          hit)))
  (define (custom-inflate x)
    (match (json:ref x '_type_ #f)
      ["<obj>"
       (<obj> make
         [a (must-ref x 'a)]
         [b (must-ref x 'b)]
         [c (must-ref x 'c)])]
      ["symbol"
       (string->symbol (must-ref x 'name))]
      [,_ x]))
  (define (custom-write op x indent wr)
    (match x
      [`(<obj> ,a ,b ,c)
       (json:write-object op #f wr
         [_type_ "<obj>"]
         [a a]
         [b b]
         [c c])]
      [,x
       (guard (symbol? x))
       (json:write-object op #f wr
         [_type_ "symbol"]
         [name (symbol->string x)])]
      [,_ #f]))
  (define (hashtable->alist ht)
    (sort cell<? (vector->list (hashtable-cells ht))))
  (define (compare x y)
    (cond
     [(eqv? x y) 'ok]
     [(pair? x)
      (and (pair? y)
           (compare (car x) (car y))
           (compare (cdr x) (cdr y)))]
     [(string? x) (and (equal? x y) 'ok)]
     [(vector? x)
      (and (vector? y)
           (compare (vector->list x) (vector->list y)))]
     [(json:object? x)
      (and (json:object? y)
           (compare (hashtable->alist x) (hashtable->alist y)))]
     [else #f]))
  (define (nop-custom-inflate x) x)
  (define (nop-custom-write op x indent wr) #f)
  ;; test inert custom-write / custom-inflate
  (for-each
   (lambda (obj)
     (match-let* ([#(,x ,_) obj])
       (round-trip x nop-custom-write nop-custom-inflate)))
   test-objs)
  (match-let*
   ([,x (<obj> make
          [a "quick"]
          [b 'line]
          [c (list
              (<obj> make [a 1] [b 2] [c 3])
              (json:make-object
               [able (<obj> make [a 4] [b 5] [c 6])]
               [baker '("one" "two")]
               [charlie "delta"])
              (<obj> make
                [a 'typical]
                [b 'fore]
                [c (json:make-object [take "cake"])]))])]
    [,string (json:object->string x #f custom-write)]
    [ok (compare x (json:string->object string custom-inflate))]
    [,bv (json:object->bytevector x #f custom-write)]
    [ok (compare x (json:bytevector->object bv custom-inflate))])
   'ok))

(mat object ()
  (match-let*
   ([#t (json:object? (json:make-object))]
    [#t (json:object? (json:make-object [foo '(1 2 3)]))]
    [#f (json:object? 7)]
    [#f (json:object? (make-hashtable string-hash string=?))]
    [#t (json:object? (json:string->object "{}"))]
    [#f (json:object? (json:string->object "[1,2,3]"))]
    [#t (json:object? (json:string->object "{\"abc\":[3,4]}"))])
   'ok))

(mat errors ()
  (define (match-form input violation-form)
    (match (syntax->datum violation-form)
      [,@input 'ok]))
  (assert-syntax-error (json:make-object ["str" 123]) "invalid key \"str\" in" match-form)
  (assert-syntax-error (json:extend-object x ["str" 123]) "invalid key \"str\" in" match-form)
  (match-let*
   ([#(EXIT #(bad-arg json:write -1))
     (catch (json:write (open-output-string) 12 -1))]
    [#(EXIT #(invalid-datum 1/2)) (catch (json:object->string 1/2))]
    [#(EXIT #(invalid-datum +inf.0)) (catch (json:object->string +inf.0))]
    [#(EXIT #(invalid-datum -inf.0)) (catch (json:object->string -inf.0))]
    [#(EXIT #(invalid-datum +nan.0)) (catch (json:object->string +nan.0))]
    [#(EXIT unexpected-eof) (catch (json:string->object "t"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "f"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "n"))]
    [#(EXIT #(unexpected-input #\, 6))
     (catch (json:string->object "{\"foo\",12}"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "\""))]
    [#(EXIT unexpected-eof) (catch (json:string->object "["))]
    [#(EXIT unexpected-eof) (catch (json:string->object "{"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "{\"foo\""))]
    [#(EXIT #(unexpected-input #\1 1)) (catch (json:string->object "{1}"))]
    [#(EXIT #(unexpected-input #\: 6))
     (catch (json:string->object "{\"a\":1:}"))]
    [#(EXIT #(unexpected-input #\} 7))
     (catch (json:string->object "{\"a\":1,}"))]
    [#(EXIT #(unexpected-input #\} 1)) (catch (json:string->object "[}"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "-"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "1."))]
    [#(EXIT unexpected-eof) (catch (json:string->object "1e"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "1e+"))]
    [#(EXIT #(unexpected-input #\. 3)) (catch (json:string->object "1.2.3"))]
    [#(EXIT #(unexpected-input #\, 1)) (catch (json:string->object "[,"))]
    [#(EXIT #(unexpected-input #\] 3)) (catch (json:string->object "[1,]"))]
    [#(EXIT #(unexpected-input #\2 3)) (catch (json:string->object "[1 2]"))]
    [#(EXIT #(unexpected-input #\. 3)) (catch (json:string->object "\"\\u.\""))]
    [#(EXIT #(unexpected-input #\g 2)) (catch (json:string->object "\"\\g\""))]
    [#(EXIT invalid-surrogate-pair) (catch (json:string->object "\"\\udc00\""))]
    [#(EXIT #(unexpected-input #\" 7))
     (catch (json:string->object "\"\\ud800\""))]
    [#(EXIT invalid-surrogate-pair)
     (catch (json:string->object "\"\\ud800\\ud801\""))])
   'ok)
  (match-let*
   ([#(EXIT #(bad-arg json:ref 7)) (catch (json:ref 7 "abc" "def"))]
    [#(EXIT #(bad-arg json:ref ("ument")))
     (catch (json:ref (json:make-object) '("ument") #f))]
    [#(EXIT #(bad-arg json:ref ()))
     (catch (json:ref (json:make-object) '() #f))]
    [#(EXIT #(bad-arg json:ref (tarry "not")))
     (let ([obj (json:make-object [tarry (json:make-object)])])
       (catch (json:ref obj '(tarry "not") #f)))]
    [#(EXIT #(bad-arg json:ref #\z))
     (catch (json:ref (json:make-object) #\z "def"))]
    [#(EXIT #(bad-arg json:update! 7)) (catch (json:update! 7 'abc values 2))]
    [#(EXIT #(bad-arg json:update! ("ument")))
     (catch (json:update! (json:make-object) '("ument") values #f))]
    [#(EXIT #(bad-arg json:update! (tarry "not")))
     (let ([obj (json:make-object [tarry (json:make-object)])])
       (catch (json:update! obj '(tarry "not") values 123)))]
    [#(EXIT #(bad-arg json:update! ()))
     (catch (json:update! (json:make-object) '() values 2))]
    [#(EXIT #(bad-arg json:update! "bogus"))
     (catch (json:update! (json:make-object) "bogus" values 2))]
    [#(EXIT #(bad-arg json:update! #\a))
     (catch (json:update! (json:make-object) #\a values 2))]
    [#(EXIT #(bad-arg json:update! #\z))
     (catch (json:update! (json:make-object) values #\z 2))]
    [#(EXIT #(bad-arg json:delete! 7)) (catch (json:delete! 7 'abc))]
    [#(EXIT #(bad-arg json:delete! ("ument")))
     (catch (json:delete! (json:make-object) '("ument")))]
    [#(EXIT #(bad-arg json:delete! ()))
     (catch (json:delete! (json:make-object) '()))]
    [#(EXIT #(bad-arg json:delete! (tarry "not")))
     (let ([obj (json:make-object [tarry (json:make-object)])])
       (catch (json:delete! obj '(tarry "not"))))]
    [#(EXIT #(bad-arg json:delete! #\z))
     (catch (json:delete! (json:make-object) #\z))]
    [#(EXIT #(bad-arg json:size large))
     (catch (json:size 'large))]
    [#(EXIT #(bad-arg json:set! 1))
     (catch (json:set! 1 2 3))]
    )
   'ok))

(mat stable-ordering ()
  (define (find-collision s1 bound)
    (define (gen)
      (let* ([len (+ 1 (random 5))]
	     [s (make-string len)])
	(do ([i 0 (fx+ i 1)]) ((fx= i len) (string->symbol s))
	  (string-set! s i
	    (integer->char (fx+ (char->integer #\a) (random 26)))))))
    (define target (symbol-hash s1))
    (let search ([i 0])
      (and (fx< i bound)
	   (let ([s2 (gen)])
	     (if (= (symbol-hash s2) target)
	         `(,s1 ,s2)
	         (search (fx+ i 1)))))))
  (match-let*
   ([,hash (symbol-hash 'foo)]
    [,@hash (symbol-hash 'byu)] ;; use find-collision if this fails
    [,obj1 (json:make-object
	    [foo 1]
	    [byu 2])]
    [,str1 (json:object->string obj1)]
    [,obj2 (json:make-object
	    [byu 2]
	    [foo 1])]
    [,@str1 (json:object->string obj2)])
   'ok))

(mat manipulate ()
  (match-let*
   ([,obj (json:make-object)]
    [#() (json:cells obj)]
    [0 (json:size obj)]
    [1 (json:ref obj 'not-found 1)]
    [2 (json:ref obj '(not-found) 2)]
    [3 (json:ref obj '(not-found "either") 3)]
    [,_ (json:update! obj 'count values 0)]
    [0 (json:ref obj 'count 'wrong)]
    [#((count . 0)) (json:cells obj)]
    [1 (json:size obj)]
    [,_ (json:update! obj 'count add1 9000)]
    [1 (json:ref obj 'count 'wrong)]
    [,_ (json:update! obj '(count) list 'wrong)]
    [(1) (json:ref obj 'count 'wrong)]
    [,_ (json:update! obj 'new vector "init")]
    [#("init") (json:ref obj 'new 'wrong)]
    [,_ (json:update! obj '(new world) string-upcase "order")]
    [#t (json:object? (json:ref obj 'new 'wrong))]
    ["ORDER" (json:ref obj '(new world) 'wrong)]
    [other (json:ref obj '(New World) 'other)]
    [,_ (json:update! obj '(new)
          (lambda (ht) (assert (json:object? ht)) 123)
          'wrong)]
    [123 (json:ref obj 'new 'wrong)]
    [,_ (json:update! obj '(new deeper path a b c) values 'quarry)]
    [quarry (json:ref obj '(new deeper path a b c) 'wrong)]
    [,_ (json:update! obj '(new deeper path a b D) values "abcd")]
    ["abcd" (json:ref obj '(new deeper path a b D) 'wrong)]
    [,_ (json:update! obj '(new deeper path a B D) values "1234")]
    ["1234" (json:ref obj '(new deeper path a B D) 'wrong)]
    [,_ (json:update! obj '(new deeper path A B D) values "okay")]
    [,_ (json:delete! obj 'not-found)]
    [,_ (json:delete! obj '(new deeper mystery))]
    ["okay" (json:ref obj '(new deeper path A B D) 'wrong)]
    [quarry (json:ref obj '(new deeper path a b c) 'wrong)]
    ["abcd" (json:ref obj '(new deeper path a b D) 'wrong)]
    [2 (json:size obj)]
    [,_ (json:delete! obj '(new deeper path a b D))]
    [gone (json:ref obj '(new deeper path a b D) 'gone)]
    [,_ (json:update! obj '(new) hashtable? 7)]
    [#t (json:ref obj '(new) 'wrong)]
    [gone (json:ref obj '(new deeper path a b D) 'gone)]
    [,_ (json:update! obj '(new) hashtable?
          (make-hashtable string-hash string=?))]
    [#f (json:ref obj '(new) 'wrong)]
    [,_ (json:update! obj 'count car 'wrong)]
    [#((count . 1) (new . #f)) (dump obj)]
    [,_ (json:set! obj '(count dracula) 13)]
    [#((count . #((dracula . 13))) (new . #f)) (dump obj)]
    [,_ (json:set! obj 'count 1)]
    [,_ (json:delete! obj '(count blessings))]
    [#((count . 1) (new . #f)) (dump obj)]
    [,_ (json:delete! obj 'not-found)]
    [#((count . 1) (new . #f)) (dump obj)]
    [,_ (json:delete! obj 'count)]
    [#((new . #f)) (dump obj)]
    [,_ (json:delete! obj '(new))]
    [#() (dump obj)]
    [,_ (json:delete! obj '(new))]
    [#() (dump obj)]
    [,ht (make-hashtable symbol-hash eq?)]
    [#(EXIT #(bad-arg json:delete! ,@ht)) (catch (json:delete! ht 'foo))]
    [#(EXIT #(bad-arg json:ref ,@ht)) (catch (json:ref ht 'foo #f))]
    [#(EXIT #(bad-arg json:update! ,@ht))
     (catch (json:update! ht 'foo values #f))]
    )
   'ok)
  ;; - behavior of json:update! is unspecified if given invalid path
  ;;   such as (foo . bar)
  ;;   in particular, we'll smash everything along that path
   )
