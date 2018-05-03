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

(define json-inline string->symbol)

(define (dump x)
  (cond
   [(hashtable? x)
    (vector-map
     (lambda (k) (cons k (dump (hashtable-ref x k #f))))
     (vector-sort string<? (hashtable-keys x)))]
   [(pair? x) (map dump x)]
   [else x]))

(define round-trip
  (case-lambda
   [(x) (round-trip x #f #f)]
   [(x custom-write custom-inflate)
    (define (->string x)
      (if custom-write
          (json:object->string x custom-write)
          (json:object->string x)))
    (define (->bytevector x)
      (if custom-write
          (json:object->bytevector x custom-write)
          (json:object->bytevector x)))
    (define (string-> x)
      (if custom-inflate
          (json:string->object x custom-inflate)
          (json:string->object x)))
    (let ([y (string-> (->string x))])
      (assert (equal? (dump x) (dump y))))
    (let ([y (string-> (utf8->string (->bytevector x)))])
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
    #(,(json:make-object ["foo" "bar"]) "{\"foo\":\"bar\"}")
    #(,(json:make-object ["foo" "bar"] ["baz" 123])
      "{\"foo\":\"bar\",\"baz\":123}")
    #(() "[]")
    #((()) "[[]]")
    #((1 "foo") "[1,\"foo\"]")

    #(,(json:make-object ["foo" '(123)]) "{\"foo\":[123]}")
    #(,(json:make-object ["foo" (json:make-object ["bar" #t])])
      "{\"foo\":{\"bar\":true}}")

    #((-123 "foo" ,(json:make-object ["bar" '()]) #\nul)
      "[-123,\"foo\",{\"bar\":[]},null]")
    ))

(mat string->object ()
  (for-each
   (lambda (obj)
     (match-let* ([#(,x ,y) obj])
       (assert (equal? (dump x) (dump (json:string->object y))))))
   test-objs))

(mat round-trip ()
  (for-each
   (lambda (obj)
     (match-let* ([#(,x ,_) obj])
       (round-trip x)))
   test-objs))

(mat google ()
  (define (handle-symbol op x wr) (and (symbol? x) (display x op) #t))
  (match-let*
   (["foo" (json:object->string (json-inline "foo") handle-symbol)]
    ["new Date(123)" (json:object->string (json-inline "new Date(123)") handle-symbol)])
   'ok))

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
            ["bar" "rel"]
            ["foo" "tstep"]
            ["ace" "rbic"]))
         (get))]
      [,@by-write
       (let ()
         (json:write-object op json:write
           ["bar" "rel"]
           ["foo" "tstep"]
           ["ace" "rbic"])
         (get))]
      [,custom-write
       (lambda (op v wr)
         (if (vector? v)
             (fprintf op "[~{\"~r\"~^,~}]" (vector->list v))
             (wr op v)))]
      ["{\"abc\":[\"four\",\"five\",\"six\"],\"def\":[7,8,9],\"ghi\":3}"
       (begin
         (json:write-object op json:write
           ["ghi" 3]
           ["abc" '#(4 5 6) custom-write]
           ["def" '(7 8 9) custom-write])
         (get))])
     'ok)))

(mat custom ()
  (define-tuple <obj> a b c)
  (define dne (box #t))
  (define (must-ref ht key)
    (let ([hit (hashtable-ref ht key dne)])
      (if (eq? hit dne)
          (errorf 'must-ref "did not find ~s" key)
          hit)))
  (define (custom-inflate x)
    (match (hashtable-ref x "_type_" #f)
      ["<obj>"
       (<obj> make
         [a (must-ref x "a")]
         [b (must-ref x "b")]
         [c (must-ref x "c")])]
      ["symbol"
       (string->symbol (must-ref x "name"))]
      [,_ x]))
  (define (custom-write op x wr)
    (match x
      [`(<obj> ,a ,b ,c)
       (json:write-object op wr
         ["_type_" "<obj>"]
         ["a" a]
         ["b" b]
         ["c" c])]
      [,x
       (guard (symbol? x))
       (json:write-object op wr
         ["_type_" "symbol"]
         ["name" (symbol->string x)])]
      [,_ #f]))
  (define (hashtable->alist ht)
    (let-values ([(keys vals) (hashtable-entries ht)])
      (sort (lambda (a b) (string<? (car a) (car b)))
        (map cons (vector->list keys) (vector->list vals)))))
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
     [(hashtable? x)
      (and (hashtable? y)
           (compare (hashtable->alist x) (hashtable->alist y)))]
     [else #f]))
  (define (nop-custom-inflate x) x)
  (define (nop-custom-write op x wr) #f)
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
               ["able" (<obj> make [a 4] [b 5] [c 6])]
               ["baker" '("one" "two")]
               ["charlie" "delta"])
              (<obj> make
                [a 'typical]
                [b 'fore]
                [c (json:make-object ["take" "cake"])]))])]
    [,string (json:object->string x custom-write)]
    [ok (compare x (json:string->object string custom-inflate))]
    [,bv (json:object->bytevector x custom-write)]
    [ok (compare x (json:string->object (utf8->string bv) custom-inflate))])
   'ok))

(mat errors ()
  (match-let*
   ([#(EXIT #(invalid-datum 1/2)) (catch (json:object->string 1/2))]
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
    [#(EXIT #(unexpected-input #\} 1)) (catch (json:string->object "[}"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "-"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "1."))]
    [#(EXIT unexpected-eof) (catch (json:string->object "1e"))]
    [#(EXIT unexpected-eof) (catch (json:string->object "1e+"))]
    [#(EXIT #(unexpected-input #\. 3)) (catch (json:string->object "1.2.3"))]
    [#(EXIT #(unexpected-input #\, 1)) (catch (json:string->object "[,"))]
    [#(EXIT #(unexpected-input #\2 3)) (catch (json:string->object "[1 2]"))]
    [#(EXIT #(unexpected-input #\. 3)) (catch (json:string->object "\"\\u.\""))]
    [#(EXIT #(unexpected-input #\g 2)) (catch (json:string->object "\"\\g\""))]
    [#(EXIT invalid-surrogate-pair) (catch (json:string->object "\"\\udc00\""))]
    [#(EXIT #(unexpected-input #\" 7))
     (catch (json:string->object "\"\\ud800\""))]
    [#(EXIT invalid-surrogate-pair)
     (catch (json:string->object "\"\\ud800\\ud801\""))])
   'ok))

(mat stable-ordering ()
  (define (fake-hash x) 3)
  (let* ([obj1 (json:extend-object (make-hashtable fake-hash string=?)
                 ["a" 1]
                 ["b" 2])]
         [str1 (json:object->string obj1)]
         [obj2 (json:extend-object (make-hashtable fake-hash string=?)
                 ["b" 2]
                 ["a" 1])]
         [str2 (json:object->string obj2)])
    (assert (equal? str1 str2))))
