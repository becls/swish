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
 (except (chezscheme) sleep)
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

(define (round-trip x)
  (let ([y (json:string->object (json:object->string x))])
    (assert (equal? (dump x) (dump y))))
  (let ([y (json:string->object (utf8->string (json:object->bytevector x)))])
    (assert (equal? (dump x) (dump y)))))

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
  (match-let*
   (["foo" (json:object->string (json-inline "foo"))]
    ["new Date(123)" (json:object->string (json-inline "new Date(123)"))])
   'ok))

(mat whitespace ()
  (match-let*
   ([(1 2 3) (json:string->object " \t\r\n[ \t\r\n1 \t\r\n, \t\r\n2 \t\r\n, \t\r\n3 \t\r\n] \t\r\n")])
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
