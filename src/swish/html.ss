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

#!chezscheme
(library (swish html)
  (export
   html->bytevector
   html->string
   html:encode
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish io)
   )

  (define (html->bytevector x)
    (call-with-bytevector-output-port
     (lambda (op) (html->string op x))
     (make-utf8-transcoder)))

  (define html->string
    (case-lambda
     [(op x)
      (match x
        [() (void)]
        [,v (guard (eq? v (void))) (void)]
        [,s (guard (string? s)) (html:encode op s)]
        [,n (guard (number? n)) (display-string (number->string n) op)]
        [(begin . ,patterns)
         (guard (list? patterns))
         (write-patterns patterns op)]
        [(cdata . ,strings)
         (guard (list? strings))
         (display-string "[!CDATA[" op)
         (write-strings strings op x)
         (display-string "]]" op)]
        [(raw . ,strings)
         (guard (list? strings))
         (write-strings strings op x)]
        [(,tag (@ . ,attrs) . ,patterns)
         (guard (and (symbol? tag) (list? patterns)))
         (tag->string op x tag attrs patterns)]
        [(,tag . ,patterns)
         (guard (and (symbol? tag) (list? patterns)))
         (tag->string op x tag '() patterns)]
        [,_ (bad-arg 'html->string x)])]
     [(x)
      (let-values ([(op get) (open-string-output-port)])
        (html->string op x)
        (get))]))

  (define html:encode
    (case-lambda
     [(op s)
      (let ([len (string-length s)])
        (do ((i 0 (+ i 1))) ((= i len))
          (let ([c (string-ref s i)])
            (case c
              [(#\") (display-string "&quot;" op)]
              [(#\&) (display-string "&amp;" op)]
              [(#\<) (display-string "&lt;" op)]
              [(#\>) (display-string "&gt;" op)]
              [else (write-char c op)]))))]
     [(s)
      (let-values ([(op get) (open-string-output-port)])
        (html:encode op s)
        (get))]))

  (define (write-patterns patterns op)
    (unless (null? patterns)
      (html->string op (car patterns))
      (write-patterns (cdr patterns) op)))

  (define (write-strings strings op x)
    (unless (null? strings)
      (let ([s (car strings)])
        (unless (string? s) (bad-arg 'html->string x))
        (display-string s op))
      (write-strings (cdr strings) op x)))

  (define (tag->string op x tag attrs patterns)
    (unless (list? attrs) (bad-arg 'html->string x))
    (case tag
      [(area base br col command embed hr img input keygen link meta param
         source track wbr)              ; void tags
       (unless (null? patterns) (bad-arg 'html->string x))
       (open-tag op x tag attrs)]
      [(html5)
       (display-string "<!DOCTYPE html>" op)
       (open-tag op x 'html
         (if (lang-present? attrs)
             attrs
             (cons '(lang "en") attrs)))
       (write-patterns patterns op)
       (close-tag op 'html)]
      [(script style)
       (open-tag op x tag attrs)
       (write-strings patterns op x)
       (close-tag op tag)]
      [else
       (open-tag op x tag attrs)
       (write-patterns patterns op)
       (close-tag op tag)]))

  (define (open-tag op x tag attrs)
    (write-char #\< op)
    (display-string (symbol->string tag) op)
    (write-attrs attrs op x)
    (write-char #\> op))

  (define (lang-present? attrs)
    (and (not (null? attrs))
         (match (car attrs)
           [(lang . ,_) #t]
           [,_ (lang-present? (cdr attrs))])))

  (define (write-attrs attrs op x)
    (unless (null? attrs)
      (match (car attrs)
        [,v (guard (eq? v (void))) (void)]
        [(,key)
         (guard (symbol? key))
         (write-char #\space op)
         (display-string (symbol->string key) op)]
        [(,key ,value)
         (guard (and (symbol? key) (or (string? value) (number? value))))
         (write-char #\space op)
         (display-string (symbol->string key) op)
         (write-char #\= op)
         (write-char #\" op)
         (if (string? value)
             (html:encode op value)
             (html:encode op (number->string value)))
         (write-char #\" op)]
        [,_ (bad-arg 'html->string x)])
      (write-attrs (cdr attrs) op x)))

  (define (close-tag op tag)
    (write-char #\< op)
    (write-char #\/ op)
    (display-string (symbol->string tag) op)
    (write-char #\> op))
  )
