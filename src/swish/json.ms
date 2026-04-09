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

(define (fault-reason x)
  (match x
    [`(catch ,reason) reason]
    [,_ #f]))

(define (key->string x)
  (if (gensym? x)
      (parameterize ([print-gensym #t]) (format "~s" x))
      (symbol->string x)))

(define (cell<? x y)
  (string<? (key->string (car x)) (key->string (car y))))

(define (dump x)
  (cond
   [(fault-reason x)]
   [(json:object? x)
    (vector-map
     (lambda (cell) (cons (car cell) (dump (cdr cell))))
     (vector-sort cell<? (hashtable-cells x)))]
   [(pair? x) (map dump x)]
   [else x]))

;; Multi-byte characters can cause differences between string and
;; bytevector port-position in errors. In those cases, we use the
;; match pattern to ignore the position.
(define-syntax err
  (syntax-rules ()
    [(_ pat)
     (case-lambda
      [() 'pat]
      [(x)
       (match x
         [`(catch pat) #t]
         [,_ #f])])]))

(define test-specs
  `(;; null
    ("null" null)
    ("n" ,(err #(json:unexpected value #\n 0 ,_)))
    ("nully" ,(err #(json:unexpected value nully 0 ,_)))

    ;; booleans
    ("true" #t)
    ("t" ,(err #(json:unexpected value #\t 0 ,_)))
    ("truey" ,(err #(json:unexpected value truey 0 ,_)))
    ("false" #f)
    ("f" ,(err #(json:unexpected value #\f 0 ,_)))
    ("falsey" ,(err #(json:unexpected value falsey 0 ,_)))

    ;; numbers
    ("0" 0)                             ; zero integer
    ("+0" [json ,(err #(json:unexpected value |+0| 0 ,_))] [json5 0]) ; positive zero integer
    ("-0" 0)                            ; negative zero integer
    ("0.0" 0.0)                         ; zero float
    ("+0.0" [json ,(err #(json:unexpected value |+0.0| 0 ,_))] [json5 0.0]) ; positive zero float
    ("-0.0" -0.0)                 ; negative zero float
    ("0e00" 0.0)                  ; zero integer with zero exponent
    ("0e12" 0.0)                  ; zero integer with integer exponent
    ("0e12.3" ,(err #(json:unexpected number #\. 4 ,_))) ; zero integer with float exponent
    ("0e" ,(err #(json:unexpected exponent #!eof 2 ,_))) ; zero integer with incomplete exponent
    ("0a" ,(err #(json:unexpected number #\a 1 ,_))) ; zero integer with non-number
    (".0" [json ,(err #(json:unexpected value |.0| 0 ,_))] [json5 0.0]) ; zero leading decimal point
    ("+.0" [json ,(err #(json:unexpected value |+.0| 0 ,_))] [json5 0.0]) ; positive zero leading decimal point
    ("-.0" [json ,(err #(json:unexpected value |-.0| 0 ,_))] [json5 -0.0]) ; negative zero leading decimal point
    ("0." [json ,(err #(json:unexpected number #!eof 2 ,_))] [json5 0.0]) ; zero trailing decimal point
    ("+0." [json ,(err #(json:unexpected value |+0.| 0 ,_))] [json5 0.0]) ; positive zero trailing decimal point
    ("-0." [json ,(err #(json:unexpected number #!eof 3 ,_))] [json5 -0.0]) ; negative zero trailing decimal point
    ("0x0" [json ,(err #(json:unexpected number #\x 1 ,_))] [json5 0]) ; zero hexadecimal
    ("-0x0" [json ,(err #(json:unexpected number #\x 2 ,_))] [json5 0]) ; negative zero hexadecimal
    ("00" ,(err #(json:unexpected number #\0 1 ,_))) ; leading zero not allowed
    ("-00" ,(err #(json:unexpected number #\0 2 ,_))) ; negative leading zero not allowed
    ("0x0" [json ,(err #(json:unexpected number #\x 1 ,_))] [json5 0]) ; zero hexadecimal

    ("123" 123)                         ; integer
    ("+123" [json ,(err #(json:unexpected value |+123| 0 ,_))] [json5 123]) ; positive integer
    ("-123" -123)                       ; negative integer
    ("1e23.4" ,(err #(json:unexpected number #\. 4 ,_))) ; integer with float exponent
    ("1e+23.4" ,(err #(json:unexpected number #\. 5 ,_))) ; integer with positive float exponent
    ("1e-23.4" ,(err #(json:unexpected number #\. 5 ,_))) ; integer with negative float exponent
    ("1e0x4" ,(err #(json:unexpected number #\x 3 ,_))) ; integer with hexadecimal exponent
    ("1e+0x4" ,(err #(json:unexpected number #\x 4 ,_))) ; integer with positive hexadecimal exponent
    ("1e-0x4" ,(err #(json:unexpected number #\x 4 ,_))) ; integer with negative hexadecimal exponent
    ("1e23" 1e23)             ; integer with integer exponent
    ("1e+23" 1e23)            ; integer with integer exponent
    ("1e-23" 1e-23)           ; integer with negative integer exponent
    ("1e-0" 1.0)              ; integer with negative zero exponent
    ("1e+308" 1e308)          ; integer with large positive exponent
    ("1e-308" 1e-308)         ; integer with large negative exponent
    ("1e" ,(err #(json:unexpected exponent #!eof 2 ,_))) ; integer with non-exponent
    ("1e+" ,(err #(json:unexpected exponent #!eof 3 ,_))) ; integer with signed non-exponent

    ("3.1415" 3.1415)                   ; float
    ("+3.1415" [json ,(err #(json:unexpected value |+3.1415| 0 ,_))] [json5 3.1415]) ; positive float
    ("-3.1415" -3.1415)                 ; negative float
    ("0.2" 0.2)                         ; float leading zero
    ("+0.2" [json ,(err #(json:unexpected value |+0.2| 0 ,_))] [json5 0.2]) ; positive float leading zero
    ("-0.2" -0.2)                       ; negative float leading zero
    ("2." [json ,(err #(json:unexpected number #!eof 2 ,_))] [json5 2.0]) ; float with trailing decimal point
    ("+2." [json ,(err #(json:unexpected value |+2.| 0 ,_))] [json5 2.0]) ; positive float with trailing decimal point
    ("-2." [json ,(err #(json:unexpected number #!eof 3 ,_))] [json5 -2.0]) ; negative float with trailing decimal point
    (".2" [json ,(err #(json:unexpected value |.2| 0 ,_))] [json5 0.2]) ; float leading decimal point
    ("+.2" [json ,(err #(json:unexpected value |+.2| 0 ,_))] [json5 0.2]) ; positive float leading decimal point
    ("-.2" [json ,(err #(json:unexpected value |-.2| 0 ,_))] [json5 -0.2]) ; negative float leading decimal point
    ("12.0e10" 1.2e11)         ; float with integer exponent
    ("1.234e+10" 1.234e10)     ; float with positive integer exponent
    ("1.234e-10" 1.234e-10)    ; float with negative integer exponent
    ("-1.234e-10" -1.234e-10)  ; negative float with negative exponent
    ("1.e4" [json ,(err #(json:unexpected number #\e 2 ,_))] [json5 10000.0]) ; float with trailing decimal point and integer exponent
    ("1.234e+308" 1.234e308)      ; float with large positive exponent
    ("1.234e-308" 1.234e-308)     ; float with large negative exponent

    ("0xd00dad" [json ,(err #(json:unexpected number #\x 1 ,_))] [json5 #xd00dad]) ; hexadecimal
    ("-0xCAB00D1E" [json ,(err #(json:unexpected number #\x 2 ,_))] [json5 #x-cab00d1e]) ; leading minus, hexadecimal
    ("0xdecafz"                         ; non-hexadecimal
     [json ,(err #(json:unexpected number #\x 1 ,_))]
     [json5 ,(err #(json:unexpected |hexadecimal number| #\z 7 ,_))])
    ("0x"                               ; empty hexadecimal
     [json ,(err #(json:unexpected number #\x 1 ,_))]
     [json5 ,(err #(json:unexpected |hexadecimal number| #!eof 2 ,_))])

    ("Infinity" [json ,(err #(json:unexpected value |Infinity| 0 ,_))] [json5 +inf.0])
    ("+Infinity" [json ,(err #(json:unexpected value |+Infinity| 0 ,_))] [json5 +inf.0])
    ("-Infinity" [json ,(err #(json:unexpected value |-Infinity| 0 ,_))] [json5 -inf.0])
    ("NaN" [json ,(err #(json:unexpected value |NaN| 0 ,_))] [json5 +nan.0])
    ("+NaN" [json ,(err #(json:unexpected value |+NaN| 0 ,_))] [json5 +nan.0])
    ("-NaN" [json ,(err #(json:unexpected value |-NaN| 0 ,_))] [json5 -nan.0])

    ;; Misc cases
    ("." ,(err #(json:unexpected value #\. 0 ,_)))   ; decimal only
    ("-" ,(err #(json:unexpected value #\- 0 ,_)))   ; minus only
    ("+" ,(err #(json:unexpected value #\+ 0 ,_)))   ; plus only
    (".a" ,(err #(json:unexpected value |.a| 0 ,_))) ; decimal folloed by non-digit
    ("1a" ,(err #(json:unexpected number #\a 1 ,_))) ; digit followed by non-digit
    ("1.a" ,(err #(json:unexpected number #\a 2 ,_))) ; non-digit in float
    ("1.2.3" ,(err #(json:unexpected number #\. 3 ,_))) ; multiple decimal points
    ("1ea" ,(err #(json:unexpected exponent #\a 2 ,_))) ; non-digit in exponent
    ("1/2" ,(err #(json:unexpected number #\/ 1 ,_))) ; Scheme rational is not valid
    ("1+2i" ,(err #(json:unexpected number #\+ 1 ,_))) ; Scheme complex is not valid
    ,@(map (lambda (n) `(,(format "~d" n) ,n)) ; fixnum/bignum border cases
        (list
         (most-negative-fixnum)
         (+ (most-negative-fixnum) 1)
         (- (most-negative-fixnum) 1)
         (most-positive-fixnum)
         (+ (most-positive-fixnum) 1)
         (fx/ (most-positive-fixnum) 2)))

    ;; strings
    ("\"\"" "")                         ; double quoted empty string
    ("\"" ,(err #(json:unexpected string #!eof 1 ,_))) ; double quoted, no data, no close
    ("\"abc\"" "abc")             ; double quoted string
    ("\"\\\"\"" "\"")             ; double quoted, escape double quote
    ("\"'\"" "'")                 ; double quoted, raw single quote
    (,(let ([op (open-output-string)]) ; double quoted Unicode and JSON escapes
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
             [(memv c '(#\\ #\/ #\")) (write-char #\\ op) (write-char c op)]
             [else (write-char c op)])))
        (write-char #\" op)
        (get-output-string op))
     ,(list->string (map integer->char (iota #xD800))))
    ("\"\\uD834\\uDD1E\x1D11F;\\ud834\\udd20\"" "\x1D11E;\x1d11f;\x1d120;") ; Unicode escapes
    ("\"\\u.\""    ; Unicode escape followed by non-hexadecimal digits
     ,(err #(json:unexpected |hexadecimal number| #\. 3 ,_)))

    ("''" [json ,(err #(json:unexpected value |''| 0 ,_))] [json5 ""]) ; single quoted empty string
    ("'"                            ; single quoted, no data, no close
     [json ,(err #(json:unexpected value #\' 0 ,_))]
     [json5 ,(err #(json:unexpected string #!eof 1 ,_))])
    ("'abc'" [json ,(err #(json:unexpected value |'abc'| 0 ,_))] [json5 "abc"]) ; single quoted string
    ("'\"'" [json ,(err #(json:unexpected value |'"'| 0 ,_))] [json5 "\""]) ; single quoted, raw double quote
    ("'\\''" [json ,(err #(json:unexpected value #\' 2 ,_))] [json5 "'"]) ; single quoted, escape single quote
    (,(let ([op (open-output-string)]) ; single quoted Unicode and JSON5 escapes
        (write-char #\' op)
        (do ([i 0 (+ i 1)]) [(= i #xD800)]
          (let ([c (integer->char i)])
            (cond
             [(eqv? c #\x08) (write-char #\\ op) (write-char #\b op)]
             [(eqv? c #\x09) (write-char #\\ op) (write-char #\t op)]
             [(eqv? c #\x0C) (write-char #\\ op) (write-char #\f op)]
             [(eqv? c #\x0A) (write-char #\\ op) (write-char #\n op)]
             [(eqv? c #\x0B) (write-char #\\ op) (write-char #\v op)]
             [(eqv? c #\x0D) (write-char #\\ op) (write-char #\r op)]
             [(char<=? c #\x1F) (fprintf op "\\u~4,'0x" i)]
             [(memv c '(#\\ #\/ #\')) (write-char #\\ op) (write-char c op)]
             [else (write-char c op)])))
        (write-char #\' op)
        (get-output-string op))
     [json ,(err ,_)]                   ; any error is acceptable
     [json5 ,(list->string (map integer->char (iota #xD800)))])

    ("'\\0'" [json ,(err #(json:unexpected value #\0 2 ,_))] [json5 "\x0;"]) ; escaped null
    ("'\\0a'"                     ; escaped null followed by non-digit
     [json ,(err #(json:unexpected value #\0 2 ,_))]
     [json5 "\x0;a"])
    ,@(map                      ; digits cannot follow an escaped null
       (lambda (n)
         (let ([c (integer->char (+ n (char->integer #\0)))])
           `(,(format "'\\0~a'" c)
             ;; These are different positions because JSON doesn't
             ;; interpret the single quote as a string, so it fails
             ;; when it encounters the first #\0. JSON5 doesn't fail
             ;; until it detects a digit after the #\0.
             [json ,(err #(json:unexpected value #\0 2 ,_))]
             [json5 ,(err #(json:unexpected string ,c 3 ,_))])))
       (iota 10))
    ,@(map        ; reverse solidus cannot be followed by digits [1-9]
       (lambda (n)
         (let ([c (integer->char (+ n (char->integer #\1)))])
           `(,(format "'\\~a'" c)
             [json ,(err #(json:unexpected value ,c 2 ,_))]
             [json5 ,(err #(json:unexpected string ,c 2 ,_))])))
       (iota 9))
    ,@(map         ; reverse solidus followed by non-escape characters
       (lambda (c)
         `(,(format "'\\~a'" c)
           [json ,(err #(json:unexpected value ,c 2 ,_))]
           [json5 ,(format "~a" c)]))
       (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

    (,(let ([op (open-output-string)])  ; hexadecimal escapes
        (write-char #\' op)
        (do ([i 0 (+ i 1)]) [(= i #xFF)]
          (let ([c (integer->char i)])
            (fprintf op "\\x~2,'0x" i)))
        (write-char #\' op)
        (get-output-string op))
     [json ,(err ,_)]                   ; any error is acceptable
     [json5 ,(list->string (map integer->char (iota #xFF)))])

    ;; string continuations
    ("'hello\\\n world'" [json ,(err #(json:unexpected value #\newline 7 ,_))] [json5 "hello world"])
    ("'hello\\\r world'" [json ,(err #(json:unexpected value #\return 7 ,_))] [json5 "hello world"])
    ("'hello\\\r\n world'" [json ,(err #(json:unexpected value #\return 7 ,_))] [json5 "hello world"])
    ("'hello\\\x2028; world'" [json ,(err #(json:unexpected value #\x2028 ,_ ,_))] [json5 "hello world"])
    ("'hello\\\x2029; world'" [json ,(err #(json:unexpected value #\x2029 ,_ ,_))] [json5 "hello world"])

    ,@(map                              ; unescaped control characters
       (lambda (n)
         (let ([c (integer->char n)])
           `(,(format "\"~a\"" c)
             [json ,(err #(json:unexpected string ,c 1 ,_))]
             [json5
              ,(if (memv c '(#\newline #\return))
                   (err #(json:unexpected string ,c 1 ,_))
                   (format "~a" c))])))
       (iota #x20))

    ;; arrays
    ("[]" ())                                         ; empty
    ("[[1]]" ((1)))                                   ; nested
    ("[1" ,(err #(json:unexpected array #!eof 2 ,_))) ; no close bracket
    ("[1," ,(err #(json:unexpected array #!eof 3 ,_))) ; no value
    ("[" ,(err #(json:unexpected array #!eof 1 ,_))) ; no value, no close bracket
    ("[," ,(err #(json:unexpected value #\, 1 ,_))) ; no value with comma
    ("[}" ,(err #(json:unexpected value #\} 1 ,_))) ; mismatched bracket
    ("[true,false,null,1,\"two\",3.0]" (#t #f null 1 "two" 3.0)) ; list of values
    ("[true,false,null,1,\"two\",3.0,]" ; trailing comma
     [json ,(err #(json:unexpected value #\] 29 ,_))]
     [json5 (#t #f null 1 "two" 3.0)])
    ("[,]" ,(err #(json:unexpected value #\, 1 ,_))) ; lone trailing comma
    ("[,true,false,null,1,\"two\",3.0,]"             ; leading comma
     ,(err #(json:unexpected value #\, 1 ,_)))
    ("[true false]" ,(err #(json:unexpected array #\f 6 ,_))) ; no comma

    ;; objects
    ("{}" ,(json:make-object))                              ; empty
    ("{,}" ,(err #(json:unexpected |object key| #\, 1 ,_))) ; lone comma
    ("{\"foo\":\"bar\"}" ,(json:make-object [foo "bar"])) ; single key/val
    ("{\"foo\":\"bar\",}"               ; trailing comma
     [json ,(err #(json:unexpected |object key| #\} 13 ,_))]
     [json5 ,(json:make-object [foo "bar"])])
    ("{\"foo\":\"bar\" \"foo2\":\"bar2\",}" ,(err #(json:unexpected object #\" 13 ,_))) ; no comma
    ("{,\"foo\":\"bar\",}" ,(err #(json:unexpected |object key| #\, 1 ,_))) ; leading comma
    ("{\"foo\":\"bar\" - }" ,(err #(json:unexpected object #\- 13 ,_))) ; trailing other character
    ("{\"foo\":\"bar\",\"baz\":123}"    ; multiple key/vals
     ,(json:make-object [foo "bar"] [baz 123]))
    ("{\"hex\":0xdeadbeef,\"other\":123}" ; multiple key/vals with hex value to start
     [json ,(err #(json:unexpected number #\x 8 ,_))]
     [json5 ,(json:make-object [hex #xdeadbeef] [other 123])])
    ("{\"foo\":[123]}" ,(json:make-object [foo '(123)])) ; nested array
    ("{\"foo\":{\"bar\":true}}"         ; nested object
     ,(json:make-object [,'foo (json:make-object [bar #t])]))
    ("{\"a\": true, \"a\": false}"      ; duplicate keys
     ;; JSON specs do not specify behavior. Current behavior is last key wins.
     ,(json:make-object [a #f]))
    ("{" ,(err #(json:unexpected |object key| #!eof 1 ,_))) ; no value, no close brace
    ("{]" ,(err #(json:unexpected |object key| #\] 1 ,_))) ; mismatched bracket
    ("{ \"a" ,(err #(json:unexpected string #!eof 4 ,_))) ; no close brace, incomplete key
    ("{\"a\"" ,(err #(json:unexpected |object value| #!eof 4 ,_))) ; no close brace, no colon
    ("{\"a\"_" ,(err #(json:unexpected |object value| #\_ 4 ,_))) ; no close brace, non-colon
    ("{\"a\":" ,(err #(json:unexpected |object value| #!eof 5 ,_))) ; no value
    ("{\"a\":1" ,(err #(json:unexpected object #!eof 6 ,_))) ; no close brace
    ("{\"a\":1," ,(err #(json:unexpected |object key| #!eof 7 ,_))) ; no close brace after comma
    ("{\"a\"}" ,(err #(json:unexpected |object value| #\} 4 ,_))) ; key, no value
    ("{1}" ,(err #(json:unexpected |object key| #\1 1 ,_))) ; invalid numeric key, no value

    ("{'foo':\"bar\"}"                  ; single quoted keys
     [json ,(err #(json:unexpected |object key| #\' 1 ,_))]
     [json5 ,(json:make-object [foo "bar"])])

    (,(ct:join #\newline                ; unquoted keys
        "{"
        "  hello: \"world\","
        "  _: 'underscore',"
        "  $: 'dollar sign',"
        "  one1: 'numerals',"
        "  _$_: 'multiple symbols',"
        "  $_$hello123world_$_: 'mixed',"
        "}")
     [json ,(err #(json:unexpected |object key| #\h 4 ,_))]
     [json5 ,(json:make-object
              [hello "world"]
              [_ "underscore"]
              [$ "dollar sign"]
              [one1 "numerals"]
              [_$_ "multiple symbols"]
              [$_$hello123world_$_ "mixed"])])
    ("{ true : 1 }"                     ; unquoted key, cannot be true
     [json ,(err #(json:unexpected |object key| #\t 2 ,_))]
     [json5 ,(err #(json:unexpected |object key| true 2 ,_))])
    ("{ false : 2 }"                   ; unquoted key, cannot be false
     [json ,(err #(json:unexpected |object key| #\f 2 ,_))]
     [json5 ,(err #(json:unexpected |object key| false 2 ,_))])
    ("{ null : 3 }"                     ; unquoted key, cannot be null
     [json ,(err #(json:unexpected |object key| #\n 2 ,_))]
     [json5 ,(err #(json:unexpected |object key| null 2 ,_))])

    ("{ \\u0061bc: 1 }" ; unquoted key, first character Unicode escaped
     [json ,(err #(json:unexpected |object key| #\\ 2 ,_))]
     [json5 ,(json:make-object [abc 1])])
    ("{ sig\\u03A3ma : 'sum' }"         ; Unicode escaped key
     [json ,(err #(json:unexpected |object key| #\s 2 ,_))]
     [json5 ,(json:make-object [sigΣma "sum"])])
    ("{ sig\x03A3;ma : 'sum' }"         ; Unicode non-escaped key
     [json ,(err #(json:unexpected |object key| #\s 2 ,_))]
     [json5 ,(json:make-object [sigΣma "sum"])])
    ("{ a\x200C;b : 'joined' }"         ; Unicode non-escaped ZWNJ
     [json ,(err #(json:unexpected |object key| #\a 2 ,_))]
     [json5 ,(json:make-object [a\x200C;b "joined"])])

    ("{ in\\valid : 'invalid' }"        ; invalid escaped key
     [json ,(err #(json:unexpected |object key| #\i 2 ,_))]
     [json5 ,(err #(json:unexpected |object key| #\v 5 ,_))])

    ("{ foo/*comment*/: 1 }" ; unquoted key, followed by block comment
     [json ,(err #(json:unexpected |object key| #\f 2 ,_))]
     [json5 ,(json:make-object [foo 1])])
    ("{ foo//comment\n: 1 }"  ; unquoted key, followed by line comment
     [json ,(err #(json:unexpected |object key| #\f 2 ,_))]
     [json5 ,(json:make-object [foo 1])])
    ("{ foo/"                         ; unquoted key, solidus then EOF
     [json ,(err #(json:unexpected |object key| #\f 2 ,_))]
     [json5 ,(err #(json:unexpected |object key| foo/ 2 ,_))])

    ;; gensym keys
    ("{\"#{foo lish}\":789,\"#{bar unique-name}\":456}"
     ,(json:make-object [,(gensym "bar" "unique-name") 456] [#{foo lish} 789]))
    ("{\"#{foo not-gensym\":123}"
     ,(json:make-object [,(string->symbol "#{foo not-gensym") 123]))
    ("{\"#{looks more like gensym}\":123}"
     ,(json:make-object [,(string->symbol "#{looks more like gensym}") 123]))

    ;; comments
    (,(ct:join #\newline                ; block comment
        "/* Top-level"
        "** multi-line"
        "** with tricky stars"
        "** block comment */"
        "true")
     [json ,(err #(json:unexpected value |/*| 0 ,_))]
     [json5 #t])
    ("/* comment */"                    ; only block comment
     [json ,(err #(json:unexpected value |/*| 0 ,_))]
     [json5 ,eof-object?])
    (,(ct:join #\newline                ; line comment
        "[1, // one"
        " 2, // two"
        " 3  // three"
        "]")
     [json ,(err #(json:unexpected value |//| 4 ,_))]
     [json5 (1 2 3)])
    ("// comment"                       ; only line comment
     [json ,(err #(json:unexpected value |//| 0 ,_))]
     [json5 ,eof-object?])
    ("[1, /* one"                    ; eof while reading block comment
     [json ,(err #(json:unexpected value |/*| 4 ,_))]
     [json5 ,(err #(json:unexpected comment #!eof 10 ,_))])
    ("[1, // one"                     ; eof while reading line comment
     [json ,(err #(json:unexpected value |//| 4 ,_))]
     [json5 ,(err #(json:unexpected array #!eof 10 ,_))])
    ("/- this is not a comment -/ false" ; solidus followed by non-comment mark
     ,(err #(json:unexpected value |/-| 0 ,_)))
    ("/" ,(err #(json:unexpected value #\/ 0 ,_)))

    ;; whitespace
    ("" ,eof-object?)                   ; no input
    (" " ,eof-object?)                  ; only whitespace
    ;; All 4 JSON whitespace characters before and after other tokens
    (" \t\r\n[ \t\r\n1 \t\r\n, \t\r\n2 \t\r\n, \t\r\n3 \t\r\n] \t\r\n" (1 2 3))

    ;; Most whitespace is outside the JSON spec
    ("{ \f a: true }"      ; page break
     [json ,(err #(json:unexpected |object key| #\page 2 ,_))]
     [json5 ,(json:make-object [a #t])])
    ("{ \x205F; a: true }"  ; Unicode Medium Mathematical Space (MMSP)
     [json ,(err #(json:unexpected |object key| #\x205F ,_ ,_))]
     [json5 ,(json:make-object [a #t])])
    ("{ \xFEFF; a: true }"              ; Byte order mark
     [json ,(err #(json:unexpected |object key| #\xFEFF ,_ ,_))]
     [json5 ,(json:make-object [a #t])])
    ))

;; These specific specs work when reading, but do not round trip.
(define test-specs-read-only
  `(("1e+1000" +inf.0)      ; integer with excessive positive exponent
    ("1e-1000" 0.0)         ; integer with excessive negative exponent
    ))

(define (test-objects json5?)
  (fold-right
   (lambda (spec acc)
     (match spec
       [(,input ,expected)
        (if (procedure? expected) acc (cons expected acc))]
       [(,input [json ,expected] [json5 ,expected5])
        (let* ([acc (if (procedure? expected) acc (cons expected acc))]
               [acc (if (or (not json5?) (procedure? expected5))
                        acc
                        (cons expected5 acc))])
          acc)]))
   '()
   test-specs))

(define test-objs (test-objects #f))
(define test-objs5 (test-objects #t))

(define (check-reader ls f)
  (define (mismatch input expected result json5?)
    `#(mismatch (input ,input)
        [,(if json5? 'JSON5 'JSON)
         (expected => ,(dump expected))
         (result => ,(dump result))]))
  (define (one-read input expected json5? acc)
    (let ([result (try (f input (json:read-options [json5? json5?])))])
      (cond
       [(equal? result expected) acc]
       [(and (hashtable? result)
             (hashtable? expected)
             (equal? (dump result) (dump expected)))
        acc]
       [(procedure? expected)
        (if (expected result)
            acc
            (cons (mismatch input (expected) result json5?) acc))]
       [else
        (cons (mismatch input expected result json5?) acc)])))
  (define (do-read input expected expected5 acc)
    (one-read input expected5 #t
      (one-read input expected #f acc)))
  (let lp ([ls ls] [acc '()])
    (match ls
      [()
       (unless (null? acc)
         (for-each pretty-print (reverse acc))
         (throw `#(FAILED ,(length acc))))]
      [(,first . ,rest)
       (match first
         [(,input ,expected)
          (lp rest
            (do-read input expected expected acc))]
         [(,input [json ,expected] [json5 ,expected5])
          (lp rest
            (do-read input expected expected5 acc))])])))

(mat read ()
  (check-reader (append test-specs test-specs-read-only)
    (lambda (str opts) (json:read (open-input-string str) opts)))
  ;; json:read should return each value each time its called, allowing
  ;; for a sequence of values.
  (match-let*
   ([,ip (open-input-string "1  \t  \n  2")]
    [1 (json:read ip)]
    [2 (json:read ip)]
    [,x (guard (eof-object? x)) (json:read ip)])
   'ok))

(mat string->object ()
  (check-reader (append test-specs test-specs-read-only)
    json:string->object)
  ;; json:string->object should fail if there is more than one value
  ;; in the input. Comments are allowed.
  (match-let*
   ([`(catch #(json:unexpected #f #\2 9 ,_))
     (try (json:string->object "1  \t  \n  2"))]
    [1 (json:string->object "1 // line comment"
         (json:read-options [json5? #t]))]
    [2 (json:string->object "2\n/* one final comment */"
         (json:read-options [json5? #t]))])
   'ok))

(mat bytevector->object ()
  ;; There are a few cases where multi-byte characters shift the
  ;; port-position in the error message. In those cases, we mark the
  ;; expected error as "any" so that any error is acceptable.
  (check-reader (append test-specs test-specs-read-only)
    (lambda (str opts)
      (json:bytevector->object (string->utf8 str) opts)))
  ;; json:bytevector->object should fail if there is more than one value
  ;; in the input. Comments are allowed.
  (match-let*
   ([`(catch #(json:unexpected #f #\2 9 ,_))
     (try (json:bytevector->object (string->utf8 "1  \t  \n  2")))]
    [1 (json:bytevector->object (string->utf8 "1 // line comment")
         (json:read-options [json5? #t]))]
    [2 (json:bytevector->object (string->utf8 "2\n/* one final comment */")
         (json:read-options [json5? #t]))])
   'ok))

(mat no-lookahead ()
  ;; unread-char allows only one character of lookahead. String and
  ;; bytevector ports may appear to support multiple unread-char
  ;; calls, but that behavior is incidental and not guaranteed. This
  ;; custom port wrapper causes unread-char to raise an error when
  ;; used incorrectly, emulating file and network stream behavior.  We
  ;; do not use custom-textual-input-port because we cannot correctly
  ;; compute port-position. See Chez Scheme's io.ss comments for
  ;; details.
  (define (make-wrapped-port ip)
    (define unread #f)
    (define handler
      (lambda args
        (match args
          [(port-name ,_) "wrapped port"]
          [(file-position ,_)
           (let ([pos (port-position ip)])
             (if unread
                 (- pos 1)
                 pos))]
          [(read-char ,_)
           (if unread
               (let ([c unread])
                 (set! unread #f)
                 c)
               (read-char ip))]
          [(peek-char ,_)
           (if unread
               unread
               (peek-char ip))]
          [(unread-char ,c ,_)
           (when unread
             (errorf 'wrapped-port "cannot call unread-char twice!"))
           (set! unread c)])))
    (make-input-port handler ""))
  (check-reader test-specs
    (lambda (str opts) (json:read (make-wrapped-port (open-input-string str)) opts))))

(define (check-round-trip ls f)
  (define (one-round-trip obj acc)
    (let* ([result (try (f obj))]
           [rd (dump result)]
           [od (dump obj)])
      (if (equal? od rd)
          acc
          (cons `#(mismatch (expected => ,od) (result => ,rd)) acc))))
  (let lp ([ls ls] [acc '()])
    (match ls
      [()
       (unless (null? acc)
         (for-each pretty-print (reverse acc))
         (throw `#(FAILED ,(length acc))))]
      [(,obj . ,rest)
       (lp rest (one-round-trip obj acc))])))

(define round-trip
  (case-lambda
   [(objs json5?) (round-trip objs json5? #f #f)]
   [(objs json5? custom-write inflate-object)
    (define (->string x)
      (cond
       [(or json5? custom-write)
        (json:object->string x #f
          (json:write-options
           [json5? json5?]
           [custom-write custom-write]))]
       [else (json:object->string x)]))
    (define (->bytevector x)
      (cond
       [(or json5? custom-write)
        (json:object->bytevector x #f
          (json:write-options
           [json5? json5?]
           [custom-write custom-write]))]
       [else (json:object->bytevector x)]))
    (define (string-> x)
      (cond
       [(or json5? inflate-object)
        (json:string->object x
          (json:read-options
           [json5? json5?]
           [inflate-object inflate-object]))]
       [else (json:string->object x)]))
    (define (bytevector-> x)
      (cond
       [(or json5? inflate-object)
        (json:bytevector->object x
          (json:read-options
           [json5? json5?]
           [inflate-object inflate-object]))]
       [else (json:bytevector->object x)]))
    (check-round-trip objs
      (lambda (obj) (string-> (->string obj))))
    (check-round-trip objs
      (lambda (obj) (bytevector-> (->bytevector obj))))
    'ok]))

(mat round-trip ()
  (round-trip test-objs #f)
  (round-trip test-objs5 #t)
  ;; show that json:write is immune to print-radix
  (parameterize ([print-radix 2])
    (round-trip test-objs #f)
    (round-trip test-objs5 #t)))

(mat write-object ()
  ;; verify syntax error is associated with the bad clause
  (assert-syntax-error
   (json:write-object op #f json:write
     [good "stuff"]
     [key 123 write-field-value nonsense])
   "invalid syntax"
   (lambda (input violation-form)
     (match (syntax->datum violation-form)
       [(key 123 write-field-value nonsense) 'ok])))

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
      [(,@by-write ,@by-write ,@by-write ,@by-write ,@by-write)
       ;; hit difference cases where we pre-compute all or part of a field write
       ;; at expand-time; ideally cp0 would take it further from here
       (let ()
         (define (go1 x)
           (json:write-object op #f json:write
             [bar "rel"]
             [foo "tstep"]
             [ace x])
           (get))
         (define (go2 x)
           (json:write-object op #f json:write
             [bar "rel"]
             [foo x]
             [ace "rbic"])
           (get))
         (define (go3 x)
           (json:write-object op #f json:write
             [bar x]
             [foo "tstep"]
             [ace "rbic"])
           (get))
         (define (go4 x y)
           (json:write-object op #f json:write
             [bar x]
             [foo "tstep"]
             [ace y])
           (get))
         (define (go5 x y z)
           (json:write-object op #f json:write
             [bar x]
             [foo z]
             [ace y])
           (get))
         (list (go1 "rbic") (go2 "tstep") (go3 "rel")
           (go4 "rel" "rbic")
           (go5 "rel" "rbic" "tstep")))]
      ["{\"pi\":3.14,\"zip\":null}"
       (begin
         (json:write-object op #f json:write
           [pi 3.14]
           [zip 'null])
         (get))]
      [,fail
       (guard (procedure? fail))
       (lambda ()
         ;; no error at expand time
         (json:write-object op #f
           (lambda (op x indent)
             (json:write op x indent
               (json:write-options
                [json5? #f])))
           [abc 123456]
           [bad +nan.0])
         (get))]
      [`(catch #(json:invalid-datum +nan.0)) (on-exit (get) (try (fail)))]
      [,write-field-value
       (lambda (op v indent wr)
         (if (vector? v)
             (fprintf op "[~{\"~r\"~^,~}]" (vector->list v))
             (wr op v indent)))]
      ["{\"abc\":[\"four\",\"five\",\"six\"],\"def\":[7,8,9],\"ghi\":3}"
       (begin
         (json:write-object op #f json:write
           [ghi 3]
           [abc '#(4 5 6) write-field-value]
           [def '(7 8 9) write-field-value])
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
      ;; interaction of json:custom-write with json:write-object
      ;; also mix in some indentation for good measure
      [,custom-write
       (lambda (op x indent wr)
         (and (box? x) (wr op (format "BOX ~a" (unbox x)) indent)))]
      [,expected
       (let ([obj (json:make-object
                   [abc 123]
                   [n0 (box 0)]
                   [n1 (list (box 1) (box 2))])])
         (json:write op obj 0 (json:write-options [custom-write custom-write]))
         (get))]
      [,@expected
       (parameterize ([json:custom-write custom-write])
         (json:write-object op 0 json:write
           [abc 123]
           [n0 (box 0)]
           [n1 (list (box 1) (box 2))])
         (get))]
      ;; exercise custom-write with write-field-value
      ["{\"aux\":11,\"zap\":[\"BOX 1\",2,\"BOX 3\"]}"
       (let ([write-vector
              (lambda (op v indent wr)
                (if (vector? v)
                    (let ([len (vector-length v)])
                      (do ([i 0 (fx+ i 1)] [pre #\[ #\,]) ((fx= i len))
                        (write-char pre op)
                        (wr op (vector-ref v i) indent))
                      (write-char #\] op))
                    (wr op v indent)))])
         (parameterize ([json:custom-write custom-write])
           (json:write-object op #f json:write
             [zap (vector (box 1) 2 (box 3)) write-vector]
             [aux 11])
           (get)))]
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
           (json:write-options
            [custom-write
             (lambda (op x indent wr)
               (cond
                [(vector? x)
                 (wr op (map (lambda (x) (format "~r" x)) (vector->list x)) indent)
                 #t]
                [else #f]))]))
         (get))]
      ["{\n  \"abc\": [\"four\",\"five\",\"six\"],\n  \"def\": [\n    7,\n    8,\n    9\n  ],\n  \"ghi\": 3\n}\n"
       (begin
         (json:write op
           (json:make-object
            [ghi 3]
            [abc '#(4 5 6)]
            [def '(7 8 9)])
           0
           (json:write-options
            [custom-write
             (lambda (op x indent wr)
               (cond
                [(vector? x)
                 ;; Explicitly bypasses pretty mechanisms.
                 (fprintf op "[~{\"~r\"~^,~}]" (vector->list x))
                 #t]
                [else #f]))]))
         (get))]
      [,goodbye (lambda (op x indent wr) (wr op "goodbye" indent))]
      ["{\n  \"list\": [\n    1,\n    2,\n    {\n      \"tiki\": \"torch\",\n      \"wiki\": \"goodbye\"\n    }\n  ]\n}\n"
       (begin
         (json:write op
           (json:make-object
            [list (list 1 2 'flubber)])
           0
           (json:write-options
            [custom-write
             (lambda (op x indent wr)
               (match x
                 [flubber
                  (json:write-object op indent wr
                    [tiki "torch"]
                    [wiki "hello" goodbye])
                  #t]
                 [,_ #f]))]))
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

(mat pretty-output-port ()
  ;; Verify json:pretty writes to current-output-port
  (let ([op (open-output-string)])
    (parameterize ([current-output-port op])
      (json:pretty #t))
    (match (get-output-string op)
      ["true\n" 'ok]))
  (let ([op (open-output-string)])
    (parameterize ([current-output-port op])
      (json:pretty #t (json:write-options [custom-write #f])))
    (match (get-output-string op)
      ["true\n" 'ok])))

(mat write-structural-char ()
  ;; json:pretty covers the internal cases of write-structural-char.
  (match-let*
   ([#(EXIT #(bad-arg json:write-structural-char #\A))
     (catch (json:write-structural-char #\A 'bad-indent 'bad-port))]
    [#(EXIT #(bad-arg json:write-structural-char bad-indent))
     (catch (json:write-structural-char #\{ 'bad-indent 'bad-port))]
    [#(EXIT #(bad-arg json:write-structural-char bad-port))
     (catch (json:write-structural-char #\{ #f 'bad-port))]
    [(,op ,_) (call-with-values open-bytevector-output-port list)]
    [#(EXIT #(bad-arg json:write-structural-char ,@op))
     (catch (json:write-structural-char #\{ #f op))])
   (define (test c indent)
     (let* ([op (open-output-string)]
            [new-indent (json:write-structural-char c indent op)])
       (list new-indent (get-output-string op))))
   ;; Writing a structural character with no indent should just give
   ;; the string containing the character.
   (for-each
    (lambda (c)
      (match-let*
       ([(#f ,s) (test c #f)])
       (unless (string=? s (format "~a" c))
         (throw `#(unexpected ,s)))))
    '(#\[ #\] #\{ #\} #\: #\,))))

(mat custom ()
  (define-tuple <obj> a b c)
  (define dne (box #t))
  (define (must-ref ht key)
    (let ([hit (json:ref ht key dne)])
      (if (eq? hit dne)
          (errorf 'must-ref "did not find ~s" key)
          hit)))
  (define (inflate-object x)
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
       (json:write-object op indent wr
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
  (define (custom-write2 op x indent wr)
    (or (custom-write op x indent wr)
        ;; We could make this more efficient by using json:write-structural-char
        ;; and looping over the vector instead of allocating an intermediate list.
        ;; We do the latter to exercise the case where `custom-write` tail-calls
        ;; `wr` on a JSON object and indent is #f.
        (and (vector? x)
             (wr op (vector->list x) indent))))
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
  (define (nop-inflate-object x) x)
  (define (nop-custom-write op x indent wr) #f)
  ;; test inert custom-write / inflate-object
  (round-trip test-objs #f nop-custom-write nop-inflate-object)
  (round-trip test-objs5 #t nop-custom-write nop-inflate-object)
  (match-let*
   ([,x (<obj> make
          [a "quick"]
          [b 'line]
          [c (list
              (<obj> make [a 1] [b 2] [c '(3 4 5)])
              (json:make-object
               [able (<obj> make [a 4] [b 'echo] [c 6])]
               [baker '("one" "two")]
               [charlie "delta"])
              (<obj> make
                [a 'typical]
                [b 'fore]
                [c (json:make-object [take "cake"])]))])]
    ;; use custom-write
    ;; first without indentation:
    [,string (json:object->string x #f (json:write-options [custom-write custom-write]))]
    [ok (compare x (json:string->object string (json:read-options [inflate-object inflate-object])))]
    [,bv (json:object->bytevector x #f (json:write-options [custom-write custom-write]))]
    [ok (compare x (json:bytevector->object bv (json:read-options [inflate-object inflate-object])))]
    ;; now indent:
    [,string (json:object->string x 0 (json:write-options [custom-write custom-write]))]
    [ok (compare x (json:string->object string (json:read-options [inflate-object inflate-object])))]
    [,bv (json:object->bytevector x 0 (json:write-options [custom-write custom-write]))]
    [ok (compare x (json:bytevector->object bv (json:read-options [inflate-object inflate-object])))])
   'ok)
  (match-let*
   ([,x (vector
         (vector 'one 2 '#("three" D))
         (<obj> make [a '#(x y z)] [b 4] [c 7]))]
    ;; now use custom-write2
    ;; without indentation
    [,string1 (json:object->string x #f (json:write-options [custom-write custom-write2]))]
    [,string2 (json:object->string x 0 (json:write-options [custom-write custom-write2]))]
    [#f (string=? string1 string2)]
    [,@string1 (pregexp-replace* "\\s" string2 "")]
    ;; By design, some vectors in the input are lists in the output.
    [,y (json:string->object string1 (json:read-options [inflate-object inflate-object]))]
    [,@y (let f ([x x]) ;; rebuild x suitable for comparison with y
           (match x
             [`(<obj> ,a ,b ,c)
              (<obj> make [a (f a)] [b (f b)] [c (f c)])]
             [,x (guard (vector? x)) (vector->list (vector-map f x))]
             [,x x]))])
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
    [,ip (open-input-string "wrong")]
    [#(EXIT #(bad-arg json:write ,@ip))
     (catch (json:write ip 123))]
    [(,op ,_) (call-with-values open-bytevector-output-port list)]
    [#(EXIT #(bad-arg json:write ,@op))
     (catch (json:write op 123))]
    [#(EXIT #(bad-arg json:write 3.0))
     (catch (json:write (open-output-string) "adventure" 3.0))]
    [,proc (lambda (x) x)]
    [#(EXIT #(bad-arg json:write ,@proc))
     (catch (json:write (open-output-string) "adventure" #f proc))]
    [#(EXIT #(bad-arg json:write 7)) (catch (json:write 7 #f 0))]
    [#(EXIT #(bad-arg json:write "typed object"))
     (catch (json:write "typed object" 123))]
    [#(EXIT #(bad-arg json:read "typed object"))
     (catch (json:read "typed object"))]
    [,proc (lambda () 'bad)]
    [#(EXIT #(bad-arg json:read ,@proc))
     (catch (json:read (open-input-string "123") proc))]
    [,op (open-output-string)]
    [#(EXIT #(bad-arg json:read ,@op))
     (catch (json:read op))]
    [,ip (open-bytevector-input-port #vu8(1 2 3 4 5))]
    [#(EXIT #(bad-arg json:read ,@ip))
     (catch (json:read ip))]
    [#(EXIT #(bad-arg json:pretty 9)) (catch (json:pretty #t 9))]
    [#(EXIT #(json:invalid-datum 1/2)) (catch (json:object->string 1/2))]
    [,json-opts (json:write-options [json5? #f])]
    [#(EXIT #(json:invalid-datum +inf.0)) (catch (json:object->string +inf.0 #f json-opts))]
    [#(EXIT #(json:invalid-datum -inf.0)) (catch (json:object->string -inf.0 #f json-opts))]
    [#(EXIT #(json:invalid-datum +nan.0)) (catch (json:object->string +nan.0 #f json-opts))]
    [#(EXIT #(json:invalid-datum (1 . 2))) (catch (json:object->string (cons 1 2) #f json-opts))]
    [#(EXIT #(json:unexpected |object value| #\, 6 ,_))
     (catch (json:string->object "{\"foo\",12}"))]
    [#(EXIT #(json:unexpected object #\: 6 ,_))
     (catch (json:string->object "{\"a\":1:}"))]
    [#(EXIT invalid-surrogate-pair) (catch (json:string->object "\"\\udc00\""))]
    [#(EXIT #(json:unexpected string #\" 7 ,_))
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
    [#(EXIT #(bad-arg json:cells apoptosis))
     (catch (json:cells 'apoptosis))]
    [#(EXIT #(bad-arg json:key<? "wrong"))
     (catch (json:key<? "wrong"))]
    [,proc1 (lambda (x) #t)]
    [#(EXIT #(bad-arg json:key<? ,@proc1))
     (catch (json:key<? proc1))]
    [#(EXIT #(bad-arg json:custom-write #t))
     (catch (json:custom-write #t))]
    [,proc3 (lambda (a b c) "bad")]
    [#(EXIT #(bad-arg json:custom-write ,@proc3))
     (catch (json:custom-write proc3))]
    [`(catch #(bad-arg stack->json 234)) (try (stack->json 234))]
    [,empty-stack (json:make-object [type "stack"])]
    [`(catch #(bad-arg stack->json port))
     (try (stack->json 'port empty-stack))]
    [`(catch #(bad-arg json-stack->string deck))
     (try (json-stack->string 'deck))]
    [,ip (open-input-string "")]
    [`(catch #(bad-arg json-stack->string ,@ip))
     (try (json-stack->string ip empty-stack))]
    [,op (let-values ([(op get) (open-bytevector-output-port)]) op)]
    [`(catch #(bad-arg json-stack->string ,@op))
     (try (json-stack->string op empty-stack))]
    [`(catch #(bad-arg json-stack->string 123))
     (try (json-stack->string (open-output-string) 123))])
   'ok))

;; default canonical ordering for JSON serialization
(mat stable-ordering ()
  (define (gen)
    (let* ([len (+ 1 (random 5))]
           [s (make-string len)])
      (do ([i 0 (fx+ i 1)]) ((fx= i len) (string->symbol s))
        (string-set! s i
          (integer->char (fx+ (char->integer #\a) (random 26)))))))
  (define (gen-keys n)
    ;; assumes n < size of the key space
    (let lp ([i n] [acc '()])
      (if (= i 0)
          acc
          (let ([key (gen)])
            (if (memq key acc)
                (lp i acc)
                (lp (- i 1) (cons key acc)))))))
  (define (scramble ls)
    (let ([v (list->vector ls)])
      (do ([i 0 (+ i 1)]) ((= i (vector-length v)) (vector->list v))
        (let ([j (random (+ i 1))])
          (unless (= i j)
            (let ([t (vector-ref v i)])
              (vector-set! v i (vector-ref v j))
              (vector-set! v j t)))))))
  (define (kvs->json kvs)
    (let ([obj (json:make-object)])
      (for-each
       (lambda (kv)
         (match-let* ([(,k . ,v) kv])
           (json:extend-object obj [,k v])))
       kvs)
      obj))
  (define (test n)
    (match-let*
     ([,kvs (map cons (gen-keys n) (iota n))]
      [,obj1 (kvs->json (scramble kvs))]
      [,str1 (json:object->string obj1)]
      [,obj2 (kvs->json (scramble kvs))]
      [,@str1 (json:object->string obj2)])
     'ok))
  (test 1)
  (test 10)
  (test 100)
  (test 1000))

(mat custom-ordering ()
  (define (test-write-object key<?)
    (let-values ([(op get) (open-string-output-port)])
      (parameterize ([json:key<? key<?])
        ((eval
          `(lambda (op)
             (json:write-object op #f json:write
               [foo2 2]
               [foo1 1]
               [foo10 10])))
         op)
        (get))))
  (define obj (json:make-object
               [foo2 2]
               [foo1 1]
               [foo10 10]))
  (define (test-write key<?)
    (let-values ([(op get) (open-string-output-port)])
      (parameterize ([json:key<? key<?])
        (json:write op obj)
        (get))))

  (match-let*
   (["{\"foo2\":2,\"foo1\":1,\"foo10\":10}" ; no sort
     (test-write-object #f)]
    ["{\"foo1\":1,\"foo10\":10,\"foo2\":2}" ; default string<?
     (test-write-object #t)]
    ["{\"foo2\":2,\"foo10\":10,\"foo1\":1}" ; custom (reverse)
     (test-write-object string>?)]
    ["{\"foo1\":1,\"foo2\":2,\"foo10\":10}" ; custom (natural)
     (test-write-object natural-string-ci<?)]
    ;; Particularly difficult to prove that json:write does not
    ;; sort. Here we serialize to a string, then deserialize to
    ;; compare that the new object matches the original.
    [,unsorted (test-write #f)]         ; no sort
    [,_ (assert
         (equal?
          (dump obj)
          (dump (json:string->object unsorted))))]

    ["{\"foo1\":1,\"foo10\":10,\"foo2\":2}" ; default string<?
     (test-write #t)]
    ["{\"foo2\":2,\"foo10\":10,\"foo1\":1}" ; custom (reverse)
     (test-write string>?)]
    ["{\"foo1\":1,\"foo2\":2,\"foo10\":10}" ; custom (natural)
     (test-write natural-string-ci<?)]
    )
   'ok)
  )

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
    [,ht (make-eq-hashtable)]
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

(mat print-parameters ()
  (parameterize ([print-radix 16] [print-precision 3] [print-subnormal-precision #t])
    (match-let*
     (["123456" (json:object->string 123456)]
      ["3141592653589793238462643" (json:object->string 3141592653589793238462643)]
      ["10.0" (json:object->string 10.0)]
      ["1e-315" (json:object->string 1e-315)])
     'ok)))

(mat internal-buffer ()
  ;; Verify that the internal buffer does not hold state after a
  ;; failure.
  (match-let*
   ([,opts (json:read-options [json5? #f])]
    [`(catch ,_) (try (json:string->object "\"foo\\xbar\"" opts))]
    ["baz" (json:string->object "\"baz\"" opts)])
   'ok)
  )

(mat extended-identifiers ()
  ;; JSON5 read syntax for identifiers limits to a ECMAScript subset
  ;; of characters, yet with Unicode escaping, it can encode almost
  ;; any character. By setting extended-identifiers? we can
  ;; Scheme-like identifiers when desired.
  (define specs
    `(("{kebab-case-identifier: 'kebab'}"
       [json ,(err #(json:unexpected |object key| #\k 1 ,_))]
       [json5 ,(json:make-object [kebab-case-identifier "kebab"])])
      ))
  (check-reader specs
    (lambda (str opts)
      (json:string->object str
        (json:read-options copy opts [extended-identifiers? #t]))))

  ;; Options
  (match-let*
   ([#t (begin (json:read-options [extended-identifiers? #f]) #t)]
    [#t (begin (json:read-options [extended-identifiers? #t]) #t)]
    [,nonsense (lambda (a) #t)]
    [`(catch #(bad-arg extended-identifiers? ,@nonsense))
     (try (json:read-options [extended-identifiers? nonsense]))])
   'ok))

(mat inflate-symbol ()
  ;; Setting inflate-symbol to values allows unquoted symbols as
  ;; values.
  (define specs
    `(("bare" bare)
      ("[a1,a2,a3]" (a1 a2 a3))
      ("{\"quoted_key\": unquoted_value}" ,(json:make-object [quoted_key 'unquoted_value]))
      ("{unquoted_key: unquoted_value}"
       [json ,(err #(json:unexpected |object key| #\u 1 ,_))]
       [json5 ,(json:make-object [unquoted_key 'unquoted_value])])
      ))

  (check-reader specs
    (lambda (str opts)
      (json:string->object str
        (json:read-options copy opts [inflate-symbol values]))))

  ;; Options
  (match-let*
   ([#t (begin (json:read-options [inflate-symbol #f]) #t)]
    [#t (begin (json:read-options [inflate-symbol (lambda (a) #t)]) #t)]
    [,proc2 (lambda (a b) #t)]
    [`(catch #(bad-arg inflate-symbol ,@proc2))
     (try (json:read-options [inflate-symbol proc2]))])
   'ok))
