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
 (swish html)
 (swish mat)
 (swish testing)
 )

(mat html ()
  (match-let*
   (["&lt;this&gt;=&quot;fun &amp; games&quot;"
     (html:encode "<this>=\"fun & games\"")]
    ["" (html->string '())]
    ["" (html->string (void))]
    ["&lt;this&gt;=&quot;fun &amp; games&quot;"
     (html->string "<this>=\"fun & games\"")]
    ["12" (html->string 12)]
    ["" (html->string '(begin))]
    ["this" (html->string '(begin "this"))]
    ["this=1, that=-1"
     (html->string `(begin ,(void) "this=" (begin 1 ", that=") -1 ,(void)))]
    ["[!CDATA[]]" (html->string '(cdata))]
    ["[!CDATA[<this>]]" (html->string '(cdata "<this>"))]
    ["[!CDATA[<this>=\"fun & games\"]]"
     (html->string '(cdata "<this>" "=" "\"fun & games\""))]
    ["" (html->string '(raw))]
    ["<script></script>" (html->string '(script))]
    ["<script>one;two;</script>" (html->string '(script "one;" "two;"))]
    ["<style></style>" (html->string '(style))]
    ["<style>one;two;</style>" (html->string '(style "one;" "two;"))]
    ["<area><base><br><col><command><embed><hr><img><input><keygen><link><meta><param><source><track><wbr>"
     (html->string '(begin (area) (base) (br) (col) (command) (embed) (hr) (img) (input) (keygen) (link) (meta) (param) (source) (track) (wbr)))]
    ["<img>" (html->string '(img (@)))]
    ["<img>" (html->string `(img (@ ,(void))))]
    ["<img hidden display=\"false\" height=\"100\">"
     (html->string `(img (@ (hidden) (display "false") (height 100))))]
    ["<html></html>" (html->string '(html))]
    ["<!DOCTYPE html><html lang=\"en\"></html>" (html->string '(html5))]
    ["<!DOCTYPE html><html lang=\"fr\"></html>" (html->string '(html5 (@ (lang "fr"))))]
    ["<!DOCTYPE html><html lang=\"en\" xmlns=\"http://example.com/xhtml\"></html>"
     (html->string '(html5 (@ (xmlns "http://example.com/xhtml"))))]
    ["<!DOCTYPE html><html xmlns=\"http://example.com/xhtml\" lang=\"fr\"></html>"
     (html->string '(html5 (@ (xmlns "http://example.com/xhtml") (lang "fr"))))]
    ["<!DOCTYPE html><html lang></html>" (html->string '(html5 (@ (lang))))]
    ["<this>" (html->string '(raw "<this>"))]
    ["<this>=\"fun & games\""
     (html->string '(raw "<this>" "=" "\"fun & games\""))]
    [,s1 "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\"><title>Example</title></head><body style=\"font-family:monospace;\"><p>Plain<br>text</p><p>&quot;Text in quotes&quot; &amp; &lt;taglike&gt;</p><p>Ten: 10</p><p>Three halves: 3/2</p><p>One point five: 1.5</p></body></html>"]
    [,exp1 `(html5
             (head
              (meta (@ (charset "UTF-8")))
              (title "Example"))
             (body (@ (style "font-family:monospace;"))
               (p "Plain" (br) "text")
               (p "\"Text in quotes\" & <taglike>")
               (begin
                 (p "Ten: " 10)
                 (p "Three halves: " 3/2)
                 (p "One point five: " 1.5))))]
    [,@s1 (html->string exp1)]
    [,bv1 (string->utf8 s1)]
    [,@bv1 (html->bytevector exp1)]

    ;; script
    [,s2 "<head><script>function f(x) {  if (x < 0) {    return true;  } else if (x > 10) {    return false;  } else {    return x;}</script></head><body></body>"]
    [,exp2 `(begin
              (head
               (script
                "function f(x) {"
                "  if (x < 0) {"
                "    return true;"
                "  } else if (x > 10) {"
                "    return false;"
                "  } else {"
                "    return x;"
                "}"))
              (body))]
    [,@s2 (html->string exp2)]
    [,bv2 (string->utf8 s2)]
    [,@bv2 (html->bytevector exp2)]

    [#(EXIT #(bad-arg html->string foo))
     (catch (html->string 'foo))]
    [#(EXIT #(bad-arg html->string (begin . 0)))
     (catch (html->string '(begin . 0)))]
    [#(EXIT #(bad-arg html->string (cdata . 0)))
     (catch (html->string '(cdata . 0)))]
    [#(EXIT #(bad-arg html->string (cdata 0)))
     (catch (html->string '(cdata 0)))]
    [#(EXIT #(bad-arg html->string (raw . 0)))
     (catch (html->string '(raw . 0)))]
    [#(EXIT #(bad-arg html->string (raw 0)))
     (catch (html->string '(raw 0)))]
    [#(EXIT #(bad-arg html->string (script . 0)))
     (catch (html->string '(script . 0)))]
    [#(EXIT #(bad-arg html->string (script 0)))
     (catch (html->string '(script 0)))]
    [#(EXIT #(bad-arg html->string (style . 0)))
     (catch (html->string '(style . 0)))]
    [#(EXIT #(bad-arg html->string (style 0)))
     (catch (html->string '(style 0)))]
    [#(EXIT #(bad-arg html->string (foo . 0)))
     (catch (html->string '(foo . 0)))]
    [#(EXIT #(bad-arg html->string (foo (@ . 0))))
     (catch (html->string '(foo (@ . 0))))]
    [#(EXIT #(bad-arg html->string (foo (@ "bad"))))
     (catch (html->string '(foo (@ "bad"))))]
    [#(EXIT #(bad-arg html->string (foo (@ (1)))))
     (catch (html->string '(foo (@ (1)))))]
    [#(EXIT #(bad-arg html->string (foo (@ (1 "none")))))
     (catch (html->string '(foo (@ (1 "none")))))]
    [#(EXIT #(bad-arg html->string (foo (@ (bar baz)))))
     (catch (html->string `(foo (@ (bar baz)))))])
   (for-each
    (lambda (tag)
      (match-let*
       ([#(EXIT #(bad-arg html->string (,@tag . 0)))
         (catch (html->string `(,tag . 0)))]
        [#(EXIT #(bad-arg html->string (,@tag (raw))))
         (catch (html->string `(,tag (raw))))])
       'ok))
    '(area base br col command embed hr img input keygen link meta param
       source track wbr))
   'ok))
