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
 (swish mat)
 (swish string-utils)
 )

(mat join ()
  (match-let*
   (["" (join '() #\,)]
    ["1" (join '(1) #\,)]
    ["1,2" (join '(1 2) #\,)]
    ["1,2,3" (join '(1 2 3) #\,)]
    ["1, 2, 3 and 4" (join '(1 2 3 4) ", " " and ")])
   'ok))

(mat symbols ()
  (match-let*
   ([|| (symbol-append)]
    [foo (symbol-append 'foo)]
    [foobar (symbol-append 'foo 'bar)])
   'ok))

(mat starts-with ()
  (match-let*
   ([#t (starts-with? "" "")]
    [#t (starts-with? "a" "")]
    [#f (starts-with? "" "a")]
    [#t (starts-with? "a" "a")]
    [#t (starts-with? "ab" "a")]
    [#f (starts-with? "a" "ab")]
    [#f (starts-with? "a" "b")]
    [#t (starts-with? "abc" "ab")]
    [#f (starts-with? "abc" "ac")]
    [#f (starts-with? "ABC" "abc")]
    [#t (starts-with-ci? "" "")]
    [#t (starts-with-ci? "a" "")]
    [#f (starts-with-ci? "" "a")]
    [#t (starts-with-ci? "a" "a")]
    [#t (starts-with-ci? "ab" "a")]
    [#f (starts-with-ci? "a" "ab")]
    [#f (starts-with-ci? "a" "b")]
    [#t (starts-with-ci? "abc" "ab")]
    [#f (starts-with-ci? "abc" "ac")]
    [#t (starts-with-ci? "ABC" "abc")]
    [#t (starts-with-ci? "abc" "ABC")])
   'ok))

(mat ends-with ()
  (match-let*
   ([#t (ends-with? "" "")]
    [#t (ends-with? "a" "")]
    [#f (ends-with? "" "a")]
    [#t (ends-with? "a" "a")]
    [#t (ends-with? "ba" "a")]
    [#f (ends-with? "a" "ba")]
    [#f (ends-with? "a" "b")]
    [#t (ends-with? "cba" "ba")]
    [#f (ends-with? "cba" "ca")]
    [#f (ends-with? "CBA" "cba")]
    [#t (ends-with-ci? "" "")]
    [#t (ends-with-ci? "a" "")]
    [#f (ends-with-ci? "" "a")]
    [#t (ends-with-ci? "a" "a")]
    [#t (ends-with-ci? "ba" "a")]
    [#f (ends-with-ci? "a" "ba")]
    [#f (ends-with-ci? "a" "b")]
    [#t (ends-with-ci? "cba" "ba")]
    [#f (ends-with-ci? "cba" "ca")]
    [#t (ends-with-ci? "CBA" "cba")]
    [#t (ends-with-ci? "cba" "CBA")])
   'ok))

(mat split ()
  (match-let*
   ([("") (split "" #\,)]
    [("one") (split "one" #\,)]
    [("one" "two") (split "one,two" #\,)]
    [("one" "two" "three") (split "one,two,three" #\,)]
    [("1,2,3,4") (split-n "1,2,3,4" #\, 1)]
    [("1" "2,3,4") (split-n "1,2,3,4" #\, 2)]
    [("1" "2" "3,4") (split-n "1,2,3,4" #\, 3)]
    [("1" "2" "3" "4") (split-n "1,2,3,4" #\, 4)]
    [("1" "2" "3" "4") (split-n "1,2,3,4" #\, 5)])
   'ok))

(mat dates ()
  (match-let*
   (["Sun, 1 Jan 2012 00:00:00 -0400"
     (format-rfc2822 (make-date 0 0 0 0 1 1 2012 (* -4 3600)))]
    ["Mon, 2 Feb 2015 01:02:03 +0000"
     (format-rfc2822 (make-date 0 3 2 1 2 2 2015 0))]
    ["Tue, 4 Mar 2014 09:59:59 +0200"
     (format-rfc2822 (make-date 0 59 59 9 4 3 2014 (* 2 3600)))]
    ["Wed, 1 Apr 2015 10:00:10 +0830"
     (format-rfc2822 (make-date 0 10 0 10 1 4 2015 (* 17/2 3600)))]
    ["Thu, 1 May 2014 00:00:00 +0000"
     (format-rfc2822 (make-date 0 0 0 0 1 5 2014 0))]
    ["Fri, 10 Jun 2016 00:00:00 +0000"
     (format-rfc2822 (make-date 0 0 0 0 10 6 2016 0))]
    ["Sat, 4 Jul 2015 00:00:00 +0000"
     (format-rfc2822 (make-date 0 0 0 0 4 7 2015 0))]
    ["Mon, 31 Aug 2015 00:00:00 +0000"
     (format-rfc2822 (make-date 0 0 0 0 31 8 2015 0))]
    ["Wed, 30 Sep 2015 00:00:00 +0000"
     (format-rfc2822 (make-date 0 0 0 0 30 9 2015 0))]
    ["Sat, 31 Oct 2015 23:59:59 +0000"
     (format-rfc2822 (make-date 0 59 59 23 31 10 2015 0))]
    ["Mon, 30 Nov 2015 00:00:00 +0000"
     (format-rfc2822 (make-date 0 0 0 0 30 11 2015 0))]
    ["Thu, 24 Dec 2015 07:00:00 +0000"
     (format-rfc2822 (make-date 0 0 0 7 24 12 2015 0))])
   'ok))

(mat trim-whitespace ()
  (define ws
    (let ([os (open-output-string)])
      (do ([i 0 (+ i 1)]) ((= i #xFFFF))
        (unless (<= #xD800 i #xDFFF)
          (let ([c (integer->char i)])
            (when (char-whitespace? c)
              (write-char c os)))))
      (get-output-string os)))
  (match-let*
   (["" (trim-whitespace "")]
    ["" (trim-whitespace ws)]
    ["a b c" (trim-whitespace (string-append ws "a b c" ws))]
    ["xyz" (trim-whitespace "xyz")]
    ["x y z" (trim-whitespace "x y z")]
    ["left" (trim-whitespace "    left")]
    ["right" (trim-whitespace "right    ")]
    ["A" (trim-whitespace " \n A \n ")]
    ["Z" (trim-whitespace "Z")])
   'ok))
