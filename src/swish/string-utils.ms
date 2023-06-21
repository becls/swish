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

(mat wrap-text ()
  (define (wt w first-w rest-w text)
    (let ([os (open-output-string)])
      (wrap-text os w first-w rest-w text)
      (get-output-string os)))
  (match-let*
   ([,in "Not even room for 1 character. All must wrap."]
    [,s (join (split in #\space) #\newline)]
    [,@s (wt 0 0 0 in)]
    [,in "NoSpacesSoNothingWrapsDespiteBeingOverWidth"]
    [,@in (wt 0 0 0 in)]
    [,s (join '("Nothing" "split:" "newlines" "were" "preserved.") #\newline)]
    [,@s (wt 999 0 0 s)]
    [,s (join '("We" "preserve" "tabs" "but" "don't" "account" "for" "width") #\tab)]
    [,@s (wt 0 0 0 s)]
    [,s (join '("This will wrap" "and tab\twill" "fit as single" "character.") #\newline)]
    [,@s (wt 14 0 0 (join (split s #\newline) #\space))]
    [,ls '("0 1 2" "3 4 5" "6 7 8" "9")]
    [,s (join ls #\newline)]
    [,@s (wt 5 0 0 "0 1 2 3 4 5 6 7 8 9")]
    [,@s (wt 5 0 0 ls)]
    [,in "This      string contains    consecutive      spaces."]
    [,s (join '("This string" "contains" "consecutive" "spaces.") #\newline)]
    [,@s (wt 11 0 0 in)]
    [,in "      Leading spaces       and    some at   the end.           "]
    [,s (join '("Leading spaces and" "some at the end.") #\newline)]
    [,@s (wt 18 0 0 in)]
    ["foo\nbar" (wt 99 0 0 "   foo   \nbar")]
    ["foo\nbar" (wt 99 0 0 "   foo   \n    bar      ")]
    ["" (wt 999 0 0 "                         ")]
    ["\r\na b c \r\n" ;; \r is just another character, not a line marker
     (wt 10 0 0 "\r\n    a    b    c    \r\n")]
    [,ls
     '("This did not need to wrap, since we had plenty of room."
       "Even more text here that will also go on the same line."
       "Wow, this is going to be a pretty long line.")]
    [,in (join ls #\space)]
    [,@in (wt 1000 0 0 in)]
    [,@in (wt 1000 0 0 ls)]
    ["   Hi\nmom" (wt 0 3 0 "Hi mom")]
    ["   Hi\n mom" (wt 0 3 1 "Hi mom")]
    [" Hi\n   mom" (wt 0 1 3 "Hi mom")]
    ["   Hi\nmom\nand\ndad" (wt 0 3 0 "Hi mom and dad")]
    ["   Hi\n mom\n and\n dad" (wt 0 3 1 "Hi mom and dad")]
    [" Hi\n   mom\n   and\n   dad" (wt 0 1 3 "Hi mom and dad")]
    ["  Hi\n  mom" (wt 0 2 2 "Hi mom")]
    ["  Hi\n  mom\n  and\n  dad" (wt 0 2 2 "Hi mom and dad")]
    ["     Hi\n  mom\n  and\n  dad\n  just\n  two?"
     (wt 0 5 2 "Hi mom and\ndad\njust\ntwo?")]
    ["" (wt 10 0 0 "")]
    ["" (wt 10 0 0 '())]
    ["" (wt 10 0 0 "    ")]
    ["" (wt 10 0 0 '("" " " "   " " " ""))])
   'ok))

(mat oxford-comma ()
  (match-let*
   (;; defaults to ~{ ... ~}
    [,fmt1 (oxford-comma "~a" " and ")]
    ["" (format fmt1 '())]
    ["1" (format fmt1 '(1))]
    ["1 and 2" (format fmt1 '(1 2))]
    ["1, 2, and 3" (format fmt1 '(1 2 3))]
    ["1, 2, 3, and 4" (format fmt1 '(1 2 3 4))]
    ["foo and bar" (format fmt1 '("foo" "bar"))]
    ["foo, bar, and baz" (format fmt1 '("foo" "bar" "baz"))]
    ;; doing a little more with each element
    [,fmt2 (oxford-comma "~:(~a~)" " and ")]
    ["" (format fmt2 '())]
    ["Tofu" (format fmt2 '(tofu))]
    ["Beef and Broccoli" (format fmt2 '(beef broccoli))]
    ["Bacon, Lettuce, and Tomato" (format fmt2 '(bacon lettuce tomato))]
    ;; can use ~{ ... ~} to format each item
    [,fmt3 (oxford-comma "~{~s -> ~s~}" " or ")]
    ["" (format fmt3 '())]
    ["a -> b" (format fmt3 '((a b)))]
    ["a -> b or c -> d" (format fmt3 '((a b) (c d)))]
    ["a -> b, c -> d, or p -> q" (format fmt3 '((a b) (c d) (p q)))]
    ;; use ~@{ ... ~} as the outer layer to accept multiple arguments
    [,fmt4 (oxford-comma "~@{" "~1{~a:~a~:}" " or " "~}")]
    ["" (format fmt4)]
    ["1:2" (format fmt4 '(1 2))]
    ["1:2 or a:b" (format fmt4 '(1 2) '("a" b))]
    ["1:2, a:b, or flap:jack" (format fmt4 '(1 2) '(a b) '(flap "jack"))]
    ;; combining
    [,fmt5 (oxford-comma (oxford-comma "~a" " and ") " or ")]
    ["" (format fmt5 '())]
    ["one" (format fmt5 '((one)))]
    ["one and two" (format fmt5 '((one two)))]
    ["one, two, and three" (format fmt5 '((one two three)))]
    ["one or carbon" (format fmt5 '((one) (carbon)))]
    ["one and two or carbon and hydrogen" (format fmt5 '((one two) (carbon hydrogen)))]
    ["one, two, and three or carbon and hydrogen" (format fmt5 '((one two three) (carbon hydrogen)))]
    ["one, two, and three or carbon, hydrogen, and nitrogen" (format fmt5 '((one two three) (carbon hydrogen nitrogen)))]
    ["1, 2, and 3, a and b, or p, d, and q" (format fmt5 '((1 2 3) (a b) (p d q)))]
    ;; more uses for prefix / suffix
    [,fmt6 (oxford-comma "~@[~{" "~(~s~)" " or " "~}~]")]
    ["" (format fmt6 #f)]
    ["" (format fmt6 '())]
    ["foo" (format fmt6 '(FOO))]
    ["foo or bar" (format fmt6 '(FOO bar))]
    ;; maybe useful as argument of ~? or ~@?
    ["try pressing the control, alt, or delete key before continuing"
     (format "try ~a the ~@? key before continuing" 'pressing
       (oxford-comma "~a" " or ") '(control alt delete))]
    ["try pressing control, alt, and delete before continuing"
     (format "try ~a ~? before continuing" 'pressing
       (oxford-comma "~@{" "~a" " and " "~}") '(control alt delete))])
   'ok)
  )

(mat ct:join ()
  (match-let*
   (["combined" (expand '(ct:join "" "comb" "in" "e" "d"))]
    ["comb--in--e--d" (expand '(ct:join "--" "comb" "in" "e" "d"))]
    ["nu_cle_ar" (expand '(ct:join #\_ "nu" "cle" "ar"))]
    ["okay" (expand '(ct:join #\! "okay"))]
    ["someday-maybe" (expand '(ct:join #\- "someday" "maybe"))]
    ["" (expand '(ct:join "xyz"))]
    ["" (expand '(ct:join "" "" ""))]
    [(#2%string-append "join-ed-" x "-o-the-r-" y)
     (expand '(ct:join "-" "join" "ed" x "o" "the" "r" y))]
    [(#2%string-append x "---ceptional")
     (expand '(ct:join "---" x "ceptional"))]
    ;; residualize call to string-append in case y evaluates to non-string
    [(#2%string-append y)
     (expand '(ct:join "phone" y))]
    [(#2%string-append x "\nbr\nok\nen\n" y)
     (expand '(ct:join #\newline x "br" "ok" "en" y))]
    [(#2%string-append x "\nbr\nok\nen\n" y "\nzap")
     (expand '(ct:join #\newline x "br" "ok" "en" y "zap"))])
   'ok))

(mat ct:string-append ()
  (match-let*
   (["combined" (expand '(ct:string-append "comb" "in" "e" "d"))]
    ["xyz" (expand '(ct:string-append "xyz"))]
    ["" (expand '(ct:string-append "" "" ""))]
    [(#2%string-append "joined" x "other" y)
     (expand '(ct:string-append "join" "ed" x "o" "the" "r" y))]
    [(#2%string-append x "ceptional")
     (expand '(ct:string-append x "ceptional"))]
    [(#2%string-append "phone" y)
     (expand '(ct:string-append "phone" y))]
    [(#2%string-append 123 "broken" 777)
     (expand '(ct:string-append 123 "br" "ok" "en" 777))]
    [(#2%string-append 123 "broken" 777 "zap")
     (expand '(ct:string-append 123 "br" "ok" "en" 777 "zap"))]
    [(#2%string-append "abc" 123 "broken" 777 "zap")
     (expand '(ct:string-append "abc" 123 "br" "ok" "en" 777 "zap"))]
    [(#2%string-append "abc" 123 "broken" 777 "zap")
     (expand '(ct:string-append "a" "" "bc" 123 "br" "ok" "en" 777 "za" "p" "" ""))]
    [(#2%string-append "abc" (#2%string #\newline) "broken" (#2%string #\newline) "zap")
     (expand
      '(ct:string-append
        "a" "bc" (string #\newline)
        "br" "ok" "en" (string #\newline)
        "za" "p" ""))]
    [(#2%string-append "abc\nbr" ok "en\nzap")
     (expand
      '(ct:string-append
        "a" "bc" "\n"
        "br" ok "en" "\n"
        "za" "p" ""))]
    ["abc\nbroken\nzap"
     (expand
      '(ct:string-append
        "a" "bc" "\n"
        "br" "ok" "en" "\n"
        "za" "p" ""))])
   'ok)
  )

(mat natural-string ()
  (define (do-comparison case-sensitive? elide-whitespace? testing-string<?)
    (match-let*
     ([,check-string<?
       (lambda (a b)
         (let ([result (testing-string<? a b)])
           ;; Antisymmetry
           (when result (assert (not (testing-string<? b a))))
           (when (and elide-whitespace? (not (eq? a "")))
             (match-let*
              ([,@result (testing-string<? (string-append "   " a) b)]
               [,@result (testing-string<? a (string-append "   " b))]
               [,@result (testing-string<? (string-append a "   " ) b)]
               [,@result (testing-string<? a (string-append b "   "))])
              'ok))
           result))]
      [#f ;; irreflexive
       (ormap
        (lambda (s)
          (or (check-string<? s s)
              (check-string<? s (string-copy s))))
        '("" "abc" "0" "001" "-100" ".0" ".003" "0.9900" "-" "a0b1"))]
      [#t
       (andmap
        (lambda (n)
          (define (ok? fmt n1 n2)
            (and (check-string<? (format fmt n1) (format fmt n2))
                 (not (check-string<? (format fmt n2) (format fmt n1)))
                 (let ([s1 (format fmt n1)])
                   (and (check-string<? s1 (string-append s1 "a"))
                        (not (check-string<? (string-append s1 "a") s1))))))
          (define (try-context positive-only? fmt)
            (and (ok? fmt n (+ n 1))
                 (or positive-only? (ok? fmt (- n 1) n))
                 (or (<= n 0)
                     (and (or positive-only? (ok? fmt (- n) n))
                          (or positive-only? (not (ok? fmt n (- n))))
                          (ok? fmt n (* n 10))
                          (ok? fmt n (* n 2))))))
          (and (try-context #f "~a")
               (try-context #t "xyz 0~a Z") ;; leading zero
               (try-context #t "xyz 000~a") ;; leading zeros
               (try-context #t "Abc~a")
               (try-context #f "~aBcd")
               (try-context #f "~a...")
               (try-context #f "~a..0.00.1")
               (try-context #f "~a----")
               (try-context #t "X +~a")))
        `(0 1 9 10 20000 900001 12345 ,(most-positive-fixnum)
          ,(* 1000 (most-positive-fixnum))))]
      [#t (check-string<? "a" "b")]
      [#t (check-string<? "abc" "abcd")]
      [#t (check-string<? "abc1" "abcd")]
      [#f (check-string<? "abcd" "abc1")]
      [#f (check-string<? "abc11" "abc2")]
      [#t (check-string<? "abc2" "abc11")]
      ;; treat - as dash if it follows alphabetic or numeric character
      ;; otherwise, treat - as negation
      [#t (check-string<? "-2" "-1")]
      [#f (check-string<? "-1" "-2")]
      [#t (check-string<? "a-1" "a-2")]
      [#f (check-string<? "a-2" "a-1")]
      [#t (check-string<? "0-1" "0-2")]
      [#f (check-string<? "0-2" "0-1")]
      [#f (check-string<? "a -1" "a -2")]
      [#t (check-string<? "a -2" "a -1")]
      [#f (check-string<? "a\t-1" "a\t-2")]
      [#t (check-string<? "a\t-2" "a\t-1")]
      [#f (check-string<? "a\n-1" "a\n-2")]
      [#t (check-string<? "a\n-2" "a\n-1")]
      [#t (check-string<? "a[-2]" "a[1]")]
      [#f (check-string<? "a[1]" "a[-2]")]
      [#t (check-string<? "xyz,-2" "xyz,1")]
      [#f (check-string<? "xyz,1" "xyz,-2")]
      ;; no ratnums
      [#f (check-string<? "1/8" "1/2")]
      [#t (check-string<? "1/2" "1/8")]
      [#f (check-string<? "10/11/2027" "8/11/2027")]
      [#t (check-string<? "8/11/2027" "10/11/2027")]
      ;; we consider ASCII digits, not just anything char-numeric? accepts
      [#t (check-string<? "123 4" "123 ④")]
      [#f (check-string<? "123 ④" "123 4")]
      [#t (check-string<? "123 4" "123 ⑷")]
      [#f (check-string<? "123 ⑷" "123 4")]
      [#f (check-string<? "12" "4㆕")]
      [#t (check-string<? "4㆕" "12")]
      ;; make sure we continue after numeric comparison only if n1 == n2
      [#f (check-string<? "12.1 xyz .7" "12 xyz .9")]
      [#t (check-string<? "12 xyz .9" "12.1 xyz .7")]
      [#t (check-string<? "10 abc" "100 abc")]
      [#f (check-string<? "100 abc" "10 abc")]
      [#f (check-string<? "100 abc" "100 abc")]
      [#f (check-string<? "100 abc123" "100 abc12")]
      [#f (check-string<? "100 abc123" "100 abc123")]
      [#t (check-string<? "100 abc123" "100 abc1234")]
      [#t (check-string<? "100 abc0000123" "100 abc1234")]
      [#f (check-string<? "1a20b300c4321d-5000e6.012345_F_9.700" "1a20b300c4321d-5000e6.012345_F_9.700")]
      [#f (check-string<? "1a20b300c4321d-5000e6.012345_F_9.700" "1a20b300c4321d-5000e6.012345_F_9.70")]
      [#f (check-string<? "1a20b300c4321d-5000e6.012345_F_9.700" "1a20b300c4321d-5000e6.012345_F_9.7")]
      [#t (check-string<? "1a20b300c4321d-5000e6.012345_F_9.700" "1a20b300c4321d-5000e6.012345_F_9.800")]
      ;; first is greater than second at 012349 vs. 012345
      [#f (check-string<? "1a20b300c4321d-5000e6.012349_F_9.700" "1a20b300c4321d-5000e6.012345_F_9.800")]
      [#t (check-string<? "..." "....")]
      [#f (check-string<? "..." "...")]
      [#f (check-string<? "..." "..")]
      ;; not a decimal point, but number comes before character
      [#f (check-string<? "xyz.100" "xyz100")]
      [#t (check-string<? "xyz100" "xyz.100")]
      ;; #f because (string<? "xyz." "xyz ") => #f
      [#f (check-string<? "xyz. 100" "xyz 100")]
      [#t (check-string<? "xyz 100" "xyz .100")]
      [#t (check-string<? ".1.2.3.4" ".1.2.3.5")]
      [#f (check-string<? ".1.2.3.4" ".1.2.3.0")]
      [#f (check-string<? ".1.2.3.4" ".1.2.3")]
      [#t (check-string<? ".1.2.3.4" ".1.20.3.4")]
      [#t (check-string<? ".1.2.3.4" ".1.2.031.4")]
      [#t (check-string<? ".1.2.3.4" ".001.0002.03.40")]
      ;; since (char<? #\. #\a)
      [#t (check-string<? "1.a" "1a")]
      [#f (check-string<? "1a" "1.a")]
      ;; leading zero corner cases: shorter run comes first
      [#t (check-string<? "a 0" "a 000")]
      [#f (check-string<? "a 000" "a 0")]
      [#f (check-string<? "a 0" "a 0")]
      [#t (check-string<? "a 0 123" "a 000 123")]
      [#f (check-string<? "a 000 123" "a 0 123")]
      [#f (check-string<? "a 0 123" "a 0 123")]
      [#t (check-string<? "a 00 1 0" "a 00 1 00")]
      [#f (check-string<? "a 00 1 00" "a 00 1 0")]
      [#t (check-string<? "a 0 2 00" "a 00 1 0")]
      [#f (check-string<? "a 00 1 0" "a 0 1 00")]
      [#t (check-string<? "a 0 1 00" "a 00 1 00")]
      [#t (check-string<? "a 0 00 1 000 2" "a 00 00 1 00 2")]
      [#t (check-string<? "a 0 000 1 000 2" "a 00 00 1 00 2")]
      [#t (check-string<? "a 00 0 2 000 2" "a 00 00 1 00 2")]
      [#f (check-string<? "a 00 00 2 000 2" "a 00 00 1 00 2")]
      [#t (check-string<? "a 0 1 000 2" "a 00 1 000 2")]
      [#f (check-string<? "a 000 4" "a 0 001")]
      [#t (check-string<? "a 000 000 4" "a 000 0000 001")]
      [#f (check-string<? "a 000 0000 001" "a 000 000 4")])
     (if case-sensitive?
         (match-let*
          ([#t (check-string<? "ABC" "abc")]
           [#f (check-string<? "abc" "ABC")]
           [#f (check-string<? "10 four" "10 Four")]
           [#t (check-string<? "10 Four" "10 four")]
           [#t (check-string<? "a 00 1 X 0" "a 00 1 x 00")]
           [#t (check-string<? "a 00 1 000 X" "a 00 1 000 x")])
          'ok)
         (match-let*
          ([#f (check-string<? "ABC" "abc")]
           [#f (check-string<? "abc" "ABC")]
           [#t (check-string<? "abc" "ABd")]
           [#f (check-string<? "10 four" "10 Four")]
           [#f (check-string<? "10 Four" "10 four")]
           [#f (check-string<? "a 00 1 X 0" "a 00 1 x 0")]
           [#f (check-string<? "a 00 1 x 0" "a 00 1 X 0")]
           [#f (check-string<? "a 00 1 000 X" "a 00 1 000 x")]
           [#f (check-string<? "a 00 1 000 x" "a 00 1 000 X")])
          'ok))))
  (do-comparison #t #t natural-string<?)
  (do-comparison #f #t natural-string-ci<?)
  (let ()
    (import natural-string)
    (do-comparison #t #f (make-natural<? token<? get-integer-token))
    (do-comparison #f #f (make-natural<? token-ci<? get-integer-token))
    ;; check token length reported by get-integer-token
    (do-comparison #t #f
      (make-natural<? token<?
        (lambda (c s i end)
          (let-values ([(tok new-i len) (get-integer-token c s i end)])
            (when tok (assert (= tok (string->number (substring s (- new-i len) new-i)))))
            (values tok new-i len)))))
    (match-let*
     ([,size<?
       (make-natural<? token<?
         (lambda (c s i end)
           (let-values ([(tok i len) (get-integer-token c s i end)])
             (if (and (number? tok) (fx< i end))
                 (let ([j i] [new-len (fx+ len 1)])
                   (case (string-ref s j)
                     [(#\b #\B) (values tok j new-len)]
                     [(#\k #\K) (values (* tok 1024) j new-len)]
                     [(#\m #\M) (values (* tok 1024 1024) j new-len)]
                     [(#\g #\G) (values (* tok 1024 1024 1024) j new-len)]
                     [else (values tok i len)]))
                 (values tok i len)))))]
      [#t (size<? "255b" "1k")]
      [#f (size<? "1k" "255b")]
      [#t (size<? "32K" "2M")]
      [#f (size<? "2M" "32K")]
      [#t (size<? "512M" "1G")]
      [#f (size<? "1G" "512M")]
      [#t (size<? "1k" "1024")] ;; shorter
      [#f (size<? "1024" "1k")])
     'ok)
    (match-let*
     ([,bad<?
       (make-natural<? token<?
         (lambda (c s i end)
           (match c
             [#\x (values c "not fixnum" 1)]
             [#\y (values c (- i 1) 1)] ;; attempt retrograde motion
             [#\z (values c i "bad len")]
             [#\A (values c i -7)]
             [,_ (values #f i 0)])))]
      [#t (bad<? "abc" "abd")]
      [#f (bad<? "abd" "abc")]
      [`(catch #(bad-return-value "not fixnum")) (try (bad<? "abc" "abx"))]
      [`(catch #(bad-return-value "not fixnum")) (try (bad<? "abx" "abc"))]
      [`(catch #(bad-return-value 3)) (try (bad<? "abcde" "abcdy"))]
      [`(catch #(bad-return-value 3)) (try (bad<? "abcdy" "abcde"))]
      [`(catch #(bad-return-value "bad len")) (try (bad<? "abcde" "abcdz"))]
      [`(catch #(bad-return-value "bad len")) (try (bad<? "abcdz" "abcde"))]
      [`(catch #(bad-return-value -7)) (try (bad<? "abcde" "abcdA"))]
      [`(catch #(bad-return-value -7)) (try (bad<? "abcdA" "abcde"))])
     'ok)
    (match-let*
     ([,warp<?
       ;; cover case where tokenizer elides all the way to end
       (make-natural<? token<?
         (lambda (c s i end)
           (if (eqv? c #\!)
               (values #f end 0) ;; warp to end
               (values #f i 0)))
         (lambda (c s i end)
           (values c i end)))]
      [#t (warp<? "abcd!skip this" "abcd ")]
      [#f (warp<? "abcd " "abcd!skip this")]
      [#f (warp<? "abcd!skip this" "abcd")]
      [#f (warp<? "abcd" "abcd!skip this")]
      [#f (warp<? "ab!cd" "ab!cdefg")]
      [#f (warp<? "ab!cddef" "ab!c")])
     'ok)
    (match-let*
     ([,trust<?
       ;; cover case where we bypass safeguard (for one but not the other)
       (make-natural<? token<?
         (trust
          (lambda (c s i end)
            (if (eqv? c #\$)
                (values 'eos end 0)
                (values #f i 0))))
         (lambda (c s i end)
           (if (eqv? c #\!)
               (values #f "BAD" 0)
               (values c i end))))]
      [#t (trust<? "ab$cddef" "abc$")]
      [#f (trust<? "ab$cddef" "ab$c")]
      [`(catch #(bad-return-value "BAD")) (try (trust<? "abc!ddef" "abc"))])
     'ok)
    )
  )
