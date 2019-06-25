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

(import
 (swish cli)
 (swish mat)
 (swish testing)
 )

(define-syntax make-ht
  (syntax-rules ()
    [(_ (key val) ...)
     (let ([ht (make-hashtable symbol-hash eq?)])
       (hashtable-set! ht 'key val)
       ...
       ht)]))

(define (dump x)
  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
  (cond
   [(hashtable? x)
    (vector-map
     (lambda (k) (cons k (dump (hashtable-ref x k #f))))
     (vector-sort symbol<? (hashtable-keys x)))]
   [(pair? x) (map dump x)]
   [(procedure? x) (dump (x))]
   [else x]))

(define (check ht1 ht2)
  (let ([t1 (dump ht1)]
        [t2 (dump ht2)])
    (unless (equal? t1 t2)
      (raise `#(not-equal ,t1 ,t2)))))

(define (check-error x str)
  (match x
    [#(EXIT ,reason)
     (let ([err (exit-reason->english reason)])
       (unless (string=? err str)
         (raise `#(not-equal ,err ,str))))]))

(define (check-error-or x . strs)
  (match x
    [#(EXIT ,reason)
     (let ([err (exit-reason->english reason)])
       (unless (member err strs)
         (raise `#(never-equal ,err ,@strs))))]))

(define (gen-spec-macro name short long type help options)
  ;; We want to make sure we try each alphabetic and numeric character
  ;; as they might directly appear in code. For all other characters,
  ;; some may be special to the reader, so we'll use the hex encoding.
  (let ([op (open-output-string)])
    (fprintf op "[")
    (fprintf op "~@[~s~]" name)
    (when short
      (fprintf op " ")
      (cond
       [(not (char? short))
        (fprintf op "~a" short)]
       [(memq short '(#\i #\I)) (fprintf op "|-~a|" short)]
       [(or (char-alphabetic? short) (char-numeric? short))
        (fprintf op "-~a" short)]
       [else (fprintf op "-\\x~x;" (char->integer short))]))
    (fprintf op "~@[ --~a~]" long)
    (fprintf op "~@[ ~s~]" type)
    (fprintf op "~@[ ~s~]" help)
    (fprintf op "~@[ (options~{ ~a~})~]" options)
    (fprintf op "]")
    (get-output-string op)))

(define (realize-specs ls)
  (eval (read (open-input-string (format "(cli-specs ~{ ~a~})" ls)))))

(mat cli-specs-macro ()
  (assert-syntax-error (cli-specs [name -short bool ""])
    "invalid -short in")
  (assert-syntax-error (cli-specs [name -s boolean ""])
    "invalid boolean in")
  (assert-syntax-error (cli-specs [name -s counter ""])
    "invalid counter in")
  (assert-syntax-error (cli-specs [name -s (string bad) ""])
    "invalid (string bad) in")
  (assert-syntax-error (cli-specs [name -s (list bad) ""])
    "invalid (list bad) in")
  (assert-syntax-error (cli-specs [name -i bool ""])
    "use |-i| or |-I| in")
  (assert-syntax-error (cli-specs [name -I bool ""])
    "use |-i| or |-I| in")
  (assert-syntax-error (cli-specs [name -s bool "" (usage)])
    "invalid (usage) in")
  (assert-syntax-error (cli-specs [name -s bool "" (usage flubber)])
    "invalid (usage flubber) in")

  (do ([i 0 (+ i 1)]) ((= i #xFF))
    (unless (<= #xD800 i #xDFFF)
      (let* ([c (integer->char i)]
             [name (string->symbol (format "char:~a" i))]
             [r (catch (realize-specs
                        (list (gen-spec-macro name c #f 'bool "" #f))))]
             [tuple (list (<arg-spec> make
                            [name name]
                            [type 'bool]
                            [short c]
                            [long #f]
                            [help ""]
                            [conflicts '()]
                            [requires '()]
                            [usage '(fit (opt (and (or short long) args)))]))])
        (cond
         [(or (char-numeric? c) (char-whitespace? c))
          ;; The macro and the tuple should both produce an error
          (match r
            [#(EXIT ,_)
             (match (catch (parse-command-line-arguments tuple '()))
               [#(EXIT ,_) (void)])])]
         [else
          ;; The macro succeeds, so the check-specs pass on the tuple
          ;; should succeed
          (match r
            [(`(<arg-spec>))
             (unless (char=? c #\-) ; -- is actually a valid long option
               (match r [,@tuple 'ok])
               (parse-command-line-arguments tuple '()))])])))))

(mat check-specs ()
  (define (try-specs specs partial-success?)
    (match-let*
     ([#(EXIT ,r) (catch (parse-command-line-arguments specs '()))]
      [#(EXIT ,@r) (catch (display-help "" specs #f (open-output-string)))])
     (cond
      [partial-success?
       (display-usage "" "" specs #f (open-output-string))
       (display-options specs #f (open-output-string))
       (format-spec (car specs) '(or short long args))]
      [else
       (match-let*
        ([#(EXIT ,@r) (catch (display-usage "" "" specs #f (open-output-string)))]
         [#(EXIT ,@r) (catch (display-options specs #f (open-output-string)))]
         [#(EXIT ,@r)
          (cond
           [(> (length specs) 1)
            ;; When there are multiple specs, we're testing the
            ;; duplicate-option case, which cannot happen from
            ;; format-spec.
            `#(EXIT ,r)]
           [else (catch (format-spec (car specs) '(or short long args)))])])
        'ok)])
     r))
  (define (full specs)
    (try-specs specs #f))
  (define (partial specs)
    (try-specs specs #t))
  (match-let*
   ([(,valid) (cli-specs [name -s --long bool "help"])]
    [#(bad-spec name 12 ,_)
     (full (list (<arg-spec> copy valid [name 12])))]
    [#(bad-spec short 12 ,_)
     (full (list (<arg-spec> copy valid [short 12])))]
    [#(bad-spec long 12 ,_)
     (full (list (<arg-spec> copy valid [long 12])))]
    [#(bad-spec type boolean ,_)
     (full (list (<arg-spec> copy valid [type 'boolean])))]
    [#(bad-spec type counter ,_)
     (full (list (<arg-spec> copy valid [type 'counter])))]
    [#(bad-spec type (string bad) ,_)
     (full (list (<arg-spec> copy valid [type '(string bad)])))]
    [#(bad-spec type (list bad) ,_)
     (full (list (<arg-spec> copy valid [type '(list bad)])))]
    [#(bad-spec help 12 ,_)
     (full (list (<arg-spec> copy valid [help 12])))]
    [#(bad-spec usage 12 ,_)
     (full (list (<arg-spec> copy valid [usage 12])))]
    [#(bad-spec usage () ,_)
     (full (list (<arg-spec> copy valid [usage '()])))]
    [#(bad-spec usage (flubber) ,_)
     (full (list (<arg-spec> copy valid [usage '(flubber)])))]
    [#(bad-spec conflicts 12 ,_)
     (full (list (<arg-spec> copy valid [conflicts 12])))]
    [#(bad-spec conflicts ("flubber") ,_)
     (full (list (<arg-spec> copy valid [conflicts '("flubber")])))]
    [#(bad-spec requires 12 ,_)
     (full (list (<arg-spec> copy valid [requires 12])))]
    [#(bad-spec requires ("flubber") ,_)
     (full (list (<arg-spec> copy valid [requires '("flubber")])))]
    [#(bad-spec duplicate-spec name ,_)
     (full (list valid (<arg-spec> copy valid [short #\a])))]
    [#(bad-spec missing-specs (does-not-exist) ,_)
     (partial (list (<arg-spec> copy valid [requires '(does-not-exist)])))]
    [#(bad-spec missing-specs (does-not-exist) ,_)
     (partial (list (<arg-spec> copy valid [conflicts '(does-not-exist)])))]
    ;; Function Specific
    [#(EXIT #(bad-spec duplicate-option-spec #\s ,_))
     (catch (parse-command-line-arguments
             (list valid (<arg-spec> copy valid [name 'name2]))
             '()))]
    [#(EXIT #(bad-spec duplicate-option-spec "long" ,_))
     (catch (parse-command-line-arguments
             (list valid (<arg-spec> copy valid [name 'name2] [short #\t]))
             '()))])
   'ok))

(mat bad-args ()
  (match-let*
   ([#(EXIT #(bad-arg parse-command-line-arguments 12))
     (catch (parse-command-line-arguments '() 12))]
    [#(EXIT #(bad-arg parse-command-line-arguments (1 2 3)))
     (catch (parse-command-line-arguments '() (list 1 2 3)))]
    [#(EXIT #(bad-arg parse-command-line-arguments 12))
     (catch (parse-command-line-arguments '() '() 12))]
    [#(EXIT #(bad-arg format-spec 12))
     (catch (format-spec (car (cli-specs default-help)) 12))]
    [#(EXIT #(bad-arg display-usage 12))
     (catch (display-usage 12 #f #f #f #f))]
    [#(EXIT #(bad-arg display-usage 12))
     (catch (display-usage "" 12 #f #f #f))]
    [#(EXIT #(bad-arg display-usage -12))
     (catch (display-usage "" "" #f -12 #f))]
    [#(EXIT #(bad-arg display-usage 12))
     (catch (display-usage "" "" #f #f 12))]
    [#(EXIT #(bad-arg display-options 12))
     (catch (display-options #f 12 #f))]
    [#(EXIT #(bad-arg display-options 12))
     (catch (display-options #f #f  12))]
    [#(EXIT #(bad-arg display-help 12))
     (catch (display-help 12 #f #f #f))]
    [#(EXIT #(bad-arg display-help 12))
     (catch (display-help "" #f 12 #f))]
    [#(EXIT #(bad-arg display-help 12))
     (catch (display-help "" #f #f 12))]
    [#(EXIT #(bad-arg help-wrap-width -12))
     (catch (help-wrap-width -12))])
   'ok))

(mat misc ()
  (define simple
    (cli-specs
     [b -b bool "boolean"]
     [c -c count "count"]
     [l -l --list (list "l" ...) "list"]
     [s -s --string (string "s") "string"]))
  (define ls '("-b" "-c" "-s" "foo" "-l" "item1" "item2"))

  (define usage "Usage: Whoa! [-bc] [-l l ...] [-s s]\n")
  (define options
    (join '("  -b                boolean"
            "  -c                count"
            "  -l, --list l ...  list"
            "  -s, --string s    string\n")
      #\newline))
  (define help (string-append usage "\n" options))

  (check (parameterize ([command-line-arguments ls])
           (parse-command-line-arguments simple))
    (make-ht
     [b #t]
     [c 1]
     [l (list "item1" "item2")]
     [s "foo"]))
  (match-let*
   ([,opt (parse-command-line-arguments simple ls)]
    [#(EXIT #(no-spec-with-name invalid)) (catch (opt 'invalid))]
    [,@usage
     (let ([op (open-output-string)])
       (parameterize ([current-output-port op])
         (display-usage "Usage:" "Whoa!" simple))
       (get-output-string op))]
    [,@options
     (let ([op (open-output-string)])
       (parameterize ([current-output-port op])
         (display-options simple))
       (get-output-string op))]
    [,@help
     (let ([op (open-output-string)])
       (parameterize ([current-output-port op])
         (display-help "Whoa!" simple))
       (get-output-string op))])
   'ok))

(mat custom-fail ()
  (define problems '())
  (define (reset) (set! problems '()))
  (define (fail . ls) (set! problems (cons ls problems)))

  (match-let*
   ([,_ (reset)]

    ;; take-pos failures
    [,opt (parse-command-line-arguments (cli-specs) '("pos") fail)]
    [(("too many arguments: ~s" ("pos"))) problems]
    [,_ (reset)]
    [,opt (parse-command-line-arguments (cli-specs [pos (list "p1" "p2") ""])
            '("pos") fail)]
    [(("option expects value: ~a" "p1 p2")) problems]

    ;; take-named failures
    [,_ (reset)]
    [,opt (parse-command-line-arguments (cli-specs [s -s (string "s1") ""])
            '("-s") fail)]
    [(("option expects value: ~a ~a" "-s" "s1")) problems]
    [,_ (reset)]
    [,opt (parse-command-line-arguments (cli-specs [l -l (list "l1" "l2") ""])
            '("-l") fail)]
    [(("option expects value: ~a ~a" "-l" "l1 l2")) problems]

    ;; take-opt failures
    [,_ (reset)]
    [,opt (parse-command-line-arguments (cli-specs) '("--long") fail)]
    [(("unexpected ~a" "--long")) problems]
    [,_ (reset)]
    [,opt (parse-command-line-arguments (cli-specs) '("-s") fail)]
    [(("unexpected ~a" "-s")) problems] )
   'ok))

(mat unexpected ()
  (check-error (catch (parse-command-line-arguments '() '("-f")))
    "Exception: unexpected -f.")
  (check-error (catch (parse-command-line-arguments '() '("--foo")))
    "Exception: unexpected --foo."))

(mat named-bool ()
  (define simple
    (cli-specs
     [simple-flag -s bool "simple flag only allowed once"]))
  (check (parse-command-line-arguments simple '("-s"))
    (make-ht [simple-flag #t]))
  (check-error (catch (parse-command-line-arguments simple '("-s" "-s")))
    "Exception: duplicate option -s.")
  (check-error (catch (parse-command-line-arguments simple '("-ss")))
    "Exception: duplicate option -s."))

(mat named-count ()
  (define simple
    (cli-specs
     [counted-flag -c count
      "flag that is counted when specified multiple times"]))
  (check (parse-command-line-arguments simple '("-c"))
    (make-ht [counted-flag 1]))
  (check (parse-command-line-arguments simple '("-c" "-c"))
    (make-ht [counted-flag 2]))
  (check (parse-command-line-arguments simple '("-cc"))
    (make-ht [counted-flag 2]))
  (check (parse-command-line-arguments simple '("-c" "-c" "-c"))
    (make-ht [counted-flag 3]))
  (check (parse-command-line-arguments simple '("-ccc"))
    (make-ht [counted-flag 3])))

(mat named-single ()
  (define simple
    (cli-specs
     [simple-value -s (string "<val>") "simple value only allowed once"]))
  (check-error (catch (parse-command-line-arguments simple '("-s")))
    "Exception: option expects value: -s <val>.")
  (check (parse-command-line-arguments simple '("-s" "here"))
    (make-ht [simple-value "here"]))
  (check-error (catch (parse-command-line-arguments simple '("-s" "here" "-s" "there")))
    "Exception: duplicate option -s.")
  ;; The empty string is a valid value.
  (check (parse-command-line-arguments simple '("-s" ""))
    (make-ht [simple-value ""])))

(mat positional-single ()
  (define simple
    (cli-specs
     [simple-value (string "<val>") "simple value only allowed once"]))
  (check (parse-command-line-arguments simple '("one"))
    (make-ht [simple-value "one"]))
  (check-error (catch (parse-command-line-arguments simple '("one" "two")))
    "Exception: too many arguments: (\"one\" \"two\").")
  ;; The empty string is a valid value.
  (check (parse-command-line-arguments simple '(""))
    (make-ht [simple-value ""])))

(mat named-list ()
  (define simple
    (cli-specs
     [multiple-value -m (list "x")
      "value that is accumulated when specified multiple times"]))
  (check-error (catch (parse-command-line-arguments simple '("-m")))
    "Exception: option expects value: -m x.")
  (check (parse-command-line-arguments simple '("-m" "here"))
    (make-ht [multiple-value (list "here")]))
  (check (parse-command-line-arguments simple '("-m" "here" "-m" "there"))
    (make-ht [multiple-value (list "here" "there")])))

(mat named-list/many ()
  (define cli
    (cli-specs
     [many --many (list "x" ...) "list of many things"]
     [simple-flag -s bool "simple flag only allowed once"]))
  (check (parse-command-line-arguments cli '("--many" "one"))
    (make-ht [many (list "one")]))
  (check (parse-command-line-arguments cli '("--many" "one" "two"))
    (make-ht [many (list "one" "two")]))
  (check (parse-command-line-arguments cli '("--many" "one" "two" "three"))
    (make-ht [many (list "one" "two" "three")]))
  (check (parse-command-line-arguments cli '("--many" "one" "two" "--many" "three"))
    (make-ht [many (list "one" "two" "three")]))
  (check (parse-command-line-arguments cli '("--many" "one" "two" "-s" "--many" "three"))
    (make-ht [many (list "one" "two" "three")] [simple-flag #t])))

(mat positional-list/many ()
  (define cli
    (cli-specs
     [many (list "x" ...) "list of many things"]
     [simple-flag -s bool "simple flag only allowed once"]))
  (check (parse-command-line-arguments cli '("one"))
    (make-ht [many (list "one")]))
  (check (parse-command-line-arguments cli '("one" "two"))
    (make-ht [many (list "one" "two")]))
  (check (parse-command-line-arguments cli '("one" "two" "three"))
    (make-ht [many (list "one" "two" "three")]))
  (check (parse-command-line-arguments cli '("one" "two" "-s"))
    (make-ht [many (list "one" "two")] [simple-flag #t])))

(mat named-list/pattern ()
  (define cli
    (cli-specs
     [two --two (list "one" "two") "list of two things"]))
  (check-error (catch (parse-command-line-arguments cli '("--two" "one")))
    "Exception: option expects value: --two one two.")
  (check (parse-command-line-arguments cli '("--two" "one" "two"))
    (make-ht [two (list "one" "two")])))

(mat named-list/pattern-rest ()
  (define simple
    (cli-specs
     [simple-value -s (string "<val>") "simple value only allowed once"]
     [cmd -c (list "a" "b" . "x") "all the rest"]))
  (check-error (catch (parse-command-line-arguments simple '("-c" "one")))
    "Exception: option expects value: -c a b x ....")
  (check (parse-command-line-arguments simple '("-s" "thing1" "-c" "one" "two"))
    (make-ht
     [simple-value "thing1"]
     [cmd (list "one" "two")]))
  (check (parse-command-line-arguments simple '("-s" "abc" "-c" "1" "2" "3" "4"))
    (make-ht
     [simple-value "abc"]
     [cmd (list "1" "2" "3" "4")])))

(mat positional-list/pattern ()
  (define cli
    (cli-specs
     [two (list "one" "two") "list of two things"]
     [one (list "one") "list of one thing"]
     [three (list "one" "two" "three") "list of three things"]))
  (check (parse-command-line-arguments cli '("1/2" "2/2" "1/1" "1/3" "2/3" "3/3"))
    (make-ht
     [one (list "1/1")]
     [three (list "1/3" "2/3" "3/3")]
     [two (list "1/2" "2/2")]))
  (check-error (catch (parse-command-line-arguments cli '("1/2" "2/2" "1/1" "1/3")))
    "Exception: option expects value: one two three.")
  (check (parse-command-line-arguments cli '("1/2" "2/2"))
    (make-ht [two (list "1/2" "2/2")]))
  (check-error (catch (parse-command-line-arguments cli '("1/2")))
    "Exception: option expects value: one two."))

(mat positional-list/rest ()
  (define simple
    (cli-specs
     [simple-value -s (string "<val>") "simple value only allowed once"]
     [rest (list . "x") "all the rest"]))
  (check (parse-command-line-arguments simple '("-s" "thing1"))
    (make-ht [simple-value "thing1"]))
  (check (parse-command-line-arguments simple '("-s" "thing1" "thing2"))
    (make-ht [simple-value "thing1"] [rest (list "thing2")]))
  (check (parse-command-line-arguments simple '("thing2" "-s" "thing1"))
    (make-ht [rest (list "thing2" "-s" "thing1")])))

(mat positional-list/pattern-rest ()
  (define simple
    (cli-specs
     [simple-value -s (string "<val>") "simple value only allowed once"]
     [cmd (list "a" "b" . "x") "all the rest"]))
  (check-error (catch (parse-command-line-arguments simple '("one")))
    "Exception: option expects value: a b x ....")
  (check (parse-command-line-arguments simple '("-s" "thing1" "one" "two"))
    (make-ht
     [simple-value "thing1"]
     [cmd (list "one" "two")]))
  (check (parse-command-line-arguments simple '("-s" "abc" "1" "2" "3" "4"))
    (make-ht
     [simple-value "abc"]
     [cmd (list "1" "2" "3" "4")])))

(mat dashdash ()
  (define cli
    (cli-specs
     [simple-flag -s bool "simple flag only allowed once"]
     [dash -- (list . "x") "help for --"]))
  (check (parse-command-line-arguments cli '("--"))
    (make-ht [dash '()]))
  (check (parse-command-line-arguments cli '("--" "foo"))
    (make-ht [dash '("foo")]))
  (check (parse-command-line-arguments cli '("-s" "--" "foo"))
    (make-ht [dash '("foo")] [simple-flag #t]))
  (check (parse-command-line-arguments cli '("--" "-s" "foo"))
    (make-ht [dash '("-s" "foo")]))
  (check (parse-command-line-arguments cli '("--" "-s" "--" "foo"))
    (make-ht [dash '("-s" "--" "foo")])))

(mat requires1 ()
  (define cli
    (cli-specs
     [profile --profile (string "<file>") "update the specified profile"]
     [coverage --coverage (string "<file>") "write coverage report to <file>"
      (requires '(profile))]))
  (check-error (catch (parse-command-line-arguments cli '("--coverage" "coverage.html")))
    "Exception: --coverage requires --profile.")
  (check (parse-command-line-arguments cli '("--coverage" "coverage.html" "--profile" "profile.dat"))
    (make-ht
     [coverage "coverage.html"]
     [profile "profile.dat"])))

(mat requires2 ()
  (define cli
    (cli-specs
     [a -a bool "option A"]
     [b -b bool "option B requires A" (requires '(a))]
     [c -c bool "option C requires B" (requires '(b))]
     [d -d bool "option D requires A and B" (requires '(a b))]))
  (check (parse-command-line-arguments cli '("-a"))
    (make-ht [a #t]))
  (check-error (catch (parse-command-line-arguments cli '("-b")))
    "Exception: -b requires -a.")
  ;; we don't compute transitive closure
  (check-error (catch (parse-command-line-arguments cli '("-c")))
    "Exception: -c requires -b.")
  (check-error-or (catch (parse-command-line-arguments cli '("-d")))
    "Exception: -d requires -a, -b."
    "Exception: -d requires -b, -a.")
  (check (parse-command-line-arguments cli '("-abcd"))
    (make-ht [a #t] [b #t] [c #t] [d #t])))

(mat conflicts1 ()
  (define cli
    (cli-specs
     [a -a --a bool "option A does not work with option B"
      (conflicts '(b))]
     [b -b --b bool "option B does not work with option A"
      (conflicts '(a))]))
  (check (parse-command-line-arguments cli '("-a"))
    (make-ht [a #t]))
  (check (parse-command-line-arguments cli '("-b"))
    (make-ht [b #t]))
  (check-error-or (catch (parse-command-line-arguments cli '("-a" "-b")))
    "Exception: --b conflicts with --a."
    "Exception: --a conflicts with --b."))

(mat conflicts2 ()
  (define cli
    (cli-specs
     [a -a bool "option A does not work with option C"]
     [b -b bool "option B does not work with option C"]
     [c -c bool "option C does not work with option A or B"
      (conflicts '(a b))]))
  (check (parse-command-line-arguments cli '("-a"))
    (make-ht [a #t]))
  (check (parse-command-line-arguments cli '("-b"))
    (make-ht [b #t]))
  (check (parse-command-line-arguments cli '("-c"))
    (make-ht [c #t]))
  (check-error-or (catch (parse-command-line-arguments cli '("-abc")))
    "Exception: -c conflicts with -a, -b."
    "Exception: -c conflicts with -b, -a."))

(mat format-spec ()
  (match-let*
   ([(,valid) (cli-specs [name -s --long bool "help"])]
    ["-s" (format-spec valid 'short)]
    ["--long" (format-spec valid 'long)]
    [#f (format-spec (<arg-spec> copy valid [type 'bool]) 'args)]
    [#f (format-spec (<arg-spec> copy valid [type 'count]) 'args)]
    ["<str>" (format-spec (<arg-spec> copy valid [type '(string "<str>")]) 'args)]
    ["" (format-spec (<arg-spec> copy valid [type '(list)]) 'args)]
    ["a" (format-spec (<arg-spec> copy valid [type '(list "a")]) 'args)]
    ["a b" (format-spec (<arg-spec> copy valid [type '(list "a" "b")]) 'args)]
    ["a b ..." (format-spec (<arg-spec> copy valid [type '(list "a" "b" ...)]) 'args)]
    ["a b ..." (format-spec (<arg-spec> copy valid [type '(list "a" . "b")]) 'args)]
    ["-s" (format-spec valid '(or short long))]
    ["--long" (format-spec valid '(or long short))]
    ["-s --long" (format-spec valid '(and short long))]
    ["[-s]" (format-spec valid '(opt short))]
    ["[--long]" (format-spec valid '(opt long))]

    [(,req) (cli-specs [name -s --long bool "help" (usage (req short))])]
    ["-s" (format-spec req)]
    [(,req) (cli-specs [name -s --long bool "help" (usage (req long))])]
    ["--long" (format-spec req)]
    [(,opt) (cli-specs [name -s --long bool "help" (usage (opt short))])]
    ["[-s]" (format-spec opt)]
    [(,opt) (cli-specs [name -s --long bool "help" (usage (opt long))])]
    ["[--long]" (format-spec opt)]
    ["[-s]" (format-spec valid #f)]
    )
   'ok))

(mat help ()
  (define (test exe-name specs args regexps)
    (match-regexps regexps
      (split
       (let ([op (open-output-string)])
         (display-help exe-name specs args op)
         (get-output-string op))
       #\newline)))
  (define (usage expected specs)
    (test "myexe" specs #f
      (list
       (if (string=? expected "")
           "^Usage: myexe$"
           (format "^Usage: myexe ~a$" (pregexp-quote expected))))))

  (test "myexe" '() #f '("^Usage: myexe$"))
  (test "myexe" (cli-specs default-help) #f
    '("^Usage: myexe \\[-h\\]$"
      seek
      "-h, --help\\s+display this help and exit"))

  ;; named booleans
  (usage "[-n]" (cli-specs [named -n bool "a boolean"]))
  (usage "[--named]" (cli-specs [named --named bool "a boolean"]))
  (usage "[-n]" (cli-specs [named -n --named bool "a boolean"]))

  ;; named counts
  (usage "[-n]" (cli-specs [named -n count "a count"]))
  (usage "[--named]" (cli-specs [named --named count "a count"]))
  (usage "[-n]" (cli-specs [named -n --named count "a count"]))

  ;; named string
  (usage "[-n <val>]" (cli-specs [named -n (string "<val>") "a value"]))
  (usage "[--named <val>]" (cli-specs [named --named (string "<val>") "a value"]))
  (usage "[-n <val>]" (cli-specs [named -n --named (string "<val>") "a value"]))

  ;; named list one
  (usage "[-n <val>]" (cli-specs [named -n (list "<val>") "a value"]))
  (usage "[--named <val>]" (cli-specs [named --named (list "<val>") "a value"]))
  (usage "[-n <val>]" (cli-specs [named -n --named (list "<val>") "a value"]))

  ;; named list many
  (usage "[-n <val> ...]"
    (cli-specs [named -n (list "<val>" ...) "many values"]))
  (usage "[--named <val> ...]"
    (cli-specs [named --named (list "<val>" ...) "many values"]))
  (usage "[-n <val> ...]"
    (cli-specs [named -n --named (list "<val>" ...) "many values"]))

  ;; named list rest
  (usage "[-n <val> ...]"
    (cli-specs [named -n (list . "<val>") "rest values"]))
  (usage "[--named <val> ...]"
    (cli-specs [named --named (list . "<val>") "rest values"]))
  (usage "[-n <val> ...]"
    (cli-specs [named -n --named (list . "<val>") "rest values"]))

  ;; positionals
  (usage "<pos>" (cli-specs [pos (string "<pos>") "a string positional"]))
  (usage "<pos> ..." (cli-specs [pos (list "<pos>" ...) "a many positional"]))
  (usage "<pos> ..." (cli-specs [pos (list . "<pos>") "a rest positional"]))

  ;; help contains whitespace only should not indent at all
  (test "help" (cli-specs [named -n bool " "]) #f '(seek "^  -n$"))

  ;; help ends in whitespace should not write trailing whitespace
  (test "help"
    (cli-specs [named -n bool "named help   "])
    #f
    '(seek "^  -n\\s{16}named help$"))

  ;; single-line where left column is exactly sized to make the right
  ;; column start on the next line followed by an arg list that has
  ;; wrapped onto the following line, also the arg uses non-word,
  ;; non-whitespace characters
  (test "help"
    (cli-specs
     [regexp-pass -r (list "<type>" "<regexp>")
      "report <regexp> matches as <type>={info|warning|error}"])
    (make-ht [regexp-pass (list "info" "Zonk:.*")])
    '(seek
      "^  -r <type> <regexp>$"
      "^\\s{20}report <regexp> matches as <type>=\\{info\\|warning\\|error\\}$"
      "^\\s{20}\\(info Zonk:\\.\\*\\)$"))

  ;; multi-line help that will coalesce with an arg that uses
  ;; non-word, non-whitespace characters.
  (test "help"
    (cli-specs
     [format --format (string "<format>")
      '("format specifiers that include the following"
        "substitution strings:"
        "%file, %type, %line, %column, %bfp, %efp, %msg")])
    (make-ht [format "%file:%type:%line:%msg"])
    '(seek
      "^  --format <format> format specifiers that include the following substitution$"
      "^\\s{20}strings: %file, %type, %line, %column, %bfp, %efp, %msg$"
      "^\\s{20}\\(%file:%type:%line:%msg)$"))

  ;; multi-line help
  (test "multi"
    (cli-specs [multi -m --multi bool
                '("line one\n"
                  "line two\n"
                  "line three")])
    #f
    '(seek
      "^  -m, --multi       line one$"
      "^\\s{20}line two$"
      "^\\s{20}line three$"))

  ;; multi-line help with arg
  (test "multi"
    (cli-specs [multi -m --multi bool
                '("line one\n"
                  "line two\n"
                  "line three")])
    (make-ht [multi "input"])
    '(seek
      "^  -m, --multi       line one$"
      "^\\s{20}line two$"
      "^\\s{20}line three \\(input\\)$"))

  ;; custom formatting: multiple specified newlines
  (test "multi"
    (cli-specs [multi -m --multi bool
                '("line one\n\n"
                  "line three")])
    #f
    '(seek
      "^  -m, --multi       line one$"
      "^$"
      "^\\s{20}line three$"))

  ;; custom formatting: whitespace followed by explicit newline
  (test "multi"
    (cli-specs [multi -m --multi bool
                '("line one \n"
                  "line two")])
    #f
    '(seek
      "^  -m, --multi       line one$"
      "^\\s{20}line two$"))

  ;; display-help uses generic hashtable-ref so we can supply json object as
  ;; the optional args parameter
  (test "json"
    (cli-specs
     [foo -f --foo (string "<dest>") "help 1"]
     [bar -b (string "<filter>") "help 2"]
     [jar -j (string "<ball>") "help 3"])
    (json:make-object [foo "pocalypse"] [bar "none"])
    '(seek
      "^  -f, --foo <dest>  help 1 \\(pocalypse\\)"
      "^  -b <filter>       help 2 \\(none\\)"
      "^  -j <ball>         help 3$"))
  )

(mat usage ()
  (define (test prefix exe-name specs width regexps)
    (match-regexps regexps
      (split
       (let ([op (open-output-string)])
         (display-usage prefix exe-name specs width op)
         (get-output-string op))
       #\newline)))
  (define (usage expected specs width)
    (test "Usage:" "myexe" specs width
      (list
       (if (string=? expected "")
           "^Usage: myexe$"
           (format "^Usage: myexe ~a$" (pregexp-quote expected))))))

  (usage "" '() #f)

  (let ([specs (cli-specs [name --named bool "help"])])
    (usage "[--named]" specs #f)
    (usage "[--named]" specs 22)
    (usage "[options]" specs 0))

  (let ([specs (cli-specs [name --named bool "help"]
                 [name2 -m --named2 bool "help"]
                 [name3 -o bool "help" (usage show opt)])])
    (usage "[-mo] [--named]" specs #f)
    (usage "[-mo] [--named]" specs 28)
    (usage "[-o] [options]" specs 0)
    (usage "[-o] [options]" specs 27))

  (let ([specs (cli-specs [name --named (string "<s>") "help"]
                 [name3 -o bool "help" (usage show opt)])])
    (usage "[-o] [--named <s>]" specs #f)
    (usage "[-o] [--named <s>]" specs 31)
    (usage "[-o] [options]" specs 0)
    (usage "[-o] [options]" specs 30))

  (let ([specs (cli-specs [name (string "pos") "help"])])
    (usage "pos" specs #f)
    (usage "pos" specs 16)
    (usage "pos" specs 0))

  (let ([specs (cli-specs [name (string "pos") "help" (usage opt)])])
    (usage "[pos]" specs #f)
    (usage "[pos]" specs 18)
    (usage "[pos]" specs 0))

  (let ([specs
         (cli-specs
          [o -o --opt bool "" (usage opt)]
          [r -r --req count "" (usage (req short))]
          [y -y --yes count "" (usage show)]
          [s -s --short (string "<s>") "" (usage short)]
          [l -l --long (list "<l>" ...) "" (usage long)]
          [p (list "<p>" ...) ""])])
    (usage "[-oy] [-s <s>] [--long <l> ...] -r <p> ..." specs #f)
    (usage "[-y] [options] <p> ..." specs 0)
    (usage "[-y] [options] <p> ..." specs 35)

    (usage "[-oy] [options] <p> ..." specs 36)
    (usage "[-y] [options] -r <p> ..." specs 38)
    (parameterize ([help-wrap-width 38])
      (usage "[-y] [options] -r <p> ..." specs #f))
    (usage "[-oy] [options] -r <p> ..." specs 39)
    (usage "[-oy] [-s <s>] [options] -r <p> ..." specs 48)
    (usage "[-oy] [-s <s>] [options] -r <p> ..." specs 54)
    (usage "[-oy] [-s <s>] [--long <l> ...] -r <p> ..." specs 55))

  (let ([specs
         (let ()
           (define template (car (cli-specs [a -a bool ""])))
           (define (make-short i basis)
             (<arg-spec> copy template
               [name (string->symbol (format "~a~a" basis i))]
               [short (integer->char (+ i (char->integer basis)))]))
           (do ([i 0 (+ i 1)]
                [ls '() (cons (make-short i #\a) ls)])
               ((= i 26) (reverse ls))))])
    (usage "[options]" specs 0)
    (usage "[options]" specs 26)
    (usage "[-a] [options]" specs 27)
    (usage "[-ab] [options]" specs 28)
    (usage "[-abc] [options]" specs 29)
    (usage "[-abcdefghijklmnopqrstuvwxyz]" specs #f)
    (usage "[-abcdefghijklmnopqrstuvwxyz]" specs 42)
    (usage "[-abcdefghijklmno] [options]" specs 41)
    (usage "[-abcdefghijklmn] [options]" specs 40)
    (usage "[-abcdefghijklm] [options]" specs 39)))
