#!chezscheme
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

(library (swish cli)
  (export
   <arg-spec>
   cli-specs
   display-help
   display-options
   display-usage
   format-spec
   help-wrap-width
   parse-command-line-arguments
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish errors)
   (swish meta)
   (swish pregexp)
   (swish string-utils)
   )

  (define-tuple <arg-spec>
    name                            ; symbol that appears in output ht
    type                            ;
    short                           ; #f | character
    long                            ; #f | string
    help                            ; string describing argument
    conflicts                       ; list of names
    requires                        ; list of names
    usage                           ; list of [show|hide|fit] and [long|short|req|opt|<how>]
    )

  (define (positional? s)
    (match s
      [`(<arg-spec> [short #f] [long #f]) #t]
      [,_ #f]))

  (define-syntax valid-short-char?
    (syntax-rules ()
      [(_ short)
       (let ([s short])
         (and (not (char=? s #\-))
              (not (char-numeric? s))
              (not (char-whitespace? s))))]))

  (define-syntax valid-type?
    (syntax-rules ()
      [(_ type short long)
       (match type
         [bool (or short long)]
         [count (or short long)]
         [(string ,s) (guard (string? s)) #t]
         [(list . ,patterns)
          (let lp ([patterns patterns])
            (match patterns
              [() #t]
              [,p (guard (string? p)) #t]
              [(,p (... ...)) (guard (string? p)) #t]
              [(,p . ,patterns) (guard (string? p)) (lp patterns)]
              [,_ #f]))]
         [,_ #f])]))

  (define-syntax valid-usage-how?
    (syntax-rules ()
      [(_ how)
       (let ()
         (define (valid-how? x)
           (match x
             [long #t]
             [short #t]
             [args #t]
             [(req ,x) (valid-how? x)]
             [(opt ,x) (valid-how? x)]
             [(and . ,rest) (andmap valid-how? rest)]
             [(or . ,rest) (andmap valid-how? rest)]
             [,_ #f]))
         (valid-how? how))]))

  (define-syntax extract-usage
    (syntax-rules ()
      [(_ usage)
       (partition (lambda (x) (memq x '(fit hide show))) usage)]))

  (define-syntax valid-usage?
    (syntax-rules ()
      [(_ usage-expr)
       (let ([usage usage-expr])
         (and (list? usage)
              (let-values ([(vis rest) (extract-usage usage)])
                (and (<= (length vis) 1)
                     (match rest
                       [(,how) (valid-usage-how? how)]
                       [,_ #f])))))]))

  (define-syntax (cli-specs x)

    (define (syntax->string x)
      (match (syntax->datum x)
        [-i (syntax-error (replace-source x #'spec) "use |-i| or |-I| in")]
        [,x (guard (symbol? x)) (format "~a" x)]
        [,_ #f]))
    (define (short? x)
      (let ([s (syntax->string x)])
        (and s
             (= (string-length s) 2)
             (char=? (string-ref s 0) #\-)
             (valid-short-char? (string-ref s 1)))))
    (define (long? x)
      (let ([s (syntax->string x)])
        (and s
             (>= (string-length s) 2)
             (char=? (string-ref s 0) #\-)
             (char=? (string-ref s 1) #\-))))
    (define (get-short x)
      (and x (string-ref (syntax->string x) 1)))
    (define (get-long x)
      (and x (let ([s (syntax->string x)])
               (substring s 2 (string-length s)))))
    (define (get-default-usage short long)
      (cond
       [short (values 'fit 'opt)]
       [long (values 'fit 'opt)]
       [else (values 'show 'req)]))
    (define (populate-defaults short long usage)
      (define base-how `(and (or short long) args))
      (define (either x y) (if (pair? x) x (list y)))
      (let-values ([(vis rest) (extract-usage usage)]
                   [(def-vis def-req) (get-default-usage short long)])
        (append
         (either vis def-vis)
         (match rest
           [(,x . ,rest)
            (guard (memq x '(opt req)))
            (cons `(,x ,base-how) rest)]
           [(,x . ,rest)
            (guard (memq x '(long short)))
            (cons `(,def-req (and ,x args)) rest)]
           [(,x . ,_)
            (guard (valid-usage-how? x))
            rest]
           [,_ (append rest (list `(,def-req ,base-how)))]))))

    (define (spec-maker spec name short long type help optionals)
      (let ([type (syntax->datum type)])
        (unless (valid-type? type short long)
          (syntax-error spec (format "invalid ~a in" type))))
      (let* ([short (get-short short)]
             [long (get-long long)]
             [clauses (collect-clauses x optionals '(conflicts requires usage))]
             [conflicts (or (find-clause 'conflicts clauses)
                            #'(conflicts '()))]
             [requires (or (find-clause 'requires clauses)
                           #'(requires '()))]
             [usage-clause (find-clause 'usage clauses)]
             [usage (syntax->datum (or usage-clause '(usage)))]
             [full-usage (populate-defaults short long (cdr usage))])
        (unless (and (valid-usage? full-usage)
                     (or (not usage-clause)
                         (not (null? (scdr usage-clause)))))
          (syntax-error spec (format "invalid ~a in" usage)))
        #`(<arg-spec> make
            [name '#,name]
            [type '#,type]
            [short #,short]
            [long #,long]
            [help #,help]
            #,conflicts
            #,requires
            [usage '#,(datum->syntax #'_ full-usage)])))

    (define (translate spec)
      (syntax-case spec ()
        [default-help
         (eq? (datum default-help) 'default-help)
         (translate #'[help -h --help bool "display this help and exit" (usage fit)])]
        [(name short long type help . optionals)
         (and (short? #'short) (long? #'long))
         (spec-maker spec #'name #'short #'long #'type #'help #'optionals)]
        [(name short type help . optionals)
         (short? #'short)
         (spec-maker spec #'name #'short #f #'type #'help #'optionals)]
        [(name long type help . optionals)
         (long? #'long)
         (spec-maker spec #'name #f #'long #'type #'help #'optionals)]
        [(name type help . optionals)
         (spec-maker spec #'name #f #f #'type #'help #'optionals)]))

    (syntax-case x ()
      [(_ spec ...)
       #`(list #,@(map translate #'(spec ...)))]))

  (define (bad-spec who what spec)
    (raise `#(bad-spec ,who ,what ,spec)))

  (define (check-specs specs) (check-specs-help specs #t))
  (define (partial-check-specs specs) (check-specs-help specs #f))

  (define (check-specs-help specs check-missing?)
    (let ([ht (make-hashtable symbol-hash eq?)])
      (define (specs-missing ls)
        (fold-right
         (lambda (x acc)
           (if (hashtable-ref ht x #f)
               acc
               (cons x acc)))
         '()
         ls))
      (for-each
       (lambda (s)
         (<arg-spec> open s [name type short long help usage])
         (unless (symbol? name) (bad-spec 'name name s))
         (unless (or (not short) (and (char? short) (valid-short-char? short)))
           (bad-spec 'short short s))
         (unless (or (not long) (string? long)) (bad-spec 'long long s))
         (unless (valid-type? type short long)
           (bad-spec 'type type s))
         (unless (or (string? help) (list? help)) (bad-spec 'help help s))
         (unless (valid-usage? usage) (bad-spec 'usage usage s))
         (hashtable-update! ht name
           (lambda (old)
             (when old (bad-spec 'duplicate-spec name s))
             s)
           #f)
         )
       specs)
      (for-each
       (lambda (s)
         (<arg-spec> open s [conflicts requires])
         (unless (and (list? conflicts) (for-all symbol? conflicts))
           (bad-spec 'conflicts conflicts s))
         (when check-missing?
           (let ([missing (specs-missing conflicts)])
             (unless (null? missing)
               (bad-spec 'missing-specs missing s))))
         (unless (and (list? requires) (for-all symbol? requires))
           (bad-spec 'requires requires s))
         (when check-missing?
           (let ([missing (specs-missing requires)])
             (unless (null? missing)
               (bad-spec 'missing-specs missing s)))))
       specs)
      ht))

  (define (parse-arguments specs ls fail)
    (let* ([name->spec (check-specs specs)] ; name -> <arg-spec>
           [option->spec                    ; short/long -> <arg-spec>
            (let ([ht (make-hashtable equal-hash equal?)])
              (for-each
               (lambda (s)
                 (<arg-spec> open s [short long])
                 (when short
                   (hashtable-update! ht short
                     (lambda (old)
                       (when old
                         (bad-spec 'duplicate-option-spec short s))
                       s)
                     #f))
                 (when long
                   (hashtable-update! ht long
                     (lambda (old)
                       (when old
                         (bad-spec 'duplicate-option-spec long s))
                       s)
                     #f)))
               specs)
              ht)]
           [pos-specs (filter
                       (lambda (s)
                         (<arg-spec> open s [short long])
                         (and (not short) (not long)))
                       specs)])
      (define (lookup-option x)
        (hashtable-ref option->spec x #f))

      (define (shortish? x)
        (and
         (char=? (string-ref x 0) #\-)
         (not (string=? x "-"))
         (not (char-numeric? (string-ref x 1)))))

      (define (maybe-option? arg)
        (or (starts-with? arg "--")
            (and (> (string-length arg) 0) (shortish? arg))))

      (define ht (make-hashtable symbol-hash eq?))

      (define (update-list ht name value)
        (hashtable-update! ht name
          (lambda (old) (append old value))
          '()))

      (define (take-pos arg arg* pos-specs)
        (match pos-specs
          [()
           (fail "too many arguments: ~s" ls)
           (take-opt arg* pos-specs)]
          [(,[spec <= `(<arg-spec> ,name ,type)] . ,pos-specs)
           (match type
             [(string ,_)
              (hashtable-set! ht name arg)
              (take-opt arg* pos-specs)]
             [(list . ,patterns)
              (let lp ([patterns patterns] [ls (cons arg arg*)] [acc '()])
                (match patterns
                  [()
                   (update-list ht name (reverse acc))
                   (take-opt ls pos-specs)]
                  [,p                   ; rest
                   (guard (string? p))
                   (update-list ht name (append (reverse acc) ls))
                   (take-opt '() pos-specs)]
                  [(,p ...)             ; many
                   (if (and (pair? ls) (not (maybe-option? (car ls))))
                       (lp patterns (cdr ls) (cons (car ls) acc))
                       (lp '() ls acc))]
                  [(,p . ,patterns)     ; one
                   (guard (and (pair? ls) (not (maybe-option? (car ls)))))
                   (lp patterns (cdr ls) (cons (car ls) acc))]
                  [,_
                   (fail "option expects value: ~a" (format-spec spec 'args))
                   (take-opt ls pos-specs)]))])]))

      (define (take-named input arg* spec pos-specs)
        (<arg-spec> open spec [name type])
        (match type
          [bool
           (hashtable-update! ht name
             (lambda (old)
               (when old (fail "duplicate option ~a" input))
               #t)
             #f)
           (take-opt arg* pos-specs)]
          [count
           (hashtable-update! ht name (lambda (old) (+ old 1)) 0)
           (take-opt arg* pos-specs)]
          [(string ,_)
           (match arg*
             [(,arg . ,rest)
              (guard (not (maybe-option? arg)))
              (hashtable-update! ht name
                (lambda (old)
                  (when old (fail "duplicate option ~a" input))
                  (or old arg))
                #f)
              (take-opt rest pos-specs)]
             [,_
              (fail "option expects value: ~a ~a" input
                (format-spec spec 'args))
              (take-opt arg* pos-specs)])]
          [(list . ,patterns)
           (let lp ([patterns patterns] [ls arg*] [acc '()])
             (match patterns
               [()
                (update-list ht name (reverse acc))
                (take-opt ls pos-specs)]
               [,p                      ; rest
                (guard (string? p))
                (update-list ht name (append (reverse acc) ls))
                (take-opt '() pos-specs)]
               [(,p ...)                ; many
                (if (and (pair? ls) (not (maybe-option? (car ls))))
                    (lp patterns (cdr ls) (cons (car ls) acc))
                    (lp '() ls acc))]
               [(,p . ,patterns)        ; one
                (guard (and (pair? ls) (not (maybe-option? (car ls)))))
                (lp patterns (cdr ls) (cons (car ls) acc))]
               [,_
                (fail "option expects value: ~a ~a" input
                  (format-spec spec 'args))
                (take-opt ls pos-specs)]))]))

      (define (take-opt ls pos-specs)
        (match ls
          [() ht]
          [(,arg . ,rest)
           (cond
            [(string=? arg "")
             (take-pos arg rest pos-specs)]
            [(starts-with? arg "--")
             (let ([larg (substring arg 2 (string-length arg))])
               (cond
                [(lookup-option larg) =>
                 (lambda (s)
                   (take-named arg rest s pos-specs))]
                [else
                 (fail "unexpected ~a" arg)
                 (take-opt rest pos-specs)]))]
            [(shortish? arg)
             (cond
              [(> (string-length arg) 2)
               (take-opt
                (append (map (lambda (c) (format "-~a" c))
                          (string->list (substring arg 1 (string-length arg))))
                  rest)
                pos-specs)]
              [(lookup-option (string-ref arg 1)) =>
               (lambda (s)
                 (take-named arg rest s pos-specs))]
              [else
               (fail "unexpected ~a" arg)
               (take-opt rest pos-specs)])]
            [else
             (take-pos arg rest pos-specs)])]))

      (define (check-conflicts ht)
        (vector-for-each
         (lambda (name)
           (let* ([s (hashtable-ref name->spec name #f)]
                  [ls (filter
                       (lambda (x) (hashtable-ref ht x #f))
                       (<arg-spec> conflicts s))])
             (unless (null? ls)
               (fail "~a conflicts with ~{~a~^, ~}"
                 (describe-spec (hashtable-ref name->spec name #f))
                 (map
                  (lambda (x)
                    (describe-spec (hashtable-ref name->spec x #f)))
                  ls)))))
         (hashtable-keys ht))
        ht)

      (define (check-requires ht)
        (vector-for-each
         (lambda (name)
           (let* ([s (hashtable-ref name->spec name #f)]
                  [ls (filter
                       (lambda (x) (not (hashtable-ref ht x #f)))
                       (<arg-spec> requires s))])
             (unless (null? ls)
               (fail "~a requires ~{~a~^, ~}"
                 (describe-spec (hashtable-ref name->spec name #f))
                 (map
                  (lambda (x)
                    (describe-spec (hashtable-ref name->spec x #f)))
                  ls)))))
         (hashtable-keys ht))
        ht)

      (let ([ht (check-requires (check-conflicts (take-opt ls pos-specs)))])
        (case-lambda
         [() ht]
         [(name)
          (unless (hashtable-ref name->spec name #f)
            (raise `#(no-spec-with-name ,name)))
          (hashtable-ref ht name #f)]))))

  (define parse-command-line-arguments
    (case-lambda
     [(specs) (parse-command-line-arguments specs (command-line-arguments))]
     [(specs ls)
      (parse-command-line-arguments specs ls
        (lambda (fmt . args)
          (apply errorf #f fmt args)))]
     [(specs ls fail)
      (arg-check 'parse-command-line-arguments
        [ls list? (lambda (x) (for-all string? x))]
        [fail procedure?])
      (parse-arguments specs ls fail)]))

  (define (maybe-list . args) (remq #f args))

  (define (patterns->str patterns)
    (let lp ([patterns patterns] [acc '()])
      (match patterns
        [()
         (format "~{~a~^ ~}" (reverse acc))]
        [,p                             ; rest
         (guard (string? p))
         (lp '() (cons "..." (cons p acc)))]
        [(,p ...)                       ; many
         (guard (string? p))
         (lp '() (cons "..." (cons p acc)))]
        [(,p . ,patterns)               ; one
         (lp patterns (cons p acc))])))

  (define (usage->how usage)
    (find (lambda (x) (valid-usage-how? x)) usage))

  (define format-spec
    (case-lambda
     [(spec) (format-spec spec #f)]
     [(spec how)
      (<arg-spec> open spec [type short long usage])
      (partial-check-specs (list spec))
      (let fmt ([how (or how (usage->how usage))])
        (match how
          [short (and short (format "-~a" short))]
          [long (and long (format "--~a" long))]
          [args
           (match type
             [bool #f]
             [count #f]
             [(string ,help) help]
             [(list . ,patterns) (patterns->str patterns)])]
          [(or . ,hows) (ormap fmt hows)]
          [(and . ,hows) (join (remq #f (map fmt hows)) #\space)]
          [(opt ,how)
           (let ([s (fmt how)])
             (and s (string-append "[" s "]")))]
          [(req ,how) (fmt how)]
          [,_ (bad-arg 'format-spec how)]))]))

  (define (describe-spec s)
    (format-spec s '(or long short args)))

  (define (help-left s)
    (let ([short (format-spec s 'short)] [long (format-spec s 'long)])
      (format "~{~a~^ ~}"
        (maybe-list
         (and (or short long)
              (format "~{~a~^, ~}" (maybe-list short long)))
         (format-spec s 'args)))))

  (define help-wrap-width
    (make-parameter 79
      (lambda (x)
        (unless (and (fixnum? x) (fx> x 0))
          (bad-arg 'help-wrap-width x))
        x)))

  (define (display-help-row s args op)
    (define indent 20)
    (define right-col-width (max 0 (- (help-wrap-width) indent)))
    (let* ([left (help-left s)]
           [right (<arg-spec> help s)]
           [right (if (list? right)
                      (join right #\space)
                      right)]
           [arg (and args (hashtable-ref args (<arg-spec> name s) #f))]
           [right (cond
                   [(string? arg)
                    (string-append right " (" arg ")")]
                   [(pair? arg)
                    (format "~a ~a" right arg)]
                   [else right])])
      (display-string "  " op)
      (display left op)
      (let* ([llen (+ (string-length left) 2)]
             [init-indent
              (cond
               [(< llen indent) (- indent llen)]
               [else
                (newline op)
                indent])])
        (wrap-text op right-col-width init-indent indent right)
        (newline op))))

  (define (display-usage-internal prefix exe-name width in-opt pos op)
    (define (prepare s)
      (<arg-spec> open s [type short long usage])
      (define flag-char
        (and short
             (pregexp-match (re "\\[-.\\]") (format-spec s))
             short))
      (cons (and (memq 'show usage) #t)
        (or flag-char (format-spec s))))
    (define (flag? x) (char? (cdr x)))
    (define (bracketed? x) (starts-with? (cdr x) "["))
    (define (fit w ls first-oh rest-oh)
      (let lp ([w w] [ls ls] [in '()] [out '()] [overhead first-oh])
        (match ls
          [() (values w (reverse in) (reverse out))]
          [(,x . ,rest)
           (match-let* ([(,show? . ,fmt) x])
             (let ([len (+ overhead (if (string? fmt) (string-length fmt) 1))])
               (cond
                [show? (lp (- w len) rest (cons fmt in) out rest-oh)]
                [(>= w len) (lp (- w len) rest (cons fmt in) out rest-oh)]
                [else (lp w rest in (cons x out) overhead)])))])))
    (define (visible? s) (not (memq 'hide (<arg-spec> usage s))))
    (define candidates (map prepare (filter visible? in-opt)))
    (define flag-oh (string-length " [-]"))
    (define (fmt-named w flag-oh)
      (let*-values ([(flag-opts arg-opts) (partition flag? candidates)]
                    [(opt-args req-args) (partition bracketed? arg-opts)]
                    [(w reqs req-other) (fit w req-args 1 1)]
                    [(w flags flag-other) (fit w flag-opts flag-oh 0)]
                    [(w opts opt-other) (fit w opt-args 1 1)]
                    [(flags) (and (pair? flags) flags)])
        (values flags
          (format "~@[ [-~{~c~}]~]~{ ~a~}~:[~; [options]~]~{ ~a~}"
            flags opts
            (or (pair? flag-other) (pair? req-other) (pair? opt-other))
            reqs))))
    (define leader (format "~a ~a" prefix exe-name))
    (define pos-args (format "~{ ~a~}" (map format-spec pos)))
    (define max-width (or width (help-wrap-width)))
    (define opt-width (- max-width (string-length leader) (string-length pos-args)))
    (define named-args
      (let-values ([(_ x) (fmt-named opt-width flag-oh)])
        (if (<= (string-length x) opt-width)
            x
            (let-values ([(min-flags minimal) (fmt-named 0 flag-oh)])
              (if (>= (string-length minimal) opt-width)
                  minimal
                  (let-values ([(_ x)
                                (fmt-named
                                 (- opt-width (string-length minimal))
                                 (if min-flags 0 flag-oh))])
                    x))))))
    (fprintf op "~a~a~a\n" leader named-args pos-args))

  (define (valid-width? n) (or (not n) (and (fixnum? n) (fx>= n 0))))

  (define display-usage
    (case-lambda
     [(prefix exe-name specs)
      (display-usage prefix exe-name specs #f)]
     [(prefix exe-name specs width)
      (display-usage prefix exe-name specs width (current-output-port))]
     [(prefix exe-name specs width op)
      (arg-check 'display-usage
        [prefix string?]
        [exe-name string?]
        [width valid-width?]
        [op output-port? textual-port?])
      (partial-check-specs specs)
      (let-values ([(pos opt) (partition positional? specs)])
        (display-usage-internal prefix exe-name width opt pos op))]))

  (define (display-options-internal opt pos args op)
    (for-each (lambda (o) (display-help-row o args op)) opt)
    (for-each (lambda (p) (display-help-row p args op)) pos))

  (define (parsed-options? args) (or (not args) (hashtable? args)))

  (define display-options
    (case-lambda
     [(specs) (display-options specs #f)]
     [(specs args) (display-options specs args (current-output-port))]
     [(specs args op)
      (arg-check 'display-options
        [args parsed-options?]
        [op output-port? textual-port?])
      (partial-check-specs specs)
      (let-values ([(pos opt) (partition positional? specs)])
        (display-options-internal opt pos args op))]))

  (define display-help
    (case-lambda
     [(exe-name specs)
      (display-help exe-name specs #f)]
     [(exe-name specs args)
      (display-help exe-name specs args (current-output-port))]
     [(exe-name specs args op)
      (arg-check 'display-help
        [exe-name string?]
        [args parsed-options?]
        [op output-port? textual-port?])
      (check-specs specs)
      (let-values ([(pos opt) (partition positional? specs)])
        (display-usage-internal "Usage:" exe-name #f opt pos op)
        (when (or (pair? opt) (pair? pos))
          (newline op)
          (display-options-internal opt pos args op)))]))

  )
