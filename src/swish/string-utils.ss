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

(library (swish string-utils)
  (export
   ct:join
   ct:string-append
   ends-with-ci?
   ends-with?
   format-rfc2822
   join
   natural-string
   natural-string-ci<?
   natural-string<?
   oxford-comma
   split
   split-n
   starts-with-ci?
   starts-with?
   symbol-append
   trim-whitespace
   wrap-text
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish meta)
   (swish pregexp))

  (define-syntax (ct:string-append x)
    (syntax-case x ()
      [(_ s ...)
       (replace-source x
         (match (combine-adjacent string? string-append #'(s ...))
           [() (datum->syntax #'_ "")]
           [(,s) (guard (string? s)) (datum->syntax #'_ s)]
           [,exprs #`(string-append #,@exprs)]))]))

  (define-syntax (ct:join x)
    (define (do-join sep ls)
      (if (or (null? ls) (null? (cdr ls)))
          ls
          (list* (car ls) sep (do-join sep (cdr ls)))))
    (syntax-case x ()
      [(_ sep s ...)
       (char? (datum sep))
       #`(ct:string-append #,@(do-join (string (datum sep)) #'(s ...)))]
      [(_ sep s ...)
       (string? (datum sep))
       #`(ct:string-append #,@(do-join (datum sep) #'(s ...)))]))

  (define join
    (case-lambda
     [(ls separator) (join ls separator separator)]
     [(ls separator last-separator)
      (if (null? ls)
          ""
          (let-values ([(op extractor) (open-string-output-port)])
            (display (car ls) op)
            (unless (null? (cdr ls))
              (do ([ls (cdr ls) (cdr ls)])
                  ((null? (cdr ls))
                   (display last-separator op)
                   (display (car ls) op))
                (display separator op)
                (display (car ls) op)))
            (extractor)))]))

  (define symbol-append
    (lambda ls (string->symbol (apply string-append (map symbol->string ls)))))

  (define (split-n str separator n)
    (let ([limit (string-length str)])
      (let lp ([s 0] [e 0] [n n])
        (cond
         [(or (fx= e limit) (fx= n 1))
          (cons (substring str s limit) '())]
         [(char=? (string-ref str e) separator)
          (cons (substring str s e) (lp (fx+ e 1) (fx+ e 1) (fx- n 1)))]
         [else (lp s (fx+ e 1) n)]))))

  (define (split str separator)
    (split-n str separator (+ (string-length str) 1)))

  (define (starts-with? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([i 0])
             (or (= i p-len)
                 (and (char=? (string-ref s i) (string-ref p i))
                      (lp (+ i 1))))))))

  (define (starts-with-ci? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([i 0])
             (or (= i p-len)
                 (and (char-ci=? (string-ref s i) (string-ref p i))
                      (lp (+ i 1))))))))

  (define (ends-with? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([si (- s-len 1)] [pi (- p-len 1)])
             (or (< pi 0)
                 (and (char=? (string-ref s si) (string-ref p pi))
                      (lp (- si 1) (- pi 1))))))))

  (define (ends-with-ci? s p)
    (let ([s-len (string-length s)]
          [p-len (string-length p)])
      (and (>= s-len p-len)
           (let lp ([si (- s-len 1)] [pi (- p-len 1)])
             (or (< pi 0)
                 (and (char-ci=? (string-ref s si) (string-ref p pi))
                      (lp (- si 1) (- pi 1))))))))

  (define (format-rfc2822 d)
    (format "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d ~c~2,'0d~2,'0d"
      (vector-ref '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
        (date-week-day d))
      (date-day d)
      (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep"
                     "Oct" "Nov" "Dec")
        (- (date-month d) 1))
      (date-year d)
      (date-hour d)
      (date-minute d)
      (date-second d)
      (if (>= (date-zone-offset d) 0) #\+ #\-)
      (quotient (abs (date-zone-offset d)) 3600)
      (remainder (quotient (abs (date-zone-offset d)) 60) 60)))

  (define (trim-whitespace s)
    (define (find-start s i len)
      (cond
       [(eqv? i len) ""]
       [(char-whitespace? (string-ref s i)) (find-start s (fx+ i 1) len)]
       [else (find-end s (fx- len 1) i len)]))
    (define (find-end s i start len)
      (cond
       [(eqv? i start)
        (if (eqv? len 1)
            s
            (string (string-ref s i)))]
       [(char-whitespace? (string-ref s i)) (find-end s (fx- i 1) start len)]
       [else
        (if (and (eqv? start 0) (eqv? (fx- len 1) i))
            s
            (substring s start (fx+ i 1)))]))
    (find-start s 0 (string-length s)))

  (define (extract text acc)
    (let lp ([start 0] [acc acc])
      (match (pregexp-match-positions (re "[^ \\n]+| |\\n") text start)
        [#f acc]
        [((,start . ,end))
         (let ([s (substring text start end)])
           (lp end
             (cons
              (match s
                [" " 'ws]
                ["\n" 'line]
                [,_ s])
              acc)))])))

  (define (minimize-whitespace ls)
    (let lp ([ls ls] [bol? #t] [ws? #f])
      (match ls
        [() '()]
        [(ws . ,rest) (lp rest bol? #t)]
        [(line . ,rest) (cons 'line (lp rest #t #f))]
        [(,x . ,rest)
         (let ([rest (cons x (lp rest #f ws?))])
           (if (or bol? (not ws?))
               rest
               (cons 'ws rest)))])))

  (define (wrap-text op width initial-indent subsequent-indent text)
    (arg-check 'wrap-text
      [op output-port? textual-port?]
      [width fixnum? fxnonnegative?]
      [initial-indent fixnum? fxnonnegative?]
      [subsequent-indent fixnum? fxnonnegative?]
      [text (lambda (x) (or (string? x) (list? x)))])
    (let lp ([ls (minimize-whitespace
                  (reverse
                   (if (string? text)
                       (extract text '())
                       (fold-left
                        (lambda (ls s)
                          (cons 'ws (extract s ls)))
                        '() text))))]
             [pos 0] [indent initial-indent] [indent? #t])
      (match ls
        [() (void)]
        [(ws . ,(rest <= (,text . ,_)))
         (guard (string? text))
         (cond
          [(< (+ (string-length text) pos) width)
           (fprintf op " ")
           (lp rest (+ pos 1) indent indent?)]
          [else
           (newline op)
           (lp rest 0 subsequent-indent #t)])]
        [(line . ,rest)
         (newline op)
         (lp rest 0 subsequent-indent #t)]
        [(,text . ,rest)
         (when indent?
           (display-string (make-string indent #\space) op))
         (display text op)
         (lp rest (+ pos (string-length text)) indent #f)])))

  (define oxford-comma
    (case-lambda
     [(each conj) (oxford-comma "~{" each conj "~}")]
     [(start each conj end)
      (string-append start each "~#[~;" conj "~;, " each "," conj "~:;, ~]" end)]))

  (module natural-string ()
    (export
     elide-whitespace
     get-integer-token
     make-natural<?
     token-ci<?
     token<?
     trust)
    (include "unsafe.ss")
    (declare-unsafe-primitives
     * + < =
     char->integer char-alphabetic? char-ci<? char-numeric? char-whitespace? char<? char=?
     fx+ fx- fx< fx= fx<= fx>=
     string-ref)

    ;; Return integer token if we find a decimal integer starting at i within s.
    ;; Handle leading +/- sign only at the start of a string or following
    ;; whitespace to avoid breaking a-1 < a-2.
    ;;
    ;; Does not handle floats or ratnums because:
    ;; - locale matters for decimal separator in floats
    ;; - floats would complicate comparing version strings
    ;; - dates would complicate comparing ratnums
    (define (get-integer-token c s i end)
      (define (->digit c)
        (fx- (char->integer c) (char->integer #\0)))
      (define (char-digit? c)
        (fx<= (char->integer #\0) (char->integer c) (char->integer #\9)))
      (define (get-n&i n i)
        (if (fx= i end)
            (values n i)
            (let ([d (string-ref s i)])
              (if (char-digit? d)
                  (get-n&i (+ (->digit d) (* n 10)) (fx+ i 1))
                  (values n i)))))
      (define (get-int n start i)
        (let-values ([(n j) (get-n&i n start)])
          (values n j (fx- j i))))
      (define (dash? s i)
        (and (fx> i 0)
             (let ([c (string-ref s (fx- i 1))])
               (or (char-alphabetic? c) (char-numeric? c)))))
      (define (get-signed i k maybe-dash?)
        (if (and maybe-dash? (dash? s i))
            (values #f i 0)
            (let-values ([(n j len) (get-int 0 (fx+ i 1) i)])
              (if (fx> len 1) ;; more than sign
                  (values (k n) j len)
                  (values #f i 0)))))
      (cond
       [(char-digit? c) (get-int (->digit c) (fx+ i 1) i)]
       [(char=? c #\-) (get-signed i - #t)]
       [(char=? c #\+) (get-signed i + #f)]
       [else (values #f i 0)]))

    (define (elide-whitespace c s i end)
      (let skip ([c c] [j i] [found #f])
        (cond
         [(char-whitespace? c)
          (let ([next (fx+ j 1)])
            (if (fx= next end)
                (values 'eos j 0)
                (skip (string-ref s next) next #\space)))]
         [(fx= i 0) (values #f j 0)] ;; ignore leading whitespace
         [found (values found (fx- j 1) 1)]
         [else (values #f j 0)])))

    (define (int? x) (or (fixnum? x) (bignum? x)))

    (define (int-token<? a b)
      (and (int? a)
           (or (not (int? b))
               (< a b)
               (and (= a b) 'shortest-wins))))

    (define (token<? a b)
      (if (and (char? a) (char? b))
          (char<? a b)
          (int-token<? a b)))

    (define (token-ci<? a b)
      (if (and (char? a) (char? b))
          (char-ci<? a b)
          (int-token<? a b)))

    (define (do-compare s1 s2 token<? get-token)
      (define (call-get-token s i end)
        (if (fx= i end)
            (values 'eos i 0)
            (get-token (string-ref s i) s i end)))
      (and (not (eq? s1 s2))
           (let ([end1 (string-length s1)] [end2 (string-length s2)])
             (let lp ([i 0] [j 0])
               (let*-values ([(a-tok i a-len) (call-get-token s1 i end1)]
                             [(b-tok j b-len) (call-get-token s2 j end2)])
                 (let ([result (token<? a-tok b-tok)])
                   (cond
                    [(eq? result #t)]
                    [(eq? result 'shortest-wins) ;; neither is at eos
                     (or (fx< a-len b-len)
                         (and (fx= a-len b-len) (lp i j)))]
                    [(token<? b-tok a-tok) #f]
                    [(or (eq? a-tok 'eos) (fx= i end1))
                     (not (or (eq? b-tok 'eos) (fx= j end2)))]
                    [(or (eq? b-tok 'eos) (fx= j end2)) #f]
                    [else (lp (fx+ i 1) (fx+ j 1))])))))))

    (define-record-type trusted
      (nongenerative)
      (sealed #t)
      (fields (immutable proc)))

    (define (trust p) (make-trusted p))

    ;; trust internal tokenizers
    (define (safeguard get-token)
      (cond
       [(eq? get-token elide-whitespace) get-token]
       [(eq? get-token get-integer-token) get-token]
       [(trusted? get-token) (trusted-proc get-token)]
       [else
        (lambda (c s i0 end)
          (let-values ([(tok i len) (get-token c s i0 end)])
            (unless (and (fixnum? i) (fx<= i0 i end))
              (throw `#(bad-return-value ,i)))
            (unless (and (fixnum? len) (fx>= len 0))
              (throw `#(bad-return-value ,len)))
            (values tok i len)))]))

    (define (compose get-token get-token*)
      (let ([get-token (safeguard get-token)])
        ;; use the i from get-token even if tok is #f so that get-token can elide
        (if (null? get-token*)
            (lambda (c s i0 end)
              (let-values ([(tok i len) (get-token c s i0 end)])
                (cond
                 [tok (values tok i len)]
                 [else (values c i 1)])))
            (let ([do-rest (compose (car get-token*) (cdr get-token*))])
              (lambda (c s i0 end)
                (let-values ([(tok i len) (get-token c s i0 end)])
                  (cond
                   [tok (values tok i len)]
                   [(fx= i i0) (do-rest c s i end)]
                   [(fx= i end) (values 'eos i 0)]
                   [else (do-rest (string-ref s i) s i end)])))))))

    (define (make-comparison token<? get-token get-token*)
      (let ([composed-get-token (compose get-token get-token*)])
        (lambda (s1 s2)
          (do-compare s1 s2 token<? composed-get-token))))

    (define make-natural<?
      (case-lambda
       [(token<? get-token)
        (make-comparison token<? get-token '())]
       [(token<? get-token . get-token*)
        (make-comparison token<? get-token get-token*)]))

    )

  (import natural-string)

  (define natural-string<? (make-natural<? token<? elide-whitespace get-integer-token))
  (define natural-string-ci<? (make-natural<? token-ci<? elide-whitespace get-integer-token))

  )
