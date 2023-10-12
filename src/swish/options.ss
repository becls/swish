;;; Copyright 2021 Beckman Coulter, Inc.
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

(library (swish options)
  (export
   define-options
   procedure/arity?
   )
  (import
   (scheme)
   (swish erlang)
   (swish meta))

  (meta define (get-rhs key clauses form)
    (syntax-case (get-clause key clauses form) ()
      [(_ val) #'val]))

  (meta define (get-rhs/default key clauses default-var)
    (syntax-case (find-clause key clauses) ()
      [(_ val) #'val]
      [_ default-var]))

  (meta define (find-rhs key clauses multiple?)
    (cond
     [(find-clause key clauses) =>
      (lambda (present)
        (syntax-case present ()
          [(key rhs0 rhs1 ...) multiple? #'(rhs0 rhs1 ...)]
          [(key rhs1) #'rhs1]))]
     [else #f]))

  (meta define (choose-refs copy-vars all-keys)
    (let ([als (map cons (syntax->datum copy-vars) copy-vars)])
      (map (lambda (key)
             (syntax-case (assq (syntax->datum key) als) ()
               [(copy-name . copy-var) #'copy-var]
               [else key]))
        (syntax->list all-keys))))

  ;; (define-options opt-name
  ;;   (required [key req-spec ...] ...)
  ;;   (optional [key opt-spec ...] ...))
  ;;
  ;; opt-name is an identifier
  ;; required, optional, must-be, filter, and default are keywords
  ;; key ... are distinct identifiers
  ;;
  ;; req-spec -> (must-be pred? ...)
  ;;           | (filter filter-expr)
  ;;
  ;; opt-spec -> req-spec
  ;;           | (default default-expr)
  ;;
  ;; The define-options form generates an internal make-record procedure that
  ;; evaluates the default-expr, pred?, and filter-expr expressions left-to-right
  ;; in the order that fields appear in the definition, though the order of
  ;; evaluation for individual expressions of a particular field is unspecified.
  ;; This means that default expressions are generative. We can bind
  ;; default-expr to a variable where this is not desirable.
  ;;
  ;; The define-options form defines:
  ;;  - a macro opt-name that supports
  ;;    - record construction and copying
  ;;      (opt-name [key val] ...)
  ;;      (opt-name copy obj [key val] ...)
  ;;      (opt-name copy* obj [key val] ...)
  ;;    - record field access
  ;;      ((opt-name key) obj)
  ;;      (opt-name key obj)
  ;;    - record predicate
  ;;      ((opt-name is?) obj)
  ;;      (opt-name is? obj)
  ;;  - a native record type called <opt-name> for use in:
  ;;    (match obj [`(<opt-name> ,key ...) ...])

  (define-syntax (define-options input)
    (define (parse-optional opt-field) (parse-spec opt-field '(default)))
    (define (parse-required opt-field) (parse-spec opt-field '()))
    (define (parse-spec field extra-specs)
      (define valid-specs (append extra-specs '(must-be filter)))
      (syntax-case field ()
        [(key kv ...)
         (let ([kv* (collect-clauses field #'(kv ...) valid-specs)])
           (with-syntax ([(fml) (generate-temporaries '(formal))]
                         [dflt-expr (find-rhs 'default kv* #f)]
                         [(pred? ...) (or (find-rhs 'must-be kv* #t) '())]
                         [filter (or (find-rhs 'filter kv* #f) #'values)])
             #`(key fml dflt-expr
                 (lambda (val)
                   (unless (and (pred? val) ...)
                     (bad-arg 'key val))
                   (filter val)))))]))
    (let f ([x input])
      (syntax-case x ()
        [(_ name (optional opt-field ...))
         (eq? (datum optional) 'optional)
         (f #'(define-options name (required) (optional opt-field ...)))]
        [(_ name (required req-field ...))
         (eq? (datum required) 'required)
         (f #'(define-options name (required req-field ...) (optional)))]
        [(_ name (required req-field ...) (optional opt-field ...))
         (and (identifier? #'name)
              (equal? (datum (required optional)) '(required optional)))
         (with-syntax
          ([<name> (compound-id #'name "<" #'name ">")]
           [<name>? (compound-id #'name "<" #'name ">?")]
           [make-<name> (compound-id #'name "make-<" #'name ">")]
           [((req-key req-fml #f req-check-expr) ...)
            (map parse-required #'(req-field ...))]
           [((opt-key opt-fml opt-dflt-expr opt-check-expr) ...)
            (map parse-optional #'(opt-field ...))]
           [:... #'(... ...)])
          (assert (valid-fields? input #'(req-key ... opt-key ...) #f '(copy copy* is?)))
          #'(module (<name> (name make-record))
              (define-record-type <name>
                (nongenerative)
                (parent <options>)
                (fields
                 (immutable req-key) ...
                 (immutable opt-key) ...))
              (define make-record
                (let ([omitted (record-type-descriptor <options>)])
                  (lambda (req-fml ... opt-fml ...)
                    (let* ([req-key (req-check-expr req-fml)] ...
                           [opt-key (opt-check-expr (if (eq? opt-fml omitted) opt-dflt-expr opt-fml))] ...)
                      (make-<name> req-key ... opt-key ...)))))
              (define-syntax (name x)
                (define fields '(req-key ... opt-key ...))
                force-library-init ;; insert reference to force library init
                (syntax-case x ()
                  [(_ copy obj [key val] :...)
                   (memq (datum copy) '(copy copy*))
                   (begin
                     (valid-fields? x #'(key :...) fields '())
                     (with-syntax ([(ref :...)
                                    (choose-refs #'(key :...) #'(req-key ... opt-key ...))])
                       (case (datum copy)
                         [copy
                          #'(match obj
                              [`(<name> ,req-key ... ,opt-key ...)
                               (let ([key val] :...)
                                 (make-record ref :...))])]
                         [copy*
                          #'(match obj
                              [`(<name> ,req-key ... ,opt-key ...)
                               ((lambda (ref :...)
                                  (let ([key val] :...)
                                    (make-record ref :...)))
                                req-key ... opt-key ...)])])))]
                  [(_ copy . _)
                   (memq (datum copy) '(copy copy*))
                   (syntax-case x ())]
                  [(_ field arg :...)
                   (or (memq (datum field) fields) (eq? (datum field) 'is?))
                   (let ([proc
                          (if (eq? (datum field) 'is?)
                              #'(lambda (x) (match x [`(<name>) #t] [,_ #f]))
                              #'(lambda (x) (match x [`(<name> ,field) field])))])
                     (syntax-case #'(arg :...) ()
                       [() proc]
                       [(obj) #`(#,proc obj)]
                       [else (syntax-case x ())]))]
                  [(_ field . _)
                   (identifier? #'field)
                   (syntax-violation #f "unknown field" x #'field)]
                  [(_ clause :...)
                   (let ([clauses (collect-clauses x #'(clause :...) fields)])
                     (with-syntax
                      ([(req-val :...) (list (get-rhs 'req-key clauses x) ...)]
                       [(opt-val :...) (list (get-rhs/default 'opt-key clauses #'omitted) ...)])
                      #`(let ([omitted (record-type-descriptor <options>)])
                          (make-record req-val :... opt-val :...))))]))))]
        ;; report syntax error using source for the *original* input,
        ;; not the source from recursive calls to f above
        [else (syntax-case input ())])))

  (define procedure/arity?
    (case-lambda
     [(mask) (lambda (p) (procedure/arity? mask p))]
     [(mask p)
      (and (procedure? p)
           (not (zero? (logand (procedure-arity-mask p) mask))))]))

  (define force-library-init)

  (define-record-type <options> (nongenerative))

  ;; install record-writer for base type that handles children
  (record-writer (record-type-descriptor <options>)
    (lambda (r p wr)
      (display-string "#" p)
      (wr (record-type-name (record-rtd r)) p)))

  )
