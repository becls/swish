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
(library (swish meta)
  (export
   add-if-absent
   collect-clauses
   compound-id
   find-clause
   find-source
   get-clause
   pretty-syntax-violation
   profile-me
   profile-me-as
   profile-omit
   replace-source
   scar
   scdr
   snull?
   syntax-datum-eq?
   with-temporaries
   )
  (import (chezscheme))

  (meta-cond
   [(compile-profile)
    (define profile-me void)
    (define-syntax (profile-me-as x)
      (syntax-case x ()
        [(_ id)
         (let* ([annotation (syntax->annotation #'id)]
                [src (and annotation (annotation-source annotation))])
           (if src
               #`(profile #,src)
               #'(void)))]))]
   [else
    (define-syntax profile-me (identifier-syntax void))
    (define-syntax profile-me-as (syntax-rules () [(_ id) void]))])

  ;; strip source annotations to exclude from profile report
  (define-syntax (profile-omit x)
    (syntax-case x ()
      [(kwd expr ...)
       (datum->syntax #'kwd `(begin ,@(syntax->datum #'(expr ...))))]))

  (define (find-source x)
    (let ([annotation (syntax->annotation x)])
      (if annotation
          (let ([src (annotation-source annotation)])
            (datum->syntax #'quote
              `'#(at ,(source-object-bfp src)
                   ,(source-file-descriptor-path (source-object-sfd src)))))
          #'#f)))

  (define (snull? x) (syntax-case x () [() #t] [_ #f]))
  (define (scar x) (syntax-case x () [(x . _) #'x]))
  (define (scdr x) (syntax-case x () [(_ . y) #'y]))

  (define (compound-id template-identifier . args)
    (define (concat args)
      (let-values ([(op get) (open-string-output-port)])
        (define (to-string x)
          (cond
           [(string? x) x]
           [(symbol? x) (symbol->string x)]
           [(identifier? x) (to-string (syntax->datum x))]
           [else (errorf 'compound-id "invalid element ~s" x)]))
        (for-each (lambda (arg) (display (to-string arg) op)) args)
        (get)))
    (datum->syntax template-identifier (string->symbol (concat args))))

  (define (syntax-datum-eq? x y) (eq? (syntax->datum x) (syntax->datum y)))

  (define (collect-clauses form clauses valid-keys)
    (let lp ([clauses clauses] [seen '()])
      (if (snull? clauses)
          (reverse seen)
          (let* ([clause (scar clauses)]
                 [key (syntax->datum (scar clause))])
            (unless (memq key valid-keys)
              (syntax-violation #f "invalid clause" form clause))
            (when (assq key seen)
              (syntax-violation #f "duplicate clause" form clause))
            (lp (scdr clauses) (cons (cons key clause) seen))))))

  (define (find-clause key clauses)
    (let ([p (assq key clauses)])
      (and p (cdr p))))

  (define (get-clause key clauses form)
    (or (find-clause key clauses)
        (syntax-error form (format "missing ~a clause in" key))))

  (define (add-if-absent key value bindings)
    (define (contains? key bindings)
      (syntax-case bindings ()
        [() #f]
        [((k v) . bindings)
         (or (eq? (datum k) key)
             (contains? key #'bindings))]))
    (if (contains? (syntax->datum key) bindings)
        bindings
        #`((#,key #,value) #,@bindings)))

  (module (replace-source)
    ;; TODO implement replace-source upstream where we don't have to
    ;;      jump through hoops to get syntax-object-expression, etc.
    ;;      (we use syntax-object-expression instead of syntax->datum
    ;;       since the latter would go too far, stripping nested annotations)
    (define so-rtd (record-rtd #'_))
    (define syntax-object? (record-predicate so-rtd))
    (define make-syntax-object (record-constructor so-rtd))
    (define flds (record-type-field-names so-rtd))
    (define (make-accessor fld-name)
      (let ([i (ormap (lambda (f i) (and (eq? f fld-name) i))
                 (vector->list flds)
                 (iota (vector-length flds)))])
        (record-accessor so-rtd i)))
    (define syntax-object-expression (make-accessor 'expression))
    (define syntax-object-wrap (make-accessor 'wrap))
    (define empty-wrap '(()))

    (define (strip-annotation x)
      (cond
       [(syntax->annotation x) => annotation-expression]
       [(syntax-object? x) (syntax-object-expression x)]
       [else x]))

    (define (rebuild x new)
      (cond
       [(identifier? x) (datum->syntax x new)]
       [(syntax-object? x) (make-syntax-object new (syntax-object-wrap x))]
       [else (make-syntax-object new empty-wrap)]))

    (define (replace-source src-expr x)
      (cond
       [(syntax->annotation src-expr) =>
        (lambda (ae)
          (rebuild x
            (make-annotation
             (strip-annotation x)
             (annotation-source ae)
             (syntax->datum x))))]
       [(syntax->annotation x) =>
        ;; no source from src-expr, so remove source from x
        (lambda (ae)
          (rebuild x (annotation-expression ae)))]
       [else x])))

  (define-syntax with-temporaries
    (syntax-rules ()
      [(_ (tmp ...) e0 e1 ...)
       (with-syntax ([(tmp ...) (generate-temporaries '(tmp ...))])
         e0 e1 ...)]))

  ;; This procedure is an alternative to syntax-violation.
  ;; Unlike syntax-violation, it:
  ;;   1. uses pretty-format abbreviations for readability and
  ;;   2. does not attempt to infer a who condition, since
  ;;      that yields confusing results in error messages
  ;;      about match patterns, e.g., attributing the error
  ;;      to quasiquote if given #'(quasiquote (<type> ,field)).
  (define pretty-syntax-violation
    (case-lambda
     [(msg form) (pretty-syntax-violation msg form #f)]
     [(msg form subform) (pretty-syntax-violation msg form subform #f)]
     [(msg form subform who)
      (call/cc
       (lambda (where)
         (raise
          (condition
           (make-who-condition who)
           (make-continuation-condition where)
           (make-syntax-violation form subform)
           (let-values ([(src start?) (#%$syntax->src form)])
             ;; add an explicit $&src condition to prevent display-condition
             ;; from formatting the syntax-violation as it looks for source
             (#%$make-src-condition src start?))
           (make-message-condition
            (parameterize ([print-level 3]
                           [print-length 6]
                           [pretty-line-length (most-positive-fixnum)]
                           [pretty-one-line-limit (most-positive-fixnum)]
                           [pretty-initial-indent 0])
              (let ([os (open-output-string)])
                (define (pretty x os)
                  (pretty-print x os)
                  ;; omit newline
                  (set-port-output-index! os
                    (- (port-output-index os) 1)))
                (display msg os)
                (when subform
                  (display " " os)
                  (pretty (syntax->datum subform) os)
                  (when form (display " in" os)))
                (when form
                  (display " " os)
                  (pretty (syntax->datum form) os))
                (get-output-string os))))))))]))

  )
