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
   bad-syntax
   collect-clauses
   compound-id
   find-clause
   find-source
   get-clause
   make-id
   profile-me
   scar
   scdr
   snull?
   syntax-datum-eq?
   with-annotated-syntax
   )
  (import (chezscheme))

  (meta-cond
   [(compile-profile) (define profile-me void)]
   [else (define-syntax profile-me (identifier-syntax void))])

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

  (define (compound-id template-identifier . obj)
    (datum->syntax template-identifier
      (string->symbol
       (apply string-append
         (map (rec to-string
                (lambda (x)
                  (cond
                   [(string? x) x]
                   [(symbol? x) (symbol->string x)]
                   [(identifier? x) (to-string (syntax->datum x))]
                   [else (errorf 'compound-id "invalid element ~s" x)])))
           obj)))))

  (define (syntax-datum-eq? x y) (eq? (syntax->datum x) (syntax->datum y)))

  (define (bad-syntax msg form subform)
    (raise
     (condition
      (make-message-condition msg)
      (make-syntax-violation form subform))))

  (define (collect-clauses form clauses valid-keys)
    (let lp ([clauses clauses] [seen '()])
      (if (snull? clauses)
          (reverse seen)
          (let* ([clause (scar clauses)]
                 [key (syntax->datum (scar clause))])
            (unless (memq key valid-keys)
              (bad-syntax "invalid clause" form clause))
            (when (assq key seen)
              (bad-syntax "duplicate clause" form clause))
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

  (define (make-id k . args)
    (let-values ([(op get) (open-string-output-port)])
      (for-each (lambda (arg) (display (syntax->datum arg) op)) args)
      (datum->syntax k (string->symbol (get)))))

  (define-syntax with-annotated-syntax
    (syntax-rules ()
      [(_ ([pat src-obj output] ...) body0 body1 ...)
       (andmap identifier? #'(pat ...))
       (with-syntax ([pat
                      (let ([anno (syntax->annotation src-obj)])
                        (if anno
                            (make-annotation #'output (annotation-source anno) (datum output))
                            #'output))]
                     ...)
         body0 body1 ...)]))

  )
