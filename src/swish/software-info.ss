#!chezscheme
(library (swish software-info)
  (export
   include-line
   software-company
   software-company-dir
   software-copyright-year
   software-internal-name
   software-product-name
   software-revision
   software-version
   swish-version
   )
  (import (chezscheme))

  (define software-company "Beckman Coulter, Inc.")

  (define software-company-dir "Beckman Coulter")

  (define software-copyright-year "2018")

  ;; The software-internal-name value is used as the name for the
  ;; compiled exe and boot files. It must not contain spaces or any
  ;; other character that requires quoting in GNU make.
  (define software-internal-name "swish")

  (define software-product-name "Swish")

  (define software-version "0.0.0")

  (define (swish-version)
    (format "~a Version ~a" software-product-name software-version))

  (define-syntax (file-not-found x)
    (syntax-case x ()
      [(_ fn) (syntax-error #'fn "file not found")]))

  (define-syntax (include-line x)
    (syntax-case x ()
      [(il filename) #'(il filename file-not-found)]
      [(il filename not-found)
       (let ([fn (datum filename)])
         (unless (string? fn) (syntax-error x "expected path as string"))
         (guard (c [else #'(not-found filename)])
           (with-source-path 'include-line fn
             (lambda (path)
               (if (file-exists? path)
                   (datum->syntax #'il (call-with-input-file path get-line))
                   #'(not-found filename))))))]))

  (define software-revision
    (let ([revs (make-hashtable symbol-hash eq?)])
      (case-lambda
       [()
        (let-values ([(keys vals) (hashtable-entries revs)])
          (vector->list (vector-map cons keys vals)))]
       [(key)
        (hashtable-ref revs key #f)]
       [(key hash)
        (cond
         [(hashtable-ref revs key #f) =>
          (lambda (hit)
            (unless (equal? hit hash)
              (errorf 'software-revision "~s already specified hash ~s" key hit)))]
         [else (hashtable-set! revs key hash)])])))

  (software-revision 'swish
    (include-line "swish/git.revision"
      (lambda (fn)
        (warningf 'software-info.ss "file ~s not found at compile time" fn)
        #f)))
  )

#!eof mats

(import (swish mat) (swish software-info))

(mat software-revision ()
  (define swish-hash (include-line "swish/git.revision"))
  (match-let*
   ([#f (software-revision 'xyz)]
    [,_ (software-revision 'xyz "pdq")]
    ["pdq" (software-revision 'xyz)]
    [,@swish-hash (software-revision 'swish)]
    [#f (software-revision 'abc)]
    [,_ (software-revision 'foo "bar")]
    ["bar" (software-revision 'foo)]
    [,ls (software-revision)]
    [(xyz . "pdq") (assq 'xyz ls)]
    [(foo . "bar") (assq 'foo ls)]
    [(swish . ,@swish-hash) (assq 'swish ls)]
    [#(EXIT ,reason) (catch (software-revision 'foo "different"))]
    [,_ (software-revision 'foo "bar")])
   'ok))
