#!chezscheme
(library (swish software-info)
  (export
   include-line
   software-info
   software-product-name
   software-revision
   software-version
   )
  (import
   (chezscheme)
   (swish app-core)
   (swish erlang)
   (swish json))

  (define info (json:make-object))

  (define (software-info) info)

  (define-syntax define-info
    (syntax-rules ()
      [(_ name leaf-key)
       (define name
         (case-lambda
          [() (name (string->symbol (or (app:name) "swish")))]
          [(key)
           (arg-check 'name [key symbol?])
           (json:ref info (cons key '(leaf-key)) #f)]
          [(key val)
           (arg-check 'name [key symbol?] [val string?])
           (json:update! info (cons key '(leaf-key)) values val)]))]))

  (define-info software-product-name product-name)
  (define-info software-version version)
  (define-info software-revision revision)

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

  (software-product-name 'swish "Swish")
  (software-version 'swish (include-line "swish/swish-version.include"))
  (software-revision 'swish (include-line "swish/swish-revision.include"))

  (software-product-name 'chezscheme "Chez Scheme")
  (software-version 'chezscheme
    (let-syntax ([scheme-version
                  (lambda (x)
                    (format "~{~a~^.~}" (call-with-values scheme-version-number list)))])
      scheme-version))
  (software-revision 'chezscheme
    (include-line "swish/chezscheme-revision.include"))
  (json:set! info '(chezscheme machine-type) (symbol->string (machine-type)))
  )

#!eof mats

(import (swish mat) (swish software-info))

(mat software-revision ()
  (define swish-hash (include-line "swish/swish-revision.include"))
  (define chezscheme-hash (include-line "swish/chezscheme-revision.include"))
  (define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
  (match-let*
   ([#f (software-revision 'xyz)]
    [,_ (software-revision 'xyz "pdq")]
    ["pdq" (software-revision 'xyz)]
    [,_ (json:delete! (software-info) 'xyz)] ;; try to keep xyz out of coverage report
    [#(EXIT #(bad-arg software-revision "string"))
     (catch (software-revision "string"))]
    [#(EXIT #(bad-arg software-version bol))
     (catch (software-version 'sym 'bol))]
    [#(EXIT #(bad-arg software-product-name 123))
     (catch (software-product-name 'zyx 123))]
    [,@swish-hash (software-revision)]
    [,@swish-hash (software-revision 'swish)]
    [,@chezscheme-hash (software-revision 'chezscheme)]
    [#f (software-revision 'abc)]
    [,_ (software-revision 'foo "bar")]
    [,_ (software-revision 'foo "write-once by default; can json:update! info if needed")]
    ["bar" (software-revision 'foo)]
    [#f (software-product-name 'foo)]
    [,_ (software-product-name 'foo "Foo")]
    [,_ (software-product-name 'foo "see above")]
    ["Swish" (software-product-name)]
    ["Foo" (software-product-name 'foo)]
    [,_ (json:delete! (software-info) 'foo)] ;; try to keep foo out of coverage report
    [,cs-version (scheme-version)]
    [,@cs-version
     (parameterize ([app:name "chezscheme"])
       (format "~a Version ~a"
         (software-product-name)
         (software-version)))]
    [#f (json:ref (software-info) '(abc version) #f)]
    [,@swish-hash (json:ref (software-info) '(swish revision) #f)]
    [#(machine-type product-name revision version)
     (vector-sort symbol<?
       (hashtable-keys
        (json:ref (software-info) 'chezscheme #f)))]
    [#(product-name revision version)
     (vector-sort symbol<?
       (hashtable-keys
        (json:ref (software-info) 'swish #f)))]
    [#(EXIT ,reason) (catch (eval '(include-line not-a-string-constant)))]
    [#t (starts-with? (exit-reason->english reason)
          "Exception: expected path as string")]
    [#(EXIT ,reason) (catch (eval '(include-line "no-such-file")))]
    ["Exception: file not found \"no-such-file\"." (exit-reason->english reason)]
    [#(EXIT "no-such-file") (catch (eval '(include-line "no-such-file" raise)))]
    [("no-such-file") (catch (eval '(include-line "no-such-file" list)))]
    )
   'ok))
