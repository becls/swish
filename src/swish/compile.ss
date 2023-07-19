(include "../osi-bootstrap.ss")
(generate-inspector-information #f)
(generate-procedure-source-information #t)

(define target-file (make-parameter #f))

(define (bail)
  (cond [(target-file) => delete-file])
  (abort))

;; jump through hoops for less verbose compile-file message
(define swish-compile-library
  (let ([stock-compile-library (compile-library-handler)])
    (lambda (source dest)
      (printf "compiling ~a\n" source)
      (parameterize ([compile-file-message #f]
                     [undefined-variable-warnings (equal? (getenv "WARN_UNDEFINED") "yes") ]
                     [print-extended-identifiers #t]
                     [target-file dest])
        (stock-compile-library source dest)))))
(compile-library-handler swish-compile-library)

(define wpo-disabled (make-hashtable equal-hash equal?))
(define wpo-disabled-object (make-hashtable string-hash string=?))
(define (exclude-from-wpo filename)
  (let ([pre-existing (make-hashtable equal-hash equal?)])
    (for-each (lambda (lib) (hashtable-set! pre-existing lib #t)) (library-list))
    (load filename)
    (for-each
     (lambda (lib)
       (unless (hashtable-contains? pre-existing lib)
         (hashtable-set! wpo-disabled lib #t)
         (hashtable-set! wpo-disabled-object (library-object-filename lib) #t)))
     (library-list))))

(define (verify-excluded-wpo missing)
  (let* ([expected-missing (hashtable-copy wpo-disabled #t)]
         [unexpected-missing
          (fold-left
           (lambda (excluded lib)
             (cond
              [(hashtable-ref expected-missing lib #f)
               (hashtable-delete! expected-missing lib)
               excluded]
              [else (cons lib excluded)]))
           '()
           missing)]
         [excludes-okay
          (or (null? unexpected-missing)
              (begin
                (printf "The following were excluded from wpo by mistake:\n~{  ~s\n~}" unexpected-missing)
                #f))]
         [includes-okay
          (or (zero? (hashtable-size expected-missing))
              (begin
                (printf "The following were included in wpo by mistake:\n~{  ~s\n~}"
                  (vector->list (hashtable-keys expected-missing)))
                #f))])
    (and excludes-okay includes-okay)))

(define (wpo-make-library lib-dir src-file dest-file)
  (define must-rebuild? (not (file-exists? dest-file)))
  (library-directories `(("." . ,lib-dir)))
  (parameterize ([generate-wpo-files #t]
                 [compile-imported-libraries #t]
                 [library-extensions '((".ss" . ".so"))]
                 [compile-library-handler
                  (lambda (source dest)
                    (set! must-rebuild? #t)
                    (swish-compile-library source dest))])
    ;; Don't force a compilation of src-file.
    ;; Instead, import the library that src-file provides and let
    ;; compile-imported-libraries and library-search-handler determine
    ;; what we need to recompile.
    (syntax-case (read (open-input-file src-file)) (library)
      [(library lib-path exports imports body ...)
       (eval (datum (import lib-path)))]))
  (cond
   [must-rebuild?
    (printf "compiling ~a\n" (path-last dest-file))
    (let ([missing
           (parameterize ([library-extensions '((".ss" . ".so"))])
             (compile-whole-library
              (string-append lib-dir "/" (path-root src-file) ".wpo")
              dest-file))])
      (unless (verify-excluded-wpo missing)
        (delete-file dest-file)
        (abort)))]
   [else (printf "~a is up to date\n" (path-last dest-file))]))

;; The custom library-search-handler here serves two purposes:
;;
;;  - First, we want to exclude some libraries from wpo optimization
;;    while compiling certain compound libraries, yet we want to re-use
;;    the .wpo files when (re-)compiling other compound libraries.
;;
;;  - Second, we often use the src/go script to experiment with changes
;;    during development. For speed, that script does not generate wpo files,
;;    but that means a subsequent "make" could find outdated .wpo files if we
;;    didn't force a recompile here when the .so exists but .wpo is out of date.
(library-search-handler
 (let ([default-library-search (library-search-handler)])
   (lambda (who path dir* all-ext*)
     (let ([extensions (map cdr all-ext*)]
           [object-file (guard (c [else #f]) (library-object-filename path))])
       (cond
        [(member ".so" extensions)
         (if object-file
             (values #f object-file (file-exists? object-file))
             (let-values ([(src-path obj-path obj-exists?)
                           (default-library-search who path dir* all-ext*)])
               (values src-path obj-path
                 (and obj-exists?
                      (or (not src-path)
                          (not (generate-wpo-files))
                          ;; finding ".so" file is not enough; we also need an
                          ;; up-to-date ".wpo" file for the use by compile-wpo
                          (let ([wpo-file (string-append (path-root obj-path) ".wpo")])
                            (and (file-exists? wpo-file)
                                 ;; account for low-resolution HFS timestamps
                                 (time>=?
                                  (file-modification-time wpo-file)
                                  (file-modification-time src-path)))))))))]
        [(member ".wpo" extensions)
         (if (and object-file (hashtable-ref wpo-disabled-object object-file #f))
             (values #f #f #f)
             (default-library-search who path dir* all-ext*))]
        [else (errorf 'compile.ss "broken")])))))

(compile-imported-libraries #t)
(when (equal? (getenv "PROFILE_MATS") "yes")
  (compile-profile #t)
  (compile-interpret-simple #f)
  (cp0-effort-limit 0)
  (run-cp0 (lambda (f x) x)))
(cd "..")
(call-with-values
  (lambda ()
    (new-cafe
     (lambda (x)
       (keyboard-interrupt-handler bail)
       (reset-handler bail)
       (eval x))))
  exit)
