(include "../osi-bootstrap.ss")
(compile-file-message #f)
(compile-library-handler
 (lambda (source dest)
   (printf "compiling ~a\n" source)
   ;; During development, we often use the src/go script to experiment with changes.
   ;; For speed, that script does not generate wpo files, but that means a subsequent
   ;; "make" could find .wpo files that are out of date.
   ;;
   ;; We now set (library-extensions '((".ss" . ".wpo"))) when building swish.library
   ;; so that the expander calls the compile-library-handler if the .wpo file is out
   ;; of date. We force a .so suffix on the dest so that compile-library does not
   ;; interfere with (generate-wpo-files).
   (compile-library source (string-append (path-root dest) ".so"))))
(compile-imported-libraries #t)
(when (equal? (getenv "PROFILE_MATS") "yes")
  (compile-profile #t)
  (compile-interpret-simple #f)
  (cp0-effort-limit 0)
  (run-cp0 (lambda (f x) x)))
(cd "..")
(new-cafe
 (lambda (x)
   (reset-handler abort)
   (eval x)))
