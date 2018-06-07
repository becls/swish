(import (scheme))

(let-syntax ([_ (begin ;; run this code at expand time
                  (compile-imported-libraries #t)
                  (library-directories '(("." . "../lib")))
                  (when (equal? (getenv "PROFILE_MATS") "yes")
                    (library-extensions '((".ss" . ".sop")))
                    (compile-profile #t)
                    (cp0-effort-limit 0))
                  void)])
  (void))

(import (swish imports))
(base-dir (path-parent (cd)))
((swish-start) (command-line-arguments))
