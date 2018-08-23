(import (scheme))

(let-syntax ([_ (begin ;; run this code at expand time
                  (compile-imported-libraries #t)
                  (if (equal? (getenv "PROFILE_MATS") "yes")
                      (library-directories '(("." . "../build/profile/lib")))
                      (library-directories '(("." . "../build/release/lib"))))
                  void)])
  (void))

(import (swish imports))
(base-dir (path-parent (cd)))
app:name ;; force initialization of (swish app) library, which sets scheme-start
(apply (scheme-start) (command-line-arguments))
