(import (scheme))

(let-syntax ([_ (begin ;; run this code at expand time
                  (compile-imported-libraries #t)
                  (if (equal? (getenv "PROFILE_MATS") "yes")
                      (library-directories '(("." . "../build/profile/lib")))
                      (library-directories '(("." . "../build/release/lib"))))
                  (include "osi-bootstrap.ss")
                  void)])
  (void))

(import (swish imports))
(base-dir (path-parent (cd)))
(apply swish-start (command-line-arguments))
