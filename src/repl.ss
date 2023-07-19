(import (scheme))

(let-syntax ([_ (begin ;; run this code at expand time
                  (compile-imported-libraries #t)
                  (undefined-variable-warnings (equal? (getenv "WARN_UNDEFINED") "yes"))
                  (let ([base (path-parent (cd))]
                        [which (if (equal? (getenv "PROFILE_MATS") "yes")
                                   'profile
                                   'release)]
                        [sep (directory-separator)])
                    (library-directories
                     `(("." . ,(format "~a~cbuild~c~a~clib"
                                 base sep sep which sep)))))
                  (include "osi-bootstrap.ss")
                  void)])
  (void))

(import (swish imports))
(base-dir (path-parent (cd)))
(apply swish-start (command-line-arguments))
