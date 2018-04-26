(import (scheme))

(meta-cond
 [(begin ;; eval-when expand
    (compile-imported-libraries #t)
    (library-directories '(("." . "../lib")))
    (when (equal? (getenv "PROFILE_MATS") "yes")
      (library-extensions '((".ss" . ".sop")))
      (compile-profile #t)
      (cp0-effort-limit 0)))
  (void)])

(import (swish imports))
((swish-start) (command-line-arguments))
