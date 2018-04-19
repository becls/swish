(import (scheme))

(meta-cond
 [(and (equal? (getenv "PROFILE_MATS") "yes")
       (compile-profile #t)
       (cp0-effort-limit 0))
  (void)])

(compile-imported-libraries #t)
(library-directories '(("." . "../lib")))

(import (swish imports))
((swish-start) (command-line-arguments))
