(import (swish app))
(eval '(let () (import (swish internal)) ($import-internal)))
(suppress-greeting #t)
(scheme-start swish-start)
