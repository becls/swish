(library (software-info)
  (export
   software-company
   software-company-dir
   software-copyright-year
   software-internal-name
   software-product-name
   software-version
   )
  (import (chezscheme))

  ;; The content of this library is also parsed by software-info.pl
  ;; and used in the build system, so it must use simple string
  ;; definitions.

  (define software-company "Beckman Coulter, Inc.")

  (define software-company-dir "Beckman Coulter")

  (define software-copyright-year "2017")

  ;; The software-internal-name value is used as the name for the
  ;; compiled exe and boot files. It must not contain spaces or any
  ;; other character that requires quoting in GNU make.

  (define software-internal-name "Swish")

  (define software-product-name "Swish")

  (define software-version "1.0.0.0")

  )
