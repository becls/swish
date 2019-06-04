;; Replace this placeholder with a page suitable for your application.
(http:respond op 200 '(("Content-Type" . "text/html"))
  (html->bytevector
   `(html5
     (head
      (meta (@ (charset "UTF-8")))
      (title "Main Page"))
     (body
      (h1 "Main Page")
      ,(format "~a Version ~a "
         (software-product-name)
         (software-version))
      (a (@ (href "swish/index")) "Diagnostics Pages")
      "."))))
