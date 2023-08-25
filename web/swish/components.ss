;;; Copyright 2018 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(define (hosted-page page-title heads . content)
  (http:respond conn 200 '(("Content-Type" . "text/html"))
    (html->bytevector
     `(html5
       (head
        (meta (@ (charset "UTF-8")))
        (title ,page-title)
        ,(css-include "css/components.css")
        ,@heads)
       (body
        ,(navigation page-title)
        (div (@ (class "content")) ,@content)
        (div (@ (class "undocked menu"))
          ,(link "#Debugging" "Debugging")
          (div (@ (class "menu item"))
            ,(panel "Params"
               `(pre ,(let ([op (open-output-string)])
                        (json:write op params 0)
                        (get-output-string op)))))))))))

;;HTML Helpers
(define (link url anchor)
  `(a (@ (href ,url)) ,anchor))

(define (js-include location)
  `(script (@ (type "text/javascript") (src ,location))))

(define (css-include location)
  `(link (@ (rel "stylesheet")
            (href ,location))))

(define (table . content)
  `(table (@ (style "padding: 0; border-spacing: 0;")) ,@content))

;;Page Helpers

(define (navigation page-title)
  (define (maybe-link url title)
    (if (equal? title page-title)
        `(span ,title)
        (link url title)))
  (define name (software-product-name))
  `(div (@ (id "main-nav"))
     (div (@ (class "hostinfo"))
       ,(when name `(span ,name "@"))
       (span ,(osi_get_hostname)))
     ,(maybe-link "index" "Home")
     ,(maybe-link "charts" "Charts")
     ,(maybe-link "errors?type=child&sql=&limit=100&offset=0" "Child Errors")
     ,(maybe-link "errors?type=gen-server&sql=&limit=100&offset=0" "Gen-Server Errors")
     ,(maybe-link "errors?type=supervisor&sql=&limit=100&offset=0" "Supervisor Errors")
     ,(maybe-link "query-db" "Log DB")
     ,(maybe-link "debug" "Debug")))

(define (panel header . content)
  `(div (@ (class "panel"))
     ;;things to think on: scrolling inside of the panel
     (h2 ,header)
     ,@content))

(define (section header . content)
  `(div (@ (class "section"))
     (h3 ,header)
     ,@content))

(define (subsection header . content)
  `(div
    (h4 ,header)
    ,@content))

(define (row label . content)
  `(div (@ (class "row"))
     (label ,label)
     ,@content))
