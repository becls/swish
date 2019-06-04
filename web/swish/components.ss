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

(define (redirect url)
  (http:respond op 302 `(("Location" . ,url)) '#vu8()))

(define (non-hosted-page op params page-title heads . content)
  (http:respond op 200 '(("Content-Type" . "text/html"))
    (html->bytevector
     `(html5
       (head
        (meta (@ (charset "UTF-8")))
        (title ,page-title)
        ,(css-include "css/components.css")
        ,(js-include "js/components.js")
        ,@heads)
       (body
        ,@content
        (div (@ (class "undocked menu"))
          ,(link "#Debugging" "Debugging")
          (div (@ (class "menu item"))
            ,(panel "Params" `(p ,(format "~a\n" params))))
          ,(link "#Navigation" "Navigation")
          (div (@ (class "menu item"))
            ,(navigation))))))))

(define (hosted-page page-title heads . content)
  (http:respond op 200 '(("Content-Type" . "text/html"))
    (html->bytevector
     `(html5
       (head
        (meta (@ (charset "UTF-8")))
        (title ,page-title)
        ,(css-include "css/components.css")
        ,(js-include "js/components.js")
        ,@heads)
       (body
        ,(docked-navigation)
        ,(column "content right"
           (apply panel page-title content))
        (div (@ (class "undocked menu"))
          ,(link "#Debugging" "Debugging")
          (div (@ (class "menu item"))
            ,(panel "Params" `(p ,(format "~a\n" params))))))))))

;;HTML Helpers
(define (link url anchor)
  `(a (@ (href ,url)) ,anchor))

(define (js-include location)
  `(script (@ (src ,location))))

(define (css-include location)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,location))))

(define (table . content)
  `(table (@ (style "padding: 0; border-spacing: 0;")) ,@content))

;;Page Helpers

(define (docked-navigation)
  (column-with-id "main-nav" "docked menu left" (navigation)))

(define (navigation)
  (panel (osi_get_hostname)
    (section (software-product-name)
      (link "index" "Home")
      (link "charts" "Charts")
      (link "errors?type=child&sql=&limit=100&offset=0" "Child Errors")
      (link "errors?type=gen-server&sql=&limit=100&offset=0" "Gen-Server Errors")
      (link "errors?type=supervisor&sql=&limit=100&offset=0" "Supervisor Errors")
      (link "query-db" "Log DB")
      (link "debug" "Debug"))))

(define (stilts height)
  `(div (@ (style ,(format "height:~apx;" height)) (class "stilts"))))

;;Form Helpers
(define (column type . content)
  `(div (@ (class ,(format "~a column" type))) ,@content))

(define (column-with-id id type . content)
  `(div (@ (id ,id) (class ,(format "~a column" type))) ,@content))

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
