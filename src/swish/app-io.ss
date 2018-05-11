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

#!chezscheme
(library (swish app-io)
  (export
   base-dir
   data-dir
   log-path
   tmp-path
   web-path
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish io)
   (swish software-info)
   )

  (define data-dir (make-parameter #f))

  (define log-path (make-parameter #f))

  (define tmp-path (make-parameter #f))

  (define web-path (make-parameter #f))

  (define base-dir
    (make-parameter (path-parent (cd))
      (lambda (base)
        (match (catch (directory? base))
          [#t
           (data-dir (path-combine base "data"))
           (log-path (path-combine (data-dir) "Log.db3"))
           (tmp-path (path-combine (data-dir) "tmp"))
           (web-path (path-combine base "web"))
           base]
          [#(EXIT ,reason) (exit reason)]
          [#f (errorf 'base-dir "no directory ~s" base)]))))
  )
