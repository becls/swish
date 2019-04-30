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

(library (swish app-io)
  (export
   app:config
   app:config-filename
   base-dir
   data-dir
   log-file
   tmp-dir
   web-dir
   )
  (import
   (chezscheme)
   (swish app-core)
   (swish erlang)
   (swish io)
   (swish json)
   (swish software-info)
   )

  (define (reject config-file reason)
    (raise `#(invalid-config-file ,config-file ,reason)))

  (define config (make-process-parameter #f))

  ;; load config on demand and cache result
  (define app:config
    (case-lambda
     [()
      (or (config)
          (let ([cfg (load-config)])
            (config cfg)
            cfg))]
     [(v)
      (unless (or (not v) (hashtable? v))
        (bad-arg 'app:config v))
      (config v)]))

  (define (load-config)
    (match (app:config-filename)
      [,filename
       (guard (regular-file? filename))
       (match (catch (json:bytevector->object (read-file filename)))
         [,ht
          (guard (hashtable? ht))
          ht]
         [#(EXIT ,reason) (reject filename reason)]
         [,_ (reject filename 'expected-dictionary)])]
      [,filename
       (json:make-object)]))

  (define (app:config-filename)
    (cond
     [(app:path) => installed-app-config-file]
     [else (path-combine (base-dir) ".config")]))

  ;; If app is installed as path/bin/app, then look in path/lib/app/config.
  ;; If app is installed at some other path, look for path/app.config.
  (define (installed-app-config-file p)
    (let ([dir (path-parent p)])
      (match (path-last dir)
        ["bin"
         (let ([app (path-root (path-last p))])
           (path-combine (path-parent dir) "lib" app "config"))]
        [,_ (string-append (path-root p) ".config")])))

  (define data-dir (make-parameter #f))

  (define log-file (make-parameter #f))

  (define tmp-dir (make-parameter #f))

  (define web-dir (make-parameter #f))

  (define base-dir
    (make-parameter (cd)
      (lambda (base)
        (match (catch (directory? base))
          [#t
           (data-dir (path-combine base "data"))
           (log-file (path-combine (data-dir) "Log.db3"))
           (tmp-dir (path-combine (data-dir) "tmp"))
           (web-dir (path-combine base "web"))
           base]
          [#(EXIT ,reason) (raise reason)]
          [#f (errorf 'base-dir "no directory ~s" base)]))))
  )
