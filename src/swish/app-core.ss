;;; Copyright 2019 Beckman Coulter, Inc.
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

(library (swish app-core)
  (export
   app-exception-handler
   app:name
   app:path
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish errors)
   (swish io))

  ;; intended to return short descriptive name of the application if known
  (define app:name
    (make-parameter #f
      (lambda (x)
        (unless (or (not x) (string? x))
          (bad-arg 'app:name x))
        (and x (path-root (path-last x))))))

  ;; intended to return full path to the application script or executable, if known
  (define app:path
    (make-parameter #f
      (lambda (x)
        (unless (or (not x) (string? x))
          (bad-arg 'app:path x))
        (and x (get-real-path x)))))

  (define (strip-prefix string prefix)
    (define slen (string-length string))
    (define plen (string-length prefix))
    (and (> slen plen)
         (string=? (substring string 0 plen) prefix)
         (substring string plen slen)))

  (define (claim-exception who c)
    (define stderr (console-error-port))
    (define os (open-output-string))
    (fprintf stderr "~a: " who)
    (guard (_ [else (display-condition c os)])
      (cond
       [(condition? c)
        (display-condition (condition (make-who-condition #f) c) os)
        (let ([text (get-output-string os)])
          (display (or (strip-prefix text "Warning: ")
                       (strip-prefix text "Exception: ")
                       text)
            os))]
       [else (display (exit-reason->english c) os)]))
    ;; add final "." since display-condition does not and exit-reason->english may or may not
    (let ([i (- (port-output-index os) 1)])
      (when (and (> i 0) (not (char=? #\. (string-ref (port-output-buffer os) i))))
        (display "." os)))
    (display (get-output-string os) stderr)
    (fresh-line stderr)
    (reset))

  (define (app-exception-handler c)
    (cond
     [(app:name) => (lambda (who) (claim-exception who c))]
     [else (default-exception-handler c)]))
  )
