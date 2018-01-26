;;; Copyright 2017 Beckman Coulter, Inc.
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

;;; Usage: (go) or (go <profile-filename>)
(import (swish app-io) (swish io))

(define go
  (case-lambda
   [() (go (path-combine data-dir "server.profile"))]
   [(filename)
    (if (file-regular? filename)
        (let ([ip (open-file-input-port filename
                    (file-options compressed)
                    (buffer-mode block))])
          (let lp ([data '()])
            (let ([ls (fasl-read ip)])
              (if (eof-object? ls)
                  (begin
                    ($profile-dump-html data)
                    (close-port ip))
                  (lp (append ls data))))))
        (display "no profile to dump\n"))]))

(let ()
  (import (chezscheme) (swish meta))

  (define (find-src x)
    (if (source-object? x)
        x
        (and (vector? x) (fx> (vector-length x) 1)
             (find-src (vector-ref x 1)))))

  (define (commas n)
    (cond
     [(and (or (fixnum? n) (bignum? n)) (>= n 0))
      (let ([s (number->string n)])
        (do ([i 0 (fx+ i 1)]
             [len (string-length s)]
             [op (open-output-string)])
            ((fx= i len) (get-output-string op))
          (when (and (fx> i 0) (fx= (fxremainder (fx- len i) 3) 0))
            (write-char #\, op))
          (write-char (string-ref s i) op)))]
     [else (number->string n)]))

  (define (annotate name data profile-op)
    ;; data = ((source . count) ...) in order of ascending bfp
    (let  ([ip (open-input-file name)]
           [op (open-output-file (string-append name ".html") 'replace)]
           [max-count (do ([ls data (cdr ls)] [n 1 (max (cdar ls) n)])
                          ((null? ls) n))]
           [hits (fold-left (lambda (hit p) (if (= (cdr p) 0) hit (+ hit 1)))
                   0 data)]
           [sites (length data)])
      (when (> sites 0)
        (let ([h-name (html-encode (stringify name))])
          (output-row profile-op
            (format "<a href=\"~a.html\">~a</a>" h-name h-name)
            hits
            sites
            (round (* (/ hits sites) 100))
            max-count)))
      (emit-header (sanitize name) op)
      (let loop ([fp 0] [ls data] [stack '()])
        (cond
         [(and (pair? stack) (= (car stack) fp))
          (display "</span>" op)
          (loop fp ls (cdr stack))]
         [(and (pair? ls) (= (source-object-bfp (caar ls)) fp))
          (let ([count (cdar ls)])
            (fprintf op "<span title=~a style=\"background-color: ~a\">"
              (commas count) (colorize count max-count)))
          (loop fp (cdr ls) (cons (source-object-efp (caar ls)) stack))]
         [else
          (let ([c (read-char ip)])
            (cond
             [(eof-object? c)
              (emit-trailer op)
              (close-input-port ip)
              (close-output-port op)]
             [else
              (print-char c op)
              (loop (+ fp 1) ls stack)]))]))))

  (define (colorize x max-count)
    (cond
     [(= x 0) "#C0C0C0"] ;; gray
     [(= max-count 1) "#FFFF80"] ;; yellow
     [else
      ;; from red to yellow on a log scale
      (let ([s (number->string
                (+ (inexact->exact
                    (round (* (/ (log x) (log max-count)) #x7F)))
                   #x80)
                16)])
        (string-append "#FF" s "80"))]))

  (define (sanitize s)
    (do ([i 0 (fx+ i 1)] [n (string-length s)] [op (open-output-string)])
        ((fx= i n) (get-output-string op))
      (print-char (string-ref s i) op)))

  (define (print-char c op)
    (case c
      [(#\") (display "&quot;" op)]
      [(#\<) (display "&lt;" op)]
      [(#\>) (display "&gt;" op)]
      [(#\&) (display "&amp;" op)]
      [else (write-char c op)]))

  (define (stringify x)
    (if (string? x)
        x
        (format "~s" x)))

  (define (html-encode s)
    (let-values ([(op get) (open-string-output-port)])
      (let ([len (string-length s)])
        (do ((i 0 (+ i 1))) ((= i len))
          (let ([c (string-ref s i)])
            (print-char c op)))
        (get))))

  (define (emit-header name op)
    (fprintf op
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
<title>~a</title>
<style type=\"text/css\">
<!--
SPAN
{
 border-width: 1px;
               border-top-width: 0;
               border-bottom-width: 0;
               border-style: solid;
               }
-->
</style>
</head>
<body>
<h1>~a</h1>

<pre>
" name name))

(define (emit-trailer op)
  (display "
</pre>
</body>
</html>" op))

(define (output-row op c1 c2 c3 c4 c5)
  (fprintf op
    "<tr><td>~a</td><td>~a</td><td>~a</td><td>~a</td><td>~a</td></tr>\n"
    c1
    (html-encode (stringify c2))
    (html-encode (stringify c3))
    (cond
     [(fixnum? c4)
      (format "<p style='color:~a'>~d%</p>"
        (cond
         [(<= c4 50) "#FF0000"]
         [(< c4 80) "#FF8800"]
         [else "#007F00"])
        c4)]
     [else
      (html-encode (stringify c4))])
    (html-encode (stringify c5))))

(define (view dump)
  (let ([table (make-skiplist sfd<?)]
        [op (open-output-file "server-profile.html" 'replace)])
    (for-each (lambda (item) (add-filedata item table)) dump)
    (fprintf op "<html>\n")
    (fprintf op "<body style='font-family:monospace;'>\n")
    (let-values ([(hits sites percentage) (summarize-coverage (table))])
      (fprintf op
        "<h2>Overall ~a% coverage with ~a of ~a sites covered.\n</h2>"
        percentage hits sites))
    (fprintf op "<table>\n")
    (output-row op "filename" "hits" "sites" "coverage" "max-count")
    (for-each
     (lambda (entry)
       ;; entry = (sfd . skiplist | #f)
       (let ([sl (cdr entry)])
         (when sl
           (annotate (source-file-descriptor-path (car entry)) (sl) op))))
     (table))
    (fprintf op "</table>\n")
    (fprintf op "</body></html>\n")
    (close-output-port op)))

(define (summarize-coverage dump)
  (let lp ([hits 0] [sites 0] [rest dump])
    (if (eq? '() rest)
        (values hits sites (if (= sites 0) 100 (round (* 100 (/ hits sites)))))
        (let* ([sl (cdr (car rest))]
               [data (if sl (sl) '())]
               [new-hits
                (fold-left (lambda (hit p) (if (= (cdr p) 0) hit (+ hit 1)))
                  0 data)]
               [new-sites (length data)])
          (lp (+ hits new-hits) (+ sites new-sites) (cdr rest))))))

(define (add-filedata item table)
  ;; item = (crecord . count)
  (cond
   [(find-src (car item)) =>
    (lambda (source)
      (let ([sfd (source-object-sfd source)]
            [count (cdr item)])
        (define (add-it sl)
          (sl source
            (lambda (x y insert update)
              (if (source-bfp=? x y)
                  (update + count)
                  (insert count))))
          sl)
        (table sfd
          (lambda (x y insert update)
            (cond
             [(sfd=? x y)
              (update (lambda (sl false) (and sl (add-it sl))) #f)]
             [(invalid-sfd? sfd) =>
              (lambda (reason)
                (printf "Skipping ~a: ~a\n"
                  (source-file-descriptor-path sfd)
                  reason)
                (insert #f))]
             [else
              (insert (add-it (make-skiplist source-bfp<?)))])))))]))

(define (invalid-sfd? sfd)
  (let ([name (source-file-descriptor-path sfd)])
    (cond
     [(path-absolute? name) "path specified"]
     [(not (file-exists? name)) "file not found"]
     [else
      (let* ([ip (open-file-input-port name)]
             [sfd2 (make-source-file-descriptor name ip)])
        (close-input-port ip)
        (if (sfd=? sfd sfd2)
            #f
            "file changed"))])))

(define (source-bfp<? x y)
  (< (source-object-bfp x) (source-object-bfp y)))

(define (source-bfp=? x y)
  (and (source-object? x) (source-object? y) (= (source-object-bfp x) (source-object-bfp y))))

(define (sfd<? x y)
  (let ([x-name (source-file-descriptor-path x)]
        [y-name (source-file-descriptor-path y)])
    (or (string<? x-name y-name)
        (and (string=? x-name y-name)
             (< (source-file-descriptor-checksum x)
                (source-file-descriptor-checksum y))))))

(define (sfd=? x y)
  (and (source-file-descriptor? x)
       (source-file-descriptor? y)
       (string=? (source-file-descriptor-path x)
         (source-file-descriptor-path y))
       (= (source-file-descriptor-checksum x)
          (source-file-descriptor-checksum y))))

(define make-skiplist
  (case-lambda
   [(key<) (make-skiplist 1000000 key<)]
   [(size key<)
    (let* ([p-inv 5]
           [max-forward
            (max (inexact->exact (round (/ (log size) (log p-inv)))) 1)]
           [first-forward 2]
           [last-forward (+ first-forward max-forward -1)]
           [maxsize (+ last-forward 1)]
           [first (make-vector maxsize #f)]
           [update (make-vector maxsize #f)])

                                        ; key
      (define (item-key x) (vector-ref x 0))
      (define (set-key! x y) (vector-set! x 0 y))

                                        ; value
      (define (item-value x) (vector-ref x 1))
      (define (set-value! x y) (vector-set! x 1 y))

                                        ; forward
      (define (item-forward x i) (vector-ref x i))
      (define (set-forward! x i y) (vector-set! x i y))

      (define (item-top x) (fx- (vector-length x) 1))

      (define random-size
        (let* ([p-denom (expt 2 28)]
               [p-num (fx/ p-denom p-inv)])
          (lambda ()
            (let loop ([size (fx+ first-forward 1)])
              (cond
               [(fx= size maxsize) maxsize]
               [(fx< (random p-denom) p-num) (loop (fx+ size 1))]
               [else size])))))

      (define (make-item key value)
        (let ([item (make-vector (random-size) #f)])
          (set-key! item key)
          (set-value! item value)
          item))

      (case-lambda
       [(key f)
        (let find ([x first] [i last-forward])
          (let ([z (item-forward x i)])
            (cond
             [(and z (not (key< key (item-key z))))
              (find z i)]
             [(fx> i first-forward)
              (vector-set! update i x)
              (find x (fx- i 1))]
             [else
              (vector-set! update i x)
              (f (item-key x) key
                (lambda (value)
                  (let ([y (make-item key value)])
                    (let loop ([i (item-top y)])
                      (when (fx>= i first-forward)
                        (let ([x (vector-ref update i)])
                          (set-forward! y i (item-forward x i))
                          (set-forward! x i y))
                        (loop (fx- i 1))))))
                (lambda (f value)
                  (set-value! x (f (item-value x) value))))])))]
       [()
        (let loop ([x (item-forward first first-forward)])
          (if x
              (cons (cons (item-key x) (item-value x))
                (loop (item-forward x first-forward)))
              '()))]))]))

(set! $profile-dump-html
  (case-lambda
   [()
    (collect (collect-maximum-generation))
    (view (profile-dump))]
   [(ls)
    (unless (list? ls)
      (error 'p-view "optional parameter must be a list"))
    (view ls)]))
)
