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

(import
 (chezscheme)
 (swish app-io)
 (swish erlang)
 (swish event-mgr)
 (swish events)
 (swish http)
 (swish io)
 (swish mat)
 (swish script-testing)
 (swish string-utils)
 (swish testing)
 (swish watcher)
 (swish websocket)
 )

(define web-dir (make-parameter (path-combine (output-dir) "web-mats")))

(define (write-web-mats)
  (define (write-exprs fn exprs)
    (write-test-file (path-combine "web-mats" fn)
      (lambda () (for-each pretty-print exprs))))

  (write-exprs "common.ss"
    '((define (my-respond x)
        (http:respond conn 200 '() x))))
  (write-exprs (path-combine "include" "mat.ss")
    '((http:include "../mat.ss")))
  (write-exprs (path-combine "include" "rooted.ss")
    '((http:include "/mat.ss")))
  (write-exprs (path-combine "include" "syntax-error.ss")
    '((http:include 12345)))
  (write-exprs (path-combine "static" "ok.html")
    `(,(string->symbol (html->string `(html (body (p "ok")))))))
  (write-exprs (path-combine "static" "ok.txt")
    '(ok))
  (write-exprs (path-combine "with-index" "index.ss")
    '((http:respond conn 200 '() (string->utf8 "ok"))))
  (write-exprs "mime-types"
    '(("css" . "text/css")
      ("gif" . "image/gif")
      ("html" . "text/html")
      ("ico" . "image/x-icon")
      ("jpg" . "image/jpeg")
      ("js" . "application/javascript")
      ("png" . "image/png")))
  (write-exprs "mat.ss"
    '((http:include "common.ss")
      (match (get-param "cmd")
        ["echo header"
         (my-respond
          (string->utf8
           (let-values ([(op get) (open-string-output-port)])
             (write
              (let-values ([(keys vals) (hashtable-entries header)])
                (define (symbol<? a b)
                  (string<? (symbol->string a) (symbol->string b)))
                (vector->list
                 (vector-sort
                  (lambda (x y) (symbol<? (car x) (car y)))
                  (vector-map cons keys vals))))
              op)
             (get))))]
        ["use status"
         (http:respond conn (string->number (get-param "status")) '() '#vu8())]
        ["echo"
         (my-respond (string->utf8 (get-param "echo")))]
        ["echo form"
         (http:call-with-form conn header 4194304 0 '()
           (lambda (fd)
             (my-respond (json:object->bytevector fd))))]
        ["echo file"
         (http:call-with-form conn header 4194304 4200288 '(file)
           (lambda (fd)
             (match-let*
              (["1" (json:ref fd 'sample #f)]
               [,ht (json:ref fd 'file #f)]
               ["file" (json:ref ht 'type #f)]
               [,filename (json:ref ht 'filename #f)])
              (my-respond (read-file filename)))))]
        ["echo unhandled"
         (my-respond
          (http:call-with-ports conn
            (lambda (ip op)
              (get-bytevector-n ip
                (string->number
                 (or (find-param "count")
                     (http:get-header "Content-Length" header)))))
            1000))]
        ["bad arg"
         (my-respond (get-param "bad"))]
        ["file-headers"
         (http:respond-file conn 200 '(("Cache-Control" . "no-cache")
                                       ("Content-Type" . "text/plain"))
           (path-combine (web-dir) "static" "ok.txt"))]
        ["internal-error"
         (cond
          [(find-param "partial") =>
           (lambda (x)
             (http:call-with-ports conn
               (lambda (ip op)
                 (put-bytevector op (string->utf8 x)))))])
         (throw 'internal-error)]
        ["bad read"
         ;; consume input, but don't respond or error
         (http:call-with-ports conn
           (lambda (ip op)
             (do ([n (string->number (get-param "count")) (- n 1)])
                 ((= n 0))
               (get-u8 ip))
             #t))]
        ["no response" #t]
        ["double response"
         (define (resp x)
           (match x
             ["file"
              (http:respond-file conn 200 '()
                (path-combine (web-dir) "static" "ok.txt"))]
             ["direct"
              (my-respond (string->utf8 "direct"))]))
         (resp (get-param "first"))
         (resp (get-param "second"))]
        ["switch-protocol"
         (http:switch-protocol
          (match (get-param "case")
            ["1" 12]
            ["2" (lambda (ip op)
                   (put-bytevector op (string->utf8 "switched"))
                   (flush-output-port op)
                   (close-port op))]))]
        ))))

(define (start-default-file-server)
  (write-web-mats)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-file-server #f 0 (web-dir)
      (http:options
       [request-timeout 500]
       [file-search-extensions '(".ss" ".html")]
       [file-transform http:enable-dynamic-pages]))))

(define (shutdown-http)
  (cond
   [(whereis 'http-sup) =>
    (lambda (pid)
      (unlink pid)
      (monitor pid)
      (kill pid 'shutdown)
      (receive (after 2000 (throw 'http-sup-still-running))
        [`(DOWN ,_ ,@pid ,_) 'ok]))]))

(define-syntax http-mat
  (syntax-rules ()
    [(_ name tags e1 e2 ...)
     (isolate-mat name tags
       (start-silent-event-mgr)
       (on-exit (shutdown-http)
         (parameterize ([compile-profile #f])
           (let () e1 e2 ...))))]))

(define (get-http-listener)
  (match-let*
   ([,sup (whereis 'http-sup)]
    [(`(<child> [pid ,http-sup] [type supervisor]))
     (supervisor:get-children sup)]
    [,children (supervisor:get-children http-sup)]
    [`(<child> [pid ,listener])
     (find (lambda (x) (eq? (<child> name x) 'http-listener)) children)])
   listener))

(define http-port (make-process-parameter #f))

(define (get-http-port)
  (cond
   [(http-port) => values]
   [else
    (let ([p (http:get-port-number (get-http-listener))])
      (http-port p)
      p)]))

(define (get-cache)
  (receive (after 1000 (throw 'timeout-waiting-for-cache-to-start))
    [`(<child-start> ,pid ,name)
     (guard (string=? (format "~a" name) "http-cache"))
     pid]))

(define (simple-tcp x)
  (let-values ([(ip op) (connect-tcp "127.0.0.1" (get-http-port))])
    (put-bytevector op x)
    (flush-output-port op)
    (let ([r (get-bytevector-all ip)])
      (close-port op)
      r)))

(define (simple-get x)
  (simple-tcp (string->utf8 (format "GET ~a HTTP/1.1\r\nConnection: close\r\n\r\n" x))))

(define (write-file fn exprs)
  (let ([op (open-file-to-replace fn)])
    (on-exit (close-port op)
      (for-each (lambda (x) (write x op) (newline op)) exprs))))

(define-tuple <result> reason details output)

(define (flush-errors)
  (receive (after 10 'ok)
    [`(<child-end>) (flush-errors)]))

(define (test-error&output input)
  (flush-errors)
  (let* ([output (simple-tcp (string->utf8 input))]
         [output (if (eof-object? output)
                     output
                     (utf8->string output))])
    (let lp ([best #f])
      (receive (after 100 (<result> make [reason best] [details best] [output output]))
        [`(<child-end> [reason normal])
         (lp (or best 'normal))]
        [`(<child-end> ,reason ,details)
         (<result> make [reason reason] [details details] [output output])]))))

(define (test input)
  (match-let* ([`(<result> ,reason ,details ,output)
                (test-error&output input)])
    (cond
     [(eq? reason 'normal) output]
     [(eq? reason 'exception) (throw details)]
     [else (throw reason)])))

(define-tuple <response> status header content)

(define (parse-response s f)
  (let ([ip (open-bytevector-input-port (string->utf8 s))])
    (let* ([status (http:read-status ip 4096)]
           [header (http:read-header ip 1048576)]
           [len (http:get-content-length header)]
           [content (and len (f (get-bytevector-n ip len)))])
      (<response> make
        [status status]
        [header header]
        [content content]))))

(define (bytevector->sexpr b)
  (read (open-utf8-bytevector b)))

(define (test-response input)
  (parse-response (test input) bytevector->sexpr))

(define internal-server-error
  "HTTP/1.1 500 \r\nContent-Length: 167\r\nCache-Control: no-cache\r\n\r\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>Internal server error</title></head><body><h1>The server was unable to complete the request.</h1></body></html>")

(http-mat handlers ()
  ;; The request-timeout option should be relatively short (500ms) for
  ;; these cases.
  (start-default-file-server)
  (capture-events)
  (when (file-directory? (tmp-dir))
    (for-each (lambda (x)
                (when (ends-with? x ".tmp")
                  (delete-file (path-combine (tmp-dir) x))))
      (directory-list (tmp-dir))))
  (match-let*
   ([#(EXIT http-request-timeout) (catch (test ""))]
    [#(EXIT #(http-unhandled-input ,bv))
     (guard (string=? (utf8->string bv) "unhandled"))
     (catch (test "unhandled\r\n"))]
    [#(EXIT #(http-unhandled-input ,bv))
     (guard (string=? (utf8->string bv) "GET / HTTP/1.0"))
     (catch (test "GET / HTTP/1.0\r\n\r\n"))]
    [#(EXIT http-input-limit-exceeded)
     (catch (test (make-string 8193 #\a)))]
    [#(EXIT http-input-limit-exceeded)
     (catch (test (string-append
                   "GET /static/ok.txt HTTP/1.1\r\n"
                   (make-string 1048577 #\a) "\r\n"
                   "\r\n")))]
    [#(EXIT http-content-limit-exceeded)
     (catch (test (string-append
                   "POST /mat?cmd=echo+form HTTP/1.1\r\n"
                   "Content-Length: 16777217\r\n"
                   "Content-Type: application/x-www-form-urlencoded\r\n"
                   "\r\n")))]
    [#(EXIT #(timeout ,_))
     (catch (test (string-append
                   "POST /mat?cmd=echo+form HTTP/1.1\r\n"
                   "Content-Length: 10\r\n"
                   "Content-Type: application/x-www-form-urlencoded\r\n"
                   "\r\n"
                   "0123")))]
    [#(EXIT #(http-invalid-path "/.."))
     (catch (test "GET /.. HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(http-invalid-path "foo"))
     (catch (test "GET foo HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(bad-arg http:respond "foo"))
     (catch (test "GET /mat?cmd=bad+arg&bad=foo HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(http-invalid-content-length "none"))
     (catch (test (string-append
                   "POST /static/ok.txt HTTP/1.1\r\n"
                   "Content-Length: none\r\n"
                   "\r\n")))]

    ;; handler does not send a response, but claims it has been handled
    [`(<result> [reason http-output-violation] [output #!eof])
     (test-error&output "GET /mat?cmd=no+response HTTP/1.1\r\n\r\n")]

    ;; handlers attempting to send multiple responses
    [`(<result> [reason http-output-violation] [output "HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: max-age=3600\r\n\r\nok\n"])
     (test-error&output "GET /mat?cmd=double+response&first=file&second=file HTTP/1.1\r\n\r\n")]
    [`(<result> [reason http-output-violation] [output "HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: max-age=3600\r\n\r\nok\n"])
     (test-error&output "GET /mat?cmd=double+response&first=file&second=direct HTTP/1.1\r\n\r\n")]
    [`(<result> [reason http-output-violation] [output "HTTP/1.1 200 \r\nContent-Length: 6\r\nCache-Control: no-cache\r\n\r\ndirect"])
     (test-error&output "GET /mat?cmd=double+response&first=direct&second=file HTTP/1.1\r\n\r\n")]
    [`(<result> [reason http-output-violation] [output "HTTP/1.1 200 \r\nContent-Length: 6\r\nCache-Control: no-cache\r\n\r\ndirect"])
     (test-error&output "GET /mat?cmd=double+response&first=direct&second=direct HTTP/1.1\r\n\r\n")]

    ;; internal server errors
    [`(<result> [reason internal-error] [output ,@internal-server-error])
     (test-error&output "GET /mat?cmd=internal-error HTTP/1.1\r\n\r\n")]
    [`(<result> [reason internal-error] [output "partial-output"])
     (test-error&output "GET /mat?cmd=internal-error&partial=partial-output HTTP/1.1\r\n\r\n")]

    ;; static content
    [#(EXIT #(http-invalid-method PUT "/static/ok.txt"))
     (catch (test "PUT /static/ok.txt HTTP/1.1\r\n\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: max-age=3600\r\n\r\nok\n"
     (test "GET /static/ok.txt HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: max-age=3600\r\n\r\nok\n"
     (test "GET /static/ok.txt HTTP/1.1\nConnection: close\n\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 36\r\nCache-Control: max-age=3600\r\nContent-Type: text/html\r\n\r\n<html><body><p>ok</p></body></html>\n"
     (test "GET /static/ok.html HTTP/1.1\nConnection: close\n\n")]

    ;; header
    [#(EXIT http-invalid-header)
     (catch (test (string-append
                   "GET /mat?cmd=echo+header HTTP/1.1\r\n"
                   "Host localhost\r\n\r\n")))]

    [`(<response> [content ((connection . "close") (host . "localhost"))])
     (test-response
      (string-append
       "GET /mat?cmd=echo+header HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: localhost\r\n\r\n"))]
    [`(<response> [content ((connection . "close") (host . "localhost"))])
     (test-response
      (string-append
       "GET /mat?cmd=echo+header HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: \t localhost\r\n\r\n"))]
    [`(<response> [content ((connection . "close") (host . "localhost")
                            (no-value . ""))])
     (test-response
      (string-append
       "GET /mat?cmd=echo+header HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: localhost\r\n"
       "No-Value:\r\n\r\n"))]
    [`(<response> [content ((connection . "close") (host . "localhost")
                            (no-value . ""))])
     (test-response
      (string-append
       "GET /mat?cmd=echo+header HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: localhost\r\n"
       "No-Value: \r\n\r\n"))]
    [`(<response> [content ((connection . "close") (host . "localhost")
                            (repeat . "a,b,c"))])
     ;; Repeated headers can be combined into a comma separated list
     ;; without leading whitespace.
     (test-response
      (string-append
       "GET /mat?cmd=echo+header HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: localhost\r\n"
       "Repeat: a\r\n"
       "Repeat:   b\r\n"
       "Repeat: \tc\r\n\r\n"))]
    [#(EXIT http-invalid-header)
     ;; An error is expected. RFC7230 indicates continued header lines
     ;; is deprecated.
     (catch
      (test
       (string-append
        "GET /mat?cmd=echo+header HTTP/1.1\r\n"
        "Connection: close\r\n"
        "Host: localhost\r\n"
        "Continued: a\r\n"
        "    b\r\n"
        "\t  c\r\n\r\n")))]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: no-cache\r\nContent-Type: text/plain\r\n\r\nok\n"
     (test
      (string-append
       "GET /mat?cmd=file-headers HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: localhost\r\n\r\n"))]

    ;; status
    [#(EXIT #(bad-arg http:write-status 99))
     (catch (test "GET /mat?cmd=use+status&status=99 HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(bad-arg http:write-status 600))
     (catch (test "GET /mat?cmd=use+status&status=600 HTTP/1.1\r\n\r\n"))]
    ["HTTP/1.1 101 \r\nCache-Control: no-cache\r\n\r\n"
     (test "GET /mat?cmd=use+status&status=101 HTTP/1.1\r\nConnection: close\r\n\r\n")]

    ;; utf8
    ["HTTP/1.1 200 \r\nContent-Length: 6\r\nCache-Control: no-cache\r\n\r\n面条"
     (test "GET /mat?cmd=echo&echo=面条 HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 6\r\nCache-Control: no-cache\r\n\r\n面条"
     (test "GET /mat?cmd=echo&echo=%E9%9D%A2%E6%9D%A1 HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 6\r\nCache-Control: no-cache\r\n\r\n面条"
     (test "GET /mat?cmd=echo&echo=%e9%9d%a2%e6%9d%a1 HTTP/1.1\r\nConnection: close\r\n\r\n")]

    ;; decoding
    ["HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n%"
     (test "GET /mat?cmd=echo&echo=% HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\n%2"
     (test "GET /mat?cmd=echo&echo=%2 HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n "
     (test "GET /mat?cmd=echo&echo=%20 HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: no-cache\r\n\r\n%q2"
     (test "GET /mat?cmd=echo&echo=%q2 HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: no-cache\r\n\r\n%2q"
     (test "GET /mat?cmd=echo&echo=%2q HTTP/1.1\r\nConnection: close\r\n\r\n")]

    ;; content params
    [`(<result> [reason #(timeout ,_)] [output #!eof])
     (test-error&output
      (string-append
       "GET /mat?cmd=echo+unhandled HTTP/1.1\r\n"
       "Content-Length: 256\r\n"
       "\r\n"
       "ok"))]
    [`(<result> [reason http-request-timeout] [output "HTTP/1.1 200 \r\nContent-Length: 7\r\nCache-Control: no-cache\r\n\r\n0123456"])
     (test-error&output
      (string-append
       "GET /mat?cmd=echo+unhandled&count=7 HTTP/1.1\r\n"
       "Content-Length: 16\r\n"
       "\r\n"
       "0123456789ABCDEF"))]
    [`(<result> [reason http-input-violation] [output "HTTP/1.1 200 \r\nContent-Length: 17\r\nCache-Control: no-cache\r\n\r\n0123456789ABCDEFG"])
     (test-error&output
      (string-append
       "GET /mat?cmd=echo+unhandled&count=17 HTTP/1.1\r\n"
       "Content-Length: 16\r\n"
       "\r\n"
       "0123456789ABCDEF"
       "GET /mat?cmd=echo+unhandled&count=7 HTTP/1.1\r\n"
       "Content-Length: 16\r\n"
       "\r\n"
       "0123456789ABCDEF"))]
    [`(<result> [reason http-input-violation] [output ,@internal-server-error])
     (test-error&output
      (string-append
       "GET /mat?cmd=bad+read&count=17 HTTP/1.1\r\n"
       "Content-Length: 16\r\n"
       "\r\n"
       "0123456789ABCDEF"
       "GET /mat?cmd=echo&echo=ok HTTP/1.1\r\n"
       "Content-Length: 16\r\n"
       "\r\n"
       "0123456789ABCDEF"))]
    [`(<result> [reason http-request-timeout] [output "HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"])
     (test-error&output
      (string-append
       "GET /mat?cmd=echo&echo=ok HTTP/1.1\r\n"
       "Content-Length: 16\r\n"
       "\r\n"
       "0123456789abcdef"))]
    ["HTTP/1.1 200 \r\nContent-Length: 13\r\nCache-Control: no-cache\r\n\r\n{\"echo\":\"ok\"}"
     (test (string-append
            "POST /mat?cmd=echo+form HTTP/1.1\r\n"
            "Connection: close\r\n"
            "Content-Length: 7\r\n"
            "Content-Type: application/x-www-form-urlencoded\r\n"
            "\r\n"
            "echo=ok"))]
    [`(<result> [reason http-input-violation] [output ,@internal-server-error])
     (test-error&output
      (string-append
       "POST /mat?cmd=bad+read&count=1 HTTP/1.1\r\n"
       "Connection: keep-alive\r\n"
       "Content-Length: 0\r\n"
       "Content-Type: application/x-www-form-urlencoded\r\n"
       "\r\n"
       "don't read me"))]
    [`(<result> [reason http-input-violation] [output "HTTP/1.1 200 \r\nContent-Length: 10\r\nCache-Control: no-cache\r\n\r\ndon't read"])
     (test-error&output
      (string-append
       "POST /mat?cmd=echo+unhandled&count=10 HTTP/1.1\r\n"
       "Connection: keep-alive\r\n"
       "Content-Length: 0\r\n"
       "Content-Type: application/x-www-form-urlencoded\r\n"
       "\r\n"
       "don't read me"))]
    ["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"
     (test (string-append
            "GET /mat?cmd=echo+unhandled HTTP/1.1\r\n"
            "Connection: close\r\n"
            "Content-Length: 2\r\n"
            "\r\n"
            "ok"))]

    ;; multipart/form-data
    [,spaces (make-string 4200000 #\space)]
    [,response (string-append "HTTP/1.1 200 \r\nContent-Length: 4200000\r\nCache-Control: no-cache\r\n\r\n" spaces)]
    [,@response
     (test (string-append
            "GET /mat?cmd=echo+file HTTP/1.1\r\n"
            "Connection: close\r\n"
            "Content-Length: 4200288\r\n"
            "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
            "\r\n"
            "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
            "Content-Disposition: form-data; name=\"sample\"\r\n"
            "\r\n"
            "1\r\n"
            "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
            "Content-Disposition: form-data; name=\"file\"; filename=\"spaces\"\r\n"
            "Content-Type: application/octet-stream\r\n"
            "\r\n"
            spaces
            "\r\n"
            "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy--\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 0\r\nCache-Control: no-cache\r\n\r\n"
     (test (string-append
            "GET /mat?cmd=echo+file HTTP/1.1\r\n"
            "Connection: close\r\n"
            "Content-Length: 287\r\n"
            "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
            "\r\n"
            "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
            "Content-Disposition: form-data; name=\"sample\"\r\n"
            "\r\n"
            "1\r\n"
            "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
            "Content-Disposition: form-data; name=\"file\"; filename=\"empty\"\r\n"
            "Content-Type: application/octet-stream\r\n"
            "\r\n"
            "\r\n"
            "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy--\r\n"))]
    [`(<result> [reason #(timeout ,_)] [output #!eof])
     (test-error&output
      (string-append
       "GET /mat?cmd=echo+file HTTP/1.1\r\n"
       "Content-Length: 44\r\n"
       "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
       "\r\n"
       "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy--"))]
    [#(EXIT http-invalid-multipart-boundary)
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 4\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "ok\r\n")))]
    [#(EXIT http-invalid-multipart-boundary)
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 42\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNyok")))]
    [#(EXIT #(http-invalid-content-disposition "none"))
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 425\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: form-data; name=\"file\"; filename=\"ok.txt\"\r\n"
             "Content-Type: application/octet-stream\r\n"
             "\r\n"
             "ok\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: form-data; name=\"file2\"; filename=\"ok2.txt\"\r\n"
             "Content-Type: application/octet-stream\r\n"
             "\r\n"
             "ok2\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: none\r\n"
             "\r\n"
             "1\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy--\r\n")))]
    [#(EXIT #(http-invalid-content-disposition "form-data; name=sample"))
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 425\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: form-data; name=\"file\"; filename=\"ok.txt\"\r\n"
             "Content-Type: application/octet-stream\r\n"
             "\r\n"
             "ok\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: form-data; name=\"file2\"; filename=\"ok2.txt\"\r\n"
             "Content-Type: application/octet-stream\r\n"
             "\r\n"
             "ok2\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: form-data; name=sample\r\n"
             "\r\n"
             "1\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy--\r\n")))]
    [#(EXIT http-content-limit-exceeded)
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 4194593\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: form-data; name=\"file\"; filename=\"ok.txt\"\r\n"
             "Content-Type: application/octet-stream\r\n"
             "\r\n"
             "ok\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "Content-Disposition: form-data; name=\"sample\"\r\n"
             "\r\n"
             (make-string 4194304 #\space)
             "\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy--\r\n")))]
    [() (filter (lambda (x) (ends-with? x ".tmp")) (directory-list (tmp-dir)))]

    ;; paths
    [#(EXIT #(http-file-not-found "/no-file"))
     (catch (test "GET /no-file HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(http-file-not-found "/no-directory/no-file"))
     (catch (test "GET /no-directory/no-file HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(http-file-not-found "/static/"))
     (catch (test "GET /static/ HTTP/1.1\r\n\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"
     (test "GET /with-index/ HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"
     (test "GET /with-index/index.ss HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"
     (test "GET /with-index/index HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 302 \r\nContent-Length: 0\r\nCache-Control: no-cache\r\nLocation: /with-index/\r\n\r\n"
     (test "GET /with-index HTTP/1.1\r\nConnection: close\r\n\r\n")]

    ;; include
    ["HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n1"
     (test "GET /include/mat?cmd=echo&echo=1 HTTP/1.1\r\nConnection: close\r\n\r\n")]
    [#(EXIT ,x)
     (guard (and (syntax-violation? x)
                 (string=? (condition-message x)
                   "invalid path 12345 in")))
     (catch (test "GET /include/syntax-error HTTP/1.1\r\n\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n7"
     (test "GET /include/rooted?cmd=echo&echo=7 HTTP/1.1\r\nConnection: close\r\n\r\n")]
    )
   'ok))

(http-mat helpers ()
  (define header
    (json:make-object
     [content-length "bar"]
     [content-type "foo"]))
  (define params
    (json:make-object
     [query "lookup"]
     [val "1"]))
  (match-let*
   ([#(EXIT #(bad-arg request-limit a))
     (catch (http:options [request-limit 'a]))]
    [#(EXIT #(bad-arg request-limit 0))
     (catch (http:options [request-limit 0]))]
    [#(EXIT #(bad-arg request-timeout a))
     (catch (http:options [request-timeout 'a]))]
    [#(EXIT #(bad-arg request-timeout 0))
     (catch (http:options [request-timeout 0]))]
    [#(EXIT #(bad-arg response-timeout a))
     (catch (http:options [response-timeout 'a]))]
    [#(EXIT #(bad-arg response-timeout 0))
     (catch (http:options [response-timeout 0]))]
    [#(EXIT #(bad-arg header-limit a))
     (catch (http:options [header-limit 'a]))]
    [#(EXIT #(bad-arg header-limit 0))
     (catch (http:options [header-limit 0]))]
    [#(EXIT #(bad-arg media-type-handler a))
     (catch (http:options [media-type-handler 'a]))]
    [#(EXIT #(bad-arg media-type-handler 0))
     (catch (http:options [media-type-handler 0]))]
    [#(EXIT #(bad-arg file-transform a))
     (catch (http:options [file-transform 'a]))]
    [#(EXIT #(bad-arg file-transform 0))
     (catch (http:options [file-transform 0]))]
    [#(EXIT #(bad-arg file-search-extensions a))
     (catch (http:options [file-search-extensions 'a]))]
    [#(EXIT #(bad-arg file-search-extensions 0))
     (catch (http:options [file-search-extensions 0]))]
    [#f (http:find-header "None" header)]
    [#f (http:find-header 'None header)]
    [#(EXIT #(bad-arg http:find-header 0))
     (catch (http:find-header 0 header))]
    ["foo" (http:find-header "Content-Type" header)]
    ["foo" (http:find-header "content-type" header)]
    [#(EXIT #(http-invalid-header "None"))
     (catch (http:get-header "None" header))]
    [#(EXIT #(http-invalid-header None))
     (catch (http:get-header 'None header))]
    [#(EXIT #(bad-arg http:get-header 0))
     (catch (http:get-header 0 header))]
    ["foo" (http:get-header "Content-Type" header)]
    ["foo" (http:get-header "content-type" header)]
    [#f (http:find-param "None" params)]
    [#f (http:find-param 'None params)]
    [#(EXIT #(bad-arg http:find-param 0))
     (catch (http:find-param 0 params))]
    [#f (http:find-param "VAL" params)]
    ["1" (http:find-param "val" params)]
    [#(EXIT #(http-invalid-param "None"))
     (catch (http:get-param "None" params))]
    [#(EXIT #(http-invalid-param None))
     (catch (http:get-param 'None params))]
    [#(EXIT #(bad-arg http:get-param 0))
     (catch (http:get-param 0 params))]
    ["1" (http:get-param "val" params)]
    ["%E9%9D%A2%E6%9D%A1" (http:percent-encode "面条")]
    ["%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%20%21%22%23%24%25%26%27%28%29%2A%2B%2C-.%2F0123456789%3A%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~%7F%C2%80%C2%81%C2%82%C2%83%C2%84%C2%85%C2%86%C2%87%C2%88%C2%89%C2%8A%C2%8B%C2%8C%C2%8D%C2%8E%C2%8F%C2%90%C2%91%C2%92%C2%93%C2%94%C2%95%C2%96%C2%97%C2%98%C2%99%C2%9A%C2%9B%C2%9C%C2%9D%C2%9E%C2%9F%C2%A0%C2%A1%C2%A2%C2%A3%C2%A4%C2%A5%C2%A6%C2%A7%C2%A8%C2%A9%C2%AA%C2%AB%C2%AC%C2%AD%C2%AE%C2%AF%C2%B0%C2%B1%C2%B2%C2%B3%C2%B4%C2%B5%C2%B6%C2%B7%C2%B8%C2%B9%C2%BA%C2%BB%C2%BC%C2%BD%C2%BE%C2%BF%C3%80%C3%81%C3%82%C3%83%C3%84%C3%85%C3%86%C3%87%C3%88%C3%89%C3%8A%C3%8B%C3%8C%C3%8D%C3%8E%C3%8F%C3%90%C3%91%C3%92%C3%93%C3%94%C3%95%C3%96%C3%97%C3%98%C3%99%C3%9A%C3%9B%C3%9C%C3%9D%C3%9E%C3%9F%C3%A0%C3%A1%C3%A2%C3%A3%C3%A4%C3%A5%C3%A6%C3%A7%C3%A8%C3%A9%C3%AA%C3%AB%C3%AC%C3%AD%C3%AE%C3%AF%C3%B0%C3%B1%C3%B2%C3%B3%C3%B4%C3%B5%C3%B6%C3%B7%C3%B8%C3%B9%C3%BA%C3%BB%C3%BC%C3%BD%C3%BE%C3%BF"
     (http:percent-encode
      (let ([s (make-string 256)])
        (do ([i 0 (+ i 1)]) ((= i 256))
          (string-set! s i (integer->char i)))
        s))]
    [,bad-header '(("good-key" . "good-val")
                   (good-key2 . "good-val2")
                   (1234 . "bad-key"))]
    [#(EXIT #(bad-arg http:write-header ,@bad-header))
     (catch (http:write-header #f bad-header))])
   'ok))

(http-mat read-status ()
  (define (test input)
    (http:read-status (open-bytevector-input-port (string->utf8 input)) 4096))
  (match-let*
   ([#(EXIT http-input-limit-exceeded) (catch (test (make-string 4097 #\a)))]
    [#f (test "")]
    [#f (test "200 OK Malformed input")]
    [#f (test "HTTP/1.1 OK 200")]
    [200 (test "HTTP/1.1 200 OK")]
    [404 (test "HTTP/1.1 404 Not Found")]
    [200 (test "HTTP/1.1 200")]
    [200 (test "HTTP/1.1 200 This text does not matter")])
   'ok))

(http-mat http ()
  (start-default-file-server)
  (capture-events)

  (let-values ([(ip op) (connect-tcp "127.0.0.1" (get-http-port))])
    (put-bytevector op (string->utf8 "Malformed Request\r\n\r\n"))
    (flush-output-port op)
    (receive (after 10 (throw 'no-child-log))
      [`(<child-end> [reason #(http-unhandled-input ,x)])
       (assert (string=? (utf8->string x) "Malformed Request"))])
    (close-port op))

  (let-values ([(ip op) (connect-tcp "127.0.0.1" (get-http-port))])
    (put-bytevector op (string->utf8 "GET /mat?cmd=echo&echo=1 HTTP/1.1\r\n"))
    (http:write-header op '(("connection" . "KeEp-AlIvE")))
    (put-bytevector op (string->utf8 "GET /mat?cmd=echo&echo=2 HTTP/1.1\r\n"))
    (http:write-header op '(("Connection" . "close")))
    (put-bytevector op (string->utf8 "GET /mat?cmd=echo&echo=3 HTTP/1.1\r\n"))
    (flush-output-port op)
    (let ([me self])
      (spawn&link
       (lambda ()
         (send me `#(read ,(utf8->string (get-bytevector-all ip)))))))
    (receive (after 100 (throw 'timeout))
      [#(read ,x)
       (unless (string=? x
                 (string-append
                  "HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n1"
                  "HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n2"))
         (throw `#(unexpected ,x)))])
    (close-port op))

  ;; fake a message indicating that accept failed
  (process-trap-exit #t)
  (let ([http (get-http-listener)]
        [fail-msg `#(accept-tcp-failed "listener" "who" "errno")])
    (link http)
    (send http fail-msg)
    (receive (after 100 (throw 'timeout))
      [`(EXIT ,@http ,@fail-msg) 'ok])))

(http-mat http-respond ()
  ;; RFC7230 section 3.3.2 disallows returning Content-Length when the
  ;; status code is 1xx, 204, or 304. The .NET Framework actually
  ;; checks this and closes the connection on a delay without
  ;; reason. This is particularly noticable on WebSocket connections.
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (let ([status (string->number (http:get-param "status" params))])
         (match (<request> path request)
           ["/case1"
            (http:respond conn status '())]
           ["/case2"
            (http:respond conn status '() '#vu8())]
           ["/case3"
            (http:respond conn status '() (string->utf8 "ok"))])))))
  (capture-events)
  (let ()
    (define (build-case case status)
      (format "GET /case~a?status=~a HTTP/1.1\r\nConnection: close\r\n\r\n"
        case status))
    (define build-result
      (case-lambda
       [(status)
        (format "HTTP/1.1 ~a \r\nCache-Control: no-cache\r\n\r\n" status)]
       [(status str)
        (format "HTTP/1.1 ~a \r\nContent-Length: ~a\r\nCache-Control: no-cache\r\n\r\n~a"
          status (string-length str) str)]))

    (assert (equal? (test (build-case 1 200)) (build-result 200)))
    (assert (equal? (test (build-case 2 200)) (build-result 200 "")))
    (assert (equal? (test (build-case 3 200)) (build-result 200 "ok")))
    (for-each
     (lambda (status)
       (assert (equal? (test (build-case 1 status)) (build-result status)))
       (assert (equal? (test (build-case 2 status)) (build-result status)))
       (match (test-error&output (build-case 3 status))
         [`(<result> [reason #(http-no-response-body-allowed ,@status)]
             [output ,@internal-server-error])
          'ok]))
     '(100 101 199 204 304))))

(http-mat mime-types ()
  (start-default-file-server)
  (capture-events)
  (let* ([mime-types (path-combine (web-dir) "mime-types")]
         [mime-types.bak (string-append mime-types ".bak")])
    (rename-path mime-types mime-types.bak)
    (on-exit (rename-path mime-types.bak mime-types)
      ;; no mime-types file; http-cache crashes; next request will restart
      (utf8->string (simple-get "/static/ok.html"))
      (let ([pid (get-cache)])
        (receive (after 1000 (throw 'timeout-waiting-for-cache-to-fail))
          [`(<gen-server-terminating> [pid ,@pid]
              [reason #(io-error ,_ uv_fs_open ,_)])
           'ok]))

      ;; invalid mime-types file; http-cache crashes; next request will restart
      (write-file mime-types
        '(("css" . "text/css")
          (invalid mime type expression)
          ("html" . "text/html")))
      (utf8->string (simple-get "/static/ok.html"))
      (let ([pid (get-cache)])
        (receive (after 1000 (throw 'timeout-waiting-for-cache-to-fail))
          [`(<gen-server-terminating> [pid ,@pid]
              [reason #(http-invalid-media-type (invalid mime type expression))])
           'ok]))))

  ;; valid mime-types file
  (match-let*
   (["HTTP/1.1 200 \r\nContent-Length: 36\r\nCache-Control: max-age=3600\r\nContent-Type: text/html\r\n\r\n<html><body><p>ok</p></body></html>\n"
     (utf8->string (simple-get "/static/ok.html"))])
   'ok))

(http-mat cache-including ()
  ;; load a file which includes other files, change an included file
  (define file1 "file1.ss")
  (define file2 "file2.ss")
  (define inc1 "inc1")
  (define inc2 "inc2")
  (define inc3 "inc3")

  (define (full fn)
    (path-combine (web-dir) fn))

  (define (check s)
    (sleep-ms 20)
    (let ([expected (format "HTTP/1.1 200 \r\nContent-Length: ~a\r\nCache-Control: no-cache\r\n\r\n~a"
                      (string-length s) s)]
          [actual (utf8->string (simple-get (string-append "/" file1)))])
      (or (string=? expected actual)
          (throw `#(unexpected ,expected ,actual)))))

  (start-default-file-server)
  ;; cache unrelated file which uses include (coverage)
  (simple-get "/include/mat?cmd=echo&echo=1")

  (write-file (full file1)
    `((http:include ,inc1)
      (reply)))
  (write-file (full inc1)
    `((http:include ,inc2)
      (http:include ,inc3)))
  (write-file (full inc3)
    `((define (noop) (void))))
  (write-file (full inc2)
    '((define (reply)
        (http:respond conn 200 '() (string->utf8 "ok")))))

  (check "ok")
  (check "ok")                          ; same request hits the cache

  (write-file (full inc2)
    '((define (reply)
        (http:respond conn 200 '() (string->utf8 "yes")))))

  (check "yes")

  ;; cache a file that includes inc2 (coverage)
  (write-file (full file2)
    `((http:include ,inc2)
      (reply)))
  (simple-get (string-append "/" file2))

  ;; modify the main cached file
  (write-file (full file1)
    `((http:include ,inc2)
      (reply)))

  (check "yes")

  (delete-file (full file1))
  (delete-file (full file2))
  (delete-file (full inc1))
  (delete-file (full inc2))
  (delete-file (full inc3))
  'ok)

(http-mat path-watcher-fail ()
  (start-default-file-server)
  (capture-events)
  (simple-get "/static/ok.html")        ; To start the cache
  (let ([path (web-dir)]
        [pid (get-cache)])
    (send pid `#(path-watcher-failed ,path -1234))
    (receive (after 1000 (throw 'timeout-waiting-for-cache-to-fail))
      [`(<gen-server-terminating> [pid ,@pid] ,reason)
       (match reason
         [#(bad-match #(path-watcher-failed ,@path -1234) ,_)
          'ok])])))

(http-mat composition ()
  (define ok
    (http:url-handler
     (match (<request> path request)
       ["/" (http:respond conn 200 '() (string->utf8 "ok"))]
       [,_ #f])))
  (define side-effector
    (http:url-handler
     (http:call-with-ports conn
       (lambda (ip op)
         (put-bytevector op (string->utf8 "fries"))))
     #f))

  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:compose-url-handlers
       (list ok (http:compose-url-handlers '()) side-effector))
      (http:options
       [request-timeout 1000])))
  (capture-events)
  (match-let*
   (["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"
     (test "GET / HTTP/1.1\r\nConnection: close\r\n\r\n")]
    [`(<result> [reason http-side-effecting-handler]
        [output "fries"])
     (test-error&output "GET /side-effects HTTP/1.1\r\n\r\n")])
   'ok))

(http-mat virtual-routing ()
  ;; As a custom url handler, process requests that start with
  ;; "/faker" like a file request, but routed to the standard
  ;; directory of files.
  (write-web-mats)
  (let ([file-handler (http:make-default-file-url-handler (web-dir))]
        [media-type-handler (http:make-default-media-type-handler (web-dir))]
        [prefix "/faker"])
    (supervisor:start&link 'http-sup 'one-for-all 0 1
      (http:configure-server #f 0
        (http:url-handler
         (<request> open request [path])
         (and (starts-with? path prefix)
              (let ([request (<request> copy request
                               [path (substring path (string-length prefix)
                                       (string-length path))])])
                (http:call-url-handler file-handler)))))))
  (capture-events)
  (match-let*
   ([`(<result> [reason #(http-file-not-found "/static/ok.txt")])
     (test-error&output
      "GET /static/ok.txt HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: max-age=3600\r\n\r\nok\n"
     (test "GET /faker/static/ok.txt HTTP/1.1\r\nConnection: close\r\n\r\n")])
   'ok))

(http-mat switch-protocol ()
  (define me self)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (send me `#(connection ,conn))
       (http:switch-protocol
        (lambda (ip op)
          (let ([ip (binary->utf8 ip)]
                [op (binary->utf8 op)])
            (let lp ()
              (let ([x (read ip)])
                (unless (eof-object? x)
                  (write x op)
                  (newline op)
                  (flush-output-port op)
                  (lp))))))))))
  (capture-events)
  (let-values ([(ip op) (connect-tcp "127.0.0.1" (get-http-port))])
    (put-bytevector op (string->utf8 "GET / HTTP/1.1\r\n\r\n"))
    (flush-output-port op)
    (let* ([ip (binary->utf8 ip)]
           [op (binary->utf8 op)]
           [reader
            (spawn&link
             (lambda ()
               (let lp ()
                 (let ([x (read ip)])
                   (unless (eof-object? x)
                     (send me `#(read ,x))
                     (lp))))))])
      (define (check-loopback ls)
        (for-each
         (lambda (x)
           (write x op)
           (newline op))
         ls)
        (flush-output-port op)
        (let lp ([ls ls])
          (match ls
            [() 'ok]
            [(,n . ,rest)
             (receive (after 100 (throw `#(failed-to-read-data ,n)))
               [#(read ,@n) (lp rest)])])))

      (check-loopback '(9 8 7 6 5))

      ;; Connection should exit normally (and be collected)
      (let ([g (make-guardian)])
        (receive
         [#(connection ,conn)
          (g conn)
          (monitor conn)
          (receive [`(DOWN ,_ ,@conn normal) 'ok])])
        (discard-events)
        (gc)
        (assert (process? (g))))

      ;; I/O Continues after the connection exited
      (check-loopback '(4 3 2 1 0)))))

(http-mat switch-protocol-fail ()
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (http:switch-protocol 12))))
  (capture-events)
  (match-let*
   ([`(<result> [reason #(bad-return-value #(switch-protocol 12))]
        [output #!eof])
     (test-error&output "GET / HTTP/1.1\r\n\r\n")])
   'ok))

(http-mat switch-protocol-via-files ()
  (start-default-file-server)
  (capture-events)
  (match-let*
   ([`(<result> [reason #(bad-return-value #(switch-protocol 12))]
        [output #!eof])
     (test-error&output "GET /mat?cmd=switch-protocol&case=1 HTTP/1.1\r\n\r\n")]
    ["switched"
     (test "GET /mat?cmd=switch-protocol&case=2 HTTP/1.1\r\n\r\n")])
   'ok))

(http-mat chaperone ()
  ;; The listener closes output ports for the dispatchers. If the
  ;; dispatcher had used on-exit, a well placed spawn&link can prevent
  ;; its execution.
  (define me self)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (http:call-with-ports conn
         (lambda (ip op)
           (send me `#(op ,op))))
       (spawn&link (lambda () (throw 'cant-catch-me)))
       (receive (after 10 'ok))
       (http:respond conn 200 '() (string->utf8 "ok")))))
  (capture-events)
  (match-let*
   ([`(<result> [reason cant-catch-me] [output #!eof])
     (test-error&output "GET / HTTP/1.1\r\n\r\n")])
   (let ([g (make-guardian)])
     (receive
      [#(op ,op)
       (g op)
       (assert (port-closed? op))])
     (discard-events)
     (gc)
     (assert (output-port? (g))))))

(http-mat response-timeout ()
  (define bytes (make-bytevector (- (expt 2 29) 1)))
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (http:respond conn 200 '() bytes))
      (http:options
       [response-timeout 15])))
  (capture-events)
  (let-values ([(ip op) (connect-tcp "127.0.0.1" (get-http-port))])
    (put-bytevector op (string->utf8 "GET / HTTP/1.1\r\n\r\n"))
    (flush-output-port op)
    ;; grab a few bytes, but otherwise don't read. This will cause the
    ;; server to fill up the network queue and eventually wait. This
    ;; causes the response-timeout to trigger.
    (do ([i 0 (+ i 1)]) ((= i 10)) (get-u8 ip))
    (receive (after 2000 (throw 'failed-to-timeout))
      [`(<child-end> [reason #(timeout ,_)]) 'ok])))

(http-mat content-length ()
  (define (test s)
    (http:get-content-length
     (if s
         (json:make-object [content-length s])
         (json:make-object))))
  (match-let*
   ([#(EXIT #(http-invalid-content-length "")) (catch (test ""))]
    [#(EXIT #(http-invalid-content-length "abc")) (catch (test "abc"))]
    [#(EXIT #(http-invalid-content-length "1a")) (catch (test "1a"))]
    [#f (test #f)]
    [0 (test "0")]
    [1 (test "1")]
    [,mpf (- (expt 2 60) 1)]
    [,@mpf (test (number->string mpf))]
    [,big (* mpf 10)]
    [,@big (test (number->string big))]
    [,too-large (* mpf 100)]
    [,too-large-str (number->string too-large)]
    [#(EXIT #(http-invalid-content-length ,@too-large-str))
     (catch (test (number->string too-large)))])
   'ok))

(http-mat websocket ()
  (define me self)

  (define (make-byte-stream prime)
    (let ([next 0])
      (lambda ()
        (set! next (modulo (+ next prime) 256))
        next)))

  (define (build-buffer size stream)
    (let ([bv (make-bytevector size)])
      (do ([i 0 (+ i 1)]) ((= i size))
        (bytevector-u8-set! bv i (stream)))
      bv))

  (define (build-string size stream)
    (let ([s (make-string size)])
      (do ([i 0 (+ i 1)]) ((= i size))
        (string-set! s i (integer->char (stream))))
      s))

  (define (get-msg from)
    (receive (after 1000 (throw 'get-msg-timeout))
      [#(ws:message ,@from ,msg) msg]))

  (define (send/expect p1 p2 data)
    (ws:send p1 data)
    (assert (equal? data (get-msg p2))))

  (define primes '(7 11 13 239 241 251))
  (define buffer-sizes '(1 127 256 1024 2048 4096 8192 65537))

  (define (check-loopback p1 p2)
    (for-each
     (lambda (prime)
       (for-each
        (lambda (size)
          (send/expect p1 p2 (build-buffer size (make-byte-stream prime)))
          (send/expect p1 p2 (build-string size (make-byte-stream prime))))
        buffer-sizes))
     primes))

  (capture-events)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (ws:upgrade conn request me))))

  (let* ([c (ws:connect "127.0.0.1" (get-http-port) "/" self
              (ws:options
               [ping-frequency 10]
               [fragmentation-size 1025]))]
         [_ (receive [#(ws:init ,@c) 'ok])]
         [s (receive [#(ws:init ,s) s])])
    (check-loopback c s)
    (check-loopback s c)
    (ws:close c)
    (receive (after 1000 (throw 'no-client-close))
      [#(ws:closed ,@c 1000 "") 'ok])
    (receive (after 1000 (throw 'no-server-close))
      [#(ws:closed ,@s 1000 "") 'ok])))

(http-mat websocket-connect-errors ()
  (define me self)
  (define (req header)
    (utf8->string
     (call-with-bytevector-output-port
      (lambda (op)
        (put-bytevector op (string->utf8 "GET / HTTP/1.1\r\n"))
        (http:write-header op header)))))

  (capture-events)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (ws:upgrade conn request me))))

  ;; Order matters here.
  (match-let*
   ([,bad400 "HTTP/1.1 400 \r\nCache-Control: no-cache\r\n\r\n"]
    [,bad426 "HTTP/1.1 426 \r\nCache-Control: no-cache\r\nSec-WebSocket-Version: 13\r\n\r\n"]
    [`(<result> [reason #(websocket-invalid-method POST)] [output ,@bad400])
     (test-error&output "POST / HTTP/1.1\r\n\r\n")]
    [,hdr '()]
    [`(<result> [reason #(websocket-missing-header host)] [output ,@bad400])
     (test-error&output (req hdr))]
    [,hdr (cons (cons "Host" (format "127.0.0.1:~a" (get-http-port))) hdr)]
    [`(<result> [reason #(websocket-missing-header upgrade)] [output ,@bad400])
     (test-error&output (req hdr))]
    [,hdr-bad (cons (cons "Upgrade" "not-websocket") hdr)]
    [`(<result> [reason #(websocket-invalid-header upgrade "not-websocket")]
        [output ,@bad400])
     (test-error&output (req hdr-bad))]
    [,hdr (cons (cons "Upgrade" "websocket") hdr)]
    [`(<result> [reason #(websocket-missing-header connection)] [output ,@bad400])
     (test-error&output (req hdr))]
    [,hdr-bad (cons (cons "Connection" "sidestep") hdr)]
    [`(<result> [reason #(websocket-invalid-header connection "sidestep")]
        [output ,@bad400])
     (test-error&output (req hdr-bad))]
    ;; Multiple values in the Connection field are supposed to be
    ;; stripped by proxies. Firefox insists on sending them even if
    ;; a proxy is not involved.
    [,hdr-firefox (cons (cons "Connection" "keep-alive, Upgrade") hdr)]
    [`(<result> [reason #(websocket-missing-header sec-websocket-version)]
        [output ,@bad426])
     (test-error&output (req hdr-firefox))]
    ;; Like the Firefox case, but includes an empty value
    [,hdr-coverage (cons (cons "Connection" "keep-alive,,Upgrade") hdr)]
    [`(<result> [reason #(websocket-missing-header sec-websocket-version)]
        [output ,@bad426])
     (test-error&output (req hdr-coverage))]
    [,hdr (cons (cons "Connection" "Upgrade") hdr)]
    [`(<result> [reason #(websocket-missing-header sec-websocket-version)]
        [output ,@bad426])
     (test-error&output (req hdr))]
    [,hdr-bad (cons (cons "Sec-WebSocket-Version" "12") hdr)]
    [`(<result> [reason #(websocket-invalid-header sec-websocket-version "12")]
        [output ,@bad426])
     (test-error&output (req hdr-bad))]
    [,hdr (cons (cons "Sec-WebSocket-Version" "13") hdr)]
    [`(<result> [reason #(websocket-missing-header sec-websocket-key)]
        [output ,@bad400])
     (test-error&output (req hdr))])
   'ok))

(http-mat websocket-bad-args ()
  (match-let*
   ([#(EXIT #(bad-arg fragmentation-size a))
     (catch (ws:options [fragmentation-size 'a]))]
    [#(EXIT #(bad-arg fragmentation-size 0))
     (catch (ws:options [fragmentation-size 0]))]
    [#(EXIT #(bad-arg maximum-message-size a))
     (catch (ws:options [maximum-message-size 'a]))]
    [#(EXIT #(bad-arg maximum-message-size 0))
     (catch (ws:options [maximum-message-size 0]))]
    [#(EXIT #(bad-arg ping-frequency a))
     (catch (ws:options [ping-frequency 'a]))]
    [#(EXIT #(bad-arg ping-frequency 0))
     (catch (ws:options [ping-frequency 0]))]
    [#(EXIT #(bad-arg pong-timeout a))
     (catch (ws:options [pong-timeout 'a]))]
    [#(EXIT #(bad-arg pong-timeout 0))
     (catch (ws:options [pong-timeout 0]))]
    [#(EXIT #(bad-arg ws:upgrade a))
     (catch (ws:upgrade 'a 'b 'c))]
    [#(EXIT #(bad-arg ws:upgrade a))
     (catch (ws:upgrade 'a 'b 'c 'd))]
    [,conn (spawn&link (lambda () (receive)))]
    [#(EXIT #(bad-arg ws:upgrade b))
     (catch (ws:upgrade conn 'b 'c 'd))]
    [,req (<request> make [method #f] [original-path #f] [path #f]
            [params #f] [host #f] [header #f])]
    [#(EXIT #(bad-arg ws:upgrade c))
     (catch (ws:upgrade conn req 'c 'd))]
    [,pid (spawn&link (lambda () (receive)))]
    [#(EXIT #(bad-arg ws:upgrade d))
     (catch (ws:upgrade conn req pid 'd))]
    [#(EXIT #(bad-arg ws:connect a))
     (catch (ws:connect 'a 'b 'c 'd))]
    [#(EXIT #(bad-arg ws:connect a))
     (catch (ws:connect 'a 'b 'c 'd 'e))]
    [#(EXIT #(bad-arg ws:connect b))
     (catch (ws:connect "127.0.0.1" 'b 'c 'd 'e))]
    [#(EXIT #(bad-arg ws:connect c))
     (catch (ws:connect "127.0.0.1" 80 'c 'd 'e))]
    [#(EXIT #(bad-arg ws:connect d))
     (catch (ws:connect "127.0.0.1" 80 "/" 'd 'e))]
    [,pid (spawn&link (lambda () (receive)))]
    [#(EXIT #(bad-arg ws:connect e))
     (catch (ws:connect "127.0.0.1" 80 "/" pid 'e))]
    [#(EXIT #(bad-arg ws:send a))
     (catch (ws:send 'test 'a))]
    [#(EXIT #(bad-arg ws:send 0))
     (catch (ws:send 'test 0))])
   'ok))

(http-mat websocket-linking ()
  (define me self)
  (capture-events)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (let ([pid (spawn
                   (lambda ()
                     (receive
                      [normal 'ok]
                      [crash (raise 'crashed)])))])
         (send me `#(ws ,self))
         (send me `#(pid ,pid))
         (when (http:find-param 'link params)
           (link pid))
         (ws:upgrade conn request pid)))))
  (let ()
    (define (do-test link? msg check)
      (discard-events)
      (let* ([req (if link? "/?link" "/")]
             [c (ws:connect "127.0.0.1" (get-http-port) req self)]
             [_ (receive [#(ws:init ,@c) 'ok])]
             [s (receive [#(ws ,s) s])]
             [pid (receive [#(pid ,pid) pid])])
        (send pid msg)
        (check c s)))
    (define (check-crash c s)
      (receive (after 100 (throw 'no-client-close))
        [#(ws:closed ,@c 1011 "") 'ok])
      (receive (after 100 (throw 'no-child-log))
        [`(<child-end> [pid ,@s] [reason crashed]) 'ok]))
    (define (check-normal c s)
      (receive (after 100 (throw 'no-client-close))
        [#(ws:closed ,@c 1000 "") 'ok])
      (receive (after 100 (throw 'no-child-log))
        [`(<child-end> [pid ,@s] [reason normal]) 'ok]))
    (do-test #f 'crash check-crash)
    (do-test #t 'crash check-crash)
    (do-test #f 'normal check-normal)
    (do-test #t 'normal check-normal)))

(http-mat websocket-maximum-message-size ()
  (define me self)
  (capture-events)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (ws:upgrade conn request me
         (ws:options
          [maximum-message-size
           (string->number (http:get-param 'max params))])))))
  (let ()
    (define (do-test max-size)
      (discard-events)
      (let* ([c (ws:connect "127.0.0.1" (get-http-port)
                  (format "/?max=~a" max-size) self)]
             [_ (receive [#(ws:init ,@c) 'ok])]
             [s (receive [#(ws:init ,s) s])])
        (let ([str (make-string max-size #\a)])
          (ws:send c str)
          (receive (after 1000 (throw 'no-echo))
            [#(ws:message ,@s ,@str) 'ok]))
        (let ([str (make-string (+ max-size 1) #\b)])
          (ws:send c str)
          (receive (after 1000 (throw 'no-server-close))
            [#(ws:closed ,@s 1002 ,_) 'ok]))
        (receive (after 1000 (throw 'no-client-close))
          [#(ws:closed ,@c 1002 ,_) 'ok]
          [#(ws:closed ,@c 1001 ,_)
           ;; When the message size is large, it is likely the server
           ;; side closes the connection, and the client side fails
           ;; for an io-error, which is then translated into a 1001
           ;; code on the client side.
           'ok])))
    (do-test 100)
    (do-test 1000)
    (do-test 10000)
    (do-test 100000)))

(http-mat websocket-ping ()
  (define me self)
  (capture-events)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (ws:upgrade conn request me
         (ws:options
          [ping-frequency 10]
          [pong-timeout 1])))))
  (let-values ([(ip op) (connect-tcp "127.0.0.1" (get-http-port))])
    (for-each
     (lambda (str)
       (put-bytevector op (string->utf8 str))
       (put-bytevector op (string->utf8 "\r\n")))
     '("GET / HTTP/1.1"
       "Connection: Upgrade"
       "Host: 127.0.0.1"
       "Upgrade: websocket"
       "Sec-Websocket-Version: 13"
       "Sec-Websocket-Key: dGhlIHNhbXBsZSBub25jZQ=="))
    (put-bytevector op (string->utf8 "\r\n"))
    (flush-output-port op)
    (receive (after 1000 (throw 'no-ws:init))
      [#(ws:init ,s)
       (receive (after 1000 (throw 'no-server-close))
         [#(ws:closed ,@s 1002 "no response from ping") 'ok])])))

(http-mat websocket-client-connect-errors ()
  (define (do-test req)
    (catch (ws:connect "127.0.0.1" (get-http-port) req self)))
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (http:respond conn
         (string->number (http:get-param 'status params))
         (fold-right
          (lambda (k acc)
            (let ([v (http:find-param k params)])
              (if v
                  (cons (cons (symbol->string k) v) acc)
                  acc)))
          '()
          '(connection upgrade sec-websocket-accept))
         '#vu8()))))
  ;; Order matters here.
  (match-let*
   ([#(EXIT #(websocket-upgrade-failed 404))
     (do-test "/?status=404")]
    [#(EXIT #(websocket-missing-header connection))
     (do-test "/?status=101")]
    [#(EXIT #(websocket-invalid-header connection "sidestep"))
     (do-test "/?status=101&connection=sidestep")]
    [#(EXIT #(websocket-missing-header upgrade))
     (do-test "/?status=101&connection=upgrade")]
    [#(EXIT #(websocket-invalid-header upgrade "not-websocket"))
     (do-test "/?status=101&connection=upgrade&upgrade=not-websocket")]
    [#(EXIT #(websocket-missing-header sec-websocket-accept))
     (do-test "/?status=101&connection=upgrade&upgrade=websocket")]
    [#(EXIT #(websocket-invalid-header sec-websocket-accept "bad-key"))
     (do-test "/?status=101&connection=upgrade&upgrade=websocket&sec-websocket-accept=bad-key")])
   'ok))
