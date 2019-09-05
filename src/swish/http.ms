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
 (swish string-utils)
 (swish testing)
 (swish watcher)
 )

(define test-port 54333)

(define (boot-http)
  (http-port-number test-port)
  (web-dir (path-combine (base-dir) "src" "swish" "web-mats"))

  (start-event-mgr)
  (event-mgr:set-log-handler            ; silence the event-mgr
   (lambda (x) x)
   (spawn&link (lambda () (receive))))
  (event-mgr:flush-buffer)

  (match (http-sup:start&link)
    [#(ok ,pid) pid]))

(define (shutdown-http)
  (cond
   [(whereis 'http-sup) =>
    (lambda (pid)
      (monitor pid)
      (receive (after 60000 (raise 'http-sup-still-running))
        [#(DOWN ,_ ,@pid ,_) 'ok]))]))

(define-syntax http-mat
  (syntax-rules ()
    [(_ name tags e1 e2 ...)
     (mat name tags ($http-mat (lambda () (boot-http) (let () e1 e2 ...))))]))

(define ($http-mat thunk)
  (let* ([pid (spawn
               (lambda ()
                 (parameterize ([compile-profile #f])
                   (thunk))))]
         [m (monitor pid)])
    (on-exit (shutdown-http)
      (receive (after 300000 (kill pid 'shutdown) (raise 'timeout))
        [#(DOWN ,_ ,@pid normal) 'ok]
        [#(DOWN ,_ ,@pid ,reason) (raise reason)]))))

(define (simple-tcp x)
  (let-values ([(ip op) (connect-tcp "127.0.0.1" test-port)])
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

(http-mat handlers ()
  (define (test input)
    (let-values ([(op get) (open-bytevector-output-port)])
      (match (catch (http:handle-input
                     (open-bytevector-input-port (string->utf8 input)) op))
        [#(EXIT ,reason) (guard (not (eq? reason 'normal))) (raise reason)]
        [,_ (utf8->string (get))])))
  (define (test-error&output input)
    (let-values ([(op get) (open-bytevector-output-port)])
      (let ([er (catch (http:handle-input
                        (open-bytevector-input-port (string->utf8 input))
                        op))])
        (cons er (utf8->string (get))))))
  (when (file-directory? (tmp-dir))
    (for-each (lambda (x)
                (when (ends-with? x ".tmp")
                  (delete-file (path-combine (tmp-dir) x))))
      (directory-list (tmp-dir))))
  (match-let*
   (["" (test "")]
    [#(EXIT #(unhandled-input ,bv))
     (guard (string=? (utf8->string bv) "unhandled"))
     (catch (test "unhandled"))]
    [#(EXIT #(unhandled-input ,bv))
     (guard (string=? (utf8->string bv) "GET / HTTP/1.0"))
     (catch (test "GET / HTTP/1.0\r\n\r\n"))]
    [#(EXIT input-limit-exceeded)
     (catch (test (make-string 4097 #\a)))]
    [#(EXIT input-limit-exceeded)
     (catch (test (string-append
                   "GET /static/ok.txt HTTP/1.1\r\n"
                   (make-string 1048577 #\a) "\r\n"
                   "\r\n")))]
    [#(EXIT content-limit-exceeded)
     (catch (test (string-append
                   "POST /static/ok.txt HTTP/1.1\r\n"
                   "Content-Length: 16777217\r\n"
                   "Content-Type: application/x-www-form-urlencoded\r\n"
                   "\r\n")))]
    [#(EXIT unexpected-eof)
     (catch (test (string-append
                   "POST /static/ok.txt HTTP/1.1\r\n"
                   "Content-Length: 10\r\n"
                   "Content-Type: application/x-www-form-urlencoded\r\n"
                   "\r\n"
                   "0123")))]
    [#(EXIT #(invalid-http-path "/.."))
     (catch (test "GET /.. HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(invalid-http-path "foo"))
     (catch (test "GET foo HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(bad-arg http:respond "foo"))
     (catch (test "GET /mat?cmd=bad+arg&bad=foo HTTP/1.1\r\n\r\n"))]
    [#(EXIT #(invalid-content-length "none"))
     (catch (test (string-append
                   "POST /static/ok.txt HTTP/1.1\r\n"
                   "Content-Length: none\r\n"
                   "\r\n")))]

    ;; internal server errors
    [,internal-server-error
      "HTTP/1.1 500 \r\nContent-Length: 167\r\nCache-Control: no-cache\r\n\r\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>Internal server error</title></head><body><h1>The server was unable to complete the request.</h1></body></html>"]
    [(#(EXIT internal-error) . ,@internal-server-error)
     (test-error&output "GET /mat?cmd=internal-error HTTP/1.1\r\n\r\n")]
    [(#(EXIT internal-error) . "partial-output")
     (test-error&output "GET /mat?cmd=internal-error&partial=partial-output HTTP/1.1\r\n\r\n")]
    ;; static content
    [#(EXIT #(invalid-http-method PUT "/static/ok.txt"))
     (catch (test "PUT /static/ok.txt HTTP/1.1\r\n\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: max-age=3600\r\n\r\nok\n"
     (test "GET /static/ok.txt HTTP/1.1\r\nConnection: close\r\n\r\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 3\r\nCache-Control: max-age=3600\r\n\r\nok\n"
     (test "GET /static/ok.txt HTTP/1.1\nConnection: close\n\n")]
    ["HTTP/1.1 200 \r\nContent-Length: 36\r\nCache-Control: max-age=3600\r\nContent-Type: text/html\r\n\r\n<html><body><p>ok</p></body></html>\n"
     (test "GET /static/ok.html HTTP/1.1\nConnection: close\n\n")]

    ;; header
    [#(EXIT invalid-header)
     (catch (test (string-append
                   "GET /mat?cmd=echo+header HTTP/1.1\r\n"
                   "Host localhost\r\n\r\n")))]
    ["HTTP/1.1 200 \r\nContent-Length: 49\r\nCache-Control: no-cache\r\n\r\n((\"Connection\" . \"close\") (\"Host\" . \"localhost\"))"
     (test (string-append
            "GET /mat?cmd=echo+header HTTP/1.1\r\n"
            "Connection: close\r\n"
            "Host: localhost\r\n\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 49\r\nCache-Control: no-cache\r\n\r\n((\"Connection\" . \"close\") (\"Host\" . \"localhost\"))"
     (test (string-append
            "GET /mat?cmd=echo+header HTTP/1.1\r\n"
            "Connection: close\r\n"
            "Host: \t localhost\r\n\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 67\r\nCache-Control: no-cache\r\n\r\n((\"Connection\" . \"close\") (\"Host\" . \"localhost\") (\"No-Value\" . \"\"))"
     (test
      (string-append
       "GET /mat?cmd=echo+header HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: localhost\r\n"
       "No-Value:\r\n\r\n"))]
    ["HTTP/1.1 200 \r\nContent-Length: 67\r\nCache-Control: no-cache\r\n\r\n((\"Connection\" . \"close\") (\"Host\" . \"localhost\") (\"No-Value\" . \"\"))"
     (test
      (string-append
       "GET /mat?cmd=echo+header HTTP/1.1\r\n"
       "Connection: close\r\n"
       "Host: localhost\r\n"
       "No-Value: \r\n\r\n"))]
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
    [#(EXIT unexpected-eof)
     (catch (test (string-append
                   "GET /mat?cmd=echo+unhandled HTTP/1.1\r\n"
                   "Content-Length: 256\r\n"
                   "\r\n"
                   "ok")))]
    ["HTTP/1.1 200 \r\nContent-Length: 7\r\nCache-Control: no-cache\r\n\r\n0123456"
     (test (string-append
            "GET /mat?cmd=echo+unhandled&count=7 HTTP/1.1\r\n"
            "Content-Length: 16\r\n"
            "\r\n"
            "0123456789ABCDEF"))]
    [(#(EXIT http-violation) .
      "HTTP/1.1 200 \r\nContent-Length: 17\r\nCache-Control: no-cache\r\n\r\n0123456789ABCDEFG")
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
    [(#(EXIT http-violation) . ,@internal-server-error)
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
    ["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"
     (test (string-append
            "GET /mat?cmd=echo&echo=ok HTTP/1.1\r\n"
            "Content-Length: 16\r\n"
            "\r\n"
            "0123456789abcdef"))]
    ["HTTP/1.1 200 \r\nContent-Length: 2\r\nCache-Control: no-cache\r\n\r\nok"
     (test (string-append
            "POST /mat HTTP/1.1\r\n"
            "Connection: close\r\n"
            "Content-Length: 16\r\n"
            "Content-Type: application/x-www-form-urlencoded\r\n"
            "\r\n"
            "cmd=echo&echo=ok"))]
    [(#(EXIT http-violation) . ,@internal-server-error)
     (test-error&output
      (string-append
       "POST /mat HTTP/1.1\r\n"
       "Connection: keep-alive\r\n"
       "Content-Length: 20\r\n"
       "Content-Type: application/x-www-form-urlencoded\r\n"
       "\r\n"
       "cmd=bad+read&count=1"
       "don't read me"))]
    [(#(EXIT http-violation) .
      "HTTP/1.1 200 \r\nContent-Length: 10\r\nCache-Control: no-cache\r\n\r\ndon't read")
     (test-error&output
      (string-append
       "POST /mat HTTP/1.1\r\n"
       "Connection: keep-alive\r\n"
       "Content-Length: 27\r\n"
       "Content-Type: application/x-www-form-urlencoded\r\n"
       "\r\n"
       "cmd=echo+unhandled&count=10"
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
    [#(EXIT unexpected-eof)
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 44\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNy--")))]
    [#(EXIT invalid-multipart-boundary)
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 4\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "ok\r\n")))]
    [#(EXIT invalid-multipart-boundary)
     (catch
      (test (string-append
             "GET /mat?cmd=echo+file HTTP/1.1\r\n"
             "Content-Length: 42\r\n"
             "Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryRMHyBKLIXfhX3ZNy\r\n"
             "\r\n"
             "------WebKitFormBoundaryRMHyBKLIXfhX3ZNyok")))]
    [#(EXIT #(invalid-content-disposition "none"))
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
    [#(EXIT #(invalid-content-disposition "form-data; name=sample"))
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
    [#(EXIT content-limit-exceeded)
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

(mat helpers ()
  (define header '(("Content-Length" . "bar") ("Content-Type" . "foo")))
  (define params '(("query" . "lookup") ("val" . "1")))
  (match-let*
   ([#(EXIT #(bad-arg http-port-number a)) (catch (http-port-number 'a))]
    [#(EXIT #(bad-arg http-port-number -1)) (catch (http-port-number -1))]
    [#(EXIT #(bad-arg http-request-limit a)) (catch (http-request-limit 'a))]
    [#(EXIT #(bad-arg http-request-limit 0)) (catch (http-request-limit 0))]
    [#(EXIT #(bad-arg http-header-limit a)) (catch (http-header-limit 'a))]
    [#(EXIT #(bad-arg http-header-limit 0)) (catch (http-header-limit 0))]
    [#(EXIT #(bad-arg http-content-limit a)) (catch (http-content-limit 'a))]
    [#(EXIT #(bad-arg http-content-limit 0)) (catch (http-content-limit 0))]
    [#f (http:find-header "None" header)]
    [#(EXIT #(bad-arg http:find-header None))
     (catch (http:find-header 'None header))]
    ["foo" (http:find-header "Content-Type" header)]
    ["foo" (http:find-header "content-type" header)]
    [#(EXIT #(invalid-header "None" ,@header))
     (catch (http:get-header "None" header))]
    [#(EXIT #(bad-arg http:get-header None))
     (catch (http:get-header 'None header))]
    ["foo" (http:get-header "Content-Type" header)]
    ["foo" (http:get-header "content-type" header)]
    [#f (http:find-param "None" params)]
    [#(EXIT #(bad-arg http:find-param None))
     (catch (http:find-param 'None params))]
    [#f (http:find-param "VAL" params)]
    ["1" (http:find-param "val" params)]
    [#(EXIT #(invalid-param "None" ,@params))
     (catch (http:get-param "None" params))]
    [#(EXIT #(bad-arg http:get-param None))
     (catch (http:get-param 'None params))]
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

(mat read-status ()
  (define (test input)
    (http:read-status (open-bytevector-input-port (string->utf8 input)) 4096))
  (match-let*
   ([#(EXIT input-limit-exceeded) (catch (test (make-string 4097 #\a)))]
    [#f (test "")]
    [#f (test "200 OK Malformed input")]
    [#f (test "HTTP/1.1 OK 200")]
    [200 (test "HTTP/1.1 200 OK")]
    [404 (test "HTTP/1.1 404 Not Found")]
    [200 (test "HTTP/1.1 200")]
    [200 (test "HTTP/1.1 200 This text does not matter")])
   'ok))

(http-mat http ()
  (capture-events)

  (assert (= test-port (http:get-port-number)))

  (let-values ([(ip op) (connect-tcp "127.0.0.1" test-port)])
    (put-bytevector op (string->utf8 "Malformed Request\r\n\r\n"))
    (flush-output-port op)
    (receive (after 10 (raise 'no-child-log))
      [`(<child-end> [reason #(unhandled-input ,x)])
       (assert (string=? (utf8->string x) "Malformed Request"))])
    (close-port op))

  (let-values ([(ip op) (connect-tcp "127.0.0.1" test-port)])
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
    (receive (after 100 (raise 'timeout))
      [#(read ,x)
       (unless (string=? x
                 (string-append
                  "HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n1"
                  "HTTP/1.1 200 \r\nContent-Length: 1\r\nCache-Control: no-cache\r\n\r\n2"))
         (raise `#(unexpected ,x)))])
    (close-port op))

  ;; fake an exit message, as if accept failed
  (process-trap-exit #t)
  (let ([http (whereis 'http-listener)])
    (link http)
    (send http `#(EXIT ,self accept-failed))
    (receive (after 100 (raise 'timeout))
      [#(EXIT ,@http ,_) 'ok])))

(http-mat mime-types ()
  (capture-events)
  (let* ([mime-types (path-combine (web-dir) "mime-types")]
         [mime-types.bak (string-append mime-types ".bak")])
    (rename-path mime-types mime-types.bak)
    (on-exit (rename-path mime-types.bak mime-types)
      ;; no mime-types file; http-cache crashes; supervisor will restart
      (utf8->string (simple-get "/static/ok.html"))
      (receive (after 1000 (raise 'timeout-waiting-for-cache-to-fail))
        [`(<gen-server-terminating> [name http-cache]
            [reason #(io-error ,_ uv_fs_open ,_)])
         'ok])

      ;; invalid mime-types file; http-cache crashes; supervisor will restart
      (write-file mime-types
        '(("css" . "text/css")
          (invalid mime type expression)
          ("html" . "text/html")))
      (utf8->string (simple-get "/static/ok.html"))
      (receive (after 1000 (raise 'timeout-waiting-for-cache-to-fail))
        [`(<gen-server-terminating> [name http-cache]
            [reason #(invalid-mime-type (invalid mime type expression))])
         'ok])))

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
    (sleep-ms 10)
    (let ([expected (format "HTTP/1.1 200 \r\nContent-Length: ~a\r\nCache-Control: no-cache\r\n\r\n~a"
                      (string-length s) s)]
          [actual (utf8->string (simple-get (string-append "/" file1)))])
      (or (string=? expected actual)
          (raise `#(unexpected ,expected ,actual)))))

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
        (http:respond op 200 '() (string->utf8 "ok")))))

  (check "ok")
  (check "ok")                          ; same request hits the cache

  (write-file (full inc2)
    '((define (reply)
        (http:respond op 200 '() (string->utf8 "yes")))))

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
  (capture-events)
  (let ([path (web-dir)])
    (send 'http-cache `#(path-watcher-failed ,path -1234))
    (receive (after 1000 (raise 'timeout-waiting-for-cache-to-fail))
      [`(<gen-server-terminating> [name http-cache] ,reason)
       (match reason
         [#(bad-match #(path-watcher-failed ,@path -1234) ,_)
          'ok])])))
