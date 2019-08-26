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

(library (swish errors)
  (export
   current-exit-reason->english
   exit-reason->english
   swish-exit-reason->english
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish io)
   (swish osi)
   )

  (define (exit-reason->english x)
    ((current-exit-reason->english) x))

  (define (swish-exit-reason->english x)
    (match x
      [#(,reason #(gen-server call (,server ,request ,timeout))) (format "Call to server ~a with request ~s and timeout ~a ms failed: ~a" server request timeout (exit-reason->english reason))]
      [#(,reason #(gen-server call (,server ,request))) (format "Call to server ~a with request ~s failed: ~a" server request (exit-reason->english reason))]
      [#(EXIT ,x) (exit-reason->english x)]
      [#(accept-tcp-failed ,l ,who ,errno) (format "Error ~d from ~a when accepting a connection on ~a:~d: ~a." errno who (listener-address l) (listener-port-number l) (errno->english errno))]
      [#(bad-arg ,who ,arg) (format "Invalid argument to ~a: ~s." who arg)]
      [#(bad-journal-mode ,mode) (format "Invalid journal mode: ~s." mode)]
      [#(bad-match ,v ,src) (format "Pattern match failed~a: ~s." (src->english src) v)]
      [#(bad-tuple ,name ,x ,src) (format "Invalid type for tuple ~a~a: ~s." name (src->english src) x)]
      [#(bad-return-value ,other) (format "Bad return value: ~s." other)]
      [#(cannot-load-shared-object ,so-name ,reason) (format "Cannot load shared object ~s: ~a" so-name (exit-reason->english reason))]
      [#(connect-tcp-failed ,hostname ,port-spec ,who ,errno) (format "Error ~d from ~a when connecting to ~a on TCP port ~a: ~a." errno who hostname port-spec (errno->english errno))]
      [#(create-watched-process-failed ,command-line ,who ,errno) (format "Error ~d from ~a during create-watched-process ~s: ~a." errno who command-line (errno->english errno))]
      [#(db-error ,who (,osi-who ,errno . ,errstr) ,detail) (format "Database error ~d in ~a from ~a on ~s: ~a." errno who osi-who detail errstr)]
      [#(db-error ,who (,osi-who . ,errno) ,detail) (format "Database error ~d in ~a from ~a on ~s: ~a." errno who osi-who detail (errno->english errno))]
      [#(db-retry-failed ,sql ,count) (format "Database query failed after ~d retries: ~a." count sql)]
      [#(deadlock ,resource) (format "Deadlock on resource ~s." resource)]
      [#(error ,reason) (exit-reason->english reason)]
      [#(error ,reason ,_stack) (exit-reason->english reason)]
      [#(find-files-failed ,spec ,who ,errno) (format "Error ~d from ~a during find-files ~s: ~a." errno who spec (errno->english errno))]
      [#(http-file-not-found ,path) (format "HTTP file not found: ~a." path)]
      [#(http-handler-failed ,reason) (format "HTTP handler failed: ~a" (exit-reason->english reason))]
      [#(invalid-config-file ,config-file ,reason) (format #f "invalid config file ~s: ~a" config-file (exit-reason->english reason))]
      [#(invalid-content-length ,x) (format "Invalid Content-Length HTTP header: ~a" x)]
      [#(invalid-context ,who) (format "Invalid context for ~a." who)]
      [#(invalid-datum ,x) (format "Invalid datum: ~s." x)]
      [#(invalid-http-method ,method ,path) (format "Invalid HTTP method ~s for path ~s." method path)]
      [#(invalid-http-path ,path) (format "Invalid HTTP path: ~a." path)]
      [#(invalid-intensity ,x) (format "Invalid intensity: ~s." x)]
      [#(invalid-mime-type ,x) (format "Invalid MIME type: ~s." x)]
      [#(invalid-number ,x) (format "Invalid number: ~s." x)]
      [#(invalid-owner ,owner) (format "Invalid owner: ~s." owner)]
      [#(invalid-param ,name ,params) (format "Invalid parameter ~s in ~s." name params)]
      [#(invalid-period ,period) (format "Invalid period: ~s." period)]
      [#(invalid-procedure ,proc) (format "Invalid procedure: ~s." proc)]
      [#(invalid-strategy ,x) (format "Invalid strategy: ~s." x)]
      [#(io-error ,name ,who ,errno) (format "I/O error ~d from ~a on ~a: ~a." errno who name (errno->english errno))]
      [#(listen-tcp-failed ,address ,port-number ,who ,errno) (format "Error ~d from ~a when listening on TCP port ~d: ~a." errno who port-number (errno->english errno))]
      [#(name-already-registered ,pid) (format "Name is already registered to ~s." pid)]
      [#(osi-error ,name ,who ,errno) (format "Error ~d from ~a during ~a: ~a." errno who name (errno->english errno))]
      [#(osi-port-closed ,who ,p) (format "Error from ~a: ~a is closed." who (osi-port-name p))]
      [#(process-already-registered ,name) (format "Process is already registered as ~a." name)]
      [#(start-specs #(duplicate-child-name ,name)) (format "Duplicate child name in start-specs: ~s." name)]
      [#(start-specs #(invalid-child-spec ,x)) (format "Invalid child-spec in start-specs: ~s." x)]
      [#(start-specs #(invalid-name ,x)) (format "Invalid name in start-specs: ~s." x)]
      [#(start-specs #(invalid-restart-type ,restart-type)) (format "Invalid restart-type in start-specs: ~s." x)]
      [#(start-specs #(invalid-shutdown ,x)) (format "Invalid shutdown in start-specs: ~s." x)]
      [#(start-specs #(invalid-thunk ,x)) (format "Invalid thunk in start-specs: ~s." x)]
      [#(start-specs #(invalid-type ,x)) (format "Invalid type in start-specs: ~s." x)]
      [#(timeout-value ,x ,src) (format "Invalid timeout value~a: ~s." (src->english src) x)]
      [#(type-already-registered ,name) (format "Type ~s is already registered." name)]
      [#(unexpected-input ,x ,position) (format "Unexpected input at position ~d: ~s." position x)]
      [#(unhandled-input ,x) (format "Unhandled HTTP input: ~s." x)]
      [#(unknown-shared-object ,so-name) (format "Unknown shared object ~s." so-name)]
      [#(unowned-resource ,resource) (format "Unowned resource: ~s." resource)]
      [#(unsupported-db-version ,name ,version) (format "The database ~s schema version (~a) is unsupported by this software." name version)]
      [#(watch-directory-failed ,path ,who ,errno) (format "Error ~d from ~a during watch-directory ~s: ~a." errno who path (errno->english errno))]
      [content-limit-exceeded "HTTP content limit exceeded."]
      [expected-dictionary "Expected dictionary."]
      [http-violation "HTTP Content-Length violation."]
      [input-limit-exceeded "HTTP input limit exceeded."]
      [invalid-header "Invalid HTTP header."]
      [invalid-surrogate-pair "Invalid Unicode surrogate pair"]
      [log-handler-already-set "The log handler is already set."]
      [no-process "No process."]
      [timeout "Timeout."]
      [unexpected-eof "Unexpected end-of-file."]

      ;; The following must come last:
      [,x
       (cond
        [(string? x) x]
        [(condition? x)
         (let ([op (open-output-string)])
           (display-condition x op)
           (write-char #\. op)
           (get-output-string op))]
        [else (format "~s" x)])]))

  (define current-exit-reason->english
    (make-parameter swish-exit-reason->english))

  (define (src->english x)
    (match x
      [#(,at ,offset ,file) (format " ~a offset ~a of ~a" at offset file)]
      [,_ ""]))

  (define (errno->english x)
    (if (pair? x)
        (cdr x)
        (osi_get_error_text x)))
  )
