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
(library (swish errors)
  (export
   current-exit-reason->english
   exit-reason->english
   exit-reason->stacks
   swish-exit-reason->english
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish internal)
   (swish io)
   (swish osi)
   )

  ($import-internal &fault-condition)

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
      [#(find-files-failed ,spec ,who ,errno) (format "Error ~d from ~a during find-files ~s: ~a." errno who spec (errno->english errno))]
      [#(http-file-not-found ,path) (format "HTTP file not found: ~a." path)]
      [#(http-handler-failed ,reason) (format "HTTP handler failed: ~a" (exit-reason->english reason))]
      [#(http-invalid-content-disposition ,x) (format "Invalid Content-Disposition HTTP header: ~a" x)]
      [#(http-invalid-content-length ,x) (format "Invalid Content-Length HTTP header: ~a" x)]
      [#(http-invalid-header ,name) (format "Invalid HTTP header: ~s." name)]
      [#(http-invalid-media-type ,x) (format "Invalid MIME type: ~s." x)]
      [#(http-invalid-method ,method ,path) (format "Invalid HTTP method ~s for path ~s." method path)]
      [#(http-invalid-param ,name) (format "Invalid HTTP parameter: ~s." name)]
      [#(http-invalid-path ,path) (format "Invalid HTTP path: ~a." path)]
      [#(http-no-response-body-allowed ,status)
       (format "HTTP response body is not allowed for status code ~a." status)]
      [#(http-unhandled-input ,x) (format "Unhandled HTTP input: ~s." x)]
      [#(invalid-config-file ,config-file ,reason) (format #f "invalid config file ~s: ~a" config-file (exit-reason->english reason))]
      [#(invalid-context ,who) (format "Invalid context for ~a." who)]
      [#(invalid-datum ,x) (format "Invalid datum: ~s." x)]
      [#(invalid-intensity ,x) (format "Invalid intensity: ~s." x)]
      [#(invalid-number ,x) (format "Invalid number: ~s." x)]
      [#(invalid-owner ,owner) (format "Invalid owner: ~s." owner)]
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
      [#(unknown-shared-object ,so-name) (format "Unknown shared object ~s." so-name)]
      [#(unowned-resource ,resource) (format "Unowned resource: ~s." resource)]
      [#(unsupported-db-version ,name ,version) (format "The database ~s schema version (~a) is unsupported by this software." name version)]
      [#(watch-directory-failed ,path ,who ,errno) (format "Error ~d from ~a during watch-directory ~s: ~a." errno who path (errno->english errno))]
      [#(websocket-control-frame-too-long ,len) (format "WebSocket control frame too long: ~d" len)]
      [#(websocket-invalid-header ,name ,actual) (format "Invalid WebSocket header ~s: ~s" name actual)]
      [#(websocket-invalid-method ,method) (format "Invalid WebSocket method: ~s" method)]
      [#(websocket-missing-header ,name) (format "Missing WebSocket header: ~s" name)]
      [#(websocket-remote-close ,code) (format "WebSocket remote endpoint closed with status code ~a" code)]
      [#(websocket-unknown-opcode ,opcode) (format "Unknown WebSocket opcode: ~d" opcode)]
      [#(websocket-upgrade-failed ,status) (format "WebSocket upgrade failed: ~s" status)]
      [expected-dictionary "Expected dictionary."]
      [http-content-limit-exceeded "HTTP content limit exceeded."]
      [http-file-upload-limit-exceeded "HTTP file upload limit exceeded."]
      [http-input-limit-exceeded "HTTP input limit exceeded."]
      [http-input-violation "Read too much HTTP input."]
      [http-invalid-header "Invalid HTTP header."]
      [http-invalid-multipart-boundary "Invalid HTTP multipart/form-data boundary."]
      [http-output-violation "Attempted to send too much HTTP output."]
      [http-request-timeout "Timeout waiting for HTTP request."]
      [http-side-effecting-handler "Invalid side-effecting HTTP handler."]
      [invalid-surrogate-pair "Invalid Unicode surrogate pair"]
      [log-handler-already-set "The log handler is already set."]
      [no-process "No process."]
      [timeout "Timeout."]
      [unexpected-eof "Unexpected end-of-file."]
      [websocket-message-limit-exceeded "WebSocket incoming message too large."]
      [websocket-no-pong "WebSocket failed to receive a ping response."]
      [websocket-protocol-violation "WebSocket protocol violation."]
      [websocket-reserved-bits-must-be-zero "WebSocket reserved bits must be zero."]
      [`(&fault-condition ,reason) (exit-reason->english reason)]

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

  (define (exit-reason->stacks reason)
    (define (get-k r)
      (if (continuation-condition? r)
          (condition-continuation r)
          (match r
            [#(EXIT ,reason) (get-k reason)]
            [,_ #f])))
    (define (cons-k r k*)
      (let ([k (get-k r)])
        (if (#%$continuation? k) (cons k k*) k*)))
    (define (add-stack k* reason)
      (match reason
        [`(&fault-condition [reason ,r] ,inner*)
         (fold-left add-stack (cons-k reason (cons-k r k*)) inner*)]
        [,_ (cons-k reason k*)]))
    (add-stack '() reason))

  (define-syntax redefine
    (syntax-rules ()
      [(_ var e) (#%$set-top-level-value! 'var e)]))

  ;; Native debugger doesn't know how to print our fault-condition and doesn't
  ;; understand multiple stacks, so package up a message condition and shadow
  ;; c's continuation by picking the first k returned by exit-reason->english,
  ;; which is typically closest to the source of the original error. Folks get
  ;; reasonable default behavior and they can inspect the condition directly
  ;; for more details.
  (redefine debug-condition
    (let ([system-debug-condition (#%$top-level-value 'debug-condition)])
      (case-lambda
       [() (system-debug-condition)]
       [(c)
        (system-debug-condition
         (match c
           [`(&fault-condition)
            (parameterize ([print-graph #t])
              (let ([msg (make-message-condition (exit-reason->english c))])
                (match (exit-reason->stacks c)
                  [(,k0 ,k1 . ,_)
                   (condition (make-continuation-condition k0) c msg)]
                  [,_ (condition c msg)])))]
           [,_ c]))])))

  )
