;;; Copyright 2020 Beckman Coulter, Inc.
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
(library (swish websocket)
  (export
   ws:close
   ws:connect
   ws:options
   ws:send
   ws:send!
   ws:upgrade
   )
  (import
   (except (chezscheme) utf8->string)
   (swish base64)
   (swish digest)
   (swish erlang)
   (swish errors)
   (swish gen-server)
   (swish http)
   (except (swish io) make-utf8-transcoder binary->utf8)
   (swish meta)
   (swish options)
   (swish pregexp)
   (swish string-utils)
   )

  (define-options ws:options
    (optional
     [fragmentation-size
      (default 1048576)
      (must-be (lambda (x) (or (not x) (and (fixnum? x) (fx> x 0)))))]
     [maximum-message-size
      (default 16777216)
      (must-be fixnum? fxpositive?)]
     [ping-frequency
      (default 30000)
      (must-be fixnum? fxpositive?)]
     [pong-timeout
      (default 5000)
      (must-be fixnum? fxpositive?)]))

  ;; We override Scheme and Swish's default UTF-8 decoding by
  ;; raising exceptions upon failure.

  (define (make-utf8-transcoder)
    (make-transcoder (utf-8-codec)
      (eol-style none)
      (error-handling-mode raise)))

  (define (utf8->string bv)
    (bytevector->string bv (make-utf8-transcoder)))

  (define (binary->utf8 bp)
    (transcoded-port bp (make-utf8-transcoder)))

  (define (sha1-hash bv)
    (let ([digest (open-digest 'sha1)])
      (on-exit (close-digest digest)
        (hash! digest bv)
        (get-hash digest))))

  (define (encode-key key)
    (utf8->string
     (base64-encode-bytevector
      (sha1-hash
       (string->utf8
        (string-append key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))))

  (define (handshake conn request)
    (<request> open request [method header])
    (define (as-list str)
      ;; RFC2616 has an augmented BNF grammar "#rule" which defines
      ;; comma-separated lists of elements.
      (fold-right
       (lambda (s acc)
         (let ([s (trim-whitespace s)])
           (if (string=? s "")
               acc
               (cons s acc))))
       '()
       (pregexp-split (re "( |\\t)*,( |\\t)*") str)))

    (define http-error
      (case-lambda
       [(conn reason) (http-error conn 400 '() reason)]
       [(conn status header reason)
        (http:respond conn status header)
        (throw reason)]))

    (define (check-header header-name expected error-code response-header)
      (let ([actual (http:find-header header-name header)])
        (cond
         [(not actual)
          (http-error conn error-code response-header
            `#(websocket-missing-header ,header-name))]
         [(string-ci=? actual expected) (void)]
         [(memp (lambda (s) (string-ci=? s expected)) (as-list actual)) (void)]
         [else
          (http-error conn error-code response-header
            `#(websocket-invalid-header ,header-name ,actual))])))

    (unless (eq? method 'GET)
      (http-error conn `#(websocket-invalid-method ,method)))
    (unless (http:find-header 'host header)
      (http-error conn `#(websocket-missing-header host)))

    (check-header 'upgrade "websocket" 400 '())
    (check-header 'connection "Upgrade" 400 '())
    (check-header 'sec-websocket-version "13" 426
      `(("Sec-WebSocket-Version" . "13")))

    (let ([key (http:find-header 'sec-websocket-key header)])
      (unless key
        (http-error conn `#(websocket-missing-header sec-websocket-key)))
      (http:respond conn 101
        `(("Connection" . "Upgrade")
          ("Upgrade" . "websocket")
          ("Sec-WebSocket-Accept" . ,(encode-key key))))))

  (define (http-upgrade server port request)
    (define (check-header header header-name expected)
      (let ([actual (http:find-header header-name header)])
        (cond
         [(not actual)
          (throw `#(websocket-missing-header ,header-name))]
         [(string-ci=? actual expected) (void)]
         [else
          (throw `#(websocket-invalid-header ,header-name ,actual))])))
    (let-values ([(ip op) (connect-tcp server port)])
      (let ([key (utf8->string (base64-encode-bytevector (random-bytevector 16)))])
        (put-bytevector op (string->utf8 (format "GET ~a HTTP/1.1\r\n" request)))
        (http:write-header op
          `(("Connection" . "Upgrade")
            ("Upgrade" . "websocket")
            ("Host" . ,(format "~a:~a" server port))
            ("Sec-WebSocket-Version" . 13)
            ("Sec-WebSocket-Key" . ,key)))
        (flush-output-port op)
        (match (http:read-status ip 4096)
          [101
           (let ([header (http:read-header ip 1048576)])
             (check-header header 'connection "Upgrade")
             (check-header header 'upgrade "websocket")
             (check-header header 'sec-websocket-accept (encode-key key))
             (values ip op))]
          [,status
           (close-port ip)
           (close-port op)
           (throw `#(websocket-upgrade-failed ,status))]))))

  (define (random-bytevector n)
    (let ([bv (make-bytevector n)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-u8-set! bv i (random 256)))
      bv))

  (define (put-u16 op u16)
    (put-u8 op (fxand #xFF (fxsrl u16 8)))
    (put-u8 op (fxand #xFF u16)))

  (include "unsafe.ss")

  (define (mask-bytevector! payload start payload-len mask-key)
    (define mask-len 4)
    (define stride 4)
    ;; keep safe bytevector-length so we check type of input bytevectors
    (declare-unsafe-primitives
     bytevector-u24-ref bytevector-u24-set!
     bytevector-u32-ref bytevector-u32-set!
     bytevector-u8-ref bytevector-u8-set!
     fx+ fx- fx= fx>= fxlogor fxmod fxsll fxxor)
    (define (mask-ref i) (bytevector-u8-ref mask-key (fxmod i mask-len)))
    (define (xor-byte! i m)
      (let ([x (bytevector-u8-ref payload i)])
        (bytevector-u8-set! payload i (fxxor x m))))
    (define (xor-range! from to mask-start)
      (do ([i from (fx+ i 1)] [j mask-start (fx+ j 1)]) ((fx= i to))
        (xor-byte! i (mask-ref j))))
    (define (load-mask len i)
      (do ([m 0 (fxlogor m (fxsll (mask-ref i) shift))]
           [i i (fx+ i 1)]
           [shift 0 (fx+ shift 8)])
          ((fx= shift len) m)))
    (define (boundary i)
      (let-values ([(d r) (fxdiv-and-mod i stride)])
        (fx* stride (fx+ d (fxmin r 1)))))
    (define end0 (fx+ start payload-len))
    (define end1 (fxmin (boundary start) end0))
    (define end2 (fx* stride (fx/ end0 stride)))
    (define mask2-start (fx- end1 start))
    (when (and (fx< -1 start end0) (fx<= end0 (bytevector-length payload)))
      (assert (fx= (bytevector-length mask-key) mask-len))
      (xor-range! start end1 0)
      (unless (fx>= end1 end2)
        (meta-cond
         [(fx<= 32 (fixnum-width))
          (let ([mask (load-mask 32 mask2-start)])
            (do ([i end1 (fx+ i stride)]) ((fx>= i end2))
              (let ([x (bytevector-u32-ref payload i 'little)])
                (bytevector-u32-set! payload i (fxxor x mask) 'little))))]
         [(fx<= 24 (fixnum-width))
          (let ([mask0 (load-mask 24 mask2-start)]
                [mask3 (mask-ref (fx+ mask2-start 3))])
            (do ([i end1 (fx+ i stride)]) ((fx>= i end2))
              (let ([x (bytevector-u24-ref payload i 'little)])
                (bytevector-u24-set! payload i (fxxor x mask0) 'little)
                (xor-byte! (fx+ i 3) mask3))))]))
      (when (fx>= end2 end1) ;; avoid redoing part of the range
        (xor-range! end2 end0 mask2-start))))

  (define (ws:read-loop message-limit ip forward)

    (define (get-u8 ip)
      (let ([x (#%get-u8 ip)])
        (if (eof-object? x)
            (throw 'unexpected-eof)
            x)))

    (define (get-u16 ip)
      (let* ([first (#%get-u8 ip)]
             [second (get-u8 ip)])
        (fxior (fxsll first 8) second)))

    (define get-u64
      ;; Allocating a single buffer for the reader process makes for very
      ;; efficient 64-bit decoding. Don't let this function be used by other
      ;; concurrent processes.
      (let ([bv (make-bytevector 8)])
        (lambda (ip)
          (unless (eqv? (get-bytevector-n! ip bv 0 8) 8)
            (throw 'unexpected-eof))
          (bytevector-u64-ref bv 0 'big))))

    (define (get-payload-len ip code)
      (match code
        [126 (get-u16 ip)]
        [127 (get-u64 ip)]
        [,len len]))

    (define (read-frame ip limit)
      (let* ([fin&opcode (get-u8 ip)]
             [final? (fxbit-set? fin&opcode 7)]
             [_ (unless (zero? (fxand fin&opcode #b01110000))
                  (throw 'websocket-reserved-bits-must-be-zero))]
             [opcode (fxand fin&opcode #x0F)]
             [control-frame? (fxbit-set? opcode 3)]
             [mask&payload-len (get-u8 ip)]
             [masked? (fxbit-set? mask&payload-len 7)]
             [payload-len (get-payload-len ip (fxand mask&payload-len #x7F))]
             [_ (when (and control-frame? (> payload-len 125))
                  (throw `#(websocket-control-frame-too-long ,payload-len)))]
             [_ (when (> payload-len limit)
                  (throw 'websocket-message-limit-exceeded))]
             [mask-key (and masked? ($get-bytevector-exactly-n ip 4))]
             [payload ($get-bytevector-exactly-n ip payload-len)])
        (when masked?
          (mask-bytevector! payload 0 payload-len mask-key))
        (values final? opcode payload)))

    (define (payloads->input-port payloads)
      (define pp 0)
      (define (r! bv start n)
        (match payloads
          [() 0]
          [(,payload . ,rest)
           (let ([count (min (- (bytevector-length payload) pp) n)])
             (cond
              [(= count 0)
               (set! payloads rest)
               (set! pp 0)
               (r! bv start n)]
              [else
               (unless (and (eq? bv payload) (fx= pp start))
                 (bytevector-copy! payload pp bv start count))
               (set! pp (+ pp count))
               count]))]))
      (make-custom-binary-input-port "websocket payload" r! #f #f #f))

    (define (extract type payloads)
      (define default-buf-size 1024)
      (define (if-eof x value)
        (if (eof-object? x)
            value
            x))
      (let ([ip (payloads->input-port payloads)])
        (if (eq? type 'text)
            (let ([bv0 (car payloads)])
              ;; If large enough, use *first* payload as our codec buffer so we
              ;; avoid one bytevector-copy! and avoid allocating a buffer here.
              (parameterize ([make-codec-buffer
                              (lambda (bp)
                                (if (fx>= (bytevector-length bv0) default-buf-size)
                                    bv0
                                    (make-bytevector default-buf-size)))])
                (if-eof (get-string-all (binary->utf8 ip)) "")))
            (if-eof (get-bytevector-all ip) '#vu8()))))

    ;; Set parameters for the reader process which creates custom ports for
    ;; decoding payloads as text and to assemble continuation payloads. We use
    ;; these ports only with block reads via get-string-all, get-bytevector-all,
    ;; or bytevector->string.
    (custom-port-buffer-size 1)
    (make-codec-buffer
     (lambda (bp)
       ;; Accelerate bytevector->string by recognizing hard-coded Chez Scheme
       ;; port name for bytevector input ports and reusing the input buffer.
       (or (and (input-port? bp) (equal? (port-name bp) "bytevector")
                (let ([bv (binary-port-input-buffer bp)])
                  ;; safe to reuse bv here because we own it
                  ;; and we don't use it for anything else
                  (and (fx>= (bytevector-length bv) 4) bv)))
           (make-bytevector 4))))

    (let lp ([type #f] [payloads '()] [limit message-limit])
      (let-values ([(final? opcode payload) (read-frame ip limit)])
        (let ([len (bytevector-length payload)])
          (case opcode
            [(0) ;; continuation frame
             (cond
              [(not type)
               (throw 'websocket-protocol-violation)]
              [final?
               (forward (extract type (reverse (cons payload payloads))))
               (lp #f '() message-limit)]
              [else
               (lp type (cons payload payloads) (- limit len))])]
            [(1) ;; text frame
             (cond
              [type
               (throw 'websocket-protocol-violation)]
              [(and final? (null? payloads))
               (forward (utf8->string payload))
               (lp #f '() message-limit)]
              [else
               (lp 'text (cons payload payloads) (- limit len))])]
            [(2) ;; binary frame
             (cond
              [type
               (throw 'websocket-protocol-violation)]
              [(and final? (null? payloads))
               (forward payload)
               (lp #f '() message-limit)]
              [else
               (lp 'binary (cons payload payloads) (- limit len))])]
            [(8) ;; connection close
             (cond
              [(= len 0) (forward 'close)]
              [(> len 1)
               (let* ([ip (open-bytevector-input-port payload)]
                      [code (get-u16 ip)]
                      [_ (when (or (< code 1000)
                                   (<= 1004 code 1006)
                                   (<= 1016 code 2999)
                                   (> code 4999))
                           (throw 'websocket-protocol-violation))]
                      [ip (binary->utf8 ip)]
                      [s (get-string-all ip)]
                      [reason (if (eof-object? s) "" s)])
                 (forward `#(close ,code ,reason)))]
              [else (throw 'websocket-protocol-violation)])]
            [(9) ;; ping
             (cond
              [final?
               (forward `#(ping ,payload))
               (lp type payloads limit)]
              [else
               (throw 'websocket-protocol-violation)])]
            [(10) ;; pong
             (cond
              [final?
               (forward `#(pong ,payload))
               (lp type payloads limit)]
              [else
               (throw 'websocket-protocol-violation)])]
            [else (throw `#(websocket-unknown-opcode ,opcode))])))))

  (define (ws:established ip op masked? process options)
    (match-define `(<ws:options> ,fragmentation-size ,maximum-message-size ,ping-frequency ,pong-timeout) options)

    (define tcp-write2 (make-tcp-write2 op))

    ;; state :=
    ;;   #(closed ,code ,ws-reason-str)
    ;;   #(ping ,waketime)
    ;;   #(pong ,deadline)

    (define put-u64
      ;; Allocating a single buffer for the writer process makes for very
      ;; efficient 64-bit encoding. Don't let this function be used by other
      ;; concurrent processes.
      (let ([bv8 (make-bytevector 8)])
        (lambda (op u64)
          (bytevector-u64-set! bv8 0 u64 'big)
          (put-bytevector op bv8))))

    (define (send-frame opcode masked? max-payload-size payload)

      (define (mask-len len)
        (if masked?
            (fxior #x80 len)
            len))

      (define (do-send final? opcode fast? start payload-len)
        (let-values ([(op get) (open-bytevector-output-port)])
          (put-u8 op
            (if final?
                (fxior #x80 opcode)
                opcode))
          (cond
           [(< payload-len 126)
            (put-u8 op (mask-len payload-len))]
           [(<= payload-len #xFFFF)
            (put-u8 op (mask-len 126))
            (put-u16 op payload-len)]
           [else
            (put-u8 op (mask-len 127))
            (put-u64 op payload-len)])
          (when masked?
            (let ([mask-key (random-bytevector 4)])
              (put-bytevector op mask-key)
              (mask-bytevector! payload start payload-len mask-key)))
          (tcp-write2 (get) payload start payload-len)))

      (let ([payload-len (bytevector-length payload)])
        (cond
         [(not max-payload-size)
          (do-send #t opcode (not masked?) 0 payload-len)]
         [(<= payload-len max-payload-size)
          (do-send #t opcode (not masked?) 0 payload-len)]
         [else
          (do-send #f opcode #f 0 max-payload-size)
          (let lp ([start max-payload-size] [remaining (- payload-len max-payload-size)])
            (cond
             [(<= remaining max-payload-size)
              (do-send #t 0 #f start remaining)]
             [else
              (do-send #f 0 #f start max-payload-size)
              (lp (+ start max-payload-size) (- remaining max-payload-size))]))])))

    (define (send-message-frame opcode masked? payload)
      (send-frame opcode masked? fragmentation-size payload))

    (define (send-close-frame masked? code reason)
      ;; Do not fail
      (catch
       (send-frame 8 masked? #f
         (call-with-bytevector-output-port
          (lambda (op)
            (put-u16 op code)
            (put-bytevector op (string->utf8 reason)))))))

    (define (send-ping-frame masked? payload)
      (send-frame 9 masked? #f payload))

    (define (send-pong-frame masked? payload)
      (send-frame 10 masked? #f payload))

    (define (on-init)
      (send process `#(ws:init ,self)))

    (define (on-close code ws-reason-str)
      (send process `#(ws:closed ,self ,code ,ws-reason-str)))

    (define (do-close reason code ws-reason-str)
      ;; We don't use exit-reason->english on reason and use that as
      ;; ws-reason-str because the WebSocket reason string has a 125
      ;; byte limit after UTF8 encoding.
      (send-close-frame masked? code ws-reason-str)
      (on-close code ws-reason-str)
      `#(stop ,reason #(closed ,code ,ws-reason-str)))

    (define (next-ping-time)
      (+ shared-read-timestamp ping-frequency))

    (define (next-pong-time)
      (+ (erlang:now) pong-timeout))

    (define (no-reply state)
      (match state
        [#(ping ,waketime) `#(no-reply ,state ,waketime)]
        [#(pong ,deadline) `#(no-reply ,state ,deadline)]))

    (define (process-died reason)
      (do-close reason
        (if (eq? reason 'normal)
            1000
            1011)
        ""))

    ;; This value is shared by the reader and writer processes. The
    ;; reader will set the current timestamp after it returns from
    ;; reading data from the original input port. The writer will look
    ;; at the timestamp to determine when it should send a ping
    ;; message. A shared variable here avoids unnecessary context
    ;; switches to the writer process just to put it back to sleep.
    (define shared-read-timestamp (erlang:now))

    (process-trap-exit #t)
    (monitor process)
    (let ([reader
           (spawn&link
            (let ([me self])
              (lambda ()
                (ws:read-loop maximum-message-size
                  (port->notify-port ip
                    (lambda (count)
                      (when (fx> count 0)
                        (set! shared-read-timestamp (erlang:now)))))
                  (lambda (msg)
                    (if (or (string? msg) (bytevector? msg))
                        (send process `#(ws:message ,me ,msg))
                        (send me `#(read ,self ,msg))))))))])
      (define (terminate reason state)
        (match state
          [#(closed ,_ ,_) 'ok]
          [,_
           (send-close-frame masked? 1001 "")
           (on-close 1001 "")])
        ;; In general, the current process may be poisoned with
        ;; process-parameters that may affect the state of future
        ;; communications, so we explicitly close the connection here.
        (force-close-output-port op)
        (tcp-write2) ;; close underlying osi port
        'ok)
      (define (handle-call msg from state) (match msg))
      (define (handle-cast msg state)
        (match msg
          [#(send ,opcode ,bv)
           (send-message-frame opcode masked?
             (if masked? (bytevector-copy bv) bv))
           (no-reply state)]
          [#(send! ,opcode ,bv)
           (send-message-frame opcode masked? bv)
           (no-reply state)]
          [close
           (do-close 'normal 1000 "")]))
      (define (handle-info msg state)
        (match msg
          [#(read ,@reader ,msg)
           (match msg
             [#(close ,code ,reason)
              (do-close
               (if (memq code '(1000 1001))
                   'normal
                   `#(websocket-remote-close ,code))
               code
               reason)]
             [close
              (do-close 'normal 1000 "")]
             [#(ping ,bv)
              (send-pong-frame masked? bv)
              (no-reply `#(ping ,(next-ping-time)))]
             [#(pong ,bv)
              (no-reply `#(ping ,(next-ping-time)))])]
          [`(EXIT ,@reader ,reason)
           (do-close reason
             (cond
              [(eq? reason 'normal) 1000]
              [(eq? reason 'websocket-message-limit-exceeded) 1009]
              [(i/o-decoding-error? reason) 1007]
              [(condition? reason) 1011]
              [else 1002])
             "")]
          [`(EXIT ,@process ,reason)
           (process-died reason)]
          [`(DOWN ,_ ,@process ,reason)
           (process-died reason)]
          [timeout
           (match state
             [#(ping ,t)
              (let ([npt (next-ping-time)])
                (cond
                 [(= t npt)
                  (send-ping-frame masked? '#vu8())
                  (no-reply `#(pong ,(next-pong-time)))]
                 [else
                  (no-reply `#(ping ,npt))]))]
             [#(pong ,deadline)
              (cond
               [(>= deadline (+ shared-read-timestamp pong-timeout))
                (do-close 'websocket-no-pong 1002 "no response from ping")]
               [else
                (no-reply `#(ping ,(next-ping-time)))])])]))

      (let ([t (next-ping-time)])
        (on-init)
        (gen-server:enter-loop `#(ping ,t) t))))

  (define ws:upgrade
    (case-lambda
     [(conn request process)
      (ws:upgrade conn request process (ws:options))]
     [(conn request process options)
      (arg-check 'ws:upgrade
        [conn process?]
        [request (lambda (x) (<request> is? x))]
        [process process?]
        [options (ws:options is?)])
      (handshake conn request)
      (http:switch-protocol
       (lambda (ip op)
         (tcp-nodelay op #t)
         (ws:established ip op #f process options)))]))

  (define ws:connect
    (case-lambda
     [(hostname port request process)
      (ws:connect hostname port request process (ws:options))]
     [(hostname port request process options)
      (arg-check 'ws:connect
        [hostname string?]
        [port (lambda (x) (and (fixnum? x) (fx<= 0 x 65535)))]
        [request string?]
        [process process?]
        [options (ws:options is?)])
      (let-values ([(ip op) (http-upgrade hostname port request)])
        (tcp-nodelay op #t)
        (spawn
         (lambda ()
           (ws:established ip op #t process options))))]))

  (define (ws:send who message)
    (gen-server:cast who
      (cond
       [(string? message) `#(send! 1 ,(string->utf8 message))]
       [(bytevector? message) `#(send 2 ,message)]
       [else (bad-arg 'ws:send message)])))

  (define (ws:send! who message)
    (gen-server:cast who
      (cond
       [(string? message) `#(send! 1 ,(string->utf8 message))]
       [(bytevector? message) `#(send! 2 ,message)]
       [else (bad-arg 'ws:send! message)])))

  (define (ws:close who)
    (gen-server:cast who 'close))
  )

#!eof mats

(load-this-exposing '(http-upgrade ws:read-loop mask-bytevector! random-bytevector))

(import
 (swish mat)
 (swish websocket))

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

(isolate-mat read-partial-u64 ()
  ;; If the code accidently uses get-bytevector-some, and the payload
  ;; size lands on a boundary that requires an additional read, this
  ;; code will fail with an unexpected-eof.
  (let* ([me self]
         [message "Hello"]
         [payload (map char->integer (string->list message))]
         [data-bv
          (u8-list->bytevector
           `(129                             ; final packet, string
             127                             ; 64-bit size
             0 0 0 0 0 0 0 ,(length payload) ; payload size
             ,@payload                       ; new packet
             136 0)                          ; closing packet, 0 size
           )]
         [ip (make-custom-binary-input-port "paused-input"
               (let ([index 0])
                 (lambda (bv start n)
                   (bytevector-copy! data-bv index bv start 1)
                   (set! index (+ index 1))
                   1))
               #f #f #f)])
    (ws:read-loop (ws:options maximum-message-size (ws:options)) ip
      (lambda (msg) (send me msg)))
    (receive [,@message 'ok])))

(isolate-mat websocket-ping-ex ()
  (define mat-pid self)

  (define payload-str (make-string 100 #\x))
  ;; This payload was generated from a sequence of 100 "x" characters
  ;; as bytes (not a string) with a mask of 0 and fragmentation size
  ;; of 3.
  (define payload
    '#vu8(#x01 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x00 #x83 #x00 #x00 #x00 #x00 #x78 #x78 #x78
          #x80 #x81 #x00 #x00 #x00 #x00 #x78))

  (define normal-close '#vu8(#x88 #x82 #x00 #x00 #x00 #x00 #x03 #xE8))

  (define (spawn-test delay close-delay)
    (spawn
     (lambda ()
       (let-values ([(ip op) (http-upgrade "127.0.0.1" (get-http-port) "/")])
         (define process self)
         (define reader
           (spawn&link
            (lambda ()
              (let lp ()
                (let ([x (get-u8 ip)])
                  (if (eof-object? x)
                      (exit x)
                      (lp)))))))
         (define timeout delay)
         (define sop
           (make-custom-binary-output-port (format "~a (slow)" (port-name op))
             (lambda (bv start n)
               (receive (after timeout 'ok))
               (let ([r (put-bytevector-some op bv start (min 64 n))])
                 (flush-output-port op)
                 r))
             #f #f (lambda () (close-port op))))
         (put-bytevector sop payload)
         (set! timeout close-delay)
         (put-bytevector sop normal-close)
         (close-output-port sop)))))

  (define (do-test delay close-delay)
    (discard-events)
    (let* ([tid (spawn-test delay close-delay)]
           [s (receive (after 1000 (throw 'ws:init-timeout))
                [#(ws:init ,s) s])])
      (gen-server:debug s '(message state reply) '())
      (events-until-dies s)))

  (define (events-until-dies pid)
    (receive
     [`(<child-end> ,@pid) '()]
     [,msg (cons msg (events-until-dies pid))]))

  (define (attempt who inputs test-case)
    ;; Build servers have different timing than our machines. Try to
    ;; be more forgiving.  Try each input in turn until one
    ;; succeeds. If they all fail, report the first failure.
    (let retry ([inputs inputs] [err #f] [err-events '()])
      (match inputs
        [() (throw `#(failed-all-inputs ,who ,err ,err-events))]
        [((,delay ,close-delay) . ,rest)
         (let ([events (do-test delay close-delay)])
           (match (try (test-case events))
             [`(catch ,reason ,e)
              (if err
                  (retry rest err err-events)
                  (retry rest e events))]
             [,result result]))])))

  (define (help-seek pred ls src)
    (match ls
      [(,first . ,rest)
       (if (pred first) ls (help-seek pred rest src))]
      [,_ (throw `#(bad-match ,ls ,src))]))

  (define-syntax make-matcher
    (syntax-rules ()
      [(_ pat) (lambda (x) (match x [pat #t] [,_ #f]))]))

  (define-match-extension seek
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(seek pat rest)
         #`((sub-match (help-seek (make-matcher pat) #,v #,(find-source #'pat))
              (pat . rest)))])))

  (start-silent-event-mgr)
  (capture-events)
  (supervisor:start&link 'http-sup 'one-for-all 0 1
    (http:configure-server #f 0
      (http:url-handler
       (ws:upgrade conn request mat-pid
         (ws:options
          [ping-frequency 500]
          [pong-timeout 100])))))

  ;; Ping frequency is much slower than the input data rate, so the
  ;; server should never send a ping (possibly one at the end). Expect
  ;; websocket to receive data and close normally.

  ;; The minimum delay needs to be less than 100 here. The close
  ;; should come immediately after the payload.
  (attempt 'case1 '((10 0) (50 0) (90 0))
    (lambda (events)
      (match-let*
       ([`(seek `(<http-request> [pid ,s]) ,events) events]
        [`(seek #(ws:message ,@s ,@payload-str) ,events) events]
        [`(seek #(ws:closed ,@s 1000 "") ,events) events])
       'ok)))

  ;; Ping frequency is barely slower than the input data rate. After
  ;; the initial payload is received the server sends a ping. Client
  ;; closes the connection. Expect websocket to receive data and close
  ;; normally.

  ;; The minimum delay is based on (* delay N) where N is the number
  ;; of times the slow output port puts bytes. For this case, 100 is a
  ;; reasonable minimum.  The maximum close-delay should be less than
  ;; (+ ping-frequency pong-timeout) or 600.
  (attempt 'case2 '((450 550) (400 575) (300 550))
    (lambda (events)
      (match-let*
       ([`(seek `(<http-request> [pid ,s]) ,events) events]
        ;; Observe a timeout where the state remained as ping.
        [`(seek `(<gen-server-debug> [type 3] [server ,@s] [message timeout]
                   [state #(ping ,_)]
                   [reply #(no-reply #(ping ,_) ,_)])
            ,events) events]
        [`(seek #(ws:message ,@s ,@payload-str) ,events) events]
        ;; Observe a timeout where the state changed from ping to pong.
        [`(seek `(<gen-server-debug> [type 3] [server ,@s] [message timeout]
                   [state #(ping ,_)]
                   [reply #(no-reply #(pong ,_) ,_)])
            ,events) events]
        [`(seek #(ws:closed ,@s 1000 "") ,events) events])
       'ok)))

  ;; Ping frequency is much faster than the input data rate. The
  ;; client does not respond. Expect disconnection for failure to
  ;; respond to ping.

  ;; The minimum delay needs to be greater than the ping frequency,
  ;; 1000 is double. The close-delay is absurd, the code should never
  ;; reach that point.
  (attempt 'case3 '((1000 10000) (2000 10000))
    (lambda (events)
      (match-let*
       ([`(seek `(<http-request> [pid ,s]) ,events) events]
        [`(seek #(ws:closed ,@s 1002 "no response from ping") ,events) events])
       'ok)))

  ;; Ping frequency is barely faster than the input data rate. The
  ;; client will send data before the pong timeout. Expect websocket
  ;; to receive data and close normally.

  ;; The minimum delay needs to be greater than the ping frequency,
  ;; but less than (+ ping-frequency pong-timeout), so (< 500 delay
  ;; 600). The close should come immediately after the payload.
  (attempt 'case4 '((510 0) (550 0) (575 0))
    (lambda (events)
      (match-let*
       ([`(seek `(<http-request> [pid ,s]) ,events) events]
        ;; Observe a timeout where the state changed from ping to pong.
        [`(seek `(<gen-server-debug> [type 3] [server ,@s] [message timeout]
                   [state #(ping ,_)]
                   [reply #(no-reply #(pong ,_) ,_)])
            ,events) events]
        ;; Observe a timeout where the state changed from pong to ping.
        [`(seek `(<gen-server-debug> [type 3] [server ,@s] [message timeout]
                   [state #(pong ,_)]
                   [reply #(no-reply #(ping ,_) ,_)])
            ,events) events]
        [`(seek #(ws:message ,@s ,@payload-str) ,events) events]
        [`(seek #(ws:closed ,@s 1000 "") ,events) events])
       'ok)))
  )

(mat mask-in-place ()
  (define mask-len 4)
  (define primes '(7 11 13 239 241 251))
  (define buffer-sizes '(0 1 4 8 256 512 1023 4000 16385))

  (define (build-buffer size prime)
    (let ([bv (make-bytevector size)])
      (do ([i 0 (+ i 1)] [n prime (fxmod (+ n prime) 256)]) ((= i size))
        (bytevector-u8-set! bv i n))
      bv))

  (define (try-mask! payload start payload-len mask-key)
    ;; sanity check arguments
    (assert (= mask-len (bytevector-length mask-key)))
    (let ([bv (bytevector-copy payload)])
      (mask-bytevector! bv start payload-len mask-key)
      bv))

  (define (gen-expected payload start payload-len mask-key)
    (let ([bv (bytevector-copy payload)]
          [bv-len (bytevector-length payload)]
          [end (+ start payload-len)])
      (when (and (< start bv-len) (<= end bv-len))
        (do ([i start (+ i 1)] [j 0 (fxmod (+ j 1) mask-len)]) ((= i end))
          (let* ([x (bytevector-u8-ref payload i)]
                 [m (bytevector-u8-ref mask-key j)])
            (bytevector-u8-set! bv i (fxxor x m)))))
      bv))

  (define (edge-cases payload mask-key)
    (define len (bytevector-length payload))
    (define (try-range start end)
      (when (and (<= 0 start end) (<= 0 end len))
        (match-let* ([,expect (gen-expected payload start end mask-key)]
                     [,@expect (try-mask! payload start end mask-key)])
          'ok)))
    (do ([i 0 (+ i 1)]) ((= i mask-len))
      (do ([j 0 (+ j 1)]) ((= j mask-len))
        (try-range i (- len j)))))

  (for-each
   (lambda (buffer-size)
     (for-each
      (lambda (prime)
        (let ([bv (build-buffer buffer-size prime)])
          (edge-cases bv #vu8(6 42 102 85))
          (edge-cases (random-bytevector buffer-size) (random-bytevector mask-len))
          (edge-cases (random-bytevector buffer-size) (random-bytevector mask-len))))
      primes))
   buffer-sizes))
