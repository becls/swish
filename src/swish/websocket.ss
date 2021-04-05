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
           (throw `#(websocket-upgrade-failed ,status))]))))

  (define (random-bytevector n)
    (let ([bv (make-bytevector n)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-u8-set! bv i (random 256)))
      bv))

  (define (put-u16 op u16)
    (put-u8 op (fxand #xFF (fxsrl u16 8)))
    (put-u8 op (fxand #xFF u16)))

  (define (ws:read-loop ip message-limit forward)

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
             [mask-key (and masked? (get-bytevector-n ip 4))]
             [payload
              (if masked?
                  (let ([bv (make-bytevector payload-len)])
                    (do ([i 0 (+ i 1)]) ((= i payload-len))
                      (let* ([x (get-u8 ip)]
                             [m (bytevector-u8-ref mask-key (fxmod i 4))])
                        (bytevector-u8-set! bv i (fxxor x m))))
                    bv)
                  (get-bytevector-n ip payload-len))])
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
               (bytevector-copy! payload pp bv start count)
               (set! pp (+ pp count))
               count]))]))
      (make-custom-binary-input-port "websocket payload" r! #f #f #f))

    (define (extract type payloads)
      (define (if-eof x value)
        (if (eof-object? x)
            value
            x))
      (let ([ip (payloads->input-port (reverse payloads))])
        (if (eq? type 'text)
            (if-eof (get-string-all (binary->utf8 ip)) "")
            (if-eof (get-bytevector-all ip) '#vu8()))))

    (let lp ([type #f] [payloads '()] [limit message-limit])
      (let-values ([(final? opcode payload) (read-frame ip limit)])
        (let ([len (bytevector-length payload)])
          (case opcode
            [(0) ;; continuation frame
             (cond
              [(not type)
               (throw 'websocket-protocol-violation)]
              [final?
               (forward (extract type (cons payload payloads)))
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

    (define (send-frame op opcode masked? max-payload-size payload)

      (define (mask-len len)
        (if masked?
            (fxior #x80 len)
            len))

      (define (do-send final? opcode fast? start payload-len)
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
        (cond
         [fast? (put-bytevector op payload)]
         [masked?
          (let ([mask-key (random-bytevector 4)])
            (put-bytevector op mask-key)
            (do ([i 0 (+ i 1)]) ((= i payload-len))
              (let ([x (bytevector-u8-ref payload (+ start i))]
                    [m (bytevector-u8-ref mask-key (fxmod i 4))])
                (put-u8 op (fxxor x m)))))]
         [else
          (put-bytevector-some op payload start payload-len)]))

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
              (lp (+ start max-payload-size) (- remaining max-payload-size))]))]))

      (flush-output-port op))

    (define (send-message-frame op opcode masked? payload)
      (send-frame op opcode masked? fragmentation-size payload))

    (define (send-close-frame op masked? code reason)
      ;; Do not fail
      (catch
       (send-frame op 8 masked? #f
         (call-with-bytevector-output-port
          (lambda (op)
            (put-u16 op code)
            (put-bytevector op (string->utf8 reason)))))))

    (define (send-ping-frame op masked? payload)
      (send-frame op 9 masked? #f payload))

    (define (send-pong-frame op masked? payload)
      (send-frame op 10 masked? #f payload))

    (define (on-init)
      (send process `#(ws:init ,self)))

    (define (on-close code ws-reason-str)
      (send process `#(ws:closed ,self ,code ,ws-reason-str)))

    (define (on-message msg)
      (send process `#(ws:message ,self ,msg)))

    (define (do-close reason code ws-reason-str)
      ;; We don't use exit-reason->english on reason and use that as
      ;; ws-reason-str because the WebSocket reason string has a 125
      ;; byte limit after UTF8 encoding.
      (send-close-frame op masked? code ws-reason-str)
      (on-close code ws-reason-str)
      `#(stop ,reason #(closed ,code ,ws-reason-str)))

    (define (next-ping-time)
      (+ (erlang:now) ping-frequency))

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

    (process-trap-exit #t)
    (monitor process)
    (let ([reader (spawn&link
                   (let ([me self])
                     (lambda ()
                       (ws:read-loop ip maximum-message-size
                         (lambda (msg)
                           (send me `#(read ,self ,msg)))))))])
      (define (terminate reason state)
        (match state
          [#(closed ,_ ,_) 'ok]
          [,_
           (send-close-frame op masked? 1001 "")
           (on-close 1001 "")])
        ;; In general, the current process may be poisoned with
        ;; process-parameters that may affect the state of future
        ;; communications, so we explicitly close the connection here.
        (force-close-output-port op)
        'ok)
      (define (handle-call msg from state) (match msg))
      (define (handle-cast msg state)
        (match msg
          [#(send ,opcode ,bv)
           (send-message-frame op opcode masked? bv)
           (no-reply state)]
          [close
           (do-close 'normal 1000 "")]))
      (define (handle-info msg state)
        (match msg
          [#(read ,@reader ,msg)
           (match msg
             [,message
              (guard (or (string? message) (bytevector? message)))
              (on-message message)
              (no-reply `#(ping ,(next-ping-time)))]
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
              (send-pong-frame op masked? bv)
              (no-reply `#(ping ,(next-ping-time)))]
             [#(pong ,bv)
              (no-reply `#(ping ,(next-ping-time)))])]
          [`(EXIT ,@reader ,reason)
           (do-close reason
             (cond
              [(eq? reason 'normal) 1000]
              [(i/o-decoding-error? reason) 1007]
              [else 1002])
             "")]
          [`(EXIT ,@process ,reason)
           (process-died reason)]
          [`(DOWN ,_ ,@process ,reason)
           (process-died reason)]
          [timeout
           (match state
             [#(ping ,_)
              (send-ping-frame op masked? '#vu8())
              (no-reply `#(pong ,(next-pong-time)))]
             [#(pong ,_)
              (do-close 'websocket-no-pong 1002 "no response from ping")])]))

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
        (spawn
         (lambda ()
           (ws:established ip op #t process options))))]))

  (define (ws:send who message)
    (gen-server:cast who
      (cond
       [(string? message) `#(send 1 ,(string->utf8 message))]
       [(bytevector? message) `#(send 2 ,message)]
       [else (bad-arg 'ws:send message)])))

  (define (ws:close who)
    (gen-server:cast who 'close))
  )

#!eof mats

(load-this-exposing '(ws:read-loop))

(import
 (swish websocket))

(isolate-mat read-partial-u64 ()
  ;; If the code accidently uses get-bytevector-some, and the payload
  ;; size lands on a boundary that requires an additional read, this
  ;; code will fail with an unexpected-eof.
  (let* ([me self]
         [message "Hello"]
         [payload (map char->integer (string->list message))]
         [data-bv
          (u8-list->bytevector
           `(129                              ; final packet, string
             127                              ; 64-bit size
             0 0 0 0 0 0 0 ,(length payload)  ; payload size
             ,@payload                        ; new packet
             136 0)                           ; closing packet, 0 size
           )]
         [ip (make-custom-binary-input-port "paused-input"
               (let ([index 0])
                 (lambda (bv start n)
                   (bytevector-copy! data-bv index bv start 1)
                   (set! index (+ index 1))
                   1))
               #f #f #f)])
    (ws:read-loop ip (ws:options maximum-message-size (ws:options))
      (lambda (msg) (send me msg)))
    (receive [,@message 'ok])))
