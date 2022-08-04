;; Copyright 2019 Beckman Coulter, Inc.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import
 (scheme)
 (swish imports)
 (swish mat)
 )

(define-syntax test-vectors
  (syntax-rules ()
    [(_ [input output] ...)
     (begin (test-case input output) ...)]))

;; macro so we get better source information from bad-match
(define-syntax test-case
  (syntax-rules (BASE64)
    [(_ (BASE64 input) output)
     (match-let*
      ([,in (string->utf8 input)]
       [,out (string->utf8 output)]
       [output (utf8->string ($encode in))]
       [input (utf8->string ($decode out))]
       [,@out ($encode in)]
       [,@in ($decode out)])
      'ok)]
    [(_ input output)
     (bytevector? (datum input))
     (match-let*
      ([,in input]
       [,out (string->utf8 output)]
       [output (utf8->string ($encode input))]
       [input ($decode out)]
       [,@out ($encode in)]
       [,@in ($decode out)])
      'ok)]))

(define (generate-bv len p)
  (let-values ([(op get-bv) (open-bytevector-output-port)])
    (do ([i 0 (+ i 1)]) ((= i len))
      (put-u8 op (p i)))
    (get-bv)))

(mat base64 ()
  (fluid-let-syntax ([$encode (identifier-syntax base64-encode-bytevector)]
                     [$decode (identifier-syntax base64-decode-bytevector)])
    (define (round-trip input)
      (match-let*
       ([,out ($encode input)]
        [,@input ($decode out)])
       'ok))
    (test-vectors
     ;; from https://tools.ietf.org/html/rfc4648#section-10
     [(BASE64 "") ""]
     [(BASE64 "f") "Zg=="]
     [(BASE64 "fo") "Zm8="]
     [(BASE64 "foo") "Zm9v"]
     [(BASE64 "foob") "Zm9vYg=="]
     [(BASE64 "fooba") "Zm9vYmE="]
     [(BASE64 "foobar") "Zm9vYmFy"]
     ;; testing zeros in the input
     [#vu8(0) "AA=="]
     [#vu8(0 0) "AAA="]
     [#vu8(0 0 0) "AAAA"]
     [#vu8(0 0 0 0) "AAAAAA=="]
     [#vu8(0 0 0 0 0) "AAAAAAA="]
     [#vu8(0 0 0 0 0 0) "AAAAAAAA"]
     ;; testing high values in the input
     [#vu8(255) "/w=="]
     [#vu8(255 255) "//8="]
     [#vu8(255 255 255) "////"]
     [#vu8(255 255 255 255) "/////w=="]
     ;; hit each character in the output encoding
     [#vu8(#x00 #x10 #x83 #x10 #x51 #x87 #x20 #x92 #x8b #x30 #xd3 #x8f
           #x41 #x14 #x93 #x51 #x55 #x97 #x61 #x96 #x9b #x71 #xd7 #x9f
           #x82 #x18 #xa3 #x92 #x59 #xa7 #xa2 #x9a #xab #xb2 #xdb #xaf
           #xc3 #x1c #xb3 #xd3 #x5d #xb7 #xe3 #x9e #xbb #xf3 #xdf #xbf)
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"])
    (round-trip (generate-bv 256 values))
    (parameterize ([random-seed 2609306995])
      (round-trip (generate-bv 1021 (lambda (i) (random 256))))
      (round-trip (generate-bv 1022 (lambda (i) (random 256))))
      (round-trip (generate-bv 1023 (lambda (i) (random 256))))
      (round-trip (generate-bv 1024 (lambda (i) (random 256)))))))

(mat base64url ()
  (fluid-let-syntax ([$encode (identifier-syntax base64url-encode-bytevector)]
                     [$decode (identifier-syntax base64url-decode-bytevector)])
    (define (round-trip input)
      (match-let*
       ([,out ($encode input)]
        [,@input ($decode out)])
       'ok))
    (test-vectors
     ;; from https://tools.ietf.org/html/rfc4648#section-10
     [(BASE64 "") ""]
     [(BASE64 "f") "Zg=="]
     [(BASE64 "fo") "Zm8="]
     [(BASE64 "foo") "Zm9v"]
     [(BASE64 "foob") "Zm9vYg=="]
     [(BASE64 "fooba") "Zm9vYmE="]
     [(BASE64 "foobar") "Zm9vYmFy"]
     ;; testing zeros in the input
     [#vu8(0) "AA=="]
     [#vu8(0 0) "AAA="]
     [#vu8(0 0 0) "AAAA"]
     [#vu8(0 0 0 0) "AAAAAA=="]
     [#vu8(0 0 0 0 0) "AAAAAAA="]
     [#vu8(0 0 0 0 0 0) "AAAAAAAA"]
     ;; testing high values in the input
     [#vu8(255) "_w=="]
     [#vu8(255 255) "__8="]
     [#vu8(255 255 255) "____"]
     [#vu8(255 255 255 255) "_____w=="]
     ;; hit each character in the output encoding
     [#vu8(#x00 #x10 #x83 #x10 #x51 #x87 #x20 #x92 #x8b #x30 #xd3 #x8f
           #x41 #x14 #x93 #x51 #x55 #x97 #x61 #x96 #x9b #x71 #xd7 #x9f
           #x82 #x18 #xa3 #x92 #x59 #xa7 #xa2 #x9a #xab #xb2 #xdb #xaf
           #xc3 #x1c #xb3 #xd3 #x5d #xb7 #xe3 #x9e #xbb #xf3 #xdf #xbf)
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"])
    (round-trip (generate-bv 256 values))
    (parameterize ([random-seed 2609306995])
      (round-trip (generate-bv 1021 (lambda (i) (random 256))))
      (round-trip (generate-bv 1022 (lambda (i) (random 256))))
      (round-trip (generate-bv 1023 (lambda (i) (random 256))))
      (round-trip (generate-bv 1024 (lambda (i) (random 256)))))))

(mat base64-errors ()
  (define bip (open-bytevector-input-port (string->utf8 "")))
  (define-values (bop get-bop) (open-bytevector-output-port))
  (match-let*
   ([#(EXIT #(bad-arg base64-encode-bytevector 7)) (catch (base64-encode-bytevector 7))]
    [#(EXIT #(bad-arg base64url-encode-bytevector 7)) (catch (base64url-encode-bytevector 7))]
    [#(EXIT #(bad-arg base64-decode-bytevector 7)) (catch (base64-decode-bytevector 7))]
    [#(EXIT #(bad-arg base64url-decode-bytevector 7)) (catch (base64url-decode-bytevector 7))]
    [,short (string->utf8 "Zm9vYg=")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@short)) (catch (base64-decode-bytevector short))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@short)) (catch (base64url-decode-bytevector short))]
    [,short (string->utf8 "fun")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@short)) (catch (base64-decode-bytevector short))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@short)) (catch (base64url-decode-bytevector short))]
    [,long (string->utf8 "VG9mdQ===")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@long)) (catch (base64-decode-bytevector long))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@long)) (catch (base64url-decode-bytevector long))]
    [,bad (string->utf8 "====")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    [,bad (string->utf8 "=BAD")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    [,bad (string->utf8 "B=AD")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    [,bad (string->utf8 "BA=D")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    [,bad (string->utf8 "VG9m Q==")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    [,bad (string->utf8 "YnVn====")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    [,bad (string->utf8 "YnVnY===")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    [,bad (string->utf8 "soBAD===")]
    [#(EXIT #(bad-arg base64-decode-bytevector ,@bad)) (catch (base64-decode-bytevector bad))]
    [#(EXIT #(bad-arg base64url-decode-bytevector ,@bad)) (catch (base64url-decode-bytevector bad))]
    )
   'ok))
