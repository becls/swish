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
 (swish digest)
 (swish imports)
 (swish mat)
 (swish testing)
 )

(define-syntax foreach
  (syntax-rules ()
    [(_ ([v ls] ...) e0 e1 ...)
     (for-each (lambda (v ...) e0 e1 ...) ls ...)]))

(define digest-algorithms
  (if (eq? default-digest-provider (current-digest-provider))
      '(sha1)
      '(md5 sha1 sha256 sha384 sha512)))

(define sample
  (case-lambda
   [() (string->utf8 "The quick brown fox jumps over the lazy dog.")]
   [(digest)
    (hex-string->hash
     (match digest
       [md5 "e4d909c290d0fb1ca068ffaddf22cbd0"]
       [sha1 "408d94384216f890ff7a0c3528e8bed1e0b01621"]
       [sha256 "ef537f25c895bfa782526529a9b63d97aa631564d5d789c2b765448c8635fb6c"]
       [sha384 "ed892481d8272ca6df370bf706e4d7bc1b5739fa2177aae6c50e946678718fc67a7af2819a021c2fc34e91bdb63409d7"]
       [sha512 "91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d12890cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed"]))]
   [(digest hmac-key)
    (hex-string->hash
     (match hmac-key
       ["asparagus"
        (match digest
          [md5 "726999b3be39d760e89dc6cec47ea7c6"]
          [sha1 "ba1b3fd9e0d1ae3f1c90bb1d26e8d19784f5ed13"]
          [sha256 "ece8d02d8bca1f93bdef2a584b4ecd8f01d61f3f77458fbd998a81520a65d048"]
          [sha384 "b794dc7999b3bd804a2441acb1d8915856532ad6c297dd34475b2ad170556284b8f098eb56da462f98dc43fae8ce436c"]
          [sha512 "9587cd2fcabb2405fb3055e53bdc507bc313310c2647486e1392a5e4c6ee0fb2fed19452249b52fab1e3920e03e5c99a2177feea248761b3f6a2b886de608447"])]
       ["tetrahedron"
        (match digest
          [md5 "31653c0d334dae8e362336e3a52d8906"]
          [sha1 "2da0332017db8de2abd14dc2cd56aec3bba57cbd"]
          [sha256 "b80f269097184bbee705f4c8fc6f0239356d9818b637d838b1d870b67d959308"]
          [sha384 "5f0e27e31e8be58c6e15d8d596f0f1a4122e4a24325592f3856d3e34893257dca04730eebc8da1312dd30012ba7e4f5b"]
          [sha512 "e4995a245974d92e697ac43646eb73fbfb1f2d48544e57c75e4c76c93695c8d501b4d563eb3e6c7a10bc755c869978c69c14250ad6c929cc5705d284773cab7e"])]))]))

(define expected-provider-name (digest-provider-name (current-digest-provider)))
(define verbose (make-parameter #f))

(define (by-chunk hasher data chunk-size)
  (let ([len (bytevector-length data)])
    (do ([i 0 (+ i chunk-size)]) ((>= i len))
      (let ([size (min chunk-size (- len i))])
        (when (verbose)
          (printf " (hash! hasher data i=~s size=~s)\n" i size))
        (hash! hasher data i size)))
    (get-hash hasher)))

(define (exercise alg key data expected)
  (match-let*
   ([,hasher (open-digest alg key)]
    [,@expected-provider-name (digest-provider-name hasher)]
    [,_ (hash! hasher data)]
    [,@expected (get-hash hasher)]
    [ok (do ([i 1 (+ i 1)]) ((= i (bytevector-length data)) 'ok)
          (match (by-chunk hasher data i)
            [,@expected 'ok]))]
    [,hasher (open-digest alg key (current-digest-provider))]
    [,_ (hash! hasher data)]
    [,@expected (get-hash hasher)])
   'ok))

(mat digest ()
  (define (get-hex digest)
    (match-let* ([,@expected-provider-name (digest-provider-name digest)])
      (hash->hex-string (get-hash digest))))
  (foreach ([alg digest-algorithms])
    (match-let*
     ([,input (sample)]
      [,expected (sample alg)]
      [ok (exercise alg #f input expected)]
      [ok (if (symbol? alg)
              (exercise (string-upcase (symbol->string alg)) #f input expected)
              'ok)])
     'ok))
  (match-let*
   ([,empty-bv (make-bytevector 0)]
    [,hash-empty-bv
     (lambda (alg)
       (let ([digest (open-digest alg)])
         (hash! digest empty-bv)
         (get-hex digest)))]
    ["da39a3ee5e6b4b0d3255bfef95601890afd80709"
     (get-hex (open-digest 'sha1))]
    ["da39a3ee5e6b4b0d3255bfef95601890afd80709"
     (hash-empty-bv 'sha1)])
   (unless (eq? default-digest-provider (current-digest-provider))
     (match-let*
      (["d41d8cd98f00b204e9800998ecf8427e"
        (get-hex (open-digest 'md5))]
       ["d41d8cd98f00b204e9800998ecf8427e"
        (hash-empty-bv 'md5)]
       ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        (get-hex (open-digest 'sha256))]
       ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        (hash-empty-bv 'sha256)]
       ["38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
        (get-hex (open-digest 'sha384))]
       ["38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"
        (hash-empty-bv 'sha384)]
       ["cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
        (get-hex (open-digest 'sha512))]
       ["cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
        (hash-empty-bv 'sha512)])
      'ok))
   'ok))

(unless (eq? default-digest-provider (current-digest-provider))
  (mat hmac-digest ()
    (foreach ([hmac-key '("asparagus" "tetrahedron")])
      (foreach ([alg digest-algorithms])
        (match-let*
         ([,input (sample)]
          [,expected (sample alg hmac-key)]
          [,hmac-key (string->utf8 hmac-key)]
          [ok (exercise alg hmac-key input expected)]
          [ok (if (symbol? alg)
                  (exercise (string-upcase (symbol->string alg)) hmac-key input expected)
                  'ok)])
         'ok)))))

(mat hex-string ()
  (define max-size (expt 2 12))
  (define random-bytevector
    (with-interrupts-disabled
     (parameterize ([random-seed 4294967295])
       (let ([bv (make-bytevector max-size)])
         (do ([i 0 (fx+ i 1)]) ((fx= i max-size))
           (bytevector-u8-set! bv i (random 256)))
         bv))))
  (do ([i 0 (+ i 7)]) ((> i max-size))
    (match-let*
     ([,input (make-bytevector i)]
      [,_ (bytevector-copy! random-bytevector (- max-size i 1) input 0 i)]
      [,hex-string (hash->hex-string input)]
      [#t (or (equal? input (hex-string->hash hex-string)) hex-string)]
      [,hex-string (string-upcase hex-string)]
      [#t (or (equal? input (hex-string->hash hex-string)) hex-string)])
     'ok))
  (match-let*
   ([,empty-bv #vu8(#xda #x39 #xa3 #xee #x5e #x6b #x4b #xd #x32 #x55 #xbf
                     #xef #x95 #x60 #x18 #x90 #xaf #xd8 #x7 #x9)]
    ["da39a3ee5e6b4b0d3255bfef95601890afd80709" (hash->hex-string empty-bv)]
    [,@empty-bv (hex-string->hash "da39a3ee5e6b4b0d3255bfef95601890aFD80709")])
   'ok))

(mat bytevector->hex-string ()
  (match-let*
   ([,bv (string->utf8 "This is only a test.")]
    ["aa5ec50ae51153ae2932a23ecd006dee613403c2"
     (bytevector->hex-string bv 'sha1)]
    ["aa5ec50ae51153ae2932a23ecd006dee613403c2"
     (bytevector->hex-string bv 'sha1 1)]
    ["aa5ec50ae51153ae2932a23ecd006dee613403c2"
     (bytevector->hex-string bv 'sha1 8)])
   (unless (eq? default-digest-provider (current-digest-provider))
     (match-let*
      (["3d57d0d7d5817e272a533c8ef6e3be9d"
        (bytevector->hex-string bv 'md5)]
       ["3d57d0d7d5817e272a533c8ef6e3be9d"
        (bytevector->hex-string bv 'md5 1)]
       ["3d57d0d7d5817e272a533c8ef6e3be9d"
        (bytevector->hex-string bv 'md5 5)]
       ["9973a0d1729566f34377e90cea4a40c0c1106d55baf2a3e0127ddcad4015962d"
        (bytevector->hex-string bv 'sha256)]
       ["9973a0d1729566f34377e90cea4a40c0c1106d55baf2a3e0127ddcad4015962d"
        (bytevector->hex-string bv 'sha256 3)]
       ["04133564638c82a3ed481f565c1a70e95699a9221a44e3f9bcdc4bdd927f22e79365311fd345623c4c55c3dfa9948b33"
        (bytevector->hex-string bv 'sha384)]
       ["04133564638c82a3ed481f565c1a70e95699a9221a44e3f9bcdc4bdd927f22e79365311fd345623c4c55c3dfa9948b33"
        (bytevector->hex-string bv 'sha384 2)]
       ["07164354d2facc5518205e345e0d276a5477fb6a665e70b98f3b1b4836e17d59aa0ad1113d3a330f67cf81e13ee0105c69e50e3b09ea5bc6f85840bc9e6c3ca2"
        (bytevector->hex-string bv 'sha512)]
       ["07164354d2facc5518205e345e0d276a5477fb6a665e70b98f3b1b4836e17d59aa0ad1113d3a330f67cf81e13ee0105c69e50e3b09ea5bc6f85840bc9e6c3ca2"
        (bytevector->hex-string bv 'sha512 7)])
      'ok))
   'ok))

(mat errors ()
  (define bip (open-bytevector-input-port (string->utf8 "")))
  (define-values (bop get-bop) (open-bytevector-output-port))
  (define-syntax fails-with
    (syntax-rules ()
      [(_ expr string)
       (match-let*
        ([#(EXIT ,reason) (catch expr)]
         [string (swish-exit-reason->english reason)])
        'ok)]))
  (match-let*
   ([,@expected-provider-name (digest-provider-name (open-digest 'sha1))]
    [#(EXIT #(bad-arg get-hash browns)) (catch (get-hash 'browns))]
    [#(EXIT #(bad-arg hash! -5)) (catch (hash! (open-digest 'sha1) (string->utf8 "foo") -5 3))]
    [#(EXIT #(bad-arg hash! 0)) (catch (hash! (open-digest 'sha1) (string->utf8 "foo") 1 0))]
    [#(EXIT #(bad-arg hash! 123)) (catch (hash! (open-digest 'sha1) 123))]
    [#(EXIT #(bad-arg hash! 123)) (catch (hash! 123 (string->utf8 "foo")))]
    [#(EXIT #(bad-arg hash->hex-string #f)) (catch (hash->hex-string #f))]
    [#(EXIT #(bad-arg hash->hex-string "tag")) (catch (hash->hex-string "tag"))]
    [#(EXIT #(bad-arg hex-string->hash "tag")) (catch (hex-string->hash "tag"))]
    [#(EXIT ,alg-reason) (catch (open-digest "!exist"))]
    [ok (match alg-reason
          [#(bad-arg open-digest "!exist") 'ok]
          [#(io-error ,_ ,_ ,_)
           (guard (not (eq? default-digest-provider (current-digest-provider))))
           'ok])]
    [#(EXIT ,@alg-reason) (catch (open-digest "!exist" "foo"))]
    [#(EXIT #(bad-arg open-digest parakeet)) (catch (open-digest 'sha1 'parakeet))]
    [#(EXIT #(bad-arg close-digest ion)) (catch (close-digest 'ion))]
    [#(EXIT #(bad-arg bytevector->hex-string not-bv)) (catch (bytevector->hex-string 'not-bv 'sha1))]
    [#(EXIT ,bv-alg-reason) (catch (bytevector->hex-string #vu8(1) 'not-alg))]
    [ok (match bv-alg-reason
          [#(bad-arg bytevector->hex-string not-alg) 'ok]
          [#(io-error ,_ ,_ ,_)
           (guard (not (eq? default-digest-provider (current-digest-provider))))
           'ok])]
    [#(EXIT #(bad-arg bytevector->hex-string 0)) (catch (bytevector->hex-string #vu8(1 2 3) 'sha1 0))]
    [#(EXIT #(bad-arg bytevector->hex-string -1)) (catch (bytevector->hex-string #vu8(1 2 3) 'sha1 -1))]
    [#(EXIT ,reason) (catch (hash! (open-digest 'sha1) #vu8(1 2 3 4 5) 3 7))]
    [,all-ok (map (lambda (x) 'ok) digest-algorithms)]
    [,d* (map open-digest digest-algorithms)]
    [ok (begin (for-each close-digest d*) (for-each close-digest d*) 'ok)]
    [,@all-ok
     (map
      (lambda (digest)
        (fails-with (hash! digest (sample))
          "Exception in hash!: cannot write to closed digest."))
      d*)]
    [,@all-ok
     (map
      (lambda (digest)
        (fails-with (get-hash digest)
          "Exception in get-hash: cannot read from closed digest."))
      d*)]
    [#(EXIT #(bad-arg open-digest tofu)) (catch (open-digest 'sha1 #f 'tofu))]
    [#(EXIT #(bad-arg make-digest-provider "foo"))
     (catch (make-digest-provider "foo" car cdr cons list))]
    [#(EXIT #(bad-arg make-digest-provider "bar"))
     (catch (make-digest-provider 'foo "bar" cdr cons list))]
    [#(EXIT #(bad-arg make-digest-provider "bar"))
     (catch (make-digest-provider 'foo car "bar" cons list))]
    [#(EXIT #(bad-arg make-digest-provider "bar"))
     (catch (make-digest-provider 'foo car cdr "bar" list))]
    [#(EXIT #(bad-arg make-digest-provider "bar"))
     (catch (make-digest-provider 'foo car cdr cons "bar"))]
    [#(EXIT #(bad-arg open-digest 123))
     (catch (open-digest 123))]
    [#(EXIT #(bad-arg open-digest not-string-or-bytevector))
     (catch (open-digest 'sha1 'not-string-or-bytevector))]
    [#(EXIT #(bad-arg open-digest "not supported by default provider"))
     (catch (open-digest 'sha1 "not supported by default provider"
              default-digest-provider))]
    [#(EXIT #(bad-arg current-digest-provider xyz))
     (catch (current-digest-provider 'xyz))]
    [#(EXIT #(bad-arg hex-string->hash cursed))
     (catch (hex-string->hash 'cursed))]
    [#(EXIT #(bad-arg hex-string->hash "cursed"))
     (catch (hex-string->hash "cursed"))]
    [#(EXIT #(io-error open-digest "fake_foreign_entry" 123))
     (catch
      (open-digest 'anything #f
        (make-digest-provider 'bad-provider
          (lambda (algorithm hmac-key)
            ;; always return an error pair
            '("fake_foreign_entry" . 123))
          values
          values
          values)))]
    )
   (unless (eq? default-digest-provider (current-digest-provider))
     (match-let*
      ([,h* (map (lambda (alg) (open-digest alg "amethyst")) digest-algorithms)]
       [ok (begin (for-each close-digest h*) (for-each close-digest h*) 'ok)]
       [,@all-ok
        (map
         (lambda (digest)
           (fails-with (hash! digest (sample))
             "Exception in hash!: cannot write to closed digest."))
         h*)]
       [,@all-ok
        (map
         (lambda (digest)
           (fails-with (get-hash digest)
             "Exception in get-hash: cannot read from closed digest."))
         h*)])
      'ok))
   'ok))

(mat sha1 ()
  (define-foreign osi_open_SHA1)
  (define-foreign osi_hash_data (digest uptr) (bv ptr) (start_index size_t) (size unsigned-32))
  (define-foreign osi_get_SHA1 (digest uptr))
  (define osi_close_SHA1* (foreign-procedure "osi_close_SHA1" (uptr) void))
  (define ctxt
    (match (osi_open_SHA1)
      [,ctxt (guard (not (pair? ctxt))) ctxt]))
  (on-exit (osi_close_SHA1* ctxt)
    (match-let*
     ([(osi_hash_data . ,@UV_EINVAL) (osi_hash_data* ctxt "not a bytevector" 0 0)]
      [(osi_hash_data . ,@UV_EINVAL) (osi_hash_data* ctxt #vu8(1 2 3) 3 1)]
      [(osi_hash_data . ,@UV_EINVAL) (osi_hash_data* ctxt #vu8(1 2 3) 1 3)])
     'ok)
    (let ([bv (string->utf8 "The quick brown fox jumps over the lazy dog.")])
      (define (build n)
        (call-with-bytevector-output-port
         (lambda (op)
           (do ([i 0 (+ i 1)]) ((fx= i n))
             (put-bytevector op bv)))))
      (define (repeat n)
        (do ([n n (- n 1)]
             [buffer (build n)]
             [start-index 0 (+ start-index size)]
             [size (bytevector-length bv)])
            ((eqv? n 0) (hash->hex-string (osi_get_SHA1 ctxt)))
          (osi_hash_data ctxt buffer start-index size)))
      (define (concat n) (bytevector->hex-string (build n) 'sha1))
      (assert (string=? "da39a3ee5e6b4b0d3255bfef95601890afd80709"
                (repeat 0) (concat 0)))
      (assert (string=? "408d94384216f890ff7a0c3528e8bed1e0b01621"
                (repeat 1) (concat 1)))
      (assert (string=? "8f5f36a551598bfe05ddc29320af6a7199c83bea"
                (repeat 2) (concat 2)))
      (assert (string=? "245ec449184e3121811c7a1506364c2e7f450a69"
                (repeat 10) (concat 10)))
      (assert (string=? "305aab2f9e6e5fac89f8c0cb40d89337be9ed4f0"
                (repeat 13) (concat 13)))
      (assert (string=? "3673e790a691891b34a41ba0f277ac25e0675abb"
                (repeat 16) (concat 16))))))

(mat digest-management ()
  (define (get-print-digests)
    (let ([os (open-output-string)])
      (print-digests os)
      (get-output-string os)))
  (match-let*
   ([,_ (gc)]
    [0 (digest-count)]
    [,n 12]
    [,ds (map open-digest (make-list n 'sha1))]
    [,@n (digest-count)]
    [,pat
     (format "(?:  [0-9]+: ~a SHA1 opened [0-9]+\n){~a}"
       (digest-provider-name (current-digest-provider))
       n)]
    [(,str)
     (guard (string? str))
     (pregexp-match pat (get-print-digests))]
    [,_ (gc)]
    [0 (digest-count)]
    ["" (get-print-digests)]
    ["#<digest-provider swish>" (format "~s" default-digest-provider)]
    ["#<SHA1 digest>" (format "~s" (open-digest 'sha1))]
    )
   'ok))
