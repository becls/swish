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

(library (swish ht)
  (export
   ht:delete
   ht:fold
   ht:is?
   ht:keys
   ht:make
   ht:ref
   ht:set
   ht:size
   )
  (import
   (chezscheme)
   (swish erlang)
   )

  (define seg-size 16)
  (define expand-load 5)
  (define contract-load 3)
  (define empty (make-vector seg-size '()))
  (define empty-segments (vector empty))

  (define-tuple <ht>
    hash-key
    equal-key?
    valid-key?
    size                                ; number of elements
    n                                   ; number of active slots
    max-n                               ; maximum slots
    bso                                 ; buddy slot offset
    expand-size
    contract-size
    segments
    )

  (define (ht:make hash-key equal-key? valid-key?)
    (<ht> make
      [hash-key hash-key]
      [equal-key? equal-key?]
      [valid-key? valid-key?]
      [size 0]
      [n seg-size]
      [max-n seg-size]
      [bso (div seg-size 2)]
      [expand-size (* seg-size expand-load)]
      [contract-size (* seg-size contract-load)]
      [segments empty-segments]))

  (define (ht:is? x)
    (profile-me)
    (<ht> is? x))

  (define (ht:size x)
    (profile-me)
    (<ht> size x))

  (define (ht:ref ht key default)
    (<ht> open ht ht. [valid-key? equal-key?])
    (unless (ht.valid-key? key) (bad-arg 'ht:ref key))
    (cond
     [(internal-find key ht.equal-key?
        (get-bucket ht (get-slot ht key))) => cdr]
     [else default]))

  (define (internal-find key equal-key? bucket)
    (and (not (null? bucket))
         (let ([kv (car bucket)])
           (if (equal-key? (car kv) key)
               kv
               (internal-find key equal-key? (cdr bucket))))))

  (define (ht:delete ht key)
    (<ht> open ht ht. [valid-key? equal-key? size segments])
    (unless (ht.valid-key? key) (bad-arg 'ht:delete key))
    (let-values ([(segments count)
                  (on-bucket (lambda (b) (internal-remove key ht.equal-key? b))
                    ht.segments (get-slot ht key))])
      (maybe-contract ht (- ht.size count) segments)))

  (define (internal-remove key equal-key? bucket)
    (if (null? bucket)
        (values '() 0)
        (let ([kv (car bucket)])
          (if (equal-key? (car kv) key)
              (values (cdr bucket) 1)
              (let-values ([(rest count)
                            (internal-remove key equal-key? (cdr bucket))])
                (if (eqv? count 0)
                    (values bucket 0)
                    (values (cons kv rest) count)))))))

  (define (ht:set ht key val)
    (<ht> open ht ht. [valid-key? equal-key? segments size])
    (unless (ht.valid-key? key) (bad-arg 'ht:set key))
    (let-values ([(segments count)
                  (on-bucket (lambda (b) (internal-set key val b ht.equal-key?))
                    ht.segments (get-slot ht key))])
      (maybe-expand ht (+ ht.size count) segments)))

  (define (internal-set key val bucket equal-key?)
    (if (null? bucket)
        (values (list (cons key val)) 1)
        (let ([kv (car bucket)] [rest (cdr bucket)])
          (if (equal-key? key (car kv))
              (values (cons (cons key val) rest) 0)
              (let-values ([(rest count)
                            (internal-set key val rest equal-key?)])
                (values (cons kv rest) count))))))

  (define (ht:fold ht f init)
    (define (fold-segments f acc segs i)
      (if (= i 0)
          acc
          (let ([seg (element i segs)])
            (fold-segments f
              (fold-segment f acc seg (vector-length seg))
              segs
              (- i 1)))))
    (define (fold-segment f acc seg i)
      (if (= i 0)
          acc
          (fold-segment f (fold-bucket f acc (element i seg)) seg (- i 1))))
    (define (fold-bucket f acc bucket)
      (if (null? bucket)
          acc
          (let ([kv (car bucket)])
            (fold-bucket f (f (car kv) (cdr kv) acc) (cdr bucket)))))
    (let ([segs (<ht> segments ht)])
      (fold-segments f init segs (vector-length segs))))

  (define (ht:keys ht)
    (ht:fold ht (lambda (key val acc) (cons key acc)) '()))

  (define (get-slot ht key)
    (<ht> open ht ht. [hash-key max-n n bso])
    (let ([hash (phash ht.hash-key key ht.max-n)])
      (if (> hash ht.n)
          (- hash ht.bso)
          hash)))

  (define (get-bucket ht slot)
    (get-bucket-s (<ht> segments ht) slot))

  (define (on-bucket f segments slot)
    (let* ([seg-i (+ (div (- slot 1) seg-size) 1)]
           [bucket-i (+ (remainder (- slot 1) seg-size) 1)]
           [seg (element seg-i segments)]
           [b0 (element bucket-i seg)])
      (let-values ([(b1 count) (f b0)])
        (if (eq? b0 b1)
            (values segments count)
            (values (set-element seg-i segments (set-element bucket-i seg b1))
              count)))))

  (define (get-bucket-s segs slot)
    (let ([seg-i (+ (div (- slot 1) seg-size) 1)]
          [bucket-i (+ (remainder (- slot 1) seg-size) 1)])
      (element bucket-i (element seg-i segs))))

  (define (put-bucket-s segs slot bucket)
    (let* ([seg-i (+ (div (- slot 1) seg-size) 1)]
           [bucket-i (+ (remainder (- slot 1) seg-size) 1)]
           [seg (set-element bucket-i (element seg-i segs) bucket)])
      (set-element seg-i segs seg)))

  (define (maybe-expand ht size segments)
    (<ht> open ht ht. [segments expand-size n bso max-n hash-key])
    (cond
     [(> size ht.expand-size)
      (let-values
          ([(max-n bso segments)
            (if (= ht.n ht.max-n)
                (values (* 2 ht.max-n) (* 2 ht.bso) (expand-segments segments))
                (values ht.max-n ht.bso segments))])
        (let* ([n (+ ht.n 1)]
               [slot1 (- n bso)]
               [b (get-bucket-s segments slot1)]
               [slot2 n])
          (let-values ([(b1 b2) (rehash b slot1 slot2 max-n ht.hash-key)])
            (let* ([segments (put-bucket-s segments slot1 b1)]
                   [segments (put-bucket-s segments slot2 b2)])
              (<ht> copy ht
                [size size]
                [n n]
                [max-n max-n]
                [bso bso]
                [expand-size (* n expand-load)]
                [contract-size (* n contract-load)]
                [segments segments])))))]
     [else (<ht> copy ht [size size] [segments segments])]))

  (define (expand-segments segments)
    (let ([n (vector-length segments)])
      (do ([v (make-vector (* n 2) empty)] [i 0 (+ i 1)])
          [(= i n) v]
        (vector-set! v i (vector-ref segments i)))))

  (define (maybe-contract ht size segments)
    (<ht> open ht ht. [segments contract-size n bso max-n])
    (cond
     [(eq? ht.segments segments) ht]
     [(and (< size ht.contract-size) (> ht.n seg-size))
      (let* ([slot1 (- ht.n ht.bso)]
             [b1 (get-bucket-s segments slot1)]
             [slot2 ht.n]
             [b2 (get-bucket-s segments slot2)]
             [segments (put-bucket-s segments slot1 (append b1 b2))]
             [segments (put-bucket-s segments slot2 '())]
             [n (- ht.n 1)])
        (if (= n ht.bso)
            (<ht> copy ht
              [max-n (div ht.max-n 2)]
              [bso (div ht.bso 2)]
              [size size]
              [n n]
              [expand-size (* n expand-load)]
              [contract-size (* n contract-load)]
              [segments (contract-segments segments)])
            (<ht> copy ht
              [size size]
              [n n]
              [expand-size (* n expand-load)]
              [contract-size (* n contract-load)]
              [segments segments])))]
     [else (<ht> copy ht [size size] [segments segments])]))

  (define (contract-segments segments)
    (let ([n (div (vector-length segments) 2)])
      (do ([v (make-vector n)] [i 0 (+ i 1)])
          [(= i n) v]
        (vector-set! v i (vector-ref segments i)))))

  (define (rehash bucket slot1 slot2 max-n hash-key)
    (if (null? bucket)
        (values '() '())
        (let* ([kv (car bucket)] [key (car kv)])
          (let-values
              ([(l1 l2) (rehash (cdr bucket) slot1 slot2 max-n hash-key)])
            (let ([slot (phash hash-key key max-n)])
              (if (= slot slot1)
                  (values (cons kv l1) l2)
                  (begin
                    (assert (= slot slot2))
                    (values l1 (cons kv l2)))))))))

  (define (phash hash-key key range)
    (+ (modulo (hash-key key) range) 1))

  (define (element index tuple)
    (vector-ref tuple (- index 1)))

  (define (set-element index tuple value)
    (let ([new (vector-copy tuple)])
      (vector-set! new (- index 1) value)
      new))
  )
