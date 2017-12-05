(define (go)
  (define open-time 0)
  (define write-time 0)
  (define read-time 0)
  (define close-time 0)
  (define-syntax profile
    (syntax-rules ()
      [(_ var e)
       (let* ([start (osi_hrtime)]
              [r e]
              [stop (osi_hrtime)])
         (set! var (+ var (- stop start)))
         r)]))
  (define (once)
    (let ([p (profile open-time
               (open-file-port "test" (+ O_RDWR O_CREAT) #o666))])
      (let ([bv (string->utf8 "Hi, Bob!\n")])
        (profile write-time (write-osi-port p bv 0 (bytevector-length bv) 0))
        (profile read-time (read-osi-port p bv 0 (bytevector-length bv) 0))
        (let ([result (utf8->string bv)])
          (unless (string=? result "Hi, Bob!\n")
            (errorf #f "Unexpected result: ~s" result)))
        (profile close-time (close-osi-port p)))))
  (let lp ([n 10000])
    (cond
     [(> n 0)
      (once)
      (lp (- n 1))]
     [else
      (display-string "---\n")
      (printf "open:  ~8d ns\n" (quotient open-time 10000))
      (printf "write: ~8d ns\n" (quotient write-time 10000))
      (printf "read:  ~8d ns\n" (quotient read-time 10000))
      (printf "close: ~8d ns\n" (quotient close-time 10000))
      (flush-output-port)
      (go)])))
