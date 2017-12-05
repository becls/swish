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

(library (swish statistics)
  (export
   <handle-counts>
   <memory-info>
   statistics:start&link
   statistics:resume
   statistics:suspend
   )
  (import
   (swish erlang)
   (swish event-mgr)
   (swish events)
   (swish gen-server)
   (swish osi)
   (except (chezscheme) define-record exit))

  (define (statistics:start&link)
    (gen-server:start&link 'statistics))

  (define (statistics:resume)
    (gen-server:cast 'statistics 'resume))

  (define (statistics:suspend)
    (gen-server:cast 'statistics 'suspend))

  (define-record <handle-counts>
    ports processes databases statements listeners hashes)

  (define-record <memory-info> page-fault-count peak-working-set-size
    working-set-size quota-peak-paged-pool-usage quota-paged-pool-usage
    quota-peak-non-paged-pool-usage quota-non-paged-pool-usage
    pagefile-usage peak-pagefile-usage private-usage)

  (define sqlite:STATUS_MEMORY_USED 0)

  (define timeout (* 5 60 1000))

  (define (init)
    (process-trap-exit #t)
    `#(ok ,(update 'startup
             (make-sstats
              (make-time 'time-thread 0 0)         ; cpu
              (make-time 'time-monotonic 0 0)      ; real
              0                                    ; bytes
              0                                    ; gc-count
              (make-time 'time-collector-cpu 0 0)  ; gc-cpu
              (make-time 'time-collector-real 0 0) ; gc-real
              0                                    ; gc-bytes
              )) ,timeout))
  (define (terminate reason state)
    (update 'shutdown state)
    'ok)
  (define (handle-call msg from state) (match msg))
  (define (handle-cast msg state)
    (match msg
      [resume (no-reply 'resume state)]
      [suspend (no-reply 'suspend state)]))
  (define (handle-info msg state)
    (match msg
      [timeout (no-reply 'update state)]))
  (define (no-reply reason state)
    `#(no-reply ,(update reason state) ,timeout))
  (define (time-duration t)
    (+ (time-second t)
       (/ (time-nanosecond t) 1000000000.0)))
  (define (update reason state)
    (let-values ([(timestamp date stats)
                  (with-interrupts-disabled
                   (let* ([timestamp (erlang:now)]
                          [date (current-date)])
                     (values timestamp date (statistics))))])
      ;; TODO: Update
      (match-let*
       ([#(,sqlite-memory ,sqlite-memory-highwater)
         (osi_get_sqlite_status* sqlite:STATUS_MEMORY_USED #t)]
        [,delta (sstats-difference stats state)])
       (system-detail <statistics>
         [timestamp timestamp]
         [date date]
         [reason reason]
         [bytes-allocated (bytes-allocated)]
         [osi-bytes-used (osi_bytes_used)]
         [sqlite-memory sqlite-memory]
         [sqlite-memory-highwater sqlite-memory-highwater]
         [ports 0]
         [processes 0]
         [databases 0]
         [statements 0]
         [listeners 0]
         [hashes 0]
         [working-set-size 0]
         [pagefile-usage 0]
         [private-usage 0]
         [cpu (time-duration (sstats-cpu delta))]
         [real (time-duration (sstats-real delta))]
         [bytes (sstats-bytes delta)]
         [gc-count (sstats-gc-count delta)]
         [gc-cpu (time-duration (sstats-gc-cpu delta))]
         [gc-real (time-duration (sstats-gc-real delta))]
         [gc-bytes (sstats-gc-bytes delta)])
       stats)))
  )
