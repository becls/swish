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

(library (swish statistics)
  (export
   statistics:resume
   statistics:start&link
   statistics:suspend
   )
  (import
   (chezscheme)
   (swish db)
   (swish erlang)
   (swish event-mgr)
   (swish events)
   (swish gen-server)
   (swish io)
   (swish json)
   (swish osi)
   )

  (define (statistics:start&link)
    (gen-server:start&link 'statistics))

  (define (statistics:resume)
    (gen-server:cast 'statistics 'resume))

  (define (statistics:suspend)
    (gen-server:cast 'statistics 'suspend))

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
    (let* ([timestamp (erlang:now)]
           [stats (statistics)]
           [date (time-utc->date (make-time 'time-utc
                                   (* (mod timestamp 1000) 1000000)
                                   (div timestamp 1000)))])
      (match-let*
       ([#(,sqlite-memory ,sqlite-memory-highwater)
         (osi_get_sqlite_status* SQLITE_STATUS_MEMORY_USED #t)]
        [,delta (sstats-difference stats state)])
       (system-detail <statistics>
         [timestamp timestamp]
         [date date]
         [reason reason]
         [bytes-allocated (bytes-allocated)]
         [current-memory-bytes (current-memory-bytes)]
         [maximum-memory-bytes
          (let ([max (maximum-memory-bytes)])
            (reset-maximum-memory-bytes!)
            max)]
         [osi-bytes-used (osi_get_bytes_used)]
         [sqlite-memory sqlite-memory]
         [sqlite-memory-highwater sqlite-memory-highwater]
         [foreign-handles (count-foreign-handles (json:make-object) json:set!)]
         [cpu (time-duration (sstats-cpu delta))]
         [real (time-duration (sstats-real delta))]
         [bytes (sstats-bytes delta)]
         [gc-count (sstats-gc-count delta)]
         [gc-cpu (time-duration (sstats-gc-cpu delta))]
         [gc-real (time-duration (sstats-gc-real delta))]
         [gc-bytes (sstats-gc-bytes delta)]
         [os-free-memory (osi_get_free_memory)])
       stats)))

  ;; External entry points are run from the event-loop process.
  (set-top-level-value! '$suspend statistics:suspend)
  (set-top-level-value! '$resume statistics:resume)
  )
