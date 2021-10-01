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

(library (swish events)
  (export
   <child-end>
   <child-start>
   <gen-server-debug>
   <gen-server-terminating>
   <http-request>
   <statistics>
   <supervisor-error>
   <system-attributes>
   <transaction-retry>
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish meta)
   )

  (define-tuple <child-end>
    timestamp
    pid
    killed
    reason
    details)
  (define-tuple <child-start>
    timestamp
    supervisor
    pid
    name
    restart-type
    type
    shutdown)
  (define-tuple <gen-server-debug>
    timestamp
    duration
    type
    client
    server
    message
    state
    reply)
  (define-tuple <gen-server-terminating>
    timestamp
    name
    pid
    last-message
    state
    reason
    details)
  (define-tuple <http-request>
    timestamp
    pid
    host
    method
    path
    header
    params)
  (define-tuple <statistics>
    timestamp
    date
    reason            ; startup | update | suspend | resume | shutdown
    bytes-allocated
    current-memory-bytes
    maximum-memory-bytes
    osi-bytes-used
    sqlite-memory
    sqlite-memory-highwater
    foreign-handles
    cpu
    real
    bytes
    gc-count
    gc-cpu
    gc-real
    gc-bytes
    os-free-memory)
  (define-tuple <supervisor-error>
    timestamp
    supervisor
    error-context
    reason
    details
    child-pid
    child-name)
  (define-tuple <system-attributes>
    timestamp
    date
    software-info
    machine-type
    computer-name
    os-pid
    os-system
    os-release
    os-version
    os-machine
    os-total-memory)
  (define-tuple <transaction-retry>
    timestamp
    database
    duration
    count
    sql)
  )
