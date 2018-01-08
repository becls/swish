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
   (swish erlang)
   (swish meta)
   (except (chezscheme) define-record exit)
   )

  (define-record <child-end>
    timestamp
    pid
    killed
    reason)
  (define-record <child-start>
    timestamp
    supervisor
    pid
    name
    restart-type
    type
    shutdown)
  (define-record <gen-server-debug>
    timestamp
    duration
    type
    client
    server
    message
    state
    reply)
  (define-record <gen-server-terminating>
    timestamp
    name
    last-message
    state
    reason)
  (define-record <http-request>
    timestamp
    pid
    host
    method
    path
    header
    params)
  (define-record <statistics>
    timestamp
    date
    reason            ; startup | update | suspend | resume | shutdown
    bytes-allocated
    osi-bytes-used
    sqlite-memory
    sqlite-memory-highwater
    ports
    processes
    databases
    statements
    listeners
    hashes
    working-set-size
    pagefile-usage
    private-usage
    cpu
    real
    bytes
    gc-count
    gc-cpu
    gc-real
    gc-bytes)
  (define-record <supervisor-error>
    timestamp
    supervisor
    error-context
    reason
    child-pid
    child-name)
  (define-record <system-attributes>
    timestamp
    date
    software-version
    computer-name)
  (define-record <transaction-retry>
    timestamp
    database
    duration
    count
    sql)
  )
