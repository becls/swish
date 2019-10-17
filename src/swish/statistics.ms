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

(import
 (chezscheme)
 (swish erlang)
 (swish event-mgr)
 (swish events)
 (swish mat)
 (swish statistics)
 (swish testing)
 )

(isolate-mat stats ()
  (capture-events)
  (process-trap-exit #t)
  (match-let*
   ([#(ok ,pid) (statistics:start&link)]
    [,@pid (whereis 'statistics)])
   (receive (after 1000 (throw 'timeout))
     [`(<statistics> [reason startup]) 'ok])
   (statistics:suspend)
   (receive (after 1000 (throw 'timeout))
     [`(<statistics> [reason suspend]) 'ok])
   (statistics:resume)
   (receive (after 1000 (throw 'timeout))
     [`(<statistics> [reason resume]) 'ok])
   (send pid 'timeout)
   (receive (after 1000 (throw 'timeout))
     [`(<statistics> [reason update]) 'ok])
   (kill pid 'shutdown)
   (receive (after 1000 (throw 'timeout))
     [`(<statistics> [reason shutdown]) 'ok])))

(start-silent-event-mgr)
