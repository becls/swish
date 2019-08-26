(http:include "common.ss")
(match (get-param "cmd")
  ["echo header"
   (my-respond
    (string->utf8
     (let-values ([(op get) (open-string-output-port)])
       (write header op)
       (get))))]
  ["use status"
   (http:respond op (string->number (get-param "status")) '() '#vu8())]
  ["echo"
   (my-respond (string->utf8 (get-param "echo")))]
  ["echo file"
   (match-let* (["1" (get-param "sample")]
                [#(<file> ,filename) (get-param "file")])
     (my-respond (read-file filename)))]
  ["echo unhandled"
   (my-respond
    (get-bytevector-n ip
      (string->number
       (or (find-param "count")
           (http:get-header "Content-Length" header)))))]
  ["bad arg"
   (my-respond (get-param "bad"))]
  ["file-headers"
   (http:respond-file op 200 '(("Cache-Control" . "no-cache")
                               ("Content-Type" . "text/plain"))
     (path-combine (web-dir) "static" "ok.txt"))]
  ["internal-error"
   (cond
    [(find-param "partial") =>
     (lambda (x) (put-bytevector op (string->utf8 x)))])
   (raise 'internal-error)]
  ["bad read"
   ;; consume input, but don't respond or error
   (do ([n (string->number (get-param "count")) (- n 1)])
       ((= n 0))
     (get-u8 ip))]
  )
