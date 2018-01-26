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

(library (swish mat)
  (export
   add-mat
   for-each-mat
   load-results
   mat
   run-mat
   run-mats
   run-mats-to-file
   summarize
   )
  (import (chezscheme))

  (define mats (make-parameter '()))
  (define (make-mat name tags test) (list* name tags test))
  (define mat-name car)
  (define mat-tags cadr)
  (define mat-test cddr)

  (define-syntax mat
    (syntax-rules ()
      [(_ name (tag ...) e1 e2 ...)
       (add-mat 'name '(tag ...) (lambda () e1 e2 ...))]))

  (define (add-mat name tags test)
    (if (assq name (mats))
        (errorf 'add-mat "mat ~a is already defined." name)
        (mats (cons (make-mat name tags test) (mats)))))

  (define (run-mat name reporter)
    (cond
     [(assq name (mats)) =>
      (lambda (mat)
        (let ([result (guard (e [else (cons 'fail e)])
                        ((mat-test mat))
                        'pass)])
          (reporter name (mat-tags mat) result)))]
     [else
      (errorf 'run-mat "mat ~a is not defined." name)]))

  (define (for-each-mat procedure)
    (for-each (lambda (mat) (procedure (mat-name mat) (mat-tags mat)))
      (reverse (mats))))

  (define-syntax run-mats
    (syntax-rules ()
      [(_) ($run-mats (map mat-name (reverse (mats))))]
      [(_ name1 name2 ...) ($run-mats '(name1 name2 ...))]))

  (define (result-type x)
    (cond
     [(eq? x 'pass) 'pass]
     [(and (pair? x) (eq? (car x) 'fail)) 'fail]
     [else #f]))

  (define (extract x)
    (if (condition? x)
        (let-values ([(op get) (open-string-output-port)])
          (display-condition x op)
          (get))
        (format "~s" x)))

  (define ($run-mats mat-names)
    (define (print-col col1 col2 col3)
      (printf " ~8a ~14a ~a\n" col1 col2 col3))
    (display "\n*********************************************************\n")
    (print-col "Result" "Test name" "Message")
    (display "=========================================================\n")
    (flush-output-port)
    (let lp ([mat-names mat-names] [passed 0] [failed 0])
      (cond
       [(null? mat-names)
        (display "*********************************************************\n\n")
        (printf "Tests run: ~s   Pass: ~s   Fail: ~s\n\n"
          (+ passed failed) passed failed)
        (flush-output-port)]
       [else
        (run-mat (car mat-names)
          (lambda (name tags result)
            (let ([rest (cdr mat-names)])
              (case (result-type result)
                [pass
                 (print-col "pass" name "")
                 (flush-output-port)
                 (lp rest (+ passed 1) failed)]
                [fail
                 (print-col "FAIL" name (extract (cdr result)))
                 (flush-output-port)
                 (lp rest passed (+ failed 1))]
                [else
                 (errorf '$run-mats "unknown result ~s" result)]))))])))

  (define (run-mats-to-file filename)
    (call-with-output-file filename
      (lambda (op)
        (for-each-mat
         (lambda (name tags)
           (run-mat name
             (lambda (name tags result)
               (write
                (list name tags
                  (case (result-type result)
                    [pass 'pass]
                    [fail (cons 'fail (extract (cdr result)))]
                    [else
                     (errorf 'run-mats-to-file "unknown result ~s" result)]))
                op)
               (newline op))))))
      'replace))

  (define (load-results filename)
    (call-with-input-file filename
      (lambda (ip)
        (let lp ()
          (let ([x (read ip)])
            (if (eof-object? x)
                '()
                (cons x (lp))))))))

  (define (summarize files)
    (let ([pass 0] [fail 0])
      (for-each
       (lambda (in-file)
         (for-each
          (lambda (ls)
            (cond
             [(and (list? ls) (= (length ls) 3) (caddr ls)) =>
              (lambda (result)
                (case (result-type result)
                  [pass (set! pass (+ pass 1))]
                  [fail (set! fail (+ fail 1))]
                  [else
                   (errorf 'summarize "unknown result ~s in file ~a"
                     result in-file)]))]
             [else
              (errorf 'summarize "unknown entry ~s in file ~a" ls in-file)]))
          (load-results in-file)))
       files)
      (values pass fail)))
  )
