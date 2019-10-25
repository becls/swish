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

(library (swish supervisor)
  (export
   <child>
   supervisor:delete-child
   supervisor:get-children
   supervisor:restart-child
   supervisor:start&link
   supervisor:start-child
   supervisor:terminate-child
   supervisor:validate-start-specs
   )
  (import
   (chezscheme)
   (swish erlang)
   (swish event-mgr)
   (swish events)
   (swish gen-server)
   )

  (define-state-tuple <supervisor-state>
    strategy                            ; one-for-one | one-for-all
    intensity                           ; integer >= 0
    period                              ; integer > 0
    children                            ; (<child> ...)
    restarts                            ; (<restart-time> ...)
    )

  (define-tuple <child>
    pid               ; process | #f
    name              ; symbol
    thunk             ; thunk
    restart-type      ; permanent | temporary | transient | watch-only
    shutdown          ; brutal-kill | integer >= 0 | infinity
    type              ; worker | supervisor
    )

  (define (supervisor:start&link name strategy intensity period start-specs)
    (gen-server:start&link name strategy intensity period start-specs))

  (define (supervisor:start-child supervisor child-spec)
    (call supervisor `#(start-child ,child-spec)))

  (define (supervisor:restart-child supervisor name)
    (call supervisor `#(restart-child ,name)))

  (define (supervisor:delete-child supervisor name)
    (call supervisor `#(delete-child ,name)))

  (define (supervisor:terminate-child supervisor name)
    (call supervisor `#(terminate-child ,name)))

  (define (supervisor:get-children supervisor)
    (call supervisor 'get-children))

  (define (call supervisor req)
    (gen-server:call supervisor req 'infinity))

  (define (init strategy intensity period start-specs)
    (process-trap-exit #t)
    (cond
     [(not (memq strategy '(one-for-one one-for-all)))
      (profile-me)
      `#(stop #(invalid-strategy ,strategy))]
     [(not (and (fixnum? intensity) (fx>= intensity 0)))
      (profile-me)
      `#(stop #(invalid-intensity ,intensity))]
     [(not (and (fixnum? period) (fx> period 0)))
      (profile-me)
      `#(stop #(invalid-period ,period))]
     [else
      (match (init-children start-specs)
        [#(ok ,children)
         (profile-me)
         `#(ok ,(<supervisor-state> make
                  [strategy strategy]
                  [intensity intensity]
                  [period period]
                  [children children]
                  [restarts '()]))]
        [,other other])]))

  (define (handle-call msg from state)
    (match msg
      [#(start-child ,child-spec)
       (match (check-child-spec child-spec)
         [#(ok ,child)
          (handle-start-child child state)]
         [,reason
          (profile-me)
          `#(reply #(error ,reason) ,state)])]
      [#(restart-child ,name)
       (match (state-find-child name state)
         [,(child <= `(<child> [pid #f]))
          (match (do-start-child child ($state children))
            [#(ok ,pid ,child)
             `#(reply #(ok ,pid) ,(state-replace-child child state))]
            [,error
             (profile-me)
             `#(reply ,error ,state)])]
         [`(<child>)
          (profile-me)
          `#(reply #(error running) ,state)]
         [#f
          (profile-me)
          `#(reply #(error not-found) ,state)])]
      [#(delete-child ,name)
       (match (state-find-child name state)
         [,(child <= `(<child> [pid #f]))
          `#(reply ok ,(state-remove-child child state))]
         [`(<child>)
          (profile-me)
          `#(reply #(error running) ,state)]
         [#f
          (profile-me)
          `#(reply #(error not-found) ,state)])]
      [#(terminate-child ,name)
       (match (state-find-child name state)
         [,(child <= `(<child>))
          `#(reply ok ,(state-replace-child (do-terminate child) state))]
         [#f
          (profile-me)
          `#(reply #(error not-found) ,state)])]
      [get-children
       (profile-me)
       `#(reply ,($state children) ,state)]))

  (define (handle-cast msg state) (match msg))

  (define (handle-info msg state)
    (match msg
      [`(EXIT ,pid ,reason ,err)
       (match (restart-child pid reason err state)
         [#(ok ,new-state)
          (profile-me)
          `#(no-reply ,new-state)]
         [#(shutdown ,new-state)
          (profile-me)
          `#(stop shutdown ,new-state)])]))

  (define (terminate reason state)
    (terminate-children ($state children))
    'ok)

  (define (init-children start-specs)
    (match (check-start-specs start-specs)
      [#(ok ,children)
       (match (start-children children)
         [#(ok ,new-children)
          (profile-me)
          `#(ok ,new-children)]
         [#(error ,new-children)
          (terminate-children new-children)
          `#(stop shutdown)])]
      [,reason
       (profile-me)
       `#(stop #(start-specs ,reason))]))

  (define (supervisor:validate-start-specs specs)
    (match (check-start-specs specs)
      [#(ok ,_) #f]
      [,reason reason]))

  (define (check-start-specs specs)
    (let lp ([specs specs] [children '()])
      (match specs
        [() `#(ok ,(reverse children))]
        [(,spec . ,rest)
         (match (check-child-spec spec)
           [#(ok ,(child <= `(<child> [name ,name])))
            (if (find-child name children)
                (begin
                  (profile-me)
                  `#(duplicate-child-name ,name))
                (lp rest (cons child children)))]
           [,reason reason])])))

  (define (check-child-spec spec)
    (match spec
      [#(,name ,thunk ,restart-type ,shutdown ,type)
       (or (and (not (symbol? name))
                `#(invalid-name ,name))
           (and (not (procedure? thunk))
                `#(invalid-thunk ,thunk))
           (and (not (memq restart-type
                       '(permanent temporary transient watch-only)))
                `#(invalid-restart-type ,restart-type))
           (and (not (memq type '(supervisor worker)))
                `#(invalid-type ,type))
           (and (not (valid-shutdown? shutdown type))
                `#(invalid-shutdown ,shutdown))
           `#(ok ,(<child> make
                    [pid #f]
                    [name name]
                    [thunk thunk]
                    [restart-type restart-type]
                    [shutdown shutdown]
                    [type type])))]
      [,_
       (profile-me)
       `#(invalid-child-spec ,spec)]))

  (define (valid-shutdown? shutdown type)
    (or (and (fixnum? shutdown) (> shutdown 0))
        (and (eq? shutdown 'infinity) (eq? type 'supervisor))
        (eq? shutdown 'brutal-kill)))

  (define (start-children children)
    (let lp ([children children] [new-children '()])
      (match children
        [() `#(ok ,new-children)]
        [(,child . ,rest)
         (match (do-start-child child new-children)
           [#(ok ,pid ,child)
            (lp rest (cons child new-children))]
           [#(error ,reason)
            `#(error ,(append (reverse rest) (cons child new-children)))])])))

  (define (report-start-error reason err child)
    (report-error 'start-error reason err child)
    `#(error ,reason))

  (define (do-start-child child children)
    (match (try ((<child> thunk child)))
      [#(ok ,pid) (guard (process? pid))
       (cond
        [(find-pid pid children)
         (profile-me)
         (report-start-error `#(duplicate-process ,pid) #f child)]
        [else
         (link pid)
         (let ([child (<child> copy child [pid pid])])
           (report-start child)
           `#(ok ,pid ,child))])]
      [ignore
       (profile-me)
       `#(ok #f ,child)]
      [#(error ,reason)
       (profile-me)
       (report-start-error reason #f child)]
      [`(catch ,reason ,err)
       (profile-me)
       (report-start-error reason err child)]
      [,other
       (profile-me)
       (report-start-error `#(bad-return-value ,other) #f child)]))

  (define (handle-start-child child state)
    (match (state-find-child (<child> name child) state)
      [#f
       (match (do-start-child child ($state children))
         [#(ok ,pid ,child)
          `#(reply #(ok ,pid)
              ,($state copy* [children (cons child children)]))]
         [#(error ,reason)
          (profile-me)
          `#(reply #(error ,reason) ,state)])]
      [`(<child> [pid #f])
       (profile-me)
       `#(reply #(error already-present) ,state)]
      [`(<child> [pid ,pid])
       (profile-me)
       `#(reply #(error #(already-started ,pid)) ,state)]))

  (define (restart-child pid reason err state)
    (cond
     [(state-find-pid pid state) =>
      (lambda (child)
        (do-restart (<child> restart-type child) reason err child state))]
     [else
      (profile-me)
      `#(ok ,state)]))

  (define (do-restart restart-type reason err child state)
    (report-end child 0 reason err)
    (match restart-type
      [permanent
       (restart child state)]
      [temporary
       `#(ok ,(state-mark-child-dead child state))]
      [transient
       (if (memq reason '(normal shutdown))
           `#(ok ,(state-mark-child-dead child state))
           (restart child state))]
      [watch-only
       `#(ok ,(state-remove-child child state))]))

  (define (restart child state)
    (match (add-restart state)
      [#(ok ,new-state)
       (strategic-restart child new-state)]
      [#(terminate ,new-state)
       (report-error 'shutdown 'reached-max-restart-intensity #f child)
       `#(shutdown ,(state-remove-child child new-state))]))

  (define (strategic-restart child state)
    (match ($state strategy)
      [one-for-one
       (match (do-start-child child ($state children))
         [#(ok ,pid ,child)
          `#(ok ,(state-replace-child child state))]
         [#(error ,reason)
          (restart child state)])]
      [one-for-all
       (match (start-children
               (terminate-children
                (mark-child-dead (<child> pid child) ($state children))))
         [#(ok ,new-children)
          (profile-me)
          `#(ok ,($state copy [children new-children]))]
         [#(error ,new-children)
          (restart child ($state copy [children new-children]))])]))

  (define (terminate-children children)
    (let lp ([children children] [acc '()])
      (match children
        [() acc]
        [(,child . ,rest) (lp rest (cons (do-terminate child) acc))])))

  (define (do-terminate child)
    (cond
     [(<child> pid child) =>
      (lambda (pid)
        (let-values ([(reason err) (shutdown pid (<child> shutdown child))])
          (report-end child 1 reason err)
          (<child> copy child [pid #f])))]
     [else child]))

  (define (shutdown pid x)
    (match (monitor-child pid)
      [ok
       (match x
         [brutal-kill
          (kill pid 'kill)
          (receive
           [`(DOWN ,_ ,@pid ,reason ,err)
            (values reason err)])]
         [,timeout
          (kill pid 'shutdown)
          (receive (after timeout
                     (kill pid 'kill)
                     (receive
                      [`(DOWN ,_ ,@pid ,reason ,err)
                       (values reason err)]))
            [`(DOWN ,_ ,@pid ,reason ,err)
             (values reason err)])])]
      [#(error ,reason ,err)
       (values reason err)]))

  (define (monitor-child pid)
    (monitor pid)
    (unlink pid)
    (receive (after 0 'ok)
      [`(EXIT ,@pid ,reason)
       (receive
        [`(DOWN ,_ ,@pid ,_ ,err)
         (profile-me)
         `#(error ,reason ,err)])]))

  (define (state-mark-child-dead child state)
    ($state copy*
      [children (mark-child-dead (<child> name child) children)]))

  (define (mark-child-dead who children)
    (match children
      [(,ch . ,chs)
       (if (or (eq? who (<child> name ch))
               (eq? who (<child> pid ch)))
           (cons (<child> copy ch [pid #f]) chs)
           (cons ch (mark-child-dead who chs)))]
      [() '()]))

  (define (state-find-child name state)
    (find-child name ($state children)))

  (define (find-child name children)
    (find (lambda (c) (eq? name (<child> name c))) children))

  (define (state-find-pid pid state)
    (find-pid pid ($state children)))

  (define (find-pid pid children)
    (find (lambda (c) (eq? pid (<child> pid c))) children))

  (define (state-replace-child child state)
    ($state copy* [children (replace-child child children)]))

  (define (replace-child child children)
    (match children
      [(,ch . ,chs)
       (if (eq? (<child> name ch) (<child> name child))
           (cons child chs)
           (cons ch (replace-child child chs)))]))

  (define (state-remove-child child state)
    ($state copy*
      [children (remp (lambda (c) (eq? (<child> name c) (<child> name child)))
                  children)]))

  (define (add-restart state)
    (let* ([now (erlang:now)]
           [r (prune-restarts (cons now ($state restarts)) now ($state period))]
           [new-state ($state copy [restarts r])])
      (if (<= (length r) ($state intensity))
          (begin
            (profile-me)
            `#(ok ,new-state))
          (begin
            (profile-me)
            `#(terminate ,new-state)))))

  (define (prune-restarts restarts now period)
    (match restarts
      [(,r . ,restarts)
       (if (<= (- now r) period)
           (cons r (prune-restarts restarts now period))
           '())]
      [() '()]))

  (define (report-error err-context reason err child)
    (let-values ([(reason details) (normalize-exit-reason reason err)])
      (system-detail <supervisor-error>
        [supervisor self]
        [error-context err-context]
        [reason reason]
        [details details]
        [child-pid (<child> pid child)]
        [child-name (<child> name child)])))

  (define (report-start child)
    (system-detail <child-start>
      [supervisor self]
      [pid (<child> pid child)]
      [name (<child> name child)]
      [restart-type (<child> restart-type child)]
      [shutdown (<child> shutdown child)]
      [type (<child> type child)]))

  (define (report-end child killed reason err)
    (let-values ([(reason details) (normalize-exit-reason reason err)])
      (system-detail <child-end>
        [pid (<child> pid child)]
        [killed killed]
        [reason reason]
        [details details])))
  )
