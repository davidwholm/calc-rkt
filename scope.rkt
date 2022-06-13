#lang racket

(provide scope
         enter-scope
         get-binding
         set-binding!)

(struct scope
  [parent bindings]
  #:transparent)

(define (enter-scope #:parent parent-scope)
  (scope parent-scope (make-hash)))

(define (get-binding scp id)
  (hash-ref (scope-bindings scp) id (λ ()
                                      (and (scope-parent scp)
                                           (get-binding (scope-parent scp) id)))))

(define (set-binding! scp id value)
  (if (get-binding scp id)
      (cond
        [(hash-has-key? (scope-bindings scp) id) (hash-set! (scope-bindings scp) id value)]
        [else (set-binding! (scope-parent scp) id value)])
      (hash-set! (scope-bindings scp) id value)))
