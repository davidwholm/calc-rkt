#lang racket

(require "scope.rkt")

(provide interpret)

(define (interpret-expr expr env)
  (match expr
    [`(plus ,e1 ,e2) (+ (interpret-expr e1 env) (interpret-expr e2 env))]
    [`(minus ,e1 ,e2) (- (interpret-expr e1 env) (interpret-expr e2 env))]
    [`(times ,e1 ,e2) (* (interpret-expr e1 env) (interpret-expr e2 env))]
    [`(divide ,e1 ,e2) (/ (interpret-expr e1 env) (interpret-expr e2 env))]
    [`(number ,e) e]
    [`(string ,e) e]
    [`(truth ,e) e]
    [`(less-than ,e1 ,e2) (< (interpret-expr e1 env) (interpret-expr e2 env))]
    [`(greater-than ,e1 ,e2) (> (interpret-expr e1 env) (interpret-expr e2 env))]
    [`(not ,e) (not (interpret-expr e env))]
    [`(equal ,e1 ,e2) (equal? (interpret-expr e1 env)
                              (interpret-expr e2 env))]
    [`(variable ,id) (or (get-binding env id)
                         (error 'interpret-expr
                                "unbound variable '~a'"
                                id))]))

(define (interpret-stmt stmt env)
  (match stmt
    [`(print ,expr) (printf "~a" (interpret-expr expr env))]
    [`(println ,expr) (printf "~a\n" (interpret-expr expr env))]
    [`(assign ,id ,expr) (set-binding! env id (interpret-expr expr env))]
    [`(if ,expr ,stmts1) (when (interpret-expr expr env)
                           (interpret stmts1 (enter-scope #:parent env)))]
    [`(while ,expr ,stmts1) (when (interpret-expr expr env)
                              (interpret stmts1 (enter-scope #:parent env))
                              (interpret-stmt stmt env))]))

(define (interpret stmts [env (scope #f (make-hash))])
  (for ([stmt stmts])
    (interpret-stmt stmt env)))
