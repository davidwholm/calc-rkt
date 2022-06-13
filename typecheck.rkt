#lang racket

(require "scope.rkt")

(provide typecheck)

(define (typecheck-expr expr env)
  (match expr
    [`(,arith-oper ,e1 ,e2)
     #:when (member arith-oper '(plus minus times divide))
     (let ([type-e1 (typecheck-expr e1 env)]
           [type-e2 (typecheck-expr e2 env)])
       (if (not (and (eq? type-e1 'number)
                     (eq? type-e2 'number)))
           (error 'typecheck-expr
                  "type error in '~a ~a ~a'" e1 arith-oper e2)
           'number))]
    [`(number ,_) 'number]
    [`(truth ,_) 'truth]
    [`(string ,_) 'string]
    [`(equal ,e1 ,e2) (let ([type-e1 (typecheck-expr e1 env)]
                            [type-e2 (typecheck-expr e2 env)])
                        (if (not (eq? type-e1 type-e2))
                            (error 'typecheck-expr
                                   "type error in '~a' and '~a'" e1 e2)
                            'truth))]
    [`(,comp-oper ,e1 ,e2)
     #:when (member comp-oper '(less-than greater-than))
     (let ([type-e1 (typecheck-expr e1 env)]
           [type-e2 (typecheck-expr e2 env)])
       (if (not (and (eq? type-e1 'number)
                     (eq? type-e2 'number)))
           (error 'typecheck-expr
                  "type error in '~a ~a ~a'" e1 comp-oper e2)
           'truth))]
    [`(not ,e) (if (not (eq? (typecheck-expr e env) 'truth))
                   (error 'typecheck-expr
                          "type error in '~a'" e)
                   'truth)]
    [`(variable ,id) (or (get-binding env id)
                         (error 'typecheck-expr
                                "unbound variable '~a'"
                                id))]))

(define (typecheck-stmt stmt env)
  (match stmt
    [`(print ,expr) (typecheck-expr expr env)]
    [`(println ,expr) (typecheck-expr expr env)]
    [`(assign ,id ,expr) (set-binding! env id (typecheck-expr expr env))]
    [`(if ,expr ,stmts1) (if (eq? (typecheck-expr expr env) 'truth)
                             (typecheck stmts1 (enter-scope #:parent env))
                             (error 'typecheck-stmt
                                    "type error in '~a'"
                                    expr))]
    [`(while ,expr ,stmts1) (if (eq? (typecheck-expr expr env) 'truth)
                                (typecheck stmts1 (enter-scope #:parent env))
                                (error 'typecheck-stmt
                                       "type error in '~a'"
                                       expr))]))

(define (typecheck stmts [env (scope #f (make-hash))])
  (for ([stmt stmts])
    (typecheck-stmt stmt env)))
