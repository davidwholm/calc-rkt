#lang racket

(require "lex.rkt"
         parser-tools/yacc
         parser-tools/lex)

(provide parse)

(define some-parser
  (parser
   [start statements]
   [end EOF]
   [tokens tokens empty-tokens]
   [src-pos]
   [error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (error 'parser
                   "unexpected token '~a' found on line ~a column ~a"
                   tok-name
                   (position-line start-pos)
                   (position-col start-pos)))]
   [precs (nonassoc EQUAL-EQUAL LESS-THAN GREATER-THAN BANG)
          (left PLUS MINUS)
          (left STAR SLASH)
          (left NEG)]
   [grammar
    (statements [(statement statements) (cons $1 $2)]
                [() '()])
    (statement [(PRINT expression SEMICOLON) `(print ,$2)]
               [(PRINTLN expression SEMICOLON) `(println ,$2)]
               [(IDENTIFIER EQUAL expression SEMICOLON) `(assign ,$1 ,$3)]
               [(IF expression LEFT-BRACE statements RIGHT-BRACE) `(if ,$2 ,$4)]
               [(WHILE expression LEFT-BRACE statements RIGHT-BRACE) `(while ,$2 ,$4)])
    (expression [(expression PLUS expression) `(plus ,$1 ,$3)]
                [(expression MINUS expression) `(minus ,$1 ,$3)]
                [(expression STAR expression) `(times ,$1 ,$3)]
                [(expression SLASH expression) `(divide ,$1 ,$3)]
                [(expression EQUAL-EQUAL expression) `(equal ,$1 ,$3)]
                [(expression LESS-THAN expression) `(less-than ,$1 ,$3)]
                [(expression GREATER-THAN expression) `(greater-than ,$1 ,$3)]
                [(LEFT-PAREN expression RIGHT-PAREN) $2]
                [(BANG expression) `(not ,$2)]
                [(MINUS expression) (prec NEG) `(minus (number 0) ,$2)]
                [(NUMBER) `(number ,$1)]
                [(STRING) `(string ,$1)]
                [(TRUE) `(truth #t)]
                [(FALSE) `(truth #f)]
                [(IDENTIFIER) `(variable ,$1)])]))

(define (parse ip)
  (port-count-lines! ip)
  (some-parser (λ ()
                 (some-lexer ip))))
