#lang racket

(require "parse.rkt"
         "interpret.rkt"
         "typecheck.rkt")

(define (interpret-file filename)
  (let ([ast (call-with-input-file filename
               (λ (in)
                 (parse in)))])
    (typecheck ast)
    (interpret ast)))

(module+ main
  (let ([file-to-interpret (command-line
                            #:program "calc"
                            #:args (filename)
                            filename)])
    (interpret-file file-to-interpret)))
