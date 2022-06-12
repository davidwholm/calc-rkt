#lang racket

(require "parse.rkt"
         "interpret.rkt"
         "typecheck.rkt")

(module+ main
  (let* ([file-to-interpret (command-line
                            #:program "calc"
                            #:args (filename)
                            filename)]
         [ast (call-with-input-file file-to-interpret
                (λ (in)
                  (parse in)))])
    (typecheck ast)
    (interpret ast)))
