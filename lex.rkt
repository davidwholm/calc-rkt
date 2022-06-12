#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide some-lexer
         empty-tokens
         tokens)

(define-empty-tokens empty-tokens (PLUS MINUS STAR SLASH EQUAL EQUAL-EQUAL LESS-THAN GREATER-THAN SEMICOLON LEFT-BRACE RIGHT-BRACE LEFT-PAREN RIGHT-PAREN PRINT PRINTLN WHILE IF TRUE FALSE BANG NEG EOF))
(define-tokens tokens (IDENTIFIER NUMBER STRING))

(define-lex-abbrevs
  [%number (:or #\0 (:: (:/ #\1 #\9) (:* numeric)))]
  [%identifier (:: alphabetic (:* (:or alphabetic
                                       numeric
                                       #\_)))]
  [%string (:: #\" (:* (:~ #\")) #\")])

(define some-lexer
  (lexer-src-pos
   [#\+ (token-PLUS)]
   [#\- (token-MINUS)]
   [#\* (token-STAR)]
   [#\/ (token-SLASH)]
   [#\= (token-EQUAL)]
   [#\; (token-SEMICOLON)]
   [#\{ (token-LEFT-BRACE)]
   [#\} (token-RIGHT-BRACE)]
   [#\( (token-LEFT-PAREN)]
   [#\) (token-RIGHT-PAREN)]
   [#\! (token-BANG)]
   [#\< (token-LESS-THAN)]
   [#\> (token-GREATER-THAN)]
   ["==" (token-EQUAL-EQUAL)]
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   ["if" (token-IF)]
   ["while" (token-WHILE)]
   ["print" (token-PRINT)]
   ["println" (token-PRINTLN)]
   [%number (token-NUMBER (string->number lexeme))]
   [%identifier (token-IDENTIFIER (string->symbol lexeme))]
   [%string (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
   [whitespace (return-without-pos (some-lexer input-port))]
   [(eof) (token-EOF)]
   [any-char (error 'lexer
                    "unrecognized character '~a' found on line ~a column ~a"
                    lexeme
                    (position-line start-pos)
                    (position-col start-pos))]))
