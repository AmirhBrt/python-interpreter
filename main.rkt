#lang racket

(require "parser.rkt")
(require "lexer.rkt")

(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (evaluate file-name)
  (parse-scan (string-join (file->lines file-name)))
  )

(evaluate "./test.py")

(provide (all-defined-out))
