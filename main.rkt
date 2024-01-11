#lang racket

(require "parser.rkt")
(require "lexer.rkt")
(require "interpreter.rkt")
(require "environment.rkt")
(require "store.rkt")

(define (parse-scan prog-string)
  (python-parser (lex-this prog-string)))

(define (evaluate file-name)
  (parse-scan (string-join (file->lines file-name))))

(define (interpreter file-name)
  (begin
  (initialize-store!)
  (value-of-statements (evaluate file-name) (init-env))))

(interpreter "./test2.py")

(provide (all-defined-out))
