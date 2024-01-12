#lang racket

(require "interpreter.rkt")
(require "passes/parser.rkt")

;;; (evaluate-parser "./test.py")
(evaluate "./test.py")

(provide (all-defined-out))
