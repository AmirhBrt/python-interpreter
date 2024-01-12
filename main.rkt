#lang racket

(require "interpreter.rkt")
(require "passes/parser.rkt")


(evaluate "./test.py")

(provide (all-defined-out))
