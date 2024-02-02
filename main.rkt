#lang racket

(require "interpreter.rkt")
(require "passes/parser.rkt")


(evaluate "./Tests/in/in2.txt")

(provide (all-defined-out))
