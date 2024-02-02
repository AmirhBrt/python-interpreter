#lang racket

(require "interpreter.rkt")
(require "passes/parser.rkt")


(evaluate "./Tests/in/in1.txt")

(provide (all-defined-out))
