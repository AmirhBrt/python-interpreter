#lang racket

(require "interpreter.rkt")
(require "passes/parser.rkt")


(evaluate "./Tests/in/in14.txt")

(provide (all-defined-out))
