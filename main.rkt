#lang racket

(require "interpreter.rkt")

(evaluate "./test.py")

(provide (all-defined-out))
