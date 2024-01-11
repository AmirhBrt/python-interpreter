#lang racket

(require "interpreter.rkt")

(evaluate "./test2.py")

(provide (all-defined-out))
