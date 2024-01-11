#lang racket
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")


(define print-vals
  (lambda (vals) 
    (cond
      [(null? vals)]
      [else (begin 
              (print-val (car vals))
              (display " ")
              (print-vals (cdr vals)))])))

(define print-val
  (lambda (v)
    (cases expval v
      (empty-val () (display "\n"))
      (num-val (num) (display num))
      (bool-val (bool) (display bool))
      (else (eopl:error "Invalid expval type")))))
     