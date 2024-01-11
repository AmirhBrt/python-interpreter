#lang racket
(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")


(define print-vals
  (lambda (vals) 
    (cond
      [(null? vals) (display "\n")]
      [else (begin 
              (print-val (car vals))
              (print-vals (cdr vals)))])))

(define print-val
  (lambda (v)
    (cases expval v
      (empty-val () 
        (display ""))
      (num-val (num) 
        (begin 
          (display num)
          (display " ")
        ))
      (bool-val (bool)
        (if 
          bool 
          (display "True ")
          (display "False ")))
      (else (eopl:error "Invalid expval type")))))

(provide (all-defined-out))