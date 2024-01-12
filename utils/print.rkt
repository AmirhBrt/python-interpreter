#lang racket

(require (lib "eopl.ss" "eopl"))
(require "../datatypes/all.rkt")

(define print-vals
  (lambda (vals) 
    (cond
      [(null? vals) (display "\n")]
      [else (begin              
              (print-val (car vals))
              (if (not (null? (cdr vals))) (display " ") (display ""))
              (print-vals (cdr vals)))])))

(define print-val
  (lambda (val)
    (cases expval val
      (empty-val () 
        (display ""))
      (num-val (num) 
        (display num))
      (bool-val (bool)
        (if 
          bool 
          (display "True")
          (display "False")))
      (ref-val (ref) 
        (let 
          ([val (deref ref)])
          (display val)))
      (array-val (arr)
        (print-arr arr))
      (else (eopl:error "Invalid expval type")))))

(define print-arr
  (lambda (arr)
    (begin
      (display "\n")
      (display "[")
      (print-arr-elements arr)
      (display "]\n"))))

(define print-arr-elements
  (lambda (arr)
    (cases array arr
      (empty-array () (display ""))
      (a-array (first rest)
        (begin
          (cases array rest
            (empty-array () 
              (print-val first))
            (a-array (f r) 
              (begin
                (print-val first)
                (display " ,"))))
          (print-arr-elements rest))))))

(provide (all-defined-out))
