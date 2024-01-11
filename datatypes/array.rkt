#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")

(define make-array
  (lambda (expvals)
      (cond 
        [(null? expvals) 
          (empty-array)]
        [else 
          (a-array
            (car expvals)
            (make-array (cdr expvals)))])))

(define get-array-as-list
  (lambda (arr)
    (cases array arr
      (empty-array () (list))
      (a-array (first rest)
        (cons first (get-array-as-list rest))))))

(define append-array
  (lambda (array1 array2)
    (make-array 
      (append 
        (get-array-as-list array1) 
        (get-array-as-list array2)))))

(define array-ref
  (lambda (arr idx)
    (letrec ((array-ref-rec (lambda (arr i)
                              (cases array arr
                                (a-array (first rest)
                                         (if (zero? i)
                                             first
                                             (array-ref-rec rest (- i 1))))
                                (else (array-index-out-of-bound-error idx))))))
      (array-ref-rec arr idx))))

(define array-set
  (lambda (arr idx val)
    (letrec ((array-set-rec (lambda (arr i)
                              (cases array arr
                                (a-array (first rest)
                                         (if (zero? i)
                                             first
                                             (array-set-rec rest (- i 1))))
                                (else (array-index-out-of-bound-error idx))))))
      (array-set-rec arr idx))))

(define array-index-out-of-bound-error
  (lambda (value)
    (eopl:error 'arr-extractor "Array index out of bound. index ~s" value)))

(provide (all-defined-out))
