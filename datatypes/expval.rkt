#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "array.rkt")

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      ;;; (bool-val (bool) 
      ;;;   (if bool 1 0))
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->ref
  (lambda (v)
    (cases expval v
      (ref-val (ref) ref)
      (else (expval-extractor-error 'reference v)))))

(define expval->array
  (lambda (v)
    (cases expval v
      (array-val (arr) arr)
      (else (expval-extractor-error 'array v)))))

(define expval->func
  (lambda (v)
    (cases expval v
      (func-val (func) func)
      (else (expval-extractor-error 'func v)))))

(define expval->array_or_num_or_bool
  (lambda (v)
    (cases expval v
      (array-val (arr) arr)
      (num-val (num) num)
      (bool-val (bool) bool)
      (else (expval-extractor-error 'array_or_num_or_bool v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

(provide (all-defined-out))
