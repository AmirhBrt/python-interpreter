#lang racket

(require (lib "eopl.ss" "eopl"))
(require "store.rkt")

(define-datatype statement statement?
  (assign 
    (var string?) 
    (expr expression?))
  (global 
    (var string?))
  (return 
    (expr expression?))
  (return_void)
  (pass)
  (break)
  (continue)
  (func 
    (name string?) 
    (params func_param*?) 
    (statements list?))
  (if_stmt 
    (cond_exp expression?) 
    (if_sts list?) 
    (else_sts list?))
  (for_stmt 
    (iter string?) 
    (list_exp expression?) 
    (sts list?))
  (print_stmt 
    (expressions expression*?)))

(define-datatype func_param func_param?
  (with_default 
    (var string?) 
    (expr expression?)))

(define-datatype func_param* func_param*?
  (empty-param)
  (func_params 
    (param func_param?) 
    (rest-params func_param*?)))

(define-datatype expression expression?
  (mult_op
    (left expression?)
    (right expression?))
  (binary_op 
    (op procedure?) 
    (left expression?) 
    (right expression?))
  (unary_op 
    (op procedure?) 
    (operand expression?))
  (function_call 
    (func expression?) 
    (params expression*?))
  (list_ref 
    (ref expression?) 
    (index expression?))
  (ref (var string?))

  (atomic_bool_exp 
    (bool boolean?))
  (atomic_num_exp 
    (num number?))
  (atomic_null_exp)
  (atomic_list_exp 
    (l expression*?)))

(define-datatype expression* expression*?
  (empty-expr)
  (expressions 
    (expr expression?) 
    (rest-exprs expression*?)))

(define-datatype environment environment?
  (empty-environment)
  (extend-environment
    (var string?)
    (val expval?)
    (env environment?)))


(define-datatype expval expval?
  (empty-val)
  (num-val 
    (num number?))
  (bool-val   
    (flag boolean?))
  (array-val  
    (array array?))
  (ref-val
    (ref reference?)))

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (bool-val (bool) 
        (if bool 1 0))
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

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


(define-datatype array array?
  (empty-array)
  (a-array
    (first expval?)
    (rest array?)))

(define make-array
  (lambda (expvals)
    (cond 
      [(null? expvals) 
        (empty-array)]
      [else 
        (a-array
          (car expvals)
          (make-array (cdr expvals)))])))

(define array-ref
  (lambda (arr idx)
    (letrec ((array-ref-rec (lambda (arr i)
                              (cases array arr
                                (a-array (car-loc cdr-loc)
                                         (if (zero? i)
                                             (deref car-loc)
                                             (array-ref-rec cdr-loc (- i 1))))
                                (else (array-out-of-bound-error idx))))))
      (array-ref-rec arr idx))))

(define array-set
  (lambda (arr idx val)
    (letrec ((array-set-rec (lambda (arr i)
                              (cases array arr
                                (a-array (car-loc cdr-loc)
                                         (if (zero? i)
                                             (setref! car-loc val)
                                             (array-set-rec cdr-loc (- i 1))))
                                (else (array-out-of-bound-error idx))))))
      (array-set-rec arr idx))))

(define array-out-of-bound-error
  (lambda (value)
    (eopl:error 'arr-extractor "Array index out of bound. index ~s" value)))


(define reference?
  (lambda (v)
    (integer? v)))

(provide (all-defined-out))
(#%provide (all-defined))