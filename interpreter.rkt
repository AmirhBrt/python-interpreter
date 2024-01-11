#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes/all.rkt")
(require "environment.rkt")
(require "passes/parser.rkt")
(require "print.rkt")


(define (interpret parse-tree)
  (begin 
    (initialize-store!)
    (value-of-statements parse-tree (init-env))
    (display "")))

(define (evaluate file-name)
  (interpret (evaluate-parser file-name)))

(define value-of-statements
  (lambda (parse-tree env)
    (cond
      [(null? parse-tree) (list (empty-val) env)]
      [(= 1 (length parse-tree)) (value-of-statement (car parse-tree) env)]
      [else (letrec ([rev (reverse parse-tree)]
                     [car-rev (car rev)]
                     [cdr-rev (reverse (cdr rev))])
            (value-of-statement car-rev (cadr (value-of-statements cdr-rev env))))])))


(define value-of-statement
  (lambda (stmt env)
    (cases statement stmt 
      (assign (var expr) 
        (letrec ([val (car (value-of-expression expr env))]
                [ref (newref val)]
                [new-env (extend-environment var (ref-val ref) env)])
          (cons (empty-val) (list new-env))))

      (print_stmt (exps) 
        (begin 
          (print-vals (get-exp-vals exps env))
          (cons (empty-val) (list env))))

      (if_stmt (exp if_sts else_sts)
        (letrec 
          ([calc_sts 
            (lambda (statements env) 
                (cond
                  [(null? statements) (list (empty-val) env)]
                  [else (calc_sts (cdr statements) 
                                  (cadr (value-of-statement (car statements) env)))]))])
          (cond
            [(expval->bool (car (value-of-expression exp env))) (calc_sts if_sts env)]
            [else (calc_sts else_sts env)])))

      (else (eopl:error "NAJAFI\n")))))



(define value-of-expression 
  (lambda (exp env) 
    (cases expression exp
      (mult_op (left right) 
        (let ([expval1 (car (value-of-expression left env))])
          (if 
            (= 0 (expval->num expval1))
            (list (num-val 0) env)
            (let ([expval2 (car (value-of-expression right env))])
              (list (num-val (* (expval->num expval1) (expval->num expval2))) env)))))
      (binary_op (op left right)
        (let ([expval1 (car (value-of-expression left  env))]
              [expval2 (car (value-of-expression right env))])
          (let ([val1 (expval->array_or_num_or_bool expval1)]
                [val2 (expval->array_or_num_or_bool expval2)])
            (let ([ans (op val1 val2)])
              (cond 
                [(boolean? ans) (list (bool-val ans) env)]
                [(number? ans) (list (num-val ans) env)]
                [else (list (array-val ans) env)])))))
      (unary_op (op exp)
        (let ([expval (car (value-of-expression exp  env))])
          (let ([val (expval->num expval)])
            (list (num-val (op 0 val)) env))))
      (list_ref (ref idx)
        (let ([arr (expval->array (car (value-of-expression ref env)))]
              [idx (expval->num (car (value-of-expression idx env)))])
          (list (array-ref arr idx) env)))
      (atomic_bool_exp (val) 
        (list (bool-val val) env))
      (atomic_num_exp (val) 
        (list (num-val val) env))
      (atomic_null_exp () 
        (list (empty-val) env))
      (atomic_list_exp (exps) 
        (list (array-val (make-array (get-exp-vals exps env))) env))
      (ref (var) 
        (list (deref (expval->ref (apply-env env var))) env))
      (else (eopl:error "BARATI\n")))))

(define get-exp-vals
  (lambda (exps env)
    (cases expression* exps
      (empty-expr  ()
        (list))
      (expressions (expr rest-exprs)
        (append 
          (get-exp-vals rest-exprs env)
          (list (car (value-of-expression expr env))))))))

(provide (all-defined-out))