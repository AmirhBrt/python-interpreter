#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "environment.rkt")
(require "parser.rkt")
(require "store.rkt")

(define value-of-statements
  (lambda (parse-tree env)
    (cond
      [(null? parse-tree) (cons (empty-val) (list env))]
      [(= 1 (length parse-tree)) (value-of-statement (car parse-tree) env)]
      [else (letrec ([rev (reverse parse-tree)]
                     [car-rev (car rev)]
                     [cdr-rev (reverse (cdr rev))])
            (value-of-statement car-rev (cadr (value-of-statements cdr-rev env))))])))


(define value-of-statement
  (lambda (stmt env)
    (cases statement stmt 
      (assign (var expr) (letrec ([val (car (value-of-expression expr env))]
                                  [ref (newref val)]
                                  [new-env (extend-environment var (ref-val ref) env)])
                          (cons (empty-val) (list new-env))))

      (print_stmt (exps) 
        (letrec ([print_exps (lambda (exps) 
                              (cases expression* exps
                                (empty-expr () (print "\n"))
                                (expressions (expr rest-exprs)
                                              (begin 
                                                (print (car (value-of-expression expr env)))
                                                (print " ")
                                                (print_exps rest-exprs)
                                              ))))])
          (print_exps exps)))
            
      (else (eopl:error "FUCK\n")))))


;;; (define value-of-statement
;;;   ())

(define value-of-expression 
  (lambda (exp env) 
    (cases expression exp
      (mult_op (left right) 
        (let ([val1 (expval->num (car (value-of-expression left env)))]
              [val2 (expval->num (car (value-of-expression right env)))])
             (list (num-val (* val1 val2)) env)))
      (atomic_bool_exp (val) (list (bool-val val) env))
      (atomic_num_exp (val) (list (num-val val) env))
      (atomic_null_exp () (list (empty-val) env))

      (else (eopl:error "BARATI\n"))
    )
  )
)

(provide (all-defined-out))