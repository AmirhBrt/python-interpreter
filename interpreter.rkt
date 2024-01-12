#lang racket

(require (lib "eopl.ss" "eopl"))

(require "datatypes/all.rkt")
(require "passes/parser.rkt")
(require "utils/environment.rkt")


(define (evaluate file-name)
  (interpret (evaluate-parser file-name)))

(define (interpret parse-tree)
  (begin 
    (initialize-store!)
    (value-of-statements parse-tree (init-env))
    (display "")))


(define value-of-statements
  (lambda (sts env)
    (cond
      [(null? sts) (list (empty-val) env)]
      [(= 1 (length sts)) (value-of-statement (car sts) env)]
      [else 
        (let ([first-statement-val (value-of-statement (car sts) env)])
          (if (or 
                (equal? (car first-statement-val) (break-val))
                (equal? (car first-statement-val) (continue-val))
                (equal? (car first-statement-val) (return-val))
                (and (= 3 (length first-statement-val)) 
                    (equal? (caddr first-statement-val) (return-val))))
              first-statement-val
              (value-of-statements (cdr sts) (cadr first-statement-val))))])))

(define value-of-statement
  (lambda (stmt env)
    (cases statement stmt 
      (assign (var expr) (let ([val (lazy-eval expr env)])
        (cond            
          [(is-global env var) 
              (begin              
                (setref! (expval->ref (apply-env env var)) val)
                (list (empty-val) env))]
          [else (let ([new-env (extend-environment var (ref-val (newref val)) env)])
                        (list (empty-val) new-env))])))

      (print_stmt (exps) 
        (begin 
          (print-vals (get-exp-vals exps env))
          (list (empty-val) env)))

      (if_stmt (exp if_sts else_sts)
        (let ([val (car (value-of-expression exp env))])
          (cond
            [(expval->bool val) (value-of-statements if_sts env)]
            [else (value-of-statements else_sts env)])))

      (pass () 
        (list (empty-val) env))
      (continue () 
        (list (continue-val) env))
      (break () 
        (list (break-val) env))
      
      (for_stmt (iter array-exp sts)
        (let ([ls (get-array-as-list (expval->array (car (value-of-expression array-exp env))))]
              [new-env (if (is-in-env env iter) 
                            env 
                            (extend-environment iter (ref-val (newref 0)) env))])
          (value-of-for-statement iter ls sts new-env)))

      (func (name params sts)
        (let ([ref (newref (func-val (normal-function name params sts)))])
          (list (empty-val) (extend-environment name (ref-val ref) env))))

      (return_void () 
        (list (empty-val) env (return-val)))
      (return (exp) 
        (let ([result (value-of-expression exp env)])
          (append result (list (return-val)))))

      (global (var) 
        (list (empty-val) (extend-environment-global var (apply-env env var) env))))))

(define value-of-for-statement
  (lambda (iter expval-ls sts env)
    (cond
      [(null? expval-ls) (list (empty-val) env)]
      [else (begin
              (setref! (expval->ref (apply-env env iter)) (car expval-ls))
              (let ([for-statement-val (value-of-statements sts env)])
                (if 
                  (equal? (break-val) (car for-statement-val))
                  (list (empty-val) (cadr for-statement-val))
                  (value-of-for-statement iter (cdr expval-ls) sts (cadr for-statement-val)))))])))

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
      (ref (var) (list (letrec ([ref2 (expval->ref (apply-env env var))]
                              [val (deref ref2)])
                                  (if (expval? val)
                                  val
                                  (let ([val-thunk (car (value-of-thunk val))])
                                        (begin                                          
                                          (setref! ref2 val-thunk)              
                                          val-thunk
                                        ))))
                        env))

      (function_call (func_name params) 
        (let ([params_value (get-array-as-list (make-array (get-exp-vals params env)))])
          (value-of-func-call params_value func_name params env))))))

(define value-of-func-call 
  (lambda (params_value func_name params env)
    (cases expression func_name
      (ref (var) (let ([func (expval->func (deref (expval->ref (apply-env env var))))])
        (cases function func
          (normal-function (name defualt_params sts) 
            (letrec ([assign-params
                  (lambda (params defualt_params)
                        (cases func_param* defualt_params
                          (empty-param () (list (empty-val) env params))
                          (func_params (first rest)
                            (cases func_param first
                              (with_default (var2 exp)
                              (letrec ([result (assign-params params rest)]
                                    [new-env (cadr result)]
                                    [new-params (caddr result)])
                                  (cond 
                                  [(null? new-params) (begin
                                    (let ([ref (newref (lazy-eval exp env))])
                                        (list (empty-val) (extend-environment var2 (ref-val ref) new-env) new-params))
                                  )]
                                  [else  (let ([ref (newref (car new-params))])
                                        (list (empty-val) (extend-environment var2 (ref-val ref) new-env) (cdr new-params)))]
                                  ))))))
                    )]) 
                (let ([new-env (cadr (assign-params params_value defualt_params))])                               
                      (list (car (value-of-statements sts new-env)) env)))
                      ))))

      (else (eopl:error "the function name must be a variable\n")))))

(define value-of-thunk
  (lambda (t) (cases lazy t
    (lazy-eval (body env)     
      (value-of-expression body env)))))

(define get-exp-vals
  (lambda (exps env)
    (cases expression* exps
      (empty-expr  ()
        (list))
      (expressions (expr rest-exprs)
        (append 
          (get-exp-vals rest-exprs env)
          (list (lazy-eval expr env)))))))


(define print-vals
  (lambda (vals) 
    (cond
      [(null? vals) (display "\n")]
      [else (begin              
              (print-val (car vals))
              (if (not (null? (cdr vals))) (display " ") (display ""))
              (print-vals (cdr vals)))])))

(define print-val
  (lambda (v)
    (let ([val (if (expval? v) v (car (value-of-thunk v)))]) 
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
        (else (eopl:error "Invalid expval type"))))))

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
