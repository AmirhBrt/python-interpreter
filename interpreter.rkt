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
    ;;; (display parse-tree)
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
  (begin
    ;;; (display "STMT:\n")
    ;;; (display stmt)
    ;;; (display "\n")
    (cases statement stmt 
      (assign (var expr) (let ([val (lazy-eval expr env)])
        (begin
        ;;; (display "gggggaklgnaklfgjaklfjklafjkldajfadfjklaj")
        (cond            
          [(is-global env var) 
              (begin                
                (setref! (expval->ref (apply-env env var)) val)
                (list (empty-val) env))]
          [else (let ([new-env (extend-environment var (ref-val (newref val)) env)])
                        (list (empty-val) new-env))])
        )        
        ))

      (print_stmt (exps) 
        (begin 
          ;;; (display (get-expvals exps env))
          (print-vals (get-expvals exps env))
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
      (begin
      ;;; (display "\nGLOBAL\n")
      ;;; (display var)
      (list (empty-val) (extend-environment-global var (apply-env env var) env)))))
      )
        ))

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
  (begin
    ;;; (display "EXPS:\n")
    ;;; (display exp)
    ;;; (display "\n")
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
                [(number? ans) (list (num-val (if (integer? ans) ans (exact->inexact ans))) env)]
                [else (list (array-val ans) env)])))))
      (unary_op (op exp)
        (let ([expval (car (value-of-expression exp  env))])
          (let ([val (expval->array_or_num_or_bool expval)])
            (let ([ans (op val)])
              (cond
                [(boolean? ans) (list (bool-val ans) env)]
                [(number? ans) (list (num-val (if (integer? ans) ans (exact->inexact ans))) env)]
                [else (list (array-val ans) env)])))))
      (list_ref (ref idx)
      (begin
      ;;; (display "--------NIMMMMMMM---------------------\n")
      ;;; (display (expval->array (car (value-of-expression ref env))))
      ;;; (display "\n--------NIMMMMMMM---------------------\n")      
        (let ([arr (expval->array (car (value-of-expression ref env)))]
              [idx (expval->num (car (value-of-expression idx env)))])
          (begin
          ;;; (display "--------NIdagkafjkajfMMMMMMM---------------------\n")
          ;;; (display (array-ref arr idx))
          ;;; (display "\n--------NfakfafkllkIMMMMMMM---------------------\n")
          (list (let ([vv (array-ref arr idx)]) (if (expval? vv) vv (car (value-of-thunk vv)))) env)
          )))
          )
      (atomic_bool_exp (val)
        (list (bool-val val) env))
      (atomic_num_exp (val)
      (begin 
        (let ([val2 (if (lazy? val) (car (value-of-thunk val)) (num-val val))])
        (list val2 env)))
      )
      (atomic_null_exp ()
        (list (empty-val) env))
      (atomic_list_exp (exps)
        (list (array-val (make-array (get-expvals exps env))) env))
      (ref (var) 
        (begin         
        ;;; (display "-----------------------------\n")
        ;;; (display var)
        ;;; (display "\n")
        ;;; (display (apply-env env var))
        ;;; (display "\n")
        ;;; (display (deref (expval->ref (apply-env env var))))
        ;;; (display "\n")
        ;;; (display "-----------------------------\n")
        (list (letrec ([ref2 (expval->ref (apply-env env var))]
                              [val (deref ref2)])
                                  (begin 
                                  ;;; (display "--------HHHHHHHHHL---------------------\n")
                                  ;;; (display val)
                                  ;;; (display "\n--------HHHHHHHHHL---------------------\n")
                                  (if (expval? val)
                                  val
                                  (letrec (                                  
                                    [val-thunk (car (value-of-thunk val))])
                                        (begin
                                        ;;; (display "--------HHHHHHHHHL---------------------\n")
                                        ;;; (display val)
                                        ;;; (display "\n--------HHHHHHHHHL---------------------\n")                                   
                                          (setref! ref2 val-thunk)              
                                          val-thunk
                                        )))
                                  )
                                  )
                        env)
        )
      )

      (function_call (func_name params) 
        (let ([params_value (get-array-as-list (make-array (get-expvals params env)))])
          (value-of-func-call params_value func_name params env)))))))

(define value-of-func-call 
  (lambda (params_value func_name params env)
    (cases expression func_name
      (ref (var) (letrec (
          [func3 (deref (expval->ref (apply-env env var))) ]
          [func (if (expval? func3) (expval->func func3) (expval->func (car (value-of-thunk func3))))]
      )
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
      (begin
      ;;; (display body)
      ;;; (display "\nthunk\n")
      
      (let ([val (value-of-expression body env)])
      (if (lazy? val) (value-of-thunk val) val))
      
      )))))

(define get-expvals
  (lambda (exps env)
    (cases expression* exps
      (empty-expr  ()
        (list))
      (expressions (expr rest-exprs)
        (append 
          (get-expvals rest-exprs env)
          (list (lazy-eval expr env)))))))


(define print-vals
  (lambda (vals) 
(begin
    ;;; (display "AK;LDGASGGKAJS:\n")
    ;;; (display vals)
    ;;; (display "\n")
    (cond
      [(null? vals) (display "\n")]
      [else (begin
              (print-val (car vals))
              (if (not (null? (cdr vals))) (display " ") (display ""))
              (print-vals (cdr vals)))]))))

(define print-val
  (lambda (v)
  (begin
    ;;; (display "\nNIMA\n")
    ;;; (display v)
    ;;; (display "\nFIN\n")
    (let ([val (if (expval? v) v (begin
      ;;; (display "\ntests\n")
      (car (value-of-thunk v))
    ))]) 
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
        (else (eopl:error "Invalid expval type")))))))

(define print-arr
  (lambda (arr)
    (begin
      (display "[")
      (print-arr-elements arr)
      (display "]"))))

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
                (display ", "))))
          (print-arr-elements rest))))))


(provide interpret evaluate)
