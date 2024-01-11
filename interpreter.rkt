#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "environment.rkt")
(require "parser.rkt")
(require "store.rkt")

(define value-of-statements
  (lambda (parse-tree env)
    (cond
      [(null? parse-tree) (cons env (list (empty-val)))]
      [(= 1 (length parse-tree)) (value-of-statement (car parse-tree) env)]
      [else (letrec ([rev (reverse parse-tree)]
                     [car-rev (car rev)]
                     [cdr-rev (reverse (cdr rev))])
            (value-of-statement car-rev (cadr value-of-statements cdr-rev)))])))


(define value-of-statement
  (lambda (stmt env)
    (cases statement stmt 
      (assign (var expr) (letrec ([val (car (value-of-expression expr env))]
                                  [ref (newref val)]
                                  [new-env (entend-env var ref env)])
                          (cons new-env (list (empty-val)))))
      (else (eopl:error "FUCK\n")))))


;;; (define value-of-statement
;;;   ())
