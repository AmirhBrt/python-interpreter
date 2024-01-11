#lang racket

(require (lib "eopl.ss" "eopl"))

(require "../datatypes/all.rkt")

(define init-env
  (lambda ()
    (empty-environment)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-environment ()
        (eopl:error 'apply-env "No binding for ~s" search-var))
      (extend-environment (var val saved-env)        
        (if (string=? search-var var)
          val
          (apply-env saved-env search-var))
        ))))

(define is-in-env
  (lambda (env search-var)
    (cases environment env
      (empty-environment () #f)
      (extend-environment (var val saved-env)
        (if 
          (string=? search-var var)
          #t
          (is-in-env saved-env search-var))))))

(provide (all-defined-out))