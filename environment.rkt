#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define init-env
  (lambda ()
    (empty-environment)))

(define apply-env
    (lambda (env search-var)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-var))
        (extend-env (var val saved-env)
          (if (eqv? search-var var)
            val
            (apply-env saved-env search-var))))))
