#lang racket

(require "datatypes.rkt")
(require (lib "eopl.ss" "eopl"))

(define init-env
  (lambda ()
    (empty-environment)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-environment ()
        (eopl:error 'apply-env "No binding for ~s" search-var))
      (extend-environment (var val saved-env)
        (if (eqv? search-var var)
          val
          (apply-env saved-env search-var))))))


(define extend-env
  (lambda (var val env)
    (extend-environment var (new-ref val))

;;; (define update-env
;;;   (lambda (var val env))
;;;     (cases environment env
;;;       (empty-environment ()
;;;         (eopl:error ')))