#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")
(require "environment.rkt")
(require "parser.rkt")
(require "store.rkt")

;;; (define value-of-program
;;;   (lambda (parse-tree env))
;;;     (cases program parse-tree
;;;       (a-program (prog-statements)
;;;         (value-of-statements prog-statements env))))

;;; (define value-of-statements
;;;   (lambda (prog-statements env)
;;;     (cases statements* prog-statements
;;;       (single-stament (a-statement)
;;;         (value-of-statement a-statement env))
;;;       (statements (rest-statements last-statement)
;;;         (letrec ((vals (value-of-statements rest-statements env)))
;;;                 (value-of-statement last-statement (cadr vals)))))))


;;; (define value-of-statement
;;;   ())
