;; -*- scheme -*-
;; Attempt to add missing R7RS small standard functions and macros
;;
;; Reference:
;; https://small.r7rs.org/attachment/r7rs.pdf
;;
;; This file is part of the LIPS - Scheme based Powerful LISP in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license

;; -----------------------------------------------------------------------------
(define-macro (do vars test . body)
  "(do ((<var> <init> <next>)) (test expression) . body)

   Iteration macro that evaluate the expression body in scope of the variables.
   On Eeach loop it increase the variables according to next expression and run
   test to check if the loop should continue. If test is signle call the macro
   will not return anything. If the test is pair of expression and value the
   macro will return that value after finish."
  (let ((return? (not (symbol? (car test)))))
    `(let (,@(map (lambda (spec)
                    `(,(car spec) ,(cadr spec)))
                 vars))
        (while (not ,(if return? `,(car test) `,test))
          ,@body
          ,@(map (lambda (spec)
                   `(set! ,(car spec) ,(caddr spec)))
              (filter (lambda (item)
                         (== (length item) 3))
                    vars)))
        ,(if return? (cadr test)))))

;; -----------------------------------------------------------------------------
(define (list-match? predicate list)
  "(list-match? predicate list)

   Function check if consecutive elements of the list match the predicate function."
  (typecheck "list-match?" predicate #("function" "macro"))
  (typecheck "list-match?" list "pair")
  (or (or (null? list)
          (null? (cdr list)))
      (and (predicate (car list) (cadr list))
           (list-match? predicate (cdr list)))))

;; -----------------------------------------------------------------------------
(define (symbol=? . args)
  "(symbol=? s1 s2 ...)

   Function check if each value is symbol and it's the same acording to string=? predicate."
  (list-match? (lambda (a b)
                 (and (symbol? a) (symbol? b) (equal? a b)))
               args))
