;; -*- scheme -*-
;; Attempt to add missing R7RS small standard functions and macros
;;
;; Reference:
;; https://small.r7rs.org/attachment/r7rs.pdf
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license

;; -----------------------------------------------------------------------------
(define-macro (do vars test . body)
  "(do ((<var> <init> <next>)) (test expression) . body)

   Iteration macro that evaluate the expression body in scope of the variables.
   On Eeach loop it increase the variables according to next expression and run
   test to check if the loop should continue. If test is signle call the macro
   will not return anything. If the test is pair of expression and value the
   macro will return that value after finish."
  (let ((return? (eq? (length test) 2)) (loop (gensym)))
    `(let ,loop (,@(map (lambda (spec)
                    `(,(car spec) ,(cadr spec)))
                 vars))
          (if (not ,(car test))
              (begin
                ,@body
                (,loop ,@(map (lambda (spec)
                                (if (null? (cddr spec))
                                    (car spec)
                                    (caddr spec)))
                              vars)))
                ,(if return? (cadr test))))))

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

;;(define (read-line . rest)
;;  (let ((port (if (null? rest) (current-input-port) (car rest))))
;;    (while (let ((char (peek-char port)))
;;             (and (not (string?

;; -----------------------------------------------------------------------------
;; function for Gauche code
;; -----------------------------------------------------------------------------
(define (values-ref values n)
  "(values-ref values n)

   Function return n value of values object which is result of value function."
  (typecheck "values-ref" values "values" 1)
  (typecheck "values-ref" n "number" 1)
  (--> values (valueOf) n))
;; -----------------------------------------------------------------------------
;; R7RS division operators (Gauche Scheme) BSD license
;; Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
;; -----------------------------------------------------------------------------
(define (quotient&remainder x y)
  (values (quotient x y) (remainder x y)))

(define (floor/ x y)
  (let ((q (quotient x y))
        (r (remainder x y)))
    (if (>= x 0)
      (if (or (> y 0) (zero? r))
        (values q r)
        (values (- q 1) (+ r y)))
      (if (and (> y 0) (not (zero? r)))
        (values (- q 1) (+ r y))
        (values q r)))))
(define (floor-quotient x y)     (values-ref (floor/ x y) 0))
(define (floor-remainder x y)    (modulo x y))
(define (truncate/ x y)          (quotient&remainder x y))
(define (truncate-quotient x y)  (quotient x y))
(define (truncate-remainder x y) (remainder x y))

;; -----------------------------------------------------------------------------
(define (make-list n . rest)
  (if (or (not (integer? n)) (<= n 0))
      (throw (new Error "make-list: first argument need to be integer larger then 0"))
      (let ((fill (if (null? rest) undefined (car rest))))
        (let iter ((n n) (result '()))
          (if (zero? n)
              result
              (iter (- n 1) (cons fill result)))))))

;; -----------------------------------------------------------------------------
(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 ...) ...)
     (lambda args
       (let ((len (length args)))
         (letrec-syntax
             ((cl (syntax-rules ::: ()
                                ((cl)
                                 (error "no matching clause"))
                                ((cl ((p :::) . body) . rest)
                                 (if (= len (length '(p :::)))
                                     (apply (lambda (p :::)
                                              . body)
                                            args)
                                     (cl . rest)))
                                ((cl ((p ::: . tail) . body)
                                     . rest)
                                 (if (>= len (length '(p :::)))
                                     (apply
                                      (lambda (p ::: . tail)
                                        . body)
                                      args)
                                     (cl . rest))))))
           (cl (params body0 ...) ...))))))
  "(case-lambda expr ...)

   Macro create new function with different version of the function depend on
   number of arguments. Each expression is similar to single lambda.

   e.g.:

       (define sum
          (case-lambda
            ((x) x)
            ((x y) (+ x y))
            ((x y z) (+ x y z))))

       (sum 1)
       (sum 1 2)
       (sum 1 2 3)

   More arguments will give error.")

;; -----------------------------------------------------------------------------
(define (boolean=? . args)
  "(boolean=? b1 b2 ...)

   Function check if all arguments are boolean and if they are the same."
  (if (< (length args) 2)
      (error "boolean=?: too few arguments")
      (reduce (lambda (acc item)
                (and (boolean? item) (eq? acc item)))
              (car args)
              (cdr args))))

;; -----------------------------------------------------------------------------
