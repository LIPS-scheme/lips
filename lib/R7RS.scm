;;   __ __                          __
;;  / / \ \       _    _  ___  ___  \ \
;; | |   \ \     | |  | || . \/ __>  | |
;; | |    > \    | |_ | ||  _/\__ \  | |
;; | |   / ^ \   |___||_||_|  <___/  | |
;;  \_\ /_/ \_\                     /_/
;;
;; <https://lips.js.org>
;;
;; Attempt to add missing R7RS small standard functions and macros
;;
;; Reference:
;; https://small.r7rs.org/attachment/r7rs.pdf
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license

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
(define-syntax let-values
  (syntax-rules ()
    ((_ ()) nil)
    ((_ () body ...) (begin body ...))
    ((_ (((x ...) values) ...) body ...)
     (apply (lambda (x ... ...)
              body ...)
            (vector->list (apply %vector-concat (map (lambda (x) ((. x "valueOf")))
                                                     (list values ...)))))))
  "(let-values binding body ...)

   The macro work similar to let but variable is list of values and value
   need to evaluate to result of calling values.")

;; -----------------------------------------------------------------------------
(define (%vector-concat . args)
  (if (null? args)
      #()
      (begin
        (typecheck "%vector-concat" (car args) "array")
        (--> (car args) (concat (apply %vector-concat (cdr args)))))))

;; -----------------------------------------------------------------------------
(define-syntax let*-values
  (syntax-rules ()
    ((_ ()) nil)
    ((_ () body ...) (begin body ...))
    ((_ ((bind values) rest ...) . body)
     (apply (lambda bind
              (let*-values (rest ...) . body))
            (vector->list ((. values "valueOf"))))))
  "(let*-values binding body ...)

   The macro work similar to let* but variable is list of values and value
   need to evaluate to result of calling values.")
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
(define (port? x)
  "(port? x)

   Function return true of argumet is nput or output port port object."
  (or (output-port? x) (input-port? x)))

;; -----------------------------------------------------------------------------
(define (square x)
  "(square z)

  Returns the square of z. This is equivalent to (* z z)."
  (* x x))

;; -----------------------------------------------------------------------------
(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...))))
  "(when test body ...)

   Macro execute body when test expression is true.")

;; -----------------------------------------------------------------------------
(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...))))
  "(unless test body ...)

   Macro execute body when test expression is false.")

;; -----------------------------------------------------------------------------
(define inexact exact->inexact)
(define exact inexact->exact)

;; -----------------------------------------------------------------------------
(define (string->vector s)
  "(string->vector string)

   Function return vector of characters created from string."
  (typecheck "string->list" s "string")
  (--> s (split "") (map (lambda (x) (lips.LCharacter x)))))

;; -----------------------------------------------------------------------------
(define (vector->string v)
  "(vector->string vector)

   Function return new string created from vector of characters."
  (typecheck "vector->list" v "array")
  (--> v (map (lambda (char) (char.valueOf))) (join "")))

;; -----------------------------------------------------------------------------
(define (vector-map fn . rest)
  "(vector-map fn vector1 vector2 ...)

   Function return new vector from applying function fn to each element
   of the vectors, similar to map for lists."
  (if (or (= (length rest) 0) (not (every vector? rest)))
      (error "vector-map: function require at least 1 vector")
      (let ((len (apply min (map vector-length rest)))
            (result (vector)))
        (do ((i 0 (+ i 1)))
            ((= i len) result)
            (let* ((args (map (lambda (v) (vector-ref v i)) rest))
                   (value (apply fn args)))
              (--> result (push value)))))))

;; -----------------------------------------------------------------------------
(define (string-map fn . rest)
  "(string-map fn string1 stringr2 ...)

   Function return new string from applying function fn to each element
   of the strings, similar to map for lists."
  (if (or (= (length rest) 0) (not (every string? rest)))
      (error "string-map: function require at least 1 string")
      (vector->string (apply vector-map fn (map string->vector rest)))))

;; -----------------------------------------------------------------------------
(define (dynamic-wind before thunk after)
  "(dynamic-wind before thunk after)

   Function accept 3 procedures/lambdas and execute thunk with before and always
   after even if error accur"
  (before)
  (let ((result (try (thunk)
                     (catch (e)
                            (error e)))))
    (after)
    result))

;; -----------------------------------------------------------------------------
(define (with-exception-handler handler thunk)
  "(with-exception-handler thunk handler)

   Procedure call and return value of thunk function, if exception happen
   it call handler procedure."
  (try (thunk)
       (catch (e)
              (handler e))))
;; -----------------------------------------------------------------------------
(define raise throw)

;; -----------------------------------------------------------------------------

