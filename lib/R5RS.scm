;; -*- scheme -*-
;; Attempt to implement R5RS standard on top of LIPS
;;
;; Reference:
;; https://schemers.org/Documents/Standards/R5RS/HTML/
;;
;; This file is part of the LIPS - Simple lisp in JavaScript
;; Copyriht (C) 2019 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;

;; TODO: add special macro lists and special macro symbols
;;       #\x as character object
;; (define #f false)
;; (define #t true)
;; '#(1 2 3)

(define (eqv? a b)
  "(eqv? a b)

   Function compare the values. It return true if they are the same, they
   need to have same type"
  (if (eq? (type a) (type b))
      (if (number? a)
          (= a b) ;; eq? in JS also works on numbers
          (if (and (pair? a) (null? a) (null? b))
              true
              (eq? a b)))
      false))

(define = ==)

(define (equal? a b)
  "(equal? a b)

   Function check if values are equal if both are pair or array
   it compares the their elements recursivly."
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((and (function? a) (function? b))
         (%same-functions a b))
        ((and (array? a) (array? b) (eq? (length a) (length b)))
         (= (--> a (filter (lambda (item i) (equal? item (. b i)))) 'length) (length a)))
        (else (eqv? a b))))

(define (every . args)
  "(every . args)

   Function return true if every argument is true otherwise it return false."
  (= (length args) (length (filter (lambda (x) x) args))))

(define make-promise
  (lambda (proc)
    "(make-promise fn)

     Function create promise from a function."
    (typecheck "make-promise" proc "function")
    (let ((result-ready? #f)
          (result #f))
      (lambda ()
        (if result-ready?
            result
            (let ((x (proc)))
              (if result-ready?
                  result
                  (begin (set! result-ready? #t)
                         (set! result x)
                         result))))))))

(define-macro (delay expression)
  "(delay expression)

   Macro will create a promise from expression that can be forced with force."
  `(make-promise (lambda () ,expression)))

(define (force promise)
  "(force promise)

   Function force the promise and evaluate delayed expression."
  (promise))

(define (positive? x)
  "(positive? x)

   Function check if number is larger then 0"
  (typecheck "positive?" x "number")
  (> x 0))

(define (negative? x)
  "(negative? x)

   Function check if number is smaller then 0"
  (typecheck "negative?" x "number")
  (< x 0))

(define (zero? x)
  "(zero? x)

   Function check if number is equal to 0"
  (typecheck "zero?" x "number")
  (= x 0))

(define (quotient a b)
  (typecheck "quotient" x "number")
  (/ a b))

(define remainder %)

(define (number->string x . rest)
  "(number->string x [radix])

   Function convert number to string with optional radix (number base)."
  (typecheck "number->string" x "number" 1)
  (let ((radix (if (null? rest) 10 (car rest))))
    (typecheck "number->string" radix "number" 2)
    (--> x (toString (--> radix (valueOf))))))

(define (boolean? x)
  "(boolean? x)

   Function return true if value is boolean."
   (eq? (type x) "boolean"))

(define (vector-ref vector i)
  "(vector-ref vector i)

   Return i element from vector."
  (typecheck "number->string" vector "array" 1)
  (typecheck "number->string" i "number" 2)
  (. vector i))

(define (vector-set! vector i obj)
  "(vector-set! vector i obj)

   Set obj as value in vector at position 1."
  (typecheck "vector-set!" vector "array" 1)
  (typecheck "vector-set!" i "number" 2)
  (set-obj! vector i obj))

(define -inf.0 (.. Number.NEGATIVE_INFINITY))
(define +inf.0 (.. Number.POSITIVE_INFINITY))

;; TODO:
;; lips> (define x '(1 2 3))
;; lips> (set-cdr (cdr x) x)
;; lips> (list? x)

(define (list? x)
  "(list? x)

   Function test if value is propert linked list structure.
   The car of each pair can be any value."
  (and (pair? x) (or (null? (cdr x)) (list? (cdr x)))))

(define (integer? x)
  (throw "Not Yet implemented"))

(define (typecheck-args _type name _list)
  "(typecheck-args args type)

   Function check if all items in array are of same type."
  (let iter ((n 1) (_list _list))
    (if (pair? _list)
        (begin
          (typecheck name (car _list) _type n)
          (iter (+ n 1) (cdr _list))))))

(define numbers? (curry typecheck-args "number"))

(define (max . args)
  "(max n1 n2 ...)

   Return maximum of it's arguments."
  (numbers? "max" args)
  (apply (.. Math.max) args))


(define (min . args)
  "(min n1 n2 ...)

   Return minimum of it's arguments."
  (numbers? "min" args)
  (apply (.. Math.min) args))

(define (make-rectangular re im)
  "(make-rectangular im re)

   Create complex number from imaginary and real part."
  ((.. lips.LNumber) (make-object :im im :re re)))

(define (real? n)
  "(real? n)"
  (and (number? n) (eq? (. n 'type) "float")))

(define (exact? n)
  "(exact? n)"
  (typecheck "exact?" n "number")
  (let ((type (. n 'type)))
    (or (eq? type "bigint") (eq? type "rational"))))

(define (exact->inexact n)
  (typecheck "exact->inexact" n "number")
  (--> n (valueOf)))

(define (inexact->exact n)
  (typecheck "inexact->exact" n "number")
  (if (real? n)
      (--> n (toRational))
      n))

(define procedure? function?)

;; generate Math functions with documentation
(define _maths (list "exp" "log" "sin" "cos" "tan" "asin" "acos" "atan" "atan"))

(define _this_env (current-environment))
(let iter ((fns _maths))
  (if (not (null? fns))
      (let* ((name (car fns))
             (LNumber (.. lips.LNumber))
             (op (. Math name))
             (fn (lambda (n) (LNumber (op n)))))
        (--> _this_env (set name fn))
        (set-obj! fn '__doc__ (concat "(" name " n)\n\nFunction calculate " name
                                  " math operation (it call JavaScript Math)." name
                                  " function."))
        (iter (cdr fns)))))

(define (modulo a b)
  "(modulo a b)

   Function return modulo operation on it's argumennts."
  (typecheck "modulo" a "number" 1)
  (typecheck "modulo" b "number" 2)
  (- a (* b (floor (/ a b)))))

(define (remainder__ a b)
  "(modulo a b)

   Function return reminder from division operation."
  (typecheck "remainder" a "number" 1)
  (typecheck "remainder" b "number" 2)
  (- a (* b (truncate (/ a b)))))

(define (quotient a b)
  "(quotient a b)

   Function return integer part from division operation."
  (typecheck "quotient" a "number" 1)
  (typecheck "quotient" b "number" 2)
  (truncate (/ a b)))

(define (list . args)
  "(list . args)

   Function create new list out of its arguments."
  args)

(define (list-tail x k)
  "(list-tail x k)

   Function return tail of k element of a list."
  (if (zero? k)
      x
      (list-tail (cdr x) (- k 1))))

;;library procedure:  (assq obj alist)
;;library procedure:  (assv obj alist)
;;library procedure:  (assoc obj alist)

(define (not x)
  "(not x)

   Function return true if value is false and false otherwise."
  (if x false true))

(define print display)

(define (inexact->exact number)
  "(inexact->exact number)

   Funcion convert real number to exact ratioanl number."
  (typecheck "rationalize" number "number")
  (--> (lips.LFloat number) (toRational 1e-20)))

(define (rationalize number tolerance)
  "(rationalize number tolerance)

   Function returns simplest rational number differing from number by no more than the tolerance."
  (typecheck "rationalize" number "number" 1)
  (typecheck "rationalize" tolerance "number" 2)
  (lips.rationalize number tolerance))

(define #f false)
(define #t true)


(define (%mem/search access op obj list)
  "(%member obj list function)

   Helper method to get first list where car equal to obj
   using provied functions as comparator."
  (if (null? list)
      false
      (if (op (access list) obj)
          list
          (%mem/search access op obj (cdr list)))))

(define (memq obj list)
  "(memq obj list)

   Function return first object in the list that match using eq? function."
  (typecheck "memq" list "pair")
  (%mem/search car eq? obj list ))

(define (memv obj list)
  "(memv obj list)

   Function return first object in the list that match using eqv? function."
  (typecheck "memv" list "pair")
  (%mem/search car eqv? obj list))

(define (member obj list)
  "(member obj list)

   Function return first object in the list that match using equal? function."
  (typecheck "member" list "pair")
  (%mem/search car equal? obj list))

(define (%assoc/acessor name)
  "(%assoc/acessor name)

   Function return carr with typecheck using give name."
  (lambda (x)
    (typecheck name x "pair")
    (caar x)))

(define (%assoc/search op obj alist)
  "(%assoc/search op obj alist)

   Generic function that used in assoc functions with defined comparator
   function."
  (typecheck "assoc" alist "pair")
  (let ((ret (%mem/search (%assoc/acessor "assoc") op obj alist)))
    (if ret
        (car ret)
        ret)))

(define (assoc obj alist)
  "(assoc obj alist)

   Function return pair from alist that match given key using equal? check."
  (%assoc/search equal? obj alist))

(define (assq obj alist)
  "(assq obj alist)

   Function return pair from alist that match given key using eq? check."
  (%assoc/search eq? obj alist))

(define (assv obj alist)
  "(assv obj alist)

   Function return pair from alist that match given key using eqv? check."
  (%assoc/search eqv? obj alist))

;; STRING FUNCTIONS

(define (list->string _list)
  "(list->string _list)

   Function return string from list of characters."
  (let ((array (list->array
                (map (lambda (x)
                       (typecheck "list->string" x "character")
                       (. x 'char))
                     _list))))
    (--> array (join ""))))
