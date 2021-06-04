;;   __ __                          __
;;  / / \ \       _    _  ___  ___  \ \
;; | |   \ \     | |  | || . \/ __>  | |
;; | |    > \    | |_ | ||  _/\__ \  | |
;; | |   / ^ \   |___||_||_|  <___/  | |
;;  \_\ /_/ \_\                     /_/
;;
;; <https://lips.js.org>
;;
;; Attempt to implement R5RS standard on top of LIPS
;;
;; Reference:
;; https://schemers.org/Documents/Standards/R5RS/HTML/
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2021 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;
;; (+ 1 (call-with-current-continuation
;;       (lambda (escape)
;;         (+ 2 (escape 3)))))
;;
;; -----------------------------------------------------------------------------
(define string-append concat)
(define = ==)
(define remainder %)
(define -inf.0 Number.NEGATIVE_INFINITY)
(define +inf.0 Number.POSITIVE_INFINITY)
(define procedure? function?)
(define expt **)
(define list->vector list->array)
(define vector->list array->list)
;; -----------------------------------------------------------------------------
(define-macro (define-symbol-macro type spec . rest)
  "(define-symbol-macro type (name . args) . body)

   Macro that creates special symbol macro for evaluator similar to build in , or `.
   It's like alias for real macro. Similar to CL reader macros but it receive already
   parsed code like normal macros. Type can be SPLICE or LITERAL symbols.
   ALL default symbol macros are literal."
  (let* ((name (car spec))
         (symbol (cadr spec))
         (args (cddr spec)))
     `(begin
        (set-special! ,symbol ',name ,(string->symbol
                                       (concat "lips.specials."
                                               (symbol->string type))))
        (define-macro (,name ,@args) ,@rest))))

;; -----------------------------------------------------------------------------
;; Vector literals syntax using parser symbol macros
;; -----------------------------------------------------------------------------
(set-special! "#" 'vector-literal lips.specials.SPLICE)

;; -----------------------------------------------------------------------------
(define-macro (vector-literal . args)
  (if (not (or (pair? args) (eq? args nil)))
      (throw (new Error (concat "Parse Error: vector require pair got "
                                (type args) " in " (repr args))))
      (let ((v (list->array args)))
        (Object.freeze v)
        v)))

;; -----------------------------------------------------------------------------
(define-syntax vector
  (syntax-rules ()
    ((_ arg ...) (list->array (list arg ...))))
  "(vector 1 2 3 (+ 3 1))
   #(1 2 3 4)

   Macro for defining vectors (JavaScript arrays). Vectors literals are
   automatically quoted. So you can use expressions inside them. Only other
   literals, like other vectors or object.")

;; -----------------------------------------------------------------------------
(set-repr! Array
           (lambda (arr q)
             ;; Array.from is used to convert emtpy to undefined
             ;; but we can't use the value because Array.from call
             ;; valueOf on its arguments
             (let ((result (--> (Array.from arr)
                                (map (lambda (x i)
                                       (if (not (in i arr))
                                           "#<empty>"
                                           (repr (. arr i) q)))))))
               (concat "#(" (--> result (join " ")) ")"))))

;; -----------------------------------------------------------------------------
(define (eqv? a b)
  "(eqv? a b)

   Function compare the values. It return true if they are the same, they
   need to have same type"
  (if (string=? (type a) (type b))
      (cond ((number? a)
             (or (and (exact? a) (exact? b) (= a b))
                 (and (inexact? a)
                      (inexact? b)
                      (cond ((a.isNaN) (b.isNaN))
                            ((and (zero? a) (zero? b))
                             (eq? a._minus b._minus))
                            ((and (complex? a) (complex? b))
                             (let ((re.a (real-part a))
                                   (re.b (real-part b))
                                   (im.a (imag-part a))
                                   (im.b (imag-part b)))
                               (and
                                (if (and (zero? re.a) (zero? re.b))
                                    (eq? (. re.a '_minus) (. re.b '_minus))
                                    true)
                                (if (and (zero? im.a) (zero? im.b))
                                    (eq? (. im.a '_minus) (. im.b '_minus))
                                    true)
                                (or (= re.a re.b)
                                    (and (--> re.a (isNaN))
                                         (--> re.b (isNaN))))
                                (or (= im.a im.b)
                                    (and (--> im.a (isNaN))
                                         (--> im.b (isNaN)))))))
                            (else (= a b))))))
            ((pair? a) (and (null? a) (null? b)))
            (else (eq? a b)))
      false))

;; -----------------------------------------------------------------------------
(define (equal? a b)
  "(equal? a b)

   Function check if values are equal if both are pair or array
   it compares the their elements recursivly."
  (cond ((and (pair? a))
         (and (pair? b)
              (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((symbol? a)
         (and (symbol? b)
              (equal? a.__name__ b.__name__)))
        ((regex? a)
         (and (regex? b)
              (equal? (. a 'source) (. b 'source))))
        ((typed-array? a)
         (and (typed-array? b)
              (equal? (Array.from a) (Array.from b))))
        ((vector? a)
         (and (vector? b)
              (= (length a) (length b))
              (--> a (every (lambda (item i)
                              (equal? item (vector-ref b i)))))))
        ((string? a)
         (and (string? b)
              (string=? a b)))
        ((function? a)
         (and (function? b)
              (%same-functions a b)))
        ((array? a)
         (and (array? b)
              (eq? (length a) (length b))
              (= (--> a (filter (lambda (item i)
                                  (equal? item (. b i))))
                      'length)
                 (length a))))
        ((plain-object? a)
         (and (plain-object? b)
              (let ((keys_a (--> (Object.keys a) (sort)))
                    (keys_b (--> (Object.keys b) (sort))))
                (and (= (length keys_a)
                        (length keys_b))
                     (equal? keys_a keys_b)
                     (equal? (--> keys_a (map (lambda (key) (. a key))))
                             (--> keys_b (map (lambda (key) (. b key)))))))))
        (else (eqv? a b))))

;; -----------------------------------------------------------------------------
(define make-promise
  (lambda (proc)
    "(make-promise fn)

     Function create promise from a function."
    (typecheck "make-promise" proc "function")
    (let ((result-ready? #f)
          (result #f))
      (let ((promise (lambda ()
                       (if result-ready?
                           result
                           (let ((x (proc)))
                             (if result-ready?
                                 result
                                 (begin (set! result-ready? #t)
                                        (set! result x)
                                        result)))))))
        (set-obj! promise (Symbol.for "promise") true)
        (set! promise.toString (lambda ()
                                 (string-append "#<promise - "
                                                (if result-ready?
                                                    (string-append "forced with "
                                                                   (type result))
                                                    "not forced")
                                                ">")))
        promise))))

;; -----------------------------------------------------------------------------
(define-macro (delay expression)
  "(delay expression)

   Macro will create a promise from expression that can be forced with (force)."
  `(make-promise (lambda () ,expression)))

;; -----------------------------------------------------------------------------
(define (force promise)
  "(force promise)

   Function force the promise and evaluate delayed expression."
  (promise))

;; -----------------------------------------------------------------------------
(define (promise? obj)
  "(promise? obj)

   Function check if value is a promise created with delay or make-promise."
  (string=? (type obj) "promise"))

;; -----------------------------------------------------------------------------
(define (positive? x)
  "(positive? x)

   Function check if number is larger then 0"
  (typecheck "positive?" x "number")
  (> x 0))

;; -----------------------------------------------------------------------------
(define (negative? x)
  "(negative? x)

   Function check if number is smaller then 0"
  (typecheck "negative?" x "number")
  (< x 0))

;; -----------------------------------------------------------------------------
(define (zero? x)
  "(zero? x)

   Function check if number is equal to 0"
  (typecheck "zero?" x "number")
  (= x 0))

;; -----------------------------------------------------------------------------
(define (quotient a b)
  "(quotient a b)

   Return quotient from divition as integer."
  (typecheck "quotient" a "number")
  (typecheck "quotient" b "number")
  (if (zero? b 0)
     (throw (new Error "quotient: divition by zero"))
     (let ((quotient (/ a b)))
       (if (integer? quotient)
           quotient
           (if (> quotient 0)
               (floor quotient)
               (ceiling quotient))))))

;; -----------------------------------------------------------------------------
(define (number->string x . rest)
  "(number->string x [radix])

   Function convert number to string with optional radix (number base)."
  (typecheck "number->string" x "number" 1)
  (let ((radix (if (null? rest) 10 (car rest))))
    (typecheck "number->string" radix "number" 2)
    (--> x (toString (--> radix (valueOf))))))

;; -----------------------------------------------------------------------------
(define (boolean? x)
  "(boolean? x)

   Function return true if value is boolean."
   (string=? (type x) "boolean"))

;; -----------------------------------------------------------------------------
(define (vector-ref vector i)
  "(vector-ref vector i)

   Return i element from vector."
  (typecheck "number->string" vector "array" 1)
  (typecheck "number->string" i "number" 2)
  (. vector i))

;; -----------------------------------------------------------------------------
(define (vector-set! vector i obj)
  "(vector-set! vector i obj)

   Set obj as value in vector at position 1."
  (typecheck "vector-set!" vector "array" 1)
  (typecheck "vector-set!" i "number" 2)
  (set-obj! vector i obj))

;; -----------------------------------------------------------------------------
(define (%number-type type x)
  (typecheck "%number-type" type (vector "string" "pair"))
  (typecheck "%number-type" x "number")
  (let* ((t x.__type__)
         (typeof (lambda (type) (string=? t type))))
    (and (number? x)
         (if (pair? type)
             (some typeof type)
             (typeof type)))))


;; -----------------------------------------------------------------------------
(define (real? x)
  "(real? x)

   Function check if argument x is real."
  (and (number? x) (or (eq? x NaN)
                       (eq? x Number.NEGATIVE_INFINITY)
                       (eq? x Number.POSITIVE_INFINITY)
                       (and (%number-type "complex" x)
                            (let ((i (imag-part x)))
                              (and (zero? i) (exact? i))))
                       (%number-type '("float" "bigint" "rational") x))))

;; -----------------------------------------------------------------------------
(define (integer? x)
  "(integer? x)

  Function check if argument x is integer."
  (and (number? x)
       (not (eq? x NaN))
       (not (eq? x Number.NEGATIVE_INFINITY))
       (not (eq? x Number.POSITIVE_INFINITY))
       (or (%number-type "bigint" x)
           (and (%number-type "float" x)
                (= (modulo x 2) 1)))))

;; -----------------------------------------------------------------------------
(define (complex? x)
  "(complex? x)

  Function check if argument x is complex."
  (and (number? x) (or (eq? x NaN)
                       (eq? x Number.NEGATIVE_INFINITY)
                       (eq? x Number.POSITIVE_INFINITY)
                       (%number-type '("complex" "float" "bigint" "rational") x))))

;; -----------------------------------------------------------------------------
(define (rational? x)
  "(rational? x)

  Function check if value is rational."
  (and (number? x)
       (not (eq? x NaN))
       (not (eq? x Number.NEGATIVE_INFINITY))
       (not (eq? x Number.POSITIVE_INFINITY))
       (or (%number-type "rational" x) (integer? x))))

;; -----------------------------------------------------------------------------
(define (typecheck-args _type name _list)
  "(typecheck-args args type)

   Function check if all items in array are of same type."
  (let iter ((n 1) (_list _list))
    (if (pair? _list)
        (begin
          (typecheck name (car _list) _type n)
          (iter (+ n 1) (cdr _list))))))

;; -----------------------------------------------------------------------------
(define numbers? (curry typecheck-args "number"))

;; -----------------------------------------------------------------------------
(define (max . args)
  "(max n1 n2 ...)

   Return maximum of it's arguments."
  (numbers? "max" args)
  (apply (.. Math.max) args))

;; -----------------------------------------------------------------------------
(define (min . args)
  "(min n1 n2 ...)

   Return minimum of it's arguments."
  (numbers? "min" args)
  (apply (.. Math.min) args))

;; -----------------------------------------------------------------------------
(define (make-rectangular re im)
  "(make-rectangular im re)

   Create complex number from imaginary and real part."
  (let ((value `((re . ,re) (im . ,im))))
    (lips.LComplex (--> value (to_object true)))))

;; -----------------------------------------------------------------------------
(define (exact? n)
  "(exact? n)"
  (typecheck "exact?" n "number")
  (let ((type n.__type__))
    (or (string=? type "bigint")
        (string=? type "rational")
        (and (string=? type "complex")
             (exact? n.__im__)
             (exact? n.__re__)))))

;; -----------------------------------------------------------------------------
(define (inexact? n)
  "(inexact? n)"
  (typecheck "inexact?" n "number")
  (not (exact? n)))

;; -----------------------------------------------------------------------------
(define (exact->inexact n)
  "(exact->inexact n)

   Convert exact number to inexact."
  (typecheck "exact->inexact" n "number")
  (if (%number-type "complex" n)
      (lips.LComplex (object :im (exact->inexact (. n '__im__))
                             :re (exact->inexact (. n '__re__))))
      (if (or (rational? n) (integer? n))
          (lips.LFloat (--> n (valueOf)) true)
          n)))

;; -----------------------------------------------------------------------------
(define (inexact->exact n)
  "(inexact->exact number)

   Funcion convert real number to exact ratioanl number."
  (typecheck "inexact->exact" n "number")
  (if (or (real? n) (%number-type "complex" n))
      (--> n (toRational))
      n))

;; -----------------------------------------------------------------------------
;; generate Math functions with documentation
(define _maths (list "log" "sin" "cos" "tan" "asin" "acos" "atan" "atan"))

;; -----------------------------------------------------------------------------
(define _this_env (current-environment))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
(define (exp x)
  (typecheck "exp" x "number")
  (if (string=? x.__type__ "complex")
      (let* ((re (real-part x))
             (im (imag-part x))
             (factor (Math.exp re)))
         (make-rectangular (* factor (cos im))
                           (* factor (sin im))))
       (Math.exp x)))

;; -----------------------------------------------------------------------------
(define (modulo a b)
  "(modulo a b)

   Function return modulo operation on it's argumennts."
  (typecheck "modulo" a "number" 1)
  (typecheck "modulo" b "number" 2)
  (- a (* b (floor (/ a b)))))
;; -----------------------------------------------------------------------------
(define (remainder__ a b)
  "(modulo a b)

   Function return reminder from division operation."
  (typecheck "remainder" a "number" 1)
  (typecheck "remainder" b "number" 2)
  (- a (* b (truncate (/ a b)))))

;; -----------------------------------------------------------------------------
(define (list-tail l k)
  "(list-tail list k)

   Returns the sublist of list obtained by omitting the first k elements."
  (typecheck "list-tail" l '("pair" "nil"))
  (if (< k 0)
      (throw (new Error "list-ref: index out of range"))
      (let ((l l) (k k))
        (while (> k 0)
          (if (null? l)
              (throw (new Error "list-tail: not enough elements in the list")))
          (set! l (cdr l))
          (set! k (- k 1)))
        l)))

;; -----------------------------------------------------------------------------
(define (list-ref l k)
  "(list-ref list n)

   Returns n element of a list."
  (typecheck "list-ref" l '("pair" "nil"))
  (if (< k 0)
      (throw (new Error "list-ref: index out of range"))
      (let ((l l) (k k))
        (while (> k 0)
          (if (or (null? (cdr l)) (null? l))
              (throw (new Error "list-ref: not enough elements in the list")))
          (set! l (cdr l))
          (set! k (- k 1)))
        (if (null? l)
            l
            (car l)))))

;; -----------------------------------------------------------------------------
(define (not x)
  "(not x)

   Function return true if value is false and false otherwise."
  (if x false true))

;; -----------------------------------------------------------------------------
(define (rationalize number tolerance)
  "(rationalize number tolerance)

   Function returns simplest rational number differing from number by no more
   than the tolerance."
  (typecheck "rationalize" number "number" 1)
  (typecheck "rationalize" tolerance "number" 2)
  (lips.rationalize number tolerance))

;; -----------------------------------------------------------------------------
(define (%mem/search access op obj list)
  "(%member obj list function)

   Helper method to get first list where car equal to obj
   using provied functions as comparator."
  (if (null? list)
      false
      (if (op (access list) obj)
          list
          (%mem/search access op obj (cdr list)))))

;; -----------------------------------------------------------------------------
(define (memq obj list)
  "(memq obj list)

   Function return first object in the list that match using eq? function."
  (typecheck "memq" list '("nil" "pair"))
  (%mem/search car eq? obj list ))

;; -----------------------------------------------------------------------------
(define (memv obj list)
  "(memv obj list)

   Function return first object in the list that match using eqv? function."
  (typecheck "memv" list '("nil" "pair"))
  (%mem/search car eqv? obj list))

;; -----------------------------------------------------------------------------
(define (member obj list)
  "(member obj list)

   Function return first object in the list that match using equal? function."
  (typecheck "member" list '("nil" "pair"))
  (%mem/search car equal? obj list))

;; -----------------------------------------------------------------------------
(define (%assoc/acessor name)
  "(%assoc/acessor name)

   Function return carr with typecheck using give name."
  (lambda (x)
    (typecheck name x "pair")
    (caar x)))

;; -----------------------------------------------------------------------------
(define (%assoc/search op obj alist)
  "(%assoc/search op obj alist)

   Generic function that used in assoc functions with defined comparator
   function."
  (typecheck "assoc" alist (vector "nil" "pair"))
  (let ((ret (%mem/search (%assoc/acessor "assoc") op obj alist)))
    (if ret
        (car ret)
        ret)))

;; -----------------------------------------------------------------------------
(define assoc (%doc
               "(assoc obj alist)

                Function return pair from alist that match given key using equal? check."
               (curry %assoc/search equal?)))

;; -----------------------------------------------------------------------------
(define assq (%doc
              "(assq obj alist)

               Function return pair from alist that match given key using eq? check."
              (curry %assoc/search eq?)))

;; -----------------------------------------------------------------------------
(define assv (%doc
              "(assv obj alist)

               Function return pair from alist that match given key using eqv? check."
              (curry %assoc/search eqv?)))

;; -----------------------------------------------------------------------------
;; STRING FUNCTIONS
;; -----------------------------------------------------------------------------
;; (let ((x (make-string 20)))
;;   (string-fill! x #\b)
;;   x)
;; -----------------------------------------------------------------------------
(define (make-string k . rest)
  "(make-string k [char])

   Function return new string with k elements, if char is provied
   it's filled with that character."
  (let ((char (if (null? rest) #\space (car rest))))
    (typecheck "make-string" k "number" 1)
    (typecheck "make-string" char "character" 2)
    (let iter ((result '()) (k k))
      (if (<= k 0)
          (list->string result)
          (iter (cons char result) (- k 1))))))

;; -----------------------------------------------------------------------------
(define (string . args)
  "(string chr1 chr2 ...)

   Function create new string from it's arguments. Each argument
   Need to be a character object."
  (for-each (lambda (x)
              (typecheck "string" x "character"))
            args)
  (list->string args))

;; -----------------------------------------------------------------------------
(define (string-copy string)
  "(string-copy string)

   Returns a copy of the given string."
  (typecheck "string-copy" string "string")
  (--> string (clone)))

;; -----------------------------------------------------------------------------
;;(let ((x "xxxxxxxxxx"))
;;   (string-fill! x #\b)
;;    x)
;; -----------------------------------------------------------------------------
(define (string-fill! string char)
  "(string-fill! symbol char)

   Function destructively fill the string with given character."
  (typecheck "string-fill!" string "string" 1)
  (typecheck "string-fill!" char "character" 2)
  (--> string (fill char)))

;; -----------------------------------------------------------------------------
(define (identity n)
  "(identity n)

   No op function. it just returns its argument."
  n)

;; -----------------------------------------------------------------------------
(define (string-copy x)
  "(string-copy x)

   Create new string based of given argument."
  (typecheck "string-copy" x "string")
  (lips.LString x))

;; -----------------------------------------------------------------------------
(define (list->string _list)
  "(list->string _list)

   Function return string from list of characters."
  (let ((array (list->array
                (map (lambda (x)
                       (typecheck "list->string" x "character")
                       (x.valueOf))
                     _list))))
    (--> array (join ""))))

;; -----------------------------------------------------------------------------
(define (string->list string)
  "(string->list string)

   Function return list of characters created from string."
  (typecheck "string->list" string "string")
  (array->list (--> string (split "") (map (lambda (x) (lips.LCharacter x))))))

;; -----------------------------------------------------------------------------
;; (let ((x "hello")) (string-set! x 0 #\H) x)
(define-macro (string-set! object index char)
  "(string-set! object index char)

   Macro that replace character in string in given index, it create new JavaScript
   string and replace old value. Object need to be symbol that point to variable
   that hold the string."
  (typecheck "string-set!" object "symbol")
  (let ((chars (gensym "chars")))
    `(begin
       (typecheck "string-set!" ,object "string")
       (typecheck "string-set!" ,index "number")
       (typecheck "string-set!" ,char "character")
       (let ((,chars (list->vector (string->list ,object))))
          (set-obj! ,chars ,index ,char)
          (set! ,object (list->string (vector->list ,chars)))))))

;; -----------------------------------------------------------------------------
(define (string-length string)
  "(string-length string)

   Function return length of the string."
  (typecheck "string-ref" string "string")
  (. string 'length))

;; -----------------------------------------------------------------------------
(define (string-ref string k)
  "(string-ref string k)

   Function return character inside string at given zero-based index."
  (typecheck "string-ref" string "string" 1)
  (typecheck "string-ref" k "number" 2)
  (lips.LCharacter (--> string (get k))))

(define (%string-cmp name string1 string2)
  "(%string-cmp name a b)

   Function compare two strings and return 0 if they are equal,
   -1 second is smaller and 1 if is larget. The function compare
   the codepoints of the character."
  (typecheck name string1 "string" 1)
  (typecheck name string2 "string" 2)
  (--> string1 (cmp string2)))

;; -----------------------------------------------------------------------------
(define (string=? string1 string2)
  "(string=? string1 string2)

   Function check if two string s are equal."
  (= (%string-cmp "string=?" string1 string2) 0))

;; -----------------------------------------------------------------------------
(define (string<? string1 string2)
  "(string<? string1 string2)

   Function return true if second string is smaller then the first one."
  (= (%string-cmp "string<?" string1 string2) -1))

;; -----------------------------------------------------------------------------
(define (string>? string1 string2)
  "(string<? string1 string2)

   Function return true if second string is larger then the first one."
  (= (%string-cmp "string>?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string<=? string1 string2)
  "(string<? string1 string2)

   Function return true if second string is not larger then the first one."
  (< (%string-cmp "string<=?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string>=? string1 string2)
  "(string<? string1 string2)

   Function return true if second character is not smaller then the first one."
  (> (%string-cmp "string>=?" string1 string2) -1))

;; -----------------------------------------------------------------------------
(define (%string-ci-cmp name string1 string2)
  "(%string-ci-cmp name a b)

   Function compare two strings ingoring case and return 0 if they are equal,
   -1 second is smaller and 1 if is larget. The function compare
   the codepoints of the character."
  (typecheck name string1 "string" 1)
  (typecheck name string2 "string" 2)
  (--> string1 (lower) (cmp (--> string2 (lower)))))

;; -----------------------------------------------------------------------------
(define (string-ci=? string1 string2)
  "(string-ci=? string1 string2)

   Function check if two string s are equal."
  (= (%string-ci-cmp "string-ci=?" string1 string2) 0))

;; -----------------------------------------------------------------------------
(define (string-ci<? string1 string2)
  "(string-ci<? string1 string2)

   Function return true if second string is smaller then the first one."
  (= (%string-ci-cmp "string-ci<?" string1 string2) -1))

;; -----------------------------------------------------------------------------
(define (string-ci>? string1 string2)
  "(string-ci<? string1 string2)

   Function return true if second string is larger then the first one."
  (= (%string-ci-cmp "string-ci>?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string-ci<=? string1 string2)
  "(string-ci<? string1 string2)

   Function return true if second string is not larger then the first one."
  (< (%string-ci-cmp "string-ci<=?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string-ci>=? string1 string2)
  "(string-ci>=? string1 string2)

   Function return true if second character is not smaller then the first one."
  (> (%string-ci-cmp "string-ci>=?" string1 string2) -1))

;; -----------------------------------------------------------------------------
;; CHARACTER FUNCTIONS
;; -----------------------------------------------------------------------------

;; (display (list->string (list #\A (integer->char 10) #\B)))
;; -----------------------------------------------------------------------------
(define char? (%doc
        "(char? obj)

         Function check if object is character."
        (curry instanceof lips.LCharacter)))

;; -----------------------------------------------------------------------------
(define (char->integer chr)
  "(char->integer chr)

   Function return codepoint of Unicode character."
  (typecheck "char->integer" chr "character")
  (--> chr.__char__ (codePointAt 0)))

;; -----------------------------------------------------------------------------
(define (integer->char n)
  "(integer->char chr)

   Function convert number argument to chararacter."
  (typecheck "integer->char" n "number")
  (if (integer? n)
      (string-ref (String.fromCodePoint n) 0)
      (throw "argument to integer->char need to be integer.")))

;; -----------------------------------------------------------------------------
(define-macro (%define-chr-re spec str re)
  "(%define-chr-re (name chr) sring re)

   Macro define procedure that test character agains regular expression."
  `(define ,spec
     ,str
     (typecheck ,(symbol->string (car spec)) ,(cadr spec) "character")
     (not (null? (--> chr (toString) (match ,re))))))

;; -----------------------------------------------------------------------------
(%define-chr-re (char-whitespace? chr)
  "(char-whitespace? chr)

   Function return true if character is whitespace."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'space-unicode-regex))))

;; -----------------------------------------------------------------------------
(%define-chr-re (char-numeric? chr)
  "(char-numeric? chr)

   Function return true if character is number."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'numeral-unicode-regex))))

;; -----------------------------------------------------------------------------
(%define-chr-re (char-alphabetic? chr)
  "(char-alphabetic? chr)

   Function return true if character is leter of the ASCII alphabet."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'letter-unicode-regex))))

;; -----------------------------------------------------------------------------
(define (%char-cmp name chr1 chr2)
  "(%char-cmp name a b)

   Function compare two characters and return 0 if they are equal,
   -1 second is smaller and 1 if is larget. The function compare
   the codepoints of the character."
  (typecheck name chr1 "character" 1)
  (typecheck name chr2 "character" 2)
  (let ((a (char->integer chr1))
        (b (char->integer chr2)))
    (cond ((= a b) 0)
          ((< a b) -1)
          (else 1))))

;; -----------------------------------------------------------------------------
(define (char=? chr1 chr2)
  "(char=? chr1 chr2)

   Function check if two characters are equal."
  (= (%char-cmp "char=?" chr1 chr2) 0))

;; -----------------------------------------------------------------------------
(define (char<? chr1 chr2)
  "(char<? chr1 chr2)

   Function return true if second character is smaller then the first one."
  (= (%char-cmp "char<?" chr1 chr2) -1))

;; -----------------------------------------------------------------------------
(define (char>? chr1 chr2)
  "(char<? chr1 chr2)

   Function return true if second character is larger then the first one."
  (= (%char-cmp "char>?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char<=? chr1 chr2)
  "(char<? chr1 chr2)

   Function return true if second character is not larger then the first one."
  (< (%char-cmp "char<=?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char>=? chr1 chr2)
  "(char<? chr1 chr2)

   Function return true if second character is not smaller then the first one."
  (> (%char-cmp "char>=?" chr1 chr2) -1))

;; -----------------------------------------------------------------------------
(define (%char-ci-cmp name chr1 chr2)
  "(%char-cmp name a b)

   Function compare two characters and return 0 if they are equal,
   -1 second is smaller and 1 if is larget. The function compare
   the codepoints of the character."
  (typecheck name chr1 "character" 1)
  (typecheck name chr2 "character" 2)
  (%char-cmp name (char-downcase chr1) (char-downcase chr2)))

;; -----------------------------------------------------------------------------
(define (char-ci=? chr1 chr2)
  "(char-ci=? chr1 chr2)

   Function check if two characters are equal."
  (= (%char-ci-cmp "char-ci=?" chr1 chr2) 0))

;; -----------------------------------------------------------------------------
(define (char-ci<? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Function return true if second character is smaller then the first one."
  (= (%char-ci-cmp "char-ci<?" chr1 chr2) -1))

;; -----------------------------------------------------------------------------
(define (char-ci>? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Function return true if second character is larger then the first one."
  (= (%char-ci-cmp "char-ci>?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char-ci<=? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Function return true if second character is not larger then the first one."
  (< (%char-ci-cmp "char-ci<=?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char-ci>=? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Function return true if second character is not smaller then the first one."
  (> (%char-ci-cmp "char-ci>=?" chr1 chr2) -1))

;; -----------------------------------------------------------------------------
(define (char-upcase char)
  "(char-upcase char)

   Create uppercase version of the character."
  (typecheck "char-upcase" char "character")
  (char.toUpperCase))

;; -----------------------------------------------------------------------------
(define (char-downcase char)
  "(char-downcase chr)

   Create lowercase version of the character."
  (typecheck "char-upcase" char "character")
  (char.toLowerCase))

;; -----------------------------------------------------------------------------
(define (char-upper-case? char)
  "(char-upper-case? char)

   Function check if character is upper case."
  (typecheck "char-upper-case?" char "character")
  (and (char-alphabetic? char)
       (char=? (char-upcase char) char)))

;; -----------------------------------------------------------------------------
(define (char-lower-case? char)
  "(char-upper-case? char)

   Function check if character is lower case."
  (typecheck "char-lower-case?" char "character")
  (and (char-alphabetic? char)
       (char=? (char-downcase char) char)))

;; -----------------------------------------------------------------------------
(define (newline . rest)
  "(newline [port])

   Write newline character to standard output or given port"
  (let ((port (if (null? rest) (current-output-port) (car rest))))
    (display "\n" port)))

;; -----------------------------------------------------------------------------
(define (write obj . rest)
  "(write obj [port])

   Write object to standard output or give port. For strings it will include
   wrap in quotes."
  (let ((port (if (null? rest) (current-output-port) (car rest))))
    (display (repr obj true) port)))

;; -----------------------------------------------------------------------------
(define (write-char char . rest)
  "(write-char char [port])

   Write single character to given port using write function."
  (typecheck "write-char" char "character")
  (if (not (null? rest))
      (typecheck "write-char" (car rest) "output-port"))
  (apply display (cons (char.valueOf) rest)))

;; -----------------------------------------------------------------------------
(define fold-right reduce)
(define fold-left fold)

;; -----------------------------------------------------------------------------
(define (make-vector n . rest)
  "(make-vector n [fill])

   Create new vector with n empty elements. If fill is specified it will set
   all elements of the vector to that value."
  (let ((result (new Array n)))
    (if (not (null? rest))
        (--> result (fill (car rest)))
        result)))

;; -----------------------------------------------------------------------------
(define (vector? n)
  "(vector? n)

   Function return true of value is vector and false if not."
  (string=? (type n) "array"))

;; -----------------------------------------------------------------------------
(define (vector-ref vec n)
  "(vector-ref vec n)

   Function return nth element of the vector vec."
  (typecheck "vector-ref" vec "array" 1)
  (typecheck "vector-ref" n "number" 2)
  (. vec n))

;; -----------------------------------------------------------------------------
(define (vector-set! vec n value)
  "(vector-set! vec n value)

   Function set nth item of the vector to value."
  (typecheck "vector-ref" vec "array" 1)
  (typecheck "vector-ref" n "number" 2)
  (set-obj! vec n value))

;; -----------------------------------------------------------------------------
(define (vector-fill! vec value)
  "(vector-fill! vec value)

   Set every element of the vector to given value."
  (typecheck "vector-ref" vec "array")
  (let recur ((n (- (length vec) 1)))
    (if (>= n 0)
        (begin
          (set-obj! vec n value)
          (recur (- n 1))))))

;; -----------------------------------------------------------------------------
(define (vector-length vec)
  "(vector-length vec)

   Function return length of the vector. If argument is not vector it throw exception."
  (typecheck "vector-length" vec "array")
  (length vec))

;; -----------------------------------------------------------------------------
;; case macro from R7RS spec https://small.r7rs.org/wiki/R7RSSmallErrata/
;; -----------------------------------------------------------------------------
(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else => result))
     (result key))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...))))
  "(case value
        ((<items>) result1)
        ((<items>) result2)
        [else result3])

   Macro for switch case statement. It test if value is any of the item. If
   item match the value it will return coresponding result expression value.
   If no value match and there is else it will return that result.")

;; -----------------------------------------------------------------------------
(--> lips.Formatter.defaults.exceptions.specials (push "case")) ;; 2 indent

;; -----------------------------------------------------------------------------
(define (numerator n)
  "(numerator n)

   Return numberator of rational or same number if n is not rational."
  (typecheck "numerator" n "number")
  (if (and (rational? n) (not (integer? n)))
      n.num
      n))

;; -----------------------------------------------------------------------------
(define (denominator n)
  "(denominator n)

   Return denominator of rational or same number if one is not rational."
  (typecheck "denominator" n "number")
  (if (and (rational? n) (not (integer? n)))
      n.denom
      (if (exact? n) 1 1.0)))

;; -----------------------------------------------------------------------------
(define (imag-part n)
  "(imag-part n)

   Return imaginary part of the complex number n."
  (typecheck "imag-part" n "number")
  (if (%number-type "complex" n)
      n.__im__
      0))

;; -----------------------------------------------------------------------------
(define (real-part n)
  "(real-part n)

   Return real part of the complex number n."
  (typecheck "real-part" n "number")
  (if (%number-type "complex" n)
      n.__re__
      n))

;; -----------------------------------------------------------------------------
(define (make-polar r angle)
  "(make-polar magnitude angle)

   Create new complex number from polar parameters."
  (typecheck "make-polar" r "number")
  (typecheck "make-polar" angle "number")
  (if (or (complex? r) (complex? angle))
      (error "make-polar: argument can't be complex")
      (let ((re (* r (sin angle)))
            (im (* r (cos angle))))
        (make-rectangular im re))))

;; -----------------------------------------------------------------------------
(define (angle x)
  "(angle x)

   Returns angle of the complex number in polar coordinate system."
  (if (not (%number-type "complex" x))
      (error "angle: number need to be complex")
      (Math.atan2 x.__im__ x.__re__)))

;; -----------------------------------------------------------------------------
(define (magnitude x)
  "(magnitude x)

   Returns magnitude of the complex number in polar coordinate system."
  (if (not (%number-type "complex" x))
      (error "magnitude: number need to be complex")
      (sqrt (+ (* x.__im__ x.__im__) (* x.__re__ x.__re__)))))

;; -----------------------------------------------------------------------------
;; ref: https://stackoverflow.com/a/14675103/387194
;; -----------------------------------------------------------------------------
(define random
  (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
    (lambda new-seed
      "(random)
       (random seed)

       Function generate new random real number using Knuth algorithm."
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (modulo (+ (* seed a) c) m)))
      (exact->inexact (/ seed m)))))

;; -----------------------------------------------------------------------------
(define (eof-object? obj)
  "(eof-object? arg)

   Function check if value is eof object, returned from input string
   port when there are no more data to read."
  (eq? obj eof))

;; -----------------------------------------------------------------------------
(define (output-port? obj)
  "(output-port? arg)

   Function return true if argument is output port."
  (instanceof lips.OutputPort obj))

;; -----------------------------------------------------------------------------
(define (input-port? obj)
  "(input-port? arg)

   Function return true if argument is input port."
  (instanceof lips.InputPort obj))

;; -----------------------------------------------------------------------------
(define (char-ready? . rest)
  "(char-ready?)
   (char-ready? port)

   Function check it characters is ready in input port. This is usefull mostly
   for interactive ports that return false if it would wait for user input.
   It return false if port is closed."
  (let ((port (if (null? rest) (current-input-port) (car rest))))
    (typecheck "char-ready?" port "input-port")
    (port.char_ready)))

;; -----------------------------------------------------------------------------
(define open-input-file
  (let ((readFile #f))
    (lambda(filename)
      "(open-input-file filename)

       Function return new Input Port with given filename. In Browser user need to
       provide global fs variable that is instance of FS interface."
      (let ((fs (--> lips.env (get '**internal-env**) (get 'fs))))
        (if (null? fs)
            (throw (new Error "open-input-file: fs not defined"))
            (begin
              (if (not (procedure? readFile))
                  (let ((_readFile (promisify fs.readFile)))
                    (set! readFile (lambda (filename)
                                     (--> (_readFile filename) (toString))))))
              (new lips.InputFilePort (readFile filename) filename)))))))

;; -----------------------------------------------------------------------------
(define (close-input-port port)
  "(close-input-port port)

   Procedure close port that was opened with open-input-file. After that
   it no longer accept reading from that port."
  (typecheck "close-input-port" port "input-port")
  (port.close))

;; -----------------------------------------------------------------------------
(define (close-output-port port)
  "(close-output-port port)

   Procedure close port that was opened with open-output-file. After that
   it no longer accept write to that port."
  (typecheck "close-output-port" port "output-port")
  (port.close))

;; -----------------------------------------------------------------------------
(define (call-with-input-file filename proc)
  "(call-with-input-file filename proc)

   Procedure open file for reading, call user defined procedure with given port
   and then close the port. It return value that was returned by user proc
   and it close the port even if user proc throw exception."
  (let ((p (open-input-file filename)))
    (try (proc p)
         (finally
          (close-input-port p)))))

;; -----------------------------------------------------------------------------
(define (call-with-output-file filename proc)
  "(call-with-output-file filename proc)

   Procedure open file for writing, call user defined procedure with port
   and then close the port. It return value that was returned by user proc and it close the port
   even if user proc throw exception."
  (let ((p (open-output-file filename)))
    (try (proc p)
         (finally
          (close-output-port p)))))

;; -----------------------------------------------------------------------------
(define (with-input-from-file string thunk)
  "(with-input-from-file string thunk)

   Procedure open file and make it current-input-port then thunk is executed.
   After thunk is executed current-input-port is restored and file port
   is closed."
  (let* ((port (open-input-file string))
         (env **interaction-environment**)
         (internal-env (env.get '**internal-env**))
         (old-stdin (internal-env.get "stdin")))
    (internal-env.set "stdin" port)
    (try
     (thunk)
     (finally
      (internal-env.set "stdin" old-stdin)
      (close-input-port port)))))

;; -----------------------------------------------------------------------------
(define (with-output-to-file string thunk)
  (let* ((port (open-output-file string))
         (env **interaction-environment**)
         (internal-env (env.get '**internal-env**))
         (old-stdout (internal-env.get "stdout")))
    (internal-env.set "stdout" port)
    (try
     (thunk)
     (finally
      (internal-env.set "stdout" old-stdout)
      (close-output-port port)))))

;; -----------------------------------------------------------------------------
(define (file-exists? filename)
  (new Promise (lambda (resolve)
                 (let ((fs (--> lips.env (get '**internal-env**) (get 'fs))))
                   (if (null? fs)
                       (throw (new Error "file-exists?: fs not defined"))
                       (fs.stat filename (lambda (err stat)
                                           (if (null? err)
                                               (resolve (stat.isFile))
                                               (resolve #f)))))))))



;; -----------------------------------------------------------------------------
(define open-output-file
  (let ((open))
    (lambda (filename)
      "(open-output-file filename)

       Function open file and return port that can be used for writing. If file
       exists it will throw an Error."
      (typecheck "open-output-file" filename "string")
      (if (not (procedure? open))
          (set! open (%fs-promisify-proc 'open "open-output-file")))
      (if (file-exists? filename)
          (throw (new Error "open-output-file: file exists"))
          (lips.OutputFilePort filename (open filename "w"))))))

;; -----------------------------------------------------------------------------
(define (scheme-report-environment version)
  "(scheme-report-environment version)

   Function return new Environment object for given Scheme Spec version.
   Only argument 5 is supported that create environemnt for R5RS."
  (typecheck "scheme-report-environment" version "number")
  (case version
    ((5) (%make-env "R5RS" * + - / < <= = > >= abs acos and angle append apply asin assoc assq assv
                    atan begin boolean? caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr
                    cadar caddar cadddr caddr cadr call-with-current-continuation call-with-input-file
                    call-with-output-file call-with-values car case cdaaar cdaadr cdaar cdadar cdaddr
                    cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr ceiling char->integer
                    char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase
                    char-lower-case? char-numeric?  char-ready?  char-upcase char-upper-case?
                    char-whitespace? char<=? char<? char=? char>=? char>? char? close-input-port
                    close-output-port complex? cond cons cos current-input-port current-output-port
                    define define-syntax delay denominator display do dynamic-wind eof-object? eq?
                    equal? eqv? eval even? exact->inexact exact? exp expt floor for-each force gcd
                    if imag-part inexact->exact inexact? input-port? integer->char integer?
                    interaction-environment lambda lcm length let let* let-syntax letrec letrec-syntax
                    list list->string list->vector list-ref list-tail list? load log magnitude
                    make-polar make-rectangular make-string make-vector map max member memq memv min
                    modulo negative? newline not null-environment null? number->string number?
                    numerator odd? open-input-file open-output-file or output-port? pair? peek-char
                    positive? procedure? quasiquote quote quotient rational? rationalize read read-char
                    real-part real? remainder reverse round scheme-report-environment set! set-car!
                    set-cdr! sin sqrt string string->list string->number string->symbol string-append
                    string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? string-copy
                    string-fill! string-length string-ref string-set! string<=? string<? string=?
                    string>=? string>? string? substring symbol->string symbol? tan truncate values
                    vector vector->list vector-fill! vector-length vector-ref vector-set! vector?
                    with-input-from-file with-output-to-file write write-char zero?))
     ((7) (throw (new Error "not yet implemented")) #;(%make-env "R7RS"))
      (else (throw (new Error (string-append "scheme-report-environment: version "
                                             (number->string version)
                                             " not supported"))))))

;; -----------------------------------------------------------------------------
