;; -*- scheme -*-
;; Attempt to implement R5RS standard on top of LIPS
;;
;; Reference:
;; https://schemers.org/Documents/Standards/R5RS/HTML/
;;
;; This file is part of the LIPS - Scheme based Powerful LISP in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;
;; (+ 1 (call-with-current-continuation
;;       (lambda (escape)
;;         (+ 2 (escape 3)))))

;; -----------------------------------------------------------------------------
(define (%doc string fn)
  (typecheck "doc" fn "function")
  (typecheck "doc" string "string")
  (set-obj! fn '__doc__ (--> string (replace /^ +/mg "")))
  fn)

(define #f false)
(define #t true)
;; -----------------------------------------------------------------------------
(define string-append concat)
(define = ==)
(define remainder %)
(define -inf.0 Number.NEGATIVE_INFINITY)
(define +inf.0 Number.POSITIVE_INFINITY)
(define procedure? function?)
(define print display) ;; old LIPS function
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
        (add-special! ,symbol ',name ,(string->symbol
                                       (concat "lips.specials."
                                               (symbol->string type))))
        (define-macro (,name ,@args) ,@rest))))

;; -----------------------------------------------------------------------------
;; Vector literals syntax using parser symbol macros
;; -----------------------------------------------------------------------------
(define-symbol-macro SPLICE (vector "#" . arg)
  "(make-vector (1 2 3))
   #(1 2 3)

   Macro for defining vectors (arrays)."
  `(list->array (list ,@arg)))

;; -----------------------------------------------------------------------------
(define-symbol-macro SPLICE (quote-vector "'#" . arg)
  "(make-vector (1 2 3))
   #(1 2 3)

   Macro for defining vectors (arrays)."
  `(list->array (list ,@(map (lambda (object) `(quote ,object)) arg))))

;; -----------------------------------------------------------------------------
(define (eqv? a b)
  "(eqv? a b)

   Function compare the values. It return true if they are the same, they
   need to have same type"
  (if (eq? (type a) (type b))
      (cond ((number? a) (= a b))
            ((pair? a) (and (null? a) (null? b)))
            (else (eq? a b)))
      false))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
(define (every . args)
  "(every . args)

   Function return true if every argument is true otherwise it return false."
  (= (length args) (length (filter (lambda (x) x) args))))

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
(define-macro (delay expression)
  "(delay expression)

   Macro will create a promise from expression that can be forced with force."
  `(make-promise (lambda () ,expression)))

;; -----------------------------------------------------------------------------
(define (force promise)
  "(force promise)

   Function force the promise and evaluate delayed expression."
  (promise))

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
  (typecheck "quotient" x "number")
  (/ a b))

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
   (eq? (type x) "boolean"))

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
(define (list? x)
  "(list? x)

   Function test if value is proper linked list structure.
   The car of each pair can be any value. It return false on cycles."
  (or (eq? x nil) ;; empty list
      (and (pair? x)
           (or (null? (cdr x))
               (and (not (x.haveCycles "cdr"))
                    (list? (cdr x)))))))

;; -----------------------------------------------------------------------------
(define (%number-type type x)
  (and (number? x) (eq? (. x 'type) type)))

;; -----------------------------------------------------------------------------
(define integer? (%doc
                  ""
                  (curry %number-type "bigint")))

;; -----------------------------------------------------------------------------
(define complex? (%doc
                  ""
                  (curry %number-type "complex")))

;; -----------------------------------------------------------------------------
(define rational? (%doc
                  ""
                  (curry %number-type "rational")))

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
  (lips.LNumber (make-object :im im :re re)))

;; -----------------------------------------------------------------------------
(define (real? n)
  "(real? n)"
  (and (number? n) (eq? (. n 'type) "float")))

;; -----------------------------------------------------------------------------
(define (exact? n)
  "(exact? n)"
  (typecheck "exact?" n "number")
  (let ((type (. n 'type)))
    (or (eq? type "bigint") (eq? type "rational"))))

(define (inexact? n)
  "(inexact? n)"
  (typecheck "inexact?" n "number")
  (not (exact? n)))

;; -----------------------------------------------------------------------------
(define (exact->inexact n)
  "(exact->inexact n)

   Convert exact number to inexact."
  (typecheck "exact->inexact" n "number")
  (lips.LFloat (--> n (valueOf))))

;; -----------------------------------------------------------------------------
(define (inexact->exact n)
  "(inexact->exact number)

   Funcion convert real number to exact ratioanl number."
  (typecheck "inexact->exact" n "number")
  (if (real? n)
      (--> n (toRational 1e-20))
      n))

(add-special! "#i" 'exact->inexact lips.specials.LITERAL)
(add-special! "#e" 'inexact->exact lips.specials.LITERAL)

;; -----------------------------------------------------------------------------
;; generate Math functions with documentation
(define _maths (list "exp" "log" "sin" "cos" "tan" "asin" "acos" "atan" "atan"))

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
(define (quotient a b)
  "(quotient a b)

   Function return integer part from division operation."
  (typecheck "quotient" a "number" 1)
  (typecheck "quotient" b "number" 2)
  (truncate (/ a b)))

;; -----------------------------------------------------------------------------
(define (list . args)
  "(list . args)

   Function create new list out of its arguments."
  args)

;; -----------------------------------------------------------------------------
(define (list-tail x k)
  "(list-tail x k)

   Function return tail of k element of a list."
  (if (zero? k)
      x
      (list-tail (cdr x) (- k 1))))

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
  (typecheck "memq" list (vector "nil" "pair"))
  (%mem/search car eq? obj list ))

;; -----------------------------------------------------------------------------
(define (memv obj list)
  "(memv obj list)

   Function return first object in the list that match using eqv? function."
  (typecheck "memv" list (vector "nil" "pair"))
  (%mem/search car eqv? obj list))

;; -----------------------------------------------------------------------------
(define (member obj list)
  "(member obj list)

   Function return first object in the list that match using equal? function."
  (typecheck "member" list (vector "nil" "pair"))
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
;; (let ((x "xxxxxxxxxx"))
;;    (string-fill! x #\b)
;;    x)
(define-macro (string-fill! string char)
  "(string-fill! symbol char)

   Macro is used to set all characters in string as char.
   In LISP strings are JavaScript strings so this need to be macro
   that overwrite slot in environement with new strings."
  (typecheck "string-fill!" string "symbol" 1)
  (let ((len (gensym))
        (char-name (gensym))
        (result (gensym)))
    `(let ((,char-name ,char))
       (typecheck "string-fill!" ,char-name "character" 2)
       (let iter ((,len (string-length ,string)) (,result '()))
         (if (<= ,len 0)
             (set! ,string (list->string ,result))
             (iter (- ,len 1) (cons ,char-name ,result)))))))

;; -----------------------------------------------------------------------------
(define (identity n)
  "(identity n)

   No op function. it just returns its argument."
  n)

;; -----------------------------------------------------------------------------
;; in JavaScript strings are not mutable not need to copy
(define string-copy identity)

;; -----------------------------------------------------------------------------
(define (list->string _list)
  "(list->string _list)

   Function return string from list of characters."
  (let ((array (list->array
                (map (lambda (x)
                       (typecheck "list->string" x "character")
                       (. x 'char))
                     _list))))
    (--> array (join ""))))

;; -----------------------------------------------------------------------------
(define (string->list string)
  "(string->list string)

   Function return list of characters created from list."
  (typecheck "string->list" string "string")
  (array->list (--> string (split "") (map (lambda (x) (lips.Character x))))))

;; -----------------------------------------------------------------------------
;; (let ((x "hello")) (string-set! x 0 #\H) x)
(define-macro (string-set! object index char)
  "(string-set! object index char)

   Macro that replace character in string in given index, it create new JavaScript
   string and replace old value. Object need to be symbol that point to variable
   that hold the string."
  (typecheck "string-set!" object "symbol")
  (let ((chars (gensym)))
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
  (lips.Character (. string k)))

;; -----------------------------------------------------------------------------
;; CHARACTER FUNCTIONS
;; -----------------------------------------------------------------------------

;; (display (list->string (list #\A (integer->char 10) #\B)))
;; -----------------------------------------------------------------------------
(define char? (%doc
        "(char? obj)

         Function check if object is character."
        (curry instanceof lips.Character)))

;; -----------------------------------------------------------------------------
(define (char->integer chr)
  "(char->integer chr)

   Function return codepoint of Unicode character."
  (typecheck "char->integer" chr "character")
  (--> chr.char (codePointAt 0)))

;; -----------------------------------------------------------------------------
(define (integer->char n)
  "(char->integer chr)

   Function convert number argument to chararacter."
  (typecheck "integer->char" n "number")
  (if (integer? n)
      (string-ref (String.fromCodePoint n) 0)
      (throw "argument to integer->char need to be integer.")))

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
(define (char<=? chr1 chr2)
  "(char<? chr1 chr2)

   Function return true if second character is not smaller then the first one."
  (> (%char-cmp "char>=?" chr1 chr2) -1))

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
(define (newline)
  "(newline)

   Write newline character to standard output."
  (display "\n"))

;; -----------------------------------------------------------------------------
(define (write obj . rest)
  "(write obj [port])

   Write object to standard output or give port. For strings it will include
   wrap in quotes."
  (let ((port (if (null? rest) (current-output-port) (car rest))))
    (port.write (repr obj true))))

;; -----------------------------------------------------------------------------
(define (write-char char . rest)
  "(write-char char [port])

   Write single character to given port using write function."
  (typecheck "write-char" char "character")
  (apply write (cons (char.valueOf) rest)))

(define fold-right reduce)

(define (fold-left fn init . lists)
  "(fold-left fn init . lists)

   Similar higher order function to reduce (fold-right) that apply the function
   on each element in reverse order and return single combined value."
  (typecheck "fold-left" fn "function")
  (for-each (lambda (x)
              (typecheck "fold-left" x "pair"))
            lists)
  (let ((lists (map reverse lists)))
    (apply reduce fn init lists)))
