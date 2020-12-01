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
;; Copyright (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;
;; (+ 1 (call-with-current-continuation
;;       (lambda (escape)
;;         (+ 2 (escape 3)))))
;;
;; Unicode support:
;;    codepoints and regular expressions where generated at 30-11-2020
;;    so it support Unicode 12.1 released on May 2019
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
      (list->array args)))

;; -----------------------------------------------------------------------------
(define-syntax vector
  (syntax-rules ()
    ((_ arg ...) (list->array (list arg ...))))
  "(vector 1 2 3 (+ 3 1))
   #(1 2 3 (+ 3 1))

   Macro for defining vectors (JavaScript arrays).")

;; -----------------------------------------------------------------------------
(set-repr! Array
           (lambda (x q)
             (let ((arr (--> x (map (lambda (x) (repr x q))))))
               (concat "#(" (--> arr (join " ")) ")"))))

;; -----------------------------------------------------------------------------
(define (eqv? a b)
  "(eqv? a b)

   Function compare the values. It return true if they are the same, they
   need to have same type"
  (if (string=? (type a) (type b))
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
        ((and (symbol? a) (symbol? b))
         (equal? a.__name__ b.__name__))
        ((and (regex? a) (regex? b))
         (equal? (. a 'source) (. b 'source)))
        ((and (vector? a) (vector? b))
         (and (= (length a) (length b))
              (--> a (every (lambda (item i)
                              (equal? item (vector-ref b i)))))))
        ((and (string? a) (string? b))
         (string=? a b))
        ((and (function? a) (function? b))
         (%same-functions a b))
        ((and (array? a) (array? b) (eq? (length a) (length b)))
         (= (--> a (filter (lambda (item i) (equal? item (. b i)))) 'length) (length a)))
        ((and (plain-object? a) (plain-object? b))
         (let ((keys_a (--> (Object.keys a) (sort)))
               (keys_b (--> (Object.keys b) (sort))))
           (and (= (length keys_a)
                   (length keys_b))
                (equal? keys_a keys_b)
                (equal? (--> keys_a (map (lambda (key) (. a key))))
                        (--> keys_b (map (lambda (key) (. b key))))))))
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
  (let* ((t x.__type__)
         (typeof (lambda (type) (string=? t type))))
    (and (number? x)
         (if (pair? type)
             (some typeof type)
             (typeof type)))))

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
                  (curry %number-type '("rational" "bigint"))))

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
    (lips.LComplex (--> value (toObject true)))))

;; -----------------------------------------------------------------------------
(define (real? n)
  "(real? n)"
  (and (number? n) (let ((type n.__type__))
                     (or (string=? type "float")
                         (string=? type "bigint")))))

;; -----------------------------------------------------------------------------
(define (exact? n)
  "(exact? n)"
  (typecheck "exact?" n "number")
  (let ((type n.__type__))
    (or (string=? type "bigint")
        (string=? type "rational")
        (and (string=? type "complex")
             (exact? n.im)
             (exact? n.re)))))

(define (inexact? n)
  "(inexact? n)"
  (typecheck "inexact?" n "number")
  (not (exact? n)))

;; -----------------------------------------------------------------------------
(define (exact->inexact n)
  "(exact->inexact n)

   Convert exact number to inexact."
  (typecheck "exact->inexact" n "number")
  (if (complex? n)
      ;; make-object (&) will use valueOf so it will be float even if it was rational
      (lips.LComplex &(:im (. n 'im) :re (. n 're)))
      (if (or (rational? n) (integer? n))
          (lips.LFloat (--> n (valueOf)) true)
          n)))

;; -----------------------------------------------------------------------------
(define (inexact->exact n)
  "(inexact->exact number)

   Funcion convert real number to exact ratioanl number."
  (typecheck "inexact->exact" n "number")
  (if (or (real? n) (complex? n))
      (--> n (toRational))
      n))

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
          (if (null? l)
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
  /\s/)

;; -----------------------------------------------------------------------------
;; compiled from /\p{N}/
;; -----------------------------------------------------------------------------
(define *numeral-unicode-regex* /(?:[0-9\xB2\xB3\xB9\xBC-\xBE\u0660-\u0669\u06F0-\u06F9\u07C0-\u07C9\u0966-\u096F\u09E6-\u09EF\u09F4-\u09F9\u0A66-\u0A6F\u0AE6-\u0AEF\u0B66-\u0B6F\u0B72-\u0B77\u0BE6-\u0BF2\u0C66-\u0C6F\u0C78-\u0C7E\u0CE6-\u0CEF\u0D58-\u0D5E\u0D66-\u0D78\u0DE6-\u0DEF\u0E50-\u0E59\u0ED0-\u0ED9\u0F20-\u0F33\u1040-\u1049\u1090-\u1099\u1369-\u137C\u16EE-\u16F0\u17E0-\u17E9\u17F0-\u17F9\u1810-\u1819\u1946-\u194F\u19D0-\u19DA\u1A80-\u1A89\u1A90-\u1A99\u1B50-\u1B59\u1BB0-\u1BB9\u1C40-\u1C49\u1C50-\u1C59\u2070\u2074-\u2079\u2080-\u2089\u2150-\u2182\u2185-\u2189\u2460-\u249B\u24EA-\u24FF\u2776-\u2793\u2CFD\u3007\u3021-\u3029\u3038-\u303A\u3192-\u3195\u3220-\u3229\u3248-\u324F\u3251-\u325F\u3280-\u3289\u32B1-\u32BF\uA620-\uA629\uA6E6-\uA6EF\uA830-\uA835\uA8D0-\uA8D9\uA900-\uA909\uA9D0-\uA9D9\uA9F0-\uA9F9\uAA50-\uAA59\uABF0-\uABF9\uFF10-\uFF19]|\uD800[\uDD07-\uDD33\uDD40-\uDD78\uDD8A\uDD8B\uDEE1-\uDEFB\uDF20-\uDF23\uDF41\uDF4A\uDFD1-\uDFD5]|\uD801[\uDCA0-\uDCA9]|\uD802[\uDC58-\uDC5F\uDC79-\uDC7F\uDCA7-\uDCAF\uDCFB-\uDCFF\uDD16-\uDD1B\uDDBC\uDDBD\uDDC0-\uDDCF\uDDD2-\uDDFF\uDE40-\uDE48\uDE7D\uDE7E\uDE9D-\uDE9F\uDEEB-\uDEEF\uDF58-\uDF5F\uDF78-\uDF7F\uDFA9-\uDFAF]|\uD803[\uDCFA-\uDCFF\uDD30-\uDD39\uDE60-\uDE7E\uDF1D-\uDF26\uDF51-\uDF54\uDFC5-\uDFCB]|\uD804[\uDC52-\uDC6F\uDCF0-\uDCF9\uDD36-\uDD3F\uDDD0-\uDDD9\uDDE1-\uDDF4\uDEF0-\uDEF9]|\uD805[\uDC50-\uDC59\uDCD0-\uDCD9\uDE50-\uDE59\uDEC0-\uDEC9\uDF30-\uDF3B]|\uD806[\uDCE0-\uDCF2\uDD50-\uDD59]|\uD807[\uDC50-\uDC6C\uDD50-\uDD59\uDDA0-\uDDA9\uDFC0-\uDFD4]|\uD809[\uDC00-\uDC6E]|\uD81A[\uDE60-\uDE69\uDF50-\uDF59\uDF5B-\uDF61]|\uD81B[\uDE80-\uDE96]|\uD834[\uDEE0-\uDEF3\uDF60-\uDF78]|\uD835[\uDFCE-\uDFFF]|\uD838[\uDD40-\uDD49\uDEF0-\uDEF9]|\uD83A[\uDCC7-\uDCCF\uDD50-\uDD59]|\uD83B[\uDC71-\uDCAB\uDCAD-\uDCAF\uDCB1-\uDCB4\uDD01-\uDD2D\uDD2F-\uDD3D]|\uD83C[\uDD00-\uDD0C]|\uD83E[\uDFF0-\uDFF9])/)

;; -----------------------------------------------------------------------------
;; compiled from /\p{L}/u
;; -----------------------------------------------------------------------------
(define *letter-unicode-regex* /(?:[A-Za-z\xAA\xB5\xBA\xC0-\xD6\xD8-\xF6\xF8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u037F\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u052F\u0531-\u0556\u0559\u0560-\u0588\u05D0-\u05EA\u05EF-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u0860-\u086A\u08A0-\u08B4\u08B6-\u08C7\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0980\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u09FC\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0AF9\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C39\u0C3D\u0C58-\u0C5A\u0C60\u0C61\u0C80\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D04-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D54-\u0D56\u0D5F-\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E86-\u0E8A\u0E8C-\u0EA3\u0EA5\u0EA7-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F5\u13F8-\u13FD\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u16F1-\u16F8\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1878\u1880-\u1884\u1887-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191E\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19B0-\u19C9\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1C80-\u1C88\u1C90-\u1CBA\u1CBD-\u1CBF\u1CE9-\u1CEC\u1CEE-\u1CF3\u1CF5\u1CF6\u1CFA\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312F\u3131-\u318E\u31A0-\u31BF\u31F0-\u31FF\u3400-\u4DBF\u4E00-\u9FFC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA69D\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA7BF\uA7C2-\uA7CA\uA7F5-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA8FD\uA8FE\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uA9E0-\uA9E4\uA9E6-\uA9EF\uA9FA-\uA9FE\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA7E-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uAB30-\uAB5A\uAB5C-\uAB69\uAB70-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]|\uD800[\uDC00-\uDC0B\uDC0D-\uDC26\uDC28-\uDC3A\uDC3C\uDC3D\uDC3F-\uDC4D\uDC50-\uDC5D\uDC80-\uDCFA\uDE80-\uDE9C\uDEA0-\uDED0\uDF00-\uDF1F\uDF2D-\uDF40\uDF42-\uDF49\uDF50-\uDF75\uDF80-\uDF9D\uDFA0-\uDFC3\uDFC8-\uDFCF]|\uD801[\uDC00-\uDC9D\uDCB0-\uDCD3\uDCD8-\uDCFB\uDD00-\uDD27\uDD30-\uDD63\uDE00-\uDF36\uDF40-\uDF55\uDF60-\uDF67]|\uD802[\uDC00-\uDC05\uDC08\uDC0A-\uDC35\uDC37\uDC38\uDC3C\uDC3F-\uDC55\uDC60-\uDC76\uDC80-\uDC9E\uDCE0-\uDCF2\uDCF4\uDCF5\uDD00-\uDD15\uDD20-\uDD39\uDD80-\uDDB7\uDDBE\uDDBF\uDE00\uDE10-\uDE13\uDE15-\uDE17\uDE19-\uDE35\uDE60-\uDE7C\uDE80-\uDE9C\uDEC0-\uDEC7\uDEC9-\uDEE4\uDF00-\uDF35\uDF40-\uDF55\uDF60-\uDF72\uDF80-\uDF91]|\uD803[\uDC00-\uDC48\uDC80-\uDCB2\uDCC0-\uDCF2\uDD00-\uDD23\uDE80-\uDEA9\uDEB0\uDEB1\uDF00-\uDF1C\uDF27\uDF30-\uDF45\uDFB0-\uDFC4\uDFE0-\uDFF6]|\uD804[\uDC03-\uDC37\uDC83-\uDCAF\uDCD0-\uDCE8\uDD03-\uDD26\uDD44\uDD47\uDD50-\uDD72\uDD76\uDD83-\uDDB2\uDDC1-\uDDC4\uDDDA\uDDDC\uDE00-\uDE11\uDE13-\uDE2B\uDE80-\uDE86\uDE88\uDE8A-\uDE8D\uDE8F-\uDE9D\uDE9F-\uDEA8\uDEB0-\uDEDE\uDF05-\uDF0C\uDF0F\uDF10\uDF13-\uDF28\uDF2A-\uDF30\uDF32\uDF33\uDF35-\uDF39\uDF3D\uDF50\uDF5D-\uDF61]|\uD805[\uDC00-\uDC34\uDC47-\uDC4A\uDC5F-\uDC61\uDC80-\uDCAF\uDCC4\uDCC5\uDCC7\uDD80-\uDDAE\uDDD8-\uDDDB\uDE00-\uDE2F\uDE44\uDE80-\uDEAA\uDEB8\uDF00-\uDF1A]|\uD806[\uDC00-\uDC2B\uDCA0-\uDCDF\uDCFF-\uDD06\uDD09\uDD0C-\uDD13\uDD15\uDD16\uDD18-\uDD2F\uDD3F\uDD41\uDDA0-\uDDA7\uDDAA-\uDDD0\uDDE1\uDDE3\uDE00\uDE0B-\uDE32\uDE3A\uDE50\uDE5C-\uDE89\uDE9D\uDEC0-\uDEF8]|\uD807[\uDC00-\uDC08\uDC0A-\uDC2E\uDC40\uDC72-\uDC8F\uDD00-\uDD06\uDD08\uDD09\uDD0B-\uDD30\uDD46\uDD60-\uDD65\uDD67\uDD68\uDD6A-\uDD89\uDD98\uDEE0-\uDEF2\uDFB0]|\uD808[\uDC00-\uDF99]|\uD809[\uDC80-\uDD43]|[\uD80C\uD81C-\uD820\uD822\uD840-\uD868\uD86A-\uD86C\uD86F-\uD872\uD874-\uD879\uD880-\uD883][\uDC00-\uDFFF]|\uD80D[\uDC00-\uDC2E]|\uD811[\uDC00-\uDE46]|\uD81A[\uDC00-\uDE38\uDE40-\uDE5E\uDED0-\uDEED\uDF00-\uDF2F\uDF40-\uDF43\uDF63-\uDF77\uDF7D-\uDF8F]|\uD81B[\uDE40-\uDE7F\uDF00-\uDF4A\uDF50\uDF93-\uDF9F\uDFE0\uDFE1\uDFE3]|\uD821[\uDC00-\uDFF7]|\uD823[\uDC00-\uDCD5\uDD00-\uDD08]|\uD82C[\uDC00-\uDD1E\uDD50-\uDD52\uDD64-\uDD67\uDD70-\uDEFB]|\uD82F[\uDC00-\uDC6A\uDC70-\uDC7C\uDC80-\uDC88\uDC90-\uDC99]|\uD835[\uDC00-\uDC54\uDC56-\uDC9C\uDC9E\uDC9F\uDCA2\uDCA5\uDCA6\uDCA9-\uDCAC\uDCAE-\uDCB9\uDCBB\uDCBD-\uDCC3\uDCC5-\uDD05\uDD07-\uDD0A\uDD0D-\uDD14\uDD16-\uDD1C\uDD1E-\uDD39\uDD3B-\uDD3E\uDD40-\uDD44\uDD46\uDD4A-\uDD50\uDD52-\uDEA5\uDEA8-\uDEC0\uDEC2-\uDEDA\uDEDC-\uDEFA\uDEFC-\uDF14\uDF16-\uDF34\uDF36-\uDF4E\uDF50-\uDF6E\uDF70-\uDF88\uDF8A-\uDFA8\uDFAA-\uDFC2\uDFC4-\uDFCB]|\uD838[\uDD00-\uDD2C\uDD37-\uDD3D\uDD4E\uDEC0-\uDEEB]|\uD83A[\uDC00-\uDCC4\uDD00-\uDD43\uDD4B]|\uD83B[\uDE00-\uDE03\uDE05-\uDE1F\uDE21\uDE22\uDE24\uDE27\uDE29-\uDE32\uDE34-\uDE37\uDE39\uDE3B\uDE42\uDE47\uDE49\uDE4B\uDE4D-\uDE4F\uDE51\uDE52\uDE54\uDE57\uDE59\uDE5B\uDE5D\uDE5F\uDE61\uDE62\uDE64\uDE67-\uDE6A\uDE6C-\uDE72\uDE74-\uDE77\uDE79-\uDE7C\uDE7E\uDE80-\uDE89\uDE8B-\uDE9B\uDEA1-\uDEA3\uDEA5-\uDEA9\uDEAB-\uDEBB]|\uD869[\uDC00-\uDEDD\uDF00-\uDFFF]|\uD86D[\uDC00-\uDF34\uDF40-\uDFFF]|\uD86E[\uDC00-\uDC1D\uDC20-\uDFFF]|\uD873[\uDC00-\uDEA1\uDEB0-\uDFFF]|\uD87A[\uDC00-\uDFE0]|\uD87E[\uDC00-\uDE1D]|\uD884[\uDC00-\uDF4A])/)

;; -----------------------------------------------------------------------------
(%define-chr-re (char-numeric? chr)
  "(char-numeric? chr)

   Function return true if character is number."
  *numeral-unicode-regex*)

;; -----------------------------------------------------------------------------
(%define-chr-re (char-alphabetic? chr)
  "(char-alphabetic? chr)

   Function return true if character is leter of the ASCII alphabet."
  *letter-unicode-regex*)


;; -----------------------------------------------------------------------------
(define *zero-number-chars* #(48 1632 1776 1984 2406 2534 2662 2790 2918 3046 3174 3302
                              3430 3558 3664 3792 3872 4160 4240 6112 6160 6470 6608 6784
                              6800 6992 7088 7232 7248 42528 43216 43264 43472 43504 43600
                              44016 65296 66720 68912 69734 69872 69942 70096 70384 70736
                              70864 71248 71360 71472 71904 72016 72784 73040 73120 92768
                              93008 120782 120792 120802 120812 120822 123200 123632 125264
                              130032))

;; -----------------------------------------------------------------------------
(define (digit-value chr)
  "(digit-value chr)

   Return digit number if character is numeral (as per char-numeric?)
   or #f otherwise."
  (typecheck "digit-value" chr "character")
  (if (char-numeric? chr)
      (let ((ord (char->integer chr)))
        (do ((i (vector-length *zero-number-chars*) (- i 1))
             (found #f)
             (result #f))
            ((or (zero? i) found) result)
          (let* ((zero (vector-ref *zero-number-chars* (- i 1)))
                 (diff (- ord zero)))
            (if (and (>= diff 0) (<= diff 9))
                (begin
                 (set! result diff)
                 (set! found #t))))))
    #f))

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
    (port.write (repr obj true))))

;; -----------------------------------------------------------------------------
(define (write-char char . rest)
  "(write-char char [port])

   Write single character to given port using write function."
  (typecheck "write-char" char "character")
  (apply write (cons (char.valueOf) rest)))

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
;; case macro from R7RS spec https://small.r7rs.org/attachment/r7rs.pdf
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
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
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
  (if (complex? n)
      n.im
      0))

;; -----------------------------------------------------------------------------
(define (real-part n)
  "(real-part n)

   Return real part of the complex number n."
  (typecheck "real-part" n "number")
  (if (complex? n)
      n.re
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
  (if (not (complex? x))
      (error "angle: number need to be complex")
      (Math.atan2 x.im x.re)))

;; -----------------------------------------------------------------------------
(define (magnitude x)
  "(magnitude x)

   Returns magnitude of the complex number in polar coordinate system."
  (if (not (complex? x))
      (error "magnitude: number need to be complex")
      (sqrt (+ (* x.im x.im) (* x.re x.re)))))

;; ---------------------------------------------------------------------------------------
;; ref: https://stackoverflow.com/a/14675103/387194
;; ---------------------------------------------------------------------------------------
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
