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
;; Copyright (C) 2019-2024 Jakub T. Jankiewicz <https://jcubic.pl/me>
;; Released under MIT license

;; -----------------------------------------------------------------------------
(define (list-match? predicate list)
  "(list-match? predicate list)

   Checks if consecutive elements of the list match the predicate function."
  (typecheck "list-match?" predicate #("function" "macro"))
  (typecheck "list-match?" list "pair")
  (or (or (null? list)
          (null? (cdr list)))
      (and (predicate (car list) (cadr list))
           (list-match? predicate (cdr list)))))

;; -----------------------------------------------------------------------------
(define (symbol=? . args)
  "(symbol=? s1 s2 ...)

   Checks if each value is symbol and it's the same according to string=? predicate."
  (list-match? (lambda (a b)
                 (and (symbol? a) (symbol? b) (equal? a b)))
               args))

;; -----------------------------------------------------------------------------
;; function for Gauche code
;; -----------------------------------------------------------------------------
(define (values-ref values n)
  "(values-ref values n)

   Returns n value of values object which is result of value function."
  (typecheck "values-ref" values "values" 1)
  (typecheck "values-ref" n "number" 1)
  (--> values (valueOf) n))

;; -----------------------------------------------------------------------------
(define-syntax let-values
  (syntax-rules (bind mktmp)
    ((let-values (binding ...) body0 body1 ...)
     (let-values bind
       (binding ...) () (begin body0 body1 ...)))
    ((let-values bind () tmps body)
     (let tmps body))
    ((let-values bind ((b0 e0) binding ...) tmps body)
     (let-values mktmp b0 e0 () (binding ...) tmps body))
    ((let-values mktmp () e0 args bindings tmps body)
     (call-with-values
         (lambda () e0)
       (lambda args
         (let-values bind
           bindings tmps body))))
    ((let-values mktmp (a . b) e0 (arg ...) bindings (tmp ...) body)
     (let-values mktmp b e0 (arg ... x) bindings (tmp ... (a x)) body))
    ((let-values mktmp a e0 (arg ...) bindings (tmp ...) body)
     (call-with-values
         (lambda () e0)
       (lambda (arg ... . x)
         (let-values bind bindings (tmp ... (a x)) body)))))
  "(let-values (binding ...) body ...)

   The macro work similar to let but variable is list of values and value
   need to evaluate to result of calling values.")

;; -----------------------------------------------------------------------------
(define (vector-append . args)
  "(vector-append v1 v2 ...)

   Returns new vector by combining it's arguments that should be vectors."
  (if (null? args)
      (vector)
      (begin
        (typecheck "vector-append" (car args) "array")
        (--> (car args) (concat (apply vector-append (cdr args)))))))

;; -----------------------------------------------------------------------------
(define (vector-copy vector . rest)
  "(vector-copy vector)
   (vector-copy vector start)
   (vector-copy vector start end)

   Returns a new vecotor that is a copy of given vector. If start
   is not provided it starts at 0, if end it's not provided it copy
   til the end of the given vector."
  (typecheck "vector-copy" vector "array")
  (let ((start (if (null? rest) 0 (car rest)))
        (end (if (or (null? rest) (null? (cdr rest))) vector.length (cadr rest))))
    (typecheck-number "vector-copy" start '("integer" "bigint"))
    (typecheck-number "vector-copy" end '("integer" "bigint"))
    (vector.slice start end)))

;; -----------------------------------------------------------------------------
(define (vector-copy! to at from . rest)
  "(vector-copy to at from)
   (vector-copy to at from start)
   (vector-copy to at from start end)

   Copies the elements of vector from between start and end into
   vector to starting at `at`. If start is missing it start at 0 and if end
   is missing it copy til the end of the vector from. It throws an error
   if vector from don't fit into the destination `to`."
  (typecheck "vector-copy!" to "array")
  (typecheck "vector-copy!" from "array")
  (typecheck-number "vector-copy!" at '("integer" "bigint"))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest)
                      (null? (cdr rest)))
                  from.length
                  (cadr rest))))
    (typecheck-number "vector-copy!" start '("integer" "bigint"))
    (typecheck-number "vector-copy!" end '("integer" "bigint"))
    (let ((len (- end start)))
      (if (< (- to.length at) len)
          (error "vector-copy!: Invalid index at"))
      (let ((source (from.slice start end)))
        (apply to.splice at len (vector->list source))))))

;; -----------------------------------------------------------------------------
(define-macro (%range-function spec . body)
  "(%range-function spec . body)

   Creates R7RS vector functions that have range start end."
  (let* ((name (car spec))
         (name-str (symbol->string name))
         (args (append (cdr spec) 'rest)))
    `(define (,name ,@args)
       ,(if (string? (car body))
            (car body))
       (let ((start (if (null? rest) 0 (car rest)))
             (end (if (or (null? rest) (null? (cdr rest)))
                      (. ,(car args) 'length)
                      (cadr rest))))
         (typecheck ,name-str start "number")
         (typecheck ,name-str end "number")
         ,@(if (string? (car body))
               (cdr body)
               body)))))

;; -----------------------------------------------------------------------------
(%range-function
 (vector->list vector)
 "(vector->list vector)
  (vector->list vector start)
  (vector->list vector start end)

  Function that copies given range of vector to list. If no start is specified it use
  start of the vector, if no end is specified it convert to the end of the vector."
 (typecheck "vector->list" vector "array")
 (array->list (vector.slice start end)))

;; -----------------------------------------------------------------------------
(%range-function
 (string->vector string)
 "(string->list string)
  (string->list string start)
  (string->list string start end)

  Function that copies given range of string to list. If no start is specified it use
  start of the string, if no end is specified it convert to the end of the string."
 (typecheck "string->vector" string "string")
 (--> (string.substring start end)
      (split "")
      (map (unary lips.LCharacter))))

;; -----------------------------------------------------------------------------
(%range-function
 (vector->string vector)
  "(vector->string vector)
   (vector->string vector start)
   (vector->string vector start end)

   Returns new string created from vector of characters in given range.
   If no start is given it create string from 0, if no end is given it return
   string to the end."
  (typecheck "vector->string" vector "array")
  (--> vector
       (slice start end)
       (map (lambda (char) (char.valueOf)))
       (join "")))

;; -----------------------------------------------------------------------------
(%range-function
 (vector-fill! vector fill)
 "(vector-fill! vector fill)
  (vector-fill! vector fill start)
  (vector-fill! vector fill start end)

  Fill vector with a given value in given range. If start is not given is start
  at 0. If end is not given it fill till the end if the vector."
 (typecheck "vector->fill!" vector "array")
 (let recur ((n (- end start)))
    (if (>= n start)
        (begin
          (set-obj! vector n fill)
          (recur (- n 1))))))

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

   More arguments will give an error.")

;; -----------------------------------------------------------------------------
(define (boolean=? . args)
  "(boolean=? b1 b2 ...)

   Checks if all arguments are boolean and if they are the same."
  (if (< (length args) 2)
      (error "boolean=?: too few arguments")
      (reduce (lambda (acc item)
                (and (boolean? item) (eq? acc item)))
              (car args)
              (cdr args))))

;; -----------------------------------------------------------------------------
(define (port? x)
  "(port? x)

   Returns true if the argument is an input or output port object."
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

   Executes body when test expression is true.")

;; -----------------------------------------------------------------------------
(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...))))
  "(unless test body ...)

   Executes body when test expression is false.")

;; -----------------------------------------------------------------------------
(define inexact exact->inexact)
(define exact inexact->exact)

;; -----------------------------------------------------------------------------
(define (exact-integer? n)
  "(exact-integer? n)

   Returns #t if z is both exact and an integer; otherwise
   returns #f."
  (and (integer? n) (exact? n)))

;; -----------------------------------------------------------------------------
(define (vector-map fn . rest)
  "(vector-map fn vector1 vector2 ...)

   Returns new vector from applying function fn to each element
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
(define (vector-for-each fn . rest)
  "(vector-for-each fn vector1 vector2 ...)

   Invokes every Returns new vector from applying function fn to each element
   of the vectors, similar to map for lists."
  (typecheck "vector-for-each" fn "function" 1)
  (if (or (= (length rest) 0) (not (every vector? rest)))
      (error "vector-for-each: function require at least 1 vector")
      (let ((len (apply min (map vector-length rest)))
            (result (vector)))
        (do ((i 0 (+ i 1)))
            ((= i len) undefined)
            (let* ((args (map (lambda (v) (vector-ref v i)) rest)))
              (apply fn args))))))

;; -----------------------------------------------------------------------------
(define (string-map fn . rest)
  "(string-map fn string1 stringr2 ...)

   Returns new string from applying function fn to each element
   of the strings, similar to map for lists."
  (typecheck "string-map" fn "function" 1)
  (if (or (= (length rest) 0) (not (every string? rest)))
      (error "string-map: function require at least 1 string")
      (vector->string (apply vector-map fn (map string->vector rest)))))

;; -----------------------------------------------------------------------------
(define (string-for-each fn . rest)
  "(string-for-each fn string1 stringr2 ...)

   Applies a function fn to each element of the strings, similar string-map.
   But the return value is undefined."
  (typecheck "string-for-each" fn "function" 1)
  (if (or (= (length rest) 0) (not (every string? rest)))
      (error "string-for-each: function require at least 1 string")
      (apply vector-for-each fn (map string->vector rest))))

;; -----------------------------------------------------------------------------
(define (string-downcase string)
  "(string-downcase string)

   Function convert a string passed as argument to lower case."
  (typecheck "string-downcase" string "string")
  (string.lower))

;; -----------------------------------------------------------------------------
(define (string-upcase string)
  "(string-downcase string)

   Function convert a string passed as argument to upper case."
  (typecheck "string-downcase" string "string")
  (string.upper))

;; -----------------------------------------------------------------------------
(define (dynamic-wind before thunk after)
  "(dynamic-wind before thunk after)

   Accepts 3 procedures/lambdas and executes before, then thunk, and
   always after even if an error occurs in thunk."
  (before)
  (let ((result (try (thunk)
                     (catch (e)
                            (after)
                            (error e)))))
    (after)
    result))

;; -----------------------------------------------------------------------------
(define (with-exception-handler handler thunk)
  "(with-exception-handler handler thunk)

   Procedure call and return value of thunk function, if exception happen
   it call handler procedure."
  (try (thunk)
       (catch (e)
              (handler e))))

;; -----------------------------------------------------------------------------
;; macro definition taken from R7RS spec
;; -----------------------------------------------------------------------------
(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values (lambda () expr)
         (lambda args #f))))
    ((define-values (var) expr)
     (define var expr))
    ((define-values (var0 var1 ... warn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
           (define warn
             (let ((v (cadr var0)))
               (set! var0 (car var0))
               v))))
    ((define-values (var0 var1 ... . warn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
       (define warn
         (let ((v (cdr var0)))
           (set! var0 (car var0))
           v))))
    ((define-values var expr)
     (define var
       (call-with-values (lambda () expr)
         list))))
  "(define-values (a b ...) expr)

   Evaluates expression expr and if it evaluates to result of values
   then it will define each value as a variable like with define.")

;; -----------------------------------------------------------------------------
(define-macro (include . files)
  "(include file ...)

   Load at least one file content and insert them into one,
   body expression."
  (if (null? files)
      (throw (new Error "include: at least one file path required"))
      (let ((result (vector)) (env (interaction-environment)))
        (if (eq? self global)
            (let* ((fs (require "fs"))
                   (readFile (lambda (file)
                               (new Promise (lambda (resolve reject)
                                              (fs.readFile file
                                                           (lambda (err data)
                                                             (if (null? err)
                                                                 (resolve (--> data
                                                                               (toString)))
                                                                 (reject err)))))))))
              (for-each (lambda (file)
                          (let* ((expr (lips.parse (readFile file) env)))
                            (set! result (--> result (concat expr)))))
                        files))
            (for-each (lambda (file)
                        (let* ((text (--> (fetch file) (text)))
                               (expr (lips.parse text env)))
                          (set! result (--> result (concat expr)))))
                      files))
        (if (> result.length 0)
            `(begin
              ,@(vector->list result))))))

;; -----------------------------------------------------------------------------
;; create scope for JavaScript value for macro
;; -----------------------------------------------------------------------------
(define-syntax syntax-error
  (syntax-rules ()
    ((_ "step" arg ...)
     (join " " (vector->list  (vector (repr arg) ...))))
    ((_ message arg ...)
     (error (format "~a ~a" message (_ "step" arg ...))))))

;; -----------------------------------------------------------------------------
;; based on https://srfi.schemers.org/srfi-0/srfi-0.html
;; -----------------------------------------------------------------------------
(define-syntax cond-expand
  (syntax-rules (and or not else r7rs srfi-0 srfi-2 srfi-4 srfi-6 srfi-10
                     srfi-22 srfi-23 srfi-28 srfi-46 srfi-69 srfi-98 srfi-111
                     srfi-139 srfi-147 srfi-156 srfi-176 srfi-193 srfi-195 srfi-210
                     srfi-236 lips r7rs complex full-unicode ieee-float ratios
                     exact-complex full-numeric-tower)
    ((cond-expand) (syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand (r7rs body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-0  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-2  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-4  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-6  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-10  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-22  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-23  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-28  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-46  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-69  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-98  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-111  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-139  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-147  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-156  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-176  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-193  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-195  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-210  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-236  body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (lips body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (complex body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (full-unicode body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (ieee-float body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (ratios body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (exact-complex body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (full-numeric-tower body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
     (cond-expand more-clauses ...)))
  "(cond-expand (cond body ...)

   Conditionally execute code based on a features by Scheme implementation.")

;; -----------------------------------------------------------------------------
(define (features)
  "(features)

   Function returns implemented features as a list."
  '(r7rs srfi-0 srfi-2 srfi-4 srfi-6 srfi-10 srfi-22 srfi-23 srfi-28 srfi-46 srfi-69
         srfi-98 srfi-111 srfi-139 srfi-147 srfi-156 srfi-176 srfi-193 srfi-195
         srfi-210 srfi-236 lips complex full-unicode ieee-float ratios exact-complex
         full-numeric-tower))

;; -----------------------------------------------------------------------------
;; the numerals can be generated using scripts/numerals.scm to get latest version
;; of the file use `make zero`
;; -----------------------------------------------------------------------------
(define *zero-number-chars* #(48 1632 1776 1984 2406 2534 2662 2790 2918 3046 3174 3302
                              3430 3558 3664 3792 3872 4160 4240 6112 6160 6470 6608 6784
                              6800 6992 7088 7232 7248 42528 43216 43264 43472 43504 43600
                              44016 65296 66720 68912 69734 69872 69942 70096 70384 70736
                              70864 71248 71360 71472 71904 72016 72784 73040 73120 92768
                              93008 120782 120792 120802 120812 120822 123200 123632 125264
                              130032))

;; -----------------------------------------------------------------------------
;; the Unicode folding case mapping generated scripts/fold.scm to get latest version
;; of the file use `make fold`
;; -----------------------------------------------------------------------------
(define *fold-case-mapping* (JSON.parse "{
    \"65\": 97, \"66\": 98, \"67\": 99, \"68\": 100,
    \"69\": 101, \"70\": 102, \"71\": 103, \"72\": 104,
    \"73\": 105, \"74\": 106, \"75\": 107, \"76\": 108,
    \"77\": 109, \"78\": 110, \"79\": 111, \"80\": 112,
    \"81\": 113, \"82\": 114, \"83\": 115, \"84\": 116,
    \"85\": 117, \"86\": 118, \"87\": 119, \"88\": 120,
    \"89\": 121, \"90\": 122, \"181\": 956, \"192\": 224,
    \"193\": 225, \"194\": 226, \"195\": 227, \"196\": 228,
    \"197\": 229, \"198\": 230, \"199\": 231, \"200\": 232,
    \"201\": 233, \"202\": 234, \"203\": 235, \"204\": 236,
    \"205\": 237, \"206\": 238, \"207\": 239, \"208\": 240,
    \"209\": 241, \"210\": 242, \"211\": 243, \"212\": 244,
    \"213\": 245, \"214\": 246, \"216\": 248, \"217\": 249,
    \"218\": 250, \"219\": 251, \"220\": 252, \"221\": 253,
    \"222\": 254, \"256\": 257, \"258\": 259, \"260\": 261,
    \"262\": 263, \"264\": 265, \"266\": 267, \"268\": 269,
    \"270\": 271, \"272\": 273, \"274\": 275, \"276\": 277,
    \"278\": 279, \"280\": 281, \"282\": 283, \"284\": 285,
    \"286\": 287, \"288\": 289, \"290\": 291, \"292\": 293,
    \"294\": 295, \"296\": 297, \"298\": 299, \"300\": 301,
    \"302\": 303, \"306\": 307, \"308\": 309, \"310\": 311,
    \"313\": 314, \"315\": 316, \"317\": 318, \"319\": 320,
    \"321\": 322, \"323\": 324, \"325\": 326, \"327\": 328,
    \"330\": 331, \"332\": 333, \"334\": 335, \"336\": 337,
    \"338\": 339, \"340\": 341, \"342\": 343, \"344\": 345,
    \"346\": 347, \"348\": 349, \"350\": 351, \"352\": 353,
    \"354\": 355, \"356\": 357, \"358\": 359, \"360\": 361,
    \"362\": 363, \"364\": 365, \"366\": 367, \"368\": 369,
    \"370\": 371, \"372\": 373, \"374\": 375, \"376\": 255,
    \"377\": 378, \"379\": 380, \"381\": 382, \"115\": 383,
    \"385\": 595, \"386\": 387, \"388\": 389, \"390\": 596,
    \"391\": 392, \"393\": 598, \"394\": 599, \"395\": 396,
    \"398\": 477, \"399\": 601, \"400\": 603, \"401\": 402,
    \"403\": 608, \"404\": 611, \"406\": 617, \"407\": 616,
    \"408\": 409, \"412\": 623, \"413\": 626, \"415\": 629,
    \"416\": 417, \"418\": 419, \"420\": 421, \"422\": 640,
    \"423\": 424, \"425\": 643, \"428\": 429, \"430\": 648,
    \"431\": 432, \"433\": 650, \"434\": 651, \"435\": 436,
    \"437\": 438, \"439\": 658, \"440\": 441, \"444\": 445,
    \"452\": 454, \"454\": 453, \"455\": 457, \"457\": 456,
    \"458\": 460, \"460\": 459, \"461\": 462, \"463\": 464,
    \"465\": 466, \"467\": 468, \"469\": 470, \"471\": 472,
    \"473\": 474, \"475\": 476, \"478\": 479, \"480\": 481,
    \"482\": 483, \"484\": 485, \"486\": 487, \"488\": 489,
    \"490\": 491, \"492\": 493, \"494\": 495, \"497\": 499,
    \"499\": 498, \"500\": 501, \"502\": 405, \"503\": 447,
    \"504\": 505, \"506\": 507, \"508\": 509, \"510\": 511,
    \"512\": 513, \"514\": 515, \"516\": 517, \"518\": 519,
    \"520\": 521, \"522\": 523, \"524\": 525, \"526\": 527,
    \"528\": 529, \"530\": 531, \"532\": 533, \"534\": 535,
    \"536\": 537, \"538\": 539, \"540\": 541, \"542\": 543,
    \"544\": 414, \"546\": 547, \"548\": 549, \"550\": 551,
    \"552\": 553, \"554\": 555, \"556\": 557, \"558\": 559,
    \"560\": 561, \"562\": 563, \"570\": 11365, \"571\": 572,
    \"573\": 410, \"574\": 11366, \"577\": 578, \"579\": 384,
    \"580\": 649, \"581\": 652, \"582\": 583, \"584\": 585,
    \"587\": 586, \"588\": 589, \"590\": 591, \"837\": 953,
    \"880\": 881, \"882\": 883, \"886\": 887, \"895\": 1011,
    \"902\": 940, \"904\": 941, \"905\": 942, \"906\": 943,
    \"908\": 972, \"910\": 973, \"911\": 974, \"913\": 945,
    \"914\": 946, \"915\": 947, \"916\": 948, \"917\": 949,
    \"918\": 950, \"919\": 951, \"920\": 952, \"921\": 953,
    \"922\": 954, \"923\": 955, \"924\": 956, \"925\": 957,
    \"926\": 958, \"927\": 959, \"928\": 960, \"929\": 961,
    \"931\": 963, \"932\": 964, \"933\": 965, \"934\": 966,
    \"935\": 967, \"936\": 968, \"937\": 969, \"938\": 970,
    \"939\": 971, \"963\": 962, \"975\": 983, \"976\": 946,
    \"977\": 952, \"981\": 966, \"982\": 960, \"984\": 985,
    \"986\": 987, \"988\": 989, \"990\": 991, \"992\": 993,
    \"994\": 995, \"996\": 997, \"998\": 999, \"1000\": 1001,
    \"1002\": 1003, \"1004\": 1005, \"1006\": 1007, \"1008\": 954,
    \"1009\": 961, \"1012\": 952, \"1013\": 949, \"1015\": 1016,
    \"1017\": 1010, \"1018\": 1019, \"1021\": 891, \"1022\": 892,
    \"1023\": 893, \"1024\": 1104, \"1025\": 1105, \"1026\": 1106,
    \"1027\": 1107, \"1028\": 1108, \"1029\": 1109, \"1030\": 1110,
    \"1031\": 1111, \"1032\": 1112, \"1033\": 1113, \"1034\": 1114,
    \"1035\": 1115, \"1036\": 1116, \"1037\": 1117, \"1038\": 1118,
    \"1039\": 1119, \"1040\": 1072, \"1041\": 1073, \"1042\": 1074,
    \"1043\": 1075, \"1044\": 1076, \"1045\": 1077, \"1046\": 1078,
    \"1047\": 1079, \"1048\": 1080, \"1049\": 1081, \"1050\": 1082,
    \"1051\": 1083, \"1052\": 1084, \"1053\": 1085, \"1054\": 1086,
    \"1055\": 1087, \"1056\": 1088, \"1057\": 1089, \"1058\": 1090,
    \"1059\": 1091, \"1060\": 1092, \"1061\": 1093, \"1062\": 1094,
    \"1063\": 1095, \"1064\": 1096, \"1065\": 1097, \"1066\": 1098,
    \"1067\": 1099, \"1068\": 1100, \"1069\": 1101, \"1070\": 1102,
    \"1071\": 1103, \"1120\": 1121, \"1122\": 1123, \"1124\": 1125,
    \"1126\": 1127, \"1128\": 1129, \"1130\": 1131, \"1132\": 1133,
    \"1134\": 1135, \"1136\": 1137, \"1138\": 1139, \"1140\": 1141,
    \"1142\": 1143, \"1144\": 1145, \"1146\": 1147, \"1148\": 1149,
    \"1150\": 1151, \"1152\": 1153, \"1162\": 1163, \"1164\": 1165,
    \"1166\": 1167, \"1168\": 1169, \"1170\": 1171, \"1172\": 1173,
    \"1174\": 1175, \"1176\": 1177, \"1178\": 1179, \"1180\": 1181,
    \"1182\": 1183, \"1184\": 1185, \"1186\": 1187, \"1188\": 1189,
    \"1190\": 1191, \"1192\": 1193, \"1194\": 1195, \"1196\": 1197,
    \"1198\": 1199, \"1200\": 1201, \"1202\": 1203, \"1204\": 1205,
    \"1206\": 1207, \"1208\": 1209, \"1210\": 1211, \"1212\": 1213,
    \"1214\": 1215, \"1216\": 1231, \"1217\": 1218, \"1219\": 1220,
    \"1221\": 1222, \"1223\": 1224, \"1225\": 1226, \"1227\": 1228,
    \"1229\": 1230, \"1232\": 1233, \"1234\": 1235, \"1236\": 1237,
    \"1238\": 1239, \"1240\": 1241, \"1242\": 1243, \"1244\": 1245,
    \"1246\": 1247, \"1248\": 1249, \"1250\": 1251, \"1252\": 1253,
    \"1254\": 1255, \"1256\": 1257, \"1258\": 1259, \"1260\": 1261,
    \"1262\": 1263, \"1264\": 1265, \"1266\": 1267, \"1268\": 1269,
    \"1270\": 1271, \"1272\": 1273, \"1274\": 1275, \"1276\": 1277,
    \"1278\": 1279, \"1280\": 1281, \"1282\": 1283, \"1284\": 1285,
    \"1286\": 1287, \"1288\": 1289, \"1290\": 1291, \"1292\": 1293,
    \"1294\": 1295, \"1296\": 1297, \"1298\": 1299, \"1300\": 1301,
    \"1302\": 1303, \"1304\": 1305, \"1306\": 1307, \"1308\": 1309,
    \"1310\": 1311, \"1312\": 1313, \"1314\": 1315, \"1316\": 1317,
    \"1318\": 1319, \"1320\": 1321, \"1322\": 1323, \"1324\": 1325,
    \"1326\": 1327, \"1329\": 1377, \"1330\": 1378, \"1331\": 1379,
    \"1332\": 1380, \"1333\": 1381, \"1334\": 1382, \"1335\": 1383,
    \"1336\": 1384, \"1337\": 1385, \"1338\": 1386, \"1339\": 1387,
    \"1340\": 1388, \"1341\": 1389, \"1342\": 1390, \"1343\": 1391,
    \"1344\": 1392, \"1345\": 1393, \"1346\": 1394, \"1347\": 1395,
    \"1348\": 1396, \"1349\": 1397, \"1350\": 1398, \"1351\": 1399,
    \"1352\": 1400, \"1353\": 1401, \"1354\": 1402, \"1355\": 1403,
    \"1356\": 1404, \"1357\": 1405, \"1358\": 1406, \"1359\": 1407,
    \"1360\": 1408, \"1361\": 1409, \"1362\": 1410, \"1363\": 1411,
    \"1364\": 1412, \"1365\": 1413, \"1366\": 1414, \"4256\": 11520,
    \"4257\": 11521, \"4258\": 11522, \"4259\": 11523, \"4260\": 11524,
    \"4261\": 11525, \"4262\": 11526, \"4263\": 11527, \"4264\": 11528,
    \"4265\": 11529, \"4266\": 11530, \"4267\": 11531, \"4268\": 11532,
    \"4269\": 11533, \"4270\": 11534, \"4271\": 11535, \"4272\": 11536,
    \"4273\": 11537, \"4274\": 11538, \"4275\": 11539, \"4276\": 11540,
    \"4277\": 11541, \"4278\": 11542, \"4279\": 11543, \"4280\": 11544,
    \"4281\": 11545, \"4282\": 11546, \"4283\": 11547, \"4284\": 11548,
    \"4285\": 11549, \"4286\": 11550, \"4287\": 11551, \"4288\": 11552,
    \"4289\": 11553, \"4290\": 11554, \"4291\": 11555, \"4292\": 11556,
    \"4293\": 11557, \"4295\": 11559, \"4301\": 11565, \"5104\": 5112,
    \"5105\": 5113, \"5106\": 5114, \"5107\": 5115, \"5108\": 5116,
    \"5109\": 5117, \"1074\": 7296, \"1076\": 7297, \"1086\": 7298,
    \"1089\": 7299, \"1090\": 7300, \"1090\": 7301, \"1098\": 7302,
    \"1123\": 7303, \"42571\": 7304, \"7312\": 4304, \"7313\": 4305,
    \"7314\": 4306, \"7315\": 4307, \"7316\": 4308, \"7317\": 4309,
    \"7318\": 4310, \"7319\": 4311, \"7320\": 4312, \"7321\": 4313,
    \"7322\": 4314, \"7323\": 4315, \"7324\": 4316, \"7325\": 4317,
    \"7326\": 4318, \"7327\": 4319, \"7328\": 4320, \"7329\": 4321,
    \"7330\": 4322, \"7331\": 4323, \"7332\": 4324, \"7333\": 4325,
    \"7334\": 4326, \"7335\": 4327, \"7336\": 4328, \"7337\": 4329,
    \"7338\": 4330, \"7339\": 4331, \"7340\": 4332, \"7341\": 4333,
    \"7342\": 4334, \"7343\": 4335, \"7344\": 4336, \"7345\": 4337,
    \"7346\": 4338, \"7347\": 4339, \"7348\": 4340, \"7349\": 4341,
    \"7350\": 4342, \"7351\": 4343, \"7352\": 4344, \"7353\": 4345,
    \"7354\": 4346, \"7357\": 4349, \"7358\": 4350, \"7359\": 4351,
    \"7680\": 7681, \"7682\": 7683, \"7684\": 7685, \"7686\": 7687,
    \"7688\": 7689, \"7690\": 7691, \"7692\": 7693, \"7694\": 7695,
    \"7696\": 7697, \"7698\": 7699, \"7700\": 7701, \"7702\": 7703,
    \"7704\": 7705, \"7706\": 7707, \"7708\": 7709, \"7710\": 7711,
    \"7712\": 7713, \"7714\": 7715, \"7716\": 7717, \"7718\": 7719,
    \"7720\": 7721, \"7722\": 7723, \"7724\": 7725, \"7726\": 7727,
    \"7728\": 7729, \"7730\": 7731, \"7732\": 7733, \"7734\": 7735,
    \"7736\": 7737, \"7738\": 7739, \"7740\": 7741, \"7742\": 7743,
    \"7744\": 7745, \"7746\": 7747, \"7748\": 7749, \"7750\": 7751,
    \"7752\": 7753, \"7754\": 7755, \"7756\": 7757, \"7758\": 7759,
    \"7760\": 7761, \"7762\": 7763, \"7764\": 7765, \"7766\": 7767,
    \"7768\": 7769, \"7770\": 7771, \"7772\": 7773, \"7774\": 7775,
    \"7776\": 7777, \"7778\": 7779, \"7780\": 7781, \"7782\": 7783,
    \"7784\": 7785, \"7786\": 7787, \"7788\": 7789, \"7790\": 7791,
    \"7792\": 7793, \"7794\": 7795, \"7796\": 7797, \"7798\": 7799,
    \"7800\": 7801, \"7802\": 7803, \"7804\": 7805, \"7806\": 7807,
    \"7808\": 7809, \"7810\": 7811, \"7812\": 7813, \"7814\": 7815,
    \"7816\": 7817, \"7818\": 7819, \"7820\": 7821, \"7822\": 7823,
    \"7824\": 7825, \"7826\": 7827, \"7828\": 7829, \"7777\": 7835,
    \"7840\": 7841, \"7842\": 7843, \"7844\": 7845, \"7846\": 7847,
    \"7848\": 7849, \"7850\": 7851, \"7852\": 7853, \"7854\": 7855,
    \"7856\": 7857, \"7858\": 7859, \"7860\": 7861, \"7862\": 7863,
    \"7864\": 7865, \"7866\": 7867, \"7868\": 7869, \"7870\": 7871,
    \"7872\": 7873, \"7874\": 7875, \"7876\": 7877, \"7878\": 7879,
    \"7880\": 7881, \"7882\": 7883, \"7884\": 7885, \"7886\": 7887,
    \"7888\": 7889, \"7890\": 7891, \"7892\": 7893, \"7894\": 7895,
    \"7896\": 7897, \"7898\": 7899, \"7900\": 7901, \"7902\": 7903,
    \"7904\": 7905, \"7906\": 7907, \"7908\": 7909, \"7910\": 7911,
    \"7912\": 7913, \"7914\": 7915, \"7916\": 7917, \"7918\": 7919,
    \"7920\": 7921, \"7922\": 7923, \"7924\": 7925, \"7926\": 7927,
    \"7928\": 7929, \"7930\": 7931, \"7932\": 7933, \"7934\": 7935,
    \"7944\": 7936, \"7945\": 7937, \"7946\": 7938, \"7947\": 7939,
    \"7948\": 7940, \"7949\": 7941, \"7950\": 7942, \"7951\": 7943,
    \"7960\": 7952, \"7961\": 7953, \"7962\": 7954, \"7963\": 7955,
    \"7964\": 7956, \"7965\": 7957, \"7976\": 7968, \"7977\": 7969,
    \"7978\": 7970, \"7979\": 7971, \"7980\": 7972, \"7981\": 7973,
    \"7982\": 7974, \"7983\": 7975, \"7992\": 7984, \"7993\": 7985,
    \"7994\": 7986, \"7995\": 7987, \"7996\": 7988, \"7997\": 7989,
    \"7998\": 7990, \"7999\": 7991, \"8008\": 8000, \"8009\": 8001,
    \"8010\": 8002, \"8011\": 8003, \"8012\": 8004, \"8013\": 8005,
    \"8025\": 8017, \"8027\": 8019, \"8029\": 8021, \"8031\": 8023,
    \"8040\": 8032, \"8041\": 8033, \"8042\": 8034, \"8043\": 8035,
    \"8044\": 8036, \"8045\": 8037, \"8046\": 8038, \"8047\": 8039,
    \"8120\": 8112, \"8121\": 8113, \"8122\": 8048, \"8123\": 8049,
    \"8126\": 953, \"8136\": 8050, \"8137\": 8051, \"8138\": 8052,
    \"8139\": 8053, \"8152\": 8144, \"8153\": 8145, \"8154\": 8054,
    \"8155\": 8055, \"8168\": 8160, \"8169\": 8161, \"8170\": 8058,
    \"8171\": 8059, \"8172\": 8165, \"8184\": 8056, \"8185\": 8057,
    \"8186\": 8060, \"8187\": 8061, \"8486\": 969, \"8490\": 107,
    \"8491\": 229, \"8498\": 8526, \"8544\": 8560, \"8545\": 8561,
    \"8546\": 8562, \"8547\": 8563, \"8548\": 8564, \"8549\": 8565,
    \"8550\": 8566, \"8551\": 8567, \"8552\": 8568, \"8553\": 8569,
    \"8554\": 8570, \"8555\": 8571, \"8556\": 8572, \"8557\": 8573,
    \"8558\": 8574, \"8559\": 8575, \"8579\": 8580, \"9398\": 9424,
    \"9399\": 9425, \"9400\": 9426, \"9401\": 9427, \"9402\": 9428,
    \"9403\": 9429, \"9404\": 9430, \"9405\": 9431, \"9406\": 9432,
    \"9407\": 9433, \"9408\": 9434, \"9409\": 9435, \"9410\": 9436,
    \"9411\": 9437, \"9412\": 9438, \"9413\": 9439, \"9414\": 9440,
    \"9415\": 9441, \"9416\": 9442, \"9417\": 9443, \"9418\": 9444,
    \"9419\": 9445, \"9420\": 9446, \"9421\": 9447, \"9422\": 9448,
    \"9423\": 9449, \"11264\": 11312, \"11265\": 11313, \"11266\": 11314,
    \"11267\": 11315, \"11268\": 11316, \"11269\": 11317, \"11270\": 11318,
    \"11271\": 11319, \"11272\": 11320, \"11273\": 11321, \"11274\": 11322,
    \"11275\": 11323, \"11276\": 11324, \"11277\": 11325, \"11278\": 11326,
    \"11279\": 11327, \"11280\": 11328, \"11281\": 11329, \"11282\": 11330,
    \"11283\": 11331, \"11284\": 11332, \"11285\": 11333, \"11286\": 11334,
    \"11287\": 11335, \"11288\": 11336, \"11289\": 11337, \"11290\": 11338,
    \"11291\": 11339, \"11292\": 11340, \"11293\": 11341, \"11294\": 11342,
    \"11295\": 11343, \"11296\": 11344, \"11297\": 11345, \"11298\": 11346,
    \"11299\": 11347, \"11348\": 11300, \"11349\": 11301, \"11302\": 11350,
    \"11351\": 11303, \"11304\": 11352, \"11305\": 11353, \"11306\": 11354,
    \"11307\": 11355, \"11308\": 11356, \"11309\": 11357, \"11310\": 11358,
    \"11311\": 11359, \"11360\": 11361, \"11362\": 619, \"11363\": 7549,
    \"11364\": 637, \"11367\": 11368, \"11369\": 11370, \"11371\": 11372,
    \"11373\": 593, \"11374\": 625, \"11375\": 592, \"11376\": 594,
    \"11378\": 11379, \"11381\": 11382, \"11390\": 575, \"11391\": 576,
    \"11392\": 11393, \"11394\": 11395, \"11396\": 11397, \"11398\": 11399,
    \"11400\": 11401, \"11402\": 11403, \"11404\": 11405, \"11406\": 11407,
    \"11408\": 11409, \"11410\": 11411, \"11412\": 11413, \"11414\": 11415,
    \"11416\": 11417, \"11418\": 11419, \"11420\": 11421, \"11422\": 11423,
    \"11424\": 11425, \"11426\": 11427, \"11428\": 11429, \"11430\": 11431,
    \"11432\": 11433, \"11434\": 11435, \"11436\": 11437, \"11438\": 11439,
    \"11440\": 11441, \"11442\": 11443, \"11444\": 11445, \"11446\": 11447,
    \"11448\": 11449, \"11450\": 11451, \"11452\": 11453, \"11454\": 11455,
    \"11456\": 11457, \"11458\": 11459, \"11460\": 11461, \"11462\": 11463,
    \"11464\": 11465, \"11466\": 11467, \"11468\": 11469, \"11470\": 11471,
    \"11472\": 11473, \"11474\": 11475, \"11476\": 11477, \"11478\": 11479,
    \"11480\": 11481, \"11482\": 11483, \"11484\": 11485, \"11486\": 11487,
    \"11488\": 11489, \"11490\": 11491, \"11499\": 11500, \"11501\": 11502,
    \"11506\": 11507, \"42560\": 42561, \"42562\": 42563, \"42564\": 42565,
    \"42566\": 42567, \"42568\": 42569, \"42570\": 42571, \"42572\": 42573,
    \"42574\": 42575, \"42576\": 42577, \"42578\": 42579, \"42580\": 42581,
    \"42582\": 42583, \"42584\": 42585, \"42586\": 42587, \"42588\": 42589,
    \"42590\": 42591, \"42592\": 42593, \"42594\": 42595, \"42596\": 42597,
    \"42598\": 42599, \"42600\": 42601, \"42602\": 42603, \"42604\": 42605,
    \"42624\": 42625, \"42626\": 42627, \"42628\": 42629, \"42630\": 42631,
    \"42632\": 42633, \"42634\": 42635, \"42636\": 42637, \"42638\": 42639,
    \"42640\": 42641, \"42642\": 42643, \"42644\": 42645, \"42646\": 42647,
    \"42648\": 42649, \"42650\": 42651, \"42786\": 42787, \"42788\": 42789,
    \"42790\": 42791, \"42792\": 42793, \"42794\": 42795, \"42796\": 42797,
    \"42798\": 42799, \"42802\": 42803, \"42804\": 42805, \"42806\": 42807,
    \"42808\": 42809, \"42810\": 42811, \"42812\": 42813, \"42814\": 42815,
    \"42816\": 42817, \"42818\": 42819, \"42820\": 42821, \"42822\": 42823,
    \"42824\": 42825, \"42826\": 42827, \"42828\": 42829, \"42830\": 42831,
    \"42832\": 42833, \"42834\": 42835, \"42836\": 42837, \"42838\": 42839,
    \"42840\": 42841, \"42842\": 42843, \"42844\": 42845, \"42846\": 42847,
    \"42848\": 42849, \"42850\": 42851, \"42852\": 42853, \"42854\": 42855,
    \"42856\": 42857, \"42858\": 42859, \"42860\": 42861, \"42862\": 42863,
    \"42873\": 42874, \"42875\": 42876, \"42877\": 7545, \"42878\": 42879,
    \"42880\": 42881, \"42882\": 42883, \"42884\": 42885, \"42886\": 42887,
    \"42891\": 42892, \"42893\": 613, \"42896\": 42897, \"42898\": 42899,
    \"42902\": 42903, \"42904\": 42905, \"42906\": 42907, \"42908\": 42909,
    \"42910\": 42911, \"42912\": 42913, \"42914\": 42915, \"42916\": 42917,
    \"42918\": 42919, \"42920\": 42921, \"42922\": 614, \"42923\": 604,
    \"42924\": 609, \"42925\": 620, \"618\": 42926, \"42928\": 670,
    \"42929\": 647, \"42930\": 669, \"42931\": 43859, \"42932\": 42933,
    \"42934\": 42935, \"42936\": 42937, \"42938\": 42939, \"42940\": 42941,
    \"42942\": 42943, \"42944\": 42945, \"42946\": 42947, \"42948\": 42900,
    \"42949\": 642, \"42950\": 7566, \"42951\": 42952, \"42953\": 42954,
    \"42960\": 42961, \"42966\": 42967, \"42968\": 42969, \"42997\": 42998,
    \"5024\": 43888, \"5025\": 43889, \"5026\": 43890, \"5027\": 43891,
    \"5028\": 43892, \"5029\": 43893, \"5030\": 43894, \"5031\": 43895,
    \"5032\": 43896, \"5033\": 43897, \"5034\": 43898, \"5035\": 43899,
    \"5036\": 43900, \"5037\": 43901, \"5038\": 43902, \"5039\": 43903,
    \"5040\": 43904, \"5041\": 43905, \"5042\": 43906, \"5043\": 43907,
    \"5044\": 43908, \"5045\": 43909, \"5046\": 43910, \"5047\": 43911,
    \"5048\": 43912, \"5049\": 43913, \"5050\": 43914, \"5051\": 43915,
    \"5052\": 43916, \"5053\": 43917, \"5054\": 43918, \"5055\": 43919,
    \"5056\": 43920, \"5057\": 43921, \"5058\": 43922, \"5059\": 43923,
    \"5060\": 43924, \"5061\": 43925, \"5062\": 43926, \"5063\": 43927,
    \"5064\": 43928, \"5065\": 43929, \"5066\": 43930, \"5067\": 43931,
    \"5068\": 43932, \"5069\": 43933, \"5070\": 43934, \"5071\": 43935,
    \"5072\": 43936, \"5073\": 43937, \"5074\": 43938, \"5075\": 43939,
    \"5076\": 43940, \"5077\": 43941, \"5078\": 43942, \"5079\": 43943,
    \"5080\": 43944, \"5081\": 43945, \"5082\": 43946, \"5083\": 43947,
    \"5084\": 43948, \"5085\": 43949, \"5086\": 43950, \"5087\": 43951,
    \"5088\": 43952, \"5089\": 43953, \"5090\": 43954, \"5091\": 43955,
    \"5092\": 43956, \"5093\": 43957, \"5094\": 43958, \"5095\": 43959,
    \"5096\": 43960, \"5097\": 43961, \"5098\": 43962, \"5099\": 43963,
    \"5100\": 43964, \"5101\": 43965, \"5102\": 43966, \"5103\": 43967,
    \"65313\": 65345, \"65314\": 65346, \"65315\": 65347, \"65316\": 65348,
    \"65317\": 65349, \"65318\": 65350, \"65319\": 65351, \"65320\": 65352,
    \"65321\": 65353, \"65322\": 65354, \"65323\": 65355, \"65324\": 65356,
    \"65325\": 65357, \"65326\": 65358, \"65327\": 65359, \"65328\": 65360,
    \"65329\": 65361, \"65330\": 65362, \"65331\": 65363, \"65332\": 65364,
    \"65333\": 65365, \"65334\": 65366, \"65335\": 65367, \"65336\": 65368,
    \"65337\": 65369, \"65338\": 65370, \"66560\": 66600, \"66561\": 66601,
    \"66562\": 66602, \"66563\": 66603, \"66564\": 66604, \"66565\": 66605,
    \"66566\": 66606, \"66567\": 66607, \"66568\": 66608, \"66569\": 66609,
    \"66570\": 66610, \"66571\": 66611, \"66572\": 66612, \"66573\": 66613,
    \"66574\": 66614, \"66575\": 66615, \"66576\": 66616, \"66577\": 66617,
    \"66578\": 66618, \"66579\": 66619, \"66580\": 66620, \"66581\": 66621,
    \"66582\": 66622, \"66583\": 66623, \"66584\": 66624, \"66585\": 66625,
    \"66586\": 66626, \"66587\": 66627, \"66588\": 66628, \"66589\": 66629,
    \"66590\": 66630, \"66591\": 66631, \"66592\": 66632, \"66593\": 66633,
    \"66594\": 66634, \"66595\": 66635, \"66596\": 66636, \"66597\": 66637,
    \"66598\": 66638, \"66599\": 66639, \"66736\": 66776, \"66737\": 66777,
    \"66738\": 66778, \"66739\": 66779, \"66740\": 66780, \"66741\": 66781,
    \"66742\": 66782, \"66743\": 66783, \"66744\": 66784, \"66745\": 66785,
    \"66746\": 66786, \"66747\": 66787, \"66748\": 66788, \"66749\": 66789,
    \"66750\": 66790, \"66751\": 66791, \"66752\": 66792, \"66753\": 66793,
    \"66754\": 66794, \"66755\": 66795, \"66756\": 66796, \"66757\": 66797,
    \"66758\": 66798, \"66759\": 66799, \"66760\": 66800, \"66761\": 66801,
    \"66762\": 66802, \"66763\": 66803, \"66764\": 66804, \"66765\": 66805,
    \"66766\": 66806, \"66767\": 66807, \"66768\": 66808, \"66769\": 66809,
    \"66770\": 66810, \"66771\": 66811, \"66928\": 66967, \"66929\": 66968,
    \"66930\": 66969, \"66931\": 66970, \"66932\": 66971, \"66933\": 66972,
    \"66934\": 66973, \"66935\": 66974, \"66936\": 66975, \"66937\": 66976,
    \"66938\": 66977, \"66940\": 66979, \"66941\": 66980, \"66942\": 66981,
    \"66943\": 66982, \"66944\": 66983, \"66945\": 66984, \"66946\": 66985,
    \"66947\": 66986, \"66948\": 66987, \"66949\": 66988, \"66950\": 66989,
    \"66951\": 66990, \"66952\": 66991, \"66953\": 66992, \"66954\": 66993,
    \"66956\": 66995, \"66957\": 66996, \"66958\": 66997, \"66959\": 66998,
    \"66960\": 66999, \"66961\": 67000, \"66962\": 67001, \"66964\": 67003,
    \"66965\": 67004, \"68736\": 68800, \"68737\": 68801, \"68738\": 68802,
    \"68739\": 68803, \"68740\": 68804, \"68741\": 68805, \"68742\": 68806,
    \"68743\": 68807, \"68744\": 68808, \"68745\": 68809, \"68746\": 68810,
    \"68747\": 68811, \"68748\": 68812, \"68749\": 68813, \"68750\": 68814,
    \"68751\": 68815, \"68752\": 68816, \"68753\": 68817, \"68754\": 68818,
    \"68755\": 68819, \"68756\": 68820, \"68757\": 68821, \"68758\": 68822,
    \"68759\": 68823, \"68760\": 68824, \"68761\": 68825, \"68762\": 68826,
    \"68763\": 68827, \"68764\": 68828, \"68765\": 68829, \"68766\": 68830,
    \"68767\": 68831, \"68768\": 68832, \"68769\": 68833, \"68770\": 68834,
    \"68771\": 68835, \"68772\": 68836, \"68773\": 68837, \"68774\": 68838,
    \"68775\": 68839, \"68776\": 68840, \"68777\": 68841, \"68778\": 68842,
    \"68779\": 68843, \"68780\": 68844, \"68781\": 68845, \"68782\": 68846,
    \"68783\": 68847, \"68784\": 68848, \"68785\": 68849, \"68786\": 68850,
    \"71840\": 71872, \"71841\": 71873, \"71842\": 71874, \"71843\": 71875,
    \"71844\": 71876, \"71845\": 71877, \"71846\": 71878, \"71847\": 71879,
    \"71848\": 71880, \"71849\": 71881, \"71850\": 71882, \"71851\": 71883,
    \"71852\": 71884, \"71853\": 71885, \"71854\": 71886, \"71855\": 71887,
    \"71856\": 71888, \"71857\": 71889, \"71858\": 71890, \"71859\": 71891,
    \"71860\": 71892, \"71861\": 71893, \"71862\": 71894, \"71863\": 71895,
    \"71864\": 71896, \"71865\": 71897, \"71866\": 71898, \"71867\": 71899,
    \"71868\": 71900, \"71869\": 71901, \"71870\": 71902, \"71871\": 71903,
    \"93760\": 93792, \"93761\": 93793, \"93762\": 93794, \"93763\": 93795,
    \"93764\": 93796, \"93765\": 93797, \"93766\": 93798, \"93767\": 93799,
    \"93768\": 93800, \"93769\": 93801, \"93770\": 93802, \"93771\": 93803,
    \"93772\": 93804, \"93773\": 93805, \"93774\": 93806, \"93775\": 93807,
    \"93776\": 93808, \"93777\": 93809, \"93778\": 93810, \"93779\": 93811,
    \"93780\": 93812, \"93781\": 93813, \"93782\": 93814, \"93783\": 93815,
    \"93784\": 93816, \"93785\": 93817, \"93786\": 93818, \"93787\": 93819,
    \"93788\": 93820, \"93789\": 93821, \"93790\": 93822, \"93791\": 93823,
    \"125184\": 125218, \"125185\": 125219, \"125186\": 125220, \"125187\": 125221,
    \"125188\": 125222, \"125189\": 125223, \"125190\": 125224, \"125191\": 125225,
    \"125192\": 125226, \"125193\": 125227, \"125194\": 125228, \"125195\": 125229,
    \"125196\": 125230, \"125197\": 125231, \"125198\": 125232, \"125199\": 125233,
    \"125200\": 125234, \"125201\": 125235, \"125202\": 125236, \"125203\": 125237,
    \"125204\": 125238, \"125205\": 125239, \"125206\": 125240, \"125207\": 125241,
    \"125208\": 125242, \"125209\": 125243, \"125210\": 125244, \"125211\": 125245,
    \"125212\": 125246, \"125213\": 125247, \"125214\": 125248, \"125215\": 125249,
    \"125216\": 125250, \"125217\": 125251
}"))

;; -----------------------------------------------------------------------------
(define (char-foldcase char)
  "(char-foldcase char)

   Returns lowercase character using the Unicode simple case-folding algorithm."
  (typecheck "char-foldcase" char "character")
  (let ((output (. *fold-case-mapping* (char->integer char))))
    (if (number? output)
        (integer->char output)
        char)))

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
(define make-bytevector make-u8vector)
(define bytevector u8vector)
(define bytevector? u8vector?)
(define bytevector-length u8vector-length)
(define bytevector-u8-ref u8vector-ref)
(define bytevector-u8-set! u8vector-set!)

;; -----------------------------------------------------------------------------
(define (bytevector-append v1 . rest)
  "(bytevector-append v1 ...)

   Create new bytevector u8vector that is created from joining each argument."
  (typecheck "bytevector-append" v1 "uint8array" 1)
  (map (lambda (arg)
         (typecheck "bytevector-append" arg "uint8array"))
       rest)
  (if (null? rest)
      v1
     (new Uint8Array (apply vector-append (Array.from v1) (map Array.from rest)))))

;; -----------------------------------------------------------------------------
(define (bytevector-copy v . rest)
  "(bytevector-copy v)
   (bytevector-copy v start)
   (bytevector-copy v start end)

   Returns a new vector from start to end. If no start and end is provided
   whole vector is copied and returned."
  (if (null? rest)
      (new Uint8Array v)
      (let ((start (car rest)))
        (if (null? (cdr rest))
            (v.slice start)
            (v.slice start (cadr rest))))))

;; -----------------------------------------------------------------------------
(define (bytevector-copy! to at from . rest)
  "(bytevector-copy! to at from)
   (bytevector-copy! to at from start)
   (bytevector-copy! to at from start end)

   Copies the bytes of bytevector from between start and end to bytevector to,
   starting at at."
  (typecheck "bytevector-copy!" to "uint8array")
  (typecheck "bytevector-copy!" from "uint8array")
  (cond ((< at 0)
         (throw (new Error "bytevector-copy! `at` need to be positive")))
        ((> at (bytevector-length to))
         (throw (new Error
                     "bytevector-copy! `at` need to be less then byte vector length"))))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (- (bytevector-length from) start)
                  (cadr rest))))
    (let ((i at) (j start))
      (while (and (< i (bytevector-length to))
                  (< i (bytevector-length from))
                  (< j (+ start end)))
        (bytevector-u8-set! to i (bytevector-u8-ref from j))
        (set! i (+ i 1))
        (set! j (+ j 1))))))

;; -----------------------------------------------------------------------------
(define string->utf8
  (let ((encoder (new TextEncoder "utf-8")))
    (lambda (string . rest)
      "(string->utf8 string)
       (string->utf8 string start)
       (string->utf8 string start end)

      Converts string into u8 bytevector using utf8 encoding.
      The start and end is the range of the input string for the conversion."
      (typecheck "string->utf8" string "string")
      (if (null? rest)
          (encoder.encode string)
          (let* ((start (car rest))
                 (len (--> (Array.from string) 'length))
                 (end (if (null? (cdr rest)) len (cadr rest))))
            (encoder.encode (substring string start end)))))))

;; -----------------------------------------------------------------------------
(define utf8->string
  (let ((decoder (new TextDecoder "utf-8")))
    (lambda (v . rest)
      "(utf8->string u8vector)
       (utf8->string u8vector start)
       (utf8->string u8vector start end)

      Converts u8 bytevector into string using utf8 encoding.
      The start and end is the range of the input byte vector for the conversion."
      (typecheck "utf8->string" v "uint8array")
      (if (null? rest)
          (decoder.decode v)
          (let* ((start (car rest))
                 (len (--> (Array.from string) 'length))
                 (end (if (null? (cdr rest)) len (cadr rest))))
            (decoder.decode (v.slice start end)))))))

;; -----------------------------------------------------------------------------
(define (open-input-string string)
  "(open-input-string string)

   Creates new string port as input that can be used to
   read S-exressions from this port using `read` function."
  (typecheck "open-input-string" string "string")
  (new lips.InputStringPort string (interaction-environment)))

;; -----------------------------------------------------------------------------
(define (open-output-string)
  "(open-output-string)

   Creates new output port that can used to write string into
   and after finish get the whole string using `get-output-string`."
  (new lips.OutputStringPort repr))

;; -----------------------------------------------------------------------------
(define (open-output-bytevector)
  "(open-output-bytevector)

   Create new output port that can be used to write binary data.
   After done with the data the output buffer can be obtained by calling
   `get-output-bytevector` function."
  (new lips.OutputByteVectorPort))

;; -----------------------------------------------------------------------------
(define (get-output-bytevector port)
  "(get-output-string port)

   Gets full string from string port. If nothing was wrote
   to given port it will return empty string."
  (if (not (instanceof lips.OutputByteVectorPort port))
      (throw (new Error (string-append
                         "get-output-bytevector: expecting output-bytevector-port get "
                         (type port))))
      (port.valueOf)))

;; -----------------------------------------------------------------------------
(define (get-output-string port)
  "(get-output-string port)

   Gets full string from string port. If nothing was wrote
   to given port it will return empty string."
  (if (not (instanceof lips.OutputStringPort port))
      (throw (new Error (string-append "get-output-string: expecting output-string-port get "
                                       (type port))))
      (port.valueOf)))

;; -----------------------------------------------------------------------------
(define (open-input-bytevector bytevector)
  "(open-input-bytevector bytevector)

   Create new input binary port with given bytevector"
  (typecheck "open-input-bytevector" bytevector "uint8array")
  (new lips.InputByteVectorPort bytevector))

;; -----------------------------------------------------------------------------
(define (open-binary-input-file filename)
  "(open-binary-input-file filename)

  Returns new Input Binary Port with given filename. In Browser
  user need to provide global fs variable that is instance of FS interface."
  (let ((u8vector (buffer->u8vector (%read-binary-file filename))))
    (new lips.InputBinaryFilePort u8vector filename)))

;; -----------------------------------------------------------------------------
(define (binary-port? port)
  "(binary-port? port)

   Function that tests if argument is binary port."
  (and (port? port) (eq? port.__type__ (Symbol.for "binary"))))

;; -----------------------------------------------------------------------------
(define (textual-port? port)
  "(textual-port? port)

   Function that tests if argument is string port."
  (and (port? port) (eq? port.__type__ (Symbol.for "text"))))

;; -----------------------------------------------------------------------------
(define-macro (%define-binary-input-lambda name docstring fn)
  (let ((port (gensym))
        (name-str (symbol->string name)))
    `(define (,name . rest)
       ,docstring
       (let ((,port (if (null? rest)
                        (current-input-port)
                        (car rest))))
         (typecheck ,name-str ,port "input-port")
         (if (not (binary-port? ,port))
             (throw (new Error (string-append ,name-str
                                              " invalid port. Binary port required.")))
             (,fn ,port))))))

;; -----------------------------------------------------------------------------
(%define-binary-input-lambda
 peek-u8
 "(peek-u8)
  (peek-u8 port)

  Return next byte from input-binary port. If there are no more bytes
  it return eof object."
 (lambda (port)
   (port.peek_u8)))

;; -----------------------------------------------------------------------------
(%define-binary-input-lambda
 read-u8
 "(read-u8)
  (read-u8 port)

  Read next byte from input-binary port. If there are no more bytes
  it return eof object."
 (lambda (port)
   (port.read_u8)))

;; -----------------------------------------------------------------------------
(%define-binary-input-lambda
 u8-ready?
 "(u8-ready?)
  (u8-ready? port)

  Returns #t if a byte is ready on the binary input port and returns #f otherwise.
  If u8-ready? returns #t then the next read-u8 operation on the given port is
  guaranteed not to hang. If the port is at end of file then u8-ready? returns #t."
 (lambda (port)
   (port.u8_ready)))

;; -----------------------------------------------------------------------------
(define (read-bytevector k . rest)
  "(read-bytevector k)
   (read-bytevector k port)

   Read next n bytes from input-binary port. If there are no more bytes
   it returns eof object. If there are less then n bytes in port it
   return the only bytes that are available"
  (let ((port (if (null? rest)
                  (current-input-port)
                  (car rest))))
    (typecheck "read-bytevector" port "input-port")
    (if (not (binary-port? port))
        (throw (new Error "read-bytevector: invalid port"))
        (port.read_u8_vector k))))

;; -----------------------------------------------------------------------------
(define-macro (%define-binary-output-lambda name type docstring fn)
  (let ((port (gensym 'port))
        (data (gensym 'data))
        (name-str (symbol->string name)))
    `(define (,name ,data . rest)
       ,docstring
       (let ((,port (if (null? rest)
                        (current-output-port)
                        (car rest))))
         (typecheck ,name-str ,port "output-port")
         (typecheck ,name-str ,data ,type)
         (if (not (binary-port? ,port))
             (throw (new Error (string-append ,name-str
                                              " invalid port. Binary port required.")))
             (,fn ,data ,port))))))

;; -----------------------------------------------------------------------------
(%define-binary-output-lambda
 write-u8
 "number"
 "(write-u8 byte)
  (write-u8 byte port)

  Write byte into binary output port."
 (lambda (data port)
   (port.write_u8 data)))

;; -----------------------------------------------------------------------------
(%define-binary-output-lambda
 write-bytevector
 "uint8array"
 "(write-bytevector bytevector)
  (write-bytevector bytevector port)

  Write byte vector into binary output port."
 (lambda (data port)
   (port.write_u8_vector data)))

;; -----------------------------------------------------------------------------
(define open-binary-output-file
  (let ((open))
    (lambda (filename)
      "(open-binary-output-file filename)

       Opens file and return port that can be used for writing. If file
       exists it will throw an Error."
      (typecheck "open-output-file" filename "string")
      (if (not (procedure? open))
          (set! open (%fs-promisify-proc 'open "open-binary-output-file")))
      (if (file-exists? filename)
          (throw (new Error "open-binary-output-file: file exists"))
          (lips.OutputBinaryFilePort filename (open filename "w"))))))

;; -----------------------------------------------------------------------------
(define (read-bytevector! vector . rest)
  "(read-bytevector! bytevector)
   (read-bytevector! bytevector port)
   (read-bytevector! bytevector port start)
   (read-bytevector! bytevector port start end)

   Reads next bytes from binary input port and write them into byte vector.
   if not start is specified it start to write into 0 position of the vector until
   the end or end the vector if no end is specified."
  (typecheck "read-bytevector!" vector "uint8array")
  (let ((port (if (null? rest) (current-input-port) (car rest)))
        (start (if (or (null? rest) (null? (cdr rest))) 0 (cadr rest)))
        (end (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
                 (bytevector-length vector)
                 (caddr rest))))
    (typecheck "read-bytevector!" port "input-port")
    (if (not (binary-port? port))
        (throw (new Error "read-bytevector!: invalid port. Binary port required."))
        (begin
          (typecheck "read-bytevector!" start "number")
          (typecheck "read-bytevector!" end "number")
          (let ((out (read-bytevector (- end start) port)))
            (vector.set out start end))))))

;; -----------------------------------------------------------------------------
(define delete-file
  (let ((unlink #f))
    (lambda (filename)
      "(delete-file filename)

       Deletes the file of given name."
      (typecheck "delete-file" filename "string")
      (if (not (procedure? unlink))
          (set! unlink (%fs-promisify-proc 'unlink "delete-file")))
      (unlink filename))))

;; -----------------------------------------------------------------------------
(define (call-with-port port proc)
  "(call-with-port port proc)

   Proc is executed with given port and after it returns, the port is closed."
  (try
   (proc port)
   (finally
    (if (procedure? port.close)
        (port.close)))))

;; -----------------------------------------------------------------------------
(define (close-port port)
  "(close-port port)

   Close input or output port."
  (typecheck "close-port" port #("input-port" "output-port"))
  (port.close))

;; -----------------------------------------------------------------------------
(define (eof-object)
  "(eof-object)

   Procedure returns eof object that indicate end of the port"
  lips.eof)

;; -----------------------------------------------------------------------------
(define (output-port-open? port)
  "(output-port-open? port)

   Checks if argument is output-port and if you can write to it."
  (and (output-port? port) (port.is_open)))

;; -----------------------------------------------------------------------------
(define (input-port-open? port)
  "(input-port-open? port)

   Checks if argument is input-port and if you can read from it."
  (and (input-port? port) (port.is_open)))

;; -----------------------------------------------------------------------------
(define (flush-output-port port)
  "(flush-output-port port)

   Function do nothing, flush is not needed in LIPS in both NodeJS and Browser.
   The function is added, so it don't throw exception when using R7RS code."
  (if #f #f))

;; -----------------------------------------------------------------------------
(define (write-string string . rest)
  "(write-string string)
   (write-string string port)
   (write-string string port start)
   (write-string string port start end)

   Writes the characters of string from start to end in left-toright order
   to the textual output port."
  (typecheck "write-string" string "string")
  (let ((port (if (null? rest) (current-output-port) (car rest)))
        (start (if (or (null? rest) (null? (cdr rest))) 0 (cadr rest)))
        (end (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
                 (string-length string)
                 (caddr rest))))
    (typecheck "write-string" port "output-port")
    (typecheck "write-string" start "number")
    (typecheck "write-string" end "number")
    (display (substring string start end) port)))

;; -----------------------------------------------------------------------------
(define (write-char char . rest)
  "(write-char string)
   (write-char string port)

   Writes the character char (not an external representation of the character)
   to the given textual output port and returns an unspecified value."
  (typecheck "write-char" char "character")
  (let ((port (if (null? rest) (current-output-port) (car rest))))
    (typecheck "write-char" port "output-port")
    (display (string char) port)))

;; -----------------------------------------------------------------------------
(define (read-string k . rest)
  "(read-string k)
   (read-string k port)

   Reads the next k characters, or as many as are available
   before the end of file, from the textual input port into a
   newly allocated string in left-to-right order and returns the
   string. If no characters are available before the end of file,
   an end-of-file object is returned."
  (typecheck "read-string" k "number")
  (let ((port (if (null? rest) (current-input-port) (car rest))))
    (typecheck "read-string" port "input-port")
    (port.read_string k)))

;; -----------------------------------------------------------------------------
(define (list-copy obj)
  "(list-copy obj)

   Copy the object passed as argument but only if it's list. The car elements
   of the list are not copied, they are passed as is."
  (typecheck "list-copy" obj #("pair" "nil"))
  (if (null? obj)
      obj
      (obj.clone false)))

;; -----------------------------------------------------------------------------
(define (list-set! l k obj)
  "(list-set! list n)

   Returns n-th element of a list."
  (set-car! (%nth-pair "list-set!" l k) obj))

;; -----------------------------------------------------------------------------
(define-macro (define-record-type name constructor pred . fields)
  "(define-record-type name constructor pred . fields)

   Macro for defining records. Example of usage:

   (define-record-type <pare>
     (kons x y)
     pare?
     (x kar set-kar!)
     (y kdr set-kdr!))

   (define p (kons 1 2))
   (print (kar p))
   ;; ==> 1
   (set-kdr! p 3)
   (print (kdr p))
   ;; ==> 3"
  (let ((obj-name (gensym 'obj-name))
        (value-name (gensym 'value-name)))
    `(begin
       (define ,name (class Object
                            (constructor (lambda (self ,@(cdr constructor))
                                           ,@(map (lambda (field)
                                                    (let* ((name (symbol->string field))
                                                           (prop (string-append "self."
                                                                                name)))
                                                      `(set! ,(string->symbol prop) ,field)))
                                                  (cdr constructor))))
                            (equal (lambda (self other)
                                     (if (instanceof ,name other)
                                         (and ,@(map (lambda (field)
                                                       (let* ((name (symbol->string field))
                                                              (self-prop (string-append "self."
                                                                                        name))
                                                              (other-prop (string-append "other."
                                                                                         name)))
                                                         `(equal? ,(string->symbol self-prop)
                                                                  ,(string->symbol other-prop))))))
                                         #f)))
                            (typeOf (lambda (self)
                                      "record"))
                            (toString (lambda (self)
                                        (string-append "#<" ,(symbol->string name) ">")))))
       (define ,constructor
         (new ,name ,@(cdr constructor)))
       (define (,pred obj)
         (instanceof ,name obj))
       ,@(map (lambda (field)
                (let ((prop-name (car field))
                      (get (cadr field))
                      (set (if (null? (cddr field))
                               nil
                               (caddr field))))
                  `(begin
                     (define (,get ,obj-name)
                       (typecheck ,(symbol->string get) ,obj-name "record")
                       (if (not (,pred ,obj-name))
                           (throw (new Error ,(string-append "object is not record of type "
                                                             (symbol->string name))))
                           (. ,obj-name ',prop-name)))
                     ,(if (not (null? set))
                          `(define (,set ,obj-name ,value-name)
                             (typecheck ,(symbol->string get) ,obj-name "record")
                             (if (not (,pred ,obj-name))
                                 (throw (new Error ,(string-append "object is not record of type "
                                                                   (symbol->string name))))
                                 (set-obj! ,obj-name ',prop-name ,value-name)))))))
              fields))))

;; -----------------------------------------------------------------------------
(define (nan? x)
  "(nan? x)

  Checks if argument x is Not a Number (NaN) value."
  (and (number? x)
       (or (x.isNaN)
           (and (%number-type "complex" x)
                (or (nan? (real-part x))
                    (nan? (imag-part x)))))))

;; -----------------------------------------------------------------------------
(define (infinite? x)
  "(infinite? x)

  Checks if value is infinite."
  (or (eq? x Number.NEGATIVE_INFINITY)
      (eq? x Number.POSITIVE_INFINITY)
      (and (number? x)
           (not (eq? x NaN))
           (%number-type "complex" x)
           (or (infinite? (real-part x))
               (infinite? (imag-part x))))))

;; -----------------------------------------------------------------------------
(define (finite? x)
  "(finite? x)

  Checks if value is finite."
  (not (infinite? x)))

;; -----------------------------------------------------------------------------
(define-class %Library Object
  (constructor
    (lambda (self name)
      (set! self.__namespace__ &())
      (set! self.__name__ name)))
  (append
   (lambda (self namespace env)
     (if (environment? (. self.__namespace__ namespace))
         (throw (new Error (string-append "namespace " namespace
                                          " already exists in library "
                                          self.__name__)))
         (set-obj! self.__namespace__ namespace env))))
  (env
   (lambda (self namespace)
     (let ((env (. self.__namespace__ namespace)))
        (if (not (environment? env))
            (throw (new Error (string-append "namespace " namespace " sdon't exists")))
            env))))
  (get
    (lambda (self namespace name)
      (--> (self.env namespace) (get name))))
  (set
    (lambda (self namespace name value)
      (--> (self.env namespace) (set name value))))
  (toString
     (lambda (self)
       (string-append "#<Library(" self.__name__ ")>"))))

;; -----------------------------------------------------------------------------
(define (%import-name library namespace names)
  `(begin
    ,@(map (lambda (name)
             `(define ,name (--> ,library (get ',namespace ',name))))
           names)))

;; -----------------------------------------------------------------------------
(define-macro (import . specs)
  "(import (library namespace))
   (import (only (library namespace) name1 name2))

   Macro for importing names from library."
  (let ((parent (current-environment)))
    `(begin
       ,@(map (lambda (spec)
                (if (not (pair? spec))
                    (throw (new Error "import: invalid syntax"))
                    (cond ((symbol=? (car spec)
                                     'only)
                           (let ((lib (caadr spec))
                                 (namespace (caaddr spec)))
                             (if (pair? (cadr spec))
                                 (%import-name ,lib
                                               ',namespace
                                               ',(caddr spec))
                                 (throw (new Error "import: invalid syntax")))))
                          (else
                           (let* ((lib-name (car spec))
                                  (lib (parent.get lib-name))
                                  (namespace (cadr spec)))
                             (%import-name lib-name
                                           namespace
                                           (env (lib.env namespace))))))))
              specs))))

;; -----------------------------------------------------------------------------
(define (new-library name namespace)
  "(new-library name)

   Create new empty library object with empty namespace."
  (let* ((parent (. (current-environment) '__parent__))
         (lib (let ((lib (--> parent (get name &(:throwError false)))))
                (if (null? lib)
                    (new %Library name)
                    lib)))
         (x (new lips.Environment
                 (string-append "library-"
                                (--> name (toLowerCase))
                                "-"
                                (--> namespace (toLowerCase))))))
    (lib.append namespace x)
    lib))

;; -----------------------------------------------------------------------------
(define (%export module namespace specs)
  `(begin
     ,@(map (lambda (expr)
              (cond ((symbol? expr)
                     `(--> ,module (set ',namespace
                                        ',expr
                                         ,expr)))
                    ((and (pair? expr) (symbol=? (car expr)
                                                 'rename))
                     `(--> ,module (set ',namespace
                                        ',(cadr expr)
                                        ,(caddr expr))))))
              specs)))

;; -----------------------------------------------------------------------------
(define-macro (define-library spec . body)
  "(define-library (library (name namespace) . body)

   Macro for defining modules inside you can use define to create functions.
   And use export name to add that name to defined environment."
  (let ((parent (. (current-environment) '__parent__))
        (module-var (gensym))
        (namespace-var (gensym))
        (name (car spec))
        (namespace (cadr spec)))
    `(let ((,module-var (new-library ,(repr name)
                                     ,(repr namespace)))
           (,namespace-var ',namespace))
       (define-macro (export . body)
         (%export ,module-var ,namespace-var body))
       ,@body
       (--> ,parent (set ',name ,module-var)))))

;; -----------------------------------------------------------------------------
(define-syntax guard
  (syntax-rules (catch aux =>)
    ((_ aux)
     '())
    ((_ aux (cond result) rest ...)
     (let ((it cond))
       (if it
           result
           (guard aux rest ...))))
    ((_ aux (cond => fn) rest ...)
     (let ((it cond))
       (if it
           (fn it)
           (guard aux rest ...))))
    ((_ aux (cond) rest ...)
     (let ((it cond))
       (if it
           it
           (guard aux rest ...))))
    ((_ (var cond1 cond2 ...)
        body ...)
     (try
       body ...
       (catch (var)
              (guard aux
                     cond1
                     cond2 ...)))))
  "(guard (variable (cond)
                    (cond => fn)
                    (cond2 result))
          body)

   Macro that executes the body and when there is exception, triggered by
   raise it's saved in variable that can be tested by conditions.")

;; -----------------------------------------------------------------------------
(define-syntax define-library/export
  (syntax-rules (rename :c)
    ((_ :c (rename to from))
     (print (string-append "export "
                           (symbol->string 'from)
                           " ==> "
                           (symbol->string 'to))))
    ((_ :c name)
     (print (string-append "export " (symbol->string 'name))))
    ((_ x ...)
     (begin
       (define-library/export :c x)
       ...))))

;; -----------------------------------------------------------------------------
;; TODO: use browserFS or LightningFS
;; -----------------------------------------------------------------------------
(define-values (current-directory set-current-directory!)
  (if (eq? self window)
      (let ((cwd (string-append location.origin (--> location.pathname
                                                     (replace #/\/[^/]+$/ "/")))))
        (values
         (lambda ()
           "(current-directory)

            Return current working directory, default is the current URL."
           cwd)
         (lambda (value)
           "(set-current-directory! string)

            Changes the current working directory to provided string."
           (typecheck "set-current-directory!" value "string")
           (set! cwd value))))
      (let ((process (require "process")))
        (values
         (lambda ()
           "(current-directory)

            Returns the current working directory, default is the path from where
            the script was executed."
          (string-append (process.cwd) "/"))
         (lambda (value)
           "(set-current-directory! string)

            Changes the current working directory to provided string."
           (typecheck "set-current-directory!" value "string")
           (process.chdir value))))))

;; -----------------------------------------------------------------------------
(define (error message . args)
  "(error message ...)

   Function raises error with given message and arguments,
   which are called invariants."
  (raise (new lips.Error message (args.to_array))))

;; -----------------------------------------------------------------------------
(define (error-object? obj)
  "(error-object? obj)

   Checks if object is of Error object thrown by error function."
  (instanceof lips.Error obj))

;; -----------------------------------------------------------------------------
(define (error-object-message obj)
  "(error-object-message error-object)

   Returns the message encapsulated by error-object."
  (if (error-object? obj)
      obj.message))

;; -----------------------------------------------------------------------------
(define (error-object-irritants obj)
  "(error-object-irritants error-object)

   Returns a list of the irritants encapsulated by error-object."
  (if (error-object? obj)
      obj.args))

;; -----------------------------------------------------------------------------
(define (get-environment-variables)
  "(get-environment-variables)

   Returns all process environment variables as an alist. This function returns
   an empty list when called in the browser."
  (if (eq? self window)
      nil
      (object->alist process.env)))

;; -----------------------------------------------------------------------------
(define (get-environment-variable name)
  "(get-environment-variable name)

   Returns given environment variable. This function returns undefined
   when called in the browser."
  (if (not (eq? self window))
      (. process.env name)))

;; -----------------------------------------------------------------------------
(define (current-second)
  "(current-second)

   Functionn return exact integer of the seconds since January 1, 1970"
  (inexact->exact (truncate (/ (+ %%start-jiffy (current-jiffy)) (jiffies-per-second)))))

;; -----------------------------------------------------------------------------
(define %%start-jiffy
  (truncate (* 1000 (if (eq? self window)
                        performance.timing.navigationStart
                        performance.timeOrigin)))
  "Constant value that indicates start jiffy of the scheme process.")

;; -----------------------------------------------------------------------------
(define (current-jiffy)
  "(current-jiffy)

   Return current jiffy. In LIPS is jiffy since start of the process.
   You can divide this value by (jiffies-per-second) to get seconds since
   start of the process. And you can add %%start-jiffy to get jiffy since
   January 1, 1970."
  (inexact->exact (truncate (* (performance.now) 1000))))

;; -----------------------------------------------------------------------------
(define (jiffies-per-second)
  1000000)
