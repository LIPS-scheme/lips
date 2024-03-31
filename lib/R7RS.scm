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
  (syntax-rules (multi single)
    ((_ ()) '())
    ((_ () body ...) (begin body ...))
    ((_ ((bind obj) rest ...) . body)
     (apply (lambda bind
              (let*-values (rest ...) . body))
            (if (instanceof lips.Values obj)
                (vector->list (obj.valueOf))
                (list obj)))))
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

(define (log z . rest)
  "(log z)
   (log z1 z2)

   Function that calculates natural logarithm (base e) of z. Where the argument
   can be any number (including complex negative and rational). If the value is 0
   it returns NaN. It two arguments are provided it will calculate logarithm
   of z1 with given base z2."
  (if (not (null? rest))
      (let ((base (car rest)))
        (/ (log z) (log base)))
      (cond ((real? z)
             (cond ((zero? z) NaN)
                   ((> z 0) (Math.log z))
                   (else
                    (+ (Math.log (abs z))
                       (* Math.PI +i)))))
            ((complex? z)
             (let ((arg (Math.atan2 (imag-part z)
                                    (real-part z))))
               (+ (Math.log (z.modulus))
                  (* +i arg))))
            ((rational? z)
             (log (exact->inexact z))))))

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
            ((= i len) #void)
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
   But the return value is #void."
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
(define (char-foldcase char)
  "(char-foldcase char)

   Returns lowercase character using the Unicode simple case-folding algorithm."
  (typecheck "char-foldcase" char "character")
  (new LCharacter (%foldcase-string char.__char__)))

;; -----------------------------------------------------------------------------
(define (string-foldcase string)
  "(string-foldcase string)

   Returns lowercase string using the Unicode simple case-folding algorithm."
  (typecheck "string-foldcase" string "string")
  (%foldcase-string string))

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
(define (%char-vector-cmp name vals)
  "(%char-vector-cmp name vector)

   Function iterate over a vector and compares each pair of two characters
   and return 0 if they are equal, -1 second is smaller and 1 if is larger.
   The function compare the codepoints of the character."
  (let* ((len (vector-length vals))
         (max (- len 1))
         (result (vector))
         (i 0))
    (while (< i max)
      (let* ((chr1 (vector-ref vals i))
             (j (+ i 1))
             (chr2 (vector-ref vals j)))
        (typecheck name chr1 "character" i)
        (typecheck name chr2 "character" j)
        (let ((a (char->integer chr1))
              (b (char->integer chr2)))
          (result.push (cond ((= a b) 0)
                             ((< a b) -1)
                             (else 1)))))
      (set! i (+ i 1)))
    result))

;; -----------------------------------------------------------------------------
(define (char=? . chars)
  "(char=? chr1 chr2 ...)

   Checks if all characters are equal."
  (--> (%char-vector-cmp "char>=?" (list->vector chars)) (every (lambda (a) (= a 0)))))

;; -----------------------------------------------------------------------------
(define (char<? . chars)
  "(char<? chr1 chr2 ...)

   Returns true if characters are monotonically increasing."
  (--> (%char-vector-cmp "char>=?" (list->vector chars)) (every (lambda (a) (= a -1)))))

;; -----------------------------------------------------------------------------
(define (char>? . chars)
  "(char<? chr1 chr2 ...)

   Returns true if characters are monotonically decreasing."
  (--> (%char-vector-cmp "char>=?" (list->vector chars)) (every (lambda (a) (= a 1)))))

;; -----------------------------------------------------------------------------
(define (char<=? . chars)
  "(char<? chr1 chr2 ...)

   Returns true if characters are monotonically non-decreasing."
  (--> (%char-vector-cmp "char>=?" (list->vector chars)) (every (lambda (a) (< a 1)))))

;; -----------------------------------------------------------------------------
(define (char>=? . chars)
  "(char<? chr1 chr2 ...)

   Returns true if characters are monotonically non-increasing."
  (--> (%char-vector-cmp "char>=?" (list->vector chars)) (every (lambda (a) (> a -1)))))

;; -----------------------------------------------------------------------------
(define (%char-ci-vector-cmp name chars)
  "(%char-cmp name chars)

   Function that compares each pair of a vector of characters and return a vector
   of numbers, where 0 if they are equal, -1 second is smaller and 1 if is larger.
   The function compare the codepoints of the character."
  (%char-vector-cmp name (--> chars (map char-downcase))))

;; -----------------------------------------------------------------------------
(define (char-ci=? . chars)
  "(char-ci=? chr1 chr2 ...)

   Checks if all characters are equal, case insensitive."
  (--> (%char-ci-vector-cmp "char-ci=?" (list->vector chars)) (every (lambda (a)
                                                                       (= a 0)))))

;; -----------------------------------------------------------------------------
(define (char-ci<? . chars)
  "(char-ci<? chr1 chr2)

   Returns true if characters are monotonically increasing case insensitive."
  (--> (%char-ci-vector-cmp "char-ci<?" (list->vector chars)) (every (lambda (a)
                                                                       (= a -1)))))

;; -----------------------------------------------------------------------------
(define (char-ci>? . chars)
  "(char-ci<? chr1 chr2 ...)

   Returns true if characters are monotonically decreasing case insensitive."
  (--> (%char-ci-vector-cmp "char-ci<?" (list->vector chars)) (every (lambda (a)
                                                                       (= a 1)))))

;; -----------------------------------------------------------------------------
(define (char-ci<=? . chars)
  "(char-ci<? chr1 chr2 ...)

   Returns true if characters are monotonically non-decreasing, case insensitive."
  (--> (%char-ci-vector-cmp "char-ci<?" (list->vector chars)) (every (lambda (a)
                                                                       (< a 1)))))

;; -----------------------------------------------------------------------------
(define (char-ci>=? . chars)
  "(char-ci<? chr1 chr2 ...)

   Returns true if characters are monotonically non-increasing, case insensitive."
  (--> (%char-ci-vector-cmp "char-ci<?" (list->vector chars)) (every (lambda (a)
                                                                       (> a -1)))))

;; -----------------------------------------------------------------------------
(define (%string-vector-cmp name strings)
  "(%string-cmp name chars)

   Function that compares each pair of strings from vector chars and a vector
   of numbers. 0 if they are equal, -1 if it is smaller and 1 if is larger.
   The function compares the codepoints of the character."
  (let* ((len (vector-length strings))
         (max (- len 1))
         (result (vector))
         (i 0))
    (while (< i max)
      (let* ((str1 (vector-ref strings i))
             (j (+ i 1))
             (str2 (vector-ref strings j)))
        (typecheck name str1 "string" i)
        (typecheck name str2 "string" j)
        (result.push (--> str1 (cmp str2))))
      (set! i (+ i 1)))
    result))

;; -----------------------------------------------------------------------------
(define (string=? . strings)
  "(string=? string1 string2 ...)

   Checks if all strings are equal."
  (--> (%string-vector-cmp "string=?" (list->vector strings))
       (every (lambda (a)
                (= a 0)))))

;; -----------------------------------------------------------------------------
(define (string<? . strings)
  "(string<? string1 string2 ...)

   Returns true if strings are monotonically increasing."
  (--> (%string-vector-cmp "string<?" (list->vector strings))
       (every (lambda (a)
                (= a -1)))))

;; -----------------------------------------------------------------------------
(define (string>? . strings)
  "(string<? string1 string2 ...)

   Returns true if strings are monotonically decreasing."
  (--> (%string-vector-cmp "string>?" (list->vector strings))
       (every (lambda (a)
                (= a 1)))))

;; -----------------------------------------------------------------------------
(define (string<=? . strings)
  "(string<? string1 string2 ...)

   Returns true if strings are monotonically non-decreasing."
  (--> (%string-vector-cmp "string<=?" (list->vector strings))
       (every (lambda (a)
                (< a 1)))))

;; -----------------------------------------------------------------------------
(define (string>=? . strings)
  "(string<? string1 string2 ...)

   Returns true if strings are monotonically non-increasing."
  (--> (%string-vector-cmp "string>=?" (list->vector strings))
       (every (lambda (a)
                (> a -1)))))

;; -----------------------------------------------------------------------------
(define (%string-ci-vector-cmp name strings)
  "(%string-ci-cmp name strings)

   Function that compares each pair from vector of strings ignoring case and
   returns array of numbers 0 if they are equal, -1 if it is smaller and 1 if is larger.
   The function compares the codepoints of the character."
  (%string-vector-cmp name (--> strings (map string-downcase))))

;; -----------------------------------------------------------------------------
(define (string-ci=? . strings)
  "(string-ci=? string1 string2 ...)

   Checks if all strings are equal, ignoring the case."
  (--> (%string-ci-vector-cmp "string-ci=?" (list->vector strings))
       (every (lambda (a)
                (= a 0)))))

;; -----------------------------------------------------------------------------
(define (string-ci<? . strings)
  "(string-ci<? string1 string2 ...)

   Returns true if strings are monotonically increasing, ignoring the case."
  (--> (%string-ci-vector-cmp "string-ci<?" (list->vector strings))
       (every (lambda (a)
                (= a -1)))))

;; -----------------------------------------------------------------------------
(define (string-ci>? . strings)
  "(string-ci>? string1 string2 ...)

   Returns true if strings are monotonically decreasing, ignoring the case"
  (--> (%string-ci-vector-cmp "string-ci>?" (list->vector strings))
       (every (lambda (a)
                (= a 1)))))

;; -----------------------------------------------------------------------------
(define (string-ci<=? . strings)
  "(string-ci<=? string1 string2 ...)

   Returns true if strings are monotonically non-decreasing, ignoring the case."
  (--> (%string-ci-vector-cmp "string-ci<=?" (list->vector strings))
       (every (lambda (a)
                (< a 1)))))

;; -----------------------------------------------------------------------------
(define (string-ci>=? . strings)
  "(string-ci>=? string1 string2 ...)

   Returns true if strings are monotonically non-increasing, ignoring the case."
  (--> (%string-ci-vector-cmp "string-ci<=?" (list->vector strings))
       (every (lambda (a)
                (> a -1)))))

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
       (set-repr! ,name (lambda () (string-append "#<record(" (symbol->string ',name) ")>")))
       ,@(map (lambda (field)
                (let ((prop-name (car field))
                      (get (cadr field))
                      (set (if (null? (cddr field))
                               '()
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
      '()
      (object->alist process.env)))

;; -----------------------------------------------------------------------------
(define (get-environment-variable name)
  "(get-environment-variable name)

   Returns given environment variable. This function returns #void
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
