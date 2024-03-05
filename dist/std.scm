;;   __ __                          __
;;  / / \ \       _    _  ___  ___  \ \
;; | |   \ \     | |  | || . \/ __>  | |
;; | |    > \    | |_ | ||  _/\__ \  | |
;; | |   / ^ \   |___||_||_|  <___/  | |
;;  \_\ /_/ \_\                     /_/
;;
;; <https://lips.js.org>
;;
;; This file contain essential functions and macros for LIPS
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2024 Jakub T. Jankiewicz <https://jcubic.pl/me>
;; Released under MIT license
;; -----------------------------------------------------------------------------
(define true #t)
(define false #f)
(define NaN +nan.0)

;; -----------------------------------------------------------------------------
(define (%doc string fn)
  (typecheck "%doc" fn "function")
  (typecheck "%doc" string "string")
  (set-obj! fn '__doc__ (--> string (replace #/^ +/mg "")))
  fn)

;; -----------------------------------------------------------------------------
(define-macro (let-syntax vars . body)
  "(let-syntax ((name fn) ...) . body)

    Works like a combination of let and define-syntax. It creates
    local macros and evaluates body in context of those macros.
    The macro to letrec-syntax is like letrec is to let."
  `(let ,vars
     ,@(map (lambda (rule)
              `(typecheck "let-syntax" ,(car rule) "syntax"))
            vars)
     ,@body))

;; -----------------------------------------------------------------------------
(define-macro (letrec-syntax vars . body)
  "(letrec-syntax ((name fn) ...) . body)

    Works like a combination of letrec and define-syntax. It creates
    local macros and evaluates the body in context of those macros."
  `(letrec ,vars
     ,@(map (lambda (rule)
              `(typecheck "letrec-syntax" ,(car rule) "syntax"))
            vars)
     ,@body))

;; -----------------------------------------------------------------------------
(define-macro (define-syntax name expr . rest)
  "(define-syntax name expression [__doc__])

   Defines a new hygienic macro using syntax-rules with optional documentation."
  (let ((expr-name (gensym "expr-name")))
    `(define ,name
       (let ((,expr-name ,expr))
         (typecheck "define-syntax" ,expr-name "syntax")
         ,expr-name)
       ,@rest)))

;; -----------------------------------------------------------------------------
(define (quoted-symbol? x)
   "(quoted-symbol? code)

   Helper function that tests if value is a quoted symbol. To be used in macros
   that pass literal code that is transformed by parser.

   usage:

      (define-macro (test x)
         (if (quoted-symbol? x)
             `',(cadr x)))

      (list 'hello (test 'world))"
   (and (pair? x) (eq? (car x) 'quote) (symbol? (cadr x)) (null? (cddr x))))

;; -----------------------------------------------------------------------------
(define-macro (--> expr . body)
  "Helper macro that simplifies calling methods on objects. It works with chaining
   usage: (--> ($ \"body\")
               (css \"color\" \"red\")
               (on \"click\" (lambda () (display \"click\"))))

          (--> document (querySelectorAll \"div\"))

          (--> (fetch \"https://jcubic.pl\")
               (text)
               (match #/<title>([^<]+)<\\/title>/)
               1)

          (--> document
               (querySelectorAll \".cmd-prompt\")
               0
               'innerHTML
               (replace #/<(\"[^\"]+\"|[^>])+>/g \"\"))

          (--> document.body
               (style.setProperty \"--color\" \"red\"))"
  (let ((obj (gensym "obj")))
    `(let* ((,obj ,expr))
       ,@(map (lambda (code)
                (let* ((value (gensym "value"))
                       (name (if (quoted-symbol? code)
                                 (symbol->string (cadr code))
                                 (if (pair? code)
                                     (symbol->string (car code))
                                     code)))
                       (accessor (if (string? name)
                                     `(. ,obj ,@(split "." name))
                                     `(. ,obj ,name)))
                       (call (and (pair? code) (not (quoted-symbol? code)))))
                  `(let ((,value ,accessor))
                     ,(if call
                          `(if (not (function? ,value))
                               (throw (new Error (string-append "--> " ,(repr name)
                                                                " is not a function"
                                                                " in expression "
                                                                ,(repr `(--> ,expr . ,body)))))
                               (set! ,obj (,value ,@(cdr code))))
                          `(set! ,obj ,value)))))
              body)
       ,obj)))


;; -----------------------------------------------------------------------------
(define-macro (define-global first . rest)
  "(define-global var value)
   (define-global (name . args) body)

   Defines functions or variables in the global context, so they can be used
   inside let and get let variables in a closure. Useful for universal macros."
  (if (pair? first)
      (let ((name (car first)))
        `(--> lips.env
              (set ,(symbol->string name) (lambda ,(cdr first) ,@rest))))
      `(--> lips.env (set ,(symbol->string first) ,(car rest)))))

;; -----------------------------------------------------------------------------
(define-macro (globalize expr . rest)
  "(globalize expr)

    Macro will get the value of the expression and add each method as function to global
    scope."
  (let* ((env (current-environment))
         (obj (eval expr env))
         (name (gensym "name"))
         (env-name (gensym "env-name"))
         (make-name (if (pair? rest)
                        (let ((pre (symbol->string (car rest))))
                          (lambda (name) (string->symbol (concat pre name))))
                        string->symbol)))
    `(let ((,name ,expr))
       ,@(filter pair?
                 (map (lambda (key)
                        (if (and (not (match #/^_/ key)) (function? (. obj key)))
                            (let* ((args (gensym "args")))
                              `(define-global (,(make-name key) . ,args)
                                 (apply (. ,name ,key) ,args)))))
                        (array->list (--> Object (keys obj))))))))

;; -----------------------------------------------------------------------------
(define (single list)
  "(single list)

   Checks if argument is list with one element."
  (and (pair? list) (not (cdr list))))

;; -----------------------------------------------------------------------------
(define (iterator? x)
   "(iterator? x)

     Checks if value is JavaScript iterator object."
   (and (object? x) (procedure? (. x Symbol.iterator))))

;; -----------------------------------------------------------------------------
(define-macro (.. expr)
  "(.. foo.bar.baz)

   Gets the value from a nested object where the argument is a period separated symbol."
  (if (not (symbol? expr))
      expr
      (let ((parts (split "." (symbol->string expr))))
        (if (single parts)
            expr
            `(. ,(string->symbol (car parts)) ,@(cdr parts))))))

;; -----------------------------------------------------------------------------
(set-special! "#:" 'gensym-interal)

;; -----------------------------------------------------------------------------
(define (gensym-interal symbol)
  "(gensym-interal symbol)

   Parser extension that creates a new quoted named gensym."
  `(quote ,(gensym symbol)))

;; -----------------------------------------------------------------------------
(define (plain-object? x)
  "(plain-object? x)

   Checks if value is a plain JavaScript object created using the object macro."
  ;; here we don't use string=? or equal? because it may not be defined
  (and (== (--> (type x) (cmp "object")) 0) (eq? (. x 'constructor) Object)))

;; -----------------------------------------------------------------------------
(define typed-array?
  (let ((TypedArray (Object.getPrototypeOf Uint8Array)))
    (lambda (o)
      "(typed-array? o)

      Function that tests if the arguments is a JavaScript typed array (Scheme byte vector)."
      (instanceof TypedArray o))))

;; -----------------------------------------------------------------------------
(define (symbol->string s)
  "(symbol->string symbol)

   Function that converts a LIPS symbol to a string."
  (typecheck "symbol->string" s "symbol")
  (let ((name s.__name__))
    (let ((str (if (string? name)
                   name
                   (name.toString))))
      (str.freeze)
      str)))

;; -----------------------------------------------------------------------------
(define (string->symbol string)
  "(string->symbol string)

   Function that converts a string to a LIPS symbol."
  (typecheck "string->symbol" string "string")
  (let ((symbol (new lips.LSymbol string)))
    (%as.data symbol)))

;; -----------------------------------------------------------------------------
(define (alist->object alist)
  "(alist->object alist)

   Function that converts alist pairs to a JavaScript object."
  (if (pair? alist)
      (alist.to_object)
      (alist->object (new lips.Pair undefined nil))))

;; -----------------------------------------------------------------------------
(define (object->alist object)
  "(object->alist object)

   Function that converts a JavaScript object to Alist"
  (typecheck "object->alist" object "object")
  (vector->list (--> (Object.entries object)
                     (map (lambda (arr)
                            (apply cons (vector->list arr)))))))

;; -----------------------------------------------------------------------------
(define (parent.frames)
  "(parent.frames)

   Returns the list of environments from parent frames (lambda function calls)"
  (let iter ((result '()) (frame (parent.frame 1)))
    (if (eq? frame (interaction-environment))
        (cons frame result)
        (if (null? frame)
            result
            (let ((parent.frame (--> frame (get 'parent.frame (object :throwError false)))))
              (if (function? parent.frame)
                  (iter (cons frame result) (parent.frame 0))
                  result))))))

;; -----------------------------------------------------------------------------
(define (pair-map fn seq-list)
  "(pair-map fn list)

   Function that calls fn argument for pairs in a list and returns a combined list with
   values returned from function fn. It works likes map but take two items from the list each time."
  (let iter ((seq-list seq-list) (result '()))
    (if (null? seq-list)
        result
        (if (and (pair? seq-list) (pair? (cdr seq-list)))
            (let* ((first (car seq-list))
                   (second (cadr seq-list))
                   (value (fn first second)))
              (if (null? value)
                  (iter (cddr seq-list) result)
                  (iter (cddr seq-list) (cons value result))))))))


;; -----------------------------------------------------------------------------
(define (object-expander readonly expr . rest)
  "(object-expander readonly '(:foo (:bar 10) (:baz (1 2 3))))
   (object-expander readonly '(:foo :bar))

   Recursive function helper for defining LIPS code to create objects
   using key like syntax. If no values are used it will create a JavaScript
   shorthand objects where keys are used for keys and the values."
  (let ((name (gensym "name"))
        (r-only (gensym "r-only"))
        (quot (if (null? rest) false (car rest))))
    (if (null? expr)
        `(alist->object ())
        `(let ((,name ,(Object.fromEntries (new Array)))
               (,r-only ,(Object.fromEntries (new Array (new Array "writable" false)))))
           ,@(let loop ((lst expr) (result nil))
               (if (null? lst)
                   (reverse result)
                   (let* ((first (car lst))
                          (no-second (null? (cdr lst)))
                          (second (if no-second nil (cadr lst))))
                     (if (not (key? first))
                         (let ((msg (string-append (type first)
                                                   " "
                                                   (repr first)
                                                   " is not a symbol!")))
                           (throw msg))
                         (let ((prop (key->string first)))
                           (if (or (key? second) no-second)
                               (let ((code `(set-obj! ,name ,prop undefined)))
                                 (loop (cdr lst) (cons code result)))
                               (let ((code (if readonly
                                               (if (and (pair? second) (key? (car second)))
                                                   `(set-obj! ,name
                                                              ,prop
                                                              ,(object-expander readonly second quot)
                                                              ,r-only)
                                                   (if quot
                                                       `(set-obj! ,name ,prop ',second ,r-only)
                                                       `(set-obj! ,name ,prop ,second ,r-only)))
                                               (if (and (pair? second) (key? (car second)))
                                                   `(set-obj! ,name
                                                              ,prop
                                                              ,(object-expander readonly second))
                                                   (if quot
                                                       `(set-obj! ,name ,prop ',second)
                                                       `(set-obj! ,name ,prop ,second))))))
                                 (loop (cddr lst) (cons code result)))))))))
           ,(if readonly
               `(Object.preventExtensions ,name))
           ,name))))

;; -----------------------------------------------------------------------------
(define-macro (object . expr)
  "(object :name value)

   Creates a JavaScript object using key like syntax."
  (try
    (object-expander false expr)
    (catch (e)
      (try
       (error e.message)
       (catch (e)
         (console.error e.message))))))

;; -----------------------------------------------------------------------------
(define-macro (object-literal . expr)
  "(object-literal :name value)

   Creates a JavaScript object using key like syntax. This is similar,
   to object but all values are quoted. This macro is used by the & object literal."
  (try
    (object-expander true expr true)
    (catch (e)
      (try
        (error e.message)
        (catch (e)
          (console.error e.message))))))

;; -----------------------------------------------------------------------------
(define (alist->assign desc . sources)
  "(alist->assign alist . list-of-alists)

   Function that works like Object.assign but for LIPS alists."
  (for-each (lambda (source)
              (for-each (lambda (pair)
                          (let* ((key (car pair))
                                 (value (cdr pair))
                                 (d-pair (assoc key desc)))
                            (if (pair? d-pair)
                                (set-cdr! d-pair value)
                                (append! desc (list pair)))))
                        source))
            sources)
  desc)

;; -----------------------------------------------------------------------------
(define (key? symbol)
  "(key? symbol)

   Checks if symbol is a keyword (has a colon as first character)."
  ;; we can't use string=? because it's in R5RS.scm we use same code that use cmp
  (and (symbol? symbol) (== (--> (substring (symbol->string symbol) 0 1) (cmp ":")) 0)))

;; -----------------------------------------------------------------------------
(define (key->string symbol)
  "(key->string symbol)

   If symbol is a keyword it converts that to string and removes the colon."
  (if (key? symbol)
      (substring (symbol->string symbol) 1)))

;; -----------------------------------------------------------------------------
(define (%as.data obj)
  "(%as.data obj)

   Marks the object as data to stop evaluation."
  (if (object? obj)
      (begin
        (set-obj! obj 'data true)
        obj)))

;; -----------------------------------------------------------------------------
(define (%hidden-props obj)
  "(%hidden-props obj)

   Returns the hidden names of an object, for ES6 class prototypes
   it returns all methods since they are indistinguishable from hidden properties
   created using defineProperty."
  (let* ((descriptors (Object.getOwnPropertyDescriptors obj))
         (names (Object.keys descriptors)))
    (--> names (filter (lambda (name)
                          (let ((descriptor (. descriptors name)))
                            (eq? descriptor.enumerable false)))))))

;; -----------------------------------------------------------------------------
(define (dir obj . rest)
  "(dir obj)

   Returns all props on the object including those in prototype chain."
  (if (or (null? obj) (eq? obj Object.prototype))
      nil
      (let ((proto (if (null? rest) false (car rest)))
            (names (Object.getOwnPropertyNames obj)))
        (if (not proto)
            (let ((hidden (%hidden-props obj)))
              (set! names (--> names
                               (filter (lambda (name)
                                          (not (hidden.includes name))))))))
        (append (array->list (--> names (map (unary string->symbol))))
                (dir (Object.getPrototypeOf obj) true)))))

;; -----------------------------------------------------------------------------
(define (tree-map f tree)
  "(tree-map fn tree)

   Tree version of map. fn is invoked on every leaf."
  (if (pair? tree)
      (cons (tree-map f (car tree)) (tree-map f (cdr tree)))
      (f tree)))

;; -----------------------------------------------------------------------------
(define (native.number x)
  "(native.number obj)

   If argument is a number it will convert it to a native number."
  (if (number? x)
      (value x)
      x))

;; -----------------------------------------------------------------------------
(define (value obj)
  "(value obj)

   Function that unwraps LNumbers and converts nil to undefined."
  (if (eq? obj nil)
      undefined
      (if (number? obj)
          ((. obj "valueOf"))
          obj)))

;; -----------------------------------------------------------------------------
(define-macro (define-formatter-rule . patterns)
  "(rule-pattern pattern)

   Anaphoric macro for defining patterns for the formatter. With Ahead, Pattern and * defined values."
  (let ((rules (gensym "rules")))
    `(let ((,rules lips.Formatter.rules)
           (Ahead (lambda (pattern)
                    (let ((Ahead (.. lips.Formatter.Ahead)))
                      (new Ahead (if (string? pattern) (new RegExp pattern) pattern)))))
           (* (Symbol.for "*"))
           (Pattern (lambda (pattern flag)
                      (new lips.Formatter.Pattern (list->array pattern)
                           (if (null? flag) undefined flag)))))
       ,@(map (lambda (pattern)
                `(--> ,rules (push (tree->array (tree-map native.number ,@pattern)))))
              patterns))))


;; -----------------------------------------------------------------------------
;; macro code taken from https://stackoverflow.com/a/27507779/387194
;; which is based on https://srfi.schemers.org/srfi-61/srfi-61.html
;; but with lowercase tokens
;; NOTE: this code make everything really slow
;;       unit tests run from 1min to 6min.
;; TODO: test this when syntax macros are compiled before evaluation
;; -----------------------------------------------------------------------------
#;(define-syntax cond
  (syntax-rules (=> else)

    ((cond (else else1 else2 ...))
     ;; The (if #t (begin ...)) wrapper ensures that there may be no
     ;; internal definitions in the body of the clause.  R5RS mandates
     ;; this in text (by referring to each subform of the clauses as
     ;; <expression>) but not in its reference implementation of cond,
     ;; which just expands to (begin ...) with no (if #t ...) wrapper.
     (if #t (begin else1 else2 ...)))

    ((cond (test => receiver) more-clause ...)
     (let ((t test))
       (cond/maybe-more t
                        (receiver t)
                        more-clause ...)))

    ((cond (generator guard => receiver) more-clause ...)
     (call-with-values (lambda () generator)
       (lambda t
         (cond/maybe-more (apply guard    t)
                          (apply receiver t)
                          more-clause ...))))

    ((cond (test) more-clause ...)
     (let ((t test))
       (cond/maybe-more t t more-clause ...)))

    ((cond (test body1 body2 ...) more-clause ...)
     (cond/maybe-more test
                      (begin body1 body2 ...)
                      more-clause ...)))
  "(cond (predicate? . body)
         (predicate? . body)
         (else . body))

   Macro for condition checks. For usage instead of nested ifs.")

;; -----------------------------------------------------------------------------
#;(define-syntax cond/maybe-more
  (syntax-rules ()
    ((cond/maybe-more test consequent)
     (if test
         consequent))
    ((cond/maybe-more test consequent clause ...)
     (if test
         consequent
         (cond clause ...))))
  "(cond/maybe-more test consequent ...)

   Helper macro used by cond.")

;; -----------------------------------------------------------------------------
(define-macro (cond . list)
  "(cond (predicate? . body)
         (predicate? . body))

   (cond (predicate? => procedure)
         (predicate? => procedure))

   Macro for condition checks. For usage instead of nested ifs.
   You can use predicate and any number of expressions. Or symbol =>
   Followed by procedure that will be invoked with result
   of the predicate."
  (if (pair? list)
      (let* ((item (car list))
             (value (gensym))
             (first (car item))
             (fn (and (not (null? (cdr item))) (eq? (cadr item) '=>)))
             (expression (if fn
                             (caddr item)
                             (cdr item)))
             (rest (cdr list)))
        `(let ((,value ,first))
           (if ,value
               ,(if fn
                    `(,expression ,value)
                    `(begin
                       ,@expression))
               ,(if (and (pair? rest)
                         (let ((x (caar rest)))
                           (or (eq? x true)
                               (and (symbol? x)
                                    (or (eq? x 'else)
                                        (eq? (--> (new lips.LString (x.literal)) (cmp "else")) 0))))))
                    `(begin
                       ,@(cdar rest))
                    (if (not (null? rest))
                        `(cond ,@rest))))))
      nil))

;; -----------------------------------------------------------------------------
(define (%r re . rest)
  "(%r re)

   Creates a new regular expression from string, to not break Emacs formatting."
   (if (null? rest)
     (new RegExp re)
     (new RegExp re (car rest))))

;; -----------------------------------------------------------------------------
;; replaced by more general formatter in JS, this is left as example of usage
;; -----------------------------------------------------------------------------
#;(define-formatter-rule ((list (list "("
                                    (%r "(?:#:)?cond")
                                    (Pattern (list "(" * ")") "+"))
                               1
                               (Ahead "[^)]"))))

;; -----------------------------------------------------------------------------
(define (interaction-environment)
  "(interaction-environment)

   Returns the interaction environment equal to lips.env. This can be overwritten
   when creating new interpreter with lips.Interpreter."
  **interaction-environment**)

;; -----------------------------------------------------------------------------
(define (current-output-port)
  "(current-output-port)

   Returns the default stdout port."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'stdout))))

;; -----------------------------------------------------------------------------
(define (current-error-port)
  "(current-output-port)

   Returns the default stderr port."
  (let-env (interaction-environment)
     (--> **internal-env** (get 'stderr))))

;; -----------------------------------------------------------------------------
(define (current-input-port)
  "(current-input-port)

   Returns the default stdin port."
  (let-env (interaction-environment)
     (--> **internal-env** (get 'stdin))))

;; -----------------------------------------------------------------------------
(define (command-line)
  "(command-line)

   Returns the command line arguments, or an empty list if not running under Node.js."
  (let ((args (let-env (interaction-environment)
                       (--> **internal-env** (get 'command-line)))))
    (if (or (null? args) (zero? (length args)))
        '("")
        (vector->list args))))

;; -----------------------------------------------------------------------------
(define (flush-output . rest)
  "(flush-output [port])

   If output-port is buffered, this causes the contents of its buffer to be written to
   the output device. Otherwise it has no effect. Returns an unspecified value."
  (let ((port (if (null? rest) (current-output-port) (car rest))))
    (typecheck "flush-output" port "output-port")
    (--> port (flush))))

;; -----------------------------------------------------------------------------
(define (regex? x)
  "(regex? x)

   Returns true if value is a regular expression, or false otherwise."
  (== (--> (type x) (cmp "regex")) 0))

;; -----------------------------------------------------------------------------
(define (set-repr! type fn)
  "(add-repr! type fn)

   Function that adds the string representation to the type, which should be a constructor function.

   Function fn should have args (obj q) and it should return a string. obj is the value that
   need to be converted to a string. If the object is nested and you need to use `repr` recursively,
   it should pass the second parameter q to repr, so string will be quoted when it's true.

   e.g.: (lambda (obj q) (string-append \"<\" (repr obj q) \">\"))"
  (typecheck "add-repr!" type "function")
  (typecheck "add-repr!" fn "function")
  (ignore (--> lips.repr (set type fn))))

;; -----------------------------------------------------------------------------
(define (unset-repr! type)
  "(unset-repr! type)

   Removes the string representation of the type, which should be constructor function,
   added by add-repr! function."
  (typecheck "unset-repr!" type "function")
  (ignore (--> lips.repr (delete type))))

;; -----------------------------------------------------------------------------
;; add syntax &(:foo 10) that evaluates (object :foo 10)
;; -----------------------------------------------------------------------------
(set-special! "&" 'object-literal lips.specials.SPLICE)
;; -----------------------------------------------------------------------------
(set-repr! Object
           (lambda (x q)
             (concat "&("
                     (--> (Object.getOwnPropertyNames x)
                          (map (lambda (key)
                                 (let ((value (repr (. x key) q))
                                       (key (repr (string->symbol key))))
                                   (concat ":" key " " value))))
                          (join " "))
                     ")")))

;; -----------------------------------------------------------------------------
(define (bound? x . rest)
  "(bound? x [env])

   Function that check if the variable is defined in the given environment, or interaction-environment
   if not specified."
  (let ((env (if (null? rest) (interaction-environment) (car rest))))
    (try (begin
           (--> env (get x))
           true)
         (catch (e)
                false))))

;; -----------------------------------------------------------------------------
(define (environment-bound? env x)
  "(environment-bound? env symbol)

   Checks if symbol is a bound variable similar to bound?."
  (typecheck "environment-bound?" env "environment" 1)
  (typecheck "environment-bound?" x "symbol" 2)
  (bound? x env))

;; -----------------------------------------------------------------------------
;; source https://stackoverflow.com/a/4297432/387194
;; -----------------------------------------------------------------------------
(define (qsort e predicate)
  "(qsort list predicate)

   Sorts the list using the quick sort algorithm according to predicate."
  (if (or (null? e) (<= (length e) 1))
      e
      (let loop ((left nil) (right nil)
                 (pivot (car e)) (rest (cdr e)))
        (if (null? rest)
            (append (append (qsort left predicate) (list pivot)) (qsort right predicate))
            (if (predicate (car rest) pivot)
                (loop (append left (list (car rest))) right pivot (cdr rest))
                (loop left (append right (list (car rest))) pivot (cdr rest)))))))

;; -----------------------------------------------------------------------------
(define (sort list . rest)
  "(sort list [predicate])

   Sorts the list using optional predicate function. If no comparison function is given
   it will use <= and sort in increasing order."
  (let ((predicate (if (null? rest) <= (car rest))))
    (typecheck "sort" list "pair")
    (typecheck "sort" predicate "function")
    (qsort list predicate)))

;; -----------------------------------------------------------------------------
(define (every fn list)
  "(every fn list)

   Function that calls fn on each item of the list, if every value returns true
   it will return true otherwise it return false.
   Analogous to Python all(map(fn, list))."
  (if (null? list)
      true
      (and (fn (car list)) (every fn (cdr list)))))

;; -----------------------------------------------------------------------------
(define-macro (promise . body)
  "(promise . body)

   Anaphoric macro that exposes resolve and reject functions from JS promise."
  `(new Promise (lambda (resolve reject)
                  (try (begin ,@body)
                       (catch (e)
                              (error e.message))))))

;; -----------------------------------------------------------------------------
(define-macro (timer time . body)
  "(timer time . body)

   Evaluates body after delay, it returns the timer ID from setTimeout.
   To clear the timer you can use native JS clearTimeout function."
  `(setTimeout (lambda () (try (begin ,@body) (catch (e) (error (.. e.message))))) ,time))

;; -----------------------------------------------------------------------------
(define-macro (wait time . expr)
  "(wait time . expr)

   Returns a promise that will resolve with the expression after delay."
  `(promise (timer ,time (resolve (begin ,@expr)))))

;; -----------------------------------------------------------------------------
(define (await value)
  "(await value)

   Unquotes a quoted promise so it can be automagically evaluated (resolved
   to its value)."
  (if (instanceof lips.QuotedPromise value)
      (value.valueOf)
      value))

;; -----------------------------------------------------------------------------
(define-macro (quote-promise expr)
  "(quote-promise expr) or '>expr

  Macro used to escape automati awaiting of the expression. It will be wrapped
  with a JavaScript class that behaves like Promise but will not be automatically
  resolved by LIPS like normal promises are."
  `(let ((env))
      (set! env (current-environment))
      (env.set (Symbol.for "__promise__") true)
      (let ((env))
        (set! env (current-environment))
        (env.set (Symbol.for "__promise__") false)
        ,expr)))

;; -----------------------------------------------------------------------------
(define (defmacro? obj)
  "(defmacro? expression)

   Checks if object is a macro and it's expandable."
  (and (macro? obj) (. obj 'defmacro)))

;; -----------------------------------------------------------------------------
(define (n-ary n fn)
  "(n-ary n fn)

   Returns a new function that limits the number of arguments to n."
  (lambda args
    (apply fn (take n args))))

;; -----------------------------------------------------------------------------
(define (take n lst)
  "(take n list)

   Returns n first values of the list."
  (let iter ((result '()) (i n) (lst lst))
    (if (or (null? lst) (<= i 0))
        (reverse result)
        (iter (cons (car lst) result) (- i 1) (cdr lst)))))

;; -----------------------------------------------------------------------------
(define (zip . lists)
  "(zip list1 list2 ...)

   Return list where elements are taken from each list.
   e.g.:
   (zip '(1 2 3) '(2 3 4))
   ;; ==> '((1 2) (2 3) (3 4))"
  (if (or (null? lists) (some null? lists))
      '()
      (cons (map car lists) (apply zip (map cdr lists)))))

;; -----------------------------------------------------------------------------
(define unary (%doc "(unary fn)

                     Returns a new function with arguments limited to one."
                    (curry n-ary 1)))

;; -----------------------------------------------------------------------------
(define binary (%doc "(binary fn)

                      Returns a new function with arguments limited to two."
                      (curry n-ary 2)))

;; -----------------------------------------------------------------------------
;; LIPS Object System
;; -----------------------------------------------------------------------------

(define (%class-lambda expr)
  "(class-lambda expr)

   Returns a lambda expression where input expression lambda have `this` as first argument."
  (let ((args (gensym 'args)))
    `(lambda ,args
       (apply ,(cadr expr) this ,args))))

;; -----------------------------------------------------------------------------
(define (%class-method-name expr)
  "(%class-method-name expr)

   Helper function that allows to use [Symbol.asyncIterator] inside method name."
  (if (pair? expr)
      (car expr)
      (list 'quote expr)))

;; -----------------------------------------------------------------------------
(define (constructor)
  "(constructor)

   Function that is present in JavaScript environment. We define it in Scheme
   to fix an issue with define-class. This function throw an error."
  (throw (new Error "Invalid call to constructor function")))

;; -----------------------------------------------------------------------------
(define-macro (define-class name parent . body)
  "(define-class name parent . body)

   Defines a class - JavaScript function constructor with prototype.

   usage:

     (define-class Person Object
         (constructor (lambda (self name)
                        (set-obj! self '_name name)))
         (hi (lambda (self)
               (display (string-append self._name \" says hi\"))
               (newline))))
     (define jack (new Person \"Jack\"))
     (jack.hi) ; prints \"Jack says hi\""
  (let iter ((functions '()) (constructor '()) (lst body))
    (if (null? lst)
        `(begin
           (define ,name ,(if (null? constructor)
                              `(lambda ())
                              ;; we return this to solve issue when constructor
                              ;; return a promise
                              ;; ref: https://stackoverflow.com/a/50885340/387194
                              (append (%class-lambda constructor)
                                      (list 'this))))
           (set-obj! ,name (Symbol.for "__class__") true)
           ,(if (not (null? parent))
                `(begin
                   (set-obj! ,name 'prototype (Object.create (. ,parent 'prototype)))
                   (set-obj! (. ,name 'prototype) 'constructor ,name)))
           (set-obj! ,name '__name__ ',name)
           ,@(map (lambda (fn)
                    `(set-obj! (. ,name 'prototype)
                               ,(%class-method-name (car fn))
                               ,(%class-lambda fn)))
                  functions))
        (let ((item (car lst)))
          (if (eq? (car item) 'constructor)
              (iter functions item (cdr lst))
              (iter (cons item functions) constructor (cdr lst)))))))

;; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    ((_)
     (error "class: parent required"))
    ((_ parent body ...)
     (let ()
       (define-class temp parent body ...)
       temp)))
  "(class <parent> body ...)

   Allows to create anonymous classes. See define-class for details.")

;; -----------------------------------------------------------------------------
(define (make-tags expr)
  "(make-tags expression)

   Returns a list structure of code with better syntax then raw LIPS"
  `(h ,(let ((val (car expr))) (if (key? val) (key->string val) val))
      (alist->object (,'quasiquote ,(pair-map (lambda (car cdr)
                                                `(,(key->string car) . (,'unquote ,cdr)))
                                              (cadr expr))))
      ,(if (not (null? (cddr expr)))
           (if (and (pair? (caddr expr)) (let ((s (caaddr expr)))
                                           (and (symbol? s) (eq? s 'list))))
               `(list->array (list ,@(map make-tags (cdaddr expr))))
               (caddr expr)))))

;; -----------------------------------------------------------------------------
(define (%sxml h expr)
  "(%sxml h expr)

   Helper function that renders the expression using create element function."
  (let* ((have-attrs (and (not (null? (cdr expr)))
                          (pair? (cadr expr))
                          (eq? (caadr expr) '@)))
         (attrs (if have-attrs
                    (cdadr expr)
                    nil))
         (rest (if have-attrs
                   (cddr expr)
                   (cdr expr))))
    `(,h ,(let* ((symbol (car expr))
                 (name (symbol->string symbol)))
            (if (char-lower-case? (car (string->list name)))
                name
                symbol))
         (alist->object (,'quasiquote ,(map (lambda (pair)
                                              (cons (symbol->string (car pair))
                                                    (list 'unquote (cadr pair))))
                                            attrs)))
         ,@(if (null? rest)
              nil
              (let ((first (car rest)))
                (if (pair? first)
                    (cond ((symbol=? 'sxml-unquote (car first))
                           (cdr first))
                          (else
                           (map (lambda (expr)
                                  (%sxml h expr))
                                rest)))
                    (list first)))))))

;; -----------------------------------------------------------------------------
;; mapping ~ and into longer form (the same as built-in , and ,@)
;; -----------------------------------------------------------------------------
(set-special! "~" 'sxml-unquote-mapper)

;; -----------------------------------------------------------------------------
(define (sxml-unquote-mapper expression)
  `(sxml-unquote ,expression))

;; -----------------------------------------------------------------------------
(define (sxml-unquote)
  "(sxml-unquote expression) or ~expression

  Treat expression as code and evaluate it inside sxml, similar to unquote
  with quasiquote."
  (throw "sxml-unquote: Can't use outside of sxml"))

;; -----------------------------------------------------------------------------
(define-macro (pragma->sxml pragma)
  `(define-macro (sxml expr)
     "(sxml expr)

     Macro for JSX like syntax but with SXML.
     e.g. usage:

     (sxml (div (@ (data-foo \"hello\")
                   (id \"foo\"))
                (span \"hello\")
                (span \"world\")))
     ;; ==> <div data-foo=\"hello\" id=\"foo\"><span>hello</span><span>world</span></div>"
     (%sxml ',pragma expr)))

;; -----------------------------------------------------------------------------
(pragma->sxml h)

;; -----------------------------------------------------------------------------
(define-macro (with-tags expr)
  "(with-tags expression)

   valutes LIPS shorter code for S-Expression equivalent of JSX.
   e.g.:

   (with-tags (:div (:class \"item\" :id \"item-1\")
                    (list (:span () \"Random Item\")
                          (:a (:onclick (lambda (e) (alert \"close\")))
                              \"close\"))))

   Above expression can be passed to function that renders JSX (like render in React, Preact)
   To get the string from the macro you can use vhtml library from npm."
  (make-tags expr))

;; -----------------------------------------------------------------------------
(define (get-resource url)
  "(get-resource url)

   Load JavaScript or CSS file in browser by adding script/link tag to head of the current document.
   When called from Node it allow it allows to load JavaScript files only."
  (typecheck "get-resource" url "string")
  (if (not (bound? 'document))
      (if (eq? self global)
          (let ((code (%read-file false url)))
            (self.eval code))
          (throw (new Error "get-script: document not defined")))
      (let ((load (lambda (node)
                    (new Promise (lambda (resolve reject)
                                   (set! node.onload (lambda ()
                                                       (resolve)))
                                   (set! node.onerror (lambda ()
                                                        (reject (string-append
                                                                 "get-resource: Failed to load "
                                                                 url))))
                                   (if document.head
                                       (document.head.appendChild node)))))))
      (cond ((url.match #/.js$/)
             (let ((script (document.createElement "script")))
               (set! script.src url)
               (load script)))
            ((url.match #/.css$/)
             (let ((link (document.createElement "link")))
               (set! link.href url)
               (set! link.rel "stylesheet")
               (load link)))))))

;; -----------------------------------------------------------------------------
(define (gensym? value)
  "(gensym? value)

   Returns #t if value is a symbol created by gensym. It returns #f otherwise."
  (and (symbol? value) (--> value (is_gensym))))

;; -----------------------------------------------------------------------------
(define (degree->radians x)
  "(degree->radians x)

   Convert degrees to radians."
  (* x (/ Math.PI 180)))

;; -----------------------------------------------------------------------------
(define (radians->degree x)
  "(radians->degree x)

   Convert radians to degrees."
  (* x (/ 180 Math.PI)))

;; -----------------------------------------------------------------------------
(define-syntax ++
  (syntax-rules ()
    ((++ x)
     (let ((tmp (+ x 1)))
       (set! x tmp)
       tmp)))
  "(++ variable)

   Works only on variables and increment the value by one.")

;; -----------------------------------------------------------------------------
(define-syntax --
  (syntax-rules ()
    ((-- x)
     (let ((tmp (- x 1)))
       (set! x tmp)
       tmp)))
  "(-- variable)

   Works only on variables and decrements the value by one.")

;; -----------------------------------------------------------------------------
(define (pretty-format . lists)
  "(pretty-format pair)

   Returns a pretty printed string from pair expression."
  (let ((code (--> (list->vector lists)
                   (map (lambda (pair i)
                          (typecheck "pretty-pair" pair "pair" i)
                          (repr pair true)))
                   (join ""))))
    (--> (new lips.Formatter code) (break) (format))))

;; -----------------------------------------------------------------------------
(define (reset)
  "(reset)

  Function resets the environment and removes all user defined variables."
  (let-env **interaction-environment**
           (let ((defaults **interaction-environment-defaults**)
                 (env **interaction-environment**))
             (--> env (list) (forEach (lambda (name)
                                        (if (not (--> defaults (includes name)))
                                            (--> env (unset name)))))))))

;; -----------------------------------------------------------------------------
(define (make-list n . rest)
  (if (or (not (integer? n)) (<= n 0))
      (throw (new Error "make-list: first argument need to be integer larger then 0"))
      (let ((fill (if (null? rest) undefined (car rest))))
        (array->list (--> (new Array n) (fill fill))))))

;; -----------------------------------------------------------------------------
(define (range stop . rest)
  "(range stop)
   (range start stop)
   (range start stop step)

   Returns a list of numbers from start to stop with optional step.
   If start is not defined it starts from 0. If start is larger than stop
   the step needs to be negative otherwise it will hang in an infinite loop."
  (let* ((i (if (null? rest) 0 stop))
         (stop (if (null? rest) stop (car rest)))
         (step (if (or (null? rest) (null? (cdr rest)))
                   1
                   (cadr rest)))
         (test (cond
                ((> i stop) (lambda (i)
                              (and (< step 0) (>= i stop))))
                ((< i stop) (lambda
                              (i) (and (> step 0) (< i stop))))
                (else (lambda () false))))
         (result (vector)))
    (typecheck "range" i "number" 1)
    (typecheck "range" step "number" 2)
    (typecheck "range" stop "number" 3)
    (while (test i)
      (result.push i)
      (set! i (+ i step)))
    (array->list result)))

;; -----------------------------------------------------------------------------
(define-macro (do-iterator spec cond . body)
  "(do-iterator (var expr) (test result) body ...)

   Iterates over iterators (e.g. creates with JavaScript generator function)
   that works with normal and async iterators. You can loop over infinite iterators
   and break the loop if you want, using expression like in do macro. Long synchronous iterators
   will block the main thread (you can't print 1000 numbers from infinite iterators,
   because it will freeze the browser), but if you use async iterators you can process
   the values as they are generated."
  (let ((gen (gensym "name"))
        (name (car spec))
        (async (gensym "async"))
        (sync (gensym "sync"))
        (iterator (gensym "iterator"))
        (test (if (null? cond) #f (car cond)))
        (result (if (null? cond) undefined (cadr cond)))
        (next (gensym "next"))
        (stop (gensym "stop"))
        (item (gensym "item")))
    `(let* ((,gen ,(cadr spec))
            (,sync (. ,gen Symbol.iterator))
            (,async (. ,gen Symbol.asyncIterator))
            (,iterator)
            (,next (lambda ()
                     ((. ,iterator "next")))))
          (if (or (procedure? ,sync) (procedure? ,async))
              (begin
                 (set! ,iterator (if (procedure? ,sync) (,sync) (,async)))
                 (let* ((,item (,next))
                        (,stop #f)
                        (,name (. ,item "value")))
                   (while (not (or (eq? (. ,item "done") #t) ,stop))
                     (if ,test
                         (set! ,stop #t)
                         (begin
                           ,@body))
                      (set! ,item (,next))
                      (set! ,name (. ,item "value"))))
                   ,result)))))

;; -----------------------------------------------------------------------------
(define (iterator->array object)
  "(iterator->array object)

   Return array from JavaScript iterator object."
  (let ((result (vector))
        (i 0))
    (do-iterator
     (item object)
     (#f result)
     (set-obj! result i item)
     (set! i (+ i 1)))))

;; -----------------------------------------------------------------------------
(set-repr! Set (lambda () "#<Set>"))
(set-repr! Map (lambda () "#<Map>"))

;; -----------------------------------------------------------------------------
(define (native-symbol? x)
  "(native-symbol? object)

   Checks if value is JavaScript Symbol."
  (and (string=? (type x) "symbol") (not (symbol? x))))

;; -----------------------------------------------------------------------------
(set-special! "’" 'warn-quote)

;; -----------------------------------------------------------------------------
(define-macro (warn-quote)
  "(warn-quote)

   Simple macro that throws an error, when you try to use ’ symbol as quote in code."
  (throw (new Error (string-append "You're using an invalid Unicode quote character. Run: "
                                   "(set-special! \"’\" 'quote)"
                                   " to allow the use of this type of quote"))))

;; -----------------------------------------------------------------------------
(define-macro (let-env-values env spec . body)
  "(let-env-values env ((name var)) . body)

   Adds mappings for variables var from specified env.
   it is similar to let-env but lexical scope is working with it."
  (let ((env-name (gensym 'env)))
    `(let ((,env-name ,env))
       (let ,(map (lambda (pair)
                    `(,(car pair) (--> ,env-name (get ',(cadr pair)))))
                  spec)
         ,@body))))

;; -----------------------------------------------------------------------------
(define (apropos name)
  "(apropos name)

   Search the current environment and display names that match the given name.
   name can be regex, string or symbol."
  (typecheck "apropos" name '("string" "regex" "symbol"))
  (let ((regex (lambda (string)
                 (new RegExp (escape-regex string)))))
    (filter (cond ((string? name) (regex name))
                  ((symbol? name) (regex (symbol->string name)))
                  (else name))
            (env (interaction-environment)))))

;; -----------------------------------------------------------------------------
(define (promisify fn)
  "(promisify fn)

   Simple function for adding promises to NodeJS two-callback based functions.
   Function tested only with fs module."
  (typecheck "promisify" fn "function")
  (lambda args
    (new Promise (lambda (resolve reject)
                   (apply fn (append args (list (lambda (err data)
                                                  (if (null? err)
                                                      (resolve data)
                                                      (reject err))))))))))

;; -----------------------------------------------------------------------------
(define-macro (list* . args)
  "(list* arg1 ...)

   Parallel asynchronous version of list. Like begin* except all values are returned in a list."
  (let ((result (gensym "result")))
     `(let ((,result (vector)))
        ,@(map (lambda (arg)
                 `(--> ,result (push '>,arg)))
               args)
        (map await (vector->list ,result)))))

;; -----------------------------------------------------------------------------
(define-macro (%not-implemented name)
  "(%not-implemented name)

   Returns new function that throws an exception with a message that this function is not implemented."
  (let ((str-name (symbol->string name)))
    `(lambda ()
       ,(string-append "(" str-name ")\n\nThis function is not yet implemented.")
       (throw (new Error ,(string-append str-name " has not been implemented"))))))

;; -----------------------------------------------------------------------------
(define-macro (%make-env name . names)
  "(%make-env name f1 f2 ...)

   Creates a new Environment with given name and defined symbols in it from the global env.
   If given function name f1 f2 ... don't exist, it will define them as functions that
   throw exception that function is not yet implemented."
  `(new lips.Environment (alist->object (list ,@(map (lambda (name)
                                                       `(cons ',name ,(let ((ref (lips.env.ref name)))
                                                                       (if (null? ref)
                                                                           `(%not-implemented ,name)
                                                                           `(lips.env.get ',name)))))
                                                     names)))
        (new lips.Environment (object
                               :interaction-environment interaction-environment
                               :**interaction-environment** **interaction-environment**)
             null "root")
        ,name))

;; -----------------------------------------------------------------------------
(define Y
  (lambda (h)
    "(Y f)

       _ __   __    _            _       _      _ __   __         _   _  _
      /  \\ \\ / /   /  __        /   ____  \\    /  \\ \\ / /    ____  \\   \\  \\
     +    \\ v /   +   \\ \\      +   / ___|  +  +    \\ v /    / ___|  +   +  +
     |     \\ /    |    \\ \\     |  | |__    |  |     \\ /    | |__    |   |  |
     |     | |    |    /  \\    |  |  __|   |  |     | |    |  __|   |   |  |
     |     | |    |   / /\\ \\   |  | |      |  |     | |    | |      |   |  |
     +     |_|    +  /_/  \\_\\  +  |_|      +  +     |_|    |_|      +   +  +
      \\_           \\_           \\_       _/    \\_                 _/  _/ _/"
          ((lambda (x) (x x))
           (lambda (g)
             (h (lambda args (apply (g g) args)))))))

;; -----------------------------------------------------------------------------
(define (indexed-db?)
  "(indexed-db?)

   Function that tests if IndexedDB is available."
  (let* ((any (lambda args
                (let iter ((args args))
                  (if (null? args)
                      false
                      (if (not (null? (car args)))
                          (car args)
                          (iter (cdr args)))))))
         (indexedDB (any window.indexedDB
                         window.indexedDB
                         window.mozIndexedDB
                         window.webkitIndexedDB)))
    (if (not (null? indexedDB))
        (try
         (begin
           ;; open will fail in about:blank
           (window.indexedDB.open "IndexedDBExistenceCheck" 3)
           true)
         (catch (e)
                false))
        false)))

;; -----------------------------------------------------------------------------
(define (environment? obj)
  "(environment? obj)

   Checks if object is a LIPS environment."
  (instanceof lips.Environment obj))

;; -----------------------------------------------------------------------------
(define %read-file
  (let ((read-file #f) (fetch-url #f))
    (lambda (binary path)
      "(%read-file binary path)

       Read file from url or file system. If binary is false it will return a
       string that contain all the content. For HTTP requests, if binary
       is false it will return an ArrayBuffer (when in a browser) or a Buffer
       (when in NodeJS). When reading from the file system in both cases it will
       return Buffer objects.

       Code that uses this function in binary mode needs to check
       if the result is ArrayBuffer or Node.js/BrowserFS Buffer object."
      (if (not read-file)
          (let ((fs (--> (interaction-environment)
                         (get '**internal-env**)
                         (get 'fs &(:throwError false)))))
            (if (not (null? fs))
                (let ((*read-file* (promisify fs.readFile)))
                  (set! read-file (lambda (path binary)
                                   (let ((buff (*read-file* path)))
                                     (if binary
                                         (if (eq? self window)
                                             (new Blob (vector buff))
                                             buff)
                                         (--> buff (toString))))))))))
      (if (not fetch-url)
          (set! fetch-url (lambda (url binary)
                            (if (eq? self window)
                                (let ((res (fetch url)))
                                  (if binary
                                      (res.arrayBuffer)
                                      (res.text)))
                                (http-get url binary)))))
      (if (not read-file)
          (fetch-url path binary)
          (if (file-exists? path)
              (read-file path binary)
              (fetch-url path binary))))))

;; -----------------------------------------------------------------------------
(define %read-binary-file (curry %read-file true))
(define %read-text-file (curry %read-file false))

;; -----------------------------------------------------------------------------
(define (%fs-promisify-proc fn message)
  "(%fs-promisify-proc fn string)

   Returns a promisified version of a fs function or throws an exception
   if fs is not available."
  (let ((fs (--> lips.env (get '**internal-env**) (get 'fs &(:throwError false)))))
    (if (null? fs)
        (throw (new Error (string-append message ": fs not defined")))
        (promisify (. fs fn)))))

;; -----------------------------------------------------------------------------
(define (response->content binary res)
  "(response->text binary res)

   Reads all text from a Node.js HTTP response object. If binary argument
   is true it will return Buffer object that can be converted to u8vector.

   ***Warning:*** it may overflow the Javascript call stack when converting the
   whole buffer to u8vector, because LIPS doesn't have TCO."
  (let ((result (vector))
        (append (if binary
                    (lambda (chunk)
                      (result.push (Buffer.from chunk "binary")))
                    (lambda (chunk)
                      (result.push chunk)))))
    (res.setEncoding (if binary "binary" "utf8"))
    (new Promise (lambda (resolve)
                   (res.on "data" append)
                   (res.on "end" (lambda ()
                                   (if binary
                                       (resolve (Buffer.concat result))
                                       (resolve (result.join "")))))))))

;; -----------------------------------------------------------------------------
(define response->buffer (curry response->content true))
(define response->text (curry response->content false))

;; -----------------------------------------------------------------------------
(define http-get
  (if (eq? self window)
      (lambda (url binary)
        "(http-get url)

         This function is only available when running LIPS in NodeJS."
        (throw (new Error "http-get: function is Node.js only.")))
      (let* ((http (. (require "http") 'get))
             (https (. (require "https") 'get)))
        (lambda (url binary)
          "(http-get url)

           Node.js function that sends a HTTP Request and returns a string or
           binary Buffer object."
          (let ((request (if (null? (url.match #/^https/)) http https)))
            (new Promise
                 (lambda (resolve reject)
                   (--> (request url
                                 (lambda (res)
                                   (if (= res.statusCode 200)
                                       (resolve (response->content binary res))
                                       (let ((code res.statusCode))
                                         (res.resume)
                                         (reject (string-append
                                                  "Request return "
                                                  (number->string code)))))))
                        (on "error" reject)))))))))

;; -----------------------------------------------------------------------------
(define (buffer->u8vector bin)
  "(buffer->u8vector bin)

   Cross platform function that can be used in both Node and browser.
   It can be used together with %read-file or %read-binary-file to convert
   the result ArrayBuffer or Buffer to u8vector."
  (if (instanceof ArrayBuffer bin)
      (new Uint8Array bin)
      (Uint8Array.from bin)))

;; -----------------------------------------------------------------------------
(define (complement fn)
  "(complement fn)

   Higher order function that returns the Boolean complement of the given function. If the function fn
   for a given arguments return true the result function will return false, if it would
   return false, the result function will return true."
  (typecheck "complement" fn "function")
  (lambda args
    (not (apply fn args))))

;; -----------------------------------------------------------------------------
(define (always constant)
  "(always constant)

   Higher-order function that returns a new thunk that always returns the given constant when called."
  (lambda ()
    constant))

;; -----------------------------------------------------------------------------
(define (once fn)
  "(once fn)

   Higher-order function that returns a new function, that only calls the original
   on the first invocation, and immediately returns the first call's result again
   on subsequent invocations."
  (typecheck "once" fn "function")
  (let ((result))
    (lambda args
      (if (string=? (type result) "undefined")
          (set! result (apply fn args)))
      result)))

;; -----------------------------------------------------------------------------
(define (flip fn)
  "(flip fn)

   Higher-order function that returns a new function where the first two arguments are swapped.

   Example:

     (define first (curry (flip vector-ref) 0))
     (first #(1 2 3))
     ;; ==> 1"
  (typecheck "flip" fn "function")
  (lambda (a b . rest)
    (apply fn b a rest)))

;; -----------------------------------------------------------------------------
(define (unfold fn init)
  "(unfold fn init)

   Returns a list from the given function and init value. The function should
   return a pair where first is the item added to the list and second is next value
   passed to the function. If the function returns false it ends the loop."
  (typecheck "unfold" fn "function")
  (let iter ((pair (fn init)) (result '()))
    (if (not pair)
        (reverse result)
        (iter (fn (cdr pair)) (cons (car pair) result)))))

;; -----------------------------------------------------------------------------
(define string-join join)
(define string-split split)

;; -----------------------------------------------------------------------------
(define (%nth-pair msg l k)
  "(%nth-pair msg list n)

   Returns nth pair of a list with given message on error."
  (typecheck msg l '("pair" "nil"))
  (typecheck-number msg k '("integer" "bigint"))
  (if (< k 0)
      (throw (new Error (string-append msg ": index out of range")))
      (let ((l l) (k k))
        (while (> k 0)
          (if (or (null? (cdr l)) (null? l))
              (throw (new Error (string-append msg ": not enough elements in the list"))))
          (set! l (cdr l))
          (set! k (- k 1)))
        l)))

;; -----------------------------------------------------------------------------
(define (nth-pair l k)
  "(nth-pair list n)

   Returns nth pair of a list."
  (%nth-pair "nth-pair" l k))

;; -----------------------------------------------------------------------------
(define (symbol-append . rest)
   "(symbol-append s1 s2 ...)

    Function that creates a new symbol from symbols passed as arguments."
   (string->symbol (apply string-append (map symbol->string rest))))

;; -----------------------------------------------------------------------------
(define-macro (set-global! name)
   "(set-global! name)

    Macro to make the name a Javascript global variable (i.e. accessible on globalThis)."
   (let ((var (symbol-append 'self. name)))
     `(set! ,var ,name)))

;; -----------------------------------------------------------------------------
(define performance (if (and (eq? self global) (not (bound? 'performance)))
                        (. (require "perf_hooks") 'performance)
                        performance))
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
;; Copyright (C) 2019-2024 Jakub T. Jankiewicz <https://jcubic.pl/me>
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
(define procedure? function?)
(define expt **)
(define list->vector list->array)
(define vector->list array->list)
(define call-with-current-continuation call/cc)

;; -----------------------------------------------------------------------------
(define (procedure? obj)
  "(procedure? expression)

   Predicate that tests if value is a callable function or continuation."
  (or (function? obj) (continuation? obj)))

;; -----------------------------------------------------------------------------
(define-macro (define-symbol-macro type spec . rest)
  "(define-symbol-macro type (name . args) . body)

   Creates special symbol macros for evaluator similar to built-in , or `.
   It's like an alias for a real macro. Similar to CL reader macros but it receives already
   parsed code like normal macros. Type can be SPLICE or LITERAL symbols (see set-special!).
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
(define (vector . rest)
  "(vector 1 2 3 (+ 3 1)) or #(1 2 3 4)

   Macro for defining vectors (Javascript Arrays). Vector literals are
   automatically quoted, so you can't use expressions inside them, only other
   literals, like other vectors or objects."
  (list->array rest))

;; -----------------------------------------------------------------------------
(set-repr! Array
           (lambda (arr q)
             ;; Array.from is used to convert empty to undefined
             ;; but we can't use the value because Array.from calls
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

   Function that compares the values. It returns true if they are the same, they
   need to have the same type."
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
            ((and (pair? a) (null? a)) (null? b))
            (else (eq? a b)))
      false))

;; -----------------------------------------------------------------------------
(define (equal? a b)
  "(equal? a b)

   The function checks if values are equal. If both are a pair or an array
   it compares their elements recursively."
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
        ((instance? a)
         (and (instance? b)
              (%same-functions b.constructor a.constructor)
              (function? a.equal)
              (a.equal b)))
        (else (eqv? a b))))

;; -----------------------------------------------------------------------------
(define make-promise
  (lambda (proc)
    "(make-promise fn)

     Function that creates a promise from a function."
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

   Will create a promise from expression that can be forced with (force)."
  `(make-promise (lambda () ,expression)))

;; -----------------------------------------------------------------------------
(define (force promise)
  "(force promise)

   Function that forces the promise and evaluates the delayed expression."
  (promise))

;; -----------------------------------------------------------------------------
(define (promise? obj)
  "(promise? obj)

   Checks if the value is a promise created with delay or make-promise."
  (string=? (type obj) "promise"))

;; -----------------------------------------------------------------------------
(define (positive? x)
  "(positive? x)

   Checks if the number is larger then 0"
  (typecheck "positive?" x "number")
  (> x 0))

;; -----------------------------------------------------------------------------
(define (negative? x)
  "(negative? x)

   Checks if the number is smaller then 0"
  (typecheck "negative?" x "number")
  (< x 0))

;; -----------------------------------------------------------------------------
(define (zero? x)
  "(zero? x)

   Checks if the number is equal to 0"
  (typecheck "zero?" x "number")
  (= x 0))

;; -----------------------------------------------------------------------------
(define (quotient a b)
  "(quotient a b)

   Return quotient from division as integer."
  (typecheck "quotient" a "number")
  (typecheck "quotient" b "number")
  (if (zero? b 0)
     (throw (new Error "quotient: division by zero"))
     (let ((quotient (/ a b)))
       (if (integer? quotient)
           quotient
           (if (> quotient 0)
               (floor quotient)
               (ceiling quotient))))))

;; -----------------------------------------------------------------------------
(define (number->string x . rest)
  "(number->string x [radix])

   Function that converts number to string with optional radix (number base)."
  (typecheck "number->string" x "number" 1)
  (let ((radix (if (null? rest) 10 (car rest))))
    (typecheck "number->string" radix "number" 2)
    (--> x (toString (--> radix (valueOf))))))

;; -----------------------------------------------------------------------------
(define (boolean? x)
  "(boolean? x)

   Returns true if value is boolean."
   (string=? (type x) "boolean"))

;; -----------------------------------------------------------------------------
(define (vector-ref vector i)
  "(vector-ref vector i)

   Return i-th element from vector."
  (typecheck "number->string" vector "array" 1)
  (typecheck "number->string" i "number" 2)
  (. vector i))

;; -----------------------------------------------------------------------------
(define (vector-set! vector i obj)
  "(vector-set! vector i obj)

   Set obj as value in vector at position i."
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

   Checks if the argument x is real."
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

  Checks if the argument x is integer."
  (and (number? x)
       (not (eq? x NaN))
       (not (eq? x Number.NEGATIVE_INFINITY))
       (not (eq? x Number.POSITIVE_INFINITY))
       (or (%number-type "bigint" x)
           (%number-type "integer" x)
           (and (%number-type "float" x)
                (= (modulo x 2) 1)))))

;; -----------------------------------------------------------------------------
(define (complex? x)
  "(complex? x)

  Checks if argument x is complex."
  (and (number? x) (or (eq? x NaN)
                       (eq? x Number.NEGATIVE_INFINITY)
                       (eq? x Number.POSITIVE_INFINITY)
                       (%number-type '("complex" "float" "bigint" "rational") x))))

;; -----------------------------------------------------------------------------
(define (rational? x)
  "(rational? x)

  Checks if the value is rational."
  (and (number? x)
       (not (eq? x NaN))
       (not (eq? x Number.NEGATIVE_INFINITY))
       (not (eq? x Number.POSITIVE_INFINITY))
       (or (%number-type "rational" x) (integer? x))))

;; -----------------------------------------------------------------------------
(define (typecheck-args _type name _list)
  "(typecheck-args args type)

   Function that makes sure that all items in the array are of same type."
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

   Returns the maximum of its arguments."
  (numbers? "max" args)
  (apply Math.max args))

;; -----------------------------------------------------------------------------
(define (min . args)
  "(min n1 n2 ...)

   Returns the minimum of its arguments."
  (numbers? "min" args)
  (apply Math.min args))

;; -----------------------------------------------------------------------------
(define (make-rectangular re im)
  "(make-rectangular im re)

   Creates a complex number from imaginary and real part (a+bi form)."
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

   Function that converts real number to exact rational number."
  (typecheck "inexact->exact" n "number")
  (if (exact? n)
      n
      (--> n (toRational))))

;; -----------------------------------------------------------------------------
(define (log z)
  "(log z)

   Function that calculates natural logarithm of z where the argument can be
   any number (including complex negative and rational).
   If the value is 0 it return NaN."
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
         (log (exact->inexact z)))))

;; -----------------------------------------------------------------------------
;; generate Math functions with documentation
(define _maths (list "asin" "acos"))

;; -----------------------------------------------------------------------------
(define _this_env (current-environment))

;; -----------------------------------------------------------------------------
(let iter ((fns _maths))
  (if (not (null? fns))
      (let* ((name (car fns))
             (op (. Math name))
             (fn (lambda (n) (lips.LNumber (op n)))))
        (--> _this_env (set name fn))
        (set-obj! fn '__doc__ (concat "(" name " n)\n\nFunction that calculates " name
                                  " math operation (it call JavaScript Math." name
                                  " function)"))
        (iter (cdr fns)))))

;; -----------------------------------------------------------------------------
(define (sin n)
  "(sin n)

  Function that calculates sine of a number."
  (typecheck "sin" n "number")
  (if (string=? n.__type__ "complex")
      (let ((re (real-part n))
            (im (imag-part n)))
        (lips.LComplex (object :re (* (Math.sin re)
                                      (Math.cosh im))
                               :im (* (Math.cos re)
                                      (Math.sinh im)))))
      (Math.sin n)))

;; -----------------------------------------------------------------------------
(define (cos n)
  "(cos n)

  Function that calculates cosine of a number."
  (typecheck "cos" n "number")
  (if (string=? n.__type__ "complex")
      (let ((re (real-part n))
            (im (imag-part n)))
        (lips.LComplex (object :re (* (Math.cos re)
                                      (Math.cosh im))
                               :im (- (* (Math.sin re)
                                         (Math.sinh im))))))
      (Math.cos n)))

;; -----------------------------------------------------------------------------
(define (tan n)
  "(tan n)

  Function that calculates tangent of a number."
  (typecheck "tan" n "number")
  (if (string=? n.__type__ "complex")
      (let* ((re (real-part n))
             (im (imag-part n))
             (re2 (* 2 re))
             (im2 (* 2 im)))
        (lips.LComplex (object :re (/ (Math.sin re2)
                                      (+ (Math.cos re2)
                                         (Math.cosh im2)))
                               :im (/ (Math.sinh im2)
                                      (+ (Math.cos re2)
                                         (Math.cosh im2))))))
      (Math.tan n)))

;; -----------------------------------------------------------------------------
(define (atan z . rest)
  "(atan z)
   (atan x y)

   Function calculates arcus tangent of a complex number.
   If two arguments are passed and they are not complex numbers
   it calculate Math.atan2 on those arguments."
  (if (and (null? rest) (complex? z))
      (cond ((nan? z) +nan.0)
            ((infinite? z)
             (let ((atan (/ Math.PI 2)))
               (if (< z 0)
                   (- atan)
                   atan)))
            (else
             ;; ref: https://youtu.be/d93AarE0lKg
             (let ((iz (* +i z)))
               (* (/ 1 +2i)
                  (log (/ (+ 1 iz)
                          (- 1 iz)))))))
      (let ((x z) (y (car rest)))
        (if (and (zero? (imag-part x))
                 (zero? (imag-part y)))
            (Math.atan2 x y)
            (error "atan: can't call with two complex numbers")))))

;; -----------------------------------------------------------------------------
(define (exp n)
  "(exp n)

  Function that calculates e raised to the power of n."
  (typecheck "exp" n "number")
  (if (string=? n.__type__ "complex")
      (let* ((re (real-part n))
             (im (imag-part n))
             (factor (Math.exp re)))
         (make-rectangular (* factor (cos im))
                           (* factor (sin im))))
       (Math.exp n)))

;; -----------------------------------------------------------------------------
(define (modulo a b)
  "(modulo a b)

   Returns modulo operation on its argumennts."
  (typecheck "modulo" a "number" 1)
  (typecheck "modulo" b "number" 2)
  (- a (* b (floor (/ a b)))))
;; -----------------------------------------------------------------------------
(define (remainder__ a b)
  "(modulo a b)

   Returns remainder from division operation."
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

   Returns n-th element of a list."
  (let ((l (%nth-pair "list-ref" l k)))
    (if (null? l)
        l
        (car l))))

;; -----------------------------------------------------------------------------
(define (not x)
  "(not x)

   Returns true if value is false and false otherwise."
  (if x false true))

;; -----------------------------------------------------------------------------
(define (rationalize number tolerance)
  "(rationalize number tolerance)

   Returns simplest rational number approximation differing from number by no more
   than the tolerance."
  (typecheck "rationalize" number "number" 1)
  (typecheck "rationalize" tolerance "number" 2)
  (lips.rationalize number tolerance))

;; -----------------------------------------------------------------------------
(define (%mem/search access op obj list)
  "(%member obj list function)

   Helper method to get first list where car equal to obj
   using provided function as comparator."
  (if (null? list)
      false
      (if (op (access list) obj)
          list
          (%mem/search access op obj (cdr list)))))

;; -----------------------------------------------------------------------------
(define (memq obj list)
  "(memq obj list)

   Returns first object in the list that match using eq? function."
  (typecheck "memq" list '("nil" "pair"))
  (%mem/search car eq? obj list ))

;; -----------------------------------------------------------------------------
(define (memv obj list)
  "(memv obj list)

   Returns first object in the list that match using eqv? function."
  (typecheck "memv" list '("nil" "pair"))
  (%mem/search car eqv? obj list))

;; -----------------------------------------------------------------------------
(define (member obj list)
  "(member obj list)

   Returns first object in the list that match using equal? function."
  (typecheck "member" list '("nil" "pair"))
  (%mem/search car equal? obj list))

;; -----------------------------------------------------------------------------
(define (%assoc/accessor name)
  "(%assoc/accessor name)

   Returns carr with typecheck using give name."
  (lambda (x)
    (typecheck name x "pair")
    (caar x)))

;; -----------------------------------------------------------------------------
(define (%assoc/search op obj alist)
  "(%assoc/search op obj alist)

   Generic function that used in assoc functions with defined comparator
   function."
  (typecheck "assoc" alist (vector "nil" "pair"))
  (let ((ret (%mem/search (%assoc/accessor "assoc") op obj alist)))
    (if ret
        (car ret)
        ret)))

;; -----------------------------------------------------------------------------
(define assoc (%doc
               "(assoc obj alist)

                Returns pair from alist that match given key using equal? check."
               (curry %assoc/search equal?)))

;; -----------------------------------------------------------------------------
(define assq (%doc
              "(assq obj alist)

               Returns pair from a list that matches given key using eq? check."
              (curry %assoc/search eq?)))

;; -----------------------------------------------------------------------------
(define assv (%doc
              "(assv obj alist)

               Returns pair from alist that match given key using eqv? check."
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

   Returns new string with k elements. If char is provided
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

   Function that creates a new string from it's arguments. Each argument
   needs to be a character object."
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

   Function that destructively fills the string with given character."
  (typecheck "string-fill!" string "string" 1)
  (typecheck "string-fill!" char "character" 2)
  (string.fill char))

;; -----------------------------------------------------------------------------
(define (identity n)
  "(identity n)

   No-op function. It just returns its argument."
  n)

;; -----------------------------------------------------------------------------
(define (string-copy x)
  "(string-copy x)

   Creates a new string based on given argument."
  (typecheck "string-copy" x "string")
  (lips.LString x))

;; -----------------------------------------------------------------------------
(define (list->string _list)
  "(list->string _list)

   Returns a string from a list of characters."
  (let ((array (list->array
                (map (lambda (x)
                       (typecheck "list->string" x "character")
                       (x.valueOf))
                     _list))))
    (--> array (join ""))))

;; -----------------------------------------------------------------------------
(define (string->list string)
  "(string->list string)

   Returns a list of characters created from string."
  (typecheck "string->list" string "string")
  (array->list (--> (Array.from string)
                    (map (lambda (x)
                           (lips.LCharacter x))))))

;; -----------------------------------------------------------------------------
(define-macro (string-set! object index char)
  "(string-set! string index char)

   Replaces character in string at a given index."
  (let ((input (gensym "input")))
    `(begin
       (let ((,input ,object))
         (typecheck "string-set!" ,input "string")
         (typecheck "string-set!" ,index "number")
         (typecheck "string-set!" ,char "character")
         (try
          (--> ,input (set ,index  ,char))
          (catch (e)
                 (error "string-set!: attempt to change an immutable string")))))))

;; -----------------------------------------------------------------------------
(define (string-length string)
  "(string-length string)

   Returns the length of the string."
  (typecheck "string-length" string "string")
  (. string 'length))

;; -----------------------------------------------------------------------------
(define (string-ref string k)
  "(string-ref string k)

   Returns character inside string at given zero-based index."
  (typecheck "string-ref" string "string" 1)
  (typecheck "string-ref" k "number" 2)
  (lips.LCharacter (string.get k)))

(define (%string-cmp name string1 string2)
  "(%string-cmp name a b)

   Function that compares two strings and returns 0 if they are equal,
   -1 if it is smaller and 1 if is larger. The function compares
   the codepoints of the character."
  (typecheck name string1 "string" 1)
  (typecheck name string2 "string" 2)
  (--> string1 (cmp string2)))

;; -----------------------------------------------------------------------------
(define (string=? string1 string2)
  "(string=? string1 string2)

   Checks if two strings are equal."
  (= (%string-cmp "string=?" string1 string2) 0))

;; -----------------------------------------------------------------------------
(define (string<? string1 string2)
  "(string<? string1 string2)

   Returns true if the second string is smaller than the first one."
  (= (%string-cmp "string<?" string1 string2) -1))

;; -----------------------------------------------------------------------------
(define (string>? string1 string2)
  "(string<? string1 string2)

   Returns true if the second string is larger than the first one."
  (= (%string-cmp "string>?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string<=? string1 string2)
  "(string<? string1 string2)

   Returns true if the second string is not larger than the first one."
  (< (%string-cmp "string<=?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string>=? string1 string2)
  "(string<? string1 string2)

   Returns true if second character is not smaller then the first one."
  (> (%string-cmp "string>=?" string1 string2) -1))

;; -----------------------------------------------------------------------------
(define (%string-ci-cmp name string1 string2)
  "(%string-ci-cmp name a b)

   Function that compares two strings ignoring case and returns 0 if they are equal,
   -1 if it is smaller and 1 if is larger. The function compares
   the codepoints of the character."
  (typecheck name string1 "string" 1)
  (typecheck name string2 "string" 2)
  (--> string1 (lower) (cmp (--> string2 (lower)))))

;; -----------------------------------------------------------------------------
(define (string-ci=? string1 string2)
  "(string-ci=? string1 string2)

   Checks if two strings are equal, ignoring case."
  (= (%string-ci-cmp "string-ci=?" string1 string2) 0))

;; -----------------------------------------------------------------------------
(define (string-ci<? string1 string2)
  "(string-ci<? string1 string2)

   Returns true if the second string is smaller than the first one, ignoring case."
  (= (%string-ci-cmp "string-ci<?" string1 string2) -1))

;; -----------------------------------------------------------------------------
(define (string-ci>? string1 string2)
  "(string-ci<? string1 string2)

   Returns true if the second string is larger than the first one, ignoring case."
  (= (%string-ci-cmp "string-ci>?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string-ci<=? string1 string2)
  "(string-ci<? string1 string2)

   Returns true if the second string is not larger than the first one, ignoring case."
  (< (%string-ci-cmp "string-ci<=?" string1 string2) 1))

;; -----------------------------------------------------------------------------
(define (string-ci>=? string1 string2)
  "(string-ci>=? string1 string2)

   Returns true if second character is not smaller than the first one, ignoring case."
  (> (%string-ci-cmp "string-ci>=?" string1 string2) -1))

;; -----------------------------------------------------------------------------
;; CHARACTER FUNCTIONS
;; -----------------------------------------------------------------------------

;; (display (list->string (list #\A (integer->char 10) #\B)))
;; -----------------------------------------------------------------------------
(define char? (%doc
        "(char? obj)

         Checks if the object is a character."
        (curry instanceof lips.LCharacter)))

;; -----------------------------------------------------------------------------
(define (char->integer chr)
  "(char->integer chr)

   Returns the codepoint of Unicode character."
  (typecheck "char->integer" chr "character")
  (--> chr.__char__ (codePointAt 0)))

;; -----------------------------------------------------------------------------
(define (integer->char n)
  "(integer->char chr)

   Function that converts number argument to character."
  (typecheck "integer->char" n "number")
  (if (integer? n)
      (string-ref (String.fromCodePoint n) 0)
      (throw "argument to integer->char need to be integer.")))

;; -----------------------------------------------------------------------------
(define-macro (%define-chr-re spec str re)
  "(%define-chr-re (name chr) string re)

   Macro defines the procedure that tests character against regular expression."
  `(define ,spec
     ,str
     (typecheck ,(symbol->string (car spec)) ,(cadr spec) "character")
     (not (null? (--> chr (toString) (match ,re))))))

;; -----------------------------------------------------------------------------
(%define-chr-re (char-whitespace? chr)
  "(char-whitespace? chr)

   Returns true if character is whitespace."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'space-unicode-regex))))

;; -----------------------------------------------------------------------------
(%define-chr-re (char-numeric? chr)
  "(char-numeric? chr)

   Returns true if character is number."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'numeral-unicode-regex))))

;; -----------------------------------------------------------------------------
(%define-chr-re (char-alphabetic? chr)
  "(char-alphabetic? chr)

   Returns true if character is leter of the ASCII alphabet."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'letter-unicode-regex))))

;; -----------------------------------------------------------------------------
(define (%char-cmp name chr1 chr2)
  "(%char-cmp name a b)

   Function that compares two characters and return 0 if they are equal,
   -1 second is smaller and 1 if is larger. The function compare
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

   Checks if two characters are equal."
  (= (%char-cmp "char=?" chr1 chr2) 0))

;; -----------------------------------------------------------------------------
(define (char<? chr1 chr2)
  "(char<? chr1 chr2)

   Returns true if second character is smaller then the first one."
  (= (%char-cmp "char<?" chr1 chr2) -1))

;; -----------------------------------------------------------------------------
(define (char>? chr1 chr2)
  "(char<? chr1 chr2)

   Returns true if second character is larger then the first one."
  (= (%char-cmp "char>?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char<=? chr1 chr2)
  "(char<? chr1 chr2)

   Returns true if second character is not larger then the first one."
  (< (%char-cmp "char<=?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char>=? chr1 chr2)
  "(char<? chr1 chr2)

   Returns true if second character is not smaller then the first one."
  (> (%char-cmp "char>=?" chr1 chr2) -1))

;; -----------------------------------------------------------------------------
(define (%char-ci-cmp name chr1 chr2)
  "(%char-cmp name a b)

   Function that compares two characters and return 0 if they are equal,
   -1 second is smaller and 1 if is larger. The function compare
   the codepoints of the character."
  (typecheck name chr1 "character" 1)
  (typecheck name chr2 "character" 2)
  (%char-cmp name (char-downcase chr1) (char-downcase chr2)))

;; -----------------------------------------------------------------------------
(define (char-ci=? chr1 chr2)
  "(char-ci=? chr1 chr2)

   Checks if two characters are equal."
  (= (%char-ci-cmp "char-ci=?" chr1 chr2) 0))

;; -----------------------------------------------------------------------------
(define (char-ci<? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Returns true if second character is smaller then the first one."
  (= (%char-ci-cmp "char-ci<?" chr1 chr2) -1))

;; -----------------------------------------------------------------------------
(define (char-ci>? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Returns true if second character is larger then the first one."
  (= (%char-ci-cmp "char-ci>?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char-ci<=? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Returns true if second character is not larger then the first one."
  (< (%char-ci-cmp "char-ci<=?" chr1 chr2) 1))

;; -----------------------------------------------------------------------------
(define (char-ci>=? chr1 chr2)
  "(char-ci<? chr1 chr2)

   Returns true if second character is not smaller then the first one."
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

   Checks if character is upper case."
  (typecheck "char-upper-case?" char "character")
  (and (char-alphabetic? char)
       (char=? (char-upcase char) char)))

;; -----------------------------------------------------------------------------
(define (char-lower-case? char)
  "(char-upper-case? char)

   Checks if character is lower case."
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
    (if (binary-port? port)
        (display obj port)
        (display (repr obj true) port))))

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

   Creates a new vector with n empty elements. If fill is specified it will set
   all elements of the vector to that value."
  (let ((result (new Array n)))
    (if (not (null? rest))
        (--> result (fill (car rest)))
        result)))

;; -----------------------------------------------------------------------------
(define (vector? n)
  "(vector? n)

   Returns true if value is vector and false if not."
  (string=? (type n) "array"))

;; -----------------------------------------------------------------------------
(define (vector-ref vec n)
  "(vector-ref vec n)

   Returns nth element of the vector vec."
  (typecheck "vector-ref" vec "array" 1)
  (typecheck "vector-ref" n "number" 2)
  (. vec n))

;; -----------------------------------------------------------------------------
(define (vector-set! vec n value)
  "(vector-set! vec n value)

   Function that sets nth item of the vector to value."
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

   Returns length of the vector. It errors if the argument is not a vector."
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
   item match the value it will return corresponding result expression value.
   If no value match and there is else it will return that result.")

;; -----------------------------------------------------------------------------
(--> lips.Formatter.defaults.exceptions.specials (push "case")) ;; 2 indent

;; -----------------------------------------------------------------------------
(define (numerator n)
  "(numerator n)

   Return numerator of rational or same number if n is not rational."
  (typecheck "numerator" n "number")
  (cond ((integer? n) n)
        ((rational? n) n.__num__)
        (else
         (numerator (inexact->exact n)))))

;; -----------------------------------------------------------------------------
(define (denominator n)
  "(denominator n)

   Return denominator of rational or same number if one is not rational."
  (typecheck "denominator" n "number")
  (cond ((integer? n) n)
        ((rational? n) n.__denom__)
        ((exact? n) 1)
        (else
         (denominator (inexact->exact n)))))

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
  ;; TODO: replace %number-type with typechecking
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

       Function that generates new random real number using Knuth algorithm."
      (if (pair? new-seed)
          (set! seed (car new-seed))
          (set! seed (modulo (+ (* seed a) c) m)))
      (exact->inexact (/ seed m)))))

;; -----------------------------------------------------------------------------
(define (eof-object? obj)
  "(eof-object? arg)

   Checks if value is eof object, returned from input string
   port when there are no more data to read."
  (eq? obj eof))

;; -----------------------------------------------------------------------------
(define (output-port? obj)
  "(output-port? arg)

   Returns true if argument is output port."
  (instanceof lips.OutputPort obj))

;; -----------------------------------------------------------------------------
(define (input-port? obj)
  "(input-port? arg)

   Returns true if argument is input port."
  (instanceof lips.InputPort obj))

;; -----------------------------------------------------------------------------
(define (char-ready? . rest)
  "(char-ready?)
   (char-ready? port)

   Checks if characters is ready in input port. This is useful mostly
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

       Returns new Input Port with given filename. In Browser user need to
       provide global fs variable that is instance of FS interface."
      (new lips.InputFilePort (%read-file false filename) filename))))

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
(define (with-input-from-port port thunk)
  "(with-input-from-port port thunk)

   Procedure use port and make it current-input-port then thunk is executed.
   After thunk is executed current-input-port is restored and given port
   is closed."
  (let* ((env **interaction-environment**)
         (internal-env (env.get '**internal-env**))
         (old-stdin (internal-env.get "stdin")))
    (internal-env.set "stdin" port)
    (try
     (thunk)
     (finally
      (internal-env.set "stdin" old-stdin)
      (close-input-port port)))))

;; -----------------------------------------------------------------------------
(define (with-input-from-file string thunk)
  "(with-input-from-file string thunk)

   Procedure open file and make it current-input-port then thunk is executed.
   After thunk is executed current-input-port is restored and file port
   is closed."
  (with-input-from-port (open-input-file string) thunk))

;; -----------------------------------------------------------------------------
(define (with-input-from-string string thunk)
  "(with-input-from-string string thunk)

   Procedure open string and make it current-input-port then thunk is executed.
   After thunk is executed current-input-port is restored and string port
   is closed."
  (with-input-from-port (open-input-string string) thunk))

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

       Function that opens file and return port that can be used for writing. If file
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

   Returns new Environment object for given Scheme Spec version.
   Only argument 5 is supported that create environment for R5RS."
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
    ((7) (%make-env "R7RS" - * / _ + < <= = => > >= abs acos and angle append apply asin assoc assq
                    assv atan begin binary-port? boolean? boolean=? bytevector bytevector?  bytevector-append
                    bytevector-copy bytevector-copy!  bytevector-length bytevector-u8-ref bytevector-u8-set!  caaaar
                    caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr call/cc
                    call-with-current-continuation call-with-input-file call-with-output-file call-with-port
                    call-with-values car case case-lambda cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
                    cddar cdddar cddddr cdddr cddr cdr ceiling char? char<? char<=? char=? char>? char>=?
                    char->integer char-alphabetic? char-ci<? char-ci<=? char-ci=? char-ci>? char-ci>=?
                    char-downcase char-foldcase char-lower-case? char-numeric? char-ready? char-upcase
                    char-upper-case? char-whitespace? close-input-port close-output-port close-port command-line
                    complex? cond cond-expand cons cos current-error-port current-input-port current-jiffy
                    current-output-port current-second define define-record-type define-syntax define-values delay
                    delay-force delete-file denominator digit-value display do dynamic-wind else emergency-exit
                    environment eof-object eof-object? eq? equal? eqv? error error-object? error-object-irritants
                    error-object-message eval even? exact exact? exact-integer? exact-integer-sqrt exit exp expt
                    features file-exists? finite? floor floor/ floor-quotient floor-remainder flush-output-port force
                    for-each gcd get-environment-variable get-environment-variables get-output-bytevector
                    get-output-string guard if imag-part import include include-ci inexact inexact? infinite?
                    input-port? input-port-open? integer? integer->char interaction-environment
                    interaction-environment jiffies-per-second lambda lcm length let let* let*-values letrec letrec*
                    letrec-syntax let-syntax let-values list list? list->string list->vector list-copy list-ref
                    list-set! list-tail load log magnitude make-bytevector make-list make-parameter make-polar
                    make-promise make-rectangular make-string make-vector map max member memq memv min modulo nan?
                    negative? newline not null? number? number->string numerator odd? open-binary-input-file
                    open-binary-output-file open-input-bytevector open-input-file open-input-string
                    open-output-bytevector open-output-file open-output-string or output-port? output-port-open? pair?
                    parameterize peek-char peek-u8 port? positive? procedure? quasiquote quote quotient raise
                    raise-continuable rational? rationalize read read-bytevector read-bytevector! read-char read-line
                    read-string read-u8 real? real-part remainder reverse round scheme-report-environment set!
                    set-car! set-cdr! sin sqrt square string string? string<? string<=? string=? string>?
                    string>=? string->list string->number string->symbol string->utf8 string->vector string-append
                    string-ci<? string-ci<=? string-ci=? string-ci>? string-ci>=? string-copy string-copy!
                    string-downcase string-fill! string-foldcase string-for-each string-length string-map string-ref
                    string-set! string-upcase substring symbol? symbol=? symbol->string syntax-error syntax-rules tan
                    textual-port? truncate truncate/ truncate-quotient truncate-remainder u8-ready? unless unquote
                    unquote-splicing utf8->string values vector vector? vector->list vector->string vector-append
                    vector-copy vector-copy! vector-fill! vector-for-each vector-length vector-map vector-ref
                    vector-set! when with-exception-handler with-input-from-file with-output-to-file write
                    write-bytevector write-char write-shared write-simple write-string write-u8 zero?))
    (else (throw (new Error (string-append "scheme-report-environment: version "
                                           (number->string version)
                                           " not supported"))))))
;; Implementation of byte vector functions - SRFI-4 and SRFI-160
;;
;; original code was based on https://small.r7rs.org/wiki/NumericVectorsCowan/17/
;; latest version is defined in
;; https://srfi.schemers.org/srfi-160/srfi-160.html
;;
;; it uses JavaScript typed arrays
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Typed_arrays
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2024 Jakub T. Jankiewicz <https://jcubic.pl/me>
;; Released under MIT license
;;

(define-macro (%make-vector prefix type help)
  "(%make-vector prefix type help)

   Mega-helper macro that creates a list of functions for single byte vectors
   based on typed arrays from JavaScript."
  (letrec ((prefix-str (symbol->string prefix))
           (type-str (symbol->string type))
           (l-type (--> type-str (toLowerCase)))
           (static (lambda (name)
                     (string->symbol (format "~a.~a" type-str name))))
           (TypedArray.from (static "from"))
           (fn-name (lambda (str)
                      (string->symbol (format str prefix-str))))
           (type-vector (fn-name "~avector"))
           (make-vector (fn-name "make-~avector"))
           (vector? (fn-name "~avector?"))
           (vector-in-range? (fn-name "%~avector-in-range?"))
           (vector-ref (fn-name "~avector-ref"))
           (repr-str (format "#~a" prefix-str))
           (vector-length (fn-name "~avector-length"))
           (vector->list (fn-name "~avector->list"))
           (vector-set! (fn-name "~avector-set!"))
           (list->tvector (fn-name "list->~avector"))
           (vector->tvector (fn-name "vector->~avector")))
    `(begin
       ;; -----------------------------------------------------------------------------
       (define (,type-vector . args)
         ,(format "(~a v1 v2 ...)

                   Create ~a from give arguments."
                  type-vector help)
         (,TypedArray.from (list->vector args)))
       ;; -----------------------------------------------------------------------------
       (define (,vector-length v)
         ,(format "(~a v)

                   return length of ~a."
                  vector-length help)
         (typecheck ,(symbol->string vector-length) v ,l-type)
         v.length)
       ;; -----------------------------------------------------------------------------
       (define (,make-vector k . fill)
         ,(format "(~a k fill)

                   Allocate new ~a of length k, with optional initial values."
                  make-vector
                  help)
         (let ((v (new ,type k)))
           (if (not (null? fill))
               (--> v (fill (car fill))))
           v))
       ;; -----------------------------------------------------------------------------
       (define (,vector? x)
         ,(format "(~a x)

                   Returns #t of argument is ~a otherwise it return #f."
                  vector?
                  help)
         (and (object? x) (equal? (. x 'constructor) ,type)))
       ;; -----------------------------------------------------------------------------
       (define (,vector-in-range? vector k)
         ,(format "(~a vector k)

                   Function that tests if index is range for ~a."
                  vector-in-range?
                  help)
         (typecheck ,(symbol->string vector-in-range?) vector ,l-type)
         (typecheck ,(symbol->string vector-in-range?) k "number")
         (let ((len (length vector)))
           (and (>= k 0) (< k len))))
       ;; -----------------------------------------------------------------------------
       (define (,vector-ref vector k)
         ,(format "(~a vector k)

                  Returns value from vector at index k. If index is out of range it throw exception."
                  vector-ref
                  help)
         (typecheck ,(symbol->string vector-ref) vector ,l-type)
         (typecheck ,(symbol->string vector-ref) k "number")
         (if (not (,vector-in-range? vector k))
             (throw (new Error ,(format "~a index out of range" vector-ref)))
             (. vector k)))
       ;; -----------------------------------------------------------------------------
       (define (,vector->list vector)
         (typecheck ,(symbol->string vector->list) vector ,l-type)
         (vector->list (Array.from vector)))
       ;; -----------------------------------------------------------------------------
       (define (,vector-set! vector k v)
         ,(format "(~a vector k)

                   Function set value of ~a at index k. If index is out of range it throw exception."
                  vector-set!
                  help)
         (typecheck ,(symbol->string vector-set!) vector ,l-type)
         (typecheck ,(symbol->string vector-set!) k "number")
         (if (not (,vector-in-range? vector k))
             (throw (new Error ,(format "~a index out of range" vector-set!)))
             (set-obj! vector k v)))
       ;; -----------------------------------------------------------------------------
       (define (,list->tvector lst)
         (typecheck ,(symbol->string list->tvector) lst "pair")
         (apply ,vector lst))
       ;; -----------------------------------------------------------------------------
       (define (,vector->tvector vector)
         (typecheck ,(symbol->string vector->tvector) vector "array")
         (,TypedArray.from vector))
       ;; -----------------------------------------------------------------------------
       (set-special! ,repr-str ',type-vector lips.specials.SPLICE)
       ;; -----------------------------------------------------------------------------
       (set-repr! ,type
                  (lambda (x _quote)
                    (string-append ,repr-str (repr (,vector->list x) _quote))))
       )))
;; -----------------------------------------------------------------------------
(%make-vector u8 Uint8Array "unsigned 8-bit integer vector (C unsigned char)")
(%make-vector s8 Int8Array "signed 8-bit integer vector (C signed char)")
(%make-vector u16 Uint16Array "unsigned 16-bit integer vector (C unsigned short)")
(%make-vector s16 Int16Array "signed 16-bit integer vector (C short)")
(%make-vector u32 Uint32Array "unsigned 32-bit integer vector (C int)")
(%make-vector s32 Int32Array "signed 32-bit integer vector (C unsigned int)")
(%make-vector f32 Float32Array "32-bit IEEE-754 floating point number vector (C float)")
(%make-vector f64 Float64Array "64-bit IEEE-754 floating point number vector (C double)")

;;vector->[type]vector!
;;list->[type]vector!
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
  (syntax-rules (multi single)
    ((_ ()) nil)
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
;; -----------------------------------------------------------------------------
;; init internal fs for LIPS Scheme Input/Output functions
;; -----------------------------------------------------------------------------
(let* ((fs (cond ((eq? self global) (require "fs"))
                 ((and (not (null? self.BrowserFS)) (indexed-db?))
                  (new Promise (lambda (resolve reject)
                                 (BrowserFS.configure
                                  &(:fs "IndexedDB"
                                        :options &())
                                  (lambda (e)
                                    (if (null? e)
                                        (resolve (BrowserFS.BFSRequire "fs"))
                                        (reject e)))))))
                 ((not (indexed-db?))
                  (console.warn (string-append "No FS found and IndexedDB "
                                               "is not available"))
                  nil))))
  (let ((internal (lips.env.get '**internal-env**)))
    (if (not (null? fs))
        (lips.set_fs fs))))
