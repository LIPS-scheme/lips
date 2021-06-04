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
;; Copyright (C) 2019-2021 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license

;; -----------------------------------------------------------------------------
(define (%doc string fn)
  (typecheck "%doc" fn "function")
  (typecheck "%doc" string "string")
  (set-obj! fn '__doc__ (--> string (replace #/^ +/mg "")))
  fn)

;; -----------------------------------------------------------------------------
(define-macro (let-syntax vars . body)
  "(let-syntax ((name fn)) body)

    Macro works like combination of let and define-syntax. It creaates
    local macros and evaluate body in context of those macros.
    The macro to letrec-syntax is like letrec is to let."
  `(let ,vars
     ,@(map (lambda (rule)
              `(typecheck "let-syntax" ,(car rule) "syntax"))
            vars)
     ,@body))

;; -----------------------------------------------------------------------------
(define-macro (letrec-syntax vars . body)
  "(letrec-syntax ((name fn)) body)

    Macro works like combination of letrec and define-syntax. It creaates
    local macros and evaluate body in context of those macros."
  `(letrec ,vars
     ,@(map (lambda (rule)
              `(typecheck "letrec-syntax" ,(car rule) "syntax"))
            vars)
     ,@body))

;; -----------------------------------------------------------------------------
(define-macro (define-syntax name expr . rest)
  "(define-syntax name expression [__doc__])

   Macro define new hygienic macro using syntax-rules with optional documentation"
  (let ((expr-name (gensym "expr-name")))
    `(define ,name
       (let ((,expr-name ,expr))
         (typecheck "define-syntax" ,expr-name "syntax")
         ,expr-name)
       ,@rest)))

;; -----------------------------------------------------------------------------
(define (quoted-symbol? x)
   "(quoted-symbol? code)

   Helper function that test if value is quoted symbol. To be used in macros
   that pass literal code that is transformed by parser.

   usage:

      (define-macro (test x)
         (if (quoted-symbol? x)
             `',(cadr x)))

      (list 'hello (test 'world))"
   (and (pair? x) (eq? (car x) 'quote) (symbol? (cadr x)) (null? (cddr x))))

;; -----------------------------------------------------------------------------
(define-macro (--> expr . code)
  "Helper macro that simplify calling methods on objects. It work with chaining

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
                (let ((value (gensym "value")))
                  `(let* ((,value ,(let ((name (cond ((quoted-symbol? code) (symbol->string (cadr code)))
                                                     ((pair? code) (symbol->string (car code)))
                                                     (true code))))
                                       (if (string? name)
                                           `(. ,obj ,@(split "." name))
                                           `(. ,obj ,name)))))
                     ,(if (and (pair? code) (not (quoted-symbol? code)))
                         `(set! ,obj (,value ,@(cdr code)))
                         `(set! ,obj ,value)))))
              code)
       ,obj)))

;; -----------------------------------------------------------------------------
(define-macro (define-global first . rest)
  "(define-global var value)
   (define-global (name . args) body)

   Macro that define functions or variables in global context, so they can be used
   inside let and get let variables in closure, Useful for universal macros."
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
                        (if (and (not (match /^_/ key)) (function? (. obj key)))
                            (let* ((args (gensym "args")))
                              `(define-global (,(make-name key) . ,args)
                                 (apply (. ,name ,key) ,args)))))
                        (array->list (--> Object (keys obj))))))))

;; -----------------------------------------------------------------------------
(define (single list)
  "(single list)

   Function check if argument is list with single element"
  (and (pair? list) (not (cdr list))))

;; -----------------------------------------------------------------------------
(define (iterator? x)
   "(iterator? x)

     Function check if value is JavaScript iterator object"
   (and (object? x) (procedure? (. x Symbol.iterator))))

;; -----------------------------------------------------------------------------
(define-macro (.. expr)
  "(.. foo.bar.baz)

   Macro that gets value from nested object where argument is comma separated symbol"
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

   Parser extension that create new quoted named gensym."
  `(quote ,(gensym symbol)))

;; -----------------------------------------------------------------------------
(define (plain-object? x)
  "(plain-object? x)

   Function check if value is plain JavaScript object. Created using object macro."
  ;; here we don't use string=? or equal? because it may not be defined
  (and (== (--> (type x) (cmp "object")) 0) (eq? (. x 'constructor) Object)))

;; -----------------------------------------------------------------------------
(define typed-array?
  (let ((TypedArray (Object.getPrototypeOf Uint8Array)))
    (lambda (o)
      "(typed-array? o)

      Function test if argumnet is JavaScript typed array (Scheme byte vector)."
      (instanceof TypedArray o))))

;; -----------------------------------------------------------------------------
(define (symbol->string s)
  "(symbol->string symbol)

   Function convert LIPS symbol to string."
  (if (symbol? s)
      (let ((name s.__name__))
        (if (string? name)
            name
            (--> name (toString))))))

;; -----------------------------------------------------------------------------
(define (string->symbol string)
  "(string->symbol string)

   Function convert string to LIPS symbol."
  (and (string? string) (%as.data (new (. lips "LSymbol") string))))

;; -----------------------------------------------------------------------------
(define (alist->object alist)
  "(alist->object alist)

   Function convert alist pairs to JavaScript object."
  (if (pair? alist)
      (alist.to_object)
      (alist->object (new lips.Pair undefined nil))))

;; -----------------------------------------------------------------------------
(define (parent.frames)
  "(parent.frames)

   Funcion return list of environments from parent frames (lambda function calls)"
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

   Function call fn argument for pairs in a list and return combined list with
   values returned from function fn. It work like the map but take two items from list"
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
  "(object-expander reaonly '(:foo (:bar 10) (:baz (1 2 3))))

   Recursive function helper for defining LIPS code for create objects
   using key like syntax."
  (let ((name (gensym "name")) (quot (if (null? rest) false (car rest))))
    (if (null? expr)
        `(alist->object ())
        `(let ((,name (alist->object '())))
           ,@(pair-map (lambda (key value)
                         (if (not (key? key))
                             (let ((msg (string-append (type key)
                                                       " "
                                                       (repr key)
                                                       " is not a symbol!")))
                               (throw msg))
                             (let ((prop (key->string key)))
                               (if (and (pair? value) (key? (car value)))
                                   `(set-obj! ,name
                                              ,prop
                                              ,(object-expander readonly value))
                                    (if quot
                                        `(set-obj! ,name ,prop ',value)
                                        `(set-obj! ,name ,prop ,value))))))
                       expr)
           ,(if readonly
               `(Object.freeze ,name))
           ,name))))

;; -----------------------------------------------------------------------------
(define-macro (object . expr)
  "(object :name value)

   Macro that create JavaScript object using key like syntax."
  (try
    (object-expander false expr)
    (catch (e)
      (error e.message))))

;; -----------------------------------------------------------------------------
(define-macro (object-literal . expr)
  "(object-literal :name value)

   Macro that create JavaScript object using key like syntax. This is similar,
   to object but all values are quoted. This macro is used with & object literal."
  (try
    (object-expander true expr true)
    (catch (e)
      (error e.message))))

;; -----------------------------------------------------------------------------
(define (alist->assign desc . sources)
  "(alist->assign alist . list-of-alists)

   Function that work like Object.assign but for LIPS alist."
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

   Function check if symbol is key symbol, have colon as first character."
  ;; we can't use string=? because it's in R5RS.scm we use same code that use cmp
  (and (symbol? symbol) (== (--> (substring (symbol->string symbol) 0 1) (cmp ":")) 0)))

;; -----------------------------------------------------------------------------
(define (key->string symbol)
  "(key->string symbol)

   If symbol is key it convert that to string - remove colon."
  (if (key? symbol)
      (substring (symbol->string symbol) 1)))

;; -----------------------------------------------------------------------------
(define (%as.data obj)
  "(%as.data obj)

   Mark object as data to stop evaluation."
  (if (object? obj)
      (begin
        (set-obj! obj 'data true)
        obj)))

;; -----------------------------------------------------------------------------
(define (%hidden-props obj)
  "(%hidden-props obj)

   Function return hidden names of an object, for ES6 class prototype
   it return all methods since they are indistinguishable from hidden property
   created using defineProperty."
  (let* ((descriptors (Object.getOwnPropertyDescriptors obj))
         (names (Object.keys descriptors)))
    (--> names (filter (lambda (name)
                          (let ((descriptor (. descriptors name)))
                            (eq? descriptor.enumerable false)))))))

;; -----------------------------------------------------------------------------
(define (dir obj . rest)
  "(dir obj)

   Function return all props on the object including those in prototype chain."
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

   Tree version of map. Function is invoked on every leaf."
  (if (pair? tree)
      (cons (tree-map f (car tree)) (tree-map f (cdr tree)))
      (f tree)))

;; -----------------------------------------------------------------------------
(define (native.number x)
  "(native.number obj)

   If argument is number it will convert to native number."
  (if (number? x)
      (value x)
      x))

;; -----------------------------------------------------------------------------
(define (value obj)
  "(value obj)

   Function unwrap LNumbers and convert nil value to undefined."
  (if (eq? obj nil)
      undefined
      (if (number? obj)
          ((. obj "valueOf"))
          obj)))

;; -----------------------------------------------------------------------------
(define-macro (define-formatter-rule . patterns)
  "(rule-pattern pattern)

   Anaphoric Macro for defining patterns for formatter. With Ahead, Pattern and * defined values."
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
(define-syntax cond
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
(define-syntax cond/maybe-more
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
   Macro for condition check. For usage instead of nested ifs."
  (if (pair? list)
      (let* ((item (car list))
             (first (car item))
             (forms (cdr item))
             (rest (cdr list)))
        `(if ,first
             (begin
               ,@forms)
             ,(if (and (pair? rest)
                       (or (eq? (caar rest) true)
                           (eq? (caar rest) 'else)))
                  `(begin
                     ,@(cdar rest))
                  (if (not (null? rest))
                      `(cond ,@rest)))))
      nil))

;; -----------------------------------------------------------------------------
(define (%r re . rest)
  "(%r re)

   Create new regular expression from string, to not break Emacs formatting"
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

   Function return interaction environement equal to lips.env can be overwritten,
   when creating new interpreter with lips.Interpreter."
  **interaction-environment**)

;; -----------------------------------------------------------------------------
(define (current-output-port)
  "(current-output-port)

   Function return default stdout port."
  (let-env (interaction-environment)
           (--> **internal-env** (get 'stdout))))

;; -----------------------------------------------------------------------------
(define (current-error-port)
  "(current-output-port)

   Function return default stdout port."
  (let-env (interaction-environment)
     (--> **internal-env** (get 'stderr))))

;; -----------------------------------------------------------------------------
(define (current-input-port)
  "current-input-port)

   Function return default stdin port."
  (let-env (interaction-environment)
     (--> **internal-env** (get 'stdin))))

;; -----------------------------------------------------------------------------
(define (regex? x)
  "(regex? x)

   Function return true of value is regular expression, it return false otherwise."
  (== (--> (type x) (cmp "regex")) 0))

;; -----------------------------------------------------------------------------
(define (set-repr! type fn)
  "(add-repr! type fn)

   Function add string represention to the type, which should be constructor function.

   Function fn should have args (obj q) and it should return string, obj is vlaue that
   need to be converted to string, if the object is nested and you need to use `repr`,
   it should pass second parameter q to repr, so string will be quoted when it's true.

   e.g.: (lambda (obj q) (string-append \"<\" (repr obj q) \">\"))"
  (typecheck "add-repr!" type "function")
  (typecheck "add-repr!" fn "function")
  (ignore (--> lips.repr (set type fn))))

;; -----------------------------------------------------------------------------
(define (unset-repr! type)
  "(unset-repr! type)

   Function remove string represention to the type, which should be constructor function,
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
                                 (concat ":" key
                                         " "
                                         (repr (. x key) q))))
                          (join " "))
                     ")")))

;; -----------------------------------------------------------------------------
(define (bound? x . rest)
  "(bound? x [env])

   Function check if variable is defined in given environement or interaction environment
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

   Function check if symbol is bound variable similar to bound?."
  (typecheck "environment-bound?" env "environment" 1)
  (typecheck "environment-bound?" x "symbol" 2)
  (bound? x env))

;; -----------------------------------------------------------------------------
;; source https://stackoverflow.com/a/4297432/387194
;; -----------------------------------------------------------------------------
(define (qsort e predicate)
  "(qsort list predicate)

   Sort the list using quick sort alorithm according to predicate."
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

   Sort the list using optional predicate function. if not function is specified
   it will use <= and sort in increasing order."
  (let ((predicate (if (null? rest) <= (car rest))))
    (typecheck "sort" list "pair")
    (typecheck "sort" predicate "function")
    (qsort list predicate)))

;; -----------------------------------------------------------------------------
(define (every fn list)
  "(every fn list)

   Function call function fn on each item of the list, if every value is true
   it will return true otherwise it return false."
  (if (null? list)
      true
      (and (fn (car list)) (every fn (cdr list)))))

;; -----------------------------------------------------------------------------
(define-macro (promise . body)
  "(promise . body)

   Anaphoric macro that expose resolve and reject functions from JS promise"
  `(new Promise (lambda (resolve reject)
                  (try (begin ,@body)
                       (catch (e)
                              (error e.message))))))

;; -----------------------------------------------------------------------------
(define-macro (timer time . body)
  "(timer time . body)

   Macro evaluate expression after delay, it return timer. To clear the timer you can use
   native JS clearTimeout function."
  `(setTimeout (lambda () (try (begin ,@body) (catch (e) (error (.. e.message))))) ,time))

;; -----------------------------------------------------------------------------
(define-macro (wait time . expr)
  "(wait time . expr)

   Function return promise that will resolve with evaluating the expression after delay."
  `(promise (timer ,time (resolve (begin ,@expr)))))

;; -----------------------------------------------------------------------------
(define (await value)
  "(await value)

   Function unquote quoted promise so it can be automagicaly evaluated (resolved
   to its value)."
  (if (instanceof lips.QuotedPromise value)
      (value.valueOf)
      value))

;; -----------------------------------------------------------------------------
(define-macro (quote-promise expr)
  "(quote-promise expr)
   '>expr

  Macro used to escape promise the whole expression, will be wrapped
  with JavaScript class that behave like Promise but will not
  auto resolve like normal promise."
  `(let ((env))
      (set! env (current-environment))
      (env.set (Symbol.for "__promise__") true)
      ,expr))

;; -----------------------------------------------------------------------------
(define (defmacro? obj)
  "(defmacro? expression)

   Function check if object is macro and it's expandable."
  (and (macro? obj) (. obj 'defmacro)))

;; -----------------------------------------------------------------------------
(define (n-ary n fn)
  "(n-ary n fn)

   Return new function that limit number of arguments to n."
  (lambda args
    (apply fn (take n args))))

;; -----------------------------------------------------------------------------
(define (take n lst)
  "(take n list)

   Return n first values of the list."
  (let iter ((result '()) (i n) (lst lst))
    (if (or (null? lst) (<= i 0))
        (reverse result)
        (iter (cons (car lst) result) (- i 1) (cdr lst)))))

;; -----------------------------------------------------------------------------
(define unary (%doc "(unary fn)

                     Function return new function with arguments limited to one."
                    (curry n-ary 1)))

;; -----------------------------------------------------------------------------
(define binary (%doc "(binary fn)

                      Function return new function with arguments limited to two."
                      (curry n-ary 2)))

;; -----------------------------------------------------------------------------
;; LIPS Object System
;; -----------------------------------------------------------------------------

(define (%class-lambda expr)
  "(class-lambda expr)

   Return lambda expression where input expression lambda have `this` as first argument."
  (let ((args (gensym 'args)))
    `(lambda ,args
       (apply ,(cadr expr) this ,args))))

;; -----------------------------------------------------------------------------
(define (%class-method-name expr)
  "(%class-method-name expr)

   Helper function that allow to use [Symbol.asyncIterator] inside method name."
  (if (pair? expr)
      (car expr)
      (list 'quote expr)))

;; -----------------------------------------------------------------------------
(define-macro (define-class name parent . body)
  "(define-class name parent . body)

   Define class - JavaScript function constructor with prototype.

   usage:

     (define-class Person Object
         (constructor (lambda (self name)
                        (set-obj! self '_name name)))
         (hi (lambda (self)
               (display (string-append self._name \" say hi\"))
               (newline))))
     (define jack (new Person \"Jack\"))
     (jack.hi)"
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

   Macro allow to create anonymous classes. See define-class for details.")

;; -----------------------------------------------------------------------------
(define (make-tags expr)
  "(make-tags expression)

   Function that return list structure of code with better syntax then raw LIPS"
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

   Helper function that render expression using create element function."
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
                    (map (lambda (expr)
                           (%sxml h expr))
                         rest)
                    (list first)))))))

;; -----------------------------------------------------------------------------
(define-macro (pragma->sxml pragma)
  `(define-macro (sxml expr)
     "(sxml expr)

     Macro for JSX like syntax but with SXML.
     e.g. usage:

     (sxml (div (@ (data-foo \"hello\")
                   (id \"foo\"))
                (span \"hello\")
                (span \"world\")))"
     (%sxml ',pragma expr)))

;; -----------------------------------------------------------------------------
(pragma->sxml h)

;; -----------------------------------------------------------------------------
(define-macro (with-tags expr)
  "(with-tags expression)

   Macro that evalute LIPS shorter code for S-Expression equivalent of JSX.
   e.g.:

   (with-tags (:div (:class \"item\" :id \"item-1\")
                    (list (:span () \"Random Item\")
                          (:a (:onclick (lambda (e) (alert \"close\")))
                              \"close\"))))

   Above expression can be passed to function that renders JSX (like render in React, Preact)
   To get the string from the macro you can use vhtml library from npm."
  (make-tags expr))

;; -----------------------------------------------------------------------------
(define (get-script url)
  "(get-script url)

   Load JavaScript file in browser by adding script tag to head of the current document."
  (if (not (bound? 'document))
      (throw (new Error "get-script: document not defined"))
      (let ((script (document.createElement "script")))
        (new Promise (lambda (resolve reject)
                        (set-obj! script 'src url)
                        (set-obj! script 'onload (lambda ()
                                                   (resolve)))
                        (set-obj! script 'onerror (lambda ()
                                                    (reject "get-script: Failed to load")))
                        (if document.head
                            (document.head.appendChild script)))))))

;; -----------------------------------------------------------------------------
(define (gensym? value)
  "(gensym? value)

   Function return #t if value is symbol and it's gensym. It returns #f otherwise."
  (and (symbol? value) (--> value (is_gensym))))

;; -----------------------------------------------------------------------------
(define (degree->radians x)
  "(degree->radians x)

   Convert degree to radians."
  (* x (/ Math.PI 180)))

;; -----------------------------------------------------------------------------
(define (radians->degree x)
  "(radians->degree x)

   Convert radians to degree."
  (* x (/ 180 Math.PI)))

;; -----------------------------------------------------------------------------
(define-syntax while
  (syntax-rules ()
    ((_ predicate body ...)
     (do ()
       ((not predicate))
       body ...)))
  "(while cond . body)

   Macro that create a loop, it exectue body until cond expression is false.")

;; -----------------------------------------------------------------------------
(define-syntax ++
  (syntax-rules ()
    ((++ x)
     (let ((tmp (+ x 1)))
       (set! x tmp)
       tmp)))
  "(++ variable)

   Macro that work only on variables and increment the value by one.")

;; -----------------------------------------------------------------------------
(define-syntax --
  (syntax-rules ()
    ((-- x)
     (let ((tmp (- x 1)))
       (set! x tmp)
       tmp)))
  "(-- variable)

   Macro that decrement the value it work only on symbols")

;; -----------------------------------------------------------------------------
(define (pretty-format . lists)
  "(pretty-format pair)

   Function return pretty printed string from pair expression."
  (let ((code (--> (list->vector lists)
                   (map (lambda (pair i)
                          (typecheck "pretty-pair" pair "pair" i)
                          (repr pair true)))
                   (join ""))))
    (--> (new lips.Formatter code) (break) (format))))

;; -----------------------------------------------------------------------------
(define (reset)
  "(reset)

  Function reset environment and remove all user defined variables."
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
(define (range n)
  "(range n)

   Function return list of n numbers from 0 to n - 1"
  (typecheck "range" n "number")
  (array->list (--> (new Array n) (fill 0) (map (lambda (_ i) i)))))

;; -----------------------------------------------------------------------------
(define-macro (do-iterator spec cond . body)
  "(do-iterator (var expr) (test) body ...)

   Macro iterate over iterators (e.g. create with JavaScript generator function)
   it works with normal and async iterators. You can loop over infinite iterators
   and break the loop if you want, using expression like in do macro, long sync iterators
   will block main thread (you can't print 1000 numbers from inf iterators,
   because it will freeze the browser), but if you use async iterators you can process
   the values as they are generated."
  (let ((gen (gensym "name"))
        (name (car spec))
        (async (gensym "async"))
        (sync (gensym "sync"))
        (iterator (gensym "iterator"))
        (test (if (null? cond) #f (car cond)))
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
                      (set! ,name (. ,item "value")))))))))

;; -----------------------------------------------------------------------------
(set-repr! Set (lambda () "#<Set>"))
(set-repr! Map (lambda () "#<Met>"))

;; -----------------------------------------------------------------------------
(define (native-symbol? x)
  "(native-symbol? object)

   Function check if value is JavaScript symbol."
  (and (string=? (type x) "symbol") (not (symbol? x))))

;; -----------------------------------------------------------------------------
(set-special! "’" 'warn-quote)

;; -----------------------------------------------------------------------------
(define-macro (warn-quote)
  "(warn-quote)

   Simple macro that throw error, when you try to use ’ symbol as quote in code"
  (throw (new Error (string-append "You're using invalid quote character run: "
                                   "(set-special! \"’\" 'quote)"
                                   " to allow running this type of quote"))))

;; -----------------------------------------------------------------------------
(define-macro (let-env-values env spec . body)
  "(let-env-values env ((name var)) . body)

   Macro add mapping for variables var from specified env,
   Macro work similar to let-env but lexical scope is working with it."
  (let ((env-name (gensym 'env)))
    `(let ((,env-name ,env))
       (let ,(map (lambda (pair)
                    `(,(car pair) (--> ,env-name (get ',(cadr pair)))))
                  spec)
         ,@body))))

;; -----------------------------------------------------------------------------
(define (apropos name)
  "(apropos name)

   Search environment and display names that match the given name.
   name can be regex, string or symbol."
  (typecheck "apropos" name '("string" "regex" "symbol"))
  (let ((regex (lambda (string)
                 (new RegExp (escape-regex string)))))
    (filter (cond ((string? name) (regex name))
                  ((symbol? name) (regex (symbol->string name)))
                  (else name))
            (env))))

;; -----------------------------------------------------------------------------
(define (promisify fn)
  "(promisify fn)

   Simple function for adding promises to NodeJS callback based function.
   Function tested only with fs module."
  (lambda args
    (new Promise (lambda (resolve reject)
                   (apply fn (append args (list (lambda (err data)
                                                  (if (null? err)
                                                      (resolve data)
                                                      (reject err))))))))))

;; -----------------------------------------------------------------------------
(define-macro (list* . args)
  "(list* arg1 ...)

   Parallel version of list."
  (let ((result (gensym "result")))
     `(let ((,result (vector)))
        ,@(map (lambda (arg)
                 `(--> ,result (push '>,arg)))
               args)
        (map await (vector->list ,result)))))

;; -----------------------------------------------------------------------------
(define-macro (%not-implemented name)
  "(not-implemented name)

   Returns new function taht throw exception that function is not implmeneted"
  (let ((str-name (symbol->string name)))
    `(lambda ()
       ,(string-append "(" str-name ")\n\nThis function is not yet implemented.")
       (throw (new Error ,(string-append str-name " has not beed implemented"))))))

;; -----------------------------------------------------------------------------
(define-macro (%make-env name . names)
  "(%make-env name f1 f2 ...)

   Create new Environment with given name and defined symbols in it from global env.
   If given function name f1 f2 ... don't exists, it will define function that
   throw exception that function is not yet implemented."
  `(new lips.Environment (alist->object (list ,@(map (lambda (name)
                                                       `(cons ',name ,(let ((ref (lips.env.ref name)))
                                                                       (if (null? ref)
                                                                           `(%not-implemented ,name)
                                                                           `(lips.env.get ',name)))))
                                                     names)))
        null
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

   Function test if indexedDB is available."
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
           (window.indexedDB.open "MyTestDatabase" 3)
           true)
         (catch (e)
                false))
        false)))

;; -----------------------------------------------------------------------------
(define (environment? obj)
  "(environment? obj)

   Function check if object is LIPS environment."
  (instanceof lips.Environment obj))

;; -----------------------------------------------------------------------------
(define %read-file
  (let ((read-file #f) (fetch-url #f))
    (lambda (binary path)
      "(%read-file binary path)

       Read file from url or file system. If binary is false it will return
       string that contain all the content. For HTTP requests, If binary
       is false it will: when in browser return ArrayBuffer and in Node
       it will return Buffer object. When reading from file system
       in both cases it will return Buffer objects.

       The code that use those function, in binary mode, need to check
       if the result is ArrayBuffer or Node.js/BrowserFS Buffer object."
      (if (not read-file)
          (let ((fs (--> lips.env (get '**internal-env**) (get 'fs))))
            (if (null? fs)
                (throw (new Error "open-input-file: fs not defined"))
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
      (cond ((char=? (string-ref path 0) #\/)
             (if (not (file-exists? path))
                 (throw (new Error (string-append "file "
                                                  path
                                                  " don't exists")))
                 (read-file path binary)))
            ((--> #/^https?:\/\// (test path))
             (fetch-url path binary))
            (else
             (%read-file binary (string-append (current-directory) path)))))))

;; -----------------------------------------------------------------------------
(define %read-binary-file (curry %read-file true))
(define %read-text-file (curry %read-file false))

;; -----------------------------------------------------------------------------
(define (%fs-promisify-proc fn message)
  "(%fs-promisify-proc fn string)

   Function return promisified version of fs function or throw exception
   if fs is not available."
  (let ((fs (--> lips.env (get '**internal-env**) (get 'fs))))
    (if (null? fs)
        (throw (new Error (string-append message ": fs not defined")))
        (promisify (. fs fn)))))

;; -----------------------------------------------------------------------------
(define (response->content binary res)
  "(response->text binary res)

   Function read all text from Node.js HTTP response object. If binary argument
   is true it will return Buffer object that can be converted to u8vector.

   ***Warrning:*** it may overflow the stack (part of Node) when converting
   whole buffer to u8vector."
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

         Node.js Function that send HTTP Request and return string or
         binary Buffer object."
        (throw (new Error "http-get: function is Node.js only.")))
      (let* ((http (. (require "http") 'get))
             (https (. (require "https") 'get)))
        (lambda (url binary)
          "(http-get url)

           Node.js Function that send HTTP Request and return string or
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

   Cross platform function that can be used in both Node and Browser.
   It can be used together with %read-file or %read-binary-file and convert
   the result ArrayBuffer or Buffer to u8vector."
  (if (instanceof ArrayBuffer bin)
      (new Uint8Array bin)
      (Uint8Array.from bin)))

;; -----------------------------------------------------------------------------
(define (complement fn)
  "(complement fn)

   Higer order function that returns complement of the given function. If the function fn
   for a given arguments return true the result function will return false, if it would
   return false, the result function will return true."
  (typecheck "complement" fn "function")
  (lambda args
    (not (apply fn args))))

;; -----------------------------------------------------------------------------
(define (always constant)
  "(always constant)

   Higher order function returns new function that always return given constant."
  (lambda ()
    constant))

;; -----------------------------------------------------------------------------
(define (once fn)
  "(once fn)

   Higher order function that return new function, that is guarantee
   to be called only once."
  (typecheck "once" fn "function")
  (let ((result))
    (lambda args
      (if (string=? (type result) "undefined")
          (set! result (apply fn args)))
      result)))

;; -----------------------------------------------------------------------------
(define (flip fn)
  "(flip fn)

   Higher order function that return new function where first two arguments are swapped.

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

   Function returns list from given function and init value. The function should
   return cons where first is the item added to the list and second is next value
   passed to the funtion. If function return false it end the loop."
  (typecheck "unfold" fn "function")
  (let iter ((pair (fn init)) (result '()))
    (if (not pair)
        (reverse result)
        (iter (fn (cdr pair)) (cons (car pair) result)))))

;; -----------------------------------------------------------------------------

