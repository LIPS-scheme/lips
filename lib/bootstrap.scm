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
                       (name (cond ((quoted-symbol? code) (symbol->string (cadr code)))
                                   ((pair? code) (symbol->string (car code)))
                                   (true code)))
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
    (if (string? name)
        name
        (name.toString))))

;; -----------------------------------------------------------------------------
(define (string->symbol string)
  "(string->symbol string)

   Function that converts a string to a LIPS symbol."
  (typecheck "string->symbol" string "string")
  (%as.data (new lips.LSymbol string)))

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
                         (or (eq? (caar rest) true)
                             (eq? (caar rest) 'else)))
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
(define-syntax while
  (syntax-rules ()
    ((_ predicate body ...)
     (do ()
       ((not predicate))
       body ...)))
  "(while cond . body)

   Creates a loop, it executes cond and body until cond expression is false.")

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
  "(do-iterator (var expr) (test) body ...)

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
