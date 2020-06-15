;; -*- scheme -*-
;; This file contain essential functions and macros for LIPS
;;
;; This file is part of the LIPS - Scheme based Powerful LISP in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license

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
  (let ((expr-name (gensym)))
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
          (--> (fetch \"https://jcubic.pl\") (text) (match /<title>([^<]+)<\\/title>/) 1)
          (--> document (querySelectorAll \".cmd-prompt\") 0 \"innerText\")"
  (let ((obj (gensym)))
    `(let* ((,obj ,expr))
       ,@(map (lambda (code)
                (let ((name (gensym))
                      (value (gensym)))
                  `(let* ((,name ,(cond ((quoted-symbol? code) (symbol->string (cadr code)))
                                        ((pair? code) (symbol->string (car code)))
                                        (true code)))
                          (,value (. ,obj ,name)))
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
        `(--> (. lips 'env)
              (set ,(symbol->string name) (lambda ,(cdr first) ,@rest))))
      `(--> (. lips 'env) (set ,(symbol->string first) ,(car rest)))))

;; -----------------------------------------------------------------------------
(define-macro (globalize expr . rest)
  "(globalize expr)

    Macro will get the value of the expression and add each method as function to global
    scope."
  (let* ((env (current-environment))
         (obj (eval expr env))
         (name (gensym))
         (env-name (gensym))
         (make-name (if (pair? rest)
                        (let ((pre (symbol->string (car rest))))
                          (lambda (name) (string->symbol (concat pre name))))
                        string->symbol)))
    `(let ((,name ,expr))
       ,@(filter pair?
                 (map (lambda (key)
                        (if (and (not (match /^_/ key)) (function? (. obj key)))
                            (let* ((args (gensym)))
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

;; ---------------------------------------------------------------------------------------
(define (plain-object? x)
  "(plain-object? x)

   Function check if value is plain JavaScript object. Created using make-object macro."
  (and (eq? (type x) "object") (eq? (. x 'constructor) Object)))

;; -----------------------------------------------------------------------------
(define (symbol->string s)
  "(symbol->string symbol)

   Function convert LIPS symbol to string."
  (if (symbol? s)
      (let ((name (. s "name")))
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
      (alist.toObject)
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
            (let ((parent.frame (--> frame (get 'parent.frame (make-object :throwError false)))))
              (if (function? parent.frame)
                  (iter (cons frame result) (parent.frame 0))
                  result))))))

;; -----------------------------------------------------------------------------
(define-macro (wait time . expr)
  "(wait time . expr)

   Function return promise that will resolve with evaluating the expression after delay."
  `(promise (timer ,time (resolve (begin ,@expr)))))

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
(define (object-expander expr)
  "(object-expander '(:foo (:bar 10) (:baz (1 2 3))))

   Recursive function helper for defining LIPS code for create objects using key like syntax."
  (let ((name (gensym)))
    (if (null? expr)
        `(alist->object ())
        `(let ((,name (alist->object '())))
           ,@(pair-map (lambda (key value)
                         (if (not (key? key))
                             (let ((msg (string-append (type key) " " (repr key) " is not a symbol!")))
                               (throw msg))
                             (let ((prop (key->string key)))
                               (if (and (pair? value) (key? (car value)))
                                   `(set-obj! ,name ,prop ,(object-expander value))
                                   `(set-obj! ,name ,prop ,value)))))
                       expr)
           ,name))))

;; -----------------------------------------------------------------------------
(define-macro (make-object . expr)
  "(make-object :name value)

   Macro that create JavaScript object using key like syntax."
  (try
    (object-expander expr)
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
(define (dir obj)
  "(dir obj)

   Function return all props on the object including those in prototype chain."
  (if (null? obj) nil
      (append (array->list (Object.getOwnPropertyNames obj))
              (dir (Object.getPrototypeOf obj)))))


;; ---------------------------------------------------------------------------------------
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
  (let ((rules (gensym)))
    `(let ((,rules (.. lips.Formatter.rules))
           (Ahead (lambda (pattern)
                    (let ((Ahead (.. lips.Formatter.Ahead)))
                      (new Ahead (if (string? pattern) (new RegExp pattern) pattern)))))
           (* ((.. Symbol.for) "*"))
           (Pattern (lambda (pattern flag)
                      (new (.. lips.Formatter.Pattern) (list->array pattern)
                           (if (null? flag) undefined flag)))))
       ,@(map (lambda (pattern)
                `(--> ,rules (push (tree->array (tree-map native.number ,@pattern)))))
              patterns))))


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
;; formatter rules for cond to break after each S-Expression
;; regex literal /[^)]/ breaks scheme emacs mode so we use string and macro use RegExp constructor
(define-formatter-rule ((list (list "(" "cond" (Pattern (list "(" * ")") "+")) 1 (Ahead "[^)]"))))

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
     stdout))

;; -----------------------------------------------------------------------------
(define (current-input-port)
  "current-input-port)

   Function return default stdin port."
  (let-env (interaction-environment)
     stdin))

(define (regex? x)
  "(regex? x)

   Function return true of value is regular expression, it return false otherwise."
  (== (--> (type x) (cmp "regex")) 0))

;; -----------------------------------------------------------------------------
;; add syntax &(:foo 10) that's transformed into (make-object :foo 10)
;; -----------------------------------------------------------------------------
(add-special! "&" 'make-object lips.specials.SPLICE)
;; -----------------------------------------------------------------------------
(define (add-repr! type fn)
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
(define (remove-repr! type)
  "(remove-repr! type)

   Function remove string represention to the type, which should be constructor function,
   added by add-repr! function."
  (typecheck "add-repr!" type "function")
  (ignore (--> lips.repr (delete type))))

;; -----------------------------------------------------------------------------
;; Object representation
(add-repr! Object
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
(define (zip . args)
  "(zip list1 list2 ...)

   Create one list by taking each element if each list."
  (if (null? args)
      nil
      (if (some null? args)
         nil
         (cons (map car args) (apply zip (map cdr args))))))
