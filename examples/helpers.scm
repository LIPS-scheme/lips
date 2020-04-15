;; Helper functions and macros to be used with LIPS
;;
;; This file is part of the LIPS - Scheme implementation in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;

;; ---------------------------------------------------------------------------------------
(define (log . args)
  "(log . args)

   Wrapper over console.log"
  (apply (.. console.log) args))

;; ---------------------------------------------------------------------------------------
(define-macro (promise . body)
  "(promise . body)

   Anaphoric macro that expose resolve and reject functions from JS promise"
  `(new Promise (lambda (resolve reject)
                  (try (begin ,@body)
                       (catch (e)
                              (error (.. e.message)))))))


;; ---------------------------------------------------------------------------------------
(define-macro (timer time . body)
  "(timer time . body)

   Macro evaluate expression after delay, it return timer. To clear the timer you can use
   native JS clearTimeout function."
  `(setTimeout (lambda () (try (begin ,@body) (catch (e) (error (.. e.message))))) ,time))

;; ---------------------------------------------------------------------------------------
(define-macro (module name . body)
  "(module module-name . body)

   Macro for defining modules inside you can use define to create functions.
   And use export name to add that name to defined environment."
  (let ((parent (. (current-environment) 'parent))
        (module-name (gensym)))
    `(let ((,module-name (new (. lips 'Environment) (concat "module-" ,(symbol->string name)))))
       (define-macro (export name) `(--> ,,module-name (set ',name ,name)))
       ,@body
       (--> ,parent (set ',name ,module-name)))))

;; ---------------------------------------------------------------------------------------
(define-macro (import name module)
  "(input function-name module)

   Macro for importing function from module"
  `(define ,name (--> ,module (get ',name))))

;; ---------------------------------------------------------------------------------------
(define (defmacro? obj)
  "(defmacro? expression)

   Function check if object is macro and it's expandable"
  (and (macro? obj) (. obj 'defmacro)))

;; ---------------------------------------------------------------------------------------
(define (assocv key alist)
  "(assocv key alist)

   Return value of alist if found or undefined of not."
  (let ((pair (assoc key alist)))
    (if (pair? pair) (cdr pair) false)))

(define (plain-object? x)
  "(plain-object? x)

   Function check if value is plain JavaScript object. Created using make-object macro."
  (and (eq? (type x) "object") (eq? (. x 'constructor) Object)))

;; ---------------------------------------------------------------------------------------
;; LIPS Object System
;; ---------------------------------------------------------------------------------------

(define (class-lambda expr)
  "(class-lambda expr)

   Return lambda expression where input expression lambda have `this` as first argument."
  (let ((args (cdadadr expr)))
    `(lambda (,@args)
       (,(cadr expr) this ,@args))))

;; ---------------------------------------------------------------------------------------
(define-macro (define-class name parent . body)
  "(define-class name parent . body)

   Define class - JavaScript function constructor with prototype."
  (let iter ((functions '()) (constructor nil) (lst body))
    (if (not (null? list))
        (if (not (eq? (caar lst) 'constructor))
            (iter (cons (car lst) functions) nil (cdr lst))
            (let* ((functions (append functions (cdr lst)))
                   (constructor (car lst)))
              `(begin
                 (define ,name ,(class-lambda constructor))
                 (--> Object (defineProperty ,name "name" (make-object :value
                                                                       ,(symbol->string name))))
                 ,(if (not (null? parent))
                      `(set-obj! ,name 'prototype (--> Object (create ,parent))))
                 ,@(map (lambda (fn)
                          `(set-obj! (. ,name 'prototype) ',(car fn) ,(class-lambda fn)))
                        functions)))))))

;; ---------------------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------------------
(define-macro (with-tags expr)
  "(with-tags expression)

   Macro that evalute LIPS shorter code for S-Expression equivalent of JSX"
  (make-tags expr))
