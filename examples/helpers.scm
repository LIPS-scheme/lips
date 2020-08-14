;; Helper functions and macros to be used with LIPS
;;
;; This file is part of the LIPS - Scheme based Powerful LISP in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;

;; ---------------------------------------------------------------------------------------
(define (log . args)
  "(log . args)

   Wrapper over console.log"
  (apply (.. console.log) args))

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
(define (assocv key alist)
  "(assocv key alist)

   Return value of alist if found or undefined if not."
  (let ((pair (assoc key alist)))
    (if (pair? pair) (cdr pair) false)))

