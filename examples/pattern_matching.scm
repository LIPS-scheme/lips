;; Example pattern matching macro
;;
;; This file is part of the LIPS - Scheme implementation in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license


;; ---------------------------------------------------------------------------------------
(define (compare a b)
  "(compare a b)

   Function that compare two values. it compare lists and any element of the list
   can be a function that will be called with other value. e.g.:
   (compare (list (list 'a) 'b) (list pair? 'b))"
  (cond ((and (pair? a) (pair? b)) (and (compare (car a) (car b))
                                        (compare (cdr a) (cdr b))))
        ((and (array? a) (array? b)) (compare (array->list a) (array->list b)))
        ((and (object? a) (object? b))
         (compare (array->list (--> Object (keys a)))
                  (array->list (--> Object (keys b)))))
        ((eq? (type a) (type b)) (eq? a b))
        ((and (function? a) (not (function? b)))
         (a b))
        ((and (not (function? a)) (function? b))
         (b a))
        (true false)))

;; ---------------------------------------------------------------------------------------
(define-macro (auto-quote arg)
  "(auto-quote list)

   Macro that create list recursively but take symbols from scope"
  (if (pair? arg)
      `(list ,@(map (lambda (item)
                      (if (symbol? item)
                          item
                          (if (pair? item)
                              `(auto-quote ,item)
                              `,item)))
                    arg))
      arg))

;; ---------------------------------------------------------------------------------------
(define-macro (match-pattern expr . list)
  "(match-patternpattern ((pattern . body) ...))

   Pattern matching macro. examples:
   (match-pattern (1 (pair? pair?) 2) ((1 (() ()) 2) (display \"match\")))
   ;; match
   (match-pattern (1 (pair? pair?) 2) ((1 (()) 2) (display \"match\")))
   (match-pattern (1 (pair? pair?) 2) ((1 (()) 2) (display \"match\")) (true \"rest\"))
   ;; rest"
 (if (pair? list)
     (let ((ex-name (gensym)))
       `(let ((,ex-name (auto-quote ,expr)))
          (cond ,@(map (lambda (item)
                         (if (eq? (car item) true)
                             `(true ,(cadr item))
                             `((compare ,ex-name (auto-quote ,(car item))) ,(cadr item))))
                       list))))))
