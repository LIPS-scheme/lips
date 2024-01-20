;; Example pattern matching macro
;;
;; This file is part of the LIPS - Scheme implementation in JavaScript
;; Copyright (C) 2019-2024 Jakub T. Jankiewicz <https://jcubic.pl/me>
;; Released under MIT license

(cond-expand
 (lips)
 (guile
  (define (object? x) #f)
  (define (type x)
          (cond ((string? x) "string")
                ((pair? x) "pair")
                ((null? x) "nil")
                ((number? x) "number")
                ((vector? x) "array")
                ((procedure? x) "function")
                ((char? x) "character")))))

;; ---------------------------------------------------------------------------------------
(define (compare a b)
  "(compare a b)

   Function that compares two values. it compares lists and any element of the list
   can be a function that will be called with another value. e.g.:
   (compare (list (list 'a) 'b) (list pair? 'b))"
  (cond ((and (pair? a) (pair? b))
         (and (compare (car a) (car b))
              (compare (cdr a) (cdr b))))
        ((and (vector? a) (vector? b))
         (compare (vector->list a) (vector->list b)))
        ((and (object? a) (object? b))
         (compare (vector->list (--> Object (keys a)))
                  (vector->list (--> Object (keys b)))))
        ((string=? (type a) (type b)) (eq? a b))
        ((and (procedure? a) (not (procedure? b)))
         (a b))
        ((and (not (procedure? a)) (procedure? b))
         (b a))
        (else #f)))

;; ---------------------------------------------------------------------------------------
(define-macro (auto-quote arg)
  "(auto-quote list)

   Macro that creates list recursively but takes symbols from scope"
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
  "(match-pattern ((pattern . body) ...))

   Pattern matching macro. examples:
   (match-pattern (1 (pair? pair?) 2) ((1 ((1) (1)) 2) (display \"match\")))
   ;; match
   (match-pattern (1 (pair? pair?) 2) ((1 ((1)) 2) (display \"match\")))
   (match-pattern (1 (pair? pair?) 2) ((1 ((1)) 2) (display \"match\")) (true \"rest\"))
   ;; rest"
 (if (pair? list)
     (let ((ex-name (gensym)))
       `(let ((,ex-name (auto-quote ,expr)))
          (cond ,@(map (lambda (item)
                         (if (eq? (car item) #t)
                             `(else ,(cadr item))
                             `((compare ,ex-name (auto-quote ,(car item))) ,@(cdr item))))
                       list))))))

;; ---------------------------------------------------------------------------------------
(match-pattern (1 (pair? pair?) 2) ((1 ((1) (1)) 2)
                                    (display "match")
                                    (newline)))
