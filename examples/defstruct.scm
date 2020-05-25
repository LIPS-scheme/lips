;; -*- scheme -*-
;; LIPS macro to create structures
;;
;; Copyright (C) 2019 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;
;; the macro defstruct generate bunch of helper functions
;; to work with single type of structures like in Common Lisp
;; original macro was created for Guile Scheme
;; at the begging there are functions that were missing in LIPS
;;
;; when you call macro amd if name is `point` and
;; arguments are `x` and `y` it will create those functions:
;;
;;   make-point
;;   point-x
;;   point-y
;;   set-point-x!
;;   set-point-y!
;;   point?
;;
;; example usage:
;;
;; (defstruct user first-name last-name age)
;; (let ((user (make-user "John" "Doe" 26)))
;;   (display (concat "name: " (user-first-name user) " " (user-last-name user)))
;;   (display (concat "age: " (user-age user)))
;;   (set-user-last-name! user "Smith")
;;   (display (concat "set!: " (set-user-age! user (+ (user-age user) 1))))
;;   (display (concat "happy birthday you're now " (user-age user) " old"))
;;   (display user))
;;

(define (defstruct:alist->object arg)
  "Function create JavaScript object from AList"
  (typecheck "defstruct:every" arg "pair")
  (--> arg (toObject)))

(define (defstruct:every fn list)
  "return true if every element return true for a function applied to every element"
  (typecheck "defstruct:every" fn "function")
  (typecheck "defstruct:every" list "pair")
  (== (length list) (length (filter fn list))))

(define (defstruct:error symbol message)
  "show error on terminal and console"
  (nop (let ((msg (concat (symbol->string symbol) ": " message)))
         ((. console "error") msg)
         (let (($ (. window "jQuery")))
           (if (not (or (null? $) (null? (. jQuery "terminal"))))
               (let ((term ((.. jQuery.terminal.active))))
                 (--> term (error msg))))))))

(define string-append concat)

;; updated original code

(define (defstruct:make-name name)
  "create struct constructor name."
  (typecheck "defstruct:make-name" name "symbol")
  (string->symbol (string-append "make-" (symbol->string name))))


(define (defstruct:make-getter name field)
  "create filed acess function name."
  (typecheck "defstruct:make-getter" name "symbol")
  (typecheck "defstruct:make-getter" field "symbol")
  (string->symbol (string-append (symbol->string name) "-"
                                 (symbol->string field))))

(define (defstruct:make-setter name field)
  "create field setter function name."
  (typecheck "defstruct:make-setter" name "symbol")
  (typecheck "defstruct:make-setter" field "symbol")
  (string->symbol (string-append "set-"
                                 (symbol->string name) "-"
                                 (symbol->string field) "!")))


(define (defstruct:make-predicate name)
  "create predicate function name."
  (typecheck "defstruct:make-predicate" name "symbol")
  (string->symbol (string-append (symbol->string name) "?")))


(define-macro (defstruct name . fields)
  "Macro implementing structures in guile based on assoc list."
  (let ((names (map gensym fields))
        (struct (gensym))
        (field-arg (gensym)))
    `(if (not (defstruct:every-unique ',fields))
         (error 'defstruct "Fields must be unique")
         (begin
           (define (,(defstruct:make-name name) ,@names)
             (map cons ',fields (list ,@names)))
           ,@(map (lambda (field)
                    `(define (,(defstruct:make-getter name field) ,struct)
                       (cdr (assoc ',field ,struct)))) fields)
           ,@(map (lambda (field)
                    `(define (,(defstruct:make-setter name field) ,struct ,field-arg)
                       (set-cdr! (assoc ',field ,struct) ,field-arg)
                       ,field-arg)) fields)
           (define (,(defstruct:make-predicate name) ,struct)
             (and (struct? ,struct)
                  (let ((result true))
                    (for-each (lambda (x y)
                                (if (not (eq? x y)) (set! result true)))
                              ',fields
                              (map car ,struct))
                    result)))))))



(define (defstruct:unique item list)
  "check if item ocour only once."
  (typecheck "last" lst "pair" 2)
  (== (length (filter (lambda (i) (eq? item i)) list)) 1))

(define (defstruct:every-unique list)
  "check if every element ocour only once."
  (typecheck "last" lst "pair")
  (defstruct:every (lambda (item) (defstruct:unique item list)) list))


(define (struct? struct)
  "check if argument is structure (not that structures are alist)."
  (and (pair? struct) (defstruct:every pair? struct)))


(define (defstruct:last lst)
  "return last element of the list."
  (typecheck "last" lst "pair")
  (if (null? lst)
      '()
      (if (null? (cdr lst))
          (car lst)
          (last (cdr lst)))))

(define (write-struct struct)
  "print structure."
  (if (struct? struct)
      (begin
        (display "#<")
        (let ((last (defstruct:last struct)))
          (for-each (lambda (field)
                      (let ((first (car field)))
                        (if (struct? first)
                            (write-struct first)
                            (display first)))
                      (display ":")
                      (let ((rest (cdr field)))
                        (if (struct? rest)
                            (write-struct rest)
                            (write rest)))
                      (if (not (eq? field last))
                          (display " ")))
                    struct)
        (display ">"))))

(define (print-struct struct)
  (write-struct struct)
  (newline))
