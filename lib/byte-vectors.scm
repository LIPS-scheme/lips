;; Implementation of byte vector functions - SRFI-4
;;
;; original code was based on https://small.r7rs.org/wiki/NumericVectorsCowan/17/
;; it use JavaScript typed arrays
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Typed_arrays
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2021 Jakub T. Jankiewicz <https://jcubic.pl/me>
;; Released under MIT license
;;

(define-macro (%make-vector prefix type help)
  "(%make-vector prefix type help)

   Mega helper macro that create list of functions for single byte vector
   based on typed array from JavaScript"
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

                   Function return #t of argument is ~a otherwise it return #f."
                  vector?
                  help)
         (and (object? x) (equal? (. x 'constructor) ,type)))
       ;; -----------------------------------------------------------------------------
       (define (,vector-in-range? vector k)
         ,(format "(~a vector k)

                   Function test if index is range for ~a."
                  vector-in-range?
                  help)
         (typecheck ,(symbol->string vector-in-range?) vector ,l-type)
         (typecheck ,(symbol->string vector-in-range?) k "number")
         (let ((len (length vector)))
           (and (>= k 0) (< k len))))
       ;; -----------------------------------------------------------------------------
       (define (,vector-ref vector k)
         ,(format "(~a vector k)

                  Function return value frome vector at index k. If index is out of range it throw exception."
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
(%make-vector u8 Uint8Array "usigned 8-bit integer vector")
(%make-vector s8 Int8Array "signed 8-bit integer vector")
(%make-vector u16 Uint16Array "usigned 16-bit integer vector")
(%make-vector s16 Int16Array "signed 16-bit integer vector")
(%make-vector u32 Uint16Array "usigned 32-bit integer vector")
(%make-vector s32 Int16Array "signed 32-bit integer vector")
(%make-vector f32 Float32Array "32-bit IEEE floating point number vector")
(%make-vector f64 Float64Array "64-bit IEEE floating point number vector")

;;vector->[type]vector!
;;list->[type]vector!
