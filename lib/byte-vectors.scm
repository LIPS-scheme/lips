(define (u8vector . args)
  "(u8vector v1 v2 ...)

   Create usigned 8-bit integer vector from give arguments."
  (Uint8Array.from (list->vector args)))

;; -----------------------------------------------------------------------------
(define (make-u8vector k . fill)
  "(make-u8vector v1 v2 ...)

   Allocate new usigned 8-bit integer vector of length k, with optional initial values."
  (let ((v (new Uint8Array k)))
    (if (not (null? fill))
        (--> v (fill (car fill))))
    v))

;; -----------------------------------------------------------------------------
(define (u8vector? x)
  "(u8vector? x)

   Function return #t of argument is usigned 8-bit integer vector otherwise it return #f."
  (and (object? x) (equal? (. x 'constructor) Uint8Array)))

;; -----------------------------------------------------------------------------
(define (%u8vector-in-range vector k)
  "(%u8vector-in-range vector k)

   Function test if index is range of vector."
  (typecheck "%u8vector-in-range" vector "array")
  (typecheck "%u8vector-in-range" k "number")
  (let ((len (length vector)))
    (and (>= k 0) (< k len))))

;; -----------------------------------------------------------------------------
(define (u8vector-ref vector k)
  "(u8vector-ref vector k)

   Function return value frome vector at index k. If index is out of range it throw exception."
  (typecheck "u8vector-ref" vector "array")
  (typecheck "u8vector-ref" k "number")
  (if (not (%u8vector-in-range vector k))
      (throw (new Error "u8vector-ref index out of range"))
      (. vector k)))

;; -----------------------------------------------------------------------------
(define (u8vector-set! vector k v)
  "(u8vector-set! vector k)

   Function set value of vector at index k. If index is out of range it throw exception."
  (typecheck "u8vector-set!" vector "array")
  (typecheck "u8vector-set!" k "number")
  (if (not (%u8vector-in-range vector k))
      (throw (new Error "u8vector-set! index out of range"))
      (set-obj! vector k v)))

;; -----------------------------------------------------------------------------
(define (list->u8vector lst)
  (typecheck "list->u8vector" lst "pair")
  (apply u8vector lst))

;; -----------------------------------------------------------------------------
(define (vector->u8vector vector)
  (typecheck "vector->u8vector" vector "vector")
  (Uint8Array.from vector))

;; -----------------------------------------------------------------------------
(define (u8vector->list vector)
  (typecheck "u8vector->list" vector "uint8array")
  (vector->list (Array.from vector)))

;; -----------------------------------------------------------------------------
(add-special! "#u8" 'u8vector lips.specials.SPLICE)

;; -----------------------------------------------------------------------------
(add-repr! Uint8Array
           (lambda (x _quote)
             (string-append "#u8" (repr (u8vector->list x) _quote))))

;;vector->[type]vector!
;;list->[type]vector!
