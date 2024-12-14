;; Example streams
;;
;; Reference:
;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyright (C) 2019-2024 Jakub T. Jankiewicz <https://jcubic.pl/me>
;; Released under MIT license
;;

(define-macro (stream-cons x y)
  `(cons ,x (delay ,y)))

;; -----------------------------------------------------------------------------
(define (stream-car stream)
  (car stream))

;; -----------------------------------------------------------------------------
(define (stream-cdr stream)
  (force (cdr stream)))

;; -----------------------------------------------------------------------------
(define head stream-car)
(define tail stream-cdr)

;; -----------------------------------------------------------------------------
(define (empty-stream? x) (eq? x the-empty-stream))

;; -----------------------------------------------------------------------------
(define the-empty-stream '())

;; -----------------------------------------------------------------------------
(define (stream-take n stream)
  "return n elements from the stream"
  (if (<= n 0)
      '()
      (cons (head stream) (stream-take (- n 1) (tail stream)))))

;; -----------------------------------------------------------------------------
(define (stream-section n stream)
  (cond ((= n 0) '())
        (else
         (cons
          (head stream)
          (stream-section
           (- n 1)
           (tail stream))))))

;; --------------------------------------------------------------------------
(define (stream-inject init fn stream)
  (let iter ((result init)
             (stream stream))
    (if (empty-stream? (stream-cdr stream))
        result
        (iter (fn result (stream-car stream))
              (stream-cdr stream)))))

;; -----------------------------------------------------------------------------
(define (stream-add s1 s2)
  (let ((h1 (head s1))
        (h2 (head s2)))
    (stream-cons
     (+ h1 h2)
     (stream-add (tail s1) (tail s2)))))

;; --------------------------------------------------------------------------
(define (stream-range n)
  (let loop ((i 0))
    (if (= i n)
        the-empty-stream
        (stream-cons i (loop (+ i 1))))))

;; --------------------------------------------------------------------------
(define (stream-reduce fun stream)
  (let iter ((result (stream-car stream))
             (stream (stream-cdr stream)))
    (if (empty-stream? stream)
        result
        (iter (fun result (stream-car stream))
              (stream-cdr stream)))))

;; -----------------------------------------------------------------------------
(define (stream-zip . streams)
  (if (empty-stream? streams)
      the-empty-stream
      (stream-cons (apply list (map stream-car streams))
                   (apply stream-zip (map stream-cdr streams)))))

;; --------------------------------------------------------------------------
(define (stream-map proc . streams)
  (define (single-map proc stream)
    (if (empty-stream? stream)
        the-empty-stream
        (stream-cons (apply proc (stream-car stream))
                     (single-map proc (stream-cdr stream)))))
  (single-map proc (apply stream-zip streams)))

;; --------------------------------------------------------------------------
(define (stream-for-each proc stream)
  (unless (empty-stream? stream)
    (proc (stream-car stream))
    (stream-for-each proc (stream-cdr stream))))

;; --------------------------------------------------------------------------
(define (stream-limit n stream)
  "return stream of n elements of stream <stream> ( -> stream = {0 .. n})"
  (let iter ((n n) (stream stream))
    (if (or (empty-stream? stream) (eq? n 0))
        the-empty-stream
        (stream-cons (stream-car stream)
                     (iter (- n 1)
                           (stream-cdr stream))))))

;; -----------------------------------------------------------------------------
(define (stream-skip n stream)
  (if (<= n 0)
      stream
      (stream-skip (- n 1) (stream-cdr stream))))

;; -----------------------------------------------------------------------------
(define (stream-slice a b stream)
  (let loop ((n (- b a)) (stream (stream-skip a stream)))
    (if (eq? n 0)
        the-empty-stream
        (stream-cons (stream-car stream)
                     (loop (- n 1) (stream-cdr stream))))))

;; -----------------------------------------------------------------------------
(define (stream-force stream)
  (let iter ((stream stream))
    (if (empty-stream? stream)
        '()
        (cons (stream-car stream)
              (iter (stream-cdr stream))))))

;; -----------------------------------------------------------------------------
;; example streams
;; -----------------------------------------------------------------------------
(define fibs
  (stream-cons 0
               (stream-cons 1
                            (stream-add (tail fibs) fibs))))

;; -----------------------------------------------------------------------------
(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

;; -----------------------------------------------------------------------------
(define ones (stream-cons 1 ones))

;; -----------------------------------------------------------------------------
(define integers (stream-cons 1 (stream-add integers ones)))

;; -----------------------------------------------------------------------------
(define (! n)
  (stream-reduce * (stream-limit n integers)))

;; -----------------------------------------------------------------------------
(define factorials
  (stream-map ! integers))

;; -----------------------------------------------------------------------------
(define (divisible? x y)
  (eq? (gcd x y) y))

;; -----------------------------------------------------------------------------
(define (sieve stream)
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not
              (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

;; -----------------------------------------------------------------------------
(define (stream-scale stream n)
  (stream-map (lambda (x) (* x n)) stream))

;;(stream-force (limit 10 (stream-map (lambda (a b) (+ a b)) integers (stream-cdr integers))))
