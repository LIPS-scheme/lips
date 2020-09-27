;; Example streams
;;
;; Reference:
;; http://people.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html
;;
;; This file is part of the LIPS - Scheme based Powerful lisp in JavaScript
;; Copyriht (C) 2019-2020 Jakub T. Jankiewicz <https://jcubic.pl>
;; Released under MIT license
;;

(define-macro (cons-stream x y)
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
(define (take n stream)
  (if (<= n 0)
    '()
     (cons (head stream) (take (- n 1) (tail stream)))))

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
(define (add-streams s1 s2)
 (let ((h1 (head s1))
       (h2 (head s2)))
   (cons-stream 
     (+ h1 h2)
     (add-streams (tail s1) (tail s2)))))

;; --------------------------------------------------------------------------
(define (stream-range n)
  (let loop ((i 0))
    (if (= i n) 
	the-empty-stream
	(cons-stream i (loop (+ i 1))))))
;; --------------------------------------------------------------------------
(define (stream-reduce fun stream)
  (let iter ((result (stream-car stream))
	     (stream stream))
    (if (empty-stream? (stream-cdr stream))
	result
	(iter (fun result (stream-car stream))
	      (stream-cdr stream)))))
	      
;; -----------------------------------------------------------------------------
(define (zip-streams . streams)
  (if (empty-stream? streams)
      the-empty-stream
      (cons-stream (apply list (map stream-car streams))
		   (apply zip-streams (map stream-cdr streams)))))

;; --------------------------------------------------------------------------
(define (stream-map proc . streams)
  (define (single-map proc stream)
	   (if (empty-stream? stream)
	       the-empty-stream
	       (cons-stream (apply proc (stream-car stream))
			    (single-map proc (stream-cdr stream)))))
  (single-map proc (apply zip-streams streams)))

;; --------------------------------------------------------------------------
(define (stream-for-each proc stream)
  (unless (empty-stream? stream)
	  (proc (stream-car stream))
	  (stream-for-each proc (stream-cdr stream))))

;; --------------------------------------------------------------------------
(define (limit n stream)
  "return stream of n element of stream <stream> ( -> stream = {0 .. n})"
  (let iter ((n n) (stream stream))
    (if (or (empty-stream? stream) (eq? n 0))
	the-empty-stream
	(cons-stream (stream-car stream)
		     (iter (- n 1)
			   (stream-cdr stream))))))

;; -----------------------------------------------------------------------------
(define (slice-stream a b stream)
  (let loop ((n (- b a)) (stream (skip-stream a stream)))
    (if (eq? n 0)
	the-empty-stream
	(cons-stream (stream-car stream)
		     (loop (- n 1) (stream-cdr stream))))))

;; -----------------------------------------------------------------------------
(define (force-stream stream)
  (let iter ((stream stream))
    (if (empty-stream? stream)
       '()
	(cons (stream-car stream)
	      (iter (stream-cdr stream))))))
	      
;; -----------------------------------------------------------------------------
;; example streams
;; -----------------------------------------------------------------------------
(define fibs
  (cons-stream 0
    (cons-stream 1
      (add-streams (tail fibs) fibs))))

;; -----------------------------------------------------------------------------
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

;; -----------------------------------------------------------------------------
(define ones (cons-stream 1 ones))

;; -----------------------------------------------------------------------------
(define integers (cons-stream 1 (add-streams integers ones)))

;; -----------------------------------------------------------------------------
(define (! n)
  (stream-reduce * (limit n integers)))

;; -----------------------------------------------------------------------------
;;(;;define factorials
;;  (stream-map ! integers))

;; -----------------------------------------------------------------------------  
(define (divisible? x y)
  (eq? (gcd x y) y))

;; -----------------------------------------------------------------------------
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
	   (lambda (x)
	     (not
	      (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))

;; -----------------------------------------------------------------------------
(define (scale-stream stream n)
  (map-stream (lambda (x) (* x n)) stream))
  
;;(force-stream (limit 10 (stream-map (lambda (a b) (+ a b)) integers (stream-cdr integers))))


