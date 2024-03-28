---
sidebar_position: 6
description: Feature of scheme that allow to create lazy lists
---

# Streams

Streams (also called delayed Lists or lazy lists) are a data structure described in
[SICP](https://web.mit.edu/6.001/6.037/sicp.pdf) Chapter 3.5. It also appears in Chapter 21 of
[Functional Programming in Scheme](https://people.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html).

The base of those data structures are two expressions `delay` and `force`.

The result of a delay is often called a Promise.

To create a lazy [pair](/docs/scheme-intro/data-types#pairs), you use cons with the first element
(`car`) and the rest (`cdr`) is a delay expression:

```scheme
(define s (cons 1 (delay 2)))
```

If you print this expression, you will get something like this (it depends on Scheme implementation)

```scheme
(write (cons 1 (delay 2)))
;; ==> (1 . #<promise - not forced>)
```

The `cdr` is a promise that needs to be forced to get evaluated.

```scheme
(let ((x (cons 1 (delay 2))))
  (write (force (cdr x)))
  (newline)
  (write x)
  (newline))
;; ==> 2
;; ==> (1 . #<promise - forced with number>)
```

With this you can create infinite lists because the delay can point into the same object.

Let's define some helper procedures:

```scheme
(define-macro (stream-cons x y)
  `(cons ,x (delay ,y)))
```

This is lazy version of cons that utilize [lisp macro](/docs/scheme-intro/macros).

You can also define `car` and `cdr` versions that work with streams:

```scheme
(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))
```

stream-car is the same as car so you can use an alias:

```scheme
(define stream-car car)
```

We also need an empty stream and predicate that check if stream is empty:

```scheme
(define (empty-stream? x) (eq? x the-empty-stream))
(define the-empty-stream '())
```

To create an infinite stream of ones, you can use code like this:

```scheme
(define ones (stream-cons 1 ones))
(write (stream-car (stream-cdr (stream-cdr ones))))
;; ==> 1
```

Let's write a procedure that take a stream and return a list of first n elements.

```scheme
(define (stream-take n stream)
  (if (<= n 0)
      '()
      (cons (stream-car stream) (stream-take (- n 1) (stream-cdr stream)))))


(write (stream-take 10 ones))
;; ==> (1 1 1 1 1 1 1 1 1 1)
```

You can define procedures that operate on streams, like procedure that add two streams:

```scheme
(define (stream-add s1 s2)
  (let ((first-a (stream-car s1))
        (first-b (stream-car s2)))
    (stream-cons
     (+ first-a first-b)
     (add-streams (stream-cdr s1) (stream-cdr s2)))))
```

You can sue this function to create stream of integers:

```scheme
(define integers (stream-cons 1 (stream-add integers ones)))
```

To prove that it works, you can get first 10 elements with `stream-take`:

```scheme
(display (stream-take 10 integers))
;; ==> (1 2 3 4 5 6 7 8 9 10)
```

We can also define higher order procedures that operate on streams. They will not execute until you
use `force`. Example cam be `stream-map`:

```scheme
(define (stream-map proc . streams)
  (define (single-map proc stream)
    (if (empty-stream? stream)
        the-empty-stream
        (stream-cons (apply proc (stream-car stream))
                     (single-map proc (stream-cdr stream)))))
  (single-map proc (apply stream-zip streams)))
```

It requires helper procedure that combine two streams by taking each element from every stream and
creates a single list in output stream.

```scheme
(define (stream-zip . streams)
  (if (empty-stream? streams)
      the-empty-stream
      (stream-cons (apply list (map stream-car streams))
                   (apply stream-zip (map stream-cdr streams)))))
```

The function work like this:

```scheme
(stream-take 5 (stream-zip integers integers ones))
;; ==> ((1 1 1) (2 2 1) (3 3 1) (4 4 1) (5 5 1))
```

With map, you can simplify `stream-add` with `stream-map`:

```scheme
(define (streams-add s1 s2)
  (stream-map + s1 s2))
```

Another useful procedures that accept stream are stream-limit and force-stream:

```scheme
(define (stream-limit n stream)
  (let loop ((n n) (stream stream))
    (if (or (empty-stream? stream) (= n 0))
        the-empty-stream
        (stream-cons (stream-car stream)
                     (loop (- n 1)
                           (stream-cdr stream))))))

(define (stream-force stream)
  (let loop ((stream stream))
    (if (empty-stream? stream)
        '()
        (cons (stream-car stream)
              (loop (stream-cdr stream))))))
```

If you call `force-stream` on infinite stream it will create infinite loop, but note that the
procedure force-stream is not tail recursive. The recursive call to named `let` is not the last
expression. The last expression is `cons`.

You can try to create a tail recursive version of the procedure as an exercise.

If you combine both procedures, you can create the same effect as with `stream-take`:

```scheme
(stream-force (stream-limit 10 integers))
;; ==> (1 2 3 4 5 6 7 8 9 10)
```

So you can implement stream-take with above procedures:

```scheme
(define (stream-take n stream)
  (stream-force (stream-limit n stream)))

(stream-take 10 integers)
;; ==> (1 2 3 4 5 6 7 8 9 10)
```

Another useful procedure is `stream-reduce` in Scheme often called `fold`:

```scheme
(define (stream-reduce fun stream)
  (let loop ((result (stream-car stream))
             (stream (stream-cdr stream)))
    (if (empty-stream? stream)
        result
        (loop (fun result (stream-car stream))
              (stream-cdr stream)))))
```

You can implement factorial function using streams:

```scheme
(define (! n)
  (stream-reduce * (stream-limit n integers)))

(! 10)
;; ==> 3628800
```

and you can use this procedure to create a stream of factorials:

```scheme
(define factorials
  (stream-map ! integers))

(stream-take 10 factorials)
;; ==> (1 2 6 24 120 720 5040 40320 362880 3628800)
```

Another example of using streams is calculation of Fibonacci numbers:

```scheme
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs) fibs))))

(stream-take 10 fibs)
;; ==> (0 1 1 2 3 5 8 13 21 34)
```
