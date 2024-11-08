---
sidebar_position: 7
description: Powerful feature of Scheme that allow to add new control flows
---

# Continuations

## What is continuation?

In Scheme and Lisp a continuation is a thing that is waiting for an expression to be evaluated.

If you have code like this:

```scheme
(+ 1 2 <slot>)
```

and `<slot>` is an expression: (e.g.: `(/ 1 10)`)

```scheme
(+ 1 2 (/ 1 10))
```

then continuation for expression `(/ 1 10)` is `(+ 1 2 <slot>)`. Continuations is the next
expression that needs to be evaluated.

Scheme is unique because it allows to access continuations. They are first class objects like
numbers or functions.

## Accessing current continuation

To access the current continuation for expression, you need to use `call-with-current-continuation`
or its abbreviation `call/cc`. The procedure `call/cc` accept a single procedure that get the
continuation as first argument:

```scheme
(call/cc (lambda (c)
           ...))
```

The continuation saved in `c` capture whole state of the Scheme interpreter. The continuation act as
a procedure that you can pass a single value to it and Scheme will jump in to the place where
continuation was captured with a given value.

## Calling continuations

You can save continuation inside a variable and call it later like a procedure.

```scheme
(define k #f)

(+ 1 (call/cc
       (lambda (continuation)
         (set! k continuation)
         2)))
;; ==> 3
(k 10)
;; ==> 11
```

Here when you call a continuation `k` with value 10 it restores the state in `(+ 1 <slot>)` and
execute that expression again with a value `10` (expression `(+ 1 10)`).

The continuation act like a procedure and return `#t` with `procedure?` predicate:

```scheme
(define k (call/cc (lambda (c) c)))
(procedure? k)
;; ==> #t
```

## Early exit

The simple thing you can do with continuations is an early exit. Scheme doesn't have a return
expression, but with continuations you can add one.

```scheme
(define (find item lst)
  (call/cc (lambda (return)
             (let loop ((lst lst))
                (if (null? lst)
                    (return #f)
                    (if (equal? item (car lst))
                        (return lst)
                        (loop (cdr lst))))))))
```

You can even create abstraction with anaphoric macro:

```scheme
(define-macro (alambda args . body)
  `(lambda ,args
     (call/cc (lambda (return)
                ,@body))))
```

and you can use this macro like normal `lambda`, but you have anaphoric `return` expression:

```scheme
(define exists? (alambda (item lst)
                         (for-each (lambda (x)
                                     (if (equal? x item)
                                         (return #t)))
                                   lst)
                         #f))

(exists? 'x '(a b c d e f))
;; ==> #f
(exists? 'd '(a b c d e f))
;; ==> #t
```

Here for-each always iterates over all elements, but with early exit, it will return immediately when
found a value.

## Loops

You can create loops with continuations:

```scheme
(define (make-range from to)
  (call/cc
   (lambda (return)
     (let ((result '()))
       (let ((loop (call/cc (lambda (k) k))))
         (if (<= from to)
             (set! result (cons from result))
             (return (reverse result)))
         (set! from (+ from 1))
         (loop loop))))))

(make-range 1 10)
;; ==> (1 2 3 4 5 6 7 8 9 10)
```

The first continuation creates an early exit, like in the previous example. But the second `call/cc` use
identity function (it return continuation). Which means that the continuation is saved in a loop
variable. And each time it's called with `loop` as an argument, it's again assigned that
continuation to loop variable. This is required for the next loop.

## Generators

Some languages have generators and a `yield` keyword. In Scheme, you can create generators with
continuations.

```scheme
(define (make-coroutine-generator proc)
  (define return #f)
  (define resume #f)
  (define yield (lambda (v)
                  (call/cc (lambda (r)
                             (set! resume r)
                             (return v)))))
  (lambda ()
    (call/cc (lambda (cc)
               (set! return cc)
               (if resume
                   (resume (if #f #f))  ; void? or yield again?
                   (begin (proc yield)
                          (set! resume (lambda (v)
                                         (return (eof-object))))
                          (return (eof-object))))))))
```

The above example came from
[SRFI 158 example implementation](https://github.com/scheme-requests-for-implementation/srfi-158/blob/master/srfi-158-impl.scm#L77-L87).

The procedure `make-coroutine-generator` allows defining generators:

```scheme
(define counter (make-coroutine-generator
                 (lambda (yield)
                   (do ((i 0 (+ i 1)))
                     ((<= 3 i))
                     (yield i)))))

(counter) ;; ==> 0
(counter) ;; ==> 1
(counter) ;; ==> 2
(counter) ;; ==> #<eof>
```

With continuations, you can do a lot of cool new flow control structures.
