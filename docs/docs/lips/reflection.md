---
sidebar_position: 2
description: A way to introspect and manipulate LIPS internals
---

# Reflection

You can use standard JavaScript methods to inspect LIPS objects.

* Scheme functions
  * `dir` - this procedure is inspired by Python function with the same name, it returns all properties
     that you can access on an object. Including those from object prototype and whole chain.
  * `env` - function return everything what is inside current-environment.

* JavaScript functions
  * `Object.keys` - this JavaScript method will return all string keys from an object.
  * `Object.values` - return the value of the object.
  * `Object.entries` - this return array of `[key,value]` pairs.
  * `Object.getOwnPropertySymbols` - similar to `Object.keys` but return all symbols

## Numbers

You can access internal representation of numbers as JavaScript objects

```scheme
(let ((num 1/2+10i))
  (print num.__im__)
  (print num.__re__)
  (print num.__re__.__denom__)
  (print num.__re__.__num__))
;; ==> 10
;; ==> 1/2
;; ==> 2
;; ==> 1
```

## Lists and Pairs

You can access Pairs as JavaScript objects:

```scheme
(let ((x '(1 2 3)))
  x.cdr.cdr.car)
;; ==> 3
```

You can also manipulate the list with `set!`:

```scheme
(let ((x '(1 2 3)))
  (set! x.cdr.cdr.cdr x)
  x)
;; ==> #0=(1 2 3 . #0#)
```

Above create a cycle. When you try to display a cycle it's printed using R7RS datum syntax.

## Strings

Same as with numbers and list you can access internals of Strings.

```scheme
(let ((str "hello"))
  (str.__string__.toUpperCase))
;; ==> "HELLO"
```

`__string__` property is read only so you can't modify its value:

```scheme
(let ((str "hello"))
  (set! str.__string__ "world")
  str)
;; ==> Cannot assign to read only property '__string__' of object '[object Object]'
```

## Characters

Similar to string you can access internals of Characters.

```scheme
(let ((x #\X)) (dir x))
;; ==> (__char__ constructor toUpperCase toLowerCase toString serialize valueOf)
```

the `__char__` property is a string that hold the value of the character.

```scheme
(let ((x #\X))
  (write x)
  (newline)
  (write x.__char__)
  (newline))
;; ==> #\X
;; ==> "X"
```

## Procedures
As described in [Core features](/docs/lips/intro#procedures), procedures are JavaScript functions,
they also hold additional properties like `__code__` and `__doc__`. The first property is the live
source code of the procedure that you can modify:

```scheme
(define (repeater x)
   "(repeater value)

    Function prints the value 1 time and modifies itself to repeat
    (+ n 1) times on the next call."
   (for-each (lambda () (print x)) (range 1))
   (let ((r (cadr (cdadddr (. repeater '__code__)))))
     (set-cdr! r (list (+ (cadr r) 1)))))
```

:::info

The `__code__` property always contain a lambda expression, because:

```scheme
(define (foo x) ...)
```

is an alias for:

```scheme
(define foo (lambda (x) ...))
```

:::

This procedure modify its source code. Each time you execute this function it will run one more
times.

```scheme
(print "1")
(repeater 'hello)
;; ==> 1
;; ==> hello
(print "2")
(repeater 'hello)
;; ==> 2
;; ==> hello
;; ==> hello
(print "3")
(repeater 'hello)
;; ==> 3
;; ==> hello
;; ==> hello
;; ==> hello
```

The first expression of the procedure is a doc string, unless a string is the only expression,
in that case it's a return value. To access doc string you can use `help` or `__doc__`.

```scheme
repeater.__doc__
"(repeater value)

Function prints the value 1 time and modifies itself to repeat
(+ n 1) times on the next call."
```
