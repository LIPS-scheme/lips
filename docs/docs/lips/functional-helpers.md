---
sidebar_position: 5
description: Code that allows to do more with less
---

# Functional and other utils

LIPS Scheme provide various of utility functions. Some of them are inspired by [Ramda.js
library](https://ramdajs.com/) and [Lodash](https://lodash.com/). Some of those functions are
defined in [SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html).

## Curry

[Curry](https://en.wikipedia.org/wiki/Currying) is a common function in functional programming that
return a new function with predefined arguments. The classic version, returns functions that accept
one argument and keep returning new function until all argumments are passed. In LIPS there is more
useful version of curry, that allow to pass more than one argument at the time. This is a common way
curry is implemented. This is working in LIPS because Scheme lambdas has length property that
indicate number of arguments.

```scheme
(define (sum a b c)
  (+ a b c))

(define add1 (curry sum 1))

(print (add1 2 4))
;; ==> #<procedure>
```

## Take and drop

Take and drop procedures operates on lists and return a new list with:
* `take` - only first n elements
* `drop` - first elements removed

```scheme
(define lst '(1 2 3 4 5))

(take lst 3)
;; ==> (1 2 3)
(drop lst 3)
;; ==> (4 5)
(equal? lst (append! (take lst 3) (drop lst 3)))
;; ==> #t
```

## range

This is common function from Python. LIPS Scheme version works exactly the same and return list of numbers:

```scheme
(range 10)
;; ==> (0 1 2 3 4 5 6 7 8 9)
(range 5 10)
;; ==> (5 6 7 8 9)
(range 0 10 2)
;; ==> (0 2 4 6 8)
(range 0 -10 -2)
;; ==> (0 -2 -4 -6 -8 -10)
```

If used with one argument it returns `n` numbers starting from `0`.
If used with two arguments, the first one is starting number and the second one is the limit.
When used with tree arguments, the last arguments is a step between the result numbers.

## filter
This is a common function in JavaScript and Python. But LIPS procedure also allow accepting
regular expression as a filter.

```scheme
(filter odd? (range 10))
;; ==> (1 3 5 7 9)
(filter #/0|7/ (range 10))
;; ==> (0 7)
```

## complement
Function return the opposite of the predicate:

```scheme
(define not-null? (complement null?))
(not-null? #null)
;; ==> #f
(not-null? 10)
;; ==> #t
```

## pluck
This is a procedure that operate on JavaScript objects it return procedure that return specified fields from an object.
If you pass single argumnent the function will return the value. Of you pass more than one argument it will return
an object with only those keys provided as arguments. Arguments can be strings or symbols.

```scheme
(define jack &(:first "Jack" :last "Kirby" :age 77))
(define stan &(:first "Stan" :last "Lee" :age 96))
(define steve &(:first "Steve" :last "Ditko" :age 91))

(define get-name (pluck "first"))
(string-append (get-name stan)
               " likes "
               (get-name jack)
               " and "
               (get-name steve))
;; ==> "Stan likes Jack and Steve"

(define full-name-data (pluck "first" "last"))


(map full-name-data (list jack stan))
;; ==> (&(:first "Jack" :last "Kirby") &(:first "Stan" :last "Lee"))

(define (full-name person)
  (let ((person (full-name-data person)))
    (--> (Object.values person) (join " "))))

(map full-name (list jack stan steve))
;; ==> ("Jack Kirby" "Stan Lee" "Steve Ditko")
```

The last example use interop with JavaScript see [Integration with
JavaScript](/docs/lips/intro#integration-with-javascript) to know more.

## flip
This is very useful procedure that return new procedure with swapped first two arguments.

```scheme
(take '(1 2 3 4) 2)
;; ==> (1 2)
(define take (flip take))
(take 2 '(1 2 3 4))
;; ==> (1 2)
(unset! take)
```

This function helps in composing functions with functions like `curry`. `unset!` is a function that
removes the binding of the first appearance of the object. Here we define take inside the current
environment and you can `unset!` it, to get the original value back.

## Combinations of functions

You can use curry with filter to create filter procedures:

```scheme
(define non-zero (curry filter (complement zero?)))

(non-zero '(1 0 2 3 0 4 5 0 6 7 0 8 9))
;; ==> (1 2 3 4 5 6 7 8 9)
```

Another thing you can do is curry the take procedure to get only two elements from the list with help of flip:

```scheme
(define first-two (curry (flip take) 2))
(first-two '(1 2 3 4))
;; ==> (1 2)
```

## Unary, binary, and n-ary
With LIPS you can change the arity of the function by forcing only specific number of arguments.

This is a classic error:

```scheme
(--> #("1" "2" "3" "4") (map string->number))
;; ==> #(1 +nan.0 +nan.0 +nan.0)
```

The error happen because Array::map pass additional arguments not only a value, and second argument
to `string->number` is Radix of the number (number base, default `10`). To fix the issue you can use `unary`:

```scheme
(--> #("1" "2" "3" "4") (map (unary string->number)))
;; ==> #(1 2 3 4)
```

Binary and n-ary work similarly but limit the number of arguments to 2 or any number.

## always and once
`always` is a procedure that return procedure that always return same value

```scheme
(map (always 1) (range 10))
;; ==> (1 1 1 1 1 1 1 1 1 1)
```

And `once` is a procedure that execute target procedure only once.

Let's create a macro that will count how many times the code is executed:

```scheme
(define-macro (with-time label expr)
  (let ((result #:result))
    `(begin
       (console.time ,label)
       (try
        ,expr
        (finally
         (console.timeEnd ,label))))))
```

**NOTE** `#:result` expression is [auto gensym](/docs/lips/extension#autogensyms) as one of
builtin [syntax extensions](/docs/lips/extension#syntax-extensions).

We can use this code to make sure that the function has been executed only once:

```scheme
(define (expensive value)
  (new Promise (lambda (resolve)
                 (setTimeout (curry resolve value) 1000))))

(with-time "expensive" (expensive "hello"))
;; ==> expensive: 1.003s
;; ==> "hello"
(define only-once (once expensive))

(with-time "expensive" (only-once "message"))
;; ==> expensive: 1.023s
;; ==> "message"

(with-time "expensive" (only-once "message"))
;; ==> expensive: 9.609ms
;; ==> "message"

(with-time "expensive" (only-once "different message"))
;; ==> expensive: 10.022ms
;; ==> "message"
```

## some and every
Work like JavaScript
[Array::some](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some)
and
[Array::every](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every).
But the operate on lists and return true when predicate return true of any element (`some`) or for
all elemebts (`every`).

```scheme
(some null? '(1 2 3 #null))
;; ==> #t
(some null? '(1 2 3))
;; ==> #f
```

```scheme
(every number? '(1 2 3 4))
;; ==> #t
(every number? '(1 2 3 "4"))
;; ==> #f
```

## pipe

Pipe is higher ordder procedure that accept functions as arguments and return a new function that apply those function in order:

```scheme
(define non-zero (curry filter (complement zero?)))
(define (fraction x)
  (/ 1 x))

(define calculate (pipe non-zero (curry map fraction)))

(calculate '(1 2 0 3 4 0 5 6 0 7 8 0 9))
;; ==> (1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9)
```

The code removes the zeros from list before applying the fraction that will throw an error on zero.

## folding

LIPS define function `reduce` that is an alias to standard Scheme procedure `fold-right` and fold that is the same
as `fold-left`. Both procedures works similarly. The take a procedure and a list and reduce it into a single value.

**NOTE** the reduce works differently than in JavaScript, the callback function get accumulator in last argument.


```scheme
(reduce cons '() '(1 2 3 4 5))
;; ==> (5 4 3 2 1)
```

The function also accept more than one list.

```scheme
(reduce (lambda (num str acc)
          (cons num (cons str acc)))
        '()
        '(1 2 3 4 5)
        '("foo" "bar" "baz" "quuz"))
;; ==> (4 "quuz" 3 "baz" 2 "bar" 1 "foo")
```

`fold` function work similarly but the order of execution is reversed.

```scheme
(fold cons '() '(1 2 3 4))
;; ==> (1 2 3 4)
```

Same as reduce it accept more than one list:

```scheme
(fold (lambda (num str acc)
          (cons num (cons str acc)))
        '()
        '(1 2 3 4 5)
        '("foo" "bar" "baz" "quuz"))
;; ==> (1 "foo" 2 "bar" 3 "baz" 4 "quuz")
```

**NOTE**: here we use `cons` to create a list, `cons` construct the list in reverse order so `reduce`
start from first element and `fold` is reversed:

```scheme
(reduce (lambda (item acc)
          (print acc)
          (cons item acc))
        '()
        '(1 2 3 4))
;; ==> (1)
;; ==> (2 1)
;; ==> (3 2 1)
;; ==> (4 3 2 1)
```

```scheme
(fold (lambda (item acc)
          (print acc)
          (cons item acc))
        '()
        '(1 2 3 4))
;; ==> (4)
;; ==> (3 4)
;; ==> (2 3 4)
;; ==> (1 2 3 4)
```

## unfold
Unfold is the opposite of folding. You can use it to create a list based on single function.
The [callback function](https://en.wikipedia.org/wiki/Callback_(computer_programming)) accept next item and should return
#f when done or pair of two values element that should be added to the output list and next value.

Here is example of a procedure that use:

```scheme
(unfold (lambda (n)
          (if (> n 50)
              false
              (cons n (+ n 10))))
        10)
;; ==> (10 20 30 40 50)
```

And here is more complex example with a utility function:

```scheme
(define (range-between a b)
  (let* ((inc (< a b))
         (test (if inc > <))
         (op (if inc + -)))
    (unfold (lambda (x)
                  (if (test x b)
                      false
                      (cons x (op x 1))))
            a)))

(range-between 10 20)
;; ==> (10 11 12 13 14 15 16 17 18 19 20)
```

## flatten
This procedure flattens the tree structure into flat list, similar to
[`Array::flat` in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flat).

```scheme
(flatten '(((((1) 2) 3) 4) 5))
;; ==> (1 2 3 4 5)
```
