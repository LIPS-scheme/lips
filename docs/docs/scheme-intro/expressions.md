---
sidebar_position: 3
---

# Core Expressions

## Printing values

To print a value you can use two type of expressions:

```scheme
(display "hello")
```

and

```scheme
(write "hello")
```

The first will print text hello without quotations. But the second will include the quotes. The
second expression allow to save the expression and later read it as Scheme code.

Both expression don't add newline at the end. To add a newline you need to use:

```scheme
(newline)
```

Or you can use escape newline character:

```scheme
(display "\n")
```

## Math expressions

Scheme define standard Math operations:
* `+` - sum all it's arguments
* `-` - subtract the arguments
* `/` - divide the arguments
* `*` - multiple the arguments

All the above can accept zero or more arguments.

trigonometry functions:
* `sin`
* `cos`
* `tan`
* `asin`
* `acos`
* `atan`

logarithms:
* `log`

Exponentiation function:
* `expt`

Exponential function
* `exp`

It also define:
* `square`
* `sqrt`

### Boolean expression
Expressions that returns `true` or `false` and operate on numbers

* `<`
* `>`
* `<=`
* `>=`
* `=`

### Functions that returns part of the number

Rational and complex numbers are create from two different numbers.

Rational numbers are created from numerator and denominator and you can get those numbers from a single rational:

```scheme
(numerator 1/2)
;; ==> 1
(denominator 1/2)
;; ==> 2
```

**NOTE:** The result values of those expressions are written as comments.

Complex numbers are created with real and imaginary parts and you can also extract those parts:

```scheme
(imag-part 10+2i)
;; ==> 2
(real-part 10+2i)
;; ==> 10
```

Scheme also define two functions `angle` and `magnitude` which can be used to get modulus and argument.

```scheme
(angle 10+10i)
;; ==> 0.7853981633974483
(magnitude 10+10i)
;; ==> 14.142135623730951
```

## Equal operators

In Scheme there are different way to compare values:

* `eq?` - compares if the values are the same object works only on basic types
* `eqv?` - compares if the values have the same represention
* `equal?` - also works any type of values, it can compare vectors and list if they are the same

In addition there are also comparators for strings:
* `string=?`
* `string<?`
* `string>?`
* `string<=?`
* `string>=?`

characters:
* `char=?`
* `char<?`
* `char>?`
* `char<=?`
* `char>=?`

String and characters also have counterpart procedures for compare with case insensitive way:
* `string-ci=?`
* `string-ci<?`
* `string-ci>?`
* `string-ci<=?`
* `string-ci>=?`

characters:
* `char-ci=?`
* `char-ci<?`
* `char-ci>?`
* `char-ci<=?`
* `char-ci>=?`

and symbols:
* `symbol=?`

## Variables

To define a variable in Scheme you use `define` special form:

```scheme
(define number 10)
```

This will define variable `number` with value 10. You can evaluate this variable and get the value back.

```scheme
number
```

This will evaluate to 10. Note that this:

```scheme
'number
```

Will evaluate into symbol `number`.


### Modification of the variable

To modify existing variable you use `set!` procedure. There is a conversion of using exclamation
mark for destructive type of procedure. Which are procedures that modify it's arguments.

```scheme
(define number 10)
(set! number (+ number 1))
```

In above expression the number is increased by `1`. The number in `(+ number 1)` reference old value
of the variable. And `set!` special form update the variable with new value.

## Local variables

You can create local variables with `let` syntax:

```scheme
(let ((x 10) (y 20))
  (+ x y))
```

This will create two variables `x` and `y` with values `10` and `20` respectively and sum those with
plus procedure.

## Conditionals

In Scheme there are 3 ways to define conditionals. The basic expression is `if` statement.

```scheme
(if (< 1 2)
    (display "this is true")
    (display "this is false"))
```

If you need to put more than one expression inside if statement (or any other expression that
expect single expression) you can use begin:

```scheme
(if (< 1 2)
    (begin
      (display "this is true")
      (newline)))
```

The else part is optional.


`cond` is another expression that allow to add multiple conditions:

```scheme
(cond ((< 2 2) (display "first"))
      ((< 2 1) (display "second"))
      (else
         (display "other")))
```

The first two expressions return false, so cond will evaluate the else condition and display `"other"`.

Case is the last of basic condition expressions. It allow to check given expression is one of the given values.

```scheme
(let ((x 'foo))
  (case x
    ((one two) (display "first"))
    ((foo bar) (display "second"))
    (else
       (display "other"))))
```

Symbol foo is of the second list so this expression will print `"second"`.


## Procedures

To define a procedure or a function you use `lambda` expression:

```scheme
(define square (lambda (x) (* x x)))
```

This define a function square that multiple it's argument by itself. Lambda is a way to create
anonymous function and define assign it to the symbol square. Name lambda is nowadays common name to
define anonymous function (example in languages like python or Java), but the name came from [Lambda
Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)

There is also a shortcut to define procedure/function:

```scheme
(define (square (x) (* x x)))
```

There are no explicit `return` statement. Only the last expression of the function is the result value.

### Recursion
You can define define a function that reference to itself:

```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 10)
;; ==> 3628800
```

You can also define recursion using named `let` syntax:

```scheme
(define (factorial n)
  (let loop ((n n))
      (if (<= n 1)
          1
          (* n (loop (- n 1))))))

(factorial 10)
;; ==> 3628800
```

#### Local Recursive Functions

By default you can define local variable with let that is an lambda that reference itself. But you can do this with `letrec` syntax:

```scheme
(letrec ((sum (lambda (x)
                (if (zero? x)
                    0
                    (+ x (sum (- x 1)))))))
  (sum 10))
;; ==> 55
```

### Tail Call Optimization

When you create recursive function and with deeply nested calls you may run out of memory. This type of
error is called [Stack Overflow](https://en.wikipedia.org/wiki/Stack_buffer_overflow).

Scheme have unique feature called TCO and optimize the code and don't consume the stack when
calculation deeply recursive function. The code written in TCO will never lead to Stack Overflow errors.

This is an example of Tail Call:

```scheme
(define (factorial n)
  (let loop ((n n) (result 1))
      (if (<= n 1)
          result
          (loop (- n 1) (* n result)))))
```

This function is similar to previous recursive function, but note that loop is the last expression,
the result of loop don't need to wait on anything. This type of code is optimized by Scheme and can
recur any number of types.

## Loops
Recursion is not the only way to create loops in Scheme. You also have `do` syntax:

```scheme
(do ((i 1 (+ i 1))
     (result '() (cons i result)))
    ((> i 10) result)
  (display i)
  (newline))
```

First list of do expression have variable initialization and increment, there can be more
expressions. In above example we have `i` and `result` variables. The `i` variable is incremented by
1 starting from 1. And `result` starts from empty list and add element to the list using `cons`.
The second list have two values stop condition and result of the whole expression. The rest is body
that is executed on each iteration.

So the code will print each number and return list of numbers.
