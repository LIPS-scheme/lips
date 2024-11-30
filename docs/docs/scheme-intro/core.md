---
sidebar_position: 3
description: Main article that shows most of the features of Scheme
---

# Core of Scheme

## Printing values

To print a value, you can use two type of expressions:

```scheme
(display "hello")
;; ==> hello
```

and

```scheme
(write "hello")
;; ==> "hello"
```

The first will print text hello without quotations. But the second will include the quotes. The
second expression allows saving the expression and later read it as Scheme code.

Both expression don't add newline at the end. To add a newline, you need to use:

```scheme
(newline)
```

Or you can use escape newline character:

```scheme
(display "\n")
```

## Math expressions

Scheme defines standard Math operations:
* `+` - sum all its arguments
* `-` - subtract the arguments
* `/` - divide the arguments
* `*` - multiply the arguments

All the above can accept zero or more arguments.

Trigonometry functions:
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

It also defines:
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

Rational and complex numbers are created from two different numbers.

Rational numbers are created from numerator and denominator, and you can get those numbers from a single rational:

```scheme
(numerator 1/2)
;; ==> 1
(denominator 1/2)
;; ==> 2
```

**NOTE:** The result values of those expressions are written as comments.

Complex numbers are created with real and imaginary parts, and you can also extract those parts:

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

## Predicates

### Equal operation

In Scheme there are a different way to compare values:

* `eq?` - compares if the values are the same object works only on basic types
* `eqv?` - compares if the values have the same representation
* `equal?` - also works any type of values, it can compare vectors and list if they are the same

### String and character comparators

In addition, there are also comparators for strings:
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

String and characters also have counterpart procedures for compare with case-insensitive way:
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

### Symbols
* `symbol=?`

### Type Predicates
* `pair?`
* `list?`
* `null?`
* `symbol?`
* `boolean?`
* `number?`
* `string?`
* `char?`
* `integer?`
* `complex?`

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

To modify (mutate) existing variable, you use `set!` procedure. There is a conversion of using exclamation
mark for destructive type of procedure. Which are procedures that modify its arguments.

```scheme
(define number 10)
(set! number (+ number 1))
(display number)
;; ==> 11
```

In the above expression, the number is increased by `1`. The number in `(+ number 1)` reference old value
of the variable. And `set!` special form update the variable with new value.

## Local variables

You can create local variables with `let` syntax:

```scheme
(let ((x 10) (y 20))
  (+ x y))
;; ==> 30
```

This will create two variables `x` and `y` with values `10` and `20` respectively and sum those with
plus procedure.

There is also additional `let*` expression that allow to return previous value in next expression:

```scheme
(let* ((x 10) (y (* x x)))
  (+ x y))
;; ==> 110
```

And `letrec` that allows to create local recursive functions (allow reference same binding):

```scheme
(letrec ((sum (lambda (list)
                (if (null? list)
                    0
                    (+ (car list) (sum (cdr list)))))))
  (sum '(1 2 3 4)))
;; ==> 10
```

## Conditionals

In Scheme there are 3 ways to define conditionals. The basic expression is `if` statement.

```scheme
(if (< 1 2)
    (write "this is true")
    (write "this is false"))
```

If you need to put more than one expression inside if statement (or any other expression that
expect single expression) you can use begin:

```scheme
(if (< 1 2)
    (begin
      (write "this is true")
      (newline)))
;; ==> "this is true"
```

The else part is optional.

You also have a shortcut for this case in `when`:

```scheme
(when (< 1 2)
     (write "this is true")
     (newline))
;; ==> "this is true"
```

There is also create the opposite with `unless`:

```scheme
(unless #f
     (write "this is true")
     (newline))
;; ==> "this is true"
```

`cond` is another expression that allow to add multiple conditions:

```scheme
(cond ((< 2 2) (write "first"))
      ((< 2 1) (write "second"))
      (else
         (write "other")))
;; ==> "other"
```

The first two expressions return false, so `cond` will evaluate the `else` condition and display `"other"`.

Case is the last of basic condition expressions. It allows checking given expression is one of the given values.

```scheme
(let ((x 'foo))
  (case x
    ((one two) (display "first"))
    ((foo bar) (display "second"))
    (else
       (display "other"))))
;; ==> second
```

Symbol foo is of the second list, so this expression will print `"second"`.

## Boolean expressions
Scheme provide 3 boolean special forms that can be used to combine other expressions:

They are not functions but special forms that can be used to create [Short-circuit
evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation) also called McCarthy evaluation
from [John McCarthy](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)) inventor of
Lisp.


* `and` - returns `true` when all elements are true value (in Scheme all values are true except `#f`),
  and stop evaluates when it finds `#f`

```scheme
(if (and (< 1 2) (> 3 1))
    (display "true"))
;; ==> true
```

* `or` - returns `#f` when all elements are `#f`, and return `#t` immediately when any of the values is `true`.

```scheme
(if (or (< 1 2) (/ 1 0))
    (display "true"))
;; ==> true
```

This expression will not evaluate `(/ 1 0)` which will give **Division by zero** error because it
stop evaluating when it finds the first true value.

* `not` - not negates the value. If the value is true it will return `#f` otherwise it will return `#t`.

```scheme
(if (not (zero? 10))
    (display "not zero"))
;; ==> not zero
```

## Procedures

To define a procedure or a function, you use `lambda` expression:

```scheme
(define square (lambda (x) (* x x)))
```

This defines a function square that multiply its argument by itself. Lambda is a way to create
anonymous function and define assign it to the symbol square. The name `lambda` is nowadays common
name to define anonymous function (example in languages like python or Java), but the name came from
[Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)

There is also a shortcut to define procedure/function:

```scheme
(define (square (x) (* x x)))
```

There are no explicit `return` statement. Only the last expression of the function is the result value.

You can also add more arguments:

```scheme
(define (sum a b)
             (+ a b))
(sum 10 20)
;; ==> 30
```

### Nested Procedures

You can define inner procedures inside other procedures:

```scheme
(define (cube x)
  (define (square x)
    (* x x))
  (* x (square x)))
```

### Immediately invoked lambda
When calling a function, that first element doesn't need to be a symbol. It can be expression which
evaluates to a function. So you can use lambda expression as first argument, but don't call it only
evaluate it immediately, without saving it in a variable.

```scheme
((lambda (x) (* x x)) 10)
;; ==> 100
```

### Variable number of arguments
Built-in `+` function allow summing all its arguments. You can create function that accept variable
number of arguments yourself.

```scheme
(define sum (lambda args (apply + args)))
```

This function invokes a function `+` with its arguments. Note that are no parentheses around
arguments. So all arguments will be saved inside `args` parameter. `apply` can be called with
procedure as first argument, multiple arguments and last argument needs to be a list.

if you invoke
```scheme
(sum 1 2 3 4)
```

The `args` will contain a list `'(1 2 3 4)`. The same, you can use improper list (with dot inside)
as arguments:

```scheme
(define expression (lambda (first . rest) (/ first (apply + rest))))
(expression 1 2 3 4)
;; ==> 1/9
```

### Optional arguments
When using improper lists as function parameter, you can create optional arguments:

```scheme
(define (rational first . rest)
  (let ((second (if (null? rest) 1 (car rest))))
    (/ first second)))
```

This will create a procedure that have second argument optional. When invoking:

```scheme
(rational 10)
```

it will evaluate:

```scheme
(/ 10 1)
;; ==> 10
```

and when you evaluate:

```scheme
(rational 1 2)
;; ==> 1/2
```

If scheme provides rational numbers, or it will return `0.5` otherwise.

### Recursion
You can define a function that reference to itself:

```scheme
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 10)
;; ==> 3628800
```

There is a main if statement that is called base condition. If the value `n` is less or equal 1 it
stop recursion and return 1. If not, it calls itself recursively decreasing the value `n`.

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

By default, you can define a local variable with let that is a `lambda` that reference itself. But you can do this with `letrec` syntax:

```scheme
(letrec ((sum (lambda (x)
                (if (zero? x)
                    0
                    (+ x (sum (- x 1)))))))
  (sum 10))
;; ==> 55
```

### Tail Call Optimization

When you create recursive function and with deeply nested calls, you may run out of memory. This type of
error is called [Stack Overflow](https://en.wikipedia.org/wiki/Stack_buffer_overflow).

Scheme have unique feature called TCO and optimize the code and don't consume the stack when
calculation, deeply recursive function. The code written in TCO will never lead to Stack Overflow errors.

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

If you need to create a recursive procedure that accumulate something, like create a list of create
a value, you need to add additional variable where you will old that value, the local variable is
ofen called result, but you can name it like you want.

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
expressions. In the above example, we have `i` and `result` variables. The `i` variable is incremented by
1 starting from 1. And `result` starts from empty list and add element to the list using `cons`.
The second list have two values, stop condition and result of the whole expression. The rest is body
that is executed on each iteration.

So the code will print each number and return a list of numbers.

## List operations

You can use `list-ref` to reference nth element of the list

```scheme
(let ((lst '(1 2 3 4 5 6 7)))
  (print (cadddr lst))
  (print (list-ref lst 3)))
```

Both expressions in let will print number `4` which is the 4th element of the list.

### Iterating over a list recursively

This is the basic pattern you use to iterate over a list using recursion:

```scheme
(define (operation lst)
  (if (null? lst)
      ...
      (operation (cdr lst))))

```

Here is an example of a function that check if an element is present in the list:


```scheme
(define (contains item lst)
  (if (null? lst)
      #f
      (if (equal? item (car lst))
          #t
          (contains item (cdr lst)))))


(let ((lst '(1 2 3 4 5 6 0 7 8 9)))
  (print (contains 0 lst)))
;; ==> #t

(let ((lst '(1 2 3 4 5 6 0 7 8 9)))
  (print (contains "x" lst)))
;; ==> #f
```

### Alists

Alists (or Association list) is a way to create objects in Scheme using lists. The alist is created with
list of cons cells:

```scheme
(list (cons "x" 10) (cons "y" 20) (cons "z" 30))
;; ==> (("x" . 10) ("y" . 20) ("z" . 30))
```

You can also create alist using quotation:

```scheme
'(("x" . 10) ("y" . 20) ("z" . 30))
```

You have 3 functions that operate on alists:

* `assq`
* `assv`
* `assoc`

The return pair that match first argument or `#f` if not found. The alist is passed as second
argument.  They use `eq?`, `eqv?`, and `equal?` respectively.

```scheme
(let ((x '((foo . 10) (bar . 20) (baz . 30))))
  (print (assoc 'bar x))
  (print (assoc 'quux x)))
;; ==> (bar . 20)
;; ==> #f
```

First call will return pair `(bar . 20)` because its `bar` symbol is present in the alist. And the
second call will print `#f`.

You can use `cond` expression with `=>` syntax to get the value of alist:

```scheme
(let* ((alist '((foo . 10) (bar . 20) (baz . 30)))
       (result (cond ((assoc 'bar alist) => cdr)
                     (else '()))))
  (if result
      (print result)))
;; ==> 20
```

## Finding element in the list

Similar to operation on alist there are 3 functions that find if the element is present in the normal list

* `memq`
* `memv`
* `member`

The return cons cell where `car` match object passed as first argument or #f if not found:

```scheme
(let ((lst '(1 2 3 x y z)))
  (print (member 'x lst))
  (print (member 'foo lst)))
;; ==> (x y z)
;; ==> #f
```

## Vector operations
Same as operation on list you can operate on list, you have different procedure to operate on vectors.

```scheme
(let ((v (vector #\h #\e #\l #\l #\o)))
  (write (vector-ref v 0))
  (newline)
  (vector-set! v 0 #\H)
  (write (vector-ref v 0))
  (newline)
  (print (vector->string v))
  (newline))
;; ==> #\h
;; ==> #\H
;; ==> Hello
```

To check if an option is a vector, you can use `vector?` predicate.

## String operations
Similar to operation on vectors and lists, you have procedures to operate on strings.

```scheme
(let ((str (string #\h #\e #\l #\l #\o)))
  (write (string-ref str 0))
  (newline)
  (string-set! str 0 #\H)
  (write (string-ref str 0))
  (newline)
  (write str)
  (newline))
;; ==> #\h
;; ==> #\H
;; ==> "Hello"
```

To check if an object is a string, you can use `string?` predicate.

## Multiple values
By default, functions in Scheme return a single value, but you can return multiple values with `values` expression.

```scheme
(define (div-mul x y)
  (values (/ x y) (* x y)))

(define (plus-minus x y)
  (values (+ x y) (- x y)))

(display (div-mul 2 10))
;; ==> 1/5 20
(display (plus-minus 2 10))
;; ==> 12 -8
```

When you try to use this value in expression:

```scheme
(let ((x (div-mul 2 10)))
  (display (* x 2)))
```

Some Scheme implementation will evaluate that expression and get the first value. And some
implementation will throw an error about expecting number but got multiple values.

To safety access both values, you can use `call-with-values` procedure:


```scheme
(call-with-values (lambda () (div-mul 2 10))
  (lambda (div mul)
    (display div)
    (newline)
    (display mul)
    (newline)))
;; ==> 1/5
;; ==> 20
```

You also have `let-values` and `let*-values` expressions, that works similar to `let` and `let*`.

```scheme
(let-values (((plus minus) (plus-minus 2 10))
             ((div mul) (div-mul 2 10)))
  (+ div mul plus minus))
;; ==> 121/5
```

Note that there are two open parentheses before div. The pair is like with let:

```scheme
(let ((x (div-mul 2 10)))
  ...)
```

And instead of `x` you have a list with two values that came from `values` expression.

The `let-values` also accept normal (single value expression like `let`) so you can mix them.
Single expression still need to be a list but with a single value.

```scheme
(let*-values (((x) 2)
              ((y) 10)
              ((div mul) (div-mul x y)))
  (+ div mul))
;; ==> 101/5
```

## Higher order functions

Functions in Scheme are [first class](https://en.wikipedia.org/wiki/First-class_citizen), which
means that are the same as any other values like numbers. And can be passed around. You can create a
function that accept other function or create a function that return a function. Functions that
operate on functions are called [higher order
functions](https://en.wikipedia.org/wiki/Higher-order_function) or higher order procedures.

Scheme define few built in higher order functions like `map`, `for-each` they both accept a function
and execute them for every element of the list. `map` return new list from the values and `for-each`
return unspecified value.

```scheme
(map square '(1 2 3 4))
;; ==> (1 4 9 16)
(map (lambda (x) (* x 10)) '(1 2 3 4))
;; ==> (10 20 30 40)
```

You can also define your own higher order functions:

```scheme
(define (filter fn lst)
  (if (null? lst)
      '()
      (let ((item (car lst)))
        (if (fn item)
            (cons item (filter fn (cdr lst)))
            (filter fn (cdr lst))))))

(filter odd? '(1 2 3 4 5))
;; ==> (1 3 5)
(filter (lambda (x) (not (zero? x))) '(1 2 0 3 0 0 0 4 5 0 6 7))
;; ==> (1 2 3 4 5 6 7)
```

You can use function that return lambda to create different list accessors for elements more than 4.

```scheme
(define (list-selector n)
  (lambda (list)
    (list-ref list n)))

(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define fifth (list-selector 4))
(define sixth (list-selector 5))
(define seventh (list-selector 6))
(define eighth (list-selector 7))
(define ninth (list-selector 8))
(define tenth (list-selector 9))
```

Another useful procedure is `alist-map`:

```scheme
(define (alist-map fun alist)
  (map (lambda (item) (fun (car item) (cdr item))) alist))
```

You can use this procedure to map over values or keys inside an [alist](#alists).

```scheme
(define alist (map cons '(a b c d) '(1 2 3 4)))

(alist-map (lambda (key value) (cons key (* value 10))) alist)
;; ==> ((a . 10) (b . 20) (c . 30) (d . 40))
(define (symbol-upcase symbol)
  (string->symbol (string-upcase (symbol->string symbol))))

(alist-map (lambda (key value) (cons (symbol-upcase key) value)) alist)
;; ==> ((A . 1) (B . 2) (C . 3) (D . 4))
```

## Closures
Scheme have lexical scope. Which means that if functions don't define a variable Scheme search them
outside in the place where procedure was defined. This allows to create closures. Which are basically
functions that have access to variables defined outside. Scheme need to keep environment in where
procedure was defined together with a procedure.

```scheme
(define counter (let ((count 0))
                  (lambda ()
                    (set! count (+ count 1))
                    count)))
```

This creates a function that have access to a variable defined outside in let expression.

```scheme
(counter)
;; ==> 1
(counter)
;; ==> 2
(counter)
;; ==> 3
```

With this, you can create functions that create counters that start with a given number:

```scheme
(define (make-counter n)
  (let ((count n))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter-1 (make-counter 100))
(define counter-2 (make-counter 0))

(counter-1)
;; ==> 101
(counter-1)
;; ==> 102
(counter-2)
;; ==> 1
(counter-1)
;; ==> 103
(counter-2)
;; ==> 2
```

Each counter has its own local state and its own counter variable.

## Objects

In Scheme you can have objects (data with behavior), the base code for objects use closures.

```scheme
(define (make-person name age)
  (lambda (action . rest)
    (case action
      ((name) name)
      ((age) age)
      ((set-name) (set! name (car rest)))
      ((set-age) (set! age (car rest))))))

(let ((jack (make-person "Jack" 22)))
  (display (jack 'name))
  (newline)
  (jack 'set-name "Mark")
  (jack 'set-age 24)
  (display (jack 'name))
  (display " is ")
  (display (jack 'age))
  (display " years old"))
;; ==> Jack
;; ==> Mark is 24 years old
```

Notice that it's function which returns a function (`lambda`). You can send a message into that object,
it will process it by returning a value from closure or mutating that value.

In the same way you can use [alist](#alists).

### Records
Anther way to create objects in Scheme, are R<sup>7</sup>RS records, they were first defined in
[SRFI-9](https://srfi.schemers.org/srfi-9) and included in the official standard.

You can define records that represent cons cells to create linked lists:

```scheme
(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))
```

The record type is defined with:
* constructor
* predicate that test if the element is of given type
* and list of fields

You can use this record like this:

```scheme
(define k (kons 1 2))
(pare? k)
;; ==> #t
(kar k)
;; ==> 1
(kdr k)
;; ==> 2
(set-kar! k 2)
(cons (kar k) (kdr k))
;; ==> (2 . 2)

(define (klist . args)
  (if (null? args)
      ()
      (kons (car args) (apply klist (cdr args)))))

(define (kprint klist)
  (display "(")
  (let loop ((klist klist))
    (when (not (null? klist))
      (display (kar klist))
      (let ((rest (kdr klist)))
        (unless (null? rest)
          (display " ")
          (loop rest)))))
  (display ")"))

(define kl (klist 1 2 3 4))

(kar (kdr (kdr (kdr kl))))
;; ==> 4

(kprint kl)
;; ==> (1 2 3 4)
```

## Dynamic variables
Even that Scheme has lexical scope, you can define dynamic variables. They are the opposite of
lexical variables. When you define a dynamic variable, Scheme will search for them not in the place
where function is defined, but in the place where it's called. That's why if you have fully dynamic lisp
you can't have closures. Unless you can somehow add lexical variables. This is the case of
[Emacs Lisp](https://en.wikipedia.org/wiki/Emacs_Lisp), lisp that is embedded into an
[Emacs editor](https://en.wikipedia.org/wiki/Emacs).

To create dynamic variable in Scheme, you can code like this:

```scheme
(define x (make-parameter 0))

(define (add-x y)
  (+ (x) y))

(add-x 10)
;; ==> 10
(parameterize ((x 10))
  (add-x 10))
;; ==> 20
```

Parameters works like procedures. Do define new dynamic parameter you use `make-parameter` and to
change its value you can use `parameterize` that works like `let`. You can also call the parameter
with different value and the parameter will use this value as default.

```scheme
(x 10)
(add-x 10)
;; ==> 20
(parameterize ((x 3))
  (add-x 3))
;; ==> 6
```

## Loading of external code
You can execute external code inside Scheme by using `load` procedure.

```scheme
(load "file.scm")
```

This will load an external file named `file.scm`. Scheme files often end with `scm`, but different
scheme implementation may use different convention.

## Eval
The `eval` procedure is used to evaluate code that is written as data.

```scheme
(define expr '(+ 1 2))
(eval expr)
;; ==> 3
```

Some scheme implementation may require to specify second argument which is environment.

You can use those procedures to get different environments:
* `(interaction-environment)` - this return environment for the REPL
* `(scheme-report-environment <number>)` - creates environment with functions from a given
  R<sup>n</sup>RS specification

## Scheme libraries
R<sup>7</sup>RS standard of Scheme also define a way to define libraries. This is a common way to
create modules that can be used inside your project or my other people.

To import a library that is part of the scheme implementation, you use `import` expression:

```scheme
(import (srfi 1))

(zip '(1 2 3)
     '(foo bar baz)
     '(one two three))
;; ==> ((1 foo one) (2 bar two) (3 baz three))
```

[SRFI-1](https://srfi.schemers.org/srfi-1/srfi-1.html) is SRFI that defines a library of procedures
that operate on lists (like `zip` procedure that join multiple lists).

You can define your own libraries with `define-library` expression:

```scheme
(define-library (example factorial)
  (export !)
  (import (scheme base))
  (begin
    (define (range n)
      (let loop ((n n) (result '()))
        (if (<= n 0)
            result
            (loop (- n 1) (cons n result)))))
    (define (! n)
      (apply * (range n)))))
```

This is definition of library `(example factorial)` that define single procedure `!` that calculate
factorial of a number. It defines helper hidden (not exported) procedure `range` that creates a list
of numbers from `1` to `n`.

You can use this library like this:

```scheme
(import (example factorial))
(! 10)
;; ==> 3628800
```

## Portable code

Scheme implementation differ. And it's hard to write code that will work on multiple Scheme
Interpreters.  Luckily in [SRFI-0](https://srfi.schemers.org/srfi-0/srfi-0.html) (first SRFI ever
created). There is defined special syntax called `cond-expand`. A lot of Scheme implementations have
this SRFI built-in so you can use it to detect different Scheme and create code that will match that
Implementation.

You can use it like this:

```scheme
(cond-expand
 (kawa)
 (guile)
 (lips)
 (gauche)
 (else))
```

`cond-expand` have list of lists in format `(identifier . code)`. For example, if you want to add
`print` function that is defined in `LIPS`, but not in other implementations, you can use code like
this:

```scheme
(cond-expand
 (lips)
 (else
  (define (print . args)
    (for-each (lambda (arg)
                (display arg)
                (newline))
              args))
  (define (lips.parse expr)
    (with-input-from-port (open-input-string expr)
      (lambda ()
        (do ((result '() (cons (read) result)))
          ((eof-object? (peek-char)) (list->vector (reverse result)))))))))
```

It will evaluate an empty list for `LIPS` and `define` a new `print` and `lips.parse` procedures for
other implementations. Those procedures are part of LIPS Scheme, see [LIPS
Tutorial](/docs/lips/intro).  Some Scheme implementations may not support `with-input-from-port`
(like GNU Kawa) so it may require to add implementation for it.
