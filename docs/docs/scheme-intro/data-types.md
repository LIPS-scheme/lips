---
sidebar_position: 2
---

# Data Types

## Numbers

R<sup>7</sup>RS specification define Numerical Tower, different type of numbers and operation on them.
But it's not required for Scheme implementation to add support for them. Some Scheme implementation do.

Basic numbers are:

* Integers, e.g. `10`, `20`, or `1000`
* floats, e.g. `1.2`, `1e-2` (you can use [scientific notation](https://en.wikipedia.org/wiki/Scientific_notation))
* rationals e.g. `1/2`, `3/4`, or `1/10`
* complex numbers `10+10i`, `1/2+1/2i`

There is also notion of exactness in those numbers. Inexact numbers are floats, the rest of the
numbers are exact because they don't give any rounding errors like floats do (this is how
[IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) standard for floating numbers work).

There are also special constant `+nan.0`, `-nan.0`, `+inf.0`, and `-inf.0`. Positive and negative
Not a number object and negative and positive infinity.

## Boolean values

Scheme define two boolean constants `#f` and `#t` but note that the only false value, according to
R<sup>7</sup>RS specification, should be `#f`. Specification also define `#true` and `#false`
aliases. Some Scheme also define `true` and `false` without hash.

## Strings
Strings in Scheme use only double quote symbols. They can be multiline. If you want to add double
quote you need to escape with with slash `"this is \"Scheme\" languguage"`.

You can also inject hex representation of a character with `"\x1B;"` this will create string with
Escape character used by Terminal emulators to add formatting (like colors).

## Characters

You can define single character as data type

```scheme
#\A
#\B
#\c
```

you can use characters form a string:
```scheme
(string #\h #\e #\l #\l #\o)
```

This evaluates into string `"hello"`

You can also split the string into individual characters:

```scheme
(string->vector "hello")
```

This evaluates into vector of characters: `#(#\h #\e #\l #\l #\o)`

## Symbols

Symbols are special type of objects that are similar to string but without quotes. They can appear as
variable names but can also be used as values but this require quotation.

Valid symbols:

```scheme
foo
bar
baz
```

You can also convert string to symbol with:

```scheme
(string->symbol "hello")
```

Scheme have conversion of using `->` arrow to define procedure that convert types.

## Comments

There are 3 types of comments in Scheme:

1. `;`semicolon create comments to the end of the line
2. `#;` quote single S-Expression (list)
3. `#|   |#` those are multiline comments that can wrap any text inside

## Empty list

An empty list is a special object in Scheme that indicates end of the list.

```scheme
()
```

## Pairs

The base of Lisp and Scheme are Pairs also called `cons` cells. You can create them with cons operation:

```scheme
(cons 1 2)
```

or with dot syntax:

```
(1 . 2)
```

But the second example require quotation otherwise Scheme will try to evaluate 1 as a function.

If cons cells are put into e sequence:

```scheme
(1 . (2 . (3 . ())))
```

The last cell is a pair of 3 and empty list. If you create cons like this, it's simplified as list when printed:

```scheme
(1 2 3)
```

To create the same list you can use `list` procedure

```scheme
(list 1 2 3)
```

To get the first element of the list you use procedure `car` and to get the rest of the list you use `cdr`.
So `car` and `cdr` returns first and second element of the Pair (cons cell).

Scheme used `car` and `cdr` for historical reasons. The first lisp interpreter was using address
registers of [IBM 704](https://en.wikipedia.org/wiki/IBM_704) mainframe computer.

* car stands for **Contents of the Address part of Register**
* cdr stands for **Contents of the Decrement part of Register**

Scheme should also define abbreviations for list accessors:

example `caddr` is third element of the list. It's the same as `(car (cdr (cdr x)))`. Often Scheme
and lisp interpreters define up to 5 combinations of `d` and `a` to get different elements out of a
list.

## Cycles

You can create list cycles directly when defining your data structure with datum syntax.

It looks like this:

```scheme
#0=(1 2 . #0#)
```

This will create an infinite list of values `(1 2 1 2 1 2 ...)`. `#0` indicate pointer and `#0#` a reference to the beginning of the list.

## Vectors

Vectors are created like list but they have hash in front:

```scheme
#(1 2 3 4)
```

This will create immutable vector that can't be changed. To create a vector that can be modified you can use

```scheme
(vector 1 2 3)
```

## Quotations
By default if you write lists they are treated as code. To create a data you need quotations.

### Base Quote
To create basic quotation you use single quote character:

```scheme
'(1 2 3 4)
```

When Scheme find this expression it will not try to execute the function `1` only return this list as data.

Vector syntax is automatically quoted.

```scheme
'#(1 2 3)
```

is the same as:

```scheme
#(1 2 3)
```

### Quasiquote

There is also different type of quotation that allow execute part of the expression. It's called quasi quote.
To create quasi quote you need back tick symbol. That's why it's often called back quotation.

```scheme
`(1 2 ,(+ 2 1))
```

comma is special syntax that can be used inside quasi quote to indicate that expression after it should be evaluated and inserted into the list. This will evaluate into same expression as:

```scheme
'(1 2 3)
```

There is also another escape symbol which is `,@`. it work similar to comma but the data inside (it must be a list) is spliced into the outer list.

```scheme
`(1 2 3 ,@(list 4 5 6))
```

The result expression will look like this:

```scheme
'(1 2 3 4 5 6)
```

You can also use quasi quote with vectors:

```scheme
`#(1 2 ,(+ 1 2))
```

This will evaluate into vector with 3 elements:

```scheme
#(1 2 3)
```

### Quotation of quotation

If you quote the quotation you will get expression that change special symbols with S-Expressions:

```scheme
''(1 2 3)
```

Will return:

```shcheme
(quote (1 2 3))
```

Because comma is just an alias for quote. You can use them interchangeably. But using symbols is
faster to type. if you quote quasi quote expression you will also get symbols expanded:

```scheme
'`(1 2 3 ,(+ 1 2) ,@(list 4 5))
```

This will be the output:

```scheme
(quasiquote (1 2 3 (unquote (+ 1 2)) (unquote-splicing (list 4 5))))
```

## Special symbols

Most special symbols in Scheme start with a hash symbol. Example are Byte vectors

```scheme
#u8(1 2 3 4)
```

Above creates 8 bit byte vector of numbers. in R<sup>7</sup>RS only unsigned 8 bit vectors are
defined. But in [SRFI-4](https://srfi.schemers.org/srfi-4/srfi-4.html) are more bit vectors types.
They all starts with hash. In different SRFI there are more examples of syntax's that start with
hash. This is just a convention everything is using.

