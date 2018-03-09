## LIPS is Pretty Simple

[![npm](https://img.shields.io/badge/npm-DEV-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/jquery.terminal.svg?branch=devel&e2ab8772e6fe0a484857cffbe93bb7bcd716f1c2)](https://travis-ci.org/jcubic/jquery.terminal)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=devel&a6562124b6b8df22c67398db686aa9f0)](https://coveralls.io/github/jcubic/lips?branch=devel)



LIPS is very simple Lisp, similar to Scheme written in JavaScript.

[Demo](https://codepen.io/jcubic/full/LQBaaV/)

## Installation

use npm

```
npm install @jcubic/lips
```

then include the file in script tag, You can grab the version from unpkg.com

```
https://unpkg.com/@jcubic/lips
```

or from rawgit

```
https://cdn.rawgit.com/jcubic/lips/master/index.js
```

## Usage

```javascript
var {exec} = require('@jcubic/lips'); // node
// or
var {exec} = lips; // browser

exec(string).forEach(function(result) {
     console.log(result);
});
```

there is also longer version if you want to split the process of evaluation:

```
var {parse, tokenize, evaluate} = require('@jcubic/lips');

parse(tokenize(string)).forEach(function(code) {
    evaluate(code);
});
```

`evaluate` and `exec` functions also accept second argument, which is Environment.
By default it's `lips.global_environment`. You can use it if you want to
have separated instances of the interpreter.

You can create new environment using:

```javascript
var env = new Environment({}, lips.global_environment);
```

First argument is an object with functions, macros and varibles (see Extending LIPS at the end).
Second argument is parent environment, you need to use global environment (or other that extend global)
otherwise you will not have any functions.

## What's in

### variables and functions

```scheme
(define x 10)
(define square (lambda (x) (* x x)))
(define (square x) (* x x))
```

### List operications

```scheme
(cons 1 2)
(cons 1 (cons 2 nil))
(list 1 2 3 4)
'(1 2 3 4)

(let ((lst '(1 2 (3 4 5 6))))
   (print (car lst))
   (print (cadaddr lst)))
```

all functions that match this regex `c[ad]{2,5}r` are defined.

### ALists

```scheme
(let ((l '((foo . "lorem") (bar "ipsum"))))
   (set-cdr (assoc l 'foo) "hello")
   (set-cdr (assoc l 'bar) "world")
   (print l))
```

### Flow constructs

```scheme
(let ((x 5))
    (while (> (-- x) 0) (print x)))
```

same as in JS

```scheme
(if (== "10" 10)
    (print "equal"))

(let ((x 10))
  (if (and (> x 1) (< x 20))
      (begin
         (print "this is x > 1")
         (print "and x < 20"))))
```

### eval

```scheme
(eval (read "(print \"hello\")"))
```

### Lisp Macros

```scheme
(defmacro (foo x) `(1 2 ,@(car x) 3 4))
```

### Async code

```scheme
(eval (read))
```

then type S-Expression like `(print 10)`. If function return Promise
the execution is paused and restored when Promise is resolved


### Access JavaScript functions and objects

```scheme
((. window "alert") "hello")
((. console "log") "hello")
```

If object is not found in environment, then window object is tested for
presense of the element.

You can execute jQuery functions

```scheme
(let* ((term ($ ".terminal")))
  ((.  term "css") "background" "red"))
```

function `$` is available because it's in window object.

or operate on strings

```
((. "foo bar baz" "replace") /^[a-z]+/g "baz")

(let ((match (. "foo bar baz" "match")))
    (array->list (match /([a-z]+)/g)))
```

### Mapping, filtering and reducing

```scheme
(map car (list
            (cons "a" 2)
            (cons "b" 3)))

(filter odd (list 1 2 3 4 5))

(filter (lambda (x)
          (== (% x 2) 0))
    (list 1 2 3 4 5))

(define (reverse list)
    (reduce (lambda (list x) (cons x list)) list))

(reverse '(1 2 3 4))
```

### Working with arrays

You can modify array with `set` function and to get the value of the array you can use `.` dot function.

```scheme
(let ((arr (list->array '(1 2 3 4))))
   (set arr 0 2)
   (print (array->list arr)))

(let* ((div ((. document "querySelectorAll") ".terminal-output > div"))
       (len (. div "length"))
       (i 0))
    (while (< i len)
       (print (. (. div i) "innerHTML"))
       (++ i)))
```

this equivalent of JavaScript code:

```javascript
var div = document.querySelectorAll(".terminal div");
var len = div.length;
var i = 0;
while (i < len) {
   console.log(div[i].innerHTML);
   ++i;
}
```

### Math and boolean operators

`< > => <= ++ -- + - * / % and or`

## Extending LIPS

to create new function from JavaScript you can use:

```javascript
env.set('replace', function(re, sub, string) {
   return string.replace(re, sub);
});
```

then you can use it in LIPS:

```
(replace /(foo|bar)/g "hello" "foo bar baz")
```

To define a macro in javascript you can use Macro constructor that accept
single function argument, that should return lisp code (instance of Pair)

```javascript

var {Macro, Pair, Symbol, nil} = lips;

env.set('quote-car', new Macro(function(code) {
    return Pair.fromArray([new Symbol('quote'), code.car.car]);
}));
```

and you can execute this macro in LIPS:

```scheme
(quote-car (foo bar baz))
```

it will return first symbol and not execute it as function foo.

if you want to create macro like quasiquote, the returned code need to be wrapped with
Quote instance.

When creating macros in JavaScript you can use helper `Pair.fromArray()`
and `code.toArray()`.

## License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018 [Jakub Jankiewicz](http://jcubic.pl/jakub-jankiewicz)
