## Lips is Pretty Simple

[![npm](https://img.shields.io/badge/npm-0.1.0-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)

Lips is very simple Lisp, similar to Scheme writen in JavaScript.

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

all functions that match this regex `c[ad]{2,5}r`

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


### Access JavaScript functions

```scheme
((. window "alert") "hello")
((. console "log") "hello")
```

If object is not found in environment, then window object is tested for
presense of the element.

You can execute jQuery functions

```scheme
(let* (($ (. window "$"))
       (term ($ ".terminal")))
  ((.  term "css") "background" "red"))
```

or operate on strings

```scheme
((. "foo bar baz" "replace") /^[a-z]+/g "baz")

(let ((match (. "foo bar baz" "match")))
    (array->list (match /([a-z]+)/g)))
```

### Mapping and filtering

```scheme
(map car (list
            (cons "a" 2)
            (cons "b" 3)))

(filter odd (list 1 2 3 4 5))

(filter (lambda (x)
          (== (% x 2) 0))
    (list 1 2 3 4 5))
```

### Math and boolean operators

`< > => <= ++ -- + - * / % and or`

## License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018 [Jakub Jankiewicz](http://jcubic.pl/jakub-jankiewicz)
