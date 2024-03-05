---
sidebar_position: 1
---

# Core features

## null and undefined
LIPS define those values as Parser constants so they can be used inside quoted expressions:

```scheme
(let ((lst '(undefined null)))
  (write (symbol? (car lst)))
  (newline)
  (write (symbol? (cadr lst)))
  (newline))
;; ==> #f
;; ==> #f
```

**NOTE** that they are not false values. LIPS follows R<sup>7</sup>RS spec and `#f` is the only falsy
value. This may change when final 1.0 is released. It's not yet decided if those should also be falsy
values, since they are part of JavaScript and it would simplify the code.

## Numerical tower
LIPS support full numerical tower (not yet 100% unit tested):

* integers - using BitInt
* floats - using JavaScript numbers
* rationals
* complex numbers (that can use integers, floats, or rationals)

## Print procedure
LIPS define helper `print` procedure that display all its arguments with newline after each element.

```scheme
(print 1 2 3)
;; ==> 1
;; ==> 2
;; ==> 3
```

## Emoji
LIPS fully supports all unicode characters including emoji:

```scheme
(define smiley #\😀)
(define poo #\💩)
(write (string-append (string smiley) " " (string poo)))
;; ==> "😀 💩"
```

You can also use them as part of symbols (e.g. as variables name):

```scheme
(define (⏏️)
  (print "ejecting"))
(⏏️)
;; ==> ejecting
```

## Macros
LIPS define both Lisp macros and Scheme hygienic macros (`syntax-rules`).

It also implements:
* [SRFI-46](https://srfi.schemers.org/srfi-46/) which allows to change ellipsis symbol for nested syntax-rules.
* [SRFI-139](https://srfi.schemers.org/srfi-139/) which allows to define
  [anaphoric syntax-rules macros](/docs/scheme-intro/macros#anaphoric-hygienic-macros).
* [SRFI-147](https://srfi.schemers.org/srfi-147/) which allows to define new syntax-rules macros to define syntax-rules macros.

### Gensyms
With lisp macros you can use gensyms they are special Scheme symbols that use JavaScript symbols
behind the sceen so they are proven to be unique. Additionaly you can use named gensym if you pass
string as first argument:

```scheme
(gensym)
;; ==> #:g5
(gensym "sym")
;; ==> #:sym
```

## Integration with JavaScript

### Dot notation
LIPS allow accessing JavaScript objects with dot notation:

```scheme
document.querySelector
;; ==> #<procedure(native)>
```
### Mutatiing object properties
You can use dot notation with `set!` to change the value:

```scheme
(set! self.foo 10)
self.foo
```

top level `self` always points to a global object `window` in browser or `global` in Node.

There is also older API that still work, which is `set-obj!` but with dot notation you don't
need it anymore:

```scheme
(set-obj! self 'foo 10)
(display self.foo)
;; ==> 10
```

In both platforms you can access global JavaScript objects like normal variables:

```scheme
(set! self.greet "hello, LIPS")
(write greet)
;; ==> "hello, LIPS"
```

### Boxing
LIPS have its own representation for numbers, strings and characters. And when
interacting with JavaScript the values may get boxed or unboxed automagically.

You should not confuse boxing with boxes ([SRFI-111](https://srfi.schemers.org/srfi-111/) and
[SRFI-195](https://srfi.schemers.org/srfi-195)). LIPS boxes are part of implementation of Scheme
data types. And SRFI boxes are containers written in Scheme. Name boxing came from JavaScript, when
primitive values are wrapped in objects when you try to use them in object context (like accessing
a property).

### Helper macros and functions
The most usefull macro in LIPS (for interacting with JavaScript) is `-->` it
acts like a chain of method calls in JavaScript

```scheme
(--> "this is string" (split " ") (reverse) (join " "))
;; ==> "string is this"
```

You can chain methods that return arrays or string and call a method of them. Above
expression is the same as JavaScript:

```javascript
"this is string".split(' ').reverse().join(' ');
```

#### Lagacy macros and functions
There are two legacy macros that are still part of LIPS, but you don't need
them anymore.

* `.` - dot function was a first way to interact with JavaScript it allowed to
  get property from an object:

```scheme
(. document 'querySelector)
```

This returned function querySelector from document object in browser.
* `..` - this is a macro is that simplify usage of `.` procedure:

```scheme
(.. document.querySelector)
```

You still sometimes may want to use this instead of `-->` when you want to get
property from an object returned by expression.

In old version of LIPS you have to execute code like this:

```scheme
((. document 'querySelector) "body")
((.. document.querySelector) "body")
```

The first expression return a Native JavaScript procedure that is then executed.

This is equivalent of:

```scheme
(document.querySelector "body")
```

### Scheme functions
Scheme functions (lambda's) are JavaScript functions so you can call them from JavaScript.

```scheme
(set! window.foo (lambda () (alert "hello")))
```

If you define function like this, in browser REPL, you can call it from JavaScript
(e.g. browser developer console).

**TODO** Screenshot

### JavaScript functions
You can call JavaScript functions from Scheme, the same as you call Scheme procedures:

```scheme
(document.querySelector "body")
;; ==> #<HTMLBodyElement>
```

In both browser and Node.js you can execute `console.log`:

```scheme
(console.log "hello, LIPS")
;; ==> hello, LIPS
```

### Callbacks

You can use Scheme functions as callbacks to JavaScript functions:

```scheme
(--> #("1" "2" "3") (map string->number))
;; ==> #(1 +nan.0 +nan.0)
```

This is classic issue with functions that accept more than one argument. You have samilar issue
in JavaScript:

```javascript
["1", "2", "3"].map(parseInt)
;; ==> [1, NaN, NaN]
```

**NOTE**: the value are differnet becaseu in Shceme i

To fix the issue you can
deifne lambda with single argument:

```scheme
(--> #(1 2 3) (map (lambda (number) (number->string number))))
;; ==> #("1" "2" "3")
```

You can also use one of functional helpers insprired by [Ramda](https://ramdajs.com/):

```scheme
(--> #(1 2 3) (map (unary number->string)))
;; ==> #("1" "2" "3")
```

The `unary` [higher-order procedure](/docs/scheme-intro/core#higher-order-functions) acept a singe
procedure and return new procedure that accept only one argumnet.

To read more check [Functional helpers](/docs/lips/functional-helpers).

**WARNING** be carful when using scheme callback functions inside JavaScript.
Since some code may be `async` and your code may break.

Example of procedures that are not wise to use are:

* `Array::forEach` - this function accepts a callaback but becasue it doesn't return
  anything, LIPS can automatically await the reponse, and you code may execute out of order.
* `String::replace` - this function can accept optional callback and if lambda is async
  you will end up with `[object Promise]` in output string:

```scheme
(--> "foo bar" (replace "foo" (lambda () (Promise.resolve "xxx"))))
"[object Promise] bar"
```

### Regular Expressions
LIPS define regular expressions it uses native JavaScript regular expressions.
At first, the syntax looked like in JavaScript. It was problematic for the parser
so you were not able to put space after `/` to distingish from divide procedure.
Later the syntax was renamed into form that start with hash `#/[0-9]/`. The same
syntax is used by [Gauche](https://practical-scheme.net/gauche/man/gauche-refe/Regular-expressions.html) implementation. But LIPS supports more flags (same as JavaScript).

### Vectors
In LIPS Scheme vectors are JavaScript arrays. So you can execute methods on them with `-->` macro:

```scheme
(--> #("one" "two" "three") (join ":"))
;; ==> "one:two:three"
```

### Object literals
In LIPS you can define object literals with `&`
[syntax extension](/docs/lips/extension#syntax-extensions):

```scheme
(define obj &(:name "Jack" :age 22))
(write obj)
;; ==> &(:name "Jack" :age 22)
(console.log obj)
;; ==> { name: 'Jack', age: 22 }
```

You can nested object literals and mix them with different object:

```scheme
(define obj &(:name "Jack" :hobbies #("swiming" "programming")))
(write obj.hobbies)
;; ==> #("swiming" "programming")
(console.log obj)
;; ==> { name: 'Jack', hobbies: [ 'swiming', 'programming' ] }
```

Object similar to Scheme vectors are immutable and everything inside is quoted.

```scheme
(define obj &(:name Jack))
(write obj)
;; ==> &(:name "Jack")
```

But to make it possible to share objects with JavaScript, native LIPS values are automatically unboxed.
So instead of symbol represention you get a JavaScript string.

You can also use quasiquote on object literals:

```scheme
(define jack (let ((name "Jack")
                   (age 22))
               `&(:name ,name :age ,age)))
(write jack)
;; ==> &(:name "Jack" :age 22)
```

### Automagic async/await
LIPS do automatic async/await so it waits for any promise before evaluating
next expression.

```scheme
(Promise.resolve "xxx")
;; ==> "xxx"
```

This simplifies code when using promises, for instance using
[fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API).

```
(--> (fetch "https://scheme.org.pl/test/") (text) (match #/<h1>([^>]+)<\/h1>/) 1)
;; ==> "Scheme is Super Fun"
```

This is equivalent of JavaScript using async/await:

```javascript
cons res = await fetch("https://scheme.org.pl/test/");
const text = await res.text();
text.match(/<h1>([^>]+)<\/h1>/)[1];
```

### Promise quotation

Sometimes you need to process promises as values, for this LIPS support quotation
of promises. You escape automagic async/await realm and get access to promise as value:
to quote a promise you use `'>`
[syntax extension](/docs/lips/extension#syntax-extensions). To again get into
automatic async/await you can use `(await)` procedure

```scheme
(let ((promise (--> '>(fetch "https://scheme.org.pl/test/")
                      (then (lambda (res)
                              (res.text)))
                      (then (lambda (text)
                              (. (text.match #/<h1>([^>]+)<\/h1>/) 1))))))
  (print (await promise)))
;; ==> Scheme is Super Fun
```

**NOTE** Inside `then` lambda promises are still automagically resolved.

```scheme
(--> '>(Promise.resolve "hello")
       (then (lambda (value)
               (print (string-append value " " (Promise.resolve "LIPS"))))))
;; ==> hello LIPS
```

### Promises vs Delay expression
Don't confuse JavaScript promises with `delay` expressions. They representation looks similar:

```scheme
(delay 10)
;; ==> #<promise - not forced>
'>(Promise.resolve 10)
;; ==> #<js-promise resolved (number)>
```

## Classes

In LIPS, you can define JavaScript classes with `define-class` macro:

```scheme
(define-class Person Object
   (constructor (lambda (self name)
                   (set! self.name name)))
   (greet (lambda (self)
            (string-append "hello, " self.name))))

(define jack (new Person "Jack"))
(write jack)
;; ==> #<Person>
(jack.greet)
;; ==> "hello, Jack"
```

`define-class` is macro written in Scheme that uses
[JavaScript prototypes](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Object_prototypes) behind the scene.

The class always need to have a base class (parent) or you need to use `null`. Classes have explicit
`self` as first argument (similar to Python) but `this` also works inside functions:

```scheme
(set! jack.run (lambda () (string-append "run, " this.name)))
(jack.run)
;; ==> "run, Jack"
```

To create new instance of a Class you can use `new` procedure.

You can also manipulate JavaScript prototypes directly:

```scheme
(write Person.prototype)
;; ==> #<prototype>
(set! Person.prototype.toString (lambda () (string-append "#<Person (" this.name ")>")))
(display (jack.toString))
;; ==> #<Person (Jack)>
```

By default toString is not used for represention of objects, but you add representation if you want.
See [Homoiconic data types](/docs/lips/extension#new-homoiconic-data-types).

## Node.js

In Node.js you can load JavaScript modules with `require`:

```scheme
(define fs (require "fs/promises"))
(let ((fname "tmp.txt"))
  (fs.writeFile fname "hello LIPS")
  (write (fs.readFile fname "utf-8")))
;; ==> "hello LIPS"
```

If you have to use callback based API in Node use
[promisify function](https://nodejs.org/api/util.html#utilpromisifyoriginal) from Module util.

You can also use Promise contructor yourself. Here is example `async` lisp macro:

```scheme
(define fs (require "fs"))

(define-macro (async expr)
  (let ((resolve (gensym "resolve"))
        (reject (gensym "reject")))
    `(new Promise (lambda (,resolve ,reject)
                    ,(append expr (list `(lambda (err data)
                                           (if (not (null? err))
                                               (,reject err)
                                               (,resolve data)))))))))

(let ((fname "tmp.txt"))
  (async (fs.writeFile fname "Hello, LIPS!"))
  (write (async (fs.readFile fname "utf-8"))))
;; ==> "Hello, LIPS!"
```
