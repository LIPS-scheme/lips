---
sidebar_position: 1
description: Core LIPS features added on top of Scheme, related to JavaScript
---

# Core features

## Special constants
LIPS define `#null` and `#void` as Parser constants so they can be used inside quoted expressions:

```scheme
(let ((lst '(#null #void)))
  (write (symbol? (car lst)))
  (newline)
  (write (symbol? (cadr lst)))
  (newline))
;; ==> #f
;; ==> #f
```

**NOTE** `#null` is the same as JavaScript `null` value and it's a false value. Similar to
[Kawa Scheme](https://www.gnu.org/software/kawa/index.html) that use `#!null`.

`#void` constants is the same as result of `(value)` or `(if #f #f)`. In Scheme it's unspecified value,
but in LIPS it's JavaScript undefined. `#void` is not false value.

```scheme
(eq? (if #f #f) (values))
;; ==> #t
```

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
LIPS fully supports all Unicode characters, including emoji:

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
* [SRFI-46](https://srfi.schemers.org/srfi-46/) which allows changing the ellipsis symbol for nested syntax-rules.
* [SRFI-139](https://srfi.schemers.org/srfi-139/) which allows defining
  [anaphoric syntax-rules macros](/docs/scheme-intro/macros#anaphoric-hygienic-macros).
* [SRFI-147](https://srfi.schemers.org/srfi-147/) which allows defining a new syntax-rules macros to define syntax-rules macros.

### Gensyms
With lisp macros you can use [gensyms](/docs/scheme-intro/macros#gensyms), they are special Scheme
symbols that use JavaScript symbols behind the scene, so they are proven to be unique. Additionally
you can use named gensym if you pass string as first argument:

```scheme
(gensym)
;; ==> #:g5
(gensym "sym")
;; ==> #:sym
```

## Single argument eval

Eval in LIPS don't require second argument to `eval`. The environment is optional and default
it's a result of calling `(interaction-environment)`.

```scheme
(define x 10)
(eval '(+ x x))
;; ==> 20
```

```scheme
(define x 10)
(let ((x 20))
  (eval '(- x)))
;; ==> -10
```

But you can also use the second arguments:

```scheme
(define x 10)
(let ((x 20))
  (eval '(- x) (current-environment)))
;; ==> -20
```

Read more about [LIPS environments](/docs/lips/environments).

## Procedures
Procedures in LIPS have access additional objects `arguments`, but the have nothing to do with JavaScript.
arguments is an array/vector with calling arguments and it have an object callee which points to the same
procedure. So you can create recursive functions with anonymous lambda:

```scheme
((lambda (n)
   (if (<= n 0)
       1
       (* n (arguments.callee (- n 1))))) 10)
;; ==> 3628800
```

This is classic factorial function written as lambda without the name.

### length property

LIPS functions similarly to JavaScript functions also have `length` property that indicate how many
arguments a function accepts. If function get more or less argumenets it's not an error like in Scheme. More arguments are ignored,
and if less arguments are passed they are `undefined` (`#void`).

```scheme
(define (sum a b c)
  (+ a b c))

(print sum.length)
;; ==> 3
```

It return number of number arguments the rest (dot notation) arguments are ignored.

```scheme
(define (sum a b . rest)
  (apply + a b rest))

(print sum.length)
;; ==> 3
(sum 1 2 3 4 5 6)
;; ==> 21
```

## Doc strings
Procedures, macros, and variables can have doc strings.

```scheme
(define (factorial n)
  "(factorial n)

   Calculate factorial of a given number"
  (if (<= n 0)
       1
       (* n (factorial (- n 1)))))
```

You can access doc string with `help` procedure or with `__doc__` property.

```scheme
(write factorial.__doc__)
"(factorial n)

Calculate factorial of a given number"
```

If you define variable or hygienic macro with doc string, the string is hidden (you can access it with `__doc__`),
so help is the only way to access it:

```scheme
(define-syntax q
  (syntax-rules ()
    ((_ x) 'x))
  "(q expression)

   Macro quote the expression")

(write q.__doc__)
;; ==> #void
(help q)
;; ==> (q expression)
;; ==>
;; ==> Macro quote the expression
```

## Typechecking

LIPS do typechecking for all scheme procedures.

```scheme
(+ "hello" 10)
;; ==> Expecting number got string
```

You can incorporate typechecking in your own code:

```scheme
(let ((x 10))
  (typecheck "let" x "string" 0))
;; ==> Expecting string got number in expression `let` (argument 0)
(let ((x "string"))
  (typecheck "let" x "number"))
;; ==> Expecting number got string in expression `let`
```

There is also another function to check type of number:

```scheme
(let ((i 10+10i))
  (typecheck-number "let" i "bigint"))
;; ==> Expecting bigint got complex in expression `let`
```

**NOTE**: In LIPS all integers are BigInts.

The last typecking function is `typecheck-args` that check if all arguments are of same type.

```scheme
(let ((number '(1 10 1/2 10+10i)))
  (typecheck-args "number" "let" number))
;; ==> #void
(let ((number '(1 10 1/2 "string")))
  (typecheck-args "number" "let" number))
;; ==>  Expecting number got string in expression `let` (argument 4)
```

## Integration with JavaScript

### Dot notation
LIPS allow accessing JavaScript objects with dot notation:

```scheme
document.querySelector
;; ==> #<procedure(native)>
```
### Mutating object properties
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

### Date and Time

Since we have full access to JavaScript, we can access the `Date` object to manipulate date and time.

```scheme
(--> (new Date "2024-01-01 12:09:2")
     (getFullYear))
;; ==> 2024
```

```scheme
(define (format-part number)
  "(format-part number)

   Convert number to string with leading zero. It should be used
   for minutes, hours, and seconds."
  (--> number (toString) (padStart 2 "0")))

(let ((date (new Date "2024-01-01 12:09:02")))
  (format "~a:~a:~a"
          (format-part (date.getHours))
          (format-part (date.getMinutes))
          (format-part (date.getSeconds))))
;; ==> "12:09:02"
```

You can also use Date time libraries like [date-fns](https://date-fns.org/).

### Interact with DOM

Here is example how to add button to the page and add onclick handler using browser DOM
([Document Object Model](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)) API.

```scheme
(let ((button (document.createElement "button")))
  (set! button.innerHTML "click <strong>me</strong>!")
  (set! button.onclick (lambda () (alert "Hello, LIPS Scheme!")))
  (let ((style button.style))
    (set! style.position "absolute")
    (set! style.zIndex 9999)
    (set! style.top 0)
    (set! style.left 0))
  (document.body.appendChild button))
```

### Boxing
LIPS have its own representation for numbers, strings and characters. And when
interacting with JavaScript the values may get boxed or unboxed automagically.

You should not confuse boxing with boxes ([SRFI-111](https://srfi.schemers.org/srfi-111/) and
[SRFI-195](https://srfi.schemers.org/srfi-195)). LIPS boxing are part of implementation of Scheme
data types. And SRFI boxes are containers written in Scheme. Name boxing came from JavaScript, when
primitive values are wrapped in objects when you try to use them in object context (like accessing
a property).

You need to be careful with some of the JavaScript native methods, since they can unbox the value when you don't
when them to be unboxed.

Example is `Array::push` using with native LIPS types:

```scheme
(let ((v (vector)))
  (v.push 1/2)
  (print v))
;; ==> #(0.5)
```
As you can see the rational number got unboxed and converted into JavaScript float numbers.
Unboxing always can make you loose some information because LIPS types needs to be converted into native JavaScript
data types. And JavaScript doesn't have a notion of rationals, there are only floating point numbers, and big ints.

### Procedures
LIPS Scheme procedures are JavaScript functions, so you can call them from JavaScript.

```scheme
(set! self.greet (lambda () "hello, LIPS"))
```

You can call this function from JavaScript

```javascript
console.log(greet());
// ==> {__string__: 'hello, LIPS'}
```

Note that the value was not automagically unboxed because we are no longer in LIPS Scheme code and LIPS can't access native
JavaScript. So to get the real a string you need to call `valueoOf()`:

```javascript
console.log(greet().valueOf());
// ==> hello, LIPS
```

#### Procedure arity
LIPS don't check the number of argumnents when calling a procedure:

```scheme
(let ((test (lambda (a b c)
              (print a b c))))
  (test 10))
;; ==> 10
;; ==> #void
;; ==> #void
```

The same as with JavaScript if you don't pass an argument it will be undefined. But you still have full compatible with Scheme and use [arguments with variable artity](/docs/scheme-intro/core#variable-number-of-arguments):

```scheme
(let ((test (lambda (first . rest)
              (apply print first rest))))
  (test 1)
  (test 2 3 4))
;; ==> 1
;; ==> 2
;; ==> 3
;; ==> 4
```

### Helper macros and functions
The most useful macro in LIPS (for interacting with JavaScript) is `-->` it acts like a chain of
method calls in JavaScript

```scheme
(--> "this is string" (split " ") (reverse) (join " "))
;; ==> "string is this"
```

You can chain methods that return arrays or string and call a method of them. The above expression
is the same as JavaScript:

```javascript
"this is string".split(' ').reverse().join(' ');
```

With --> you can also gab property of a function:

```scheme
(--> #/x/ (test.call #/foo/ "foo"))
;; ==> #t
(let ((test-bar (--> #/x/ (test.bind #/bar/i))))
  (test-bar "BAR"))
;; ==> #t
```

You can also return a function:

```scheme
(define test (--> #/x/ test))
(test.call #/foo/ "foo")
;; ==> #t
```

Read more about [function::bind](https://tinyurl.com/ykvb836s) and
[function::call](https://tinyurl.com/yc6j7fdh) on [MDN](https://developer.mozilla.org/en-US/).

#### Legacy macros and functions
There are two legacy macros that are still part of LIPS, but you don't need
them most of the time.

* `.` - dot function was a first way to interact with JavaScript, it allowed to
  get property from an object:

```scheme
(. document 'querySelector)
```

This returned function querySelector from document object in browser. Note that dot a function can only appear
as first element of the list (it's handled in special way by the parser). In any other place dot is a pair separator,
see documentation about [Pairs in Scheme](/docs/scheme-intro/data-types#pairs).

* `..` - this is a macro is that simplify usage of `.` procedure:

```scheme
(.. document.querySelector)
```

You still sometimes may want to use this instead of `-->` when you want to get
property from an object returned by expression.

In the old version of LIPS, you have to execute code like this:

```scheme
((. document 'querySelector) "body")
((.. document.querySelector) "body")
```

The first expression return a Native JavaScript procedure that is then executed.

This is equivalent of:

```scheme
(document.querySelector "body")
```

**NOTE** the only time when you still need `.` function is when you want to get the property of
object returned by expression.

```scheme
(let ((style (. (document.querySelector "body") 'style)))
  (set! style.background "red"))
```

Here we get a [style object](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/style)
from [the DOM node](https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement) without sorting the
reference to the DOM node.

**NOTE** because dot notation in symbols is not special syntax you can use code like this:

```scheme
(let ((x #(1 2 3)))
  (print x.0)
  (print x.1)
  (print x.2))
;; ==> 1
;; ==> 2
;; ==> 3
```

### Scheme functions
Scheme functions (lambda's) are JavaScript functions, so you can call them from JavaScript.

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
// ==> [1, NaN, NaN]
```

**NOTE**: the value are different becaseu in Shceme i

To fix the issue you can
define lambda with single argument:

```scheme
(--> #("1" "2" "3") (map (lambda (str) (string->number str))))
;; ==> #(1 2 3)
```

You can also use one of functional helpers insprired by [Ramda](https://ramdajs.com/):

```scheme
(--> #("1" "2" "3") (map (unary string->number)))
;; ==> #(1 2 3)
```

The `unary` [higher-order procedure](/docs/scheme-intro/core#higher-order-functions) accept a single
procedure and return new procedure that accept only one argument.

To read more check [Functional helpers](/docs/lips/functional-helpers).

**WARNING** be careful when using scheme callback functions inside JavaScript.
Since some code may be `async` and your code may break.

Example of procedures that are not wise to use are:

* `Array::forEach` - this function accepts a callaback but because it doesn't return
  anything, LIPS can't automatically await the response, and your code may execute out of order.
* `String::replace` - this function can accept optional callback and if `lambda` is async
  you will end up with `[object Promise]` in output string. Any macro or function can return
  a promise in LIPS, and if any of the expression inside a function return a Promise, the whole
  function return a Promise and become async. Here is example code that demonstrate the problem:

```scheme
(--> "foo bar" (replace "foo" (lambda () (Promise.resolve "xxx"))))
"[object Promise] bar"
```

Instead of `Array::replace` you should use LIPS Scheme `replace` procedure that works with async `lambda`:

```scheme
(replace #/[a-z]+/g (lambda ()
                      (Promise.resolve "lips"))
         "foo bar")
;; ==> "lips lips"
```

### Regular Expressions
LIPS define regular expressions it uses native JavaScript regular expressions.
At first, the syntax looked like in JavaScript. It was problematic for the parser
so you were not able to put space after `/` to distinguish from divide procedure.
Later, the syntax was renamed into form that start with hash `#/[0-9]/`. The same
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

You can nest object literals and mix them with different object:

```scheme
(define obj &(:name "Jack" :hobbies #("swimming" "programming")))
(write obj.hobbies)
;; ==> #("swimming" "programming")
(console.log obj)
;; ==> { name: 'Jack', hobbies: [ 'swiming', 'programming' ] }
```

Object similar to Scheme vectors, are immutable, and everything inside is quoted automatically:

```scheme
(define obj &(:name Jack))
(write obj)
;; ==> &(:name "Jack")
```

But to make it possible to share objects with JavaScript, native LIPS values are automatically unboxed.
So instead of symbol representation you get a JavaScript string.

You can also use quasiquote with object literals:

```scheme
(define jack (let ((name "Jack")
                   (age 22))
               `&(:name ,name :age ,age)))
(write jack)
;; ==> &(:name "Jack" :age 22)
```

**NOTE**: because of the construction of [syntax extensions](/docs/lips/extension#syntax-extensions) and
[quasiquote](/docs/scheme-intro/data-types#quasiquote), you can't splice a list inside object literals:

```scheme
(let ((args (list ':foo "lorem" ':bar "ipsum")))
  `&(,@args))
;; ==> pair (unquote-splicing args) is not a symbol!
```

The reason why this work like this is because, syntax extensions (`&`) runs at parse time and LIPS macros are runtime.
This may change in the future when [expansion time will be implemented](https://github.com/jcubic/lips/issues/169).

Objects also have longhand form with `object` macro:

```scheme
(let ((obj (object :name 'Jack)))
  (write obj))
;; ==> &(:name "Jack")
```

But note that object macro is async (return a Promise) so it may be problematic when used it
with native JavaScript code.

Using long form `(object)` syntax you can use splicing with help of `eval`:

```scheme
(let ((args '(:foo "lorem" :bar "ipsum")))
   (eval `(object ,@args)))
;; ==> &(:foo "lorem" :bar "ipsum")
```

The same you can use macros that will return LIPS Scheme code:

```scheme
(define-macro (create-object . args)
  `(object ,@args))

(create-object :foo "lorem" :bar "ipsum")
;; ==> &(:foo "lorem" :bar "ipsum")
```

**NOTE**: this example macro works the same `object` is it's not that useful, but you can create
more complex code where you will be able to generate object literals with splicing.

Object literal also have shorthad notation:

```scheme
(let ((obj &(:x :y)))
  (write obj))
;; ==> &(:x #void :y #void)
```

It creates two writtable slots, the rest of the props are read only:

```scheme
(let ((obj &(:x :y)))
  (set! obj.x 10)
  (set! obj.y 20)
  (write obj))
;; ==> &(:x 10 :y 20)

(let ((obj &(:x :y)))
  (set! obj.z 20)
  (write obj))
;; ==> Cannot add property z, object is not extensible

(let ((obj &(:x :y :z 10)))
  (set! obj.z 20)
  (write obj))
;; ==> Cannot assign to read only property 'z' of object '#<Object>'
```

### Automagic async/await
LIPS do automatic async/await so it waits for any promise before evaluating
next expression.

```scheme
(Promise.resolve "xxx")
;; ==> "xxx"
```

This simplifies code when using promises, for instance using
[fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) (AJAX).

```scheme
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

### Promises vs delay expression
Don't confuse JavaScript promises with `delay` expressions. Their representation looks similar:

```scheme
(delay 10)
;; ==> #<promise - not forced>
'>(Promise.resolve 10)
;; ==> #<js-promise resolved (number)>
```

You can check if a value is a promise by quoting the expression and using `promise?` predicate:

```scheme
(let ((a '>10)
      (b '>(Promise.resolve 10)))
  (print (promise? a))
  (print (promise? b)))
;; ==> #f
;; ==> #t
```

### Exceptions
LIPS Scheme use javascript exception system. To throw an exception you use:

```scheme
(throw "This is error")
;; ==> Error: This is error
```

or

```scheme
(raise (new Error "error"))
```

The `raise` procedure throw any object and `throw` wraps the argument in `new Error`.

You can catch exceptions with LIPS specific try..catch..finally:

```scheme
(try
 (throw "nasty")
 (catch (e)
        (print (string-append "error " e.message " was caught"))))
;; ==> error nasty was caught
```

You can also have finally expression:

```scheme
(try
 (throw "nasty")
 (catch (e)
        (print (string-append "error " e.message " was caught")))
 (finally
  (print "nothing happened")))
;; ==> error nasty was caught
;; ==> nothing happened
```

You can also define `finally` without `catch`:

```scheme
(try
 (throw "nasty")
 (finally
  (print "after error")))
;; ==> after error
;; ==> nasty
```

**NOTE** the order of execution is not expected, but it may change in the future.

LIPS also define R<sup>7</sup>RS guard `procedure` that is just a macro that use try..catch behind the scene:

```scheme
(guard (e ((list? e) (print (string-append "Error: " (car e)))))
       (raise '("error")))
;; ==> Error: error
```

### JavaScript Generars and iterators
Right now there is no way to define JavaScript generators inside LIPS. You can create iterator using
[iteration prorocol](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols),
But to have yield keyword you need [continuations](/docs/scheme-intro/continuations), they are part of the
LIPS Roadmap.

Here is example of creating iterator in LIPS:

```scheme
(let ((obj (object))
      (max 5))
  (set-obj! obj Symbol.iterator
            (lambda ()
              (let ((i 0))
                `&(:next ,(lambda ()
                            (set! i (+ i 1))
                            (if (> i max)
                                `&(:done #t)
                                `&(:done #f :value ,(/ 1 i))))))))
  (print (iterator->array obj))
  (print (Array.from obj)))
;; ==> #(1 1/2 1/3 1/4 1/5)
;; ==> #(1 1/2 1/3 1/4 1/5)
```

`Array.from` can't be used for every possible case because it will unbox the values (and convert
rational to float), here it doesn't happen because LIPS don't treat JavaScript iterators in any
special way (it may change in the future). But `Array.from` will convert the array of rationals to
float if used on normal vector:

```scheme
(Array.from #(1/2 1/3 1/4 1/5))
;; ==> #(0.5 0.3333333333333333 0.25 0.2)
```

**NOTE**: be careful when using iterator protocol because any function side Scheme can return a promise. If you would change
quoted object literal `` `&() `` with longhand `object` you will get an error because `object` is async.

You can abstract the use of iteration protocol with a macro, but to have real `yield` keyword like
syntax you need `call/cc`.

You can also define generators inside JavaScript using `self.eval` (JavaScript global `eval`):

```scheme
(define gen (self.eval "(async function* gen(time, ...args) {
                          function delay(time) {
                            return new Promise((resolve) => {
                              setTimeout(resolve, time);
                            });
                          }
                          for (let x of args) {
                            await delay(time);
                            yield x;
                          }
                        })"))

(iterator->array (gen 100 1 2 3 4 5))
;; ==> #(1 2 3 4 5)
```

Here is example of async generator written in JavaScript.

### Classes

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

To create the new instance of a Class, you can use `new` procedure.

You can also manipulate JavaScript prototypes directly:

```scheme
(write Person.prototype)
;; ==> #<prototype>
(set! Person.prototype.toString (lambda () (string-append "#<Person (" this.name ")>")))
(display (jack.toString))
;; ==> #<Person (Jack)>
```

By default toString is not used for representation of objects, but you add representation if you want.
See [Homoiconic data types](/docs/lips/extension#new-homoiconic-data-types).

### Node.js

In Node.js, you can load JavaScript modules with `require`:

```scheme
(define fs (require "fs/promises"))
(let ((fname "tmp.txt"))
  (fs.writeFile fname "hello LIPS")
  (write (fs.readFile fname "utf-8")))
;; ==> "hello LIPS"
```

In above code, you can see example of [automagic async/await](#automagic-asyncawait).

If you have to use callback based API in Node, use
[promisify function](https://nodejs.org/api/util.html#utilpromisifyoriginal) from Module util.

You can also use the `Promise` constructor yourself:

```scheme
(define fs (require "fs"))

(define-macro (async expr)
  (let ((resolve (gensym "resolve"))
        (reject (gensym "reject")))
    `(new Promise (lambda (,resolve ,reject)
                    ,(append expr (list `(lambda (err data)
                                           ;; Node.js error is null when no error
                                           (if err
                                               (,reject err)
                                               (,resolve data)))))))))

(let ((fname "tmp.txt"))
  (async (fs.writeFile fname "Hello, LIPS!"))
  (write (async (fs.readFile fname "utf-8"))))
;; ==> "Hello, LIPS!"
```

In the above example, we import a regular callback based fs module and use the `Promise` constructor
abstracted away with a [lisp macro](/docs/scheme-intro/macros#lisp-macros).

### Finding LIPS Scheme directory

With help from `(require.resolve)` you can get the path of the root directory of LIPS Scheme:

```scheme
(--> (require.resolve "@jcubic/lips") (replace #/dist\/[^\/]+$/ ""))
```

Node.js REPL load lips from Common.jS file and `require.resolve` returns path to file
`dist/lips.cjs`, by removing with with String::replace and regular expression you can the real path
to the root of the LIPS Scheme.

## Binary compiler

LIPS Scheme have dumb binary compiler. The compiler is a way to compress the LIPS Scheme code and
create binary file that is faster to load. Compiler is use to make bootstrapping faster. The binary
file use [CBOR](https://en.wikipedia.org/wiki/CBOR) serialization format that is then compressed
with [LZJB](https://en.wikipedia.org/wiki/LZJB) algorithm that is pretty fast. And it can still be
compress further with gzip by the HTTP server.

To compile/compress a file you can use `-c` flag when executing `lips` executable.

```bash
$ lips -c file.scm
```

You can then execute the code with:

```bash
$ lips -c file.xcb
```

Will create `file.xcb` in same directory. For smaller files it make not have a difference when
loading `.xcb` or `.scm` files.

**NOTE**: directives `#!fold-case` and `#!no-fold-case` work only inside the parser and they are treated
as comments, so you can't compile the code that have those directives.

## Limitations

LISP Scheme currently don't support [continuations](/docs/scheme-intro/continuations) and [Tail Call
Optimization](/docs/scheme-intro/core#tail-call-optimization).  But they are part of the roadmap for
version 1.0.
