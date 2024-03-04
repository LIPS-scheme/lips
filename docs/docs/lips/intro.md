---
sidebar_position: 1
---

# Core features

## Macros
LIPS define both Lisp macros and Scheme hygienic macros (`syntax-rules`).

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
interacting with JavaScript the values may get boxed or unboxed automatigally.

### Macros
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

### Callbacks

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
