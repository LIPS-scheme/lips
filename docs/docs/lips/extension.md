---
sidebar_position: 6
description: A way to extends LIPS syntax, not only with macros
---

# Extending LIPS

There are two ways to extend LIPS Scheme, one is through [macros](#macros) and the other ways is with
[syntax extensions](#syntax-extensions).

## Macros

LIPS allow creating Lisp macros and Scheme hygienic macros. Right now the limitations of macros is
that they are runtime.  There are no expansion time. Macros act like function calls, but they
transform the code and the interpreter evaluates the code that is returned by the macro. They ware
implemented like this, because this is how I understood the macros when they first got
implemented. There is a [plan to create proper macro
expansion](https://github.com/jcubic/lips/issues/169).

Quasiquote works with object literals, like with vectors:

```scheme
(let* ((x 10)
       (y 20)
       (obj `&(:x ,x :y ,y)))
  (print obj))
```

to define a lisp macro, you use syntax defined in [Scheme Tutorial about Macros](/docs/scheme-intro/macros).

```scheme
(define-macro (for var start end . body)
  `(for-each (lambda (,var)
               ,@body)
             (range ,start ,(+ end 1))))

(let ((result (vector)))
  (for i 10 20
       (result.push i))
  (print result))
;; ==> #(10 11 12 13 14 15 16 17 18 19 20)
```

You can define macro that create shorthand syntax like in JavaScript:

```javascript
const x = 10;
const y = 20;
const obj = { x, y };
console.log(obj);
// { x: 10, y: 20 }
```

You can create macro that will work the same in LIPS Scheme:

```scheme
(define (symbol->key symbol)
  (string->symbol (string-append ":" (symbol->string symbol))))

(define-macro (expand . args)
  `(object ,@(reduce (lambda (symbol acc)
                       (let ((key (symbol->key symbol)))
                         (append acc (list key symbol))))
                     '()
                     args)))
(let* ((x 10)
       (y 20)
       (obj (expand x y)))
  (print obj))
;; ==> &(:x 10 :y 20)
```

### Hygienic macros
LIPS define hygienic macros in form of standard `syntax-rules` expression. Note that there are know
bugs in `syntax-rules` see [issue #43 on GitHub](https://github.com/jcubic/lips/issues/43) and [unit
tests](https://github.com/jcubic/lips/blob/devel/tests/syntax.scm) that have tests marked as
failing.

If you find a case of failing macro, don't hessitate to create an issue. You can also check if your
case is not already listed on above links. You can also just create a comment on issue #43 with your
broken test case.

LIPS Scheme define those extensions to syntax-rules macros:

* [SRFI-46](https://srfi.schemers.org/srfi-46/srfi-46.html) (changing ellipsis symbol: see
  [Nested Hygienic Macros](/docs/scheme-intro/macros#nested-hygienic-macros)
* [SRFI-139](https://srfi.schemers.org/srfi-139/srfi-139.html) see
  [Syntax Parameters](/docs/scheme-intro/macros#anaphoric-hygienic-macros)
* [SRFI 147](https://srfi.schemers.org/srfi-147/srfi-147.html) allow defining new syntax-rules transformers

### Macroexpand
LIPS define `macroexpand` and `macroexpand-1` but they are macros and the expression don't need to be quoted.
There is an [issue to change those expressions into functions](https://github.com/jcubic/lips/issues/323) like
in [Common Lisp](http://clhs.lisp.se/Body/f_mexp_.htm).

## Syntax extensions

Syntax extensions are a way to add new syntax to LIPS Scheme. They are executed at parse time. Object literals and
vector literals are added using syntax extensions. Syntax extension modify the Parser and allow to add new behavior at
parse time.

To add syntax extension you use:

```scheme
(set-special! "##" 'my-function lips.specials.LITERAL)
```

The syntax extension can point to a macro or a function. When extension is a function it's invoked and the result data
is returned from the parser:

```scheme
(define (my-function number)
  `(list ,number ,number))
```

if you define the function like this and execute:

```scheme
##10
;; ==> (10 10)
```

To see the expansion of syntax extension you can use `lips.parse`:

```scheme
(lips.parse "##10")
;; ==> #((list 10 10))
```

**NOTE**: The `lips.parse` function return array/vector of parsed expressions.

There are 3 types of syntax extensions `SPLICE`, `LITERAL`, and `SYMBOL`. You define them using
constants defined in `lips.specials` object.

* `LITERAL` - used above pass it's argument as is, with literal syntax extension you can execute it
  on any argument. This is default when no constant in `set-special!` is used.
* `SPLICE` - if you execute syntax `##(1 2 3)` the arguments will be spliced, so the function or a
  macro needs to use improper list. Or use named arguments if syntax accept fixed amount of arguments.
* `SYMBOL` - this type of extensions don't accept any arguments and can be used to define parser constants.

### Splice syntax extensions

```scheme
(set-special! "##" 'complex lips.specials.SPLICE)

(define (complex real imag)
  (make-rectangular real imag))
```

This syntax extension will define complex numbers and will work only on lists:

```scheme
##(10 20)
;; ==> 10+20i
```

Since it's a macro it evaluate at parse time:

```scheme
(lips.parse "##(10 20)")
;; ==> #(10+20i)
```

With splice syntax extension you can limit the number of arguments (remember that LIPS don't check
[arity](https://en.wikipedia.org/wiki/Arity)).

```scheme
(define (complex . args)
  (if (not (= (length args) 2))
      (throw "Invalid invocation of ## syntax extension")
    (apply make-rectangular args)))
```

```scheme
(lips.parse "##(10 20)")
;; ==> #(10+20i)
(lips.parse "##(1 2 3)")
;; ==> Invalid invocation of ## syntax extension
```

### Symbol syntax extensions

The last type of syntax extensions are symbols they don't accept any arguments and can be used to
define parser constants.

```scheme
(set-special! "nil" 'nil-fn lips.specials.SYMBOL)
(define (nil-fn) '())
```

This will define constant `#nil`. It's different from `nil` variable:

```scheme
(define nil '())

(eq? nil #nil)
;; ==> #t
(eq? (car '(nil)) (car '(#nil)))
;; ==> #f
(symbol? (car '(nil)))
;; ==> #f
(symbol? (car '(#nil)))
;; ==> #f
(eq? (car '(#nil)) '())
;; ==> #t
```

### Autogensyms

With syntax extensions you can define autogensyms expressions:

```scheme
(set-special! "#:" 'keyword lips.specials.LITERAL)

(define (keyword symbol)
  `(gensym ',symbol))

(let ((x #:foo))
  (write x))
;; ==> #:foo
```

This allow to create named [gensyms](/docs/lips/intro#gensyms) that are unique:

```scheme
(eq? #:foo #:foo)
;; ==> #f
```

You can use them with lisp macros instead of `gensym` expressions. The autogensyms are actually part
of the standard library.

### String interpolation

With syntax extensions you can create string interpolation that expand into a Scheme code:

```scheme
(set-special! "$" 'interpolate)

(define (interpolate str)
  (typecheck "interpolate" str "string")
  (let* ((re #/(\$\{[^\}]+\})/)
         (parts (--> str (split re) (filter Boolean))))
    `(string-append ,@(map (lambda (part)
                             (if (not (null? (part.match re)))
                                 (let* ((expr (part.replace #/(^\$\{)|(\}$)/g ""))
                                        (port (open-input-string expr))
                                        (value (with-input-from-port port read)))
                                   `(repr ,value))
                                 part))
                           (vector->list parts)))))

(pprint (macroexpand-1 (let ((x 10)) $"x = ${(+ x 2)}")))
;; ==> (let ((x 10))
;; ==>   (string-append "x = " (repr (+ x 2))))

(let ((x 10))
  $"x = ${(+ x 2)}")
;; ==> "x = 12"
```

The limitation of this solution is that you can't use strings inside `${ ... }`. It will break the
Lexer.  In the future there may be a way to define such syntax extensions (See [Add full string
interpolation as syntax extension](https://github.com/jcubic/lips/issues/321)).

### Accessing parser
In LIPS syntax extensions you can access the parser instance, so you can implement syntax
extension that return line number:

```scheme
(set-special! "#:num" 'line-num lips.specials.SYMBOL)

(define (line-num)
  (let* ((lexer lips.__parser__.__lexer__)
         (token lexer.__token__))
    (write token)
    (newline)
    ;; line number start from 0
    (+ token.line 1)))

(print (list
        #:num
          #:num))
;; ==> &(:token "#:num" :col 8 :offset 260 :line 11)
;; ==> &(:token "#:num" :col 10 :offset 274 :line 12)
;; ==> (12 13)
```

**NOTE**: The provided output will be exactly the same, when the code will be put into a single file
and executed.

### Standard input
In syntax extensions `current-input-port` points into the parser stream. So you can implement
your own parser logic. The best way to implement custom syntax extension (that works similar to
common lips reader macros).

```scheme
(set-special! "$" 'raw-string lips.specials.SYMBOL)

(define (raw-string)
  (if (char=? (peek-char) #\")
      (begin
        (read-char)
        (let loop ((result (vector)) (char (peek-char)))
          (read-char)
          (if (char=? char #\")
              (apply string (vector->list result))
              (loop (vector-append result (vector char)) (peek-char)))))))

(print $"foo \ bar")
;; ==> "foo \\ bar"
```

This extension implements raw string, like in Python, where you don't need to escape the characters that are thread literally.
Similarly, you can implement strings that use backticks, you only need to replace `#\"` with `` #\` ``.

```scheme
(set-special! "$" 'raw-string lips.specials.SYMBOL)

(define (raw-string)
  (if (char=? (peek-char) #\`)
      (begin
        (read-char)
        (let loop ((result (vector)) (char (peek-char)))
          (read-char)
          (if (char=? char #\`)
              (apply string (vector->list result))
              (loop (vector-append result (vector char)) (peek-char)))))))

(print $`foo \ bar`)
;; ==> "foo \\ bar"
```

With this feature in hand you can implement full string interpolation (that will probably be part of
LIPS Scheme in the future).

### Limitations

The limitation of syntax extensions is that you can't define a variable that starts with the
same characters as syntax extension. This may be a benefit and not a limitation:

## New Homoiconic data types

With LIPS, you can define representation of custom data types that are the same when printed and read.

To create custom representation of new data type you can use `set-repr!` expression. It only works
with JavaScript classes.  But Scheme records in LIPS define new JavaScript class. So you can create
new records and create different representation for them.

```scheme
(define-record-type :Person
  (make-person name age)
  person?
  (name person-name set-name!)
  (age person-age set-age!))

(set-repr! :Person (lambda (obj quot)
                     (string-append "(make-person "
                                    (repr (person-name obj) quot)
                                    " "
                                    (repr (person-age obj) quot)
                                    ")")))

(write (make-person "Mick Jagger" 80))
;; ==> (make-person "Mick Jagger" 80)
(display (make-person "Mick Jagger" 80))
;; ==> (make-person Mick Jagger 80)
```

As you can see the `display` don't quote the strings because of `repr` expression that use `quot`
argument to the `set-repr!` handler.

### Combining with syntax extensions

You can combine syntax extensions with custom representation:

```scheme
(set-special! ":P" 'make-person lips.specials.SPLICE)

(set-repr! :Person (lambda (obj quot)
                     (string-append ":P("
                                    (repr (person-name obj) quot)
                                    " "
                                    (repr (person-age obj) quot)
                                    ")")))

(write :P("Mick Jagger" 80))
;; ==> :P("Mick Jagger" 80)
```
