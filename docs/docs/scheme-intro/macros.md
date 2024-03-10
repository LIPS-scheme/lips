---
sidebar_position: 5
description: Macros are the most powerful feature of Lisp and Scheme
---

# Macros

Macros are the most powerful feature of the language (both lisp and Scheme).  Macros allows you to add
new syntax to the language, it also allows making simpler code and reduce
[boilerplate](https://en.wikipedia.org/wiki/Boilerplate_code).

Macros works like a function, but the arguments of the macro are not evaluated before passing the
value to the function. But instead the code of the arguments are passed to the macro, the macro then
can manipulate the code and return new code that is then evaluated. This happens during expansion
time before evaluation even happen.

## Lisp Macros
Some scheme implementation supports lisp macros, so I will describe them first.

If you have code like this

```scheme
(foo (+ 1 2))
```

And `foo` is a function, the `(+ 1 2)` will be evaluated and `3` will be passed to the function. But if
`foo` is a macro, the data structure `(+ 1 2)` will be passed to the macro.

To define a macro, you use usually use `define-macro` syntax. Lisp macros in Scheme are not standard,
so the syntax may change depending on
[Scheme implementation](/docs/scheme-intro/what-is-lisp#scheme-implementations).

```scheme
(define-macro (foo expr)
  (case (car expr)
    ((+) (set-car! expr '-))
    ((-) (set-car! expr '+)))
  expr)
```

This macro swap first element of the expression passed as argument. If you pass sum of two numbers
It will subtract the values:

```scheme
(foo (+ 1 2))
;; ==> -1
```

if you use minus symbol it will add up the numbers:

```scheme
(foo (- 1 2))
;; ==> 3
```

Instead of modification of existing code you can also create new list:

```scheme
(define-macro (foo expr)
  (list (case (car expr) ((+) '-) ((-) '+))
        (cadr expr)
        (caddr expr)))
```

It will work similarly but only with two numbers:

```scheme
(foo (- 1 2))
;; ==> 3
(foo (+ 1 2))
;; ==> -1
```

### Macroexpand

Some scheme implementation have function `macroexpand` that allow to inspect the result expression
returned by the macro.

```scheme
(macroexpand '(foo (+ 1 2)))
;; ==> (- 1 2)
```

Macros can be nested, so one expression can expand into something you don't expect to see. For this
you have a function called `macroexpand-1` that should expand macro one time. Which in turn should
expand just your macro.

### New Control Flow Constructs

With macros, you can define new control flow (e.g. like `if` statements). Here is an example of
`when` macro that is part of R7RS standard.

```scheme
(define-macro (when test . body)
  `(if ,test
       (begin
         ,@body)))

(let ((x 0))
  (when (zero? x)
    (display "x")
    (display " = ")
    (display "zero")
    (newline)))
;; ==> x = zero
```

As you probably already know to use multiple expressions in
[if statement](/docs/scheme-intro/core#conditionals) you need to use `begin`. The macro use
[quasiquote syntax](/docs/scheme-intro/data-types#quasiquote).

You can use `macroexpand` to see what will be the output of the expression:

```scheme
(macroexpand (let ((x 0))
               (when (zero? x)
                 (display "zero")
                 (newline))))
;; ==> (let ((x 0))
;; ==>   (if (zero? x)
;; ==>       (begin
;; ==>         (display "zero")
;; ==>         (newline))))
```

### Gensyms

If you create lisp macros you often may end up with expansion and user code to collide and use the
same variables. You call this accidental capture of identifiers.

```scheme
(define-macro (when expr body)
  `(let ((tmp ,expr))
     (if tmp
         (begin
           ,@body))))
```

If you define macro like this the user of your macro may want to use `tmp` variable and the code
will give unexpected behavior:

```scheme
(let ((tmp 1000))
  (when (> tmp 0)
    (display tmp)
    (newline)))
;; ==> #t
```

This will print `#t` but you expect it to print `1000`. This problem can be solved with special kind
of symbols called `gensyms`. Each gensym is a unique symbol.

```scheme
(define-macro (when expr body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,expr))
       (if ,tmp
           (begin
             ,@body)))))
```

Notice that let that call `gensym` is outside quasiquote so it will be evaluated when macro is
executed by the output code will have a unique symbol instead of hard coded symbol `tmp`.

If you try to evaluate the macro, you will get proper results:

```scheme
(let ((tmp 1000))
  (when (> tmp 0)
    (display tmp)
    (newline)))
;; ==> 1000
```

### Anaphoric Macros

Anaphoric macros are special kind of macros that leverage the leaking of internal data outside
the macro. This is called intentional capture of identifiers. They often expose one or more variable
that can be used by the users of the macro.

Example of such macro is `aif`:

```scheme
(define-macro (aif test true false)
  `(let ((it ,test))
     (if it
         ,true
         ,false)))
```

This macro uses `it` variable to hold the testing value that can be used inside user code:

```scheme
(let ((alist '((a . 10) (b . 20) (c . 30))))
  (aif (assoc 'a alist)
       (begin
         (display (cdr it))
         (newline))))
;; ==> 10
```

If you only have one branch like in above code you can define `awhen` macro:

```scheme
(define-macro (awhen test . body)
  `(let ((it ,test))
     (if it
         (begin
           ,@body))))

(let ((alist '((a . 10) (b . 20) (c . 30))))
  (awhen (assoc 'a alist)
    (display (cdr it))
    (newline)))
;; ==> 10
```

## Scheme Hygienic Macros

The problem with Lisp macros is that they are not hygienic. But what it means?

### Hygiene

If macro is hygienic, it means that it guaranty no leaking of internal code outside of macro. In
other words guaranteed not to cause the accidental capture of identifiers. Scheme standard define
new macro system called `syntax-rules` that is hygienic.

But we have `gensym` is this not enough to make the macros safe? No

Here is an example implementation of `unless` macro that is part of Scheme that fails because it's
not hygienic.

```scheme
(define-macro (unless test . body)
  `(if (not ,test)
       (begin
         ,@body)))
```

But in Scheme you can define a variable named not and completely break the macro:

```scheme
(let ((not (lambda (x) x)))
  (unless #f
    (display "this should not run")
    (newline)))
;; ==> this should not run
```

This will print the expression because the unless macro uses not procedure that got overwritten by the
user code. Hygiene of macros means that something like this can't happen.

### Syntax-rules

The `syntax-rules` in Scheme is different type of macros than lisp macros. It uses a special pattern
matching language. Syntax-rules is guarantee by the sec to be hygienic.

Here is the simple definition of a hygienic macro in Scheme:

```scheme
(define-syntax unless
  (syntax-rules ()
    ((_ test body ...)
     (if (not test)
         (begin
           body ...)))))
```

This macro is hygienic. If you use same test as before:

```scheme
(let ((not (lambda (x) x)))
  (unless #t
    (display "this should not run")
    (newline)))
```

It will not print any value.

### Syntax-rules pattern language

Syntax rules macro is defined like this:

```scheme
(define-syntax foo
  (syntax-rules ()
    ((name <pattern>) <expansion>)
    ((name <different pattern>) <different expansion>)))
```

The first element if the macro is a list of identifiers that can be used in the pattern.

```scheme
(define-syntax for
  (syntax-rules (in)
    ((for element in list body ...)
     (for-each (lambda (element)
            body ...)
          list))))
```

This is an example of a `for` macro that have `in` special keyword inside the parentheses. This
macro can be used like this:

```scheme
(for i in '(1 2 3 4)
  (display i)
  (newline))
;; ==> 1
;; ==> 2
;; ==> 3
;; ==> 4
```

If you try to overwrite the `in` symbol with variable:

```scheme
(let ((in #t))
  (for i in '(1 2 3 4)
    (display i)
    (newline)))
;; ==> syntax-rules: no matching syntax in macro
```

You will get an error because in is no longer a special identifier. It's now a variable.

The rest are the list of pattern and expansion. You can build a shape of the code your macro accept
and use part of the pattern in output macro.

the first element of the pattern is often `_` it matches against the name of the macro.

### Elipsis

In lisp macros if you wanted to define a list of any values (including no values) you use
[improper list](/docs/scheme-intro/data-types#improper-list) (list with dot). In syntax-rules
pattern you use an ellipsis to indicate a list of items. The ellipsis is after the symbol.

Example of usage of ellipsis:

```scheme
(define-syntax values
  (syntax-rules ()
    ((_ ((a . b) ...))
     '(a ...))))
```

This macro use an alist as a pattern and only return the values. Note that it doesn't work on a
variable that hold the alist only for alist defined inside the code:

```scheme
(values ((foo . "lorem") (bar . "ipsum") (baz . "dolor")))
;; ==> (foo bar baz)
```

### Nested Hygienic Macros
There are two ways to defined nested macros, macros that define macros. One is escape of ellipsis
with `(... ...)` syntax.

```scheme
(define-syntax define-for
  (syntax-rules ()
    ((_ symbol)
     (define-syntax symbol
       (syntax-rules ()
         ((_ (var start end) body (... ...))
          (let loop ((var start))
            (if (<= var end)
                (begin
                  body (... ...)
                  (loop (+ var 1)))))))))))
```

This macro defines macros that act like for loop, but using tail recursive, named let. You can use this macro like this:

```scheme
(define-for loop)

(begin
  (loop (i 1 10)
        (display i)
        (if (< i 10)
            (display " ")))
  (newline))
;; ==> 1 2 3 4 5 6 7 8 9 10
```

Another way to define nested marcros is using
[SRFI-46](https://srfi.schemers.org/srfi-46/srfi-46.html) syntax, which allow to change the symbol of ellipsis:

```scheme
(define-syntax define-for
  (syntax-rules ()
    ((_ symbol)
     (define-syntax symbol
       (syntax-rules ::: ()
         ((_ (var start end) body :::)
          (let loop ((var start))
            (if (<= var end)
                (begin
                  body :::
                  (loop (+ var 1)))))))))))
```

The macro works exactly the same as previous one:

```scheme
(define-for loop)

(begin
  (loop (i 1 10)
        (display i)
        (if (< i 10)
            (display " ")))
  (newline))
;; ==> 1 2 3 4 5 6 7 8 9 10
```

### Identifiers
Inside macros you can add identifers can can be used like keywords from other programming langauges. They match only
if literal symbol was used and it was not shadowed (overwritten) by variable with same name.

```scheme
(define-syntax for
  (syntax-rules (==>)
     ((_ (var start ==> end) body ...)
      (let loop ((var start))
         (if (<= var end)
             (begin
                body ...
                (loop (+ var 1))))))))
```

This for macro define symbol `==>` that can be used as part of the syntax:

```scheme
(let ((start 1)
      (end 10))
  (for (i start ==> end)
     (display i)
     (if (< i end)
         (display " ")))
  (newline))
;; ==> 1 2 3 4 5 6 7 8 9 10
```

If the symbol `==>` is shadowed by local variable the macro will not match and give an error:

```scheme
(let ((start 1)
      (end 10)
      (==> "this will not work"))
  (for (i start ==> end)
     (display i)
     (if (< i end)
         (display " ")))
  (newline))
;; ==> syntax-rules: no matching syntax in macro
```

special keywords (created with identifiers) can be optional:

```scheme
(define-syntax for
  (syntax-rules (==>)
     ((_ (var start end) body ...)
      (_ (var start ==> end) body ...))
     ((_ (var start ==> end) body ...)
      (let loop ((var start))
         (if (<= var end)
             (begin
                body ...
                (loop (+ var 1))))))))
```

This is recursive `syntax-rules` that when using without `==>` symbol it just add it between `start`
and `end`.

### Anaphoric Hygienic Macros
By default Scheme `syntax-rules` macros don't allow creating anaphoric macros like lisp macro do.
But with [SRFI-139](https://srfi.schemers.org/srfi-139/srfi-139.html) you can implement such macros.

**Note**: that not every scheme implementation support this <abbr title="Scheme Request For
Implementation">SRFI</abbr>.

Here is example of `awhen` anaphoric macro that use this <abbr title="Scheme Request For
Implementation">SRFI</abbr>:

```scheme
(define-syntax-parameter it (syntax-rules () ((_) (syntax-error "Use outside aif"))))

(define-syntax awhen
  (syntax-rules ()
    ((_ test body ...)
     (let ((tmp test))
       (syntax-parameterize
        ((it (syntax-rules ()
               ((_) tmp))))
        (if tmp
            (begin
              body ...)))))))
```

The `syntax-paremetirize` works similar to
[parameters from R<sup>7</sup>RS](/docs/scheme-intro/core#dynamic-variables).

You can use this macro like this:

```scheme
(let ((alist '((foo . "lorem") (bar . "ipsum") (baz . "dolor"))))
  (awhen (assoc 'bar alist)
    (write (cdr (it)))
    (newline)))
;; ==> "ipsum"
```

Note the difference, the parameter needs to be wrapped by parentheses like a procedure/macro call.
