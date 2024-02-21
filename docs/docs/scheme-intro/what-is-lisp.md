---
sidebar_position: 1
---

# What is Lisp

![Lisp cycle](./img/lisp_cycles.png)

Lisp is second oldest programming language (after Fortran) that is still in use.
Lisp is an acronym for **LISt Processing**. The main feature of Lisp is it's lack of syntax.
Everything is written as S-Expression which is a list wrapped in parentheses with space between elements.

```scheme
(+ 1 2 3)
```

This is basic lisp expression. The difference between Scheme and other programming languages that often
write the same expression as:

```javascript
1 + 2 + 3
```

Is that in Lisp there are no operators. The above expression is just procedure application (invoking a function).
Plus is not an operator only a symbol that point into addition procedure that is executed. So in fact in other
programming languages this should be written as:

```javascript
+(1, 2, 3)
```

This is obviously invalid syntax (in most languages).

## Nesting expressions

The S-Expressions can be nested:

```scheme
(+ (* 3 (/ 1 2)) (+ 1 2))
```

But you can't add parentheses randomly to wrap expressions, like in other languages. Parentheses is always
procedure application (or special form that will be described later).

S-Expression is most efficient way to write function application, and you can form with it any nested trees.

## What is Scheme

So now what is Scheme. Scheme is a dialect of Lisp, there are other well known dialects of Lisp,
like Common Lisp, Racket, Clojure. They all have one in common, they all use S-Expressions for
syntax (or lack of).

Scheme was designed by ''Guy L. Steele'' and ''Gerald Jay Sussman'' in 1970s. They were playing with
an idea called the actor model and trying to understand it by creating simple implementation. That
implementation later lead to Scheme programming languages.

### Standards

Scheme is standardized in form of R<sup>n</sup>RS documents. Revised<sup>n</sup> Report on the
Algorithmic Language Scheme. Where power indicate how many times it was revisited. Power of 2 means
Revisited Revisited.

The latest standard is R<sup>7</sup>RS Small and there is version large in the making.

### Scheme Implementations

You can find different implementations of the Programming language that re more or less compatible
with specification.

Example implementations:

* [Guile](https://www.gnu.org/software/guile/)
* [Kawa](https://www.gnu.org/software/kawa/index.html)
* [Gauche](https://practical-scheme.net/gauche/)
* [Chiken](https://www.call-cc.org/)
* [LIPS](https://lips.js.org/)

The official website for Scheme programming language is [scheme.org](https://www.scheme.org/).

### SRFI Documents

SRFI stands for Scheme Requests for Implementations. And are official documents that add new
features to the languages. Some of the SRFI may land in new version of R<sup>n</sup>RS
specification. The website for SRFI documents is located at
[srfi.schemers.org](https://srfi.schemers.org/).
