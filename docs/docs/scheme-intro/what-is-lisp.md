---
sidebar_position: 1
description: What is Lisp and what is Scheme and a bit of history
---

# What is Lisp and Scheme

[![Lisp cycle](./img/lisp_cycles.png)](https://xkcd.com/297/)

Lisp is the second-oldest programming language (after Fortran) that is still in use.  Lisp is an
acronym for **LISt Processing**. It was invented by
[John McCarthy](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)) in 1958 at
[MIT](https://en.wikipedia.org/wiki/Massachusetts_Institute_of_Technology). The main feature of Lisp
is its lack of syntax.  The idea for Lisp language came from mathematics, to be exact
[Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) defined by
[Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church), which was invented or discovered to
prove that the [halting problem](https://en.wikipedia.org/wiki/Halting_problem) is unsolvable.

The most distinguishing things about lisp is a notion that code and data are represented using the
same [data structures](https://en.wikipedia.org/wiki/Data_structure), in lisp they are lists. This
is a very important characteristic, and it's called
[Homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity).

## S-Expressions

In Lisp, everything is written as S-Expression, which is a list wrapped in parentheses with space
between elements.

```scheme
(+ 1 2 3)
```

This is basic lisp expression. The difference between Scheme and other programming languages that often
write the same expression as:

```javascript
1 + 2 + 3
```

Is that in Lisp there are no operators. The above expression is just procedure application (function call).

**NOTE**: We will use procedure and function interchangeably in this tutorial.

Plus is not an operator, only a symbol that point into an addition procedure that is executed. So in
fact in other programming languages this should be written as:

```javascript
+(1, 2, 3)
```

This is obviously invalid syntax (in most languages).

## Nesting expressions

The S-Expressions can be nested:

```scheme
(+ (* 3 (/ 1 2)) (+ 1 2))
```

But you can't add parentheses randomly to wrap expressions, like in other languages. Parentheses are
always procedure application (or special form that will be described later).

S-Expression is the most efficient way to write function application, and you can form with it any
nested trees.

## What is Scheme

So now what is Scheme. Scheme is a dialect of Lisp, there are other well known dialects of Lisp,
like Common Lisp, Racket, Clojure. They all have one in common, they all use S-Expressions for
syntax (or lack of).

Scheme was designed by *Guy L. Steele* and *Gerald Jay Sussman* in a 1970s. They were playing with
an idea called the actor model and trying to understand it by creating a simple implementation. That
implementation later led to Scheme programming languages.

## REPL
REPL or [Read-Eval-Print Loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop),
is a way to interact with interpreter in an interactive way. Most modern interpreted programming languages
that some kind of REPL, but it was first introduced in 1964 by
[L. Peter Deutsch](https://en.wikipedia.org/wiki/L._Peter_Deutsch) and
[Edmund Berkele](https://en.wikipedia.org/wiki/Edmund_Berkeley) for Lisp implementation on
[PDP-1](https://en.wikipedia.org/wiki/PDP-1).

To run REPL you often need to run scheme or lisp executable. It's often called from
the [terminal interface](https://en.wikipedia.org/wiki/Terminal_emulator).
When the scheme or lisp system runs you will get a prompt that may look like this:

```
scheme>
```

And you can type your scheme code and press enter to execute it (it's often called evaluation of the expression).

### Standards

Scheme is standardized in form of [R<sup>n</sup>RS documents](https://standards.scheme.org/).
Revised<sup>n</sup> Report on the Algorithmic Language Scheme. Where power indicate how many times
it was revisited. Power of 2 means Revisited Revisited.

The latest standard is R<sup>7</sup>RS Small, and there is version large in the making.

### Scheme Implementations

You can find different implementations of the programming language that re more or less compatible
with specification.

Example implementations:

* [Guile](https://www.gnu.org/software/guile/)
* [Kawa](https://www.gnu.org/software/kawa/index.html)
* [Gauche](https://practical-scheme.net/gauche/)
* [Chiken](https://www.call-cc.org/)
* [LIPS](https://lips.js.org/)

The official website for Scheme programming language is [scheme.org](https://www.scheme.org/), which
contains more up to date [list of Scheme implementations](https://get.scheme.org/).

### SRFI Documents

SRFI stands for Scheme Requests for Implementations. And are official documents that add new
features to the languages. Some of the SRFI may land in new version of R<sup>n</sup>RS
specification. The website for SRFI documents is located at
[srfi.schemers.org](https://srfi.schemers.org/).
