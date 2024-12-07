---
sidebar_position: 7
description: features of Node.js and Web REPL
---

# REPL

LIPS Scheme REPL ([Read Even Print Loop](https://en.wikipedia.org/wiki/Read–eval–print_loop)) is
a way to interact with running LIPS Scheme session.

## Web REPL

Web REPL you can access from [Home page](/) or as a [bookmarklet](/#bookmark) use
[jQuery Terminal](https://terminal.jcubic.pl/) and supports those features:
* syntax highlighting using [Prism.js](https://prismjs.com/),
* parentheses matching - when you type close parenthesis it will jump for a split second into it's
  matching open parenthesis,
* auto indentation - when you press enter it auto indent and if you copy paste the code it will reformat it.
* doc string tooltip - when you hover over a symbol it will show the docstring in a tooltip

## Node.js REPL

Node.js version of the REPL, also supports syntax highlighting and auto indentation. It also
supports [paste bracket mode from Node.js](https://github.com/nodejs/node/pull/47150) (added by
[Jakub T. Jankiewicz](https://jcubic.pl/me) and released in
[v20.6.0](https://nodejs.org/en/blog/release/v20.6.0) and [v18.19.0](https://nodejs.org/en/blog/release/v18.19.0)),
to properly handle copy-paste of Scheme code.

To learn more you can read this article:
[Handling auto-indentation in Node.js REPL](https://jakub.jankiewicz.org/blog/node-repl-auto-indent/).

From version [1.0.0-beta.20](https://github.com/LIPS-scheme/lips/releases/tag/1.0.0-beta.20)
the Node.js REPL also supports parentheses matching. It's supported by
[CLisp](https://www.gnu.org/software/clisp/), a [Common Lisp](https://common-lisp.net/) interpreter.

## Procedures useful in REPL

There are few procedures useful in the REPL:

* `help` - prints doc string for a given procedure, macro, or a variable (see documentation about
  [Doc Strings](/docs/lips/intro#doc-strings)),
* `apropos` - function return list of procedures from environment that match a string or a regex,
* `env` - function returns list of symbols which is everything that is inside an environment,
* `dir` - function return all properties from an object including those in prototype chain (a class).
* `repr` - function return representation of the object as a string, it's inspired by Python. The
  `repr` function accepts two arguments the second one is a boolean that indicate if it should work
  like write or like display. Write will write string with quotes. So if you want the strings inside
  the string to be quoted use `(repr obj #t)`

To read more about introspection of LIPS Scheme in REPL, see [documentation about Reflection](/docs/lips/reflection).
