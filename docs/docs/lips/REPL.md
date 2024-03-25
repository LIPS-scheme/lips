---
sidebar_position: 7
description: features of Node.js and Web REPL
---

# REPL

## Web REPL

Web REPL you can access from [Home page](/) or as a [bookmarklet](/#bookmark) use
[jQuery Terminal](https://terminal.jcubic.pl/) and supports those features:
* syntax highlighting using [Prism.js](https://prismjs.com/),
* parentheses matching - when you type close parenthesis it will jump for a split second into it's
  matching open parenthesis,
* auto indentation - when you press enter it auto indent and if you copy paste the code it will reformat it.
* doc string tooltip - when you hover over a symbol it will show the docstring in a tooltip

## Node.js REPL

Node.js version of the REPL, also supports syntax hightlighting and auto indentation. It also
supports [paste bracket mode from Node.js](https://github.com/nodejs/node/pull/47150) (added by
[Jakub T. Jankiewicz](https://jcubic.pl/me) and released in
[v20.6.0](https://nodejs.org/en/blog/release/v20.6.0)), to properly handle copy-paste of Scheme
code.

In the future the Node.js REPL may also support parentheses matching. It's supported by
[CLisp](https://www.gnu.org/software/clisp/) and [Common Lisp](https://common-lisp.net/)
interpreter.
