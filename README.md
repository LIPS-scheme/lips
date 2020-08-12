![LIPS - Scheme Based Powerful Lisp Language](https://github.com/jcubic/lips/blob/devel/assets/lips.svg?raw=true)

[![Join the chat at https://gitter.im/jcubic/lips](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jcubic/lips)
[![npm](https://img.shields.io/badge/npm-DEV-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/lips.svg?branch=devel&4a64e7652184e9e6a31c50e3719e38d32dbe77fd)](https://travis-ci.org/jcubic/lips)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=devel&2c48907438a7265935a7b21e6931008d)](https://coveralls.io/github/jcubic/lips?branch=devel)


LIPS is Powerful Lisp based on Scheme dialect and R5RS specification
(and part of R7RS), it have extensions that make it easier to interacti
with JavaScript, it work in Browser and Node.js.

[Demo](https://jcubic.github.io/lips/#demo)

## Features

* Almost fully Compatible with R5RS,
* Lisp macros, backquote and macroexpand,
* Scheme Hygienic Macros with `sytnax-rules`,
* Parser Syntax extensions and string representation,
* RegExp-es are first class objects,
* Functions in LIPS are normal JavaScript functions,
* You can invoke native JavaScript functions and methods from Lips,
* Promises are treated as values they resolve to (so async code look like sync - like auto `async/await`),
* Library agnostic - you can use it with any JavaScript library,
* Almost no dependencies (only bn.js for big integers),
* Easy extension using JavaScript using Macros or functions,
* Builtin pretty printer,
* JavaScript object literals with `&(:foo 10)` (created using parser extensions),
* Builtin help system like in Emacs Lisp,
* You can access `__doc__` and `__code__` of any defined functions,
* You can access and modify function code while function is running,
* BigInt support, if your browser don't support them, you will need to use [bn.js](https://github.com/indutny/bn.js/),
* Almost everything is first class object including macros and functions,
* Functions code introspection/manipulation at runtime, give more power to the programmer,
* Optional dynamic scope (can be used to build Emacs Lisp interpreter on top of LIPS).

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
https://cdn.rawgit.com/jcubic/lips/devel/dist/lips.min.js
```

## Usage


Simplest way is to include the lips code in script tag:

```html
<script type="text/x-lips">
(let ((what "world")
      (greet "hello"))
   (display (string-append "hello" " " what)))
</script>
```

or use `src` attribute:

```html
<script type="text/x-scheme" src="example.lips"></script>
```

Running programmatically:

```javascript
var {exec} = require('@jcubic/lips'); // node
// or
var {exec} = lips; // browser

exec(string).then(function(results) {
     results.forEach(function(result) {
        console.log(result.toString());
     });
});
```

More documentation in [Getting Started Guide](https://github.com/jcubic/lips/wiki/Getting-Started) and
in [docs page](https://jcubic.github.io/lips/docs.html).

## Standalone executable

if you install lips globally with:

```
npm install -g @jcubic/lips
```

you can run interpreter from terminal:

```
$ lips
  __ __                          __
 / / \ \       _    _  ___  ___  \ \
| |   \ \     | |  | || . \/ __>  | |
| |    > \    | |_ | ||  _/\__ \  | |
| |   / ^ \   |___||_||_|  <___/  | |
 \_\ /_/ \_\                     /_/

LIPS Scheme Interpreter DEV (2020-08-12)
Copyright (c) 2018-2020 Jakub T. Jankiewicz <https://jcubic.pl/me>

Type (env) to see environment with functions macros and variables.
You can also use (help name) to display help for specic function or macro.

lips> (define (square x)
...     (* x x))
lips> (square 10)
100
lips>
```

One feature of LIPS REPL is that it auto indent the lines when you press enter
and didn't finish the code.

You can also run code as string with:

```
lips -c '(let ((what "World")) (display (string-append "Hello " what)))'
```

and you can run a file using:

```
cat > foo.scm <<EOF
(let ((what "World"))
  (display (string-append "Hello " what))
  (newline))
EOF

lips foo.scm
```

You can also write executable files that use lips using shebang (SRFI-22)

```
cat foo.scm
#!/usr/bin/env lips

(let ((what "World"))
  (display (string-append "Hello " what))
  (newline))

chmod a+x foo.scm
./foo.scm
```

Executable also return S-Expression according to SRFI-176 use `lips --version` or `lips -V`.

## Name

The name LIPS is recursive acronym which expand into LIPS Is Pretty Simple.
This name was used because first version was very simple implementation.
It's not that simple anymore, but still it should be pretty easy to modify.
Note that some parts of the code may be more complex.

## License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018-2020 [Jakub T. Jankiewicz](https://jcubic.pl/jakub-jankiewicz)
