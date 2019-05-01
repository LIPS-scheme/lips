## LIPS is Pretty Simple

[![npm](https://img.shields.io/badge/npm-{{VER}}-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/jquery.terminal.svg?branch={{BRANCH}}&{{COMMIT}})](https://travis-ci.org/jcubic/jquery.terminal)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch={{BRANCH}}&{{CHECKSUM}})](https://coveralls.io/github/jcubic/lips?branch={{BRANCH}})


LIPS is very simple Lisp, similar to Scheme written in JavaScript.

[Demo](https://jcubic.github.io/lips/#demo)

## Key features

* Full lisp macros, backquote and macroexpand,
* Functions in lips are normal JavaScript functions,
* You can invoke native JavaScript functions and methods from Lips,
* Promises are treated as values they resolve to (so async code look like sync - like auto `async/await`),
* Library agnostic - you can use it with any library,
* Easy extension using JavaScript using Macros or functions,
* RegExp-es are first class objects,
* BigInt support, if your browser don't support them, you will need to use [bn.js](https://github.com/indutny/bn.js/),
* Optional dynamic scope.

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
   (print (concat "hello" " " what)))
</script>
```

or use `src` attribute:

```html
<script type="text/x-lips" src="example.lips"></script>
```

More documentation in [Getting Started Guide](https://github.com/jcubic/lips/wiki/Getting-Started) and
in [docs page](https://jcubic.github.io/lips/docs.html).

## License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018-2019 [Jakub T. Jankiewicz](https://jcubic.pl/jakub-jankiewicz)
