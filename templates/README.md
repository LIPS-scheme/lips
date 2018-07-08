## LIPS is Pretty Simple

[![npm](https://img.shields.io/badge/npm-{{VER}}-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/jquery.terminal.svg?branch={{BRANCH}}&{{COMMIT}})](https://travis-ci.org/jcubic/jquery.terminal)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch={{BRANCH}}&{{CHECKSUM}})](https://coveralls.io/github/jcubic/lips?branch={{BRANCH}})


LIPS is very simple Lisp, similar to Scheme written in JavaScript.

[Demo](https://jcubic.github.io/lips/#demo)

## Key features

* Lisp Macros and backquote,
* Functions in lips are normal javascript functions,
* Easy extension using JavaScript using Macros or functions,
* Regexes are first class objects,
* BigInt support,
* Optional dynamic scope,
* Promises are treated as values they resolve to.

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
https://cdn.rawgit.com/jcubic/lips/{{BRANCH}}/dist/lips.min.js
```

## Usage

```javascript
var {exec} = require('@jcubic/lips'); // node
// or
var {exec} = lips; // browser

exec(string).forEach(function(result) {
     console.log(result);
});
```

there is also longer version if you want to split the process of evaluation:

```
var {parse, tokenize, evaluate} = require('@jcubic/lips');

parse(tokenize(string)).forEach(function(code) {
    evaluate(code);
});
```

`evaluate` and `exec` functions also accept second argument, which is Environment.
By default it's `lips.global_environment`. You can use it if you want to
have separated instances of the interpreter.

You can create new environment using:

```javascript
var env = new Environment({}, lips.global_environment);
```

First argument is an object with functions, macros and varibles (see Extending LIPS at the end).
Second argument is parent environment, you need to use global environment (or other that extend global)
otherwise you will not have any functions.

You can also use script tag to execute LIPS code:

```html
<script type="text-x/lips">
(let ((what "world")
      (greet "hello"))
   (print (concat "hello" " " what)))
</script>
```

## License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018 [Jakub Jankiewicz](http://jcubic.pl/jakub-jankiewicz)
