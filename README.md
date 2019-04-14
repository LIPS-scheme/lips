## LIPS is Pretty Simple

[![npm](https://img.shields.io/badge/npm-0.9.0-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/jquery.terminal.svg?branch=master&4912f43f5a0a8836940ba8e35fe069c538cae4b4)](https://travis-ci.org/jcubic/jquery.terminal)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=master&4605127ab4ae61bb9393947f167c7edb)](https://coveralls.io/github/jcubic/lips?branch=master)


LIPS is very simple Lisp, similar to Scheme written in JavaScript.

[Demo](https://jcubic.github.io/lips/#demo)

## Key features

* Full lisp macros and backquote,
* Functions in lips are normal JavaScript functions,
* You can invoke native JavaScript functions and methods from Lips,
* Easy extension using JavaScript using Macros or functions,
* RegExp-es are first class objects,
* Promises are treated as values they resolve to (so async code look like sync),
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


You can also run the interpreter programmatically:


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

there is also longer version if you want to split the process of evaluation:

```
var {parse, tokenize, evaluate} = require('@jcubic/lips');

parse(tokenize(string)).forEach(function(code) {
    evaluate(code);
});
```

`evaluate` and `exec` functions also accept second argument, which is Environment.  By
default it's `lips.env`. You can use it if you want to have separated instances of the
interpreter.

You can create new environment using:

```javascript
var env = new Environment({}, lips.env);
```

First argument is an object with functions, macros and variables (see Extending LIPS at
the end of [docs](https://jcubic.github.io/lips/docs.html)).  Second argument is parent
environment, you need to use global environment (or other that extend global) otherwise
you will not have any functions.

You can also use helper function:


```javascript
var env = lips.env.inherit('name', {});
```


While calling exec, optionally you can provide 3rd options as environment for dynamic
scope or value `true`, you can also use 2 arguments where first is code (string) or AST
(tree of Pairs) with `evaluate` and second is value `true`:

```
// dynamic scope
exec('(+ 10 10)', env, env)
exec('(+ 10 10)', true)

// lexical scope
exec('(+ 10 10)')
exec('(+ 10 10)', env)
```

Exec function always return a `Promise` for array of values, value can be Pair that you
can convert to Array using `Pair::toArray()`, `LNumber` that wrap BigInt or native numbers
(if your browser don't support BigInt and you don't include bn.js). You can get native
value out if BigInt using `LNumber::valueOf()` but you may lost precision or get
completely different value if your value is big. You can also use `LNumber::toString()` to
get number representation as string (works for all values).

`evaluate` function return normal values or a Promise, so you will need to check the type
of the value, some expressions return explicit `Promise` like `let` and `let*`, so you can
use fetch to get text value in one `let`. `exec` make this easier to always return
`Promise`.

## License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018-2019 [Jakub T. Jankiewicz](https://jcubic.pl/jakub-jankiewicz)
