## LIPS is Pretty Simple

[![npm](https://img.shields.io/badge/npm-0.5.0-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/jquery.terminal.svg?branch=master&ce5fdee75492b52d49bfbbc34dbcfd9ffb20fe36)](https://travis-ci.org/jcubic/jquery.terminal)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=master&d80d86213c8f8d97d1352ed4276a11f7)](https://coveralls.io/github/jcubic/lips?branch=master)


LIPS is very simple Lisp, similar to Scheme written in JavaScript.

[Demo](https://jcubic.github.io/lips/#demo)

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
https://cdn.rawgit.com/jcubic/lips/master/dist/lips.min.js
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
