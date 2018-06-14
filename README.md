## LIPS is Pretty Simple

[![npm](https://img.shields.io/badge/npm-DEV-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/jquery.terminal.svg?branch=devel&97a82be4530eedcdaf7879b75aeadafb5aa4b4dd)](https://travis-ci.org/jcubic/jquery.terminal)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=devel&8cd27d9771922623e9d7bcc851ac4ce6)](https://coveralls.io/github/jcubic/lips?branch=devel)


LIPS is very simple Lisp, similar to Scheme written in JavaScript. With optional dynamic scope.

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
https://cdn.rawgit.com/jcubic/lips/devel/dist/lips.min.js
```

## Usage

```javascript
var {exec} = require('@jcubic/lips'); // node
// or
var {exec} = lips; // browser

exec(string).then(function(results) {
    results.forEach(function(result) {
        console.log(result);
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

`evaluate` and `exec` functions also accept second argument, which is Environment.
By default it's `lips.global_environment`. You can use it if you want to
have separated instances of the interpreter.

It you want to use dynamic scope you can pass two arguments your global scopes and dynamic scope,
scope and true or just true to have global scope.




You can create new environment using:

```javascript
var env = new Environment({}, lips.global_environment, 'name');
```

First argument is an object with functions, macros and variables (see Extending LIPS in
[docs](https://jcubic.github.io/lips/docs.html)).
Second argument is parent environment, you need to use global environment (or other that extend global)
otherwise you will not have any functions.

You can also use inherit method to create new child environment:

```javascript
var env = lips.global_environment.inherit({fn: function() { return 42; }}, 'name of the scope');
var env = lips.global_environment.inherit('name of the scope');
var env = lips.global_environment.inherit();
```


name of the scope is optional, it was used mainly for debugging while working on dynamic scope.
But each function you define have Environment in this context so you can check the name of your scope
in your functions written in JavaScript.

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
