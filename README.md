![LIPS - Scheme Based Powerful Lisp Language](https://github.com/jcubic/lips/blob/master/assets/lips.svg?raw=true)

[![npm](https://img.shields.io/badge/npm-0.20.1-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/lips.svg?branch=master&2b24bb0edb0c69f7873fc943df34bc7c925c1625)](https://travis-ci.org/jcubic/lips)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=master&52aba06ba0a695568564177bab7ef6e8)](https://coveralls.io/github/jcubic/lips?branch=master)

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
* Almost everything is first class object including macros and functions,
* Functions code introspection/manipulation at runtime give more power to the programmer,
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
   (display (concat "hello" " " what)))
</script>
```

or use `src` attribute:

```html
<script type="text/x-lips" src="example.lips"></script>
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
LIPS Interpreter (Simple Scheme like Lisp)
Copyright (c) 2018-2019 Jakub T. Jankiewicz <https://jcubic.pl/me>

lips> (define (square x)
...     (* x x))
lips> (square 10)
100
lips>
```

One feature of LIPS REPL is that it auto indent the lines when you press enter and didn't finish the code.

You can also run code as string with:

```
lips -c '(let ((what "World")) (display (concat "Hello " what)))'
```

and you can run a file using:

```
cat > foo.lips <<EOF
(let ((what "World"))
  (display (concat "Hello " what)))
EOF

lips foo.lips
```

You can also write executable files that use lips shebang

```
cat > foo.lips <<EOF
#!/usr/bin/env lips
(let ((what "World"))
  (display (concat "Hello " what)))
EOF
chmod a+x foo.lips
foo.lips
```

```
cat <<EOF
something
EOF
```

> if just example of using cat to create multiline file from bash, you should use proper editor for
> writing files.

## License

Released under [MIT](http://opensource.org/licenses/MIT) license

Copyright (c) 2018-2019 [Jakub T. Jankiewicz](https://jcubic.pl/jakub-jankiewicz)
