![LIPS - Scheme Based Powerful Lisp Language](https://github.com/jcubic/lips/blob/devel/assets/lips.svg?raw=true)

[![npm](https://img.shields.io/badge/npm-1.0.0%E2%80%93beta.4-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/lips.svg?branch=devel&8c7bd0670008de11b91d6ab02a5b5d426a2fdf17)](https://travis-ci.org/jcubic/lips)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=devel&2c48907438a7265935a7b21e6931008d)](https://coveralls.io/github/jcubic/lips?branch=devel)
[![Join Gitter Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jcubic/lips)

LIPS is Powerful Lisp based language. It's based on Scheme dialect and R5RS
specification (and part on R7RS), it have extensions that make it easier to interact
with JavaScript, it work in Browser and Node.js.

The name is recursive acronym which stands for LIPS is Pretty Simple.

[Demo](https://jcubic.github.io/lips/#demo)

[1.0 Beta demo](https://jcubic.github.io/lips/beta.html)

## Features

* Compatibile with Scheme specification (R5RS and R7RS)
* Allow to define of new syntax, using parser extensions.
* Allow to create new data types, that fit nicely into reader and writer.
* Introspection and modifaction of the functions code at runtime.
* Syntax highligthing in NPM installed binaries and on the Web REPL.
* Builtin autoformatting (pretty printing) of code.
* Lips macros, Scheme hygienic macros with syntax-rules and macroexpand.
* Auto unwrapping of JavaScript promises.
* Almost no dependencies.
* Builtin help system in REPL (functions and macros have doc strings).
* Literal Regular Expressions.

## Integration with JavaScript

* JavaScript like doted notation.
* Object literals with `&(:foo 10)` (created using parser extensions),
* Direct access to JavaScript methods, functions and properties.
* Access to internal implementation details.
* Creating new JavaScript classes.
* Modification of prototype of the objects.

## Installation

To install you can use use npm (or yarn):

```
npm install @jcubic/lips
```

or yarn:

```
yarn add @jcubic/lips
```

To install 1.0.0 beta version use:

```
npm install @jcubic/lips@beta
```

then include the file in script tag, You can grab the version from unpkg.com

```
https://unpkg.com/@jcubic/lips
```

or from jsdelivery (beta version)

```
https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js
```

## Bookmarklet REPL


You can also run REPL on any page while you learn Scheme using bookmarklet:

```
https://github.com/jcubic/lips/blob/master/lib/js/bookmark.js
```

Create any link in your bookmarks, edit it and copy paste the content of that file.
Affter you click on the link it will create REPL at the bottom of the page.
(NOTE: it may don't work on every page because of content security policy, 
e.g. google.com or gihub.com)

## Usage


Simplest way is to include the lips code in script tag:

```html
<script type="text/x-scheme">
(let ((what "world")
      (greet "hello"))
   (display (string-append "hello" " " what)))
</script>
```

or use `src` attribute:

```html
<script type="text/x-scheme" src="example.scm"></script>
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

![LIPS: Scheme interactive terminal](https://github.com/jcubic/lips/blob/devel/assets/screencast.gif?raw=true)


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

## License

Released under [MIT](http://opensource.org/licenses/MIT) license<br/>
Copyright (c) 2018-2020 [Jakub T. Jankiewicz](https://jcubic.pl/jakub-jankiewicz)
