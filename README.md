![LIPS - Scheme Based Powerful Lisp Language](https://github.com/jcubic/lips/blob/devel/assets/lips.svg?raw=true)

[![npm](https://img.shields.io/badge/npm-1.0.0%E2%80%93beta.7-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/lips.svg?branch=devel&c8b827d9437b3dc65f5700aaf9fc3cc35f819865)](https://travis-ci.org/jcubic/lips)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=devel&2c48907438a7265935a7b21e6931008d)](https://coveralls.io/github/jcubic/lips?branch=devel)
[![Join Gitter Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jcubic/lips)
<a href="https://twitter.com/intent/tweet?text=Powerful%20Scheme%20based%20lisp%20language%20written%20in%20JavaScript.%20It%20makes%20life%20easier%20by%20better%20interaction%20with%20JS.%20Use%20full%20power%20of%20JavaScript%2C%20lisp%20and%20npm%20to%20create%20your%20applications%20via%20@jcubic&url=https://github.com/jcubic/lips&hashtags=javascript,opensource,lisp,scheme,language,programming">
   <img src="https://github.com/jcubic/lips/blob/devel/assets/tweet-shield.svg?raw=true" alt="Tweet" height="20"/>
</a>

[![LIPS at Product Hunt](https://api.producthunt.com/widgets/embed-image/v1/featured.svg?post_id=273619&theme=dark)](https://www.producthunt.com/posts/lips)

[LIPS is a powerful Scheme-based, Lisp language written in JavaScript](https://jcubic.github.io/lips).
It is based on the Scheme dialect and the R5RS/R7RS specifications. It has extensions to make it easier
to interact with JavaScript. It work both in the browser and with Node.js.

The name is a recursive acronym which stands for LIPS Is Pretty Simple.

## Demo

[Demo](https://jcubic.github.io/lips/#demo)

[1.0 Beta demo](https://jcubic.github.io/lips/beta.html)

## Features

* Literal regular expression.
* Asynchronous execution.
* Possibility to add new syntax (similar to vectors and object).
* Powerful introspection.
* Great integration with JavaScript.
* Auto formatting lisp of code (pretty print)
* Lisp and hygienic Scheme macros and macroexpand.
* Builtin help system.

## Installation

To install you can use npm (or yarn):

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

then include the file in the script tag. You can grab the version from unpkg.com

```
https://unpkg.com/@jcubic/lips
```

or from jsdelivery

```
https://cdn.jsdelivr.net/npm/@jcubic/lips/dist/lips.min.js
```

and  beta version


```
https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js
```

## Bookmarklet REPL


You can also run the REPL on any page while you learn Scheme using the bookmarklet:

```
https://github.com/jcubic/lips/blob/master/lib/js/bookmark.js
```

Create any link in your bookmarks, edit it and copy paste the content of that file.
Affter you click on the link it will create the REPL at the bottom of the page.
(NOTE: It may not work on every page because of content security policy;
e.g. google.com or gihub.com)

If you have trouble with creating the bookmarklet you can open
[LISP Scheme home page](https://jcubic.github.io/lips/#bookmark) where you can
find a link that you can drag to your bookmarks.

## Usage


The simplest way is to include the lips code in the script tag:

```html
<script type="text/x-scheme">
(let ((what "world")
      (greet "hello"))
   (display (string-append "hello" " " what)))
</script>
```

or use the `src` attribute:

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

If you install lips globally with:

```
npm install -g @jcubic/lips
```

you can run the interpreter from the terminal:

![LIPS: Scheme interactive terminal](https://github.com/jcubic/lips/blob/devel/assets/screencast.gif?raw=true)


You can also run code in a string with:

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

Executables also return a S-Expression according to SRFI-176 use `lips --version` or `lips -V`.

## Credits
* Font used in logo is [Telegrafico](https://www.dafont.com/telegrafico.font) by [ficod](https://www.deviantart.com/ficod)

## License

Released under [MIT](http://opensource.org/licenses/MIT) license<br/>
Copyright (c) 2018-2020 [Jakub T. Jankiewicz](https://jcubic.pl/jakub-jankiewicz)
