![LIPS - Scheme Based Powerful Lisp Language](https://github.com/jcubic/lips/blob/devel/assets/lips.svg?raw=true)

[![npm](https://img.shields.io/badge/npm-1.0.0%E2%80%93beta.9-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
[![travis](https://travis-ci.org/jcubic/lips.svg?branch=devel&5fa64ccc0e5f77c887604b7fbf62dbf03fbc86bf)](https://travis-ci.org/jcubic/lips)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=devel&f6ff9bc2f84ef7b55e3e590fe6bc423b)](https://coveralls.io/github/jcubic/lips?branch=devel)
[![Join Gitter Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jcubic/lips)
<a href="https://twitter.com/intent/tweet?text=Powerful%20Scheme%20based%20lisp%20language%20written%20in%20JavaScript.%20It%20makes%20life%20easier%20by%20better%20interaction%20with%20JS.%20Use%20full%20power%20of%20JavaScript%2C%20lisp%20and%20npm%20to%20create%20your%20applications%20via%20@jcubic&url=https://github.com/jcubic/lips&hashtags=javascript,opensource,lisp,scheme,language,programming">
   <img src="https://github.com/jcubic/lips/blob/devel/assets/tweet-shield.svg?raw=true" alt="Tweet" height="20"/>
</a>

[![LIPS at Product Hunt](https://api.producthunt.com/widgets/embed-image/v1/featured.svg?post_id=273619&theme=dark)](https://www.producthunt.com/posts/lips)

[LIPS is a powerful Scheme-based, Lisp language written in JavaScript](https://lips.js.org).
It is based on the Scheme dialect and the R5RS/R7RS specifications. It has extensions to make it easier
to interact with JavaScript. It work both in the browser and with Node.js.

The name is a recursive acronym which stands for LIPS Is Pretty Simple.

## Demo

[Demo](https://lips.js.org/#demo)

[1.0 Beta demo](https://lips.js.org/beta.html)

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
[LISP Scheme home page](https://lips.js.org/#bookmark) where you can
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

## Boostraping Scheme system

To have fully working Scheme environment, you need to bootstrap the language and load
all scheme code written in LIPS. To do that you can use this:

```
(let-env lips.env.__parent__
    (load "./lib/bootstrap.scm")
    (load "./lib/R5RS.scm")
    (load "./lib/R7RS.scm"))
```

you need to use path to lib files, you can host them yourself or use CDN.

```
(let-env lips.env.__parent__
  (let ((path "https://cdn.jsdelivr.net/gh/jcubic/lips@devel/lib/"))
    (load (concat path "bootstrap.scm"))
    (load (concat path "R5RS.scm"))
    (load (concat path "R7RS.scm"))))
```

If you use server you will need to enable CORS if your website is on different domain,
because load will use AJAX to get the files.

**NOTE:** as of now there is not other solution, but it may change when version 1.0 is out.
Because the files use syntax extensions that modify the parser you can't combine those
files into one bigger file. Here is issue
[Make parser ES generator #80](https://github.com/jcubic/lips/issues/80)
that will make this possible. Other idea is to add `data-path=""` attribute on script tag
that will bootstrap the system from given path.

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

When running exec you will also need to bootstrap the language and loaded files from `/lib/` directory.

More documentation about stable version in
[Getting Started Guide](https://github.com/jcubic/lips/wiki/Getting-Started) and
in [docs page](https://lips.js.org/docs.html).

Documentation about beta version can be found in
[v1.0 draft document](https://github.com/jcubic/lips/wiki/v1.0-draft).

## Standalone executable

**NOTE:** Executable don't require bootstrapping lib files.

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
