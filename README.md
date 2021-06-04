![LIPS - Scheme Based Powerful Lisp Language](https://github.com/jcubic/lips/blob/master/assets/lips.svg?raw=true)

[![npm](https://img.shields.io/badge/npm-1.0.0%E2%80%93beta.14-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
![1.0.0 Complete](https://img.shields.io/github/milestones/progress-percent/jcubic/lips/1?label=1.0.0%20Complete)
[![travis](https://travis-ci.org/jcubic/lips.svg?branch=master&058809fbfbc2e0d038998043cc42545a0051410d)](https://travis-ci.org/jcubic/lips)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=master&25a417d0b61dd73dde71da232355132a)](https://coveralls.io/github/jcubic/lips?branch=master)
[![Join Gitter Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jcubic/lips)
[![GitHub license](https://img.shields.io/github/license/jcubic/lips.svg)](https://github.com/jcubic/lips/blob/master/LICENSE)
![NPM Download Count](https://img.shields.io/npm/dm/@jcubic/lips)
![JSDelivr Download count](https://img.shields.io/jsdelivr/npm/hm/@jcubic/lips)
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fjcubic%2Flips.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fjcubic%2Flips?ref=badge_shield)

[![GitHub stars](https://img.shields.io/github/stars/jcubic/lips.svg?style=social&label=Star&maxAge=2592000)](https://GitHub.com/jcubic/lips/stargazers/)
<a href="https://twitter.com/intent/tweet?text=Powerful%20Scheme%20based%20lisp%20language%20written%20in%20JavaScript.%20It%20makes%20life%20easier%20by%20better%20interaction%20with%20JS.%20Use%20full%20power%20of%20JS%2C%20lisp%20and%20npm%20to%20create%20your%20applications%20via%20@lips_lang&url=https://github.com/jcubic/lips&hashtags=javascript,opensource,lisp,scheme,language,programming">
   <img src="https://github.com/jcubic/lips/blob/devel/assets/tweet-shield.svg?raw=true" alt="Tweet" height="20"/>
</a>

[LIPS is a powerful Scheme-based, Lisp language written in JavaScript](https://lips.js.org).
It is based on the Scheme dialect and the R5RS/R7RS specifications. It has extensions to make it easier
to interact with JavaScript. It work both in the browser and with Node.js.

The name is a recursive acronym which stands for LIPS Is Pretty Simple.

## Demo

[Web REPL Demo](https://lips.js.org/#demo)

## Features

* Literal regular expression.
* Asynchronous execution (auto resolving of promises).
* Possibility to add new syntax (similar to vectors and object).
* Numerical tower and Big Integer support.
* Powerful introspection.
* Great integration with JavaScript.
* Auto formatting lisp of code (pretty print)
* Lisp and hygienic Scheme macros and macroexpand.
* Builtin help system.

## Installation

To install you can use npm (or yarn)<br/>
**NOTE:** The version that is on NPM is heavily outdated, use beta version:


```
npm install @jcubic/lips@beta
```

or yarn:

```
yarn add @jcubic/lips@beta
```

then include the file in the script tag. You can grab the version from unpkg.com

```
https://unpkg.com/@jcubic/lips@beta
```

or from jsDelivr (that's seems a bit faster)

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
<script type="text/x-scheme" bootstrap>
(let ((what "world")
      (greet "hello"))
   (display (string-append "hello" " " what)))
</script>
```

or use the `src` attribute:

```html
<script type="text/x-scheme" bootstrap src="example.scm"></script>
```

## Bootstrapping Scheme system

Big part of LIPS is written in LIPS itself, but to use full power of LIPS you need
to load those additional Scheme files. The easiest way is to add `bootstrap` attribute
on first script tag with `text/x-scheme` type. By default it will use CDN from
[jsdelivr](https://www.jsdelivr.com/). To load each file using builtin load function
(that will fetch the file using AJAX and evaluate it).

```
<script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js" bootstrap></script>
```

## Running LIPS programmatically

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

Documentation about beta version can be found in
[Wiki](https://github.com/jcubic/lips/wiki/v1.0.0).

## Standalone executable

**NOTE:** Executable don't require bootstrapping lib files.

If you install lips globally with:

```
npm install -g @jcubic/lips@beta
```

you can run the interpreter from the terminal:

![LIPS: Scheme interactive terminal](https://github.com/jcubic/lips/blob/master/assets/screencast.gif?raw=true)


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

## Links
* [Gitter Chat](https://gitter.im/jcubic/lips)
* [LISP: Scheme in JavaScript Git Repository](https://github.com/jcubic/lips)
* [Official Website](https://lips.js.org/)

## Roadmap

### 1.0
- [x] Full support for R5RS
- [ ] Full support for R7RS
  - [ ] R7RS libraries (`import`/`export`/`define-library`).
- [ ] Continuations.
- [ ] Tail Call Optimization (TCO).
- [ ] Fully tested Numerical Tower.
- [ ] All recursive function in JS don't consume stack.
- [ ] Picture language (possibly inspired by P5.js).
- [ ] Finish `syntax-rules` (ignore limitations of current approach).
  - [ ] Objects.
  - [ ] Vectors.

### 1.1
- [ ] Fully working JSON Compiler (for faster parsing and loading std lib).
- [ ] Allow to use read/port in syntax extensions (similar to CL reader macros).
- [ ] Proper expansion time for both macro system.
- [ ] Fully working and tested R7RS hygienic Macros (`syntax-rules`).

## Acknowledgments
* Font used in logo is [Telegrafico](https://www.dafont.com/telegrafico.font) by [ficod](https://www.deviantart.com/ficod).
* Current parser is inspired by implementation in [BiwaScheme](https://www.biwascheme.org/) by Yutaka HARA (yhara).
* `fetch` polyfill use [unfetch](https://github.com/developit/unfetch) by Jason Miller.
* Browser `init` function use [ContentLoaded](http://javascript.nwbox.com/ContentLoaded/).
* The rationalize algorithm is based on [Kawa Scheme](https://www.gnu.org/software/kawa/index.html) by Per M.A. Bothner, Alan Bawden and Marc Feeley.
* `ucs2decode` function taken from [punycode.js](https://github.com/bestiejs/punycode.js) by [Mathias Bynens](https://mathiasbynens.be/).
* [Rosetta Code](https://rosettacode.org/) is used for:
  * [gdc](https://rosettacode.org/wiki/Greatest_common_divisor#JavaScript),
  * [lcm](https://rosettacode.org/wiki/Least_common_multiple#JavaScript),
  * [LFloat::toRational](https://rosettacode.org/wiki/Convert_decimal_number_to_rational).
* [StackOverlow](https://stackoverflow.com) code was used for functions:
  * [fworker](https://stackoverflow.com/a/10372280/387194),
  * [flatten](https://stackoverflow.com/a/27282907/387194),
  * [allPossibleCases](https://stackoverflow.com/a/4331218/387194).
* Code formatter is roughly based on [scheme-style](http://community.schemewiki.org/?scheme-style) and GNU Emacs scheme mode.

## License

Released under [MIT](http://opensource.org/licenses/MIT) license<br/>
Copyright (c) 2018-2021 [Jakub T. Jankiewicz](https://jcubic.pl/me)

[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fjcubic%2Flips.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fjcubic%2Flips?ref=badge_large)
