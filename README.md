<h1 align="center">
  <img src="https://github.com/jcubic/lips/blob/devel/assets/lips.svg?raw=true"
       alt="LIPS - Scheme Based Powerful Lisp Language" />
</h1>

[![X (formerly Twitter) Follow](https://img.shields.io/twitter/follow/lips_lang)](https://twitter.com/lips_lang)
[![npm](https://img.shields.io/badge/npm-1.0.0%E2%80%93beta.19-blue.svg)](https://www.npmjs.com/package/@jcubic/lips)
![1.0.0 Complete](https://img.shields.io/github/milestones/progress-percent/jcubic/lips/1?label=1.0.0%20Complete)
[![Build and test](https://github.com/jcubic/lips/actions/workflows/build.yaml/badge.svg?branch=devel&event=push)](https://github.com/jcubic/lips/actions/workflows/build.yaml)
[![Coverage Status](https://coveralls.io/repos/github/jcubic/lips/badge.svg?branch=devel&743f5850f78d354efa7dc6171c26e2ad)](https://coveralls.io/github/jcubic/lips?branch=devel)
[![Join Gitter Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jcubic/lips)
![NPM Download Count](https://img.shields.io/npm/dm/@jcubic/lips)
![JSDelivr Download count](https://img.shields.io/jsdelivr/npm/hm/@jcubic/lips)
<a href="https://codeclimate.com/github/jcubic/lips/maintainability"><img src="https://api.codeclimate.com/v1/badges/876398746c020dd1bb97/maintainability" /></a>
[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fjcubic%2Flips.svg?type=shield)](https://app.fossa.io/projects/git%2Bgithub.com%2Fjcubic%2Flips?ref=badge_shield)

[![GitHub stars](https://img.shields.io/github/stars/jcubic/lips.svg?style=social&label=Star&maxAge=2592000)](https://GitHub.com/jcubic/lips/stargazers/)
<a href="https://twitter.com/intent/tweet?text=Powerful%20Scheme%20based%20lisp%20language%20written%20in%20JavaScript.%20It%20makes%20life%20easier%20by%20better%20interaction%20with%20JS.%20Use%20full%20power%20of%20JS%2C%20lisp,%20and%20npm%20to%20create%20your%20applications%20via%20@lips_lang&url=https://github.com/jcubic/lips&hashtags=javascript,opensource,lisp,scheme,language,programming">
   <img src="https://github.com/jcubic/lips/blob/devel/assets/tweet-shield.svg?raw=true" alt="Tweet" height="20"/>
</a>

[LIPS is a powerful Scheme-based, Lisp language written in JavaScript](https://lips.js.org).
It is based on the Scheme dialect of lisp and the R5RS/R7RS specifications. It has extensions to make it easier
to interact with JavaScript and extend the language. It works both in the browser and with Node.js.

The aim of the project is to support full R7RS specification and be compatible with [Scheme programming language](https://www.scheme.org/).

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

Create any link in your bookmarks, edit it and copy-paste the content of that file.
After you click on the link it will create the REPL at the bottom of the page.
(NOTE: It may not work on every page because of content security policy;
e.g. google.com or gihub.com)

If you have trouble with creating the bookmarklet, you can open
[LISP Scheme home page](https://lips.js.org/#bookmark) where you can
find a link that you can drag to your bookmarks.

## Usage

The simplest way is to include the lips code in the script tag:

```html
<script type="text/x-scheme" bootstrap>
(let ((what "world")
      (greet "hello"))
   (display (string-append greet " " what)))
</script>
```

or use the `src` attribute:

```html
<script type="text/x-scheme" bootstrap src="example.scm"></script>
```

## Bootstrapping Scheme system

Big part of LIPS is written in LIPS itself, but to use full power of LIPS you need
to load those additional Scheme files. The easiest way is to add `bootstrap` attribute
on first script tag with `text/x-scheme` type. By default, it will use CDN from
[jsdelivr](https://www.jsdelivr.com/). To load each file using builtin load function
(that will fetch the file using AJAX and evaluate it).

```html
<script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js" bootstrap></script>
```

You can also specify the path where LIPS should search for standard library.

```html
<script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js"
        bootstrap="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/std.xcb">
</script>
```

You can use `bootstrap="./std.xcb"` if there is `std.xcb` file in local directory.
You can also bootstrap with `std.scm` or `std.min.scm` but xcb file is the fastest,
because it's already parsed and compiled into binary format.

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

## FOSDEM'23 Presentation [Video]

[![FOSDEM 2023 - LIPS Scheme: Powerful introspection and extensibility](https://github.com/jcubic/lips/blob/devel/assets/fosdem-intro.png?raw=true)](https://fosdem.org/2023/schedule/event/lipsscheme/)

## Limitations

### Performance
Because LIPS is tree walking interpreter, sometimes it may be slow. Especially if you want to
process long arrays and use callback function. If the array is quite large each piece of code
inside the callback may slow down the processing. For example see:

script [reference.scm](https://github.com/jcubic/lips/blob/devel/scripts/reference.scm)

That generates reference documentation for all builtin functions and macros.
The slow part is `(names.sort name-compare)` (`Array::sort`) that take quite time to calculate,
because the array with functions and macros is quite large. If you came into performance issue,
you can write the part of the code in JavaScript. If you want to do this in LIPS Scheme you can use
something like this:

```scheme
(let ((fn (self.eval "(function(a, b) {
                         /* any complex code in JS */
                         return a.localeCompare(b);
                      })")))
   (arr.sort fn))
```

Another example of slow performance is using LIPS with React, the more code you put into components
the slower the app will become.

Examples:
* [Preact app that update SVG](https://codepen.io/jcubic/pen/PojYxBP) - it requires using debounce.
* [React with Hooks](https://codepen.io/jcubic/pen/PoKQmpq?editors=1000) - on click the UI freezes for ~300ms, you can see warnings in dev tools.

The issue with performance is tracked in [#197](https://github.com/jcubic/lips/issues/197).

### JavaScript callbacks

Another limitation is when using JavaScript libraries that require normal values but get a Promise instead.
This can happen with React/Preact and when the component returns a Promise. Some macros can be async
(return a Promise), which will break the React app when used in components. An example of a macro that is async is
`do` macro. So when using React/Preact and when you need to use a promise, use promise quotation and `useEffect`.

## Supported SRFI

### built-in

| description | spec |
| :--- | ---: |
| Feature-based conditional expansion construct | [SRFI-0](https://srfi.schemers.org/srfi-0/) |
| Homogeneous numeric vector datatypes | [SRFI-4](https://srfi.schemers.org/srfi-4/) |
| Basic String Ports | [SRFI-6](https://srfi.schemers.org/srfi-6/) |
| Running Scheme Scripts on Unix | [SRFI-22](https://srfi.schemers.org/srfi-22/) |
| Error reporting mechanism | [SRFI-23](https://srfi.schemers.org/srfi-23/) |
| Basic Format Strings | [SRFI-28](https://srfi.schemers.org/srfi-28/) |
| Basic Syntax-rules Extensions | [SRFI-46](https://srfi.schemers.org/srfi-46/) |
| An interface to access environment variables | [SRFI-98](https://srfi.schemers.org/srfi-98/) |
| Syntax parameters | [SRFI-139](https://srfi.schemers.org/srfi-139/) |
| Custom macro transformers | [SRFI-147](https://srfi.schemers.org/srfi-147/) |
| Version flag | [SRFI-176](https://srfi.schemers.org/srfi-176/) |
| Command line | [SRFI-193](https://srfi.schemers.org/srfi-193/) |

### require manual loading

You can load the SRFI with special syntax that start with `@lips`
which points to root LIPS directory:

```scheme
(load "@lips/lib/srfi/<number>.scm")
```

They should be loaded as R7RS libraries in final 1.0.0 version

| description | spec |
| :--- | ---: |
| List Library | [SRFI-1](https://srfi.schemers.org/srfi-1/) |
| `AND-LET*`: an AND with local bindings, a guarded `LET*` special form | [SRFI-2](https://srfi.schemers.org/srfi-2/) |
| receive: Binding to multiple values | [SRFI-8](https://srfi.schemers.org/srfi-8/) |
| `#,` external form | [SRFI-10](https://srfi.schemers.org/srfi-10/) |
| Notation for Specializing Parameters without Currying | [SRFI-26](https://srfi.schemers.org/srfi-26/) |
| Basic hash tables | [SRFI-69](https://srfi.schemers.org/srfi-69/) |
| Boxes | [SRFI-111](https://srfi.schemers.org/srfi-111/) |
| Syntactic combiners for binary predicates | [SRFI-156](https://srfi.schemers.org/srfi-156/) |
| Multiple-value boxes | [SRFI-195](https://srfi.schemers.org/srfi-195) |
| Procedures and Syntax for Multiple Values | [SRFI-210](https://srfi.schemers.org/srfi-210/) |
| Evaluating expressions in an unspecified order | [SRFI-236](https://srfi.schemers.org/srfi-236) |
| Destructuring Lists | [SRFI 239](https://srfi.schemers.org/srfi-239/) |

in Web (e.g. in Web REPL) you can also use full URL:

```scheme
(load "https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/srfi/<NUMBER>.scm")
```

## Links
* [Gitter Chat for discussions](https://gitter.im/jcubic/lips)
* [LIPS: Scheme in JavaScript Git Repository](https://github.com/jcubic/lips)
* [Official Website](https://lips.js.org/)
* [Twitter/X](https://twitter.com/LIPS_scheme)

### Articles
* [How to start learning Lisp](https://jcubic.pl/2019/05/jak-zaczac-nauke-lispa.html) (PL with translator widget)

### Press
* [JavaScript Weekly](https://javascriptweekly.com/issues/669)
* [Hacker News](https://news.ycombinator.com/item?id=38819212)
* [Brian Lovin](https://brianlovin.com/hn/38819212) (HN clone)

## Projects that use LIPS
* [Conzept](https://conze.pt) - Topic exploration system for the 21st century (see also their [Twitter account](https://twitter.com/conzept__) and [Command API Documentation](https://conze.pt/guide/command_api))

## Roadmap
### 1.0
- [x] Full support for R5RS
- [ ] Full support for R7RS
  - [ ] R7RS libraries (`import`/`export`/`define-library`).
  - [ ] Continuations.
  - [ ] Tail Call Optimization (TCO).
  - [ ] Fully tested Numerical Tower.
- [x] Fully working binary compiler (for faster parsing and loading std lib).
- [ ] Finish `syntax-rules` (ignore limitations of current approach).
  - [ ] Objects.
  - [ ] Vectors.

### Future Plans
- [ ] Picture language (possibly inspired by P5.js, see [SRFI-203](https://srfi.schemers.org/srfi-203/srfi-203.html)).
- [ ] Stepper/Debugger.
- [ ] Allow to use read/port in syntax extensions (similar to CL reader macros).
- [ ] Proper expansion time for both macro systems.
- [ ] Fully working and tested R7RS hygienic Macros (`syntax-rules`).
- [ ] All recursive function in JS don't consume stack.

### WIP Side projects
- [ ] [KISS](https://github.com/jcubic/kiss) (Chrome extension REPL).
- [ ] [SMILE](https://github.com/jcubic/smile) (Web IDE), need to start over.

## How you can help

I'm working on version 1.0. If you find any bugs, you can help by reporting them.
If you have some Scheme code that doesn't work (note about the limitations)
don't hesitate to [report an issue](https://github.com/jcubic/lips/issues/new).

You can also propose a feature or improvement to the library, it doesn't always have to be defects.

If you enjoy the library, you can write about it on a blog post and share information about it or write on Social Media.
Don't forget to link to the project website (it's
[good for SEO](https://itnext.io/seo-for-open-source-projects-1a6b17ffeb8b)).

Of course, if you want you can also contribute with code, but there are way easier ways to help.

I would also love to see if you use the library, I may even share the links of projects that use it.

## Acknowledgments
* Font used in logo is [Telegrafico](https://www.dafont.com/telegrafico.font) by [ficod](https://www.deviantart.com/ficod).
* Part of the current Parser is inspired by implementation in [BiwaScheme](https://www.biwascheme.org/) by Yutaka HARA (yhara).
* `fetch` polyfill use [unfetch](https://github.com/developit/unfetch) by Jason Miller.
* Browser `init` function use [ContentLoaded](http://javascript.nwbox.com/ContentLoaded/).
* The rationalize algorithm is based on [Kawa Scheme](https://www.gnu.org/software/kawa/index.html) by Per M.A. Bothner, Alan Bawden and Marc Feeley.
* `ucs2decode` function taken from [punycode.js](https://github.com/bestiejs/punycode.js) by [Mathias Bynens](https://mathiasbynens.be/).
* [Rosetta Code](https://rosettacode.org/) was used for:
  * [gdc](https://rosettacode.org/wiki/Greatest_common_divisor#JavaScript),
  * [lcm](https://rosettacode.org/wiki/Least_common_multiple#JavaScript),
  * [LFloat::toRational](https://rosettacode.org/wiki/Convert_decimal_number_to_rational).
* [StackOverlow](https://stackoverflow.com) code was used for functions:
  * [fworker](https://stackoverflow.com/a/10372280/387194),
  * [flatten](https://stackoverflow.com/a/27282907/387194),
  * [allPossibleCases](https://stackoverflow.com/a/4331218/387194),
  * [async replace](https://stackoverflow.com/a/48032528/387194).
* Code formatter is roughly based on [scheme-style](http://community.schemewiki.org/?scheme-style) and GNU Emacs scheme mode.
* Some helpers in standard library are inspired by same functions from [RamdaJS library](https://ramdajs.com/).

Special thanks to [Lassi Kortela](https://github.com/lassik) for helping with Scheme code.

## License

Released under [MIT](http://opensource.org/licenses/MIT) license<br/>
Copyright (c) 2018-2024 [Jakub T. Jankiewicz](https://jcubic.pl/me)

[![FOSSA Status](https://app.fossa.io/api/projects/git%2Bgithub.com%2Fjcubic%2Flips.svg?type=large)](https://app.fossa.io/projects/git%2Bgithub.com%2Fjcubic%2Flips?ref=badge_large)
