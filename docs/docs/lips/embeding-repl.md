---
sidebar_position: 9
description: "Embedding the LIPS Scheme REPL"
---

# Embedding LIPS REPL

To embed the LIPS REPL you need on your website you need:

## A HTML Web page

First you need to have a website where you want to embed the REPL. You can create simple HTML page
if you don't have one already:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>LIPS Scheme REPL</title>
  </head>
  <body>
  </body>
</html>
```

## Create placeholder for the REPL

Then you need to create a div that will hold the REPL UI.

```html
<div id="term"></div>
```

I usually name it `term` from terminal, because the REPL use [jQuery
Terminal](https://terminal.jcubic.pl/) library. You don't need to use `id`, you can create any div you
want, but you should be able to target it specifically with
[CSS selector](https://developer.mozilla.org/en-US/docs/Learn/CSS/Building_blocks/Selectors).

If you want full screen terminal you don't need to use the `<div>` and use `<body>` tag when
creating the terminal.  It's described how at the end of the document.

## Include main LIPS file

Then you need to include LIPS:

```html
<script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js" bootstrap></script>
```

Make sure you include `bootstrap` attribute, with it LIPS will load its standard library that is
written in Scheme.

If you want to have access to filesystem functions ([IO ports](/docs/scheme-intro/input-output)) you
need to include browserFS before LIPS:

```html
<script src="https://cdn.jsdelivr.net/npm/browserfs@1.x.x/dist/browserfs.min.js"></script>
```

## Include jQuery
jQuery is a dependency for the [jQuery Terminal](https://terminal.jcubic.pl/).

```html
<script src="https://cdn.jsdelivr.net/npm/jquery"></script>
```

## Include jQuery Terminal library

This is main library that add Terminal look and feel:

```html
<script src="https://cdn.jsdelivr.net/npm/jquery.terminal/js/jquery.terminal.min.js"></script>
<link href="https://cdn.jsdelivr.net/npm/jquery.terminal/css/jquery.terminal.min.css" rel="stylesheet"/>
```

## Include Terminal LIPS REPL files

This is the main code that use jQuery Terminal and its features to implement the REPL:

```html
<link href="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/css/terminal.css"
      rel="stylesheet"/>
<script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/lib/js/terminal.js"></script>
```

## Include Prism files

[PrismJS](https://prismjs.com/) is a library that you can use for the syntax highlighting. It has
generic Scheme support, but LIPS tweak that a bit and add support for LIPS Scheme specific syntax
like literal Regular Expressions, or object literals.

```html
<script src="https://cdn.jsdelivr.net/combine/npm/prismjs/prism.js,npm/jquery.terminal/js/prism.js,npm/prismjs/components/prism-scheme.min.js"></script>
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/terminal-prism/css/prism-coy.css"/>
```

`terminal-prism` npm package contain versions of the Prism CSS that is transformed for the jQuery Terminal.

## JavaScript code
This is the code that use features of jQuery Terminal and main Terminal LIPS code to create the working REPL:

```javascript
// Scheme syntax highlighting
$.terminal.syntax('scheme');

const term = terminal({
    selector: '#term', // you can use body for full screen terminal
    dynamic: false, // dynamic scope, to have normal Scheme you should use false
    name: 'demo', // name of the terminal if you want multiple REPL on same page
    lips // LIPS namespace
});
```

If you want LIPS greeting like in lips website you can also add this code:

```javascript
// LIPS Greetings
const intro = `(λ LIPS) version ${lips.version}
Copyright (c) 2018-${new Date().getFullYear()} [[!;;;;https://jcubic.pl/me]Jakub T. Jankiewicz]
Type (env) to see environment with functions macros and variables
You can also use (help obj)to display help for specific function or macro.
Use (dir name) to get list of JavaScript properties and (apropos name)
to display list of matched names in environment.
`
// use same highlight for names in RegEx, so they are in color and have doc strings
term.echo(intro.replace(/(\((?:env|dir|help|apropos)[^)]*\))/g, function(code) {
    return $.terminal.prism('scheme', code, { echo: true });
}), {
    formatters: false
});
```

## Working Demo

Here is a working [Codepen REPL Demo](https://codepen.io/jcubic/pen/OJepZbd).
