---
sidebar_position: 1
---

# Getting Started

## Browser

When using LIPS Scheme interpreter in browser you need to include the main script file.

```html
<script src="https://unpkg.com/@jcubic/lips@beta/dist/lips.min.js"></script>
```

or [jsDelivr](https://www.jsdelivr.com/) that is somewhat faster:

```html
<script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js"></script>
```

After adding script tag with main file, you can use Scheme code inside script tag:

### Running Scheme Code Inline

```html
<script type="text/x-scheme" bootstrap>
(let ((what "world")
      (greet "hello"))
   (display (string-append "hello" " " what))
   (newline))
</script>
```

**NOTE**: Only the core of LIPS is written in JavaScript, almost half of it it's written in Scheme.
So if you want to load the standard library (to have full LIPS), you should use `bootstrap` or
`data-bootstrap` attribute that will load it for you. You can optionaly specify the location of the
file.

```html
<script type="text/x-scheme" bootstrap="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/std.xcb">
(let ((what "world")
      (greet "hello"))
   (display (string-append "hello" " " what))
   (newline))
</script>
```

`xcb` file is simple binary format that LIPS uses to speed up parsing the the code. You can also use
`.scm` file or `.min.scm` file that may be little bit bigger.

**NOTE** The `bootstrap` attribute can also be included on main script tag with the JavaScript file.

### Running External Scheme Code

You can also use `src` attribute to link to source file. Like you normally do with JavaScript:

```html
<script type="text/x-scheme" src="example.scm"><script>
```

## Node.js

To install LIPS you can use NPM:

```bash
npm install -g @jcubic/lips@beta
```

You should use beta, because the so call stable version is really old and outdated. Because of so many
breaking changes no new stable version was released and instead 1.0 beta started.

If LIPS is installed globally just use `lips` command to start the REPL:

![LIPS REPL sesssion in Terminal](/img/screencast.gif)

By default, splash screen is shown you can hide it with option `-q`. If you're using bash you can create an
alias:

```bash
alias lips='lips -q'
```

and you will not see the splash again.

### Executing files

You can also execute scheme code with:

```bash
lips foo.scm
```

Note, that with lisp executable you don't need to manually bootstrap the standard library. But you can change
which file is loaded or disable the loading of the file completely using `--bootstrap` flag.

```bash
lips --bootstrap dist/std.scm foo.scm
```

This will run foo.scm file and bootstrap from main scheme file.

```bash
lips --bootstrap none foo.scm
```

This will run the code without loading the standard library. So LIPS will have only functions
and macros defined in JavaScript. This is called Core of LIPS with most of the essentials.

### Executing expressions

You can execute expression with `-e` flag (short of `eval`):

```bash
lips -e '(print "hello world")'
```

### Standalone scripts

You can also write scripts using LIPS with [shebang](https://en.wikipedia.org/wiki/Shebang_(Unix)).
This extension is defined in [SRFI-22](https://srfi.schemers.org/srfi-22/srfi-22.html).

```scheme
#!/usr/bin/env lips
(let ((what "World"))
  (print (string-append "Hello " what)))
```

If you write code like this and save it in `script.scm` on Unix like systems (Linux, macOS, or Windows with WSL)
you can change the execution permission:

```bash
chmod +x script.scm
```

and execute the script by providing the name:

```bash
./script.scm
```

**NOTE**: by default most systems don't execute files in current directory so you need to provide `./` in front.
You can change that if you add dot (current working directory) to the `$PATH` environment variable:

```bash
export $PATH=".:$PATH"
```

### Node.js project

Afeter you have installed LIPS you can create a new Node.js project and write LIPS Scheme code
instead of JavaScript, using everything Node.js provides. See documentation about [Integration with
JavaScript](/docs/lips/intro#integration-with-javascript).

```bash
mkdir my-project
cd my-project
npm init -y
```

Then you can install npm packages

```bash
npm install braces
```

and use them in LIPS Scheme:

```scheme
(define braces (require "braces"))

(write (braces "{01..10}" &(:expand #t)))
;; ==> #("01" "02" "03" "04" "05" "06" "07" "08" "09" "10")
```

**NOTE**: [braces](https://www.npmjs.com/package/braces) is a popular package to expand bash like
expressions, it's used as [deep dependency for
TailwindCSS](https://shubhamjain.co/2024/02/29/why-is-number-package-have-59m-downloads/).

## Executing LIPS prammatically

You can also execute LIPS from JavaScript:

```javascript
const { exec } = require('@jcubic/lips');
// or
import { exec } from '@jcubic/lips';

exec('(let ((a 10) (b 20)) (* a b))').then(result => {
    results.forEach(function(result) {
        if (typeof result !== 'undefined') {
            console.log(result.toString());
        }
    });
});
```

`exec` is the main function that can be used to evaluate expressions. It returns a Promise of Array
of results.

### Creating REPL

If you want to create REPL or similar thing you can use Interpreter interface which allow to change
stdin and stdout.

```javascript
import { Interpreter, InputPort, OutputPort } from '@jcubic/lips';

const interpreter = Interpreter('<name>', {
    stdin: InputPort(function() {
        return new Promise(function(resolve) {
          // resolve with a string when data is ready
        });
    },
    stdout: OutputPort(function(obj) {
        // you will get any obect and need to print it
        // you can use this.get('repr') function from LIPS environment
        // to get represention of the object as string
        if (typeof obj !== 'string') {
            obj = this.get('repr')(obj);
        }
    })
});
```

Anything you add to the object passed to Interpreter will be added to global scope.

The Interpreter have a method `exec` that work the same as thhe one exported from LIPS.

### Bootstraping

**Note**: that you also need to bootstrap the standard library to have fully working Scheme system.

```javascript
await interpreter.exec(`(let-env lips.env.__parent__
                          (load "<path or URL>/dist/std.xcb"))`);
```

`lips.env` is user environment and `__parent__` is real top level global environment.  To see more
about `let-env` expression check [documentation about LIPS environments](/docs/lips/environments).
