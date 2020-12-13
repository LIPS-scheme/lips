---
layout: post
title:  "Writing Web Worker Code in Scheme"
date:   2020-11-11 22:29:07 +0100
author: jcubic
description: TODO
image:
  url: /img/web-worker.jpg
  alt: "Farmer with Shovel and text Webworker and Lambda character inside parenthesis"
---

In this article I will show how to use LIPS Scheme inside web worker. Which is basically separated thread
that can run in browser that will not freeze the page when you're doing calculation.

<!-- more -->

We can write JavaScript code that will create worker and call rpc method on that worker.
This is how the API look like in JavaScript.

```javascript
var url = 'http://localhost/projects/jcubic/lips'
var worker = new lips.Worker(url);
(async function() {
    const [ result ] = await worker.rpc('eval', [
       `(let* ((what "hello")
            (who "world")
            (message (string-append what " " who)))
       (display message)
       message)`
   ]);
   console.log(result);
})();
```

but this is not very interesting and don't look very good. Lets look how to do the same in Scheme code.
Lets write code that will write file in worker using lighting-fs library
First you need to include the script tag with LIPS library.

```html
<script src="https://cdn.jsdelivr.net/npm/@jcubic/lips@beta/dist/lips.min.js"></script>
```

then you can write your script that with `type="text/x-scheme"` and inside your scheme code:

```html
<script type="text/x-scheme">
;; your scheme code here
</script>
```

First thing to do in scheme, is to bootstrap the LIPS Scheme system.

```scheme
(define url "http://localhost/projects/jcubic/lips")
(begin
  ;; bootstrap LIPS Scheme system
  (load (concat url "/lib/bootstrap.scm") lips.env.__parent__)
  (load (concat url "/lib/R5RS.scm") lips.env.__parent__)
  (load (concat url "/lib/R7RS.scm") lips.env.__parent__))
```

When we bootstrap the system we need to use code functions (`concat` is defined in JavaScript, where
`string-append` is scheme function defined in R5RS and it's just alias for `concat`).

Then we can create web worker with LIPS API:

```scheme
(define worker (new lips.Worker url))
```

worker interface expose `rpc` method that you can use to evaluate code (the method will return a promise
that will be resolved when worker return the value, which is hidden by automatic unwrapping of promises):

```scheme
(worker.rpc "eval" (vector (--> '(display "hello, worker!") (toString true))))
```

interface require all arguments to the methods to be in array (scheme vectors) and to call eval you need to pass a string.
That's why the code call method `toString` on quoted list to get the string, the same can be done using standard
`repr` function, or using standard scheme code that do the same (using [SRFI-6](https://srfi.schemers.org/srfi-6/srfi-6.html)).


```scheme
(define (expr->string expr)
  "(expr->string expr)

  Function convert expression to string using write method and string port."
  (let ((out (open-output-string)))
    (write expr out)
    (get-output-string out)))

(worker.rpc "eval" (vector (expr->string '(display "hello, worker!"))))
```

In both examples it will invoke the `display` function that will console log the string.

The code to invoke rpc method don't look very nice, lest write macro that will wrap the usage of that API.

```scheme
(define-macro (worker.send . args)
   `(worker.rpc "eval" (vector ,(apply string-append (map expr->string args)))))

;; instead of expr->string you can also use lambda:
(lambda (code) (--> code (toString true)))
```

After you have this macro you can use it like this:

```scheme
(worker.send (display "hello, worker!"))
```

Lets do something more interesting. Lets create a file inside web worker and read it outside.
We will use [lightning-fs](https://github.com/isomorphic-git/lightning-fs) library (that's part
of [isomorphic-git](https://isomorphic-git.org/)).

First we will write the file `worker.scm`, that will write some string to a file.

```scheme
(let ((base "https://cdn.jsdelivr.net/npm/@isomorphic-git/lightning-fs"))
  (importScripts (string-append base "/dist/lightning-fs.min.js")))

(define fs (let ((fs (new LightningFS)))
              (fs.init &(:wipe #t
                         :fileDbName "worker"))
              fs.promises))

(fs.writeFile "/test.txt" "hello, world! from file written in webwoker")
```

in worker we don't need to bootstrap scheme system because when we create a worker we pass URL that
point to LIPS and worker interface will load all necessary files for you.

The script will import UMD module of the library from jsdelivr create instance of the file system.
Using object literal `&(:foo #t)` will create JavaScript object `{foo: true}`. Passing `#t` to
wipe mean that we get fresh file system each time. After creating the fs object code code get promises
field that will have promisifed version of all functions, so it will work nicely with automatic
resolving of promises in LIPS Scheme.

Next line is calling method writeFile on fs object that will write content of the file in indexedDB
(used by lighting-fs as storage).

Now lets read that outside in main code:

```scheme
;; url was defined at the beginning we will need to pass whole path to make load work
(worker.send (load `(string-append ,url "/worker.scm")))
```

and read the file:

```scheme
(define fs (let ((fs (new LightningFS "worker")))
              (fs.init &(:fileDbName "worker"))
              fs.promises))
(display (fs.readFile "/test.txt" "utf8"))
```

The code for creating instance of lighting fs is almost the same except there are no wipe option set to `true`,
so we can read the file that was written inside worker.



