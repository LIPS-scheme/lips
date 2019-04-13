## 0.9.0
### Features
* better binary (options -f -c + read stdin + REPL + indent + catch exception)
* indent API function
* allow to use dots and variable names
* `.` function now work with multiple arguments and gets nested object value
* add `undefined` to default Environment so you can use it in Lips code
* add function `nop`
* tokenizer with meta data now include newlines and comments (nothing is removed)
### Bugs
* fix offset in tokenizer
* fix assign variables from variables (e.g. function or variable aliases)
* fix return value of Math operation that should return float [#1](https://github.com/jcubic/lips/issues/1)
* fix tokenizer meta data with strings outside of S-Expressions or in lonely lines
* fix print of lips Symbols (update string function)
* fix missing recursive calls to unquote in quasiquote macro
* fix parser when using multiple specials (unquote, quote etc.) in a row
* fix scope when evaluating macros

## 0.8.1
### Bugfix
* use exception.code for code that triggered exception

## 0.8.0
### Features
* new nth and reverse functions
* new type checking functions null? regex? pair? string? number? symbol? array? object? boolean?
* add lips source code that throwed exception in JavaScript error message
### Bugfix
* fix lambda with rest parameter
### Breaking
* `if` now check if argument is boolean

## 0.7.1
### Bugfix
* fix curry function

## 0.7.0
### Features
* `(let iter` macro that's transformation of `(let* ((iter (lambda () ...`
* expandable `(let iter` and `(define (foo)` by macroexpand
### Bugfix
* fix macroexpand evaluation of code
* use nil in cons when cdr is empty list

## 0.6.1
### Bugfix
* fix reduce infinite loop
* fix invoking lambdas in dynamic scope
* fix print of Array with multi line strings and BigInts

## 0.6.0
### Breaking
* change api for evaluate to have object `{env, dynamic_scope, error}`
* rename set to set!, set-cdr to set-cdr! and set-car to set-car!
* rename defmacro to define-macro like in scheme
* change order of arguments in env.inherit
## Features
* add bit operation functions
* add float, round and ceil functions
* add env variable to lips namespace which is alias to global_environment
* add find and for-each higher order functions
* add length function
* add new function which create instance of objects
* allow to set value using set! and dot expression that is used to get the value
* better invalid mime on script tag detection
* use ES6 symbols as names for lips symbols in gensym function
### Bugs
* show error when invoking trampoline in dynamic scope
* fix eq? and >= functions
* fix set! (change existing reference or create new if not existing in scope chain)
* fix return value of if macro
* fix defmacro
* fix parsing expressions that have multiple special characters in a row
* handle float calculations
* throw exception when try to execute native non function value
* fix resolving Promises when evaluating code
* don't allow regex that start with space (so it work when you have two divisions in one S-Expression)
* fix parsing float numbers that start with dot
* prevent evaluation of quoted expressions in macros
* fix macro sharing body between invocations
* fix read function
* fix parsing boolean values
* fix map, reduce and filter to handle async functions
* fix access variables that have value of undefined

## 0.5.4
### Bugs
* use src file for node and dist build file for unpkg

## 0.5.3
### Bugs
* fix version number in exported field

## 0.5.2
### Bugs
* use dist/lips.min.js as main for unpkg
* npm housekeeping

## 0.5.1
### Bugs
* fix lambda with symbol as parameters
* fix for TCO factorial using Y combinator & trampoline

## 0.5.0
### Breaking
* exec now return promise with array of results
* all numbers are wrapped with LNumber
### Features
* add support for new u and s regex flags
* optional dynamic scope
* add `not`, `abs`, `sqrt` and `**` functions
* wrap numbers in LNumber that use BigInt or bn.js if possible
### Bugs
* fix lambda with no parameters
* fix define with Promise like `(define x (let ...`
* fix - with single argument

## 0.4.1-2
### Bugs
* fix for Node

## 0.4.0
### Features
* lambda with reset parameter (like in scheme)
* new functions: 1+, 1-, curry and range
* execute LIPS code from script tag with text-x/lips type attribute
* add string functions: join, split, replace, match, search
* new second parameter to `tokenize` that make it return array of extra objects `{token, col, line, offset}`
* Pair.flatten and lips function flatten

### Bugs
* fix (reduce + list)
* fix handling of empty list
* make let* handle promises
* fix evaluate of macro that return a Promise
* fix evaluating code with promises

## 0.3.0
### Features
* exec api method
### Bugfixes
* fix processing of multiple backquote and unquote

## 0.2.1/0.2.2
* fix build system

## 0.2.0
### Features
* Add reduce and set functions
* add while, ++ and -- macros
* ignore comments everything after ; but not inside strings and regexes
* gensym and load functions
* better string function
* Pair methods for working with ALists + Pair::reduce
* throw exception on car/cdr with non list

### Bugs
* fix parsing empty strings
* fix various errors catch by lint
* fix parsing ALists with list as keys and values
* fix parsing quasiquote that evaluate to single pair out if unquote

## 0.1.0
* Initial version
