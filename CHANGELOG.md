## 0.21.0
### Breaking
* rename `ceil` to `ceiling` (like in R5RS spec)
* break `let*` to work the same as in scheme (it was working like `letrec`)
* `if` no longer require boolean as cond
* change how arrays are presented, they are look like scheme vector `#(...)`
* rename `string` to `repr`
* new API `lips.Intepreter` - for using interpreter that have different stdout/stdin
### Features
* start implementing R5RS spec
* streams example
* `<`, `>`, `==`, `<=` and `>=` now have variable number of arguments
* new `throw` function that throw JS exception
* expose `typecheck` function that test type of the arguments and throw exception if don't match
* new `gdc` and `lcm` functions
* new `string->number` and `number->string` functions
* new `list?` function
* new `vector-ref` (same as `(. v n)` but with typecheck)
* new `+inf.0` and `-inf.0`
* new `max`, `min` and `truncate`
* restore `print` it's now alias for display
* you can now access nested objects as symbols (e.g `(lips.LNumber 10)`)
* new Number types (complex and rational)
* input/output ports + string ports
* add `letrec` macro
* `inexact->exact` and `rationalize` (thanks to Kawa/C-Gambit projects)
* parsing hex, octal, binary, complex, rational numbers
* characters literals `#\A` and character and string functions
* display `#t` and `#f` for boolean values in terminal
* new `interaction-environment` function + `load` use that env by default
* improve executable
* user can define string representation (with object that have `toString` lambda)
* Hygienic macros: `syntax-rules` transformer macros
### Bug fixes
* fix list function [#27](https://github.com/jcubic/lips/issues/27)
* fix `(begin (gensym))` macro that evaluate to symbol
* update defstruct to export only needed functions (local functions are namespaced)
* prevent of evaluation of `(define (x . a) a)) (x 1 2 3)` (`list` in scheme)
* fix `LNumber::String` with radix
* fix formatting of float numbers
* fix try..catch
* fix display of function/macro docs (remove white space in parser)
* fix loading modules with `require`
* fix macro expand when expansion return list with macro that is data
* fix lips executable in GNU Emacs run-scheme (comint mode)
* unbox values in new
* fix named let with nil [#46](https://github.com/jcubic/lips/issues/46)

## 0.20.1
### Bug fixes
* fix formatter indent

## 0.20.0
### Breaking
* replace `print` with `display` (more like scheme)
* replace `lips.Symbol` with `LSymbol`
* replace `delay` with `wait` (to allow using scheme delay/force)
* change `timer` function to just `setTimeout` with expression (old `timer` is now `wait`)
### Features
* improve `-->` macro to handle numbers and strings and symbols (quoted or not)
* new function `quoted-symbol?`
* allow to use else in cond (same as true - similar to scheme)
* new macro: `promise`
* experimental parent.frame (inspired by R)
* add `arguments.callee` to lambdas
### Bug fixes
* fix for-each with function that use this (like print)
* fix pretty print of lisp code
* fix formatting/indent of list of full S-Expressions

## 0.19.0
### Features
* add require.resolve function
### Bug fixes
* fix stack trace on Node.js errors
* fix display of functions as values in executable
* require is defined only in Node.js

## 0.18.2
### Bug fixes
* bootstrap helpers.lips in executable

## 0.18.1
### Bug fixes
* fix auto indent and add version number to executable

## 0.18.0
### Features
* add copyright notes to binary and improve the calling it
* allow to call lips from shebang
### Bug fixes
* trim lines in functions doc strings [#23](https://github.com/jcubic/lips/issues/23)

## 0.17.2
### Bug fixes
* fix list of false values [#22](https://github.com/jcubic/lips/issues/22)

## 0.17.1
### Bug fixes
* fix map last value [#21](https://github.com/jcubic/lips/issues/21)

## 0.17.0
### Features
* revert breaking change of read to return Array
* use proper read function in executable REPL

## 0.16.3
### Bug fixes
* fix map on empty list
* fix exception when calling error

## 0.16.2
### Bug fixes
* unbind functions before setting property with `set-obj!`
* make gensyms names shorter (like in CLISP)
* fix double quasiquote + unquote unquote splice (and other quirks)

## 0.16.1
### Bug fixes
* fix setting property Descriptor on functions (to change name for preact components)

## 0.16.0
### Features
* `this` inside lambdas that are object created with new (same as in JS) [#19](https://github.com/jcubic/lips/issues/19)
* `string` and `type` return `instance` for objects created by `new`
* `pprint` function [#17](https://github.com/jcubic/lips/issues/17)
### Bug fixes
* fix set-obj! on functions [#18](https://github.com/jcubic/lips/issues/18)
* fix string formatting [#16](https://github.com/jcubic/lips/issues/16)

## 0.15.4
### Bug fixes
* fix formatter when there is newline at the newline of the code
* fix duplicated copyright notes and missing Facebook note in output from rollup and babel

## 0.15.3
### Bug fixes
* fix regenerator from babel in dist files

## 0.15.2
### Bug fixes
* fix `ignore` macro
* fix `load` function in node
* fix `Pair::append` and `append` functions with non pair
* throw exception when more unquote then quasiquotes
* fix double unquote symbol
* throw error when unquote-splice on atom in the middle of the list
* don't create cycles in unquote-splicing
* fix cloning cycles

## 0.15.1
### Bug fixes
* fix `load` function outside of Node
* return undefined from `load` function
* fix formatting of lambda expressions
* fix `symbol->string` function when called with gensym symbols
* improve `-->` and `..` macros
* quote `define-macro` arguments so they don't evaluated if inside expression

## 0.15.0
### Features
* new macro `..`
* formatter rules for example `cond` macro
* `tree->array` function
* `__code__` inside Macro
* `define-formatter-rule` macro for breaking S-Expressions in Formatter
### Bug fixes
* fix lips.Formatter pattern matching
* better toString and `string` functions (keep class names for internal objects in minified js file)
* fix `__code__` on lambda expressions (that also includes `(define (name . arg)`)
* evaluate of symbols and promises returned from macros

## 0.14.0
### Features
* add `unset!`, `add-special!` and `remove-special!` functions
* add `define-symbol-macro` meta macro that create parser macros (parser transformer symbol)
* rewrite `help` as macro so it works with parser macros
* `require` function for Node.js + `load` work in Node using `fs.readFile`
* stack trace in exception in lips binary
* parser accept strings as argument (no need to call tokenizer - but it also accept array of tokens)
* modules mechanism as macros
* pattern matching as macro
* `macro?` function
* `cond` macro
### Bug fixes
* fix white space in docs for macros
* fix two cases in parser: `` `(,(list 1 2)) `` and `` `(+ ,,(list 'foo)) `` [#12](https://github.com/jcubic/lips/issues/12)
* fix quaisquote macro (eval of single unquote in double quasiquote) [#14](https://github.com/jcubic/lips/issues/14)
* add scope to exec in `load` function
* fix formatting of multi line strings
* fix `object?` when called with nil and number
* fix `macroexpand` macro

## 0.12.0
### Features
* new macro `begin*` that run their arguments in parallel if they async
* add typecheck to rest of the functions
* new `real?` and `pluck` functions
* stack trace for exceptions
### Bug fixes
* fix `find` and `filter` to check if function from argument return nil
* fix accessing FileReader object (try to copy read only prototype)
* fix parser error when parsing symbol macros [#10](https://github.com/jcubic/lips/issues/10)
* fix undefined values in macro with missing arguments [#11](https://github.com/jcubic/lips/issues/11)
* fix macroexpand [#9](https://github.com/jcubic/lips/issues/9)
* fix line breaking in S-Expression Formatter

## 0.11.2
### Breaking
* remove `nop` (you can use `(begin undefined)`)
### Bug fixes
* fix unquote-splicing on nil (e.g. processing of `` `(list ,@nil)``)
* cdr and car should return nil on empty list
* typecheck and process nil in c[ad]+r functions

## 0.11.1
### Bug fixes
* fix parsing special forms as literal symbols [#7](https://github.com/jcubic/lips/issues/7)
* fix unquote-splicing of empty list (e.g. processing of `` `(list ,@(list))``)

## 0.11.0
### Features
* Add support to list cycles
* new macro `ignore` that will swallow promises
* add `try` macro
* add `current-environment` function
* `eval` LIPS function now accept environment as second argument
* better toString of gensym symbols (it now shows `#gensym_1#` instead of `Symbol(#gensym)` check `macroexpand`)
* macroexpand now produce formatted LIPS code (API exposed in Formatter::break that break LIPS code into lines)
### Bug fixes
* call error in evaluate when promise returned from function is rejected
* fix errors in `map`, `reduce`, `fold` and `for-each` (that use `map`)
* fix exception when call to match fail to match

## 0.10.4
### Bug fixes
* fix accessing props on native objects like Object function [#6](https://github.com/jcubic/lips/issues/6)

## 0.10.3
### Bug fixes
* fix print that get stdout from global env not from user one + better babel fix added in 0.10.1

## 0.10.2
### Bug fixes
* fix version number in source code

## 0.10.1
### Bug fixes
* fix for babel when using for-each (weakBind was not working because babel was using `apply(void 0, ...`)
* add missing `error` function used in example macros

## 0.10.0
### Breaking
* change order of `split`, `replace`, `match`, `search` and `instanceof` so you can use them with `curry`
* `append` and `append!` no require list or nil second argument
* apply is no longer Macro it's now a function (so it can be used in curry)
### Features
* change `append!` from macro to function (it will work with any value as fist list argument not only with names
* `help` function and help system inside functions. First expression after function arguments is docs.
* add `pipe`, `compose`, `fold` and `some` functions
* new function `unbind` to help write function that work with functions as objects (LIPS wrap them)
* add alias for `.` as `get` so you can pass it to curry, with `.` LIPS will try to create pair
* lambda function introspection
* hyperapp example
* better loading of code
* make `map`, `filter`, `find`, `for-each` and `reduce` sync when possible
* basic type checking
* documented `unquote-splicing` and `unquote` functions that throw error when used outside of `quasiquote`
* allow to extend the parser (kind of reader macros)
### Bug fixes
* fix `if` it should not return false if false value is undefined
* fix even and odd functions with BigInt
* fix curry
* fix when inside quasiquote there is empty list
* make `load` use `lips.exec`, so it execute code sequentially
* fix `append`
* fix `timer` to use with expression
* fix macros without arguments
* fix calling `setTimeout` [#5](https://github.com/jcubic/lips/issues/5)

## 0.9.0
### Breaking
* `for-each` and `map` works as in Scheme spec (use multiple arguments and don't use index)
### Features
* better binary (options -f -c + read stdin + REPL + indent + catch exception)
* indent API function
* allow to use dots and variable names
* `.` function now work with multiple arguments and gets nested object value
* add `undefined` to default Environment so you can use it in Lips code
* add function `nop`
* tokenizer with meta data now include newlines and comments (nothing is removed)
### Bug fixes
* fix offset in tokenizer
* fix assign variables from variables (e.g. function or variable aliases)
* fix return value of Math operation that should return float [#1](https://github.com/jcubic/lips/issues/1)
* fix tokenizer meta data with strings outside of S-Expressions or in lonely lines
* fix print of lips Symbols (update string function)
* fix missing recursive calls to unquote in quasiquote macro
* fix major issues with quasiquote that make macros don't work quite right
* fix parser when using multiple specials (unquote, quote etc.) in a row
* fix scope when evaluating macros
* fix async issues with list values inside let, lambda and begin macros
* don't evaluate next expressions in `and` and `or` macros

## 0.8.1
### Bug fixes
* use exception.code for code that triggered exception

## 0.8.0
### Features
* new nth and reverse functions
* new type checking functions null? regex? pair? string? number? symbol? array? object? boolean?
* add lips source code that throwed exception in JavaScript error message
### Bug fixes
* fix lambda with rest parameter
### Breaking
* `if` now check if argument is boolean

## 0.7.1
### Bug fixes
* fix curry function

## 0.7.0
### Features
* `(let iter` macro that's transformation of `(let* ((iter (lambda () ...`
* expandable `(let iter` and `(define (foo)` by macroexpand
### Bug fixes
* fix macroexpand evaluation of code
* use nil in cons when cdr is empty list

## 0.6.1
### Bug fixes
* fix reduce infinite loop
* fix invoking lambdas in dynamic scope
* fix print of Array with multi line strings and BigInts

## 0.6.0
### Breaking
* change api for evaluate to have object `{env, dynamic_scope, error}`
* rename set to set!, set-cdr to set-cdr! and set-car to set-car!
* rename defmacro to define-macro like in scheme
* change order of arguments in env.inherit
### Features
* add bit operation functions
* add float, round and ceil functions
* add env variable to lips namespace which is alias to global_environment
* add find and for-each higher order functions
* add length function
* add new function which create instance of objects
* allow to set value using set! and dot expression that is used to get the value
* better invalid mime on script tag detection
* use ES6 symbols as names for lips symbols in gensym function
### Bug fixes
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
### Bug fixes
* use src file for node and dist build file for unpkg

## 0.5.3
### Bug fixes
* fix version number in exported field

## 0.5.2
### Bug fixes
* use dist/lips.min.js as main for unpkg
* npm housekeeping

## 0.5.1
### Bug fixes
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
### Bug fixes
* fix lambda with no parameters
* fix define with Promise like `(define x (let ...`
* fix - with single argument

## 0.4.1-2
### Bug fixes
* fix for Node

## 0.4.0
### Features
* lambda with reset parameter (like in scheme)
* new functions: 1+, 1-, curry and range
* execute LIPS code from script tag with text-x/lips type attribute
* add string functions: join, split, replace, match, search
* new second parameter to `tokenize` that make it return array of extra objects `{token, col, line, offset}`
* Pair.flatten and lips function flatten
### Bug fixes
* fix (reduce + list)
* fix handling of empty list
* make let* handle promises
* fix evaluate of macro that return a Promise
* fix evaluating code with promises

## 0.3.0
### Features
* exec api method
### Bug fixes
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
### Bug fixes
* fix parsing empty strings
* fix various errors catch by lint
* fix parsing ALists with list as keys and values
* fix parsing quasiquote that evaluate to single pair out if unquote

## 0.1.0
* Initial version
