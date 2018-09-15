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
