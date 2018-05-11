## 0.4.0
### Features
* lambda with reset parameter (like in scheme)
* new functions: 1+, 1-, curry and range
* execute LIPS code from script tag with text-x/lips type attribute
* add string functions: join, split, replace, match, search
* new helper function `lips.tokens` that return array of objects `{token, offset}`

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
