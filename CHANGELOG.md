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
