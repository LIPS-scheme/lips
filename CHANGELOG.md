## 1.0.0-beta.19.1
### Feature
* allow to call `load` with `@lips` prefix [#354](https://github.com/jcubic/lips/issues/354)
### Bugfix
* fix duplicated identifiers in syntax-rules (case of SRFI-239 example implementation)
* fix `load` of absolute path in Node
* fix require of LIPS package from Node

## 1.0.0-beta.19
### Breaking
* change `get-environment-variables` and `get-environment-variable` returns nil and undefined in the browser
* `true`, `false`, and `NaN` are no longer parser constants [#292](https://github.com/jcubic/lips/issues/292)
* get rid of node polyfills from output files
* `eval` use `interaction-environment` stead of `current-environment` by default
* remove support for string argument to `read` [#327](https://github.com/jcubic/lips/issues/327)
* make `null` falsy value [#330](https://github.com/jcubic/lips/issues/330)
* remove `nil` parser constant [#332](https://github.com/jcubic/lips/issues/332)
* replace `undefined` with `#void`, and `null` with `#null`
* characters are again unboxed into strings by JavaScript code [#329](https://github.com/jcubic/lips/issues/329)
* code that throw exception now return exit code 1
* change order of arguments in `take`
* `(type #void)` (JavaScript `undefined`) is now `void`
### Features
* add `vector-for-each` and `vector-copy!` function from R7RS
* add `string-for-each`, `string-downcase`, and `string-upcase` from R7RS
* add `typecheck-number` function
* add `char-foldcase` and `string-foldcase` functions
* add `list-set!` and `nth-pair` functions
* add `SRFI-61`, `SRFI-139`, and `SRFI-210`
* add `continuations?` function
* add `iterator->array` function
* add immutable strings as in R7RS spec [#285](https://github.com/jcubic/lips/issues/285)
* add R7RS `char<...>?` and `string<...>?` functions [#298](https://github.com/jcubic/lips/issues/298)
* improve syntax-rule exception message (appending macro code)
* update `log` to accept two arguments [#301](https://github.com/jcubic/lips/issues/301)
* allow to use `data-bootstrap` attribute on script tags
* make `atan` work for complex numbers
* save error logs in home dir
* add timestamp to error logs
* make `+inf.0` and `-inf.0` parser constants
* add `zip` procedure (inspired by [Lodash](https://docs-lodash.com/v4/zip/))
* add `with-input-from-string` and `read-all` [#327](https://github.com/jcubic/lips/issues/327)
* add `#!fold-case` and `#!no-fold-case` directives [#342](https://github.com/jcubic/lips/issues/342)
* add `drop` procedure
* allow to use unquoted symbols in `-->` to get the function
* add `regex` procedure (rename from `%r` used in bootstrap.scm)
* support unterminated strings in REPL [#280](https://github.com/jcubic/lips/issues/280)
* expose parser and lexer instance from syntax extensions [#308](https://github.com/jcubic/lips/issues/308)
### Bugfix
* fix `let-values` to allow binding to list [#281](https://github.com/jcubic/lips/issues/281)
* fix wrong strings in `string-fill!`
* fix `string-set!` to mutate the string and work on any expression
* fix tokenizing regex that ends with hash
* fix bookmark on SRFI-210 document [#287](https://github.com/jcubic/lips/issues/287)
* fix `syntax-rules` in macro `define-value` [#290](https://github.com/jcubic/lips/issues/290)
* fix indentation of `let-values`
* various fixes to `syntax-rules`
* fix `procedure?` to return true for continuations
* fix `lips --help` screen
* fix `cond-expand` to skip not-matched symbols
* fix shadowing `syntax-rules` identifiers with let [#291](https://github.com/jcubic/lips/issues/291)
* fix nested syntax rules with symbols after nested ellipsis
* fix Dark Mode colors and scrolling of the page when using Bookmarklet on English Wikipedia
* remove dependencies on `cond` from `-->` macro so you can use use `-->` inside `cond`
* fix handling of recursive `flatten` `syntax-rules` macro [#304](https://github.com/jcubic/lips/issues/304)
* fix syntax-rules macro that manipulate code (see tests/syntax.scm and undswap macro)
* fix `(read)` in the Web REPL
* fix Node REPL with `npm install` [#305](https://github.com/jcubic/lips/issues/305)
* fix formatting `syntax-rules`
* improve floating point representation and parsing
* fix bookmark when hovering over boolean values
* fix quoted cycle in REPL [#313](https://github.com/jcubic/lips/issues/313)
* fix set-repr! on base class [#314](https://github.com/jcubic/lips/issues/314)
* fix `repr` of delay expressions [#315](https://github.com/jcubic/lips/issues/315)
* fix `try..catch` [#317](https://github.com/jcubic/lips/issues/317)
* fix handling `^` and `$` syntax extension [#318](https://github.com/jcubic/lips/issues/318)
* fix mixed values in `let*-values` [#322](https://github.com/jcubic/lips/issues/322)
* fix `do` macro [#324](https://github.com/jcubic/lips/issues/324)
* fix `string->number` that leads to NaN [#326](https://github.com/jcubic/lips/issues/326)
* fix unintentional unboxing in `iterator->array` [#328](https://github.com/jcubic/lips/issues/328)
* fix `replace` with async `lambda` [#319](https://github.com/jcubic/lips/issues/319)
* fix `values` without arguments [#331](https://github.com/jcubic/lips/issues/331)
* improve working of REPL in Emacs
* fix `(expt +i +i)`
* fix let with duplicated variables [#335](https://github.com/jcubic/lips/issues/335)
* fix escape ellipsis in syntax-rules [#334](https://github.com/jcubic/lips/issues/334)
* fix parsing inexact complex without real part and `inexact->exact` procedure [#340](https://github.com/jcubic/lips/issues/340)
* fix Petrofsky let [#341](https://github.com/jcubic/lips/issues/341)
* fix `repr` of cycles
* fix parsing regex that have escaped open bracket
* fix parsing quotation without data
* fix reading syntax-extensions from input ports
* fix parsing syntax-extensions that start with `#f` or `#t` [#343](https://github.com/jcubic/lips/issues/343)
* fix repr of curried function (output of `curry`)

## 1.0.0-beta.18
### Breaking
* change undocumented arguments to `lips.exec` into an object
* change default export into named exports in ES Module
### Features
* add R7RS `guard` macro
* add R7RS `parameterize` and `make-parameter`
* add `shuffle` function
* add support to ES Modules [#254](https://github.com/jcubic/lips/issues/254)
* add support for `(scheme-report-environment 7)`
* add a way to compare custom instances with `equal?` [#270](https://github.com/jcubic/lips/issues/270)
* add support for `equal?` on records [#273](https://github.com/jcubic/lips/issues/273)
* expose `lips.set_fs` function [#256](https://github.com/jcubic/lips/issues/256)
### Bugfix
* remove evaluating of async list data as first argument
* fix `number->string` for binary numbers
* fix macro expand on let with more than one binding
* fix shallow `list->array`
* fix resolving promises inside quoted promise realm
* fix undocumented symbol syntax extensions
* fix odd? even? on non integers
* fix object literals with null value [#264](https://github.com/jcubic/lips/issues/264)
* fix version and date in executable [#261](https://github.com/jcubic/lips/issues/261)
* fix error on extra close parenthesis [#263](https://github.com/jcubic/lips/issues/263)
* fix `scheme-report-environment` [#268](https://github.com/jcubic/lips/issues/268)
* fix writing to binary ports
* fix unboxing object literals
* throw error on comparing complex numbers [#248](https://github.com/jcubic/lips/issues/248)
* make `integer?` works for normal native numbers
* fix parsing newline character literal
* fix reading local files in browser [#276](https://github.com/jcubic/lips/issues/276)
* fix parsing invalid expression `(1 . 2 3)` [#245](https://github.com/jcubic/lips/issues/245)
* fix invalid error message for not matched `syntax-rules` [#243](https://github.com/jcubic/lips/issues/243)
* fix silent error when class don't have parent [#272](https://github.com/jcubic/lips/issues/272)
* fix `try..catch` [#163](https://github.com/jcubic/lips/issues/163)

## 1.0.0-beta.17
### Breaking
* chars are now not automatically unboxed to strings [#233](https://github.com/jcubic/lips/issues/233)
### Features
* make bookmark REPL dockable [#205](https://github.com/jcubic/lips/issues/205)
* make Strings iterators that return characters
* improve object literals [#237](https://github.com/jcubic/lips/issues/237)
* better error message with exception on invalid string literal
* add non-standard `with-input-from-string` function
### Bugfix
* fix using performance object for older version of Node
* fixing escaped symbols in CodeMirror
* fix parsing strings [#193](https://github.com/jcubic/lips/issues/193)
* add proper error message to `-->` macro [#200](https://github.com/jcubic/lips/issues/200)
* fix `performance.timeOrigin` on Node 12 [#207](https://github.com/jcubic/lips/issues/207)
* fix string->list to handle longer code points
* fix numerator and denominator [#213](https://github.com/jcubic/lips/issues/213)
* fix Map object repr
* fix parsing regular expressions [#238](https://github.com/jcubic/lips/issues/238)
* fix exception when syntax-rule macro contain undefined
* fix REPL clearing stdin when using read-line [#253](https://github.com/jcubic/lips/issues/253)
* add proper handling of [Paste Brackets mode in REPL for NodeJS](https://github.com/nodejs/node/pull/47150)

## 1.0.0-beta.16
### Breaking
* replace `get-script` with `get-resource` that work in Node and in browser and allows to load CSS
### Features
* add `object->alist`, `command-line`, and `symbol-append` procedures
* add SRFI: 193
* add `get-environment-variable` and `get-environment-variables`
* improve bootstrapping by using same URL of the script if possible
* add `set-global!` macro
* add `current-second`, `current-jiffy`, and `jiffies-per-second` procedures
* improve error message in `-->` macro when function not found
### Bugfix
* fix async script execution in browser [#190](https://github.com/jcubic/lips/issues/190)
* fix type of `process.env`
* fix module load path when load scripts from Node.js
* fix bug in `quasiquote` and `unquote-splicing` [#194](https://github.com/jcubic/lips/issues/194)
* fix `inexact->exact` on integers [#199](https://github.com/jcubic/lips/issues/199)
* throw error on missing parentheses [#198](https://github.com/jcubic/lips/issues/198)
* fix empty syntax extensions as functions

## 1.0.0-beta.15
### Breaking
* bootstrap URL now require full URL to the file with standard library
### Features
* improve performance of `**` operator when JS support exponential operator
* add alias for `string-join` and `string-split`
* lists are now iterators
* add optional `=>` syntax to `cond` macro (as per R7RS)
* make `undefined` parser constant
* compile into binary CBOR format with LZJB compression
* add support for `log` of complex, rational, and negative numbers
* allow to define shorthand object literals [#185](https://github.com/jcubic/lips/issues/185)
* add Buffered Output Port and `flush-output` function
### Bugfix
* fix scoping issue in nested `syntax-rules`
* fix `repr` of object that have space in key
* property throw error when name is used without ellipsis
* fix using dot notation inside syntax-rules macros
* typecheck `string->symbol` and `symbol->string`
* fix `parent.frame` inside `Interpreter`
* fix `eval` without env
* fix quote as delimiter
* fix comma dot as two tokens
* fix printing symbols that have brackets or quotes
* fix resolving dot notation (e.g. `lips.LComplex.prototype.modulus`)
* fix `repr` of native type prototypes (e.g. `LComplex`)
* fix using prototypes inside objects (e.g. `(object :foo Number.prototype)`)
* fix `sqrt` of rational where numerator or denominator is float
* fix `sqrt` of negative real that are like integers [#175](https://github.com/jcubic/lips/issues/175)
* fix boxing NaN value
* fix `length` of nil
* fix trimming spaces in `Env::set`
* fix `repr` of symbols with impossible characters created with `string->symbol`
* fix `eqv?` on same pairs
* fix `abs` on inexact numbers that can be represented as integers [#181](https://github.com/jcubic/lips/issues/181)
* fix extra newline when calling print in browser [#186](https://github.com/jcubic/lips/issues/186)

## 1.0.0-beta.14
### Breaking
* remove `zip` function that just `(map list l1 l2 ...)`
* `raise` now throws object as is (just like JS `throw` keyword)
* `error` now throws LipsError exception as per R7RS
### Features
* R7RS datum labels
* allow to use `set-repr!` on records (record type name is now a class)
* match function return `#f` instead of `nil` when fail to match (so it work in `cond` or `if`)
* new functions `complement`, `always`, `once`, `flip`, `unfold` (inspired by Ramda)
* add codemirror hits for std lib symbols
* experimental compiler to JSON (not 100% ready)
* add support for `exp` on complex numbers
* add R7RS `error-object?`, `error-object-message` and `error-object-irritants`
* make `NaN`, `null` and `undefined` parser constants
* return proper repr of instances of `define-class`
### Bugfix
* fix `set-repr!` on classes
* fix conflict with jQuery plugns in bookmark
* fix swallowing errors when printing values in Node REPL
* fix mixed quoted and normal promises in let binding [#154](https://github.com/jcubic/lips/issues/154)
* fix problem were await affect quoted promise in macros (e.g. `begin`) [#153](https://github.com/jcubic/lips/issues/153)
* typecheck second argument to `set-obj!`
* fix `case` macro (use implementation from R7RS Errata)
* fix async constructor in `define-class`
* fix methods with improper lists in `define-class`
* fix null inside quasiquote

## 1.0.0-beta.12
### Breaking
* `env` and `dir` now returns symbols instead of strings
* `repr` and `type` of quoted promises now return `#<promise>` and `promise`
* numbers and characters properties are immutable
### Features
* create minfied std scheme file for faster bootstrap
* add `list-copy`
* add `define-record-type`
* add `escape-regex` function
* make `apropos` accept symbol as argument
* add doc strings for `**interaction-environment**` and `**internal-env**`
* add `letrec*` that in LIPS is exactly the same as `letrec`
* add `pragma->sxml` macro that define `sxml` macro (default is `h`)
* hide `fs` in internal env
* automatic bootstrapping of `fs` with BrowserFS if exists
* `pprint` in both REPLs now print in color [#33](https://github.com/jcubic/lips/issues/33)
* add `nan?`, `infinite?` and `finite?` functions
* add `+nan.0` and `-nan.0` (R7RS)
* properly handle negative inexact zero
* new `environment?` function
* add `current-directory` and `set-...` from SRFI-170
* add gensym literals (e.g. `#:foo`)
* fix pretty print of different cases of `let`
* add binary input/output procedures from R7RS
* update vector functions that in R7RS get start and end arguments
* add state props and better repr to quotedPromise
### Bugfix
* fix prism highlighting of names (for new context help)
* fix error when using help in node REPL and there are no doc string
* fix escaping regex operators when using string with `apropos`
* fix typechecking of number operators [#128](https://github.com/jcubic/lips/issues/128)
* fix indent of call-with- (input-file, output-file and port)
* fix eq? and type of NaN [#130](https://github.com/jcubic/lips/issues/130)
* fix number predicates
* fix `real-part` function
* fix parsing complex with 0 inexact imaginary part
* fix option -t --trace in Node REPL
* fix `eqv?` on exact and inexact numbers according to R7RS spec
* fix `exact->inexact` on complex numbers
* fix arithmetic with single complex value
* fix parsing regex that have escape slash and parenthesis (`#/( \\/)/g`)
* fix parsing regex that have single slash in class brackets (`#/\/[^/]+$/`)
* fix division on single argument (now `(/ n)` == `(/ 1 n)`)
* fix complex operation that result in real (e.g. multiplication over conjugation)
* fix `list-ref` according to R7RS errata
* fix formatter (pretty print) on multiline strings and atoms
* fix formatter indent of `let*`
* fix repr of vectors (arrays with empty value)
* fix promise quotation of object macro call [#139](https://github.com/jcubic/lips/issues/139)
* fix unquote-splicing inside direct quasiquote vector [#140](https://github.com/jcubic/lips/issues/140)

## 1.0.0-beta.11
### Breaking
* remove repr of HTMLElement (it's now default instance of a class)
* regular expressions are now prefixed with hash like in Gauche e.g. `#/foo/`
### Features
* add support for quasiquote on objects and vectors
* interning symbols mechanism [#106](https://github.com/jcubic/lips/issues/106)
* new macro `quoted-promise` and syntax `'>` [#54](https://github.com/jcubic/lips/issues/54)
* new function await that return original promise from QuotedPromise
* bytevector functions (R7RS)
* improve detecting and repr of iterators
* add `eof` variable to global env
* add `apropos` function
* new incremental Lexer that allow to modify parser while it's running
* add SRFI-10 sharp-comma
* new syntax try..catch..finally
* new parallel `list*` macro
* new `scheme-report-environment` function (R5RS)
* update `**` to work on negative numbers
### Bugfix
* fix resolving promises on vectors and objects
* fix context in methods of plain objects [#103](https://github.com/jcubic/lips/issues/103)
* fix `equal?` on typed arrays
* fix detecting user repr (when object is subclass)
* fix calling port methods (e.g. calling `get-output-string` function)
* fix prism highlighting of multiple regular expression
* fix parsing chars for Unicode outside of BMP
* fix when try..catch is promise that rejects and catch throws
* fix resolving promises when apply function they are not executed in sequentially
* fix `eval` builtin function to evaluate everything

## 1.0.0-beta.10
### Breaking
* change behavior of `and` and `or` to work like `if` (`#f` only falsy value)
* vectors and object literals are immutable (to prevent weird sharing data bugs)
### Features
* add `promise?` function
* improve `append`/`append!` to work according to spec and accept multiple arguments
* set `global` and `window` to undefined when not in Node or Window respectively
* add `self` as global context for Node, browser and worker
* add repr of global object
* add repr of classes
* add support for creating anonymous classes
* add `do-iterator` macro
* add repr of `Map` and `Set`
* add `digit-value` function
* add `#d` prefix for decimal numbers
* add `include` macro
* add `--debug` option to executable (that load src file instead of dist)
* add `cond-expand` syntax macro
* add new `native-symbol?` function
* add warning about using `’` quote, with code example to execute to enable it
* add `__dirname` and `__filename` variables to Node
* add support for variables in method properties of classes (like in JS)
### Bugfix
* fix load file that have shebang
* fix repr of eof object
* fix read from input-string-port [#85](https://github.com/jcubic/lips/issues/85)
* fix running web worker from file generated by babel (dist)
* fix repr of lambda function (no `:lambda` as name)
* fix parse complex big num (e.g.: `100e+100i`)
* fix repr of scheme functions if Function.prototype.toString was modified
* fix repr of iterators
* fix access symbol properties of binded functions (LIPS internals)
* fix duplicated line in executable [#89](https://github.com/jcubic/lips/issues/89)
* fix `char-numeric?` and `char-alphabetic?` to handle all unicode
* fix characters with more then one code point
* fix `repr` of functions that are created by `self.eval`
* fix formatting strings in REPL
* fix `set!` `nil` to object field
* fix `lambda` in `define-class` that have improper list as arguments
* throw exception when applying function to improper list
* fix `join` with nil
* fix accessing JavaScript objects in syntax-rules macros
* fix write of strings with newline
* fix indent of `when` and `unless`
* fix usage of Drag&Drop with bookmarklet on pdf files
* fix accessing methods on pattern variables in syntax macros [#83](https://github.com/jcubic/lips/issues/83)
* fix quasiquote to work like quote if not unquotes (R7RS spec)

## 1.0.0-beta.9
### Breaking
* throw exception when calling `(-)`
* `if` it now see #f the only falsy value to match Scheme spec
### Features
* add `let-values`, `let*-values` and `define-values` syntax macros
* add `exact-integer?` and `vector-append` functions
* add `stderr` port and `current-error-port` function
* add environment as second argument to `load` (according to R7RS spec)
### Bugfix
* fix empty vector literal
* fix edge case in nested syntax-rules when variable in parent got expanded into identifier
* fix nesting unquote in quasiquote [#53](https://github.com/jcubic/lips/issues/53)
* fix order of evaluation in `,,@x`
* fix name collision while catching DOM exceptions [#84](https://github.com/jcubic/lips/issues/84)
* fix integer? and other number type checks
* fix warnings about require while loading from Webpack
* fix unquote-splice multiple lists
* fix syntax highlighting (prism scheme syntax patching)
* fix load inside let that have global variable defined that was not undefined
* fix creating LIPS classes from SXML

## 1.0.0-beta.8
### Breaking
* internal properties like `name` are now `__name__`
### Features
* add `raise` (alias for `throw`) and `with-exception-handler` (that wraps `throw..catch`)
### Bugfix
* fix truncate core function
* fix splice nil in quasiquote inside list [#68](https://github.com/jcubic/lips/issues/68)
* fix do macro inside syntax-rules [#81](https://github.com/jcubic/lips/issues/81)
* fix `string-copy` and `string-set!`
* fix `error` function to work like R7RS spec
* fix edge case in `syntax-rules` found when executing SRFI-26 cut macro
* fix unboxing of LIPS values when calling native JavaScript functions
* fix unboxing of callback functions [#76](https://github.com/jcubic/lips/issues/76)

## 1.0.0-beta.7
### Breaking
* remove second argument from `filter` callback
* change mapping `&` from `make-object` to `object`
* parser is now async and parser extensions are evaluated in parse time
### Features
* new `random` procedure
* string repr of all procedures and macros in REPL now have name
* add `sxml` macro
### Bugfix
* fix write vs display of character atoms
* fix mixed case of named characters (e.g.: `#\Space`)
* fix `do` macro scope according to R7RS
* fix vector literals inside quoted list [#79](https://github.com/jcubic/lips/issues/79)

## 1.0.0-beta.6
### Breaking
* `real?` now return true on integers (bigInts)
* `set!` throw error when variable not defined
### Features
* new `string->vector`, `vector->string`, `vector-map` and `string-map` (R7RS)
* new `dynamic-wind` function (R7RS)
* make `equal?` compare plain objects
### Bugfix
* fix repr of HTMLElement
* fix creating LCharacter from scheme string
* fix `do` macro with let like scope for next values
* fix line breaking in formatter
* fix parsing options in executable (fix for `alias lips="lips -q"`)
* throw exception when evaluating `(if)`
* fix `object?` and `plain-object?`
* throw exception when invoking syntax that shadow literal identifier
* fix parsing cons dot in `(() . foo)`

## 1.0.0-beta.5
### Features
* new function `make-list`
* `(/ 0.0 0.0)` return NaN
* bookmarklet always fetch latest version
* `case-lambda`, `unless` and `when` hygienic macros (code from R7RS)
* new `boolean=?`, `port?` and `square` functions (R7RS)
* new `inexact` and `exact` aliases (R7RS)
* rational `numerator` and `denominator` function (R5RS)
* complex `imag-part`, `real-part`, `make-polar`, `angle` and `magnitude` functions (R5RS)
* new function: `radians->degree` and `degree->radians`
### Bugfix
* allow to use undefined in let
* fix function call that return NaN
* fix pretty print of syntax-rules

## 1.0.0-beta.4
### Breaking
* add/remove-specials and add/remove-repr are now set/unset
* rename wrong `gdc` to correct `gcd`
### Features
* doc strings on variables
* persistent history in executable REPL
* repr of promises (from `delay` function)
* allow `set!` with doted JS notation
* new functions `gensym?` and `prototype?`
### Bugfix
* fix Environment::has (ref) on variable that are undefined
* fix macroexpand with pprint on syntax macros
* fix formatter breaking rules definition in LIPS + (+ fix `cond`)
* fix rational? on integers (bigint)
* fix parsing quoted atoms (e.g. `'#f`, `'#\x` or `'#xA`)
* fix parsing escape slash inside strings
* don't throw exception on parse error in string->number (return false)
* fix silent accepting two big code point `#\xFFFFFF`
* fix custom repr of instances created by define-class
* fix parsing R7RS hex literals in strings
* fix syntax-rules with `(a b ...)` that match single item
* fix swallowed undefined from `(cons 1 undefined)`
* fix identifiers in syntax-rules
* fix byte vectors
* make print and pprint always call newline
* fix escape ellipsis from R7RS
* fix unboxing values (seen with stack overflow on dir date object)

## 1.0.0-beta.3
## Bugfix
* fix broken REPL

## 1.0.0-beta.1
### Breaking
* rename `ceil` to `ceiling` (like in R5RS spec)
* break `let*` to work the same as in scheme (it was working like `letrec`)
* `if` no longer require boolean as cond
* change how arrays are presented, they are look like scheme vector `#(...)`
* rename `string` to `repr`
* new API `lips.Interpreter` - for using interpreter that have different stdout/stdin
* balanced_parenthesis now throw exception on broken code
* remove global env from public API (you can still access it using `env.parent`)
* remove `->` function (it will collide with SRFI 197 that may be implemented in the future)
* `every` have now same signature as `some` which is `(fn list)`
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
* improve balancing parenthesis
* expose balanced method in public API (preferred name)
* optional brackets
* `iterator?` function and detection if iterators from `repr` and `type` [#51](https://github.com/jcubic/lips/issues/51)
* `add-repr!` function to add string representations to objects
* new `string=?`, `sort`, `in`, `list-tail` and `bound?` functions
* add new `exit` function into executable interpreter
* -e/eval option (help and docs will show new options, instead of -c/code but old still works)
* `case` macro
* relative load inside load + auto .scm suffix
* new `zip`, `n-ary`, `binary`, `unary`, `take` functions
* new `get-script` function
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
* use parser code in string->number
* fix repr of native object instances (e.g.: `Uint8Array`)
* fix display/write without newline in both REPLs
* fix evaluating single false value
* fix dir to not include values of `Object.prototype`


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
* add lips source code that threw exception in JavaScript error message
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
