# Function Reference

## %
```
(% n1 n2)

Function get reminder of it's arguments.
```

## &
```
(& a b)

Function calculate and bit operation.
```

## *
```
(* . numbers)

Multiplicate all numbers passed as arguments. If single value is passed
it will return that value.
```

## **
```
(** a b)

Function calculate number a to to the power of b.
```

## +
```
(+ . numbers)

Sum all numbers passed as arguments. If single value is passed it will
return that value.
```

## -
```
(- n1 n2 ...)
(- n)

Substract number passed as argument. If only one argument is passed
it will negate the value.
```

## .
```
(. obj . args)
(get obj . args)

Function use object as base and keep using arguments to get the
property of JavaScript object. Arguments need to be a strings.
e.g. `(. console "log")` if you use any function inside LIPS is
will be weakly bind (can be rebind), so you can call this log function
without problem unlike in JavaScript when you use
`var log = console.log`.
`get` is an alias because . don't work in every place, e.g. you can't
pass it as argument.
```

## /
```
(/ n1 n2 ...)
(/ n)

Divide number passed as arguments one by one. If single argument
is passed it will calculate (/ 1 n1).
```

## 1+
```
(1+ number)

Function add 1 to the number and return result.
```

## 1-
```
(1- number)

Function substract 1 from the number and return result.
```

## <
```
(< x1 x2 ...)

Function compare its numerical arguments and check if they are
monotonically decreasing
```

## <<
```
(<< a b)

Function left shit the value a by value b.
```

## <=
```
(<= x1 x2 ...)

Function compare its numerical arguments and check if they are
monotonically nonincreasing
```

## =
```
(== x1 x2 ...)

Function compare its numerical arguments and check if they are equal
```

## ==
```
(== x1 x2 ...)

Function compare its numerical arguments and check if they are equal
```

## >
```
(> x1 x2 ...)

Function compare its numerical arguments and check if they are
monotonically increasing
```

## >=
```
(>= x1 x2 ...)

Function compare its numerical arguments and check if they are
monotonically nondecreasing
```

## >>
```
(>> a b)

Function right shit the value a by value b.
```

## Y
```
(Y f)

  _ __   __    _            _       _      _ __   __         _   _  _
 /  \ \ / /   /  __        /   ____  \    /  \ \ / /    ____  \   \  \
+    \ v /   +   \ \      +   / ___|  +  +    \ v /    / ___|  +   +  +
|     \ /    |    \ \     |  | |__    |  |     \ /    | |__    |   |  |
|     | |    |    /  \    |  |  __|   |  |     | |    |  __|   |   |  |
|     | |    |   / /\ \   |  | |      |  |     | |    | |      |   |  |
+     |_|    +  /_/  \_\  +  |_|      +  +     |_|    |_|      +   +  +
 \_           \_           \_       _/    \_                 _/  _/ _/
```

## abs
```
(abs number)

Function create absolute value from number.
```

## acos
```
#<undefined>
```

## alist->assign
```
(alist->assign alist . list-of-alists)

Function that work like Object.assign but for LIPS alist.
```

## alist->object
```
(alist->object alist)

Function convert alist pairs to JavaScript object.
```

## always
```
(always constant)

Higher order function returns new function that always return given constant.
```

## angle
```
(angle x)

Returns angle of the complex number in polar coordinate system.
```

## append
```
(append item ...)

Function will create new list with eac argument appended to the end.
It will always return new list and not modify it's arguments.
```

## append!
```
(append! arg1 ...)

Destructive version of append, it modify the list in place. It return
new list where each argument is appened to the end. It may modify
lists added as arguments.
```

## apply
```
(apply fn list)

Function that call function with list of arguments.
```

## apropos
```
(apropos name)

Search environment and display names that match the given name.
name can be regex, string or symbol.
```

## array->list
```
(array->list array)

Function convert JavaScript array to LIPS list.
```

## array?
```
(array? expression)

Function check if value is an arrray.
```

## asin
```
#<undefined>
```

## assoc
```
(assoc obj alist)

Function return pair from alist that match given key using equal? check.
```

## assq
```
(assq obj alist)

Function return pair from alist that match given key using eq? check.
```

## assv
```
(assv obj alist)

Function return pair from alist that match given key using eqv? check.
```

## atan
```
#<undefined>
```

## await
```
(await value)

Function unquote quoted promise so it can be automagicaly evaluated (resolved
to its value).
```

## binary
```
(binary fn)

Function return new function with arguments limited to two.
```

## binary-port?
```
(binary-port? port)

Function test if argument is binary port.
```

## boolean=?
```
(boolean=? b1 b2 ...)

Function check if all arguments are boolean and if they are the same.
```

## boolean?
```
(boolean? x)

Function return true if value is boolean.
```

## bound?
```
(bound? x [env])

Function check if variable is defined in given environement or interaction environment
if not specified.
```

## buffer->u8vector
```
(buffer->u8vector bin)

Cross platform function that can be used in both Node and Browser.
It can be used together with %read-file or %read-binary-file and convert
the result ArrayBuffer or Buffer to u8vector.
```

## bytevector
```
(u8vector v1 v2 ...)

Create usigned 8-bit integer vector from give arguments.
```

## bytevector-append
```
(bytevector-append v1 ...)

Create new bytevector u8vector that is created from joining each argument.
```

## bytevector-copy
```
(bytevector-copy v)
(bytevector-copy v start)
(bytevector-copy v start end)

Function and return new vector from start to end. If no start and end is provided
whole vector is copied and returned.
```

## bytevector-copy!
```
(bytevector-copy! to at from)
(bytevector-copy! to at from start)
(bytevector-copy! to at from start end)

Copies the bytes of bytevector from between start and end to bytevector to,
starting at at.
```

## bytevector-length
```
(u8vector-length v)

return length of usigned 8-bit integer vector.
```

## bytevector-u8-ref
```
(u8vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## bytevector-u8-set!
```
(u8vector-set! vector k)

Function set value of usigned 8-bit integer vector at index k. If index is out of range it throw exception.
```

## bytevector?
```
(u8vector? x)

Function return #t of argument is usigned 8-bit integer vector otherwise it return #f.
```

## caaaaar
```
(caaaaar arg)

Function calculate (car (car (car (car (car arg)))))
```

## caaaadr
```
(caaaadr arg)

Function calculate (car (car (car (car (cdr arg)))))
```

## caaaar
```
(caaaar arg)

Function calculate (car (car (car (car arg))))
```

## caaadar
```
(caaadar arg)

Function calculate (car (car (car (cdr (car arg)))))
```

## caaaddr
```
(caaaddr arg)

Function calculate (car (car (car (cdr (cdr arg)))))
```

## caaadr
```
(caaadr arg)

Function calculate (car (car (car (cdr arg))))
```

## caaar
```
(caaar arg)

Function calculate (car (car (car arg)))
```

## caadaar
```
(caadaar arg)

Function calculate (car (car (cdr (car (car arg)))))
```

## caadadr
```
(caadadr arg)

Function calculate (car (car (cdr (car (cdr arg)))))
```

## caadar
```
(caadar arg)

Function calculate (car (car (cdr (car arg))))
```

## caaddar
```
(caaddar arg)

Function calculate (car (car (cdr (cdr (car arg)))))
```

## caadddr
```
(caadddr arg)

Function calculate (car (car (cdr (cdr (cdr arg)))))
```

## caaddr
```
(caaddr arg)

Function calculate (car (car (cdr (cdr arg))))
```

## caadr
```
(caadr arg)

Function calculate (car (car (cdr arg)))
```

## caar
```
(caar arg)

Function calculate (car (car arg))
```

## cadaaar
```
(cadaaar arg)

Function calculate (car (cdr (car (car (car arg)))))
```

## cadaadr
```
(cadaadr arg)

Function calculate (car (cdr (car (car (cdr arg)))))
```

## cadaar
```
(cadaar arg)

Function calculate (car (cdr (car (car arg))))
```

## cadadar
```
(cadadar arg)

Function calculate (car (cdr (car (cdr (car arg)))))
```

## cadaddr
```
(cadaddr arg)

Function calculate (car (cdr (car (cdr (cdr arg)))))
```

## cadadr
```
(cadadr arg)

Function calculate (car (cdr (car (cdr arg))))
```

## cadar
```
(cadar arg)

Function calculate (car (cdr (car arg)))
```

## caddaar
```
(caddaar arg)

Function calculate (car (cdr (cdr (car (car arg)))))
```

## caddadr
```
(caddadr arg)

Function calculate (car (cdr (cdr (car (cdr arg)))))
```

## caddar
```
(caddar arg)

Function calculate (car (cdr (cdr (car arg))))
```

## cadddar
```
(cadddar arg)

Function calculate (car (cdr (cdr (cdr (car arg)))))
```

## caddddr
```
(caddddr arg)

Function calculate (car (cdr (cdr (cdr (cdr arg)))))
```

## cadddr
```
(cadddr arg)

Function calculate (car (cdr (cdr (cdr arg))))
```

## caddr
```
(caddr arg)

Function calculate (car (cdr (cdr arg)))
```

## cadr
```
(cadr arg)

Function calculate (car (cdr arg))
```

## call-with-input-file
```
(call-with-input-file filename proc)

Procedure open file for reading, call user defined procedure with given port
and then close the port. It return value that was returned by user proc
and it close the port even if user proc throw exception.
```

## call-with-output-file
```
(call-with-output-file filename proc)

Procedure open file for writing, call user defined procedure with port
and then close the port. It return value that was returned by user proc and it close the port
even if user proc throw exception.
```

## call-with-port
```
(call-with-port port proc)

Proc is executed with given port and after it returns, the port is closed.
```

## call-with-values
```
(call-with-values producer consumer)

Calls its producer argument with no values and a continuation that,
when passed some values, calls the consumer procedure with those
values as arguments.
```

## car
```
(car pair)

Function returns car (head) of the list/pair.
```

## cdaaaar
```
(cdaaaar arg)

Function calculate (cdr (car (car (car (car arg)))))
```

## cdaaadr
```
(cdaaadr arg)

Function calculate (cdr (car (car (car (cdr arg)))))
```

## cdaaar
```
(cdaaar arg)

Function calculate (cdr (car (car (car arg))))
```

## cdaadar
```
(cdaadar arg)

Function calculate (cdr (car (car (cdr (car arg)))))
```

## cdaaddr
```
(cdaaddr arg)

Function calculate (cdr (car (car (cdr (cdr arg)))))
```

## cdaadr
```
(cdaadr arg)

Function calculate (cdr (car (car (cdr arg))))
```

## cdaar
```
(cdaar arg)

Function calculate (cdr (car (car arg)))
```

## cdadaar
```
(cdadaar arg)

Function calculate (cdr (car (cdr (car (car arg)))))
```

## cdadadr
```
(cdadadr arg)

Function calculate (cdr (car (cdr (car (cdr arg)))))
```

## cdadar
```
(cdadar arg)

Function calculate (cdr (car (cdr (car arg))))
```

## cdaddar
```
(cdaddar arg)

Function calculate (cdr (car (cdr (cdr (car arg)))))
```

## cdadddr
```
(cdadddr arg)

Function calculate (cdr (car (cdr (cdr (cdr arg)))))
```

## cdaddr
```
(cdaddr arg)

Function calculate (cdr (car (cdr (cdr arg))))
```

## cdadr
```
(cdadr arg)

Function calculate (cdr (car (cdr arg)))
```

## cdar
```
(cdar arg)

Function calculate (cdr (car arg))
```

## cddaaar
```
(cddaaar arg)

Function calculate (cdr (cdr (car (car (car arg)))))
```

## cddaadr
```
(cddaadr arg)

Function calculate (cdr (cdr (car (car (cdr arg)))))
```

## cddaar
```
(cddaar arg)

Function calculate (cdr (cdr (car (car arg))))
```

## cddadar
```
(cddadar arg)

Function calculate (cdr (cdr (car (cdr (car arg)))))
```

## cddaddr
```
(cddaddr arg)

Function calculate (cdr (cdr (car (cdr (cdr arg)))))
```

## cddadr
```
(cddadr arg)

Function calculate (cdr (cdr (car (cdr arg))))
```

## cddar
```
(cddar arg)

Function calculate (cdr (cdr (car arg)))
```

## cdddaar
```
(cdddaar arg)

Function calculate (cdr (cdr (cdr (car (car arg)))))
```

## cdddadr
```
(cdddadr arg)

Function calculate (cdr (cdr (cdr (car (cdr arg)))))
```

## cdddar
```
(cdddar arg)

Function calculate (cdr (cdr (cdr (car arg))))
```

## cddddar
```
(cddddar arg)

Function calculate (cdr (cdr (cdr (cdr (car arg)))))
```

## cdddddr
```
(cdddddr arg)

Function calculate (cdr (cdr (cdr (cdr (cdr arg)))))
```

## cddddr
```
(cddddr arg)

Function calculate (cdr (cdr (cdr (cdr arg))))
```

## cdddr
```
(cdddr arg)

Function calculate (cdr (cdr (cdr arg)))
```

## cddr
```
(cddr arg)

Function calculate (cdr (cdr arg))
```

## cdr
```
(cdr pair)

Function returns cdr (tail) of the list/pair.
```

## ceiling
```
(ceiling number)

Function calculate ceiling of a number.
```

## char->integer
```
(char->integer chr)

Function return codepoint of Unicode character.
```

## char-alphabetic?
```
(char-alphabetic? chr)

Function return true if character is leter of the ASCII alphabet.
```

## char-ci<=?
```
(char-ci<? chr1 chr2)

Function return true if second character is not larger then the first one.
```

## char-ci<?
```
(char-ci<? chr1 chr2)

Function return true if second character is smaller then the first one.
```

## char-ci=?
```
(char-ci=? chr1 chr2)

Function check if two characters are equal.
```

## char-ci>=?
```
(char-ci<? chr1 chr2)

Function return true if second character is not smaller then the first one.
```

## char-ci>?
```
(char-ci<? chr1 chr2)

Function return true if second character is larger then the first one.
```

## char-downcase
```
(char-downcase chr)

Create lowercase version of the character.
```

## char-lower-case?
```
(char-upper-case? char)

Function check if character is lower case.
```

## char-numeric?
```
(char-numeric? chr)

Function return true if character is number.
```

## char-ready?
```
(char-ready?)
(char-ready? port)

Function check it characters is ready in input port. This is usefull mostly
for interactive ports that return false if it would wait for user input.
It return false if port is closed.
```

## char-upcase
```
(char-upcase char)

Create uppercase version of the character.
```

## char-upper-case?
```
(char-upper-case? char)

Function check if character is upper case.
```

## char-whitespace?
```
(char-whitespace? chr)

Function return true if character is whitespace.
```

## char<=?
```
(char<? chr1 chr2)

Function return true if second character is not larger then the first one.
```

## char<?
```
(char<? chr1 chr2)

Function return true if second character is smaller then the first one.
```

## char=?
```
(char=? chr1 chr2)

Function check if two characters are equal.
```

## char>=?
```
(char<? chr1 chr2)

Function return true if second character is not smaller then the first one.
```

## char>?
```
(char<? chr1 chr2)

Function return true if second character is larger then the first one.
```

## char?
```
(char? obj)

Function check if object is character.
```

## clone
```
(clone list)

Function return clone of the list.
```

## close-input-port
```
(close-input-port port)

Procedure close port that was opened with open-input-file. After that
it no longer accept reading from that port.
```

## close-output-port
```
(close-output-port port)

Procedure close port that was opened with open-output-file. After that
it no longer accept write to that port.
```

## close-port
```
(close-port port)

Close input or output port.
```

## complement
```
(complement fn)

Higer order function that returns complement of the given function. If the function fn
for a given arguments return true the result function will return false, if it would
return false, the result function will return true.
```

## complex?
```
(complex? x)

Function check if argument x is complex.
```

## compose
```
(compose . fns)

Higher order function and create new function that apply all functions
From right to left and return it's value. Reverse of compose.
e.g.:
((compose (curry + 2) (curry * 3)) 3)
11

```

## concat
```
(concat . strings)

Function create new string by joining its arguments
```

## cons
```
(cons left right)

Function return new Pair out of two arguments.
```

## cos
```
#<undefined>
```

## current-directory
```
(current-directory)

Return corrent working directory, default it's path from where
the script was executed.
```

## current-environment
```
(current-environment)

Function return current environement.
```

## current-error-port
```
(current-output-port)

Function return default stdout port.
```

## current-input-port
```
current-input-port)

Function return default stdin port.
```

## current-output-port
```
(current-output-port)

Function return default stdout port.
```

## curry
```
(curry fn . args)

Higher order function that create curried version of the function.
The result function will have parially applied arguments and it
will keep returning functions until all arguments are added

e.g.:
(define (add a b c d) (+ a b c d))
(define add1 (curry add 1))
(define add12 (add 2))
(display (add12 3 4))
```

## debugger
```
(debugger)

Function stop JavaScript code in debugger.
```

## defmacro?
```
(defmacro? expression)

Function check if object is macro and it's expandable.
```

## degree->radians
```
(degree->radians x)

Convert degree to radians.
```

## delete-file
```
(delete-file filename)

Function delete the file of given name.
```

## denominator
```
(denominator n)

Return denominator of rational or same number if one is not rational.
```

## digit-value
```
(digit-value chr)

Return digit number if character is numeral (as per char-numeric?)
or #f otherwise.
```

## dir
```
(dir obj)

Function return all props on the object including those in prototype chain.
```

## display
```
(display arg [port])

Function send string to standard output or provied port.
```

## display-error
```
(display-error . args)

Display error message.
```

## dynamic-wind
```
(dynamic-wind before thunk after)

Function accept 3 procedures/lambdas and execute thunk with before and always
after even if error accur
```

## empty?
```
(empty? object)

Function return true if value is undfined empty list.
```

## env
```
(env)
(env obj)

Function return list of values (functions, macros and variables)
inside environment and it's parents.
```

## environment-bound?
```
(environment-bound? env symbol)

Function check if symbol is bound variable similar to bound?.
```

## environment?
```
(environment? obj)

Function check if object is LIPS environment.
```

## eof-object
```
(eof-object)

Procedure returns eof object that indicate end of the port
```

## eof-object?
```
(eof-object? arg)

Function check if value is eof object, returned from input string
port when there are no more data to read.
```

## eq?
```
(eq? a b)

Function compare two values if they are identical.
```

## equal?
```
(equal? a b)

Function check if values are equal if both are pair or array
it compares the their elements recursivly.
```

## eqv?
```
(eqv? a b)

Function compare the values. It return true if they are the same, they
need to have same type
```

## error
```
(error message ...)

Function raises error with given message and arguments,
which are called invariants.
```

## error-object-irritants
```
(error-object-irritants error-object)

Returns a list of the irritants encapsulated by error-object.
```

## error-object-message
```
(error-object-message error-object)

Returns the message encapsulated by error-object.
```

## error-object?
```
(error-object? obj)

Function check if object is of Error object throwed by error function.
```

## escape-regex
```
(escape-regex string)

Function return new string where all special operators used in regex,
are escaped with slash so they can be used in RegExp constructor
to match literal string
```

## eval
```
(eval expr)
(eval expr environment)

Function evalute LIPS Scheme code.
```

## even?
```
(even? number)

Function check if number is even.
```

## every
```
(every fn list)

Function call function fn on each item of the list, if every value is true
it will return true otherwise it return false.
```

## exact
```
(inexact->exact number)

Funcion convert real number to exact ratioanl number.
```

## exact->inexact
```
(exact->inexact n)

Convert exact number to inexact.
```

## exact-integer?
```
(exact-integer? n)

Function returns #t if z is both exact and an integer; otherwise
returns #f.
```

## exact?
```
(exact? n)
```

## exp
```
#<undefined>
```

## expt
```
(** a b)

Function calculate number a to to the power of b.
```

## f32vector
```
(f32vector v1 v2 ...)

Create 32-bit IEEE floating point number vector from give arguments.
```

## f32vector->list
```
#<undefined>
```

## f32vector-length
```
(f32vector-length v)

return length of 32-bit IEEE floating point number vector.
```

## f32vector-ref
```
(f32vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## f32vector-set!
```
(f32vector-set! vector k)

Function set value of 32-bit IEEE floating point number vector at index k. If index is out of range it throw exception.
```

## f32vector?
```
(f32vector? x)

Function return #t of argument is 32-bit IEEE floating point number vector otherwise it return #f.
```

## f64vector
```
(f64vector v1 v2 ...)

Create 64-bit IEEE floating point number vector from give arguments.
```

## f64vector->list
```
#<undefined>
```

## f64vector-length
```
(f64vector-length v)

return length of 64-bit IEEE floating point number vector.
```

## f64vector-ref
```
(f64vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## f64vector-set!
```
(f64vector-set! vector k)

Function set value of 64-bit IEEE floating point number vector at index k. If index is out of range it throw exception.
```

## f64vector?
```
(f64vector? x)

Function return #t of argument is 64-bit IEEE floating point number vector otherwise it return #f.
```

## features
```
#<undefined>
```

## file-exists?
```
#<undefined>
```

## filter
```
(filter fn list)
(filter regex list)

Higher order function that call `fn` for each element of the list
and return list for only those elements for which funtion return
true value. If called with regex it will create matcher function.
```

## find
```
(find fn list)
(find regex list)

Higher order Function find first value for which function return true.
If called with regex it will create matcher function.
```

## finite?
```
(finite? x)

Function check if value is finite.
```

## flatten
```
(flatten list)

Return shallow list from tree structure (pairs).
```

## flip
```
(flip fn)

Higher order function that return new function where first two arguments are swapped.

Example:

  (define first (curry (flip vector-ref) 0))
  (first #(1 2 3))
  ;; ==> 1
```

## floor
```
(floor number)

Function calculate floor of a number.
```

## floor-quotient
```
#<undefined>
```

## floor-remainder
```
#<undefined>
```

## floor/
```
#<undefined>
```

## flush-output-port
```
(flush-output-port port)

Functio do nothing, flush is not needed in LIPS in both NodeJS and Browser.
The function is added, so it don't throw exception when using R7RS code.
```

## fold
```
(fold fn init . lists)

Function fold is reverse of the reduce. it call function `fn`
on each elements of the list and return single value.
e.g. it call (fn a1 b1 (fn a2 b2 (fn a3 b3 '())))
for: (fold fn '() alist blist)
```

## fold-left
```
(fold fn init . lists)

Function fold is reverse of the reduce. it call function `fn`
on each elements of the list and return single value.
e.g. it call (fn a1 b1 (fn a2 b2 (fn a3 b3 '())))
for: (fold fn '() alist blist)
```

## fold-right
```
(reduce fn init list . lists)

Higher order function take each element of the list and call
the function with result of previous call or init and next element
on the list until each element is processed and return single value
as result of last call to `fn` function.
e.g. it call (fn a3 b3 (fn a2 b2 (fn a1 b1 init)))
for (reduce fn init alist blist)
```

## for-each
```
(for-each fn . lists)

Higher order function that call function `fn` by for each
value of the argument. If you provide more then one list as argument
it will take each value from each list and call `fn` function
with that many argument as number of list arguments.
```

## force
```
(force promise)

Function force the promise and evaluate delayed expression.
```

## format
```
(format string n1 n2 ...)

Function accepts string template and replacing any escape sequences
by arguments:

* ~a value as if printed with display
* ~s value as if printed with write
* ~% newline character
* ~~ literal tilde '~' is inserted

if there missing arguments or other escape character it throw exception.
```

## function?
```
(function? expression)

Function check if value is a function.
```

## gcd
```
(gcd n1 n2 ...)

Function return the greatest common divisor of their arguments.
```

## gensym
```
(gensym)

Function generate unique symbol, to use with macros as meta name.
```

## gensym-interal
```
(gensym-interal symbol)

Parser extension that create new quoted named gensym.
```

## gensym?
```
(gensym? value)

Function return #t if value is symbol and it's gensym. It returns #f otherwise.
```

## get
```
(. obj . args)
(get obj . args)

Function use object as base and keep using arguments to get the
property of JavaScript object. Arguments need to be a strings.
e.g. `(. console "log")` if you use any function inside LIPS is
will be weakly bind (can be rebind), so you can call this log function
without problem unlike in JavaScript when you use
`var log = console.log`.
`get` is an alias because . don't work in every place, e.g. you can't
pass it as argument.
```

## get-output-bytevector
```
(get-output-string port)

Function get full string from string port. If nothing was wrote
to given port it will return empty string.
```

## get-output-string
```
(get-output-string port)

Function get full string from string port. If nothing was wrote
to given port it will return empty string.
```

## get-script
```
(get-script url)

Load JavaScript file in browser by adding script tag to head of the current document.
```

## http-get
```
(http-get url)

Node.js Function that send HTTP Request and return string or
binary Buffer object.
```

## identity
```
(identity n)

No op function. it just returns its argument.
```

## imag-part
```
(imag-part n)

Return imaginary part of the complex number n.
```

## in
```
(in key value)

Function use is in operator to check if value is in object.
```

## indexed-db?
```
(indexed-db?)

Function test if indexedDB is available.
```

## inexact
```
(exact->inexact n)

Convert exact number to inexact.
```

## inexact->exact
```
(inexact->exact number)

Funcion convert real number to exact ratioanl number.
```

## inexact?
```
(inexact? n)
```

## infinite?
```
(infinite? x)

Function check if value is infinite.
```

## input-port-open?
```
(input-port-open? port)

Function check if argument is input-port and if you can read from it.
```

## input-port?
```
(input-port? arg)

Function return true if argument is input port.
```

## instanceof
```
(instanceof type obj)

Function check of object is instance of object.
```

## integer->char
```
(integer->char chr)

Function convert number argument to chararacter.
```

## integer?
```
(integer? x)

Function check if argument x is integer.
```

## interaction-environment
```
(interaction-environment)

Function return interaction environement equal to lips.env can be overwritten,
when creating new interpreter with lips.Interpreter.
```

## iterator?
```
(iterator? x)

 Function check if value is JavaScript iterator object
```

## join
```
(join separator list)

Function return string by joining elements of the list
```

## key->string
```
(key->string symbol)

If symbol is key it convert that to string - remove colon.
```

## key?
```
(key? symbol)

Function check if symbol is key symbol, have colon as first character.
```

## lcm
```
(lcm n1 n2 ...)

Function return the least common multiple of their arguments.
```

## length
```
(length expression)

Function return length of the object, the object can be list
or any object that have length property.
```

## list
```
(list . args)

Function create new list out of its arguments.
```

## list->array
```
(list->array list)

Function convert LIPS list into JavaScript array.
```

## list->f32vector
```
#<undefined>
```

## list->f64vector
```
#<undefined>
```

## list->s16vector
```
#<undefined>
```

## list->s32vector
```
#<undefined>
```

## list->s8vector
```
#<undefined>
```

## list->string
```
(list->string _list)

Function return string from list of characters.
```

## list->u16vector
```
#<undefined>
```

## list->u32vector
```
#<undefined>
```

## list->u8vector
```
#<undefined>
```

## list->vector
```
(list->array list)

Function convert LIPS list into JavaScript array.
```

## list-copy
```
(list-copy obj)

Copy the object passed as argument but only if it's list. The car elements
of the list are not copied, they are passed as is.
```

## list-match?
```
(list-match? predicate list)

Function check if consecutive elements of the list match the predicate function.
```

## list-ref
```
(list-ref list n)

Returns n element of a list.
```

## list-tail
```
(list-tail list k)

Returns the sublist of list obtained by omitting the first k elements.
```

## list?
```
(list? obj)

Function test if value is proper linked list structure.
The car of each pair can be any value. It return false on cycles."
```

## load
```
(load filename)
(load filename environment)

Function fetch the file and evaluate its content as LIPS code,
If second argument is provided and it's environment the evaluation
will happen in that environment.
```

## log
```
(log z)

Funcntion calculates natural logarithm of z. Where argument can be
any number (including complex negative and rational).
If the value is 0 it return NaN.
```

## macro?
```
(macro? expression)

Function check if value is a macro.
```

## magnitude
```
(magnitude x)

Returns magnitude of the complex number in polar coordinate system.
```

## make-bytevector
```
(make-u8vector k fill)

Allocate new usigned 8-bit integer vector of length k, with optional initial values.
```

## make-f32vector
```
(make-f32vector k fill)

Allocate new 32-bit IEEE floating point number vector of length k, with optional initial values.
```

## make-f64vector
```
(make-f64vector k fill)

Allocate new 64-bit IEEE floating point number vector of length k, with optional initial values.
```

## make-list
```
#<undefined>
```

## make-polar
```
(make-polar magnitude angle)

Create new complex number from polar parameters.
```

## make-promise
```
(make-promise fn)

Function create promise from a function.
```

## make-rectangular
```
(make-rectangular im re)

Create complex number from imaginary and real part.
```

## make-s16vector
```
(make-s16vector k fill)

Allocate new signed 16-bit integer vector of length k, with optional initial values.
```

## make-s32vector
```
(make-s32vector k fill)

Allocate new signed 32-bit integer vector of length k, with optional initial values.
```

## make-s8vector
```
(make-s8vector k fill)

Allocate new signed 8-bit integer vector of length k, with optional initial values.
```

## make-string
```
(make-string k [char])

Function return new string with k elements, if char is provied
it's filled with that character.
```

## make-tags
```
(make-tags expression)

Function that return list structure of code with better syntax then raw LIPS
```

## make-u16vector
```
(make-u16vector k fill)

Allocate new usigned 16-bit integer vector of length k, with optional initial values.
```

## make-u32vector
```
(make-u32vector k fill)

Allocate new usigned 32-bit integer vector of length k, with optional initial values.
```

## make-u8vector
```
(make-u8vector k fill)

Allocate new usigned 8-bit integer vector of length k, with optional initial values.
```

## make-vector
```
(make-vector n [fill])

Create new vector with n empty elements. If fill is specified it will set
all elements of the vector to that value.
```

## map
```
(map fn . lists)

Higher order function that call function `fn` by for each
value of the argument. If you provide more then one list as argument
it will take each value from each list and call `fn` function
with that many argument as number of list arguments. The return
values of the function call is acumulated in result list and
returned by the call to map.
```

## match
```
(match pattern string)

function return match object from JavaScript as list or #f if not match.
```

## max
```
(max n1 n2 ...)

Return maximum of it's arguments.
```

## member
```
(member obj list)

Function return first object in the list that match using equal? function.
```

## memq
```
(memq obj list)

Function return first object in the list that match using eq? function.
```

## memv
```
(memv obj list)

Function return first object in the list that match using eqv? function.
```

## min
```
(min n1 n2 ...)

Return minimum of it's arguments.
```

## modulo
```
(modulo a b)

Function return modulo operation on it's argumennts.
```

## n-ary
```
(n-ary n fn)

Return new function that limit number of arguments to n.
```

## nan?
```
(nan? x)

Function check if argument x is Not a Number (NaN) value.
```

## native-symbol?
```
(native-symbol? object)

Function check if value is JavaScript symbol.
```

## native.number
```
(native.number obj)

If argument is number it will convert to native number.
```

## negative?
```
(negative? x)

Function check if number is smaller then 0
```

## new
```
(new obj . args)

Function create new JavaScript instance of an object.
```

## new-library
```
(new-library name)

Create new empty library object with empty namespace.
```

## newline
```
(newline [port])

Write newline character to standard output or given port
```

## not
```
(not x)

Function return true if value is false and false otherwise.
```

## nth
```
(nth index obj)

Function return nth element of the list or array. If used with different
value it will throw exception
```

## null-environment
```
(null-environment)

Function return new base environment with std lib.
```

## null?
```
(null? expression)

Function check if value is nulish.
```

## number->string
```
(number->string x [radix])

Function convert number to string with optional radix (number base).
```

## number?
```
(number? expression)

Function check if value is a number or NaN value.
```

## numbers?
```
#<undefined>
```

## numerator
```
(numerator n)

Return numberator of rational or same number if n is not rational.
```

## object-expander
```
(object-expander reaonly '(:foo (:bar 10) (:baz (1 2 3))))

Recursive function helper for defining LIPS code for create objects
using key like syntax.
```

## object?
```
(object? expression)

Function check if value is an plain object.
```

## odd?
```
(odd? number)

Function check if number os odd.
```

## once
```
(once fn)

Higher order function that return new function, that is guarantee
to be called only once.
```

## open-binary-input-file
```
(open-binary-input-file filename)

Function return new Input Binary Port with given filename. In Browser
user need to provide global fs variable that is instance of FS interface.
```

## open-binary-output-file
```
(open-binary-output-file filename)

Function open file and return port that can be used for writing. If file
exists it will throw an Error.
```

## open-input-bytevector
```
(open-input-bytevector bytevector)

Create new input binary port with given bytevector
```

## open-input-file
```
(open-input-file filename)

Function return new Input Port with given filename. In Browser user need to
provide global fs variable that is instance of FS interface.
```

## open-input-string
```
(open-input-string string)

Function create new string port as input that can be used to
read S-exressions from this port using `read` function.
```

## open-output-bytevector
```
(open-output-bytevector)

Create new output port that can be used to write binary data.
After done with the data the output buffer can be obtained by calling
`get-output-bytevector` function.
```

## open-output-file
```
(open-output-file filename)

Function open file and return port that can be used for writing. If file
exists it will throw an Error.
```

## open-output-string
```
(open-output-string)

Function create new output port that can used to write string into
and after finish get the whole string using `get-output-string`.
```

## output-port-open?
```
(output-port-open? port)

Function check if argument is output-port and if you can write to it.
```

## output-port?
```
(output-port? arg)

Function return true if argument is output port.
```

## pair-map
```
(pair-map fn list)

Function call fn argument for pairs in a list and return combined list with
values returned from function fn. It work like the map but take two items from list
```

## pair?
```
(pair? expression)

Function check if value is a pair or list structure.
```

## parent.frame
```
(parent.frame)

Return parent environment if called from inside function.
If no parent frame found it return nil.
```

## parent.frames
```
(parent.frames)

Funcion return list of environments from parent frames (lambda function calls)
```

## peek-char
```
(peek-char port)

Function get character from string port or EOF object if no more
data in string port.
```

## peek-u8
```
(peek-u8)
(peek-u8 port)

Return next byte from input-binary port. If there are no more bytes
it return eof object.
```

## pipe
```
(pipe . fns)

Higher order function and create new function that apply all functions
From left to right and return it's value. Reverse of compose.
e.g.:
((pipe (curry + 2) (curry * 3)) 3)
15
```

## plain-object?
```
(plain-object? x)

Function check if value is plain JavaScript object. Created using object macro.
```

## pluck
```
(pluck . string)

If called with single string it will return function that will return
key from object. If called with more then one argument function will
return new object by taking all properties from given object.
```

## port?
```
(port? x)

Function return true of argumet is nput or output port port object.
```

## positive?
```
(positive? x)

Function check if number is larger then 0
```

## pprint
```
(pprint expression)

Pretty print list expression, if called with non-pair it just call
print function with passed argument.
```

## pretty-format
```
(pretty-format pair)

Function return pretty printed string from pair expression.
```

## print
```
(print . args)

Function convert each argument to string and print the result to
standard output (by default it's console but it can be defined
it user code), the function call newline after printing each arg.
```

## procedure?
```
(function? expression)

Function check if value is a function.
```

## promise?
```
(promise? obj)

Function check if value is a promise created with delay or make-promise.
```

## promisify
```
(promisify fn)

Simple function for adding promises to NodeJS callback based function.
Function tested only with fs module.
```

## prototype?
```
(prototype? obj)

Function check if value is JavaScript Object prototype.
```

## qsort
```
(qsort list predicate)

Sort the list using quick sort alorithm according to predicate.
```

## quoted-symbol?
```
(quoted-symbol? code)

Helper function that test if value is quoted symbol. To be used in macros
that pass literal code that is transformed by parser.

usage:

  (define-macro (test x)
     (if (quoted-symbol? x)
         `',(cadr x)))

  (list 'hello (test 'world))
```

## quotient
```
(quotient a b)

Return quotient from divition as integer.
```

## quotient&remainder
```
#<undefined>
```

## radians->degree
```
(radians->degree x)

Convert radians to degree.
```

## raise
```
(raise obj)

Throws new exception with given object.
```

## random
```
(random)
(random seed)

Function generate new random real number using Knuth algorithm.
```

## range
```
(range n)

Function return list of n numbers from 0 to n - 1
```

## rational?
```
(rational? x)

Function check if value is rational.
```

## rationalize
```
(rationalize number tolerance)

Function returns simplest rational number differing from number by no more
than the tolerance.
```

## read
```
(read [string])

Function if used with string will parse the string and return
list structure of LIPS code. If called without an argument it
will read string from standard input (using browser prompt or
user defined way) and call itself with that string (parse is)
function can be used together with eval to evaluate code from
string
```

## read-bytevector
```
(read-bytevector k)
(read-bytevector k port)

Read next n bytes from input-binary port. If there are no more bytes
it returns eof object. If there are less then n bytes in port it
return the only bytes that are available
```

## read-bytevector!
```
(read-bytevector! bytevector)
(read-bytevector! bytevector port)
(read-bytevector! bytevector port start)
(read-bytevector! bytevector port start end)

Function read next bytes from binary input port and write them into byte vector.
if not start is specified it start to write into 0 position of the vector until
the end or end the vector if no end is specified.
```

## read-char
```
(read-char port)

Function read next character from input port.
```

## read-line
```
(read-char port)

Function read next character from input port.
```

## read-string
```
(read-string k)
(read-string k port)

Reads the next k characters, or as many as are available
before the end of file, from the textual input port into a
newly allocated string in left-to-right order and returns the
string. If no characters are available before the end of file,
an end-of-file object is returned.
```

## read-u8
```
(read-u8)
(read-u8 port)

Read next byte from input-binary port. If there are no more bytes
it return eof object.
```

## real-part
```
(real-part n)

Return real part of the complex number n.
```

## real?
```
(real? x)

Function check if argument x is real.
```

## reduce
```
(reduce fn init list . lists)

Higher order function take each element of the list and call
the function with result of previous call or init and next element
on the list until each element is processed and return single value
as result of last call to `fn` function.
e.g. it call (fn a3 b3 (fn a2 b2 (fn a1 b1 init)))
for (reduce fn init alist blist)
```

## regex?
```
(regex? x)

Function return true of value is regular expression, it return false otherwise.
```

## remainder
```
(% n1 n2)

Function get reminder of it's arguments.
```

## remainder__
```
(modulo a b)

Function return reminder from division operation.
```

## replace
```
(replace pattern replacement string)

Function change pattern to replacement inside string. Pattern can be string
or regex and replacement can be function or string.
```

## repr
```
(repr obj)

Function return string LIPS representation of an object as string.
```

## require
```
(require module)

Function to be used inside Node.js to import the module.
```

## require.resolve
```
(require.resolve path)

Return path relative the current module.
```

## reset
```
(reset)

Function reset environment and remove all user defined variables.
```

## response->buffer
```
#<undefined>
```

## response->content
```
(response->text binary res)

Function read all text from Node.js HTTP response object. If binary argument
is true it will return Buffer object that can be converted to u8vector.

***Warrning:*** it may overflow the stack (part of Node) when converting
whole buffer to u8vector.
```

## response->text
```
#<undefined>
```

## reverse
```
(reverse list)

Function will reverse the list or array. If value is not a list
or array it will throw exception.
```

## round
```
(round number)

Function calculate round of a number.
```

## s16vector
```
(s16vector v1 v2 ...)

Create signed 16-bit integer vector from give arguments.
```

## s16vector->list
```
#<undefined>
```

## s16vector-length
```
(s16vector-length v)

return length of signed 16-bit integer vector.
```

## s16vector-ref
```
(s16vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## s16vector-set!
```
(s16vector-set! vector k)

Function set value of signed 16-bit integer vector at index k. If index is out of range it throw exception.
```

## s16vector?
```
(s16vector? x)

Function return #t of argument is signed 16-bit integer vector otherwise it return #f.
```

## s32vector
```
(s32vector v1 v2 ...)

Create signed 32-bit integer vector from give arguments.
```

## s32vector->list
```
#<undefined>
```

## s32vector-length
```
(s32vector-length v)

return length of signed 32-bit integer vector.
```

## s32vector-ref
```
(s32vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## s32vector-set!
```
(s32vector-set! vector k)

Function set value of signed 32-bit integer vector at index k. If index is out of range it throw exception.
```

## s32vector?
```
(s32vector? x)

Function return #t of argument is signed 32-bit integer vector otherwise it return #f.
```

## s8vector
```
(s8vector v1 v2 ...)

Create signed 8-bit integer vector from give arguments.
```

## s8vector->list
```
#<undefined>
```

## s8vector-length
```
(s8vector-length v)

return length of signed 8-bit integer vector.
```

## s8vector-ref
```
(s8vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## s8vector-set!
```
(s8vector-set! vector k)

Function set value of signed 8-bit integer vector at index k. If index is out of range it throw exception.
```

## s8vector?
```
(s8vector? x)

Function return #t of argument is signed 8-bit integer vector otherwise it return #f.
```

## scheme-report-environment
```
(scheme-report-environment version)

Function return new Environment object for given Scheme Spec version.
Only argument 5 is supported that create environemnt for R5RS.
```

## search
```
(search pattern string)

Function return first found index of the pattern inside a string
```

## set-car!
```
(set-car! obj value)

Function that set car (head) of the list/pair to specified value.
It can destroy the list. Old value is lost.
```

## set-cdr!
```
(set-cdr! obj value)

Function that set cdr (tail) of the list/pair to specified value.
It can destroy the list. Old value is lost.
```

## set-current-directory!
```
(set-current-directory! string)

Function change current working directory to provided string.
```

## set-obj!
```
(set-obj! obj key value)

Function set property of JavaScript object
```

## set-repr!
```
(add-repr! type fn)

Function add string represention to the type, which should be constructor function.

Function fn should have args (obj q) and it should return string, obj is vlaue that
need to be converted to string, if the object is nested and you need to use `repr`,
it should pass second parameter q to repr, so string will be quoted when it's true.

e.g.: (lambda (obj q) (string-append "<" (repr obj q) ">"))
```

## set-special!
```
(set-special! symbol name [type])

Add special symbol to the list of transforming operators by the parser.
e.g.: `(add-special! "#" 'x)` will allow to use `#(1 2 3)` and it will be
transformed into (x (1 2 3)) so you can write x macro that will process
the list. 3rd argument is optional and it can be constant value
lips.specials.SPLICE if this constant is used it will transform
`#(1 2 3)` into (x 1 2 3) that is required by # that define vectors.
```

## sin
```
#<undefined>
```

## single
```
(single list)

Function check if argument is list with single element
```

## some
```
(some fn list)

Higher order function that call argument on each element of the list.
It stops when function fn return true for a value if so it will
return true. If none of the values give true, the function return false
```

## sort
```
(sort list [predicate])

Sort the list using optional predicate function. if not function is specified
it will use <= and sort in increasing order.
```

## split
```
(split separator string)

Function create list by splitting string by separatar that can
be a string or regular expression.
```

## sqrt
```
(sqrt number)

Function return square root of the number.
```

## square
```
(square z)

Returns the square of z. This is equivalent to (* z z).
```

## string
```
(string chr1 chr2 ...)

Function create new string from it's arguments. Each argument
Need to be a character object.
```

## string->list
```
(string->list string)

Function return list of characters created from string.
```

## string->number
```
(string->number number [radix])

Function convert string to number.
```

## string->symbol
```
(string->symbol string)

Function convert string to LIPS symbol.
```

## string->utf8
```
(string->utf8 string)
(string->utf8 string start)
(string->utf8 string start end)

Function converts string into u8 bytevector using utf8 encoding.
The start and end is the range of the input string for the conversion.
```

## string->vector
```
(string->list string)
(string->list string start)
(string->list string start end)

Function copy given range of string to list. If no start is specified it use
start of the string, if no end is specified it convert to the end of the string.
```

## string-append
```
(concat . strings)

Function create new string by joining its arguments
```

## string-ci<=?
```
(string-ci<? string1 string2)

Function return true if second string is not larger then the first one.
```

## string-ci<?
```
(string-ci<? string1 string2)

Function return true if second string is smaller then the first one.
```

## string-ci=?
```
(string-ci=? string1 string2)

Function check if two string s are equal.
```

## string-ci>=?
```
(string-ci>=? string1 string2)

Function return true if second character is not smaller then the first one.
```

## string-ci>?
```
(string-ci<? string1 string2)

Function return true if second string is larger then the first one.
```

## string-copy
```
(string-copy x)

Create new string based of given argument.
```

## string-fill!
```
(string-fill! symbol char)

Function destructively fill the string with given character.
```

## string-join
```
(join separator list)

Function return string by joining elements of the list
```

## string-length
```
(string-length string)

Function return length of the string.
```

## string-map
```
(string-map fn string1 stringr2 ...)

Function return new string from applying function fn to each element
of the strings, similar to map for lists.
```

## string-ref
```
(string-ref string k)

Function return character inside string at given zero-based index.
```

## string-split
```
(split separator string)

Function create list by splitting string by separatar that can
be a string or regular expression.
```

## string<=?
```
(string<? string1 string2)

Function return true if second string is not larger then the first one.
```

## string<?
```
(string<? string1 string2)

Function return true if second string is smaller then the first one.
```

## string=?
```
(string=? string1 string2)

Function check if two string s are equal.
```

## string>=?
```
(string<? string1 string2)

Function return true if second character is not smaller then the first one.
```

## string>?
```
(string<? string1 string2)

Function return true if second string is larger then the first one.
```

## string?
```
(string? expression)

Function check if value is a string.
```

## substring
```
(substring string start end)

Function return part of the string starting at start ending with end.
```

## symbol->string
```
(symbol->string symbol)

Function convert LIPS symbol to string.
```

## symbol=?
```
(symbol=? s1 s2 ...)

Function check if each value is symbol and it's the same acording to string=? predicate.
```

## symbol?
```
(symbol? expression)

Function check if value is LIPS symbol
```

## take
```
(take n list)

Return n first values of the list.
```

## tan
```
#<undefined>
```

## textual-port?
```
(textual-port? port)

Function test if argument is string port.
```

## throw
```
(throw string)

Throws new expection.
```

## tree->array
```
(tree->array list)

Function convert LIPS list structure into JavaScript array.
```

## tree-map
```
(tree-map fn tree)

Tree version of map. Function is invoked on every leaf.
```

## truncate
```
(truncate n)

Function return integer value from real number.
```

## truncate-quotient
```
#<undefined>
```

## truncate-remainder
```
#<undefined>
```

## truncate/
```
#<undefined>
```

## type
```
(type object)

Function return type of an object as string.
```

## typecheck
```
(typecheck label value type [position])

Function check type and throw exception if type don't match.
Type can be string or list of strings. Position optional argument
is used to created proper error message.
```

## typecheck-args
```
(typecheck-args args type)

Function check if all items in array are of same type.
```

## typed-array?
```
(typed-array? o)

Function test if argumnet is JavaScript typed array (Scheme byte vector).
```

## u16vector
```
(u16vector v1 v2 ...)

Create usigned 16-bit integer vector from give arguments.
```

## u16vector->list
```
#<undefined>
```

## u16vector-length
```
(u16vector-length v)

return length of usigned 16-bit integer vector.
```

## u16vector-ref
```
(u16vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## u16vector-set!
```
(u16vector-set! vector k)

Function set value of usigned 16-bit integer vector at index k. If index is out of range it throw exception.
```

## u16vector?
```
(u16vector? x)

Function return #t of argument is usigned 16-bit integer vector otherwise it return #f.
```

## u32vector
```
(u32vector v1 v2 ...)

Create usigned 32-bit integer vector from give arguments.
```

## u32vector->list
```
#<undefined>
```

## u32vector-length
```
(u32vector-length v)

return length of usigned 32-bit integer vector.
```

## u32vector-ref
```
(u32vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## u32vector-set!
```
(u32vector-set! vector k)

Function set value of usigned 32-bit integer vector at index k. If index is out of range it throw exception.
```

## u32vector?
```
(u32vector? x)

Function return #t of argument is usigned 32-bit integer vector otherwise it return #f.
```

## u8-ready?
```
(u8-ready?)
(u8-ready? port)

Returns #t if a byte is ready on the binary input port and returns #f otherwise.
If u8-ready? returns #t then the next read-u8 operation on the given port is
guaranteed not to hang. If the port is at end of file then u8-ready? returns #t.
```

## u8vector
```
(u8vector v1 v2 ...)

Create usigned 8-bit integer vector from give arguments.
```

## u8vector->list
```
#<undefined>
```

## u8vector-length
```
(u8vector-length v)

return length of usigned 8-bit integer vector.
```

## u8vector-ref
```
(u8vector-ref vector k)

Function return value frome vector at index k. If index is out of range it throw exception.
```

## u8vector-set!
```
(u8vector-set! vector k)

Function set value of usigned 8-bit integer vector at index k. If index is out of range it throw exception.
```

## u8vector?
```
(u8vector? x)

Function return #t of argument is usigned 8-bit integer vector otherwise it return #f.
```

## unary
```
(unary fn)

Function return new function with arguments limited to one.
```

## unbind
```
(unbind fn)

Function remove bidning from function so you can get props from it.
```

## unfold
```
(unfold fn init)

Function returns list from given function and init value. The function should
return cons where first is the item added to the list and second is next value
passed to the funtion. If function return false it end the loop.
```

## unquote
```
(unquote code)

Special form to be used in quasiquote macro, parser is processing special
characters , and create call to this pseudo function. It can be used
to evalute expression inside and return the value, the output is inserted
into list structure created by queasiquote.
```

## unquote-splicing
```
(unquote-splicing code)

Special form to be used in quasiquote macro, parser is processing special
characters ,@ and create call to this pseudo function. It can be used
to evalute expression inside and return the value without parenthesis.
the value will be joined to the output list structure.
```

## unset-repr!
```
(unset-repr! type)

Function remove string represention to the type, which should be constructor function,
added by add-repr! function.
```

## unset-special!
```
(unset-special! name)

Function remove special symbol from parser. Added by `set-special!`,
name must be a string.
```

## utf8->string
```
(utf8->string u8vector)
(utf8->string u8vector start)
(utf8->string u8vector start end)

Function converts u8 bytevector into string using utf8 encoding.
The start and end is the range of the input byte vector for the conversion.
```

## value
```
(value obj)

Function unwrap LNumbers and convert nil value to undefined.
```

## values
```
(values a1 a2 ...)

If called with more then one elment it will create special
Values object that can be used in call-with-values function
```

## values-ref
```
(values-ref values n)

Function return n value of values object which is result of value function.
```

## vector->f32vector
```
#<undefined>
```

## vector->f64vector
```
#<undefined>
```

## vector->list
```
(vector->list vector)
(vector->list vector start)
(vector->list vector start end)

Function copy given range of vector to list. If no start is specified it use
start of the vector, if no end is specified it convert to the end of the vector.
```

## vector->s16vector
```
#<undefined>
```

## vector->s32vector
```
#<undefined>
```

## vector->s8vector
```
#<undefined>
```

## vector->string
```
(vector->string vector)
(vector->string vector start)
(vector->string vector start end)

Function return new string created from vector of characters in given range.
If no start is given it create string from 0, if no end is given it return
string to the end.
```

## vector->u16vector
```
#<undefined>
```

## vector->u32vector
```
#<undefined>
```

## vector->u8vector
```
#<undefined>
```

## vector-append
```
(vector-append v1 v2 ...)

Function return new vector by combining it's arguments that should be vectors.
```

## vector-fill!
```
(vector-fill! vector fill)
(vector-fill! vector fill start)
(vector-fill! vector fill start end)

Fill vector with a given value in given range. If start is not given is start
at 0. If end is not given it fill till the end if the vector.
```

## vector-length
```
(vector-length vec)

Function return length of the vector. If argument is not vector it throw exception.
```

## vector-map
```
(vector-map fn vector1 vector2 ...)

Function return new vector from applying function fn to each element
of the vectors, similar to map for lists.
```

## vector-ref
```
(vector-ref vec n)

Function return nth element of the vector vec.
```

## vector-set!
```
(vector-set! vec n value)

Function set nth item of the vector to value.
```

## vector?
```
(vector? n)

Function return true of value is vector and false if not.
```

## with-exception-handler
```
(with-exception-handler handler thunk)

Procedure call and return value of thunk function, if exception happen
it call handler procedure.
```

## with-input-from-file
```
(with-input-from-file string thunk)

Procedure open file and make it current-input-port then thunk is executed.
After thunk is executed current-input-port is restored and file port
is closed.
```

## with-output-to-file
```
#<undefined>
```

## write
```
(write obj [port])

Write object to standard output or give port. For strings it will include
wrap in quotes.
```

## write-bytevector
```
(write-bytevector bytevector)
(write-bytevector bytevector port)

Write byte vector into binary output port.
```

## write-char
```
(write-char string)
(write-char string port)

Writes the character char (not an external representation of the character)
to the given textual output port and returns an unspecified value.
```

## write-string
```
(write-string string)
(write-string string port)
(write-string string port start)
(write-string string port start end)

Writes the characters of string from start to end in left-toright order
to the textual output port.
```

## write-u8
```
(write-u8 byte)
(write-u8 byte port)

Write byte into binary output port.
```

## zero?
```
(zero? x)

Function check if number is equal to 0
```

## |
```
(& a b)

Function calculate or bit operation.
```

## ~
```
(~ number)

Function negate the value.
```

