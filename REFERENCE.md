# Function Reference

## -
```
(- n1 n2 ...)
(- n)

Subtracts n2 and subsequent numbers from n1. If only one argument is passed
it will negate the value.
```

## .
```
(. obj . args)
(get obj . args)

This function uses an object as a base and keeps using arguments to get the
property of JavaScript object. Arguments need to be a strings.
e.g. `(. console "log")` if you use any function inside LIPS it
will be weakly bound (can be rebound), so you can call this log function
without problem unlike in JavaScript when you use
`var log = console.log`.
`get` is an alias because . doesn't work everywhere, e.g. you can't
pass it as an argument.
```

## *
```
(* . numbers)

Multiplies all numbers passed as arguments. If single value is passed
it will return that value.
```

## **
```
(** a b)

Function that calculates number a to to the power of b.
```

## /
```
(/ n1 n2 ...)
(/ n)

Divides n1 by n2 and subsequent arguments one by one. If single argument
is passed it will calculate (/ 1 n).
```

## &
```
(& a b)

Function that calculates the bitwise and operation.
```

## %
```
(% n1 n2)

Function returns the remainder of n1/n2 (modulo).
```

## +
```
(+ . numbers)

Sums all numbers passed as arguments. If single value is passed it will
return that value.
```

## <
```
(< x1 x2 ...)

Function that compares its numerical arguments and checks if they are
monotonically increasing, i.e. x1 < x2 and x2 < x3 and so on.
```

## <<
```
(<< a b)

Function that left shifts the value a by value b bits.
```

## <=
```
(<= x1 x2 ...)

Function that compares its numerical arguments and checks if they are
monotonically nondecreasing, i.e. x1 <= x2 and x2 <= x3 and so on.
```

## =
```
(== x1 x2 ...)

Function that compares its numerical arguments and checks if they are
all equal.
```

## ==
```
(== x1 x2 ...)

Function that compares its numerical arguments and checks if they are
all equal.
```

## >
```
(> x1 x2 x3 ...)

Function that compares its numerical arguments and checks if they are
monotonically decreasing, i.e. x1 > x2 and x2 > x3 and so on.
```

## >=
```
(>= x1 x2 ...)

Function that compares its numerical arguments and checks if they are
monotonically nonincreasing, i.e. x1 >= x2 and x2 >= x3 and so on.
```

## >>
```
(>> a b)

Function that right shifts the value a by value b bits.
```

## |
```
(| a b)

Function that calculates the bitwise or operation.
```

## ~
```
(~ number)

Function that calculates the bitwise inverse (flip all the bits).
```

## 1-
```
(1- number)

Function that subtracts 1 from the number and return result.
```

## 1+
```
(1+ number)

Function that adds 1 to the number and return result.
```

## abs
```
(abs number)

Function that returns the absolute value (magnitude) of number.
```

## acos
```
#<undefined>
```

## alist->assign
```
(alist->assign alist . list-of-alists)

Function that works like Object.assign but for LIPS alists.
```

## alist->object
```
(alist->object alist)

Function that converts alist pairs to a JavaScript object.
```

## always
```
(always constant)

Higher-order function that returns a new thunk that always returns the given constant when called.
```

## angle
```
(angle x)

Returns angle of the complex number in polar coordinate system.
```

## append
```
(append item ...)

Function that creates a new list with each argument appended end-to-end.
It will always return a new list and not modify its arguments.
```

## append!
```
(append! arg1 ...)

Destructive version of append, it can modify the lists in place. It returns
a new list where each argument is appended to the end. It may modify
lists added as arguments.
```

## apply
```
(apply fn list)

Function that calls fn with the list of arguments.
```

## apropos
```
(apropos name)

Search the current environment and display names that match the given name.
name can be regex, string or symbol.
```

## array->list
```
(array->list array)

Function that converts a JavaScript array to a LIPS cons list.
```

## array?
```
(array? expression)

Predicate that tests if value is an array.
```

## asin
```
#<undefined>
```

## assoc
```
(assoc obj alist)

Returns pair from alist that match given key using equal? check.
```

## assq
```
(assq obj alist)

Returns pair from a list that matches given key using eq? check.
```

## assv
```
(assv obj alist)

Returns pair from alist that match given key using eqv? check.
```

## atan
```
#<undefined>
```

## await
```
(await value)

Unquotes a quoted promise so it can be automagically evaluated (resolved
to its value).
```

## binary
```
(binary fn)

Returns a new function with arguments limited to two.
```

## binary-port?
```
(binary-port? port)

Function that tests if argument is binary port.
```

## boolean?
```
(boolean? x)

Returns true if value is boolean.
```

## boolean=?
```
(boolean=? b1 b2 ...)

Checks if all arguments are boolean and if they are the same.
```

## bound?
```
(bound? x [env])

Function that check if the variable is defined in the given environment, or interaction-environment
if not specified.
```

## buffer->u8vector
```
(buffer->u8vector bin)

Cross platform function that can be used in both Node and browser.
It can be used together with %read-file or %read-binary-file to convert
the result ArrayBuffer or Buffer to u8vector.
```

## bytevector
```
(u8vector v1 v2 ...)

Create unsigned 8-bit integer vector (C unsigned char) from give arguments.
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

Returns a new vector from start to end. If no start and end is provided
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

return length of unsigned 8-bit integer vector (C unsigned char).
```

## bytevector-u8-ref
```
(u8vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## bytevector-u8-set!
```
(u8vector-set! vector k)

Function set value of unsigned 8-bit integer vector (C unsigned char) at index k. If index is out of range it throw exception.
```

## bytevector?
```
(u8vector? x)

Returns #t of argument is unsigned 8-bit integer vector (C unsigned char) otherwise it return #f.
```

## caaaaar
```
(caaaaar arg)

Function that calculates (car (car (car (car (car arg)))))
```

## caaaadr
```
(caaaadr arg)

Function that calculates (car (car (car (car (cdr arg)))))
```

## caaaar
```
(caaaar arg)

Function that calculates (car (car (car (car arg))))
```

## caaadar
```
(caaadar arg)

Function that calculates (car (car (car (cdr (car arg)))))
```

## caaaddr
```
(caaaddr arg)

Function that calculates (car (car (car (cdr (cdr arg)))))
```

## caaadr
```
(caaadr arg)

Function that calculates (car (car (car (cdr arg))))
```

## caaar
```
(caaar arg)

Function that calculates (car (car (car arg)))
```

## caadaar
```
(caadaar arg)

Function that calculates (car (car (cdr (car (car arg)))))
```

## caadadr
```
(caadadr arg)

Function that calculates (car (car (cdr (car (cdr arg)))))
```

## caadar
```
(caadar arg)

Function that calculates (car (car (cdr (car arg))))
```

## caaddar
```
(caaddar arg)

Function that calculates (car (car (cdr (cdr (car arg)))))
```

## caadddr
```
(caadddr arg)

Function that calculates (car (car (cdr (cdr (cdr arg)))))
```

## caaddr
```
(caaddr arg)

Function that calculates (car (car (cdr (cdr arg))))
```

## caadr
```
(caadr arg)

Function that calculates (car (car (cdr arg)))
```

## caar
```
(caar arg)

Function that calculates (car (car arg))
```

## cadaaar
```
(cadaaar arg)

Function that calculates (car (cdr (car (car (car arg)))))
```

## cadaadr
```
(cadaadr arg)

Function that calculates (car (cdr (car (car (cdr arg)))))
```

## cadaar
```
(cadaar arg)

Function that calculates (car (cdr (car (car arg))))
```

## cadadar
```
(cadadar arg)

Function that calculates (car (cdr (car (cdr (car arg)))))
```

## cadaddr
```
(cadaddr arg)

Function that calculates (car (cdr (car (cdr (cdr arg)))))
```

## cadadr
```
(cadadr arg)

Function that calculates (car (cdr (car (cdr arg))))
```

## cadar
```
(cadar arg)

Function that calculates (car (cdr (car arg)))
```

## caddaar
```
(caddaar arg)

Function that calculates (car (cdr (cdr (car (car arg)))))
```

## caddadr
```
(caddadr arg)

Function that calculates (car (cdr (cdr (car (cdr arg)))))
```

## caddar
```
(caddar arg)

Function that calculates (car (cdr (cdr (car arg))))
```

## cadddar
```
(cadddar arg)

Function that calculates (car (cdr (cdr (cdr (car arg)))))
```

## caddddr
```
(caddddr arg)

Function that calculates (car (cdr (cdr (cdr (cdr arg)))))
```

## cadddr
```
(cadddr arg)

Function that calculates (car (cdr (cdr (cdr arg))))
```

## caddr
```
(caddr arg)

Function that calculates (car (cdr (cdr arg)))
```

## cadr
```
(cadr arg)

Function that calculates (car (cdr arg))
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

Calls the producer procedure with no arguments, then calls the
consumer procedure with the returned value as an argument -- unless
the returned value is a special Values object created by (values), if it is
the values are unpacked and the consumer is called with multiple arguments.
```

## car
```
(car pair)

This function returns the car (item 1) of the list.
```

## cdaaaar
```
(cdaaaar arg)

Function that calculates (cdr (car (car (car (car arg)))))
```

## cdaaadr
```
(cdaaadr arg)

Function that calculates (cdr (car (car (car (cdr arg)))))
```

## cdaaar
```
(cdaaar arg)

Function that calculates (cdr (car (car (car arg))))
```

## cdaadar
```
(cdaadar arg)

Function that calculates (cdr (car (car (cdr (car arg)))))
```

## cdaaddr
```
(cdaaddr arg)

Function that calculates (cdr (car (car (cdr (cdr arg)))))
```

## cdaadr
```
(cdaadr arg)

Function that calculates (cdr (car (car (cdr arg))))
```

## cdaar
```
(cdaar arg)

Function that calculates (cdr (car (car arg)))
```

## cdadaar
```
(cdadaar arg)

Function that calculates (cdr (car (cdr (car (car arg)))))
```

## cdadadr
```
(cdadadr arg)

Function that calculates (cdr (car (cdr (car (cdr arg)))))
```

## cdadar
```
(cdadar arg)

Function that calculates (cdr (car (cdr (car arg))))
```

## cdaddar
```
(cdaddar arg)

Function that calculates (cdr (car (cdr (cdr (car arg)))))
```

## cdadddr
```
(cdadddr arg)

Function that calculates (cdr (car (cdr (cdr (cdr arg)))))
```

## cdaddr
```
(cdaddr arg)

Function that calculates (cdr (car (cdr (cdr arg))))
```

## cdadr
```
(cdadr arg)

Function that calculates (cdr (car (cdr arg)))
```

## cdar
```
(cdar arg)

Function that calculates (cdr (car arg))
```

## cddaaar
```
(cddaaar arg)

Function that calculates (cdr (cdr (car (car (car arg)))))
```

## cddaadr
```
(cddaadr arg)

Function that calculates (cdr (cdr (car (car (cdr arg)))))
```

## cddaar
```
(cddaar arg)

Function that calculates (cdr (cdr (car (car arg))))
```

## cddadar
```
(cddadar arg)

Function that calculates (cdr (cdr (car (cdr (car arg)))))
```

## cddaddr
```
(cddaddr arg)

Function that calculates (cdr (cdr (car (cdr (cdr arg)))))
```

## cddadr
```
(cddadr arg)

Function that calculates (cdr (cdr (car (cdr arg))))
```

## cddar
```
(cddar arg)

Function that calculates (cdr (cdr (car arg)))
```

## cdddaar
```
(cdddaar arg)

Function that calculates (cdr (cdr (cdr (car (car arg)))))
```

## cdddadr
```
(cdddadr arg)

Function that calculates (cdr (cdr (cdr (car (cdr arg)))))
```

## cdddar
```
(cdddar arg)

Function that calculates (cdr (cdr (cdr (car arg))))
```

## cddddar
```
(cddddar arg)

Function that calculates (cdr (cdr (cdr (cdr (car arg)))))
```

## cdddddr
```
(cdddddr arg)

Function that calculates (cdr (cdr (cdr (cdr (cdr arg)))))
```

## cddddr
```
(cddddr arg)

Function that calculates (cdr (cdr (cdr (cdr arg))))
```

## cdddr
```
(cdddr arg)

Function that calculates (cdr (cdr (cdr arg)))
```

## cddr
```
(cddr arg)

Function that calculates (cdr (cdr arg))
```

## cdr
```
(cdr pair)

This function returns the cdr (all but first) of the list.
```

## ceiling
```
(ceiling number)

Function that calculates the ceiling of a number.
```

## char->integer
```
(char->integer chr)

Returns the codepoint of Unicode character.
```

## char-alphabetic?
```
(char-alphabetic? chr)

Returns true if character is leter of the ASCII alphabet.
```

## char-ci<?
```
(char-ci<? chr1 chr2)

Returns true if second character is smaller then the first one.
```

## char-ci<=?
```
(char-ci<? chr1 chr2)

Returns true if second character is not larger then the first one.
```

## char-ci=?
```
(char-ci=? chr1 chr2)

Checks if two characters are equal.
```

## char-ci>?
```
(char-ci<? chr1 chr2)

Returns true if second character is larger then the first one.
```

## char-ci>=?
```
(char-ci<? chr1 chr2)

Returns true if second character is not smaller then the first one.
```

## char-downcase
```
(char-downcase chr)

Create lowercase version of the character.
```

## char-lower-case?
```
(char-upper-case? char)

Checks if character is lower case.
```

## char-numeric?
```
(char-numeric? chr)

Returns true if character is number.
```

## char-ready?
```
(char-ready?)
(char-ready? port)

Checks if characters is ready in input port. This is useful mostly
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

Checks if character is upper case.
```

## char-whitespace?
```
(char-whitespace? chr)

Returns true if character is whitespace.
```

## char?
```
(char? obj)

Checks if the object is a character.
```

## char<?
```
(char<? chr1 chr2)

Returns true if second character is smaller then the first one.
```

## char<=?
```
(char<? chr1 chr2)

Returns true if second character is not larger then the first one.
```

## char=?
```
(char=? chr1 chr2)

Checks if two characters are equal.
```

## char>?
```
(char<? chr1 chr2)

Returns true if second character is larger then the first one.
```

## char>=?
```
(char<? chr1 chr2)

Returns true if second character is not smaller then the first one.
```

## clone
```
(clone list)

Function that returns a clone of the list, that does not share any pairs with the
original, so the clone can be safely mutated without affecting the original.
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

## command-line
```
(command-line)

Returns the command line arguments, or an empty list if not running under Node.js.
```

## complement
```
(complement fn)

Higher order function that returns the Boolean complement of the given function. If the function fn
for a given arguments return true the result function will return false, if it would
return false, the result function will return true.
```

## complex?
```
(complex? x)

Checks if argument x is complex.
```

## compose
```
(compose . fns)

Higher-order function that creates a new function that applies all functions
from right to left and returns the last value. Reverse of pipe.
e.g.:
((compose (curry + 2) (curry * 3)) 10) --> (+ 2 (* 3 10)) --> 32
```

## concat
```
(concat . strings)

Function that creates a new string by joining its arguments.
```

## cons
```
(cons left right)

This function returns a new list with the first appended
before the second. If the second is not a list cons will
return a dotted pair.
```

## cos
```
(cos n)

Function that calculates cosine of a number.
```

## current-directory
```
(current-directory)

Returns the current working directory, default is the path from where
the script was executed.
```

## current-environment
```
(current-environment)

Function that returns the current environment (they're first-class objects!)
```

## current-error-port
```
(current-output-port)

Returns the default stderr port.
```

## current-input-port
```
(current-input-port)

Returns the default stdin port.
```

## current-jiffy
```
(current-jiffy)

Return current jiffy. In LIPS is jiffy since start of the process.
You can divide this value by (jiffies-per-second) to get seconds since
start of the process. And you can add %%start-jiffy to get jiffy since
January 1, 1970.
```

## current-output-port
```
(current-output-port)

Returns the default stdout port.
```

## current-second
```
(current-second)

Functionn return exact integer of the seconds since January 1, 1970
```

## curry
```
(curry fn . args)

Higher-order function that creates a curried version of the function.
The result function will have partially applied arguments and it
will keep returning one-argument functions until all arguments are provided,
then it calls the original function with the accumulated arguments.

e.g.:
(define (add a b c d) (+ a b c d))
(define add1 (curry add 1))
(define add12 (add 2))
(display (add12 3 4))
```

## debugger
```
(debugger)

Function that triggers the JavaScript debugger (e.g. the browser devtools)
using the "debugger;" statement. If a debugger is not running this
function does nothing.
```

## defmacro?
```
(defmacro? expression)

Checks if object is a macro and it's expandable.
```

## degree->radians
```
(degree->radians x)

Convert degrees to radians.
```

## delete-file
```
(delete-file filename)

Deletes the file of given name.
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

Returns all props on the object including those in prototype chain.
```

## display
```
(display string [port])

This function outputs the string to the standard output or
the port if given. No newline.
```

## display-error
```
(display-error . args)

Display an error message on stderr.
```

## dynamic-wind
```
(dynamic-wind before thunk after)

Accepts 3 procedures/lambdas and executes before, then thunk, and 
always after even if an error occurs in thunk.
```

## empty?
```
(empty? object)

Function that returns #t if value is nil (an empty list) or undefined.
```

## env
```
(env)
(env obj)

Function that returns a list of names (functions, macros and variables)
that are bound in the current environment or one of its parents.
```

## environment-bound?
```
(environment-bound? env symbol)

Checks if symbol is a bound variable similar to bound?.
```

## environment?
```
(environment? obj)

Checks if object is a LIPS environment.
```

## eof-object
```
(eof-object)

Procedure returns eof object that indicate end of the port
```

## eof-object?
```
(eof-object? arg)

Checks if value is eof object, returned from input string
port when there are no more data to read.
```

## eq?
```
(eq? a b)

Function that compares two values if they are identical.
```

## equal?
```
(equal? a b)

The function checks if values are equal. If both are a pair or an array
it compares their elements recursively.
```

## eqv?
```
(eqv? a b)

Function that compares the values. It returns true if they are the same, they
need to have the same type.
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

Checks if object is of Error object thrown by error function.
```

## escape-regex
```
(escape-regex string)

Function that returns a new string where all special operators used in regex,
are escaped with backslashes so they can be used in the RegExp constructor
to match a literal string.
```

## eval
```
(eval expr)
(eval expr environment)

Function that evaluates LIPS Scheme code. If the second argument is provided
it will be the environment that the code is evaluated in.
```

## even?
```
(even? number)

Checks if number is even.
```

## every
```
(every fn list)

Function that calls fn on each item of the list, if every value returns true
it will return true otherwise it return false.
Analogous to Python all(map(fn, list)).
```

## exact
```
(inexact->exact number)

Function that converts real number to exact rational number.
```

## exact->inexact
```
(exact->inexact n)

Convert exact number to inexact.
```

## exact-integer?
```
(exact-integer? n)

Returns #t if z is both exact and an integer; otherwise
returns #f.
```

## exact?
```
(exact? n)
```

## exp
```
(exp n)

Function that calculates e raised to the power of n.
```

## expt
```
(** a b)

Function that calculates number a to to the power of b.
```

## f32vector
```
(f32vector v1 v2 ...)

Create 32-bit IEEE-754 floating point number vector (C float) from give arguments.
```

## f32vector->list
```
#<undefined>
```

## f32vector-length
```
(f32vector-length v)

return length of 32-bit IEEE-754 floating point number vector (C float).
```

## f32vector-ref
```
(f32vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## f32vector-set!
```
(f32vector-set! vector k)

Function set value of 32-bit IEEE-754 floating point number vector (C float) at index k. If index is out of range it throw exception.
```

## f32vector?
```
(f32vector? x)

Returns #t of argument is 32-bit IEEE-754 floating point number vector (C float) otherwise it return #f.
```

## f64vector
```
(f64vector v1 v2 ...)

Create 64-bit IEEE-754 floating point number vector (C double) from give arguments.
```

## f64vector->list
```
#<undefined>
```

## f64vector-length
```
(f64vector-length v)

return length of 64-bit IEEE-754 floating point number vector (C double).
```

## f64vector-ref
```
(f64vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## f64vector-set!
```
(f64vector-set! vector k)

Function set value of 64-bit IEEE-754 floating point number vector (C double) at index k. If index is out of range it throw exception.
```

## f64vector?
```
(f64vector? x)

Returns #t of argument is 64-bit IEEE-754 floating point number vector (C double) otherwise it return #f.
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

Higher-order function that calls `fn` for each element of the list
and return a new list for only those elements for which fn returns
a truthy value. If called with a regex it will create a matcher function.
```

## find
```
(find fn list)
(find regex list)

Higher-order function that finds the first value for which fn return true.
If called with a regex it will create a matcher function.
```

## finite?
```
(finite? x)

Checks if value is finite.
```

## flatten
```
(flatten list)

Returns a shallow list from tree structure (pairs).
```

## flip
```
(flip fn)

Higher-order function that returns a new function where the first two arguments are swapped.

Example:

  (define first (curry (flip vector-ref) 0))
  (first #(1 2 3))
  ;; ==> 1
```

## floor
```
(floor number)

Function that calculates the floor of a number.
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

## flush-output
```
(flush-output [port])

If output-port is buffered, this causes the contents of its buffer to be written to
the output device. Otherwise it has no effect. Returns an unspecified value.
```

## flush-output-port
```
(flush-output-port port)

Function do nothing, flush is not needed in LIPS in both NodeJS and Browser.
The function is added, so it don't throw exception when using R7RS code.
```

## fold
```
(fold fn init . lists)

Function fold is left-to-right reversal of reduce. It call `fn`
on each pair of elements of the list and returns a single value.
e.g. it computes (fn 'a 'x (fn 'b 'y (fn 'c 'z 'foo)))
for: (fold fn 'foo '(a b c) '(x y z))
```

## fold-left
```
(fold fn init . lists)

Function fold is left-to-right reversal of reduce. It call `fn`
on each pair of elements of the list and returns a single value.
e.g. it computes (fn 'a 'x (fn 'b 'y (fn 'c 'z 'foo)))
for: (fold fn 'foo '(a b c) '(x y z))
```

## fold-right
```
(reduce fn init list . lists)

Higher-order function that takes each element of the list and calls
the fn with result of previous call or init and the next element
of the list until each element is processed, and returns a single value
as result of last call to `fn` function.
e.g. it computes (fn 'c 'z (fn 'b 'y (fn 'a 'x 'foo)))
for: (reduce fn 'foo '(a b c) '(x y z))
```

## for-each
```
(for-each fn . lists)

Higher-order function that calls function `fn` on each
value of the argument. If you provide more than one list
it will take each value from each list and call `fn` function
with that many arguments as number of list arguments.
```

## force
```
(force promise)

Function that forces the promise and evaluates the delayed expression.
```

## format
```
(format string n1 n2 ...)

This function accepts a string template and replaces any
escape sequences in its inputs:

* ~a value as if printed with `display`
* ~s value as if printed with `write`
* ~% newline character
* ~~ literal tilde '~'

If there are missing inputs or other escape characters it
will error.
```

## function?
```
(function? expression)

Predicate that tests if value is a callable function.
```

## gcd
```
(gcd n1 n2 ...)

Function that returns the greatest common divisor of the arguments.
```

## gensym
```
(gensym)

Generates a unique symbol that is not bound anywhere,
to use with macros as meta name.
```

## gensym-interal
```
(gensym-interal symbol)

Parser extension that creates a new quoted named gensym.
```

## gensym?
```
(gensym? value)

Returns #t if value is a symbol created by gensym. It returns #f otherwise.
```

## get
```
(. obj . args)
(get obj . args)

This function uses an object as a base and keeps using arguments to get the
property of JavaScript object. Arguments need to be a strings.
e.g. `(. console "log")` if you use any function inside LIPS it
will be weakly bound (can be rebound), so you can call this log function
without problem unlike in JavaScript when you use
`var log = console.log`.
`get` is an alias because . doesn't work everywhere, e.g. you can't
pass it as an argument.
```

## get-environment-variable
```
(get-environment-variable name)

Returns given environment variable. This function throws exception
when called in browser.
```

## get-environment-variables
```
(get-environment-variables)

Returns all process environment variables as an alist. This function throws exception
when called in browser.
```

## get-output-bytevector
```
(get-output-string port)

Gets full string from string port. If nothing was wrote
to given port it will return empty string.
```

## get-output-string
```
(get-output-string port)

Gets full string from string port. If nothing was wrote
to given port it will return empty string.
```

## get-resource
```
(get-resource url)

Load JavaScript or CSS file in browser by adding script/link tag to head of the current document.
When called from Node it allow it allows to load JavaScript files only.
```

## http-get
```
(http-get url)

Node.js function that sends a HTTP Request and returns a string or
binary Buffer object.
```

## identity
```
(identity n)

No-op function. It just returns its argument.
```

## imag-part
```
(imag-part n)

Return imaginary part of the complex number n.
```

## in
```
(in key value)

Function that uses the Javascript "in" operator to check if key is
a valid property in the value.
```

## indexed-db?
```
(indexed-db?)

Function that tests if IndexedDB is available.
```

## inexact
```
(exact->inexact n)

Convert exact number to inexact.
```

## inexact->exact
```
(inexact->exact number)

Function that converts real number to exact rational number.
```

## inexact?
```
(inexact? n)
```

## infinite?
```
(infinite? x)

Checks if value is infinite.
```

## input-port-open?
```
(input-port-open? port)

Checks if argument is input-port and if you can read from it.
```

## input-port?
```
(input-port? arg)

Returns true if argument is input port.
```

## instanceof
```
(instanceof type obj)

Predicate that tests if the obj is an instance of type.
```

## integer->char
```
(integer->char chr)

Function that converts number argument to character.
```

## integer?
```
(integer? x)

Checks if the argument x is integer.
```

## interaction-environment
```
(interaction-environment)

Returns the interaction environment equal to lips.env. This can be overwritten
when creating new interpreter with lips.Interpreter.
```

## iterator?
```
(iterator? x)

 Checks if value is JavaScript iterator object.
```

## jiffies-per-second
```
#<undefined>
```

## join
```
(join separator list)

Function that returns a string by joining elements of the list using separator.
```

## key->string
```
(key->string symbol)

If symbol is a keyword it converts that to string and removes the colon.
```

## key?
```
(key? symbol)

Checks if symbol is a keyword (has a colon as first character).
```

## lcm
```
(lcm n1 n2 ...)

Function that returns the least common multiple of the arguments.
```

## length
```
(length expression)

Function that returns the length of the object. The object can be a LIPS
list or any object that has a "length" property. Returns undefined if the
length could not be found.
```

## list
```
(list . args)

Function that creates a new list out of its arguments.
```

## list->array
```
(list->array list)

Function that converts a LIPS list into a JavaScript array.
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

Returns a string from a list of characters.
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

Function that converts a LIPS list into a JavaScript array.
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

Checks if consecutive elements of the list match the predicate function.
```

## list-ref
```
(list-ref list n)

Returns n-th element of a list.
```

## list-tail
```
(list-tail list k)

Returns the sublist of list obtained by omitting the first k elements.
```

## list?
```
(list? obj)

Predicate that tests if value is a proper linked list structure.
The car of each pair can be any value. It returns false on cyclic lists."
```

## load
```
(load filename)
(load filename environment)

Fetches the file (from disk or network) and evaluates its content as LIPS code.
If the second argument is provided and it's an environment the evaluation
will happen in that environment.
```

## log
```
(log z)

Function that calculates natural logarithm of z where the argument can be
any number (including complex negative and rational).
If the value is 0 it return NaN.
```

## macro?
```
(macro? expression)

Predicate that tests if value is a macro.
```

## magnitude
```
(magnitude x)

Returns magnitude of the complex number in polar coordinate system.
```

## make-bytevector
```
(make-u8vector k fill)

Allocate new unsigned 8-bit integer vector (C unsigned char) of length k, with optional initial values.
```

## make-f32vector
```
(make-f32vector k fill)

Allocate new 32-bit IEEE-754 floating point number vector (C float) of length k, with optional initial values.
```

## make-f64vector
```
(make-f64vector k fill)

Allocate new 64-bit IEEE-754 floating point number vector (C double) of length k, with optional initial values.
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

Function that creates a promise from a function.
```

## make-rectangular
```
(make-rectangular im re)

Creates a complex number from imaginary and real part (a+bi form).
```

## make-s16vector
```
(make-s16vector k fill)

Allocate new signed 16-bit integer vector (C short) of length k, with optional initial values.
```

## make-s32vector
```
(make-s32vector k fill)

Allocate new signed 32-bit integer vector (C unsigned int) of length k, with optional initial values.
```

## make-s8vector
```
(make-s8vector k fill)

Allocate new signed 8-bit integer vector (C signed char) of length k, with optional initial values.
```

## make-string
```
(make-string k [char])

Returns new string with k elements. If char is provided
it's filled with that character.
```

## make-tags
```
(make-tags expression)

Returns a list structure of code with better syntax then raw LIPS
```

## make-u16vector
```
(make-u16vector k fill)

Allocate new unsigned 16-bit integer vector (C unsigned short) of length k, with optional initial values.
```

## make-u32vector
```
(make-u32vector k fill)

Allocate new unsigned 32-bit integer vector (C int) of length k, with optional initial values.
```

## make-u8vector
```
(make-u8vector k fill)

Allocate new unsigned 8-bit integer vector (C unsigned char) of length k, with optional initial values.
```

## make-vector
```
(make-vector n [fill])

Creates a new vector with n empty elements. If fill is specified it will set
all elements of the vector to that value.
```

## map
```
(map fn . lists)

Higher-order function that calls function `fn` with each
value of the list. If you provide more then one list as argument
it will take each value from each list and call `fn` function
with that many argument as number of list arguments. The return
values of the fn calls are accumulated in a result list and
returned by map.
```

## match
```
(match pattern string)

Function that returns a match object from JavaScript as a list or #f if
no match.
```

## max
```
(max n1 n2 ...)

Returns the maximum of its arguments.
```

## member
```
(member obj list)

Returns first object in the list that match using equal? function.
```

## memq
```
(memq obj list)

Returns first object in the list that match using eq? function.
```

## memv
```
(memv obj list)

Returns first object in the list that match using eqv? function.
```

## min
```
(min n1 n2 ...)

Returns the minimum of its arguments.
```

## modulo
```
(modulo a b)

Returns modulo operation on its argumennts.
```

## n-ary
```
(n-ary n fn)

Returns a new function that limits the number of arguments to n.
```

## nan?
```
(nan? x)

Checks if argument x is Not a Number (NaN) value.
```

## native-symbol?
```
(native-symbol? object)

Checks if value is JavaScript Symbol.
```

## native.number
```
(native.number obj)

If argument is a number it will convert it to a native number.
```

## negative?
```
(negative? x)

Checks if the number is smaller then 0
```

## new
```
(new obj . args)

Function that creates new JavaScript instance of an object.
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

Returns true if value is false and false otherwise.
```

## nth
```
(nth index obj)

Function that returns the nth element of the list or array.
If used with a non-indexable value it will error.
```

## null-environment
```
(null-environment)

Returns a clean environment with only the standard library.
```

## null?
```
(null? expression)

Predicate that tests if value is null-ish (i.e. undefined, nil, or
Javascript null).
```

## number->string
```
(number->string x [radix])

Function that converts number to string with optional radix (number base).
```

## number?
```
(number? expression)

Predicate that tests if value is a number or NaN value.
```

## numbers?
```
#<undefined>
```

## numerator
```
(numerator n)

Return numerator of rational or same number if n is not rational.
```

## object->alist
```
(object->alist object)

Function that converts a JavaScript object to Alist
```

## object-expander
```
(object-expander readonly '(:foo (:bar 10) (:baz (1 2 3))))
(object-expander readonly '(:foo :bar))

Recursive function helper for defining LIPS code to create objects
using key like syntax. If no values are used it will create a JavaScript
shorthand objects where keys are used for keys and the values.
```

## object?
```
(object? expression)

Predicate that tests if value is an plain object (not another LIPS type).
```

## odd?
```
(odd? number)

Checks if number is odd.
```

## once
```
(once fn)

Higher-order function that returns a new function, that only calls the original
on the first invocation, and immediately returns the first call's result again
on subsequent invocations.
```

## open-binary-input-file
```
(open-binary-input-file filename)

Returns new Input Binary Port with given filename. In Browser
user need to provide global fs variable that is instance of FS interface.
```

## open-binary-output-file
```
(open-binary-output-file filename)

Opens file and return port that can be used for writing. If file
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

Returns new Input Port with given filename. In Browser user need to
provide global fs variable that is instance of FS interface.
```

## open-input-string
```
(open-input-string string)

Creates new string port as input that can be used to
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

Function that opens file and return port that can be used for writing. If file
exists it will throw an Error.
```

## open-output-string
```
(open-output-string)

Creates new output port that can used to write string into
and after finish get the whole string using `get-output-string`.
```

## output-port-open?
```
(output-port-open? port)

Checks if argument is output-port and if you can write to it.
```

## output-port?
```
(output-port? arg)

Returns true if argument is output port.
```

## pair-map
```
(pair-map fn list)

Function that calls fn argument for pairs in a list and returns a combined list with
values returned from function fn. It works likes map but take two items from the list each time.
```

## pair?
```
(pair? expression)

Predicate that tests if value is a pair or list structure.
```

## parent.frame
```
(parent.frame)

Returns the parent environment if called from inside a function.
If no parent frame can be found it returns nil.
```

## parent.frames
```
(parent.frames)

Returns the list of environments from parent frames (lambda function calls)
```

## peek-char
```
(peek-char port)

This function reads and returns a character from the string
port, or, if there is no more data in the string port, it
returns an EOF.
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

Higher-order function that creates a new function that applies all functions
from left to right and returns the last value. Reverse of compose.
e.g.:
((pipe (curry + 2) (curry * 3)) 10) --> (* 3 (+ 2 10)) --> 36
```

## plain-object?
```
(plain-object? x)

Checks if value is a plain JavaScript object created using the object macro.
```

## pluck
```
(pluck . strings)

If called with a single string it will return a function that when
called with an object will return that key from the object.
If called with more then one string the returned function will
create a new object by copying all properties from the given object.
```

## port?
```
(port? x)

Returns true if the argument is an input or output port object.
```

## positive?
```
(positive? x)

Checks if the number is larger then 0
```

## pprint
```
(pprint expression)

This function will pretty print its input to stdout. If it is called
with a non-list, it will just call the print function on its
input.
```

## pretty-format
```
(pretty-format pair)

Returns a pretty printed string from pair expression.
```

## print
```
(print . args)

This function converts each input into a string and prints
the result to the standard output (by default it's the
console but it can be defined in user code). This function
calls `(newline)` after printing each input.
```

## procedure?
```
(function? expression)

Predicate that tests if value is a callable function.
```

## promise?
```
(promise? obj)

Checks if the value is a promise created with delay or make-promise.
```

## promisify
```
(promisify fn)

Simple function for adding promises to NodeJS two-callback based functions.
Function tested only with fs module.
```

## prototype?
```
(prototype? obj)

Predicate that tests if value is a valid JavaScript prototype,
i.e. calling (new) with it will not throw '<x> is not a constructor'.
```

## qsort
```
(qsort list predicate)

Sorts the list using the quick sort algorithm according to predicate.
```

## quoted-symbol?
```
(quoted-symbol? code)

Helper function that tests if value is a quoted symbol. To be used in macros
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

Return quotient from division as integer.
```

## quotient&remainder
```
#<undefined>
```

## radians->degree
```
(radians->degree x)

Convert radians to degrees.
```

## raise
```
(raise obj)

Throws the object verbatim (no wrapping an a new Error).
```

## random
```
(random)
(random seed)

Function that generates new random real number using Knuth algorithm.
```

## range
```
(range stop)
(range start stop)
(range start stop step)

Returns a list of numbers from start to stop with optional step.
If start is not defined it starts from 0. If start is larger than stop
the step needs to be negative otherwise it will hang in an infinite loop.
```

## rational?
```
(rational? x)

Checks if the value is rational.
```

## rationalize
```
(rationalize number tolerance)

Returns simplest rational number approximation differing from number by no more
than the tolerance.
```

## read
```
(read [string])

This function, if used with a string, will parse it and
return the LIPS code, if there is any. If called with a
port, it will parse the next item from the port. If called
without an input, it will read a string from standard input
(using the browser's prompt or a user defined input method)
and calls itself with that string. This function can be used
together with `eval` to evaluate code from a string.
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

Reads next bytes from binary input port and write them into byte vector.
if not start is specified it start to write into 0 position of the vector until
the end or end the vector if no end is specified.
```

## read-char
```
(read-char port)

This function reads and returns the next character from the
input port.
```

## read-line
```
(read-char port)

This function reads and returns the next line from the input
port.
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

Checks if the argument x is real.
```

## reduce
```
(reduce fn init list . lists)

Higher-order function that takes each element of the list and calls
the fn with result of previous call or init and the next element
of the list until each element is processed, and returns a single value
as result of last call to `fn` function.
e.g. it computes (fn 'c 'z (fn 'b 'y (fn 'a 'x 'foo)))
for: (reduce fn 'foo '(a b c) '(x y z))
```

## regex?
```
(regex? x)

Returns true if value is a regular expression, or false otherwise.
```

## remainder
```
(% n1 n2)

Function returns the remainder of n1/n2 (modulo).
```

## remainder__
```
(modulo a b)

Returns remainder from division operation.
```

## replace
```
(replace pattern replacement string)

Function that changes pattern to replacement inside string. Pattern can be a
string or regex and replacement can be function or string. See Javascript
String.replace().
```

## repr
```
(repr obj)

Function that returns a LIPS code representation of the object as a string.
```

## require
```
(require module)

Function used inside Node.js to import a module.
```

## require.resolve
```
(require.resolve path)

Returns the path relative to the current module.

Only available when LIPS is running under Node.js.
```

## reset
```
(reset)

Function resets the environment and removes all user defined variables.
```

## response->buffer
```
#<undefined>
```

## response->content
```
(response->text binary res)

Reads all text from a Node.js HTTP response object. If binary argument
is true it will return Buffer object that can be converted to u8vector.

***Warning:*** it may overflow the Javascript call stack when converting the
whole buffer to u8vector, because LIPS doesn't have TCO.
```

## response->text
```
#<undefined>
```

## reverse
```
(reverse list)

Function that reverses the list or array. If value is not a list
or array it will error.
```

## round
```
(round number)

Function that calculates the round of a number.
```

## s16vector
```
(s16vector v1 v2 ...)

Create signed 16-bit integer vector (C short) from give arguments.
```

## s16vector->list
```
#<undefined>
```

## s16vector-length
```
(s16vector-length v)

return length of signed 16-bit integer vector (C short).
```

## s16vector-ref
```
(s16vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## s16vector-set!
```
(s16vector-set! vector k)

Function set value of signed 16-bit integer vector (C short) at index k. If index is out of range it throw exception.
```

## s16vector?
```
(s16vector? x)

Returns #t of argument is signed 16-bit integer vector (C short) otherwise it return #f.
```

## s32vector
```
(s32vector v1 v2 ...)

Create signed 32-bit integer vector (C unsigned int) from give arguments.
```

## s32vector->list
```
#<undefined>
```

## s32vector-length
```
(s32vector-length v)

return length of signed 32-bit integer vector (C unsigned int).
```

## s32vector-ref
```
(s32vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## s32vector-set!
```
(s32vector-set! vector k)

Function set value of signed 32-bit integer vector (C unsigned int) at index k. If index is out of range it throw exception.
```

## s32vector?
```
(s32vector? x)

Returns #t of argument is signed 32-bit integer vector (C unsigned int) otherwise it return #f.
```

## s8vector
```
(s8vector v1 v2 ...)

Create signed 8-bit integer vector (C signed char) from give arguments.
```

## s8vector->list
```
#<undefined>
```

## s8vector-length
```
(s8vector-length v)

return length of signed 8-bit integer vector (C signed char).
```

## s8vector-ref
```
(s8vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## s8vector-set!
```
(s8vector-set! vector k)

Function set value of signed 8-bit integer vector (C signed char) at index k. If index is out of range it throw exception.
```

## s8vector?
```
(s8vector? x)

Returns #t of argument is signed 8-bit integer vector (C signed char) otherwise it return #f.
```

## scheme-report-environment
```
(scheme-report-environment version)

Returns new Environment object for given Scheme Spec version.
Only argument 5 is supported that create environment for R5RS.
```

## search
```
(search pattern string)

Function that returns the first found index of the pattern inside a string.
```

## set-car!
```
(set-car! obj value)

Function that sets the car (first item) of the list/pair to specified value.
The old value is lost.
```

## set-cdr!
```
(set-cdr! obj value)

Function that sets the cdr (tail) of the list/pair to specified value.
It will destroy the list. The old tail is lost.
```

## set-current-directory!
```
(set-current-directory! string)

Changes the current working directory to provided string.
```

## set-obj!
```
(set-obj! obj key value)
(set-obj! obj key value props)

Function set a property of a JavaScript object. props should be a vector of pairs,
passed to Object.defineProperty.
```

## set-repr!
```
(add-repr! type fn)

Function that adds the string representation to the type, which should be a constructor function.

Function fn should have args (obj q) and it should return a string. obj is the value that
need to be converted to a string. If the object is nested and you need to use `repr` recursively,
it should pass the second parameter q to repr, so string will be quoted when it's true.

e.g.: (lambda (obj q) (string-append "<" (repr obj q) ">"))
```

## set-special!
```
(set-special! symbol name [type])

Add a special symbol to the list of transforming operators by the parser.
e.g.: `(add-special! "#" 'x)` will allow to use `#(1 2 3)` and it will be
transformed into (x (1 2 3)) so you can write x macro that will process
the list. 3rd argument is optional, and it can be one of two values:
lips.specials.LITERAL, which is the default behavior, or
lips.specials.SPLICE which causes the value to be unpacked into the expression.
This can be used for e.g. to make `#(1 2 3)` into (x 1 2 3) that is needed
by # that defines vectors.
```

## sin
```
(sin n)

Function that calculates sine of a number.
```

## single
```
(single list)

Checks if argument is list with one element.
```

## some
```
(some fn list)

Higher-order function that calls fn on each element of the list.
It stops and returns true when fn returns true for a value.
If none of the values give true, some will return false.
Analogous to Python any(map(fn, list)).
```

## sort
```
(sort list [predicate])

Sorts the list using optional predicate function. If no comparison function is given
it will use <= and sort in increasing order.
```

## split
```
(split separator string)

Function that creates a list by splitting string by separator which can
be a string or regular expression.
```

## sqrt
```
(sqrt number)

Function that returns the square root of the number.
```

## square
```
(square z)

Returns the square of z. This is equivalent to (* z z).
```

## string
```
(string chr1 chr2 ...)

Function that creates a new string from it's arguments. Each argument
needs to be a character object.
```

## string->list
```
(string->list string)

Returns a list of characters created from string.
```

## string->number
```
(string->number number [radix])

Function that parses a string into a number.
```

## string->symbol
```
(string->symbol string)

Function that converts a string to a LIPS symbol.
```

## string->utf8
```
(string->utf8 string)
(string->utf8 string start)
(string->utf8 string start end)

Converts string into u8 bytevector using utf8 encoding.
The start and end is the range of the input string for the conversion.
```

## string->vector
```
(string->list string)
(string->list string start)
(string->list string start end)

Function that copies given range of string to list. If no start is specified it use
start of the string, if no end is specified it convert to the end of the string.
```

## string-append
```
(concat . strings)

Function that creates a new string by joining its arguments.
```

## string-ci<?
```
(string-ci<? string1 string2)

Returns true if the second string is smaller than the first one, ignoring case.
```

## string-ci<=?
```
(string-ci<? string1 string2)

Returns true if the second string is not larger than the first one, ignoring case.
```

## string-ci=?
```
(string-ci=? string1 string2)

Checks if two strings are equal, ignoring case.
```

## string-ci>?
```
(string-ci<? string1 string2)

Returns true if the second string is larger than the first one, ignoring case.
```

## string-ci>=?
```
(string-ci>=? string1 string2)

Returns true if second character is not smaller than the first one, ignoring case.
```

## string-copy
```
(string-copy x)

Creates a new string based on given argument.
```

## string-fill!
```
(string-fill! symbol char)

Function that destructively fills the string with given character.
```

## string-join
```
(join separator list)

Function that returns a string by joining elements of the list using separator.
```

## string-length
```
(string-length string)

Returns the length of the string.
```

## string-map
```
(string-map fn string1 stringr2 ...)

Returns new string from applying function fn to each element
of the strings, similar to map for lists.
```

## string-ref
```
(string-ref string k)

Returns character inside string at given zero-based index.
```

## string-split
```
(split separator string)

Function that creates a list by splitting string by separator which can
be a string or regular expression.
```

## string?
```
(string? expression)

Predicate that tests if value is a string.
```

## string<?
```
(string<? string1 string2)

Returns true if the second string is smaller than the first one.
```

## string<=?
```
(string<? string1 string2)

Returns true if the second string is not larger than the first one.
```

## string=?
```
(string=? string1 string2)

Checks if two strings are equal.
```

## string>?
```
(string<? string1 string2)

Returns true if the second string is larger than the first one.
```

## string>=?
```
(string<? string1 string2)

Returns true if second character is not smaller then the first one.
```

## substring
```
(substring string start end)

Function that returns the slice of the string starting at start and ending
with end.
```

## sxml-unquote
```
(sxml-unquote expression) or ~expression

Treat expression as code and evaluate it inside sxml, similar to unquote
with quasiquote.
```

## sxml-unquote-mapper
```
#<undefined>
```

## symbol->string
```
(symbol->string symbol)

Function that converts a LIPS symbol to a string.
```

## symbol-append
```
(symbol-append s1 s2 ...)

Function that creates a new symbol from symbols passed as arguments.
```

## symbol?
```
(symbol? expression)

Predicate that tests if value is a LIPS symbol.
```

## symbol=?
```
(symbol=? s1 s2 ...)

Checks if each value is symbol and it's the same according to string=? predicate.
```

## take
```
(take n list)

Returns n first values of the list.
```

## tan
```
(tan n)

Function that calculates tangent of a number.
```

## textual-port?
```
(textual-port? port)

Function that tests if argument is string port.
```

## throw
```
(throw string)

Throws a new exception.
```

## tree->array
```
(tree->array list)

Function that converts a LIPS cons tree structure into a JavaScript array.
```

## tree-map
```
(tree-map fn tree)

Tree version of map. fn is invoked on every leaf.
```

## truncate
```
(truncate n)

Function that returns the integer part (floor) of a real number.
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

Function that returns the type of an object as string.
```

## typecheck
```
(typecheck label value type [position])

Checks the type of value and errors if the type is not one allowed.  Type can be
string or list of strings. The position optional argument is used to create a
proper error message for the nth argument of function calls.
```

## typecheck-args
```
(typecheck-args args type)

Function that makes sure that all items in the array are of same type.
```

## typed-array?
```
(typed-array? o)

Function that tests if the arguments is a JavaScript typed array (Scheme byte vector).
```

## u16vector
```
(u16vector v1 v2 ...)

Create unsigned 16-bit integer vector (C unsigned short) from give arguments.
```

## u16vector->list
```
#<undefined>
```

## u16vector-length
```
(u16vector-length v)

return length of unsigned 16-bit integer vector (C unsigned short).
```

## u16vector-ref
```
(u16vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## u16vector-set!
```
(u16vector-set! vector k)

Function set value of unsigned 16-bit integer vector (C unsigned short) at index k. If index is out of range it throw exception.
```

## u16vector?
```
(u16vector? x)

Returns #t of argument is unsigned 16-bit integer vector (C unsigned short) otherwise it return #f.
```

## u32vector
```
(u32vector v1 v2 ...)

Create unsigned 32-bit integer vector (C int) from give arguments.
```

## u32vector->list
```
#<undefined>
```

## u32vector-length
```
(u32vector-length v)

return length of unsigned 32-bit integer vector (C int).
```

## u32vector-ref
```
(u32vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## u32vector-set!
```
(u32vector-set! vector k)

Function set value of unsigned 32-bit integer vector (C int) at index k. If index is out of range it throw exception.
```

## u32vector?
```
(u32vector? x)

Returns #t of argument is unsigned 32-bit integer vector (C int) otherwise it return #f.
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

Create unsigned 8-bit integer vector (C unsigned char) from give arguments.
```

## u8vector->list
```
#<undefined>
```

## u8vector-length
```
(u8vector-length v)

return length of unsigned 8-bit integer vector (C unsigned char).
```

## u8vector-ref
```
(u8vector-ref vector k)

Returns value from vector at index k. If index is out of range it throw exception.
```

## u8vector-set!
```
(u8vector-set! vector k)

Function set value of unsigned 8-bit integer vector (C unsigned char) at index k. If index is out of range it throw exception.
```

## u8vector?
```
(u8vector? x)

Returns #t of argument is unsigned 8-bit integer vector (C unsigned char) otherwise it return #f.
```

## unary
```
(unary fn)

Returns a new function with arguments limited to one.
```

## unbind
```
(unbind fn)

Function that removes the weak 'this' binding from a function so you
can get properties from the actual function object.
```

## unfold
```
(unfold fn init)

Returns a list from the given function and init value. The function should
return a pair where first is the item added to the list and second is next value
passed to the function. If the function returns false it ends the loop.
```

## unquote
```
(unquote code) or ,code

Special form used in the quasiquote macro. It evaluates the expression inside and
substitutes the value into quasiquote's result.
```

## unquote-splicing
```
(unquote-splicing code) or ,@code

Special form used in the quasiquote macro. It evaluates the expression inside and
splices the list into quasiquote's result. If it is not the last element of the
expression, the computed value must be a pair.
```

## unset-repr!
```
(unset-repr! type)

Removes the string representation of the type, which should be constructor function,
added by add-repr! function.
```

## unset-special!
```
(unset-special! name)

Function that removes a special symbol from parser added by `set-special!`,
name must be a string.
```

## utf8->string
```
(utf8->string u8vector)
(utf8->string u8vector start)
(utf8->string u8vector start end)

Converts u8 bytevector into string using utf8 encoding.
The start and end is the range of the input byte vector for the conversion.
```

## value
```
(value obj)

Function that unwraps LNumbers and converts nil to undefined.
```

## values
```
(values a1 a2 ...)

If called with more then one element it will create a special
Values object that can be used in the call-with-values function.
```

## values-ref
```
(values-ref values n)

Returns n value of values object which is result of value function.
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

Function that copies given range of vector to list. If no start is specified it use
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

Returns new string created from vector of characters in given range.
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

Returns new vector by combining it's arguments that should be vectors.
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

Returns length of the vector. It errors if the argument is not a vector.
```

## vector-map
```
(vector-map fn vector1 vector2 ...)

Returns new vector from applying function fn to each element
of the vectors, similar to map for lists.
```

## vector-ref
```
(vector-ref vec n)

Returns nth element of the vector vec.
```

## vector-set!
```
(vector-set! vec n value)

Function that sets nth item of the vector to value.
```

## vector?
```
(vector? n)

Returns true if value is vector and false if not.
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

## with-input-from-port
```
(with-input-from-port port thunk)

Procedure use port and make it current-input-port then thunk is executed.
After thunk is executed current-input-port is restored and given port
is closed.
```

## with-input-from-string
```
(with-input-from-string string thunk)

Procedure open string and make it current-input-port then thunk is executed.
After thunk is executed current-input-port is restored and string port
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

## zero?
```
(zero? x)

Checks if the number is equal to 0
```

