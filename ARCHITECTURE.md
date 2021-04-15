# LIPS Scheme Architecture

Main interpreter data flow.

[![Scheme interpreter diagram](https://github.com/jcubic/lips/blob/devel/assets/Interpreter.svg?raw=true)](https://github.com/jcubic/lips/blob/devel/assets/Interpreter.svg)

Main public function is `exec` that accept string (code) and Environment instance. If environment is not specified it use main user environment (same as lips.env) which is child of global environment.

`exec` use internal `parse` function that is JavaScript generator that parse single S-Expression and `exec` use `evaluate` to evaluate the expression.
`evaluate` use multiple features like handling of Syntax, Macros and it auto resolve promises. `evaluate` may ot may not return a promise. But `exec` always return a promise, so it's easier to use. You can never know which expression return a promise and which don't. `evaluate` also use `apply` function that was inspired by meta circual evaluator (but it was refactored into this state, it was not like this from beginning).

## UML Diagram of all classes

[![Scheme interpreter UML Diagram](https://github.com/jcubic/lips/blob/devel/assets/classDiagram.svg?raw=true)](https://github.com/jcubic/lips/blob/devel/assets/classDiagram.svgg)

## Lexer

Lexer is created as simple state machine with `Lexer._rules` that specify
all the states. The sate change is simple it can change from null to given
state for a given token (e.g. symbol), remain in same state and move from given state to null. The last change produce new token. Rules are dynamic
the parser can be update by syntax extensions so `Lexer.rules` is a getter
that return proper states with dynamic state rules generate from specials
(syntax extensions).

To speed up generating of rules at parse time, the Lexer use caching, that is invalidated
when specials are modified (new syntax is added or removed).

The most interesting methods of `Lexer` are `peek` and `skip` that
can be used with the parser. It also have methods to be used with
I/O ports that can be used to read string and then continue to parse rest of the content. The reason why Lexer have those methods (like `peek_char`) is
that Lexer is used as the only interface in ports functions:
`peek-char` `read-char` and `read`.


## Parser

## Environment

## Interpreter

## Pair

## Symbols

## Numbers

## Strings

## Characters

## Macros

## Formatter

## InputPort

## OutputPort

## Worker

## Values

## Value

## QuotedPromise

