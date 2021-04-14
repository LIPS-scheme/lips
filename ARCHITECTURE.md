# LIPS Scheme Architecture

Main interpreter data flow.

[![Scheme interpreter diagram](https://github.com/jcubic/lips/blob/devel/assets/Interpreter.svg?raw=true)](https://github.com/jcubic/lips/blob/devel/assets/Interpreter.svg)

Main public function is `exec` that accept string (code) and Environment instance. If environment is not specified it use main user environment (same as lips.env) which is child of global environment.

`exec` use internal `parse` function that is JavaScript generator that parse single S-Expression and `exec` use `evaluate` to evaluate the expression.
`evaluate` use multiple features like handling of Syntax, Macros and it auto resolve promises. `evaluate` may ot may not return a promise. But `exec` always return a promise, so it's easier to use. You can never know which expression return a promise and which don't. `evaluate` also use `apply` function that was inspired by meta circual evaluator (but it was refactored into this state, it was not like this from beginning).

## UML Diagram of all classes

[![Scheme interpreter UML Diagram](https://github.com/jcubic/lips/blob/devel/assets/classDiagram.svg?raw=true)](https://github.com/jcubic/lips/blob/devel/assets/classDiagram.svgg)
