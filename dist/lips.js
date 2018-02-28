/**@license
 * LIPS is Pretty Simple - version DEV
 *
 * Copyright (c) 2018 Jakub Jankiewicz <http://jcubic.pl/me>
 * Released under the MIT license
 *
 * build: Wed, 28 Feb 2018 21:48:32 +0000
 */
"use strict";
/* global define, module, setTimeout, jQuery */

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

(function (root, factory) {
    if (typeof define === 'function' && define.amd) {
        // AMD. Register as an anonymous module.
        define([], function () {
            return root.lips = factory();
        });
    } else if ((typeof module === 'undefined' ? 'undefined' : _typeof(module)) === 'object' && module.exports) {
        // Node/CommonJS
        module.exports = factory();
    } else {
        root.lips = factory();
    }
})(typeof self !== 'undefined' ? self : undefined, function (undefined) {
    // parse_argument based on function from jQuery Terminal
    var re_re = /^\/((?:\\\/|[^/]|\[[^\]]*\/[^\]]*\])+)\/([gimy]*)$/;
    var float_re = /^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$/;
    // ----------------------------------------------------------------------
    function parse_argument(arg) {
        function parse_string(string) {
            // remove quotes if before are even number of slashes
            // we don't remove slases becuase they are handled by JSON.parse
            //string = string.replace(/([^\\])['"]$/, '$1');
            if (string.match(/^['"]/)) {
                if (string === '""' || string === "''") {
                    return '';
                }
                var quote = string[0];
                var re = new RegExp("((^|[^\\\\])(?:\\\\\\\\)*)" + quote, "g");
                string = string.replace(re, "$1");
            }
            // use build in function to parse rest of escaped characters
            return JSON.parse('"' + string + '"');
        }
        var regex = arg.match(re_re);
        if (regex) {
            return new RegExp(regex[1], regex[2]);
        } else if (arg.match(/['"]/)) {
            return parse_string(arg);
        } else if (arg.match(/^-?[0-9]+$/)) {
            return parseInt(arg, 10);
        } else if (arg.match(float_re)) {
            return parseFloat(arg);
        } else if (arg === 'nil') {
            return nil;
        } else {
            return new _Symbol(arg);
        }
    }
    // ----------------------------------------------------------------------
    /* eslint-disable */
    var tokens_re = /("[^"\\]*(?:\\[\S\s][^"\\]*)*"|\/[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|;.*|\(|\)|'|\.|,@|,|`|[^(\s)]+)/gi;
    /* eslint-enable */
    // ----------------------------------------------------------------------
    function tokenize(str) {
        return str.split('\n').map(function (line) {
            return line.split(tokens_re).map(function (token) {
                if (token.match(/^;/)) {
                    return null;
                }
                return token.trim();
            }).filter(Boolean);
        }).reduce(function (arr, tokens) {
            return arr.concat(tokens);
        }, []);
    }
    // ----------------------------------------------------------------------
    var specials = {
        "'": new _Symbol('quote'),
        '`': new _Symbol('quasiquote'),
        ',': new _Symbol('unquote'),
        ',@': new _Symbol('unquote-splicing')
    };
    // ----------------------------------------------------------------------
    // :: tokens are the array of strings from tokenizer
    // :: the return value is lisp code created out of Pair class
    // ----------------------------------------------------------------------
    function parse(tokens) {
        var stack = [];
        var result = [];
        var special = null;
        var special_tokens = Object.keys(specials);
        var special_forms = special_tokens.map(function (s) {
            return specials[s].name;
        });
        var parents = 0;
        var first_value = false;
        tokens.forEach(function (token) {
            var top = stack[stack.length - 1];
            if (special_tokens.indexOf(token) !== -1) {
                special = token;
            } else if (token === '(') {
                first_value = true;
                parents++;
                if (special) {
                    stack.push([specials[special]]);
                    special = null;
                }
                stack.push([]);
            } else if (token === '.' && !first_value) {
                stack[stack.length - 1] = Pair.fromArray(top);
            } else if (token === ')') {
                parents--;
                if (!stack.length) {
                    throw new Error('Unbalanced parenthesis 1');
                }
                if (stack.length === 1) {
                    result.push(stack.pop());
                } else if (stack.length > 1) {
                    var list = stack.pop();
                    top = stack[stack.length - 1];
                    top.push(list);
                    if (top instanceof Array && top[0] instanceof _Symbol && special_forms.includes(top[0].name) && stack.length > 1) {
                        stack.pop();
                        if (stack[stack.length - 1].length === 0) {
                            stack[stack.length - 1] = top;
                        } else if (stack[stack.length - 1] instanceof Pair) {
                            if (stack[stack.length - 1].cdr instanceof Pair) {
                                stack[stack.length - 1] = new Pair(stack[stack.length - 1], Pair.fromArray(top));
                            } else {
                                stack[stack.length - 1].cdr = Pair.fromArray(top);
                            }
                        } else {
                            stack[stack.length - 1].push(top);
                        }
                    }
                }
                if (parents === 0 && stack.length) {
                    result.push(stack.pop());
                }
            } else {
                first_value = false;
                var value = parse_argument(token);
                if (special) {
                    value = [specials[special], value];
                    special = false;
                }
                if (top instanceof Pair) {
                    var node = top;
                    while (true) {
                        if (node.cdr === nil) {
                            node.cdr = value;
                            break;
                        } else {
                            node = node.cdr;
                        }
                    }
                } else if (!stack.length) {
                    result.push(value);
                } else {
                    top.push(value);
                }
            }
        });
        if (stack.length) {
            throw new Error('Unbalanced parenthesis 2');
        }
        return result.map(function (arg) {
            if (arg instanceof Array) {
                return Pair.fromArray(arg);
            }
            return arg;
        });
    }
    // ----------------------------------------------------------------------
    // :: Symbol constructor
    // ----------------------------------------------------------------------
    function _Symbol(name) {
        this.name = name;
    }
    _Symbol.is = function (symbol, name) {
        return symbol instanceof _Symbol && typeof name === 'string' && symbol.name === name;
    };
    _Symbol.prototype.toJSON = _Symbol.prototype.toString = function () {
        //return '<#symbol \'' + this.name + '\'>';
        return this.name;
    };
    // ----------------------------------------------------------------------
    // :: Pair constructor
    // ----------------------------------------------------------------------
    function Pair(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
    Pair.prototype.length = function () {
        var len = 0;
        var node = this;
        while (true) {
            if (node === nil) {
                break;
            }
            len++;
            node = node.cdr;
        }
        return len;
    };
    Pair.prototype.clone = function () {
        var cdr;
        if (this.cdr === nil) {
            cdr = nil;
        } else {
            cdr = this.cdr.clone();
        }
        return new Pair(this.car, cdr);
    };
    Pair.prototype.toArray = function () {
        if (this.cdr === nil && this.car === nil) {
            return [];
        }
        var result = [];
        if (this.car instanceof Pair) {
            result.push(this.car.toArray());
        } else {
            result.push(this.car);
        }
        if (this.cdr instanceof Pair) {
            result = result.concat(this.cdr.toArray());
        }
        return result;
    };
    Pair.fromArray = function (array) {
        if (array instanceof Pair) {
            return array;
        }
        if (array.length && !array instanceof Array) {
            array = [].concat(_toConsumableArray(array));
        }
        if (array.length === 0) {
            return new Pair(nil, nil);
        } else {
            var car;
            if (array[0] instanceof Array) {
                car = Pair.fromArray(array[0]);
            } else {
                car = array[0];
            }
            if (array.length === 1) {
                return new Pair(car, nil);
            } else {
                return new Pair(car, Pair.fromArray(array.slice(1)));
            }
        }
    };
    Pair.prototype.transform = function (fn) {
        var visited = [];
        function recur(pair) {
            if (pair instanceof Pair) {
                if (pair.replace) {
                    delete pair.replace;
                    return pair;
                }
                var car = fn(pair.car);
                if (car instanceof Pair) {
                    car = recur(car);
                    visited.push(car);
                }
                var cdr = fn(pair.cdr);
                if (cdr instanceof Pair) {
                    cdr = recur(cdr);
                    visited.push(cdr);
                }
                return new Pair(car, cdr);
            }
            return pair;
        }
        return recur(this);
    };
    Pair.prototype.toString = function () {
        var arr = ['('];
        if (typeof this.car === 'string') {
            arr.push(JSON.stringify(this.car));
        } else if (typeof this.car !== 'undefined') {
            arr.push(this.car);
        }
        if (this.cdr instanceof Pair) {
            arr.push(' ');
            arr.push(this.cdr.toString().replace(/^\(|\)$/g, ''));
        } else if (typeof this.cdr !== 'undefined' && this.cdr !== nil) {
            if (typeof this.cdr === 'string') {
                arr = arr.concat([' . ', JSON.stringify(this.cdr)]);
            } else {
                arr = arr.concat([' . ', this.cdr]);
            }
        }
        arr.push(')');
        return arr.join('');
    };
    Pair.prototype.append = function (pair) {
        var p = this;
        while (true) {
            if (p instanceof Pair && p.cdr !== nil) {
                p = p.cdr;
            } else {
                break;
            }
        }
        p.cdr = pair;
        return this;
    };

    // ----------------------------------------------------------------------
    // :: Nil constructor with only once instance
    // ----------------------------------------------------------------------
    function Nil() {}
    Nil.prototype.toString = function () {
        return 'nil';
    };
    var nil = new Nil();

    // ----------------------------------------------------------------------
    // :: Macro constructor
    // ----------------------------------------------------------------------
    function Macro(fn) {
        this.fn = fn;
    }
    Macro.prototype.invoke = function (code, env) {
        return this.fn.call(env, code);
    };

    // ----------------------------------------------------------------------
    // :: Environment constructor (parent argument is optional)
    // ----------------------------------------------------------------------
    function Environment(obj, parent) {
        this.env = obj;
        this.parent = parent;
    }
    Environment.prototype.get = function (symbol) {
        if (symbol instanceof _Symbol) {
            if (typeof this.env[symbol.name] !== 'undefined') {
                return this.env[symbol.name];
            }
        } else if (typeof symbol === 'string') {
            if (typeof this.env[symbol] !== 'undefined') {
                return this.env[symbol];
            }
        }

        if (this.parent instanceof Environment) {
            return this.parent.get(symbol);
        } else if (symbol instanceof _Symbol) {
            if (typeof window[symbol.name] !== 'undefined') {
                return window[symbol.name];
            }
        } else if (typeof symbol === 'string') {
            if (typeof window[symbol] !== 'undefined') {
                return window[symbol];
            }
        }
    };
    Environment.prototype.set = function (name, value) {
        this.env[name] = value;
    };
    // ----------------------------------------------------------------------
    // :: Quote constructor used to pause evaluation from Macro
    // ----------------------------------------------------------------------
    function Quote(value) {
        this.value = value;
    }
    // ----------------------------------------------------------------------
    // :: function that return macro for let and let*
    // ----------------------------------------------------------------------
    function let_macro(asterisk) {
        return new Macro(function (code) {
            var _this = this;

            var args = this.get('list->array')(code.car);
            var env = new Environment({}, this);
            args.forEach(function (pair) {
                env.set(pair.car, evaluate(pair.cdr.car, asterisk ? env : _this));
            });
            var output = new Pair(new _Symbol('begin'), code.cdr);
            return new Quote(evaluate(output, env));
        });
    }
    var gensym = function () {
        var count = 0;
        return function () {
            count++;
            return new _Symbol('#' + count);
        };
    }();
    function request(url) {
        var method = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'GET';
        var headers = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {};
        var data = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;

        var xhr = new XMLHttpRequest();
        xhr.open(method, url, true);
        Object.keys(headers).forEach(function (name) {
            xhr.setRequestHeader(name, headers[name]);
        });
        return new Promise(function (resolve) {
            xhr.onreadystatechange = function () {
                if (xhr.readyState === 4 && xhr.status === 200) {
                    resolve(xhr.responseText);
                }
            };
            if (data !== null) {
                xhr.send(data);
            } else {
                xhr.send();
            }
        });
    }
    var global_env = new Environment({
        nil: nil,
        window: window,
        'true': true,
        'false': false,
        stdout: {
            write: function write() {
                var _console;

                (_console = console).log.apply(_console, arguments);
            }
        },
        stdin: {
            read: function read() {
                return new Promise(function (resolve) {
                    resolve(prompt(''));
                });
            }
        },
        cons: function cons(car, cdr) {
            return new Pair(car, cdr);
        },
        car: function car(list) {
            if (list instanceof Pair) {
                return list.car;
            }
        },
        cdr: function cdr(list) {
            if (list instanceof Pair) {
                return list.cdr;
            }
        },
        'set-car': function setCar(slot, value) {
            slot.car = value;
        },
        'set-cdr': function setCdr(slot, value) {
            slot.cdr = value;
        },
        assoc: function assoc(list, key) {
            var node = list;
            var name = key instanceof _Symbol ? key.name : key;
            while (true) {
                var car = node.car.car;
                if (car instanceof _Symbol && car.name === name || car.name === name) {
                    return node.car;
                } else {
                    node = node.cdr;
                }
            }
        },
        gensym: gensym,
        load: function load(file) {
            var _this2 = this;

            request(file).then(function (code) {
                _this2.get('eval')(_this2.get('read')(code));
            });
        },
        'while': new Macro(function (code) {
            var self = this;
            var begin = new Pair(new _Symbol('begin'), code.cdr);
            return new Promise(function (resolve) {
                var result;
                (function loop() {
                    function next(cond) {
                        if (cond) {
                            var value = evaluate(begin, self);
                            if (value instanceof Promise) {
                                value.then(function (value) {
                                    result = value;
                                    loop();
                                });
                            } else {
                                result = value;
                                loop();
                            }
                        } else {
                            resolve(result);
                        }
                    }
                    var cond = evaluate(code.car, self);
                    if (cond instanceof Promise) {
                        cond.then(next);
                    } else {
                        next(cond);
                    }
                })();
            });
        }),
        'if': new Macro(function (code) {
            var _this3 = this;

            var resolve = function resolve(cond) {
                if (cond) {
                    var true_value = evaluate(code.cdr.car, _this3);
                    if (typeof true_value === 'undefined') {
                        return;
                    }
                    return true_value;
                } else if (code.cdr.cdr.car instanceof Pair) {
                    var false_value = evaluate(code.cdr.cdr.car, _this3);
                    if (typeof false_value === 'undefined') {
                        return false;
                    }
                    return false_value;
                } else {
                    return false;
                }
            };
            var cond = evaluate(code.car, this);
            if (cond instanceof Promise) {
                return cond.then(resolve);
            } else {
                return resolve(cond);
            }
        }),
        'let*': let_macro(true),
        'let': let_macro(false),
        'begin': new Macro(function (code) {
            var _this4 = this;

            var arr = this.get('list->array')(code);
            return arr.reduce(function (_, code) {
                return evaluate(code, _this4);
            }, 0);
        }),
        timer: new Macro(function (code) {
            var _this5 = this;

            return new Promise(function (resolve) {
                setTimeout(function () {
                    resolve(new Quote(evaluate(code.cdr, _this5)));
                }, code.car);
            });
        }),
        define: new Macro(function (code) {
            if (code.car instanceof Pair && code.car.car instanceof _Symbol) {
                var new_code = new Pair(new _Symbol("define"), new Pair(code.car.car, new Pair(new Pair(new _Symbol("lambda"), new Pair(code.car.cdr, code.cdr)))));
                return new_code;
            }
            var value = code.cdr.car;
            if (value instanceof Pair) {
                value = evaluate(value, this);
            }
            if (code.car instanceof _Symbol) {
                this.env[code.car.name] = value;
            }
        }),
        set: function set(obj, key, value) {
            obj[key] = value;
        },
        'eval': function _eval(code) {
            var _this6 = this;

            if (code instanceof Pair) {
                return evaluate(code, this);
            }
            if (code instanceof Array) {
                var result;
                code.forEach(function (code) {
                    result = evaluate(code, _this6);
                });
                return result;
            }
        },
        lambda: new Macro(function (code) {
            var _this7 = this;

            return function () {
                var env = new Environment({}, _this7);
                var name = code.car;
                var i = 0;
                var value;
                while (true) {
                    if (name.car !== nil) {
                        if (typeof (arguments.length <= i ? undefined : arguments[i]) === 'undefined') {
                            value = nil;
                        } else {
                            value = arguments.length <= i ? undefined : arguments[i];
                        }
                        env.env[name.car.name] = value;
                    }
                    if (name.cdr === nil) {
                        break;
                    }
                    i++;
                    name = name.cdr;
                }
                return evaluate(code.cdr.car, env);
            };
        }),
        defmacro: new Macro(function (macro) {
            if (macro.car.car instanceof _Symbol) {
                this.env[macro.car.car.name] = new Macro(function (code) {
                    var env = new Environment({}, this);
                    var name = macro.car.cdr;
                    var arg = code;
                    while (true) {
                        if (name.car !== nil && arg.car !== nil) {
                            env.env[name.car.name] = arg.car;
                        }
                        if (name.cdr === nil) {
                            break;
                        }
                        arg = arg.cdr;
                        name = name.cdr;
                    }
                    return evaluate(macro.cdr.car, env);
                });
            }
        }),
        quote: new Macro(function (arg) {
            return new Quote(arg.car);
        }),
        quasiquote: new Macro(function (arg) {
            var self = this;
            function recur(pair) {
                if (pair instanceof Pair) {
                    var eval_pair;
                    if (_Symbol.is(pair.car.car, 'unquote-splicing')) {
                        eval_pair = evaluate(pair.car.cdr.car, self);
                        if (!eval_pair instanceof Pair) {
                            throw new Error('Value of unquote-splicing need' + ' to be pair');
                        }
                        if (pair.cdr instanceof Pair) {
                            if (eval_pair instanceof Pair) {
                                eval_pair.cdr.append(recur(pair.cdr));
                            } else {
                                eval_pair = new Pair(eval_pair, recur(pair.cdr));
                            }
                        }
                        return eval_pair;
                    }
                    if (_Symbol.is(pair.car, 'unquote-splicing')) {
                        eval_pair = evaluate(pair.cdr.car, self);
                        if (!eval_pair instanceof Pair) {
                            throw new Error('Value of unquote-splicing' + ' need to be pair');
                        }
                        return eval_pair;
                    }
                    if (_Symbol.is(pair.car, 'unquote')) {
                        if (pair.cdr.cdr !== nil) {
                            return new Pair(evaluate(pair.cdr.car, self), pair.cdr.cdr);
                        } else {
                            return evaluate(pair.cdr.car, self);
                        }
                    }
                    var car = pair.car;
                    if (car instanceof Pair) {
                        car = recur(car);
                    }
                    var cdr = pair.cdr;
                    if (cdr instanceof Pair) {
                        cdr = recur(cdr);
                    }
                    return new Pair(car, cdr);
                }
                return pair;
            }
            return new Quote(recur(arg.car));
        }),
        clone: function clone(list) {
            return list.clone();
        },
        append: function append(list, item) {
            return this.get('append!')(list.clone(), item);
        },
        'append!': function append(list, item) {
            var node = list;
            while (true) {
                if (node.cdr === nil) {
                    node.cdr = item;
                    break;
                }
                node = node.cdr;
            }
            return list;
        },
        list: function list() {
            return Pair.fromArray([].slice.call(arguments));
        },
        concat: function concat() {
            return [].join.call(arguments, '');
        },
        string: function string(obj) {
            if (typeof jQuery !== 'undefined' && obj instanceof jQuery.fn.init) {
                return '<#jQuery>';
            }
            if (obj instanceof Macro) {
                //return '<#Macro>';
            }
            if (typeof obj === 'undefined') {
                return '<#undefined>';
            }
            if (typeof obj === 'function') {
                return '<#function>';
            }
            if (obj instanceof Array || obj === null) {
                return JSON.stringify(obj);
            }
            if (obj instanceof Pair) {
                return obj.toString();
            }
            if ((typeof obj === 'undefined' ? 'undefined' : _typeof(obj)) === 'object') {
                var name = obj.constructor.name;
                if (name !== '') {
                    return '<#' + name + '>';
                }
                return '<#Object>';
            }
            if (typeof obj !== 'string') {
                return obj.toString();
            }
            return obj;
        },
        env: function env(_env) {
            _env = _env || this;
            var names = Object.keys(_env.env);
            var result;
            if (names.length) {
                result = Pair.fromArray(names);
            } else {
                result = nil;
            }
            if (_env.parent !== undefined) {
                return this.get('env').call(this, _env.parent).append(result);
            }
            return result;
        },
        '.': function _(obj, arg) {
            var name = arg instanceof _Symbol ? arg.name : arg;
            var value = obj[name];
            if (typeof value === 'function') {
                return value.bind(obj);
            }
            return value;
        },
        read: function read(arg) {
            var _this8 = this;

            if (typeof arg === 'string') {
                return parse(tokenize(arg));
            }
            return this.get('stdin').read().then(function (text) {
                return _this8.get('read').call(_this8, text);
            });
        },
        print: function print() {
            var _get,
                _this9 = this;

            for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
                args[_key] = arguments[_key];
            }

            (_get = this.get('stdout')).write.apply(_get, _toConsumableArray(args.map(function (arg) {
                return _this9.get('string')(arg);
            })));
        },
        'array->list': function arrayList(array) {
            return Pair.fromArray(array);
        },
        'list->array': function listArray(list) {
            var result = [];
            var node = list;
            while (true) {
                if (node instanceof Pair) {
                    result.push(node.car);
                    node = node.cdr;
                } else {
                    break;
                }
            }
            return result;
        },
        filter: function filter(fn, list) {
            return Pair.fromArray(this.get('list->array')(list).filter(fn));
        },
        odd: function odd(num) {
            return num % 2 === 1;
        },
        even: function even(num) {
            return num % 2 === 0;
        },
        apply: function apply(fn, list) {
            var args = this.get('list->array')(list);
            return fn.apply(null, args);
        },
        map: function map(fn, list) {
            var result = this.get('list->array')(list).map(fn);
            if (result.length) {
                return Pair.fromArray(result);
            } else {
                return nil;
            }
        },
        reduce: function reduce(fn, list) {
            var arr = this.get('list->array')(list);
            return arr.reduce(function (list, item) {
                return fn(list, item);
            }, nil);
        },
        // math functions
        '*': function _() {
            for (var _len2 = arguments.length, args = Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
                args[_key2] = arguments[_key2];
            }

            return args.reduce(function (a, b) {
                return a * b;
            });
        },
        '+': function _() {
            for (var _len3 = arguments.length, args = Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
                args[_key3] = arguments[_key3];
            }

            return args.reduce(function (a, b) {
                return a + b;
            });
        },
        '-': function _() {
            for (var _len4 = arguments.length, args = Array(_len4), _key4 = 0; _key4 < _len4; _key4++) {
                args[_key4] = arguments[_key4];
            }

            return args.reduce(function (a, b) {
                return a - b;
            });
        },
        '/': function _() {
            for (var _len5 = arguments.length, args = Array(_len5), _key5 = 0; _key5 < _len5; _key5++) {
                args[_key5] = arguments[_key5];
            }

            return args.reduce(function (a, b) {
                return a / b;
            });
        },
        '%': function _(a, b) {
            return a % b;
        },
        // Booleans
        "==": function _(a, b) {
            return a === b;
        },
        '>': function _(a, b) {
            return a > b;
        },
        '<': function _(a, b) {
            return a < b;
        },
        '<=': function _(a, b) {
            return a <= b;
        },
        '>=': function _(a, b) {
            return a >= b;
        },
        or: new Macro(function (code) {
            var args = this.get('list->array')(code);
            var self = this;
            return new Promise(function (resolve) {
                var result;
                (function loop() {
                    function next(value) {
                        result = value;
                        if (result) {
                            resolve(value);
                        }
                        loop();
                    }
                    var arg = args.shift();
                    if (typeof arg === 'undefined') {
                        if (result) {
                            resolve(result);
                        } else {
                            resolve(false);
                        }
                    } else {
                        var value = evaluate(arg, self);
                        if (value instanceof Promise) {
                            value.then(next);
                        } else {
                            next(value);
                        }
                    }
                })();
            });
        }),
        and: new Macro(function (code) {
            var args = this.get('list->array')(code);
            var self = this;
            return new Promise(function (resolve) {
                var result;
                (function loop() {
                    function next(value) {
                        result = value;
                        if (!result) {
                            resolve(false);
                        }
                        loop();
                    }
                    var arg = args.shift();
                    if (typeof arg === 'undefined') {
                        if (result) {
                            resolve(result);
                        } else {
                            resolve(false);
                        }
                    } else {
                        var value = evaluate(arg, self);
                        if (value instanceof Promise) {
                            value.then(next);
                        } else {
                            next(value);
                        }
                    }
                })();
            });
        }),
        '++': new Macro(function (code) {
            var value = this.get(code.car) + 1;
            this.set(code.car, value);
            return value;
        }),
        '--': new Macro(function (code) {
            var value = this.get(code.car) - 1;
            this.set(code.car, value);
            return value;
        })
    });

    // ----------------------------------------------------------------------
    // source: https://stackoverflow.com/a/4331218/387194
    function allPossibleCases(arr) {
        if (arr.length === 1) {
            return arr[0];
        } else {
            var result = [];
            // recur with the rest of array
            var allCasesOfRest = allPossibleCases(arr.slice(1));
            for (var i = 0; i < allCasesOfRest.length; i++) {
                for (var j = 0; j < arr[0].length; j++) {
                    result.push(arr[0][j] + allCasesOfRest[i]);
                }
            }
            return result;
        }
    }

    // ----------------------------------------------------------------------
    function combinations(input, start, end) {
        var result = [];
        for (var i = start; i <= end; ++i) {
            var input_arr = [];
            for (var j = 0; j < i; ++j) {
                input_arr.push(input);
            }
            result = result.concat(allPossibleCases(input_arr));
        }
        return result;
    }
    // ----------------------------------------------------------------------
    // cadr caddr cadadr etc.
    combinations(['d', 'a'], 2, 5).forEach(function (spec) {
        var chars = spec.split('').reverse();
        global_env.set('c' + spec + 'r', function (arg) {
            return chars.reduce(function (list, type) {
                if (type === 'a') {
                    return list.car;
                } else {
                    return list.cdr;
                }
            }, arg);
        });
    });

    // ----------------------------------------------------------------------
    function evaluate(code, env) {
        env = env || global_env;
        var value;
        if (typeof code === 'undefined') {
            return;
        }
        var first = code.car;
        var rest = code.cdr;
        if (first instanceof Pair) {
            value = evaluate(first, env);
            if (typeof value !== 'function') {
                throw new Error(env.get('string')(value) + ' is not a function');
            }
        }
        if (typeof first === 'function') {
            value = first;
        }
        if (first instanceof _Symbol) {
            value = env.get(first);
            if (value instanceof Macro) {
                value = value.invoke(rest, env);
                if (value instanceof Quote) {
                    return value.value;
                }
                return evaluate(value, env);
            } else if (typeof value !== 'function') {
                throw new Error('Unknown function `' + first.name + '\'');
            }
        }
        if (typeof value === 'function') {
            var args = [];
            var node = rest;
            while (true) {
                if (node instanceof Pair) {
                    args.push(evaluate(node.car, env));
                    node = node.cdr;
                } else {
                    break;
                }
            }
            var promises = args.filter(function (arg) {
                return arg instanceof Promise;
            });
            if (promises.length) {
                return Promise.all(args).then(function (args) {
                    return value.apply(env, args);
                });
            }
            return value.apply(env, args);
        } else if (code instanceof _Symbol) {
            value = env.get(code);
            if (value === 'undefined') {
                throw new Error('Unbound variable `' + code.name + '\'');
            }
            return value;
        } else {
            return code;
        }
    }
    // ----------------------------------------------------------------------

    function balanced(code) {
        var re = /[()]/;
        var parenthesis = tokenize(code).filter(function (token) {
            return token.match(re);
        });
        var open = parenthesis.filter(function (p) {
            return p === ')';
        });
        var close = parenthesis.filter(function (p) {
            return p === '(';
        });
        return open.length === close.length;
    }
    // --------------------------------------
    Pair.unDry = function (value) {
        return new Pair(value.car, value.cdr);
    };
    Pair.prototype.toDry = function () {
        return {
            value: {
                car: this.car,
                cdr: this.cdr
            }
        };
    };
    Nil.prototype.toDry = function () {
        return {
            value: null
        };
    };
    Nil.unDry = function () {
        return nil;
    };
    _Symbol.prototype.toDry = function () {
        return {
            value: {
                name: this.name
            }
        };
    };
    _Symbol.unDry = function (value) {
        return new _Symbol(value.name);
    };
    return {
        version: 'DEV',
        parse: parse,
        tokenize: tokenize,
        evaluate: evaluate,
        Environment: Environment,
        global_environment: global_env,
        balanced_parenthesis: balanced,
        Macro: Macro,
        Quote: Quote,
        Pair: Pair,
        nil: nil,
        Symbol: _Symbol
    };
});

