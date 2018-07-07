/**@license
 * LIPS is Pretty Simple - version 0.5.0
 *
 * Copyright (c) 2018 Jakub Jankiewicz <http://jcubic.pl/me>
 * Released under the MIT license
 *
 * build: Sat, 07 Jul 2018 14:24:14 +0000
 */
"use strict";
/* global define, module, setTimeout, jQuery, global, BigInt, require */

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

(function (root, factory) {
    if (typeof define === 'function' && define.amd) {
        // AMD. Register as an anonymous module.
        define(['bn.js'], function (BN) {
            return root.lips = factory(root, BN);
        });
    } else if ((typeof module === 'undefined' ? 'undefined' : _typeof(module)) === 'object' && module.exports) {
        // Node/CommonJS
        module.exports = factory(root, require('bn.js'));
    } else {
        root.lips = factory(root, root.BN);
    }
})(typeof window !== 'undefined' ? window : global, function (root, BN, undefined) {
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
            return LNumber(parseInt(arg, 10));
        } else if (arg.match(float_re)) {
            return LNumber(parseFloat(arg));
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
    function tokenize(str, extra) {
        if (extra) {
            return tokens(str);
        } else {
            return tokens(str).map(function (token) {
                return token.token.trim();
            }).filter(function (token) {
                return token && !token.match(/^;/);
            });
        }
    }
    // ----------------------------------------------------------------------
    function tokens(str) {
        var count = 0;
        return str.split('\n').map(function (line, i) {
            var col = 0;
            // correction for newline characters
            count += i === 0 ? 0 : 1;
            return line.split(tokens_re).filter(Boolean).map(function (token) {
                var result = {
                    col: col,
                    line: i,
                    token: token,
                    offset: count
                };
                col += token.length;
                count += token.length;
                return result;
            });
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
        var specials_stack = [];
        var single_list_specials = [];
        function pop_join() {
            var top = stack[stack.length - 1];
            if (top instanceof Array && top[0] instanceof _Symbol && special_forms.includes(top[0].name) && stack.length > 1) {
                stack.pop();
                if (stack[stack.length - 1].length === 1 && stack[stack.length - 1][0] instanceof _Symbol) {
                    stack[stack.length - 1].push(top);
                } else if (stack[stack.length - 1].length === 0) {
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
        tokens.forEach(function (token) {
            var top = stack[stack.length - 1];
            if (special_tokens.indexOf(token) !== -1) {
                special = token;
                stack.push([specials[special]]);
                if (!special) {
                    single_list_specials = [];
                }
                single_list_specials.push(special);
            } else if (token === '(') {
                first_value = true;
                parents++;
                if (special) {
                    specials_stack.push(single_list_specials);
                    single_list_specials = [];
                }
                stack.push([]);
                special = null;
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
                    if (top instanceof Array) {
                        top.push(list);
                    } else if (top instanceof Pair) {
                        top.append(Pair.fromArray(list));
                    }
                    if (specials_stack.length) {
                        single_list_specials = specials_stack.pop();
                        while (single_list_specials.length) {
                            pop_join();
                            single_list_specials.pop();
                        }
                    } else {
                        pop_join();
                    }
                }
                if (parents === 0 && stack.length) {
                    result.push(stack.pop());
                }
            } else {
                first_value = false;
                var value = parse_argument(token);
                if (special) {
                    // special without list like ,foo
                    stack[stack.length - 1][1] = value;
                    value = stack.pop();
                    special = false;
                }
                top = stack[stack.length - 1];
                if (top instanceof Pair) {
                    var node = top;
                    while (true) {
                        if (node.cdr === nil) {
                            if (value instanceof Array) {
                                node.cdr = Pair.fromArray(value);
                            } else {
                                node.cdr = value;
                            }
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
            console.log({ end: stack.slice() });
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
    // :: flatten nested arrays
    // :: source: https://stackoverflow.com/a/27282907/387194
    // ----------------------------------------------------------------------
    function flatten(array, mutable) {
        var toString = Object.prototype.toString;
        var arrayTypeStr = '[object Array]';

        var result = [];
        var nodes = mutable && array || array.slice();
        var node;

        if (!array.length) {
            return result;
        }

        node = nodes.pop();

        do {
            if (toString.call(node) === arrayTypeStr) {
                nodes.push.apply(nodes, node);
            } else {
                result.push(node);
            }
        } while (nodes.length && (node = nodes.pop()) !== undefined);

        result.reverse(); // we reverse result to restore the original order
        return result;
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
    // :: Nil constructor with only once instance
    // ----------------------------------------------------------------------
    function Nil() {}
    Nil.prototype.toString = function () {
        return 'nil';
    };
    var nil = new Nil();
    // ----------------------------------------------------------------------
    // :: Pair constructor
    // ----------------------------------------------------------------------
    function Pair(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    // ----------------------------------------------------------------------
    Pair.prototype.flatten = function () {
        return Pair.fromArray(flatten(this.toArray()));
    };

    // ----------------------------------------------------------------------
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

    // ----------------------------------------------------------------------
    Pair.prototype.clone = function () {
        var cdr = this.cdr;
        var car = this.car;
        if (car instanceof Pair) {
            car = car.clone();
        }
        if (cdr instanceof Pair) {
            cdr = this.cdr.clone();
        }
        return new Pair(car, cdr);
    };

    // ----------------------------------------------------------------------
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

    // ----------------------------------------------------------------------
    Pair.prototype.isEmptyList = function () {
        return typeof this.car === 'undefined' && this.cdr === nil;
    };

    // ----------------------------------------------------------------------
    Pair.fromArray = function (array) {
        if (array instanceof Pair) {
            return array;
        }
        if (array.length && !array instanceof Array) {
            array = [].concat(_toConsumableArray(array));
        }
        if (array.length === 0) {
            return new Pair(undefined, nil);
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

    // ----------------------------------------------------------------------
    Pair.prototype.toObject = function () {
        var node = this;
        var result = {};
        while (true) {
            if (node instanceof Pair && node.car instanceof Pair) {
                var pair = node.car;
                var name = pair.car;
                if (name instanceof _Symbol) {
                    name = name.name;
                }
                result[name] = pair.cdr;
                node = node.cdr;
            } else {
                break;
            }
        }
        return result;
    };

    // ----------------------------------------------------------------------
    Pair.fromPairs = function (array) {
        return array.reduce(function (list, pair) {
            return new Pair(new Pair(new _Symbol(pair[0]), pair[1]), list);
        }, nil);
    };

    // ----------------------------------------------------------------------
    Pair.fromObject = function (obj) {
        var array = Object.keys(obj).map(function (key) {
            return [key, obj[key]];
        });
        return Pair.fromPairs(array);
    };

    // ----------------------------------------------------------------------
    Pair.prototype.reduce = function (fn) {
        var node = this;
        var result = nil;
        while (true) {
            if (node !== nil) {
                result = fn(result, node.car);
                node = node.cdr;
            } else {
                break;
            }
        }
        return result;
    };

    // ----------------------------------------------------------------------
    Pair.prototype.reverse = function () {
        var node = this;
        var prev = nil;
        while (node !== nil) {
            var next = node.cdr;
            node.cdr = prev;
            prev = node;
            node = next;
        }
        return prev;
    };

    // ----------------------------------------------------------------------
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

    // ----------------------------------------------------------------------
    Pair.prototype.toString = function () {
        var arr = ['('];
        if (this.car !== undefined) {
            if (typeof this.car === 'function') {
                arr.push('<#function ' + (this.car.name || 'anonymous') + '>');
            } else if (typeof this.car === 'string') {
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
        }
        arr.push(')');
        return arr.join('');
    };

    // ----------------------------------------------------------------------
    Pair.prototype.append = function (pair) {
        if (pair instanceof Array) {
            return this.append(Pair.fromArray(pair));
        }
        var p = this;
        if (p.car === undefined) {
            if (pair instanceof Pair) {
                this.car = pair.car;
                this.cdr = pair.cdr;
            } else {
                return pair;
            }
        } else {
            while (true) {
                if (p instanceof Pair && p.cdr !== nil) {
                    p = p.cdr;
                } else {
                    break;
                }
            }
            p.cdr = pair;
        }
        return this;
    };

    // ----------------------------------------------------------------------
    // :: Macro constructor
    // ----------------------------------------------------------------------
    function Macro(fn) {
        this.fn = fn;
    }
    Macro.prototype.invoke = function (name, code, env, dynamic_scope) {
        return this.fn.call(env, code, dynamic_scope, name);
    };
    // ----------------------------------------------------------------------
    // :: Number wrapper that handle BigNumbers
    // ----------------------------------------------------------------------
    function LNumber(n) {
        if (n instanceof LNumber) {
            return n;
        }
        if (typeof this !== 'undefined' && this.constructor !== LNumber || typeof this === 'undefined') {
            return new LNumber(n);
        }
        if (!LNumber.isNumber(n)) {
            throw new Error("You can't create LNumber from " + (typeof n === 'undefined' ? 'undefined' : _typeof(n)));
        }
        // prevent infite loop https://github.com/indutny/bn.js/issues/186
        if (n === null) {
            n = 0;
        }
        if (LNumber.isFloat(n)) {
            this.value = n;
        } else if (typeof BigInt !== 'undefined') {
            if (typeof n !== 'bigint') {
                this.value = BigInt(n);
            } else {
                this.value = n;
            }
        } else if (typeof BN !== 'undefined' && !(n instanceof BN)) {
            this.value = new BN(n);
        } else {
            this.value = n;
        }
    }
    // ----------------------------------------------------------------------
    LNumber.isFloat = function isFloat(n) {
        return Number(n) === n && n % 1 !== 0;
    };
    // ----------------------------------------------------------------------
    LNumber.isNumber = function (n) {
        return n instanceof LNumber || LNumber.isNative(n) || LNumber.isBN(n);
    };
    // ----------------------------------------------------------------------
    LNumber.isNative = function (n) {
        return typeof n === 'bigint' || typeof n === 'number';
    };
    // ----------------------------------------------------------------------
    LNumber.isBN = function (n) {
        return typeof BN !== 'undefined' && n instanceof BN;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.toString = function () {
        console.log(this.value);
        return this.value.toString();
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.valueOf = function () {
        if (LNumber.isNative(this.value)) {
            return Number(this.value);
        } else if (LNumber.isBN(this.value)) {
            return this.value.toNumber();
        }
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.coerce = function (n) {
        if (n === null) {
            n = 0;
        }
        var value;
        if (n instanceof LNumber) {
            value = n.value;
        } else {
            value = n;
        }
        if (LNumber.isFloat(value)) {
            // skip
        } else if (typeof this.value === 'bigint' && typeof value !== 'bigint') {
            value = BigInt(value);
        } else if (typeof BN !== 'undefined' && this.value instanceof BN && !value instanceof BN) {
            value = new BN(value);
        }
        return LNumber(value);
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.add = function (n) {
        n = this.coerce(n);
        if (LNumber.isNative(n.value)) {
            n.value = this.value + n.value;
        } else if (LNumber.isBN(this.value)) {
            n.value.iadd(this.value);
        }
        return n;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.sub = function (n) {
        n = this.coerce(n);
        if (LNumber.isNative(n.value)) {
            n.value = this.value - n.value;
        } else if (LNumber.isBN(this.value)) {
            n.value.isub(this.value);
        }
        return n;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.mul = function (n) {
        n = this.coerce(n);
        if (LNumber.isNative(n.value)) {
            n.value = this.value * n.value;
        } else if (LNumber.isBN(this.value)) {
            n.value.imul(this.value);
        }
        return n;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.div = function (n) {
        n = this.coerce(n);
        if (LNumber.isNative(n.value)) {
            n.value = this.value / n.value;
        } else if (LNumber.isBN(this.value)) {
            n.value.idiv(this.value);
        }
        return n;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.mod = function (n) {
        n = this.coerce(n);
        if (LNumber.isNative(n.value)) {
            n.value = this.value % n.value;
        } else if (LNumber.isBN(this.value)) {
            n.value.imod(this.value);
        }
        return n;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.sqrt = function () {
        var value;
        if (LNumber.isNative(this.value)) {
            value = Math.sqrt(this.value);
        } else if (LNumber.isBN(this.value)) {
            value = this.value.sqrt();
        }
        return new LNumber(value);
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.pow = function (n) {
        n = this.coerce(n);
        if (LNumber.isNative(this.value)) {
            try {
                var pow = new Function('a,b', 'return a**b;');
                n.value = pow(this.value, n.value);
            } catch (e) {
                throw new Error("Power operator not supported");
            }
        } else if (LNumber.isBN(this.value)) {
            n.value = this.value.pow(n.value);
        } else {
            n.value = Math.pow(this.value, n.value);
        }
        return n;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.neg = function () {
        var value = this.value;
        if (LNumber.isNative(value)) {
            value = -value;
        } else if (LNumber.isBN(value)) {
            value = value.neg();
        }
        return new LNumber(value);
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.abs = function () {
        var value = this.value;
        if (LNumber.isNative(this.value)) {
            if (value < 0) {
                value = -value;
            }
        } else if (LNumber.isBN(value)) {
            value.iabs();
        }
        return new LNumber(value);
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.isOdd = function () {
        if (LNumber.isNative(this.value)) {
            return this.value % 2 === 1;
        } else if (LNumber.isBN(this.value)) {
            return this.value.isOdd();
        }
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.isEven = function () {
        return !this.isOdd();
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.cmp = function (n) {
        n = this.coerce(n);
        if (LNumber.isNative(this.value)) {
            if (this.value < n.value) {
                return -1;
            } else if (this.value === n.value) {
                return 0;
            } else {
                return 1;
            }
        } else if (LNumber.isBN(this.value)) {
            return this.value.cmp(n.value);
        }
    };
    // ----------------------------------------------------------------------
    // :: Environment constructor (parent and name arguments are optional)
    // ----------------------------------------------------------------------
    function Environment(obj, parent, name) {
        this.env = obj;
        this.parent = parent;
        this.name = name;
    }
    // ----------------------------------------------------------------------
    Environment.prototype.inherit = function (obj, name) {
        if (typeof obj === 'string') {
            name = obj;
            obj = {};
        }
        if (!name) {
            name = 'child of ' + (this.name || 'unknown');
        }
        return new Environment(obj || {}, this, name);
    };
    // ----------------------------------------------------------------------
    Environment.prototype.get = function (symbol) {
        var value;
        if (symbol instanceof _Symbol) {
            if (typeof this.env[symbol.name] !== 'undefined') {
                value = this.env[symbol.name];
            }
        } else if (typeof symbol === 'string') {
            if (typeof this.env[symbol] !== 'undefined') {
                value = this.env[symbol];
            }
        }
        if (typeof value !== 'undefined') {
            if (LNumber.isNumber(value)) {
                return LNumber(value);
            }
            return value;
        }

        if (this.parent instanceof Environment) {
            return this.parent.get(symbol);
        } else {
            var name;
            if (symbol instanceof _Symbol) {
                name = symbol.name;
            } else if (typeof symbol === 'string') {
                name = symbol;
            }
            if (name) {
                var type = _typeof(root[name]);
                if (type === 'function') {
                    return root[name].bind(root);
                } else if (type !== 'undefined') {
                    return root[name];
                }
            }
        }
    };
    // ----------------------------------------------------------------------
    Environment.prototype.set = function (name, value) {
        if (LNumber.isNumber(value)) {
            value = LNumber(value);
        }
        if (name instanceof _Symbol) {
            name = name.name;
        }
        this.env[name] = value;
    };
    // ----------------------------------------------------------------------
    // :: Quote constructor used to pause evaluation from Macro
    // ----------------------------------------------------------------------
    function Quote(value) {
        this.value = value;
    }
    // ----------------------------------------------------------------------
    // :: Unquote is used for multiple backticks and unquote
    // ----------------------------------------------------------------------
    function Unquote(value, count) {
        this.value = value;
        this.count = count;
    }
    Unquote.prototype.toString = function () {
        return '<#unquote[' + this.count + '] ' + this.value + '>';
    };
    // ----------------------------------------------------------------------
    // :: function that return macro for let and let*
    // ----------------------------------------------------------------------
    function let_macro(asterisk) {
        return new Macro(function (code, dynamic_scope) {
            var args = this.get('list->array')(code.car);
            var env = this.inherit('let');
            return new Promise(function (resolve) {
                var promises = [];
                var i = 0;
                (function loop() {
                    var pair = args[i++];
                    function set(value) {
                        if (value instanceof Promise) {
                            promises.push(value);
                            return value.then(set);
                        } else {
                            env.set(pair.car, value);
                        }
                    }
                    if (!pair) {
                        var output = new Pair(new _Symbol('begin'), code.cdr);
                        resolve(new Quote(evaluate(output, env, dynamic_scope ? env : dynamic_scope)));
                    } else {
                        var value = evaluate(pair.cdr.car, asterisk ? env : this, dynamic_scope);
                        var promise = set(value);
                        if (promise instanceof Promise) {
                            promise.then(function () {
                                Promise.all(promises).then(loop);
                            });
                        } else {
                            loop();
                        }
                    }
                })();
            });
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
        'true': true,
        'false': false,
        // ------------------------------------------------------------------
        stdout: {
            write: function write() {
                var _console;

                (_console = console).log.apply(_console, arguments);
            }
        },
        // ------------------------------------------------------------------
        stdin: {
            read: function read() {
                return new Promise(function (resolve) {
                    resolve(prompt(''));
                });
            }
        },
        // ------------------------------------------------------------------
        cons: function cons(car, cdr) {
            return new Pair(car, cdr);
        },
        // ------------------------------------------------------------------
        car: function car(list) {
            if (list instanceof Pair) {
                return list.car;
            } else {
                throw new Error('argument to car need to be a list');
            }
        },
        // ------------------------------------------------------------------
        cdr: function cdr(list) {
            if (list instanceof Pair) {
                return list.cdr;
            } else {
                throw new Error('argument to cdr need to be a list');
            }
        },
        // ------------------------------------------------------------------
        'set-car': function setCar(slot, value) {
            slot.car = value;
        },
        // ------------------------------------------------------------------
        'set-cdr': function setCdr(slot, value) {
            slot.cdr = value;
        },
        // ------------------------------------------------------------------
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
        // ------------------------------------------------------------------
        gensym: gensym,
        // ------------------------------------------------------------------
        load: function load(file) {
            var _this = this;

            request(file).then(function (code) {
                _this.get('eval')(_this.get('read')(code));
            });
        },
        // ------------------------------------------------------------------
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
        // ------------------------------------------------------------------
        'if': new Macro(function (code) {
            var _this2 = this;

            var resolve = function resolve(cond) {
                if (cond) {
                    var true_value = evaluate(code.cdr.car, _this2);
                    if (typeof true_value === 'undefined') {
                        return;
                    }
                    return true_value;
                } else if (code.cdr.cdr.car instanceof Pair) {
                    var false_value = evaluate(code.cdr.cdr.car, _this2);
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
        // ------------------------------------------------------------------
        'let*': let_macro(true),
        // ------------------------------------------------------------------
        'let': let_macro(false),
        // ------------------------------------------------------------------
        'begin': new Macro(function (code, dynamic_scope) {
            var _this3 = this;

            var arr = this.get('list->array')(code);
            return arr.reduce(function (_, code) {
                return evaluate(code, _this3, dynamic_scope);
            }, 0);
        }),
        // ------------------------------------------------------------------
        timer: new Macro(function (code) {
            var _this4 = this;

            return new Promise(function (resolve) {
                setTimeout(function () {
                    resolve(new Quote(evaluate(code.cdr, _this4)));
                }, code.car);
            });
        }),
        // ------------------------------------------------------------------
        define: new Macro(function (code, dynamic_scope) {
            var _this5 = this;

            if (code.car instanceof Pair && code.car.car instanceof _Symbol) {
                var new_code = new Pair(new _Symbol("define"), new Pair(code.car.car, new Pair(new Pair(new _Symbol("lambda"), new Pair(code.car.cdr, code.cdr)))));
                return new_code;
            }
            var value = code.cdr.car;
            if (value instanceof Pair) {
                value = evaluate(value, this, dynamic_scope);
            }
            if (code.car instanceof _Symbol) {
                if (value instanceof Promise) {
                    return value.then(function (value) {
                        _this5.set(code.car, value);
                    });
                } else {
                    this.set(code.car, value);
                }
            }
        }),
        // ------------------------------------------------------------------
        set: function set(obj, key, value) {
            obj[key] = value;
        },
        // ------------------------------------------------------------------
        'eval': function _eval(code, dynamic_scope) {
            var _this6 = this;

            if (code instanceof Pair) {
                return evaluate(code, this, dynamic_scope);
            }
            if (code instanceof Array) {
                var result;
                code.forEach(function (code) {
                    result = evaluate(code, _this6, dynamic_scope);
                });
                return result;
            }
        },
        // ------------------------------------------------------------------
        lambda: new Macro(function (code, dynamic_scope) {
            var self = this;
            return function () {
                var env = (dynamic_scope ? this : self).inherit('lambda');
                var name = code.car;
                var i = 0;
                var value;
                if (!name.isEmptyList()) {
                    for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
                        args[_key] = arguments[_key];
                    }

                    while (true) {
                        if (name.car !== nil) {
                            if (name instanceof _Symbol) {
                                // rest argument,  can also be first argument
                                value = Pair.fromArray(args.slice(i));
                                env.env[name.name] = value;
                                break;
                            } else {
                                if (typeof args[i] === 'undefined') {
                                    value = nil;
                                } else {
                                    value = args[i];
                                }
                                env.env[name.car.name] = value;
                            }
                        }
                        if (name.cdr === nil) {
                            break;
                        }
                        i++;
                        name = name.cdr;
                    }
                }
                return evaluate(code.cdr.car, env, dynamic_scope ? env : undefined);
            };
        }),
        // ------------------------------------------------------------------
        defmacro: new Macro(function (macro) {
            if (macro.car.car instanceof _Symbol) {
                this.env[macro.car.car.name] = new Macro(function (code) {
                    var env = new Environment({}, this, 'defmacro');
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
        // ------------------------------------------------------------------
        quote: new Macro(function (arg) {
            return new Quote(arg.car);
        }),
        // ------------------------------------------------------------------
        quasiquote: new Macro(function (arg) {
            var self = this;
            var max_unquote = 0;
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
                    if (_Symbol.is(pair.car, 'unquote')) {
                        var head = pair.cdr;
                        var node = head;
                        var parent = node;
                        var unquote_count = 1;
                        while (_Symbol.is(node.car.car, 'unquote')) {
                            parent = node;
                            unquote_count++;
                            node = node.car.cdr.car;
                        }
                        if (unquote_count > max_unquote) {
                            max_unquote = unquote_count;
                        }
                        // we use Unquote to proccess inner most unquote first
                        // in unquote function afer processing whole s-expression
                        if (parent === node) {
                            if (pair.cdr.cdr !== nil) {
                                return new Pair(new Unquote(pair.cdr.car, unquote_count), pair.cdr.cdr);
                            } else {
                                return new Unquote(pair.cdr.car, unquote_count);
                            }
                        } else if (parent.cdr.cdr !== nil) {
                            parent.car.cdr = new Pair(new Unquote(node, unquote_count), parent.cdr === nil ? nil : parent.cdr.cdr);
                        } else {
                            parent.car.cdr = new Unquote(node, unquote_count);
                        }
                        return head.car;
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
            function unquote(pair) {
                if (pair instanceof Unquote) {
                    if (max_unquote === pair.count) {
                        return evaluate(pair.value, self);
                    } else {
                        return new Pair(new _Symbol('unquote'), new Pair(unquote(pair.value), nil));
                    }
                }
                if (pair instanceof Pair) {
                    var car = pair.car;
                    if (car instanceof Pair || car instanceof Unquote) {
                        car = unquote(car);
                    }
                    var cdr = pair.cdr;
                    if (cdr instanceof Pair || cdr instanceof Unquote) {
                        cdr = unquote(cdr);
                    }
                    return new Pair(car, cdr);
                }
                return pair;
            }
            return new Quote(unquote(recur(arg.car)));
        }),
        // ------------------------------------------------------------------
        clone: function clone(list) {
            return list.clone();
        },
        // ------------------------------------------------------------------
        append: function append(list, item) {
            return this.get('append!')(list.clone(), item);
        },
        // ------------------------------------------------------------------
        'append!': function append(list, item) {
            return list.append(item);
        },
        // ------------------------------------------------------------------
        list: function list() {
            return Pair.fromArray([].slice.call(arguments));
        },
        // ------------------------------------------------------------------
        concat: function concat() {
            return [].join.call(arguments, '');
        },
        // ------------------------------------------------------------------
        join: function join(separator, list) {
            return this.get('list->array')(list).join(separator);
        },
        // ------------------------------------------------------------------
        split: function split(string, separator) {
            return this.get('array->list')(string.split(separator));
        },
        // ------------------------------------------------------------------
        replace: function replace(string, pattern, replacement) {
            return string.replace(pattern, replacement);
        },
        // ------------------------------------------------------------------
        match: function match(string, pattern) {
            return this.get('array->list')(string.match(pattern));
        },
        // ------------------------------------------------------------------
        search: function search(string, pattern) {
            return string.search(pattern);
        },
        // ------------------------------------------------------------------
        string: function string(obj) {
            if (typeof jQuery !== 'undefined' && obj instanceof jQuery.fn.init) {
                return '<#jQuery>';
            }
            if (obj instanceof Macro) {
                //return '<#Macro>';
            }
            if (obj instanceof LNumber) {
                return obj.value.toString();
            }
            if (typeof obj === 'undefined') {
                return '<#undefined>';
            }
            if (typeof obj === 'function') {
                return '<#function>';
            }
            if (obj === nil) {
                return 'nil';
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
        // ------------------------------------------------------------------
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
        // ------------------------------------------------------------------
        '.': function _(obj, arg) {
            var name = arg instanceof _Symbol ? arg.name : arg;
            var value = obj[name];
            if (typeof value === 'function') {
                return value.bind(obj);
            }
            return value;
        },
        type: function type(obj) {
            return typeof obj === 'undefined' ? 'undefined' : _typeof(obj);
        },
        'instanceof': function _instanceof(obj, type) {
            return obj instanceof type;
        },
        // ------------------------------------------------------------------
        read: function read(arg) {
            var _this7 = this;

            if (typeof arg === 'string') {
                return parse(tokenize(arg));
            }
            return this.get('stdin').read().then(function (text) {
                return _this7.get('read').call(_this7, text);
            });
        },
        // ------------------------------------------------------------------
        print: function print() {
            var _get,
                _this8 = this;

            for (var _len2 = arguments.length, args = Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
                args[_key2] = arguments[_key2];
            }

            (_get = this.get('stdout')).write.apply(_get, _toConsumableArray(args.map(function (arg) {
                return _this8.get('string')(arg);
            })));
        },
        // ------------------------------------------------------------------
        flatten: function flatten(list) {
            return list.flatten();
        },
        // ------------------------------------------------------------------
        'array->list': function arrayList(array) {
            return Pair.fromArray(array);
        },
        // ------------------------------------------------------------------
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
        // ------------------------------------------------------------------
        filter: function filter(fn, list) {
            return Pair.fromArray(this.get('list->array')(list).filter(fn));
        },
        // ------------------------------------------------------------------
        apply: function apply(fn, list) {
            var args = this.get('list->array')(list);
            return fn.apply(null, args);
        },
        // ------------------------------------------------------------------
        map: function map(fn, list) {
            var result = this.get('list->array')(list).map(fn);
            if (result.length) {
                return Pair.fromArray(result);
            } else {
                return nil;
            }
        },
        // ------------------------------------------------------------------
        reduce: function reduce(fn, list) {
            var init = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;

            var arr = this.get('list->array')(list);
            if (init === null) {
                return arr.slice(1).reduce(function (acc, item) {
                    return fn(acc, item);
                }, arr[0]);
            } else {
                return arr.reduce(function (acc, item) {
                    return fn(acc, item);
                }, init);
            }
        },
        // ------------------------------------------------------------------
        curry: function curry(fn) {
            for (var _len3 = arguments.length, init_args = Array(_len3 > 1 ? _len3 - 1 : 0), _key3 = 1; _key3 < _len3; _key3++) {
                init_args[_key3 - 1] = arguments[_key3];
            }

            var len = fn.length;
            return function () {
                var args = init_args.slice();
                function call() {
                    for (var _len4 = arguments.length, more_args = Array(_len4), _key4 = 0; _key4 < _len4; _key4++) {
                        more_args[_key4] = arguments[_key4];
                    }

                    args = args.concat(more_args);
                    if (args.length >= len) {
                        return fn.apply(this, args);
                    } else {
                        return call;
                    }
                }
                return call.apply(this, arguments);
            };
        },
        // ------------------------------------------------------------------
        odd: function odd(num) {
            return LNumber(num).isOdd();
        },
        // ------------------------------------------------------------------
        even: function even(num) {
            return LNumber(num).isEvent();
        },
        // ------------------------------------------------------------------
        range: function range(n) {
            if (n instanceof LNumber) {
                n = n.valueOf();
            }
            return Pair.fromArray(new Array(n).fill(0).map(function (_, i) {
                return LNumber(i);
            }));
        },
        // ------------------------------------------------------------------
        // math functions
        '*': function _() {
            for (var _len5 = arguments.length, args = Array(_len5), _key5 = 0; _key5 < _len5; _key5++) {
                args[_key5] = arguments[_key5];
            }

            if (args.length) {
                return args.reduce(function (a, b) {
                    return LNumber(a).mul(b);
                });
            }
        },
        // ------------------------------------------------------------------
        '+': function _() {
            for (var _len6 = arguments.length, args = Array(_len6), _key6 = 0; _key6 < _len6; _key6++) {
                args[_key6] = arguments[_key6];
            }

            if (args.length) {
                return args.reduce(function (a, b) {
                    if (LNumber.isNumber(a) && LNumber.isNumber(b)) {
                        return LNumber(a).add(b);
                    } else if (typeof a === 'string') {
                        throw new Error("To concatenate strings use `concat`");
                    }
                    return a + b;
                });
            }
        },
        // ------------------------------------------------------------------
        '-': function _() {
            for (var _len7 = arguments.length, args = Array(_len7), _key7 = 0; _key7 < _len7; _key7++) {
                args[_key7] = arguments[_key7];
            }

            if (args.length === 1) {
                return LNumber(args[0]).neg();
            }
            if (args.length) {
                return args.reduce(function (a, b) {
                    return LNumber(a).sub(b);
                });
            }
        },
        // ------------------------------------------------------------------
        '/': function _() {
            for (var _len8 = arguments.length, args = Array(_len8), _key8 = 0; _key8 < _len8; _key8++) {
                args[_key8] = arguments[_key8];
            }

            if (args.length) {
                return args.reduce(function (a, b) {
                    return LNumber(a).div(b);
                });
            }
        },
        // ------------------------------------------------------------------
        'abs': function abs(n) {
            return LNumber(n).abs();
        },
        // ------------------------------------------------------------------
        'sqrt': function sqrt(n) {
            if (n instanceof LNumber) {
                return Math.sqrt(n.valueOf());
            }
            return Math.sqrt(n);
        },
        // ------------------------------------------------------------------
        '**': function _(a, b) {
            return LNumber(a).pow(b);
        },
        // ------------------------------------------------------------------
        '1+': function _(number) {
            return LNumber(number).add(1);
        },
        // ------------------------------------------------------------------
        '1-': function _(number) {
            return LNumber(number).sub(1);
        },
        // ------------------------------------------------------------------
        '++': new Macro(function (code) {
            var car = this.get(code.car);
            var value = LNumber(car).add(1);
            this.set(code.car, value);
            return value;
        }),
        // ------------------------------------------------------------------
        '--': new Macro(function (code) {
            var car = this.get(code.car);
            var value = LNumber(car).sub(1);
            this.set(code.car, value);
            return value;
        }),
        // ------------------------------------------------------------------
        '%': function _(a, b) {
            return LNumber(a).mod(b);
        },
        // ------------------------------------------------------------------
        // Booleans
        "==": function _(a, b) {
            return LNumber(a).cmp(b) === 0;
        },
        // ------------------------------------------------------------------
        '>': function _(a, b) {
            return LNumber(a).cmp(b) === 1;
        },
        // ------------------------------------------------------------------
        '<': function _(a, b) {
            return LNumber(a).cmp(b) === -1;
        },
        // ------------------------------------------------------------------
        '<=': function _(a, b) {
            return [0, -1].includes(LNumber(a).cmp(b));
        },
        // ------------------------------------------------------------------
        '>=': function _(a, b) {
            [0, 1].includes(LNumber(a).cmp(b));
        },
        // ------------------------------------------------------------------
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
        // ------------------------------------------------------------------
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
        not: function not(value) {
            if (value === nil) {
                return true;
            }
            return !value;
        },
        '->': function _(obj, name) {
            for (var _len9 = arguments.length, args = Array(_len9 > 2 ? _len9 - 2 : 0), _key9 = 2; _key9 < _len9; _key9++) {
                args[_key9 - 2] = arguments[_key9];
            }

            return obj[name].apply(obj, args);
        }
    }, undefined, 'global');

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
    if (typeof global !== 'undefined') {
        global_env.set('global', global);
    } else if (typeof window !== 'undefined') {
        global_env.set('window', window);
    }

    // ----------------------------------------------------------------------
    function evaluate(code, env, dynamic_scope) {
        /*
        if (code instanceof Pair) {
            if (code.car.name) {
                console.log(code.car.name);
            }
            console.log({
                env: env ? env.name : undefined,
                dynamic: dynamic_scope ? dynamic_scope.name : undefined,
                code: code && code.toString()
            });
        }*/
        if (dynamic_scope === true) {
            env = dynamic_scope = env || global_env;
        } else if (env === true) {
            env = dynamic_scope = global_env;
        } else {
            env = env || global_env;
        }
        var value;
        if (typeof code === 'undefined') {
            return;
        }
        var first = code.car;
        var rest = code.cdr;
        if (first instanceof Pair) {
            value = evaluate(first, env, dynamic_scope);
            if (value instanceof Promise) {
                return value.then(function (value) {
                    return evaluate(new Pair(value, code.cdr), env, dynamic_scope);
                });
            } else if (typeof value !== 'function') {
                throw new Error(env.get('string')(value) + ' is not a function');
            }
        }
        if (typeof first === 'function') {
            value = first;
        }
        if (first instanceof _Symbol) {
            value = env.get(first);
            if (value instanceof Macro) {
                value = value.invoke(first, rest, env, dynamic_scope);
                if (value instanceof Quote) {
                    return value.value;
                } else if (value instanceof Promise) {
                    return value.then(function (value) {
                        if (value instanceof Quote) {
                            return value.value;
                        }
                        return evaluate(value, env, dynamic_scope);
                    });
                }
                return evaluate(value, env, dynamic_scope);
            } else if (typeof value !== 'function') {
                throw new Error('Unknown function `' + first.name + '\'');
            }
        }
        if (typeof value === 'function') {
            var args = [];
            var node = rest;
            while (true) {
                if (node instanceof Pair) {
                    args.push(evaluate(node.car, env, dynamic_scope));
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
                    return value.apply(dynamic_scope || env, args);
                });
            }
            return value.apply(dynamic_scope || env, args);
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
    function exec(string, env, dynamic_scope) {
        if (dynamic_scope === true) {
            env = dynamic_scope = env || global_env;
        } else if (env === true) {
            env = dynamic_scope = global_env;
        } else {
            env = env || global_env;
        }
        var list = parse(tokenize(string));
        return new Promise(function (resolve, reject) {
            var results = [];
            (function recur() {
                function next(value) {
                    results.push(value);
                    recur();
                }
                var code = list.shift();
                if (!code) {
                    resolve(results);
                } else {
                    try {
                        var result = evaluate(code, env, dynamic_scope);
                    } catch (e) {
                        return reject(e);
                    }
                    if (result instanceof Promise) {
                        result.then(next).catch(reject);
                    } else {
                        next(result);
                    }
                }
            })();
        });
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

    // ----------------------------------------------------------------------
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
    // ----------------------------------------------------------------------
    function init() {
        var lips_mime = 'text-x/lips';
        if (window.document) {
            Array.from(document.querySelectorAll('script')).forEach(function (script) {
                var type = script.getAttribute('type');
                if (type === lips_mime) {
                    exec(script.innerHTML);
                } else if (type === 'text-x/lisp') {
                    console.warn('Expecting ' + lips_mime + ' found ' + type);
                }
            });
        }
    }
    // ----------------------------------------------------------------------
    function load(callback) {
        if (typeof window !== 'undefined') {
            if (window.addEventListener) {
                window.addEventListener("load", callback, false);
            } else if (window.attachEvent) {
                window.attachEvent("onload", callback);
            } else if (typeof window.onload === 'function') {
                (function (old) {
                    window.onload = function () {
                        callback();
                        old();
                    };
                })(window.onload);
            } else {
                window.onload = callback;
            }
        }
    }
    // ----------------------------------------------------------------------
    load(function () {
        setTimeout(init, 0);
    });
    // --------------------------------------
    return {
        version: '0.5.0',
        exec: exec,
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
        Symbol: _Symbol,
        LNumber: LNumber
    };
});

