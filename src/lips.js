/**@license
 * LIPS is Pretty Simple - simple scheme like lisp in JavaScript
 *
 * Copyright (c) 2018-2019 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 *
 */
"use strict";
/* global define, module, setTimeout, jQuery, global, BigInt, require, Blob */
(function(root, factory) {
    if (typeof define === 'function' && define.amd) {
        // AMD. Register as an anonymous module.
        define(['bn.js'], function(BN) {
            return (root.lips = factory(root, BN));
        });
    } else if (typeof module === 'object' && module.exports) {
        // Node/CommonJS
        module.exports = factory(root, require('bn.js'));
    } else {
        root.lips = factory(root, root.BN);
    }
})(typeof window !== 'undefined' ? window : global, function(root, BN, undefined) {
    /* eslint-disable */
    function log(x, regex = null) {
        function msg(x) {
            var value = global_env.get('string')(x);
            if (regex === null || regex.test(value)) {
                console.log(global_env.get('type')(x) + ": " + value);
            }
        }
        if (isPromise(x)) {
            x.then(msg);
        } else {
            msg(x);
        }
        return x;
    }
    if (!root.fetch) {
        root.fetch = function(url, options) {
            options = options || {};
            return new Promise( (resolve, reject) => {
                let request = new XMLHttpRequest();

                request.open(options.method || 'get', url, true);

                for (let i in options.headers) {
                    request.setRequestHeader(i, options.headers[i]);
                }

                request.withCredentials = options.credentials=='include';

                request.onload = () => {
                    resolve(response());
                };

                request.onerror = reject;

                request.send(options.body || null);

                function response() {
                    let keys = [],
                        all = [],
                        headers = {},
                        header;

                    request.getAllResponseHeaders().replace(/^(.*?):[^\S\n]*([\s\S]*?)$/gm, (m, key, value) => {
                        keys.push(key = key.toLowerCase());
                        all.push([key, value]);
                        header = headers[key];
                        headers[key] = header ? `${header},${value}` : value;
                    });

                    return {
                        ok: (request.status/100|0) == 2,    // 200-299
                        status: request.status,
                        statusText: request.statusText,
                        url: request.responseURL,
                        clone: response,
                        text: () => Promise.resolve(request.responseText),
                        json: () => Promise.resolve(request.responseText).then(JSON.parse),
                        blob: () => Promise.resolve(new Blob([request.response])),
                        headers: {
                            keys: () => keys,
                            entries: () => all,
                            get: n => headers[n.toLowerCase()],
                            has: n => n.toLowerCase() in headers
                        }
                    };
                }
            });
        };
    }
    /* eslint-enable */
    // parse_argument based on function from jQuery Terminal
    var re_re = /^\/((?:\\\/|[^/]|\[[^\]]*\/[^\]]*\])+)\/([gimy]*)$/;
    var int_re = /^[-+]?[0-9]+([eE][-+]?[0-9]+)?$/;
    var float_re = /^([-+]?((\.[0-9]+|[0-9]+\.[0-9]+)([eE][-+]?[0-9]+)?)|[0-9]+\.)$/;
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
            return JSON.parse('"' + string.replace(/\n/g, '\\n') + '"');
        }
        var regex = arg.match(re_re);
        if (regex) {
            return new RegExp(regex[1], regex[2]);
        } else if (arg.match(/['"]/)) {
            return parse_string(arg);
        } else if (arg.match(int_re)) {
            return LNumber(parseFloat(arg));
        } else if (arg.match(float_re)) {
            return LNumber(parseFloat(arg), true);
        } else if (arg === 'nil') {
            return nil;
        } else if (arg === 'true') {
            return true;
        } else if (arg === 'false') {
            return false;
        } else {
            return new Symbol(arg);
        }
    }
    // ----------------------------------------------------------------------
    /* eslint-disable */
    var pre_parse_re = /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|;.*)/g;
    var tokens_re = /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|\(|\)|'|"(?:\\[\S\s]|[^"])+|\n|(?:\\[\S\s]|[^"])*"|;.*|(?:[-+]?(?:(?:\.[0-9]+|[0-9]+\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\.)[0-9]|\.{2,}|\.|,@|,|`|[^(\s)]+)/gim;
    /* eslint-enable */
    // ----------------------------------------------------------------------
    function last_item(array) {
        return array[array.length - 1];
    }
    // ----------------------------------------------------------------------
    function tokens(str) {
        str = str.replace(/\n\r|\r/g, '\n');
        var count = 0;
        var line = 0;
        var tokens = [];
        var current_line = [];
        var col = 0;
        str.split(pre_parse_re).filter(Boolean).forEach(function(string) {
            if (string.match(pre_parse_re)) {
                col = 0;
                if (current_line.length) {
                    var last_token = last_item(current_line);
                    if (last_token.token.match(/\n/)) {
                        var last_line = last_token.token.split('\n').pop();
                        col += last_line.length;
                    } else {
                        col += last_token.token.length;
                    }
                    col += last_token.col;
                }
                var token = {
                    col,
                    line,
                    token: string,
                    offset: count
                };
                tokens.push(token);
                current_line.push(token);
                count += string.length;
                col += string.length;
                line += (string.match("\n") || []).length;
                return;
            }
            string.split(tokens_re).filter(Boolean).forEach(function(string) {
                var token = {
                    col,
                    line,
                    token: string,
                    offset: count
                };
                col += string.length;
                count += string.length;
                tokens.push(token);
                current_line.push(token);
                if (string === '\n') {
                    ++line;
                    current_line = [];
                    col = 0;
                }
            });
        });
        return tokens;
    }
    // ----------------------------------------------------------------------
    function tokenize(str, extra) {
        if (extra) {
            return tokens(str);
        } else {
            return tokens(str).map(function(token) {
                return token.token.trim();
            }).filter(function(token) {
                return token && !token.match(/^;/);
            });
        }
    }
    // ----------------------------------------------------------------------
    var specials = {
        "'": new Symbol('quote'),
        '`': new Symbol('quasiquote'),
        ',': new Symbol('unquote'),
        ',@': new Symbol('unquote-splicing')
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
        var special_forms = special_tokens.map(s => specials[s].name);
        var parents = 0;
        var first_value = false;
        var specials_stack = [];
        var single_list_specials = [];
        var special_count = 0;
        function pop_join() {
            var top = stack[stack.length - 1];
            if (top instanceof Array && top[0] instanceof Symbol &&
                special_forms.includes(top[0].name) &&
                stack.length > 1) {
                stack.pop();
                if (stack[stack.length - 1].length === 1 &&
                    stack[stack.length - 1][0] instanceof Symbol) {
                    stack[stack.length - 1].push(top);
                } else if (stack[stack.length - 1].length === 0) {
                    stack[stack.length - 1] = top;
                } else if (stack[stack.length - 1] instanceof Pair) {
                    if (stack[stack.length - 1].cdr instanceof Pair) {
                        stack[stack.length - 1] = new Pair(
                            stack[stack.length - 1],
                            Pair.fromArray(top)
                        );
                    } else {
                        stack[stack.length - 1].cdr = Pair.fromArray(top);
                    }
                } else {
                    stack[stack.length - 1].push(top);
                }
            }
        }
        tokens.forEach(function(token) {
            var top = stack[stack.length - 1];
            if (special_tokens.indexOf(token) !== -1) {
                special_count++;
                special = token;
                stack.push([specials[special]]);
                if (!special) {
                    single_list_specials = [];
                }
                single_list_specials.push(special);
            } else {
                if (special) {
                    specials_stack.push(single_list_specials);
                    single_list_specials = [];
                }
                if (token === '(') {
                    first_value = true;
                    parents++;
                    stack.push([]);
                    special = null;
                    special_count = 0;
                } else if (token === '.' && !first_value) {
                    stack[stack.length - 1] = Pair.fromArray(top);
                } else if (token === ')') {
                    parents--;
                    if (!stack.length) {
                        throw new Error('Unbalanced parenthesis');
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
                        while (special_count--) {
                            stack[stack.length - 1][1] = value;
                            value = stack.pop();
                        }
                        special_count = 0;
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
            }
        });
        if (stack.length) {
            dump(result);
            throw new Error('Unbalanced parenthesis 2');
        }
        return result.map((arg) => {
            if (arg instanceof Array) {
                return Pair.fromArray(arg);
            }
            return arg;
        });
    }
    // ----------------------------------------------------------------------
    function dump(arr) {
        if (false) {
            console.log(arr.map((arg) => {
                if (arg instanceof Array) {
                    return Pair.fromArray(arg);
                }
                return arg;
            }).toString());
        }
    }
    // ----------------------------------------------------------------------
    // return last S-Expression
    // ----------------------------------------------------------------------
    function previousSexp(tokens) {
        var count = 1;
        var i = tokens.length;
        while (count > 0) {
            var token = tokens[--i];
            if (!token) {
                return;
            }
            if (token.token === '(') {
                count--;
            } else if (token.token === ')') {
                count++;
            }
        }
        return tokens.slice(i);
    }
    // ----------------------------------------------------------------------
    // :: find number of spaces in line
    // ----------------------------------------------------------------------
    function lineIndent(tokens) {
        if (!tokens || !tokens.length) {
            return 0;
        }
        var i = tokens.length;
        if (tokens[i - 1].token === '\n') {
            return 0;
        }
        while (--i) {
            if (tokens[i].token === '\n') {
                var token = (tokens[i + 1] || {}).token;
                if (token) {
                    return token.length;
                }
            }
        }
        return 0;
    }
    // ----------------------------------------------------------------------
    // :: Code formatter class
    // :: based on http://community.schemewiki.org/?scheme-style
    // :: and GNU Emacs scheme mode
    // :: it rely on meta data from tokenizer function
    // ----------------------------------------------------------------------
    function Formatter(code) {
        this._code = code.replace(/\r/g, '');
    }
    // ----------------------------------------------------------------------
    Formatter.defaults = {
        offset: 0,
        indent: 2,
        specials: ['define', 'lambda', 'let', 'let*', 'define-macro']
    };
    // ----------------------------------------------------------------------
    // :: return indent for next line
    // ----------------------------------------------------------------------
    Formatter.prototype._options = function _options(options) {
        var defaults = Formatter.defaults;
        if (typeof options === 'undefined') {
            return Object.assign({}, defaults);
        }
        var specials = options && options.specials || [];
        return Object.assign({}, defaults, options, {
            specials: defaults.specials.concat(specials)
        });
    };
    // ----------------------------------------------------------------------
    Formatter.prototype.indent = function indent(options) {
        var tokens = tokenize(this._code, true);
        return this._indent(tokens, options);
    };
    // ----------------------------------------------------------------------
    Formatter.prototype._indent = function _indent(tokens, options) {
        var settings = this._options(options);
        var spaces = lineIndent(tokens);
        var specials = settings.specials;
        var sexp = previousSexp(tokens);
        if (sexp) {
            if (sexp[0].line > 0) {
                settings.offset = 0;
            }
            if (sexp.length === 1) {
                return settings.offset + sexp[0].col + 1;
            } else if (specials.indexOf(sexp[1].token) !== -1) {
                return settings.offset + sexp[0].col + settings.indent;
            } else if (sexp[0].line < sexp[1].line) {
                return settings.offset + sexp[0].col + 1;
            } else if (sexp.length > 3 && sexp[1].line === sexp[3].line) {
                if (sexp[1].token === '(') {
                    return settings.offset + sexp[1].col;
                }
                return settings.offset + sexp[3].col;
            } else if (sexp[0].line === sexp[1].line) {
                return settings.offset + settings.indent + sexp[0].col;
            } else {
                var next_tokens = sexp.slice(2);
                for (var i = 0; i < next_tokens.length; ++i) {
                    var token = next_tokens[i];
                    if (token.token.trim()) {
                        return token.col;
                    }
                }
            }
        }
        return spaces + settings.indent;
    };
    // ----------------------------------------------------------------------
    Formatter.prototype._spaces = function(i) {
        return new Array(i + 1).join(' ');
    };
    // ----------------------------------------------------------------------
    // :: auto formatting of code, it require to have newlines
    // ----------------------------------------------------------------------
    Formatter.prototype.format = function format(options) {
        // prepare code with single space after newline
        // so we have space token to align
        var code = this._code.replace(/\n\s*/g, '\n ');
        var tokens = tokenize(code, true);
        var settings = this._options(options);
        var indent = 0;
        var offset = 0;
        for (var i = 0; i < tokens.length; ++i) {
            var token = tokens[i];
            if (token.token === '\n') {
                indent = this._indent(tokens.slice(0, i), settings);
                offset += indent;
                if (tokens[i + 1]) {
                    tokens[i + 1].token = this._spaces(indent);
                    // because we have single space as initial indent
                    indent--;
                    offset--;
                    for (var j = i + 2; j < tokens.length; ++j) {
                        tokens[j].offset += offset;
                        tokens[j].col += indent;
                        if (tokens[j].token === '\n') {
                            // ++i is called after the loop
                            i = j - 1;
                            break;
                        }
                    }
                }
            }
        }
        return tokens.map(token => token.token).join('');
    };
    // ----------------------------------------------------------------------
    // :: flatten nested arrays
    // :: source: https://stackoverflow.com/a/27282907/387194
    // ----------------------------------------------------------------------
    function flatten(array, mutable) {
        var toString = Object.prototype.toString;
        var arrayTypeStr = '[object Array]';

        var result = [];
        var nodes = (mutable && array) || array.slice();
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
    // detect if object is ES6 Symbol that work with polyfills
    // ----------------------------------------------------------------------
    function isSymbol(x) {
        return typeof x === 'symbol' ||
            typeof x === 'object' &&
            Object.prototype.toString.call(x) === '[object Symbol]';
    }
    // ----------------------------------------------------------------------
    // :: Symbol constructor
    // ----------------------------------------------------------------------
    function Symbol(name) {
        if (typeof this !== 'undefined' && this.constructor !== Symbol ||
            typeof this === 'undefined') {
            return new Symbol(name);
        }
        this.name = name;
    }
    // ----------------------------------------------------------------------
    Symbol.is = function(symbol, name) {
        return symbol instanceof Symbol &&
            ((typeof name === 'string' && symbol.name === name) ||
             (name instanceof RegExp && name.test(symbol.name)));
    };
    // ----------------------------------------------------------------------
    Symbol.prototype.toJSON = Symbol.prototype.toString = function() {
        //return '<#symbol \'' + this.name + '\'>';
        if (isSymbol(this.name)) {
            return this.name.toString();
        }
        return this.name;
    };
    // ----------------------------------------------------------------------
    // :: Nil constructor with only once instance
    // ----------------------------------------------------------------------
    function Nil() {}
    Nil.prototype.toString = function() {
        return 'nil';
    };
    var nil = new Nil();
    // ----------------------------------------------------------------------
    // :: Pair constructor
    // ----------------------------------------------------------------------
    function Pair(car, cdr) {
        if (typeof this !== 'undefined' && this.constructor !== Pair ||
            typeof this === 'undefined') {
            return new Pair(car, cdr);
        }
        this.car = car;
        this.cdr = cdr;
    }

    // ----------------------------------------------------------------------
    function emptyList() {
        return new Pair(undefined, nil);
    }

    // ----------------------------------------------------------------------
    Pair.prototype.flatten = function() {
        return Pair.fromArray(flatten(this.toArray()));
    };

    // ----------------------------------------------------------------------
    Pair.prototype.length = function() {
        if (isEmptyList(this)) {
            return 0;
        }
        var len = 0;
        var node = this;
        while (true) {
            if (!node || node === nil || !(node instanceof Pair)) {
                break;
            }
            len++;
            node = node.cdr;
        }
        return len;
    };

    // ----------------------------------------------------------------------
    Pair.prototype.clone = function() {
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
    Pair.prototype.toArray = function() {
        if (this.isEmptyList()) {
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
    Pair.prototype.isEmptyList = function() {
        return typeof this.car === 'undefined' && this.cdr === nil;
    };

    // ----------------------------------------------------------------------
    Pair.fromArray = function(array) {
        if (array instanceof Pair) {
            return array;
        }
        if (array.length && !(array instanceof Array)) {
            array = [...array];
        }
        if (array.length === 0) {
            return emptyList();
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
    Pair.prototype.toObject = function() {
        var node = this;
        var result = {};
        while (true) {
            if (node instanceof Pair && node.car instanceof Pair) {
                var pair = node.car;
                var name = pair.car;
                if (name instanceof Symbol) {
                    name = name.name;
                }
                var cdr = pair.cdr;
                if (cdr instanceof Pair) {
                    cdr = cdr.toObject();
                }
                if (cdr instanceof LNumber) {
                    cdr = cdr.valueOf();
                }
                result[name] = cdr;
                node = node.cdr;
            } else {
                break;
            }
        }
        return result;
    };

    // ----------------------------------------------------------------------
    Pair.fromPairs = function(array) {
        return array.reduce((list, pair) => {
            return new Pair(
                new Pair(
                    new Symbol(pair[0]),
                    pair[1]
                ),
                list
            );
        }, nil);
    };

    // ----------------------------------------------------------------------
    Pair.fromObject = function(obj) {
        var array = Object.keys(obj).map((key) => [key, obj[key]]);
        return Pair.fromPairs(array);
    };

    // ----------------------------------------------------------------------
    Pair.prototype.reduce = function(fn) {
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
    Pair.prototype.reverse = function() {
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
    Pair.prototype.transform = function(fn) {
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
    Pair.prototype.map = function(fn) {
        if (typeof this.car !== 'undefined') {
            return new Pair(fn(this.car), isEmptyList(this.cdr) ? nil : this.cdr.map(fn));
        } else {
            return nil;
        }
    };

    // ----------------------------------------------------------------------
    Pair.prototype.toString = function() {
        var arr = ['('];
        if (this.car !== undefined) {
            if (typeof this.car === 'function') {
                arr.push('<#function ' + (this.car.name || 'anonymous') + '>');
            } else if (typeof this.car === 'string') {
                arr.push(JSON.stringify(this.car));
            } else if (this.car instanceof Symbol) {
                arr.push(this.car.toString());
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
    Pair.prototype.append = function(pair) {
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
    function equal(x, y) {
        if (x instanceof LNumber && y instanceof LNumber) {
            return x.cmp(y) === 0;
        } else if (typeof x === 'number' || typeof y === 'number') {
            return LNumber(x).cmp(LNumber(y));
        } else if (x instanceof Symbol && y instanceof Symbol) {
            return x.name === y.name;
        } else {
            return x === y;
        }
    }
    // ----------------------------------------------------------------------
    function isEmptyList(x) {
        return x instanceof Pair && x.isEmptyList() || x === nil;
    }
    // ----------------------------------------------------------------------
    // :: Macro constructor
    // ----------------------------------------------------------------------
    function Macro(name, fn) {
        if (typeof this !== 'undefined' && this.constructor !== Macro ||
            typeof this === 'undefined') {
            return new Macro(name, fn);
        }
        this.name = name;
        this.fn = fn;
    }
    Macro.defmacro = function(name, fn) {
        var macro = new Macro(name, fn);
        macro.defmacro = true;
        return macro;
    };
    Macro.prototype.invoke = function(code, { env, dynamic_scope, error }, macro_expand) {
        var args = {
            dynamic_scope,
            error,
            macro_expand
        };
        var result = this.fn.call(env, code, args, this.name);
        return macro_expand ? quote(result) : result;
    };
    Macro.prototype.toString = function() {
        return '#<Macro ' + this.name + '>';
    };
    var macro = 'define-macro';
    function macro_expand(single) {
        return async function(code, args) {
            var env = args['env'] = this;
            async function traverse(node) {
                if (node instanceof Pair && node.car instanceof Symbol) {
                    try {
                        var value = env.get(node.car);
                        if (value instanceof Macro && value.defmacro) {
                            var result = await value.invoke(node.cdr, args, true);
                            if (result instanceof Pair) {
                                return result;
                            }
                        }
                    } catch (e) {
                        // ignore variables
                    }
                }
                var car = node.car;
                if (car instanceof Pair) {
                    car = await traverse(car);
                }
                var cdr = node.cdr;
                if (cdr instanceof Pair) {
                    cdr = await traverse(cdr);
                }
                var pair = new Pair(car, cdr);
                return pair;
            }
            var new_code = code;
            if (single) {
                return quote((await traverse(code)).car);
            } else {
                while (true) {
                    new_code = await traverse(code);
                    if (code.toString() === new_code.toString()) {
                        break;
                    }
                    code = new_code;
                }
                return quote(new_code.car);
            }
        };
    }
    // ----------------------------------------------------------------------
    // :: check for nullish values
    function is_null(value) {
        return typeof value === 'undefined' || value === nil || value === null;
    }
    // ----------------------------------------------------------------------
    function isNativeFunction(fn) {
        return typeof fn === 'function' &&
            fn.toString().match(/\{\s*\[native code\]\s*\}/);
    }
    // ----------------------------------------------------------------------
    function isPromise(o) {
        return o instanceof Promise ||
            (typeof o !== 'undefined' && typeof o.then === 'function');
    }
    // ----------------------------------------------------------------------
    // :: weak version fn.bind as function - it can be rebinded and
    // :: and applied with different context after bind
    // ----------------------------------------------------------------------
    function weakBind(fn, context, ...args) {
        let binded = function(...moreArgs) {
            const args = [...binded.__bind.args, ...moreArgs];
            return binded.__bind.fn.apply(context, args);
        };
        binded.__bind = {
            args: fn.__bind ? fn.__bind.args.concat(args) : args,
            fn: fn.__bind ? fn.__bind.fn : fn
        };
        binded.apply = function(context, args) {
            return binded.__bind.fn.apply(context, args);
        };
        binded.call = function(context, ...args) {
            return binded.__bind.fn.call(context, ...args);
        };
        binded.bind = function(context, ...moreArgs) {
            return weakBind(binded, context, ...moreArgs);
        };
        return binded;
    }
    // ----------------------------------------------------------------------
    // :: function that return macro for let and let*
    // ----------------------------------------------------------------------
    function let_macro(asterisk) {
        var name = 'let' + (asterisk ? '*' : '');
        return Macro.defmacro(name, function(code, options) {
            var { dynamic_scope, error, macro_expand } = options;
            var args;
            // named let:
            // (let iter ((x 10)) (iter (- x 1))) -> (let* ((iter (lambda (x) ...
            if (code.car instanceof Symbol) {
                if (!(code.cdr.car instanceof Pair)) {
                    throw new Error('let require list of pairs');
                }
                var params = code.cdr.car.map(pair => pair.car);
                args = code.cdr.car.map(pair => pair.cdr.car);
                return Pair.fromArray([
                    Symbol('let*'),
                    [[code.car, Pair(
                        Symbol('lambda'),
                        Pair(params, code.cdr.cdr))]],
                    Pair(code.car, args)
                ]);
            } else if (macro_expand) {
                // Macro.defmacro are special macros that should return lisp code
                // here we use evaluate, so we need to check special flag set by
                // macroexpand to prevent evaluation of code in normal let
                return;
            }
            var self = this;
            args = this.get('list->array')(code.car);
            var env = self.inherit('let');
            var i = 0;
            return (function loop() {
                var pair = args[i++];
                function set(value) {
                    if (isPromise(value)) {
                        return value.then(set);
                    } else if (typeof value === 'undefined') {
                        env.set(pair.car, nil);
                    } else {
                        env.set(pair.car, value);
                    }
                }
                if (dynamic_scope) {
                    dynamic_scope = asterisk ? env : self;
                }
                if (!pair) {
                    var output = new Pair(new Symbol('begin'), code.cdr);
                    return evaluate(output, {
                        env,
                        dynamic_scope,
                        error
                    });
                } else {
                    var value = evaluate(pair.cdr.car, {
                        env: asterisk ? env : self,
                        dynamic_scope,
                        error
                    });
                    var promise = set(value);
                    if (isPromise(promise)) {
                        return promise.then(loop);
                    } else {
                        return loop();
                    }
                }
            })();
        });
    }
    // ----------------------------------------------------------------------
    // :: Number wrapper that handle BigNumbers
    // ----------------------------------------------------------------------
    function LNumber(n, float) {
        if (typeof this !== 'undefined' && this.constructor !== LNumber ||
            typeof this === 'undefined') {
            return new LNumber(n, float === true ? true : undefined);
        }
        if (n instanceof LNumber) {
            return LNumber(n.value, n.float);
        }
        if (!LNumber.isNumber(n)) {
            throw new Error("You can't create LNumber from " + typeof n);
        }
        // prevent infite loop https://github.com/indutny/bn.js/issues/186
        if (n === null) {
            n = 0;
        }
        if (LNumber.isFloat(n)) {
            this.value = n;
        } else if (float) {
            this.value = n;
            this.float = true;
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
    LNumber.isNumber = function(n) {
        return n instanceof LNumber ||
            (!Number.isNaN(n) && LNumber.isNative(n) || LNumber.isBN(n));
    };
    // ----------------------------------------------------------------------
    LNumber.isNative = function(n) {
        return typeof n === 'bigint' || typeof n === 'number';
    };
    // ----------------------------------------------------------------------
    LNumber.isBN = function(n) {
        return typeof BN !== 'undefined' && n instanceof BN;
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.toString = LNumber.prototype.toJSON = function() {
        return this.value.toString();
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.isBigNumber = function() {
        return typeof this.value === 'bigint' ||
            typeof BN !== 'undefined' && !(this.value instanceof BN);
    };
    // ----------------------------------------------------------------------
    ['floor', 'ceil', 'round'].forEach(fn => {
        LNumber.prototype[fn] = function() {
            if (this.float || LNumber.isFloat(this.value)) {
                return LNumber(Math[fn](this.value));
            } else {
                return LNumber(this.value);
            }
        };
    });
    // ----------------------------------------------------------------------
    LNumber.prototype.valueOf = function() {
        if (LNumber.isNative(this.value)) {
            return Number(this.value);
        } else if (LNumber.isBN(this.value)) {
            return this.value.toNumber();
        }
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.coerce = function(n) {
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
        } else if (typeof BN !== 'undefined' && this.value instanceof BN &&
            !value instanceof BN) {
            value = new BN(value);
        }
        return LNumber(value);
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.op = function(op, n) {
        var ops = {
            '*': function(a, b) {
                return a * b;
            },
            '+': function(a, b) {
                return a + b;
            },
            '-': function(a, b) {
                return a - b;
            },
            '/': function(a, b) {
                return a / b;
            },
            '%': function(a, b) {
                return a % b;
            },
            '|': function(a, b) {
                return a | b;
            },
            '&': function(a, b) {
                return a & b;
            },
            '~': function(a) {
                return ~a;
            },
            '>>': function(a, b) {
                return a >> b;
            },
            '<<': function(a, b) {
                return a << b;
            }
        };
        if (LNumber.isFloat(n) || (n instanceof LNumber &&
                                   (LNumber.isFloat(n.value) || n.float)) ||
            (LNumber.isFloat(this.value) || this.float)) {
            var value = n instanceof LNumber ? n.valueOf() : n;
            return LNumber(ops[op](this.valueOf(), value), true);
        }
        n = this.coerce(n);
        if (LNumber.isNative(n.value) && LNumber.isNative(this.value)) {
            return LNumber(ops[op](this.value, n.value));
        }
        if (LNumber.isBN(this.value) && LNumber.isBN(n.value)) {
            var bn_op = {
                '+': 'iadd',
                '-': 'isub',
                '*': 'imul',
                '/': 'idiv',
                '%': 'imod',
                '|': 'ior',
                '&': 'iand',
                '~': 'inot',
                '<<': 'ishrn',
                '>>': 'ishln'
            };
            op = bn_op[op];
            return LNumber(this.value.clone()[op](n, value));
        }
    };
    // ----------------------------------------------------------------------
    var ops = {
        '+': 'add',
        '-': 'sub',
        '*': 'mul',
        '/': 'div',
        '%': 'mod',
        '|': 'or',
        '&': 'and',
        '~': 'neg',
        '<<': 'shl',
        '>>': 'shr'
    };
    Object.keys(ops).forEach(op => {
        LNumber.prototype[ops[op]] = function(n) {
            return this.op(op, n);
        };
    });
    // ----------------------------------------------------------------------
    LNumber.prototype.sqrt = function() {
        var value;
        if (LNumber.isNative(this.value)) {
            value = Math.sqrt(this.value);
        } else if (LNumber.isBN(this.value)) {
            value = this.value.sqrt();
        }
        return new LNumber(value);
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.pow = function(n) {
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
    LNumber.prototype.neg = function() {
        var value = this.value;
        if (LNumber.isNative(value)) {
            value = -value;
        } else if (LNumber.isBN(value)) {
            value = value.neg();
        }
        return new LNumber(value);
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.abs = function() {
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
    LNumber.prototype.isOdd = function() {
        if (LNumber.isNative(this.value)) {
            return this.value % 2 === 1;
        } else if (LNumber.isBN(this.value)) {
            return this.value.isOdd();
        }
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.isEven = function() {
        return !this.isOdd();
    };
    // ----------------------------------------------------------------------
    LNumber.prototype.cmp = function(n) {
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
    Environment.prototype.inherit = function(name, obj = {}) {
        if (typeof name === "object") {
            obj = name;
        }
        if (!name || typeof name === "object") {
            name = 'child of ' + (this.name || 'unknown');
        }
        return new Environment(obj || {}, this, name);
    };
    // ----------------------------------------------------------------------
    Environment.prototype.get = function(symbol) {
        var value;
        var defined = false;
        if (symbol instanceof Symbol) {
            if (symbol.name in this.env) {
                value = this.env[symbol.name];
                defined = true;
            }
        } else if (typeof symbol === 'string') {
            if (typeof this.env[symbol] !== 'undefined') {
                value = this.env[symbol];
                defined = true;
            }
        }
        if (defined) {
            if (LNumber.isNumber(value)) {
                return LNumber(value);
            }
            if (typeof value === 'function') {
                return weakBind(value, this);
            }
            return value;
        }
        if (this.parent instanceof Environment) {
            return this.parent.get(symbol);
        } else {
            var name;
            if (symbol instanceof Symbol) {
                name = symbol.name;
            } else if (typeof symbol === 'string') {
                name = symbol;
            }
            if (name) {
                var type = typeof root[name];
                if (type === 'function') {
                    // this is maily done for console.log
                    if (isNativeFunction(root[name])) {
                        return root[name].bind(root);
                    } else {
                        return root[name];
                    }
                } else if (type !== 'undefined') {
                    return root[name];
                }
            }
        }
        name = (name.name || name).toString();
        throw new Error("Unbound variable `" + name + "'");
    };
    // ----------------------------------------------------------------------
    Environment.prototype.set = function(name, value) {
        if (LNumber.isNumber(value)) {
            value = LNumber(value);
        }
        if (name instanceof Symbol) {
            name = name.name;
        }
        this.env[name] = value;
    };
    // ----------------------------------------------------------------------
    Environment.prototype.has = function(name) {
        return typeof this.env[name] !== 'undefined';
    };
    // ----------------------------------------------------------------------
    Environment.prototype.ref = function(name) {
        var env = this;
        while (true) {
            if (!env) {
                break;
            }
            if (env.has(name)) {
                return env;
            }
            env = env.parent;
        }
    };
    // ----------------------------------------------------------------------
    // :: Quote funtion used to pause evaluation from Macro
    // ----------------------------------------------------------------------
    function quote(value) {
        if (isPromise(value)) {
            return value.then(quote);
        }
        if (value instanceof Pair || value instanceof Symbol) {
            value.data = true;
        }
        return value;
    }
    // ----------------------------------------------------------------------
    // :: Unquote is used for multiple backticks and unquote
    // ----------------------------------------------------------------------
    function Unquote(value, count) {
        this.value = value;
        this.count = count;
    }
    Unquote.prototype.toString = function() {
        return '<#unquote[' + this.count + '] ' + this.value + '>';
    };
    // ----------------------------------------------------------------------
    var gensym = (function() {
        var count = 0;
        return function(name = null) {
            // use ES6 symbol as name for lips symbol (they are unique)
            if (name !== null) {
                return new Symbol(root.Symbol('#' + name));
            }
            count++;
            return new Symbol(root.Symbol('#gensym_' + count));
        };
    })();
    // ----------------------------------------------------------------------
    var global_env = new Environment({
        nil: nil,
        'undefined': undefined,
        'true': true,
        'NaN': NaN,
        'false': false,
        // ------------------------------------------------------------------
        stdout: {
            write: function(...args) {
                console.log(...args);
            }
        },
        // ------------------------------------------------------------------
        stdin: {
            read: function() {
                return new Promise((resolve) => {
                    resolve(prompt(''));
                });
            }
        },
        'this': function() {
            return this;
        },
        /*
        test: function() {
            return Promise.resolve(undefined);
        },*/
        // ------------------------------------------------------------------
        cons: function(car, cdr) {
            if (isEmptyList(cdr)) {
                cdr = nil;
            }
            return new Pair(car, cdr);
        },
        // ------------------------------------------------------------------
        car: function(list) {
            if (list instanceof Pair) {
                return list.car;
            } else {
                throw new Error('argument to car need to be a list');
            }
        },
        // ------------------------------------------------------------------
        cdr: function(list) {
            if (list instanceof Pair) {
                return list.cdr;
            } else {
                throw new Error('argument to cdr need to be a list');
            }
        },
        // ------------------------------------------------------------------
        'set!': new Macro('set!', function(code, { dynamic_scope, error } = {}) {
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            var value = evaluate(code.cdr.car, { env: this, dynamic_scope, error });
            value = maybe_promise(value);
            var ref;
            if (code.car instanceof Pair && Symbol.is(code.car.car, '.')) {
                var second = code.car.cdr.car;
                var thrid = code.car.cdr.cdr.car;
                var object = evaluate(second, { env: this, dynamic_scope, error });
                var key = evaluate(thrid, { env: this, dynamic_scope, error });
                if (isPromise(value)) {
                    return value.then(value => {
                        object[key] = value;
                    });
                } else {
                    object[key] = value;
                    return value;
                }
            }
            if (!(code.car instanceof Symbol)) {
                throw new Error('set! first argument need to be a symbol');
            }
            ref = this.ref(code.car.name);
            if (!ref) {
                ref = this;
            }
            if (isPromise(value)) {
                return value.then(value => ref.set(code.car, value));
            } else {
                ref.set(code.car, value);
            }
        }),
        // ------------------------------------------------------------------
        'set-car!': function(slot, value) {
            slot.car = value;
        },
        // ------------------------------------------------------------------
        'set-cdr!': function(slot, value) {
            slot.cdr = value;
        },
        // ------------------------------------------------------------------
        'empty?': function(x) {
            return typeof x === 'undefined' || isEmptyList(x);
        },
        // ------------------------------------------------------------------
        assoc: function(key, list) {
            if (key instanceof Pair && !(list instanceof Pair)) {
                throw new Error('First argument to assoc new to a key');
            }
            var node = list;
            while (true) {
                if (!(node instanceof Pair) || this.get('empty?')(node)) {
                    break;
                }
                var car = node.car.car;
                if (equal(car, key)) {
                    return node.car;
                } else {
                    node = node.cdr;
                }
            }
            return nil;
        },
        // ------------------------------------------------------------------
        gensym: gensym,
        // ------------------------------------------------------------------
        load: function(file) {
            root.fetch(file).then(res => res.text()).then(code => {
                this.get('eval')(this.get('read')(code));
            });
        },
        // ------------------------------------------------------------------
        'while': new Macro('while', async function(code, { dynamic_scope, error }) {
            var self = this;
            var begin = new Pair(
                new Symbol('begin'),
                code.cdr
            );
            var result;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            while (true) {
                var cond = await evaluate(code.car, {
                    env: self,
                    dynamic_scope,
                    error
                });
                if (cond && !isEmptyList(cond)) {
                    result = await evaluate(begin, {
                        env: self,
                        dynamic_scope,
                        error
                    });
                } else {
                    return result;
                }
            }
        }),
        // ------------------------------------------------------------------
        'if': new Macro('if', function(code, { dynamic_scope, error }) {
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            var env = this;
            var resolve = (cond) => {
                if (typeof cond !== 'boolean') {
                    throw new Error('if: value need to be boolean');
                }
                if (cond) {
                    return evaluate(code.cdr.car, {
                        env,
                        dynamic_scope,
                        error
                    });
                } else {
                    return evaluate(code.cdr.cdr.car, {
                        env,
                        dynamic_scope,
                        error
                    });
                }
            };
            var cond = evaluate(code.car, { env, dynamic_scope, error });
            if (isPromise(cond)) {
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
        'begin': new Macro('begin', function(code, { dynamic_scope, error }) {
            var arr = this.get('list->array')(code);
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            var env = this;
            var result;
            return (function loop() {
                if (arr.length) {
                    var code = arr.shift();
                    result = evaluate(code, { env, dynamic_scope, error });
                    if (isPromise(result)) {
                        return result.then(value => {
                            result = value;
                            return loop();
                        });
                    } else {
                        return loop();
                    }
                } else {
                    return result;
                }
            })();
        }),
        nop: function() {},
        // ------------------------------------------------------------------
        timer: new Macro('timer', function(code, { dynamic_scope, error } = {}) {
            var env = this;
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            return new Promise((resolve) => {
                setTimeout(() => {
                    resolve(evaluate(code.cdr, {
                        env,
                        dynamic_scope,
                        error
                    }));
                }, code.car);
            });
        }),
        // ------------------------------------------------------------------
        define: Macro.defmacro('define', function(code, eval_args) {
            var env = this;
            if (code.car instanceof Pair &&
                code.car.car instanceof Symbol) {
                var new_code = new Pair(
                    new Symbol("define"),
                    new Pair(
                        code.car.car,
                        new Pair(
                            new Pair(
                                new Symbol("lambda"),
                                new Pair(
                                    code.car.cdr,
                                    code.cdr
                                )
                            )
                        )
                    )
                );
                return new_code;
            } else if (eval_args.macro_expand) {
                // prevent evaluation in macroexpand
                return;
            }
            if (eval_args.dynamic_scope) {
                eval_args.dynamic_scope = this;
            }
            eval_args.env = env;
            var value = code.cdr.car;
            if (value instanceof Pair) {
                value = evaluate(value, eval_args);
            } else if (value instanceof Symbol) {
                value = env.get(value);
            }
            if (code.car instanceof Symbol) {
                if (isPromise(value)) {
                    return value.then(value => {
                        env.set(code.car, value);
                    });
                } else {
                    env.set(code.car, value);
                }
            }
        }),
        // ------------------------------------------------------------------
        'set-obj': function(obj, key, value) {
            obj[key] = value;
        },
        // ------------------------------------------------------------------
        'eval': function(code) {
            if (code instanceof Pair) {
                return evaluate(code, {
                    env: this,
                    dynamic_scope: this,
                    error: e => this.get('print')(e.message)
                });
            }
            if (code instanceof Array) {
                var result;
                code.forEach((code) => {
                    result = evaluate(code, {
                        env: this,
                        dynamic_scope: this,
                        error: e => this.get('print')(e.message)
                    });
                });
                return result;
            }
        },
        // ------------------------------------------------------------------
        lambda: new Macro('lambda', function(code, { dynamic_scope, error } = {}) {
            var self = this;
            function lambda(...args) {
                var env = (dynamic_scope ? this : self).inherit('lambda');
                var name = code.car;
                var i = 0;
                var value;
                if (name instanceof Symbol || !isEmptyList(name)) {
                    while (true) {
                        if (name.car !== nil) {
                            if (name instanceof Symbol) {
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
                if (dynamic_scope) {
                    dynamic_scope = env;
                }
                var output = new Pair(new Symbol('begin'), code.cdr);
                return evaluate(output, { env, dynamic_scope, error });
            }
            var length = code.car instanceof Pair ? code.car.length() : null;
            if (!(code.car instanceof Pair)) {
                return lambda; // variable arguments
            }
            // list of arguments (name don't matter
            var args = new Array(length).fill(0).map((_, i) => 'a' + i).join(',');
            // hack that create function with specific length
            var wrapper = new Function(`f`, `return function(${args}) {
                return f.apply(this, arguments);
            };`);
            return wrapper(lambda);
        }),
        'macroexpand': new Macro('macro-expand', macro_expand()),
        'macroexpand-1': new Macro('macro-expand', macro_expand(true)),
        // ------------------------------------------------------------------
        'define-macro': new Macro(macro, function(macro, { dynamic_scope, error }) {
            function clear(node) {
                if (node instanceof Pair) {
                    delete node.data;
                }
                return node;
            }
            if (macro.car instanceof Pair && macro.car.car instanceof Symbol) {
                var name = macro.car.car.name;
                this.env[name] = Macro.defmacro(name, function(code, { macro_expand }) {
                    var env = new Environment({}, this, 'defmacro');
                    var name = macro.car.cdr;
                    var arg = code;
                    while (true) {
                        if (name instanceof Symbol) {
                            env.env[name.name] = arg;
                            break;
                        } else if (name.car !== nil && arg.car !== nil) {
                            env.env[name.car.name] = arg.car;
                        }
                        if (name.cdr === nil) {
                            break;
                        }
                        arg = arg.cdr;
                        name = name.cdr;
                    }
                    if (dynamic_scope) {
                        dynamic_scope = env;
                    }
                    // evaluate macro
                    if (macro.cdr instanceof Pair) {
                        // this eval will return lips code
                        var pair = macro.cdr.reduce(function(result, node) {
                            return evaluate(node, { env, dynamic_scope, error });
                        });
                        if (macro_expand) {
                            return pair;
                        }
                        // second evalute of code that is returned from macro
                        // need different env because we need to call it in scope
                        // were it was called
                        pair = evaluate(pair, { env: this, dynamic_scope, error });
                        if (isPromise(pair)) {
                            return pair.then(clear);
                        }
                        return clear(pair);
                    }
                });
            }
        }),
        // ------------------------------------------------------------------
        quote: new Macro('quote', function(arg) {
            return quote(arg.car);
        }),
        // ------------------------------------------------------------------
        quasiquote: new Macro('quasiquote', function(arg, { dynamic_scope, error }) {
            var self = this;
            var max_unquote = 0;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            async function recur(pair) {
                if (pair instanceof Pair) {
                    var eval_pair;
                    if (Symbol.is(pair.car.car, 'unquote-splicing')) {
                        eval_pair = await evaluate(pair.car.cdr.car, {
                            env: self,
                            dynamic_scope,
                            error
                        });
                        if (!eval_pair instanceof Pair) {
                            throw new Error('Value of unquote-splicing need' +
                                            ' to be pair');
                        }
                        if (pair.cdr instanceof Pair) {
                            if (eval_pair instanceof Pair) {
                                eval_pair.cdr.append(await recur(pair.cdr));
                            } else {
                                eval_pair = new Pair(
                                    eval_pair,
                                    await recur(pair.cdr)
                                );
                            }
                        }
                        return eval_pair;
                    }
                    if (Symbol.is(pair.car, 'unquote')) {
                        var head = pair.cdr;
                        var node = head;
                        var parent = node;
                        var unquote_count = 1;
                        while (Symbol.is(node.car.car, 'unquote')) {
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
                                return new Pair(
                                    new Unquote(pair.cdr.car, unquote_count),
                                    await recur(pair.cdr.cdr)
                                );
                            } else {
                                return new Unquote(pair.cdr.car, unquote_count);
                            }
                        } else if (parent.cdr.cdr !== nil) {
                            parent.car.cdr = new Pair(
                                new Unquote(node, unquote_count),
                                parent.cdr === nil ? nil : await recur(parent.cdr.cdr)
                            );
                        } else {
                            parent.car.cdr = new Unquote(node, unquote_count);
                        }
                        return head.car;
                    }
                    var car = pair.car;
                    var cdr = pair.cdr;
                    if (car instanceof Pair) {
                        car = await recur(car);
                    }
                    if (cdr instanceof Pair) {
                        cdr = await recur(cdr);
                    }
                    return new Pair(car, cdr);
                }
                return pair;
            }
            async function unquoting(pair) {
                if (pair instanceof Unquote) {
                    if (max_unquote === pair.count) {
                        return evaluate(pair.value, { env: self, dynamic_scope, error });
                    } else {
                        return new Pair(
                            new Symbol('unquote'),
                            new Pair(
                                await unquoting(pair.value),
                                nil
                            )
                        );
                    }
                }
                if (pair instanceof Pair) {
                    var car = pair.car;
                    if (car instanceof Pair || car instanceof Unquote) {
                        car = await unquoting(car);
                    }
                    var cdr = pair.cdr;
                    if (cdr instanceof Pair || cdr instanceof Unquote) {
                        cdr = await unquoting(cdr);
                    }
                    return new Pair(car, cdr);
                }
                return pair;
            }
            return recur(arg.car).then(unquoting).then(quote);
        }),
        // ------------------------------------------------------------------
        clone: function(list) {
            return list.clone();
        },
        // ------------------------------------------------------------------
        append: function(list, item) {
            return list.clone().append([item]);
        },
        // ------------------------------------------------------------------
        reverse: function(arg) {
            if (arg instanceof Pair) {
                var arr = this.get('list->array')(arg).reverse();
                return this.get('array->list')(arr);
            } else if (!(arg instanceof Array)) {
                throw new Error('Invlid value for reverse');
            } else {
                return arg.reverse();
            }
        },
        // ------------------------------------------------------------------
        nth: function(index, obj) {
            if (obj instanceof Pair) {
                var node = obj;
                var count = 0;
                while (count < index) {
                    if (!node.cdr || node.cdr === nil) {
                        return nil;
                    }
                    node = node.cdr;
                    count++;
                }
                return node.car;
            } else if (obj instanceof Array) {
                return obj[index];
            } else {
                throw new Error('Invalid object for nth');
            }
        },
        // ------------------------------------------------------------------
        'append!': new Macro('append!', function(code, { dynamic_scope, error }) {
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            var value = evaluate(code.cdr.car, { env: this, dynamic_scope, error });
            return this.get(code.car).append([value]);
        }),
        // ------------------------------------------------------------------
        list: function() {
            return Pair.fromArray([].slice.call(arguments));
        },
        concat: function() {
            return [].join.call(arguments, '');
        },
        // ------------------------------------------------------------------
        join: function(separator, list) {
            return this.get('list->array')(list).join(separator);
        },
        // ------------------------------------------------------------------
        split: function(string, separator) {
            return this.get('array->list')(string.split(separator));
        },
        // ------------------------------------------------------------------
        replace: function(string, pattern, replacement) {
            return string.replace(pattern, replacement);
        },
        // ------------------------------------------------------------------
        match: function(string, pattern) {
            return this.get('array->list')(string.match(pattern));
        },
        // ------------------------------------------------------------------
        search: function(string, pattern) {
            return string.search(pattern);
        },
        // ------------------------------------------------------------------
        string: function string(obj, quote) {
            if (typeof jQuery !== 'undefined' &&
                obj instanceof jQuery.fn.init) {
                return '<#jQuery(' + obj.length + ')>';
            }
            if (obj instanceof LNumber) {
                return obj.value.toString();
            }
            if (typeof obj === 'undefined') {
                return '<#undefined>';
            }
            if (typeof obj === 'function') {
                if (isNativeFunction(obj)) {
                    return '<#function(native)>';
                }
                return '<#function>';
            }
            if (obj === nil) {
                return 'nil';
            }
            if (obj instanceof Array) {
                return '[' + obj.map(x => string(x, true)).join(', ') + ']';
            }
            if (obj === null || (typeof obj === 'string' && quote)) {
                return JSON.stringify(obj);
            }
            if (obj instanceof Pair || obj instanceof Symbol) {
                return obj.toString();
            }
            if (typeof obj === 'object') {
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
        env: function(env) {
            env = env || this;
            var names = Object.keys(env.env);
            var result;
            if (names.length) {
                result = Pair.fromArray(names);
            } else {
                result = nil;
            }
            if (env.parent !== undefined) {
                return this.get('env').call(this, env.parent).append(result);
            }
            return result;
        },
        'new': function(obj, ...args) {
            return new obj(...args);
        },
        // ------------------------------------------------------------------
        '.': function(obj, ...args) {
            for (let arg of args) {
                var name = arg instanceof Symbol ? arg.name : arg;
                var value = obj[name];
                if (typeof value === 'function') {
                    value = value.bind(obj);
                }
                obj = value;
            }
            return value;
        },
        // ------------------------------------------------------------------
        type: function(obj) {
            var mapping = {
                'pair': Pair,
                'symbol': Symbol
            };
            for (let [key, value] of Object.entries(mapping)) {
                if (obj instanceof value) {
                    return key;
                }
            }
            if (obj instanceof LNumber) {
                if (obj.isBigNumber()) {
                    return 'LNumber(bigint)';
                }
                return 'number';
            }
            return typeof obj;
        },
        // ------------------------------------------------------------------
        'instanceof': function(obj, type) {
            return obj instanceof type;
        },
        // ------------------------------------------------------------------
        'number?': LNumber.isNumber,
        // ------------------------------------------------------------------
        'string?': function(obj) {
            return typeof obj === 'string';
        },
        // ------------------------------------------------------------------
        'pair?': function(obj) {
            return obj instanceof Pair;
        },
        // ------------------------------------------------------------------
        'regex?': function(obj) {
            return obj instanceof RegExp;
        },
        // ------------------------------------------------------------------
        'null?': function(obj) {
            return is_null(obj) || (obj instanceof Pair && obj.isEmptyList());
        },
        // ------------------------------------------------------------------
        'boolean?': function(obj) {
            return typeof obj === 'boolean';
        },
        // ------------------------------------------------------------------
        'symbol?': function(obj) {
            return obj instanceof Symbol;
        },
        // ------------------------------------------------------------------
        'array?': function(obj) {
            return obj instanceof Array;
        },
        // ------------------------------------------------------------------
        'object?': function(obj) {
            return obj !== null && typeof obj === 'object' && !(obj instanceof Array);
        },
        // ------------------------------------------------------------------
        read: function read(arg) {
            if (typeof arg === 'string') {
                arg = parse(tokenize(arg));
                if (arg.length) {
                    return arg[arg.length - 1];
                }
                return emptyList();
            }
            return this.get('stdin').read().then((text) => {
                return read.call(this, text);
            });
        },
        // ------------------------------------------------------------------
        print: function(...args) {
            this.get('stdout').write(...args.map((arg) => {
                return this.get('string')(arg);
            }));
        },
        // ------------------------------------------------------------------
        flatten: function(list) {
            return list.flatten();
        },
        // ------------------------------------------------------------------
        'array->list': function(array) {
            return Pair.fromArray(array);
        },
        // ------------------------------------------------------------------
        'list->array': function(list) {
            if (list instanceof Pair && list.isEmptyList()) {
                return [];
            }
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
        apply: new Macro('apply', function(code, { dynamic_scope, error } = {}) {
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            function type_check(fn) {
                if (typeof fn !== 'function') {
                    var message;
                    if (code.car instanceof Symbol) {
                        message = "Variable `" + code.car.name +
                            "' is not a function";
                    } else {
                        message = "Expression `" + code.car.toString() +
                            "' is not a function";
                    }
                    throw new Error(message);
                }
            }
            var invoke = fn => {
                type_check(fn);
                var args = evaluate(code.cdr.car, { env: this, dynamic_scope, error });
                args = this.get('list->array')(args);
                if (args.filter(isPromise).length) {
                    return Promise.all(args).then(args => fn.apply(this, args));
                } else {
                    return fn.apply(this, args);
                }
            };
            var fn = evaluate(code.car, { env: this, dynamic_scope, error });
            if (isPromise(fn)) {
                return fn.then(invoke);
            } else {
                return invoke(fn);
            }
        }),
        // ------------------------------------------------------------------
        'length': function(obj) {
            if (!obj) {
                return LNumber(0);
            }
            if (obj instanceof Pair) {
                return LNumber(obj.length());
            }
            if ("length" in obj) {
                return LNumber(obj.length);
            }
        },
        // ------------------------------------------------------------------
        find: async function(fn, list) {
            var array = this.get('list->array')(list);
            for (var i = 0; i < array.length; ++i) {
                if (await fn(array[i], i)) {
                    return array[i];
                }
            }
        },
        // ------------------------------------------------------------------
        'for-each': async function(fn, ...args) {
            await this.get('map')(fn, ...args);
        },
        // ------------------------------------------------------------------
        map: async function(fn, ...args) {
            var array = args.map(list => this.get('list->array')(list));
            var result = [];
            var i = 0;
            while (i < array[0].length) {
                var item = array.map((_, j) => array[j][i]);
                var value = await fn(...item);
                result.push(value);
                i++;
            }
            return Pair.fromArray(result);
        },
        // ------------------------------------------------------------------
        reduce: async function(fn, list, init = null) {
            var array = this.get('list->array')(list);
            if (list.length === 0) {
                return nil;
            }
            var result = init;
            if (init === null) {
                result = array.unshift();
            }
            var i = 0;
            while (i < array.length) {
                var item = array[i++];
                result = await fn(result, item);
            }
            if (typeof result === 'number') {
                return LNumber(result);
            }
            return result;
        },
        // ------------------------------------------------------------------
        filter: async function(fn, list) {
            var array = this.get('list->array')(list);
            var result = [];
            var i = 0;
            while (i < array.length) {
                var item = array[i++];
                var cond = await fn(item, i);
                if (cond) {
                    result.push(item);
                }
            }
            return Pair.fromArray(result, true);
        },
        // ------------------------------------------------------------------
        range: function(n) {
            if (n instanceof LNumber) {
                n = n.valueOf();
            }
            return Pair.fromArray(new Array(n).fill(0).map((_, i) => LNumber(i)));
        },
        // ------------------------------------------------------------------
        curry: function(fn, ...init_args) {
            var len = fn.length;
            return function() {
                var args = init_args.slice();
                function call(...more_args) {
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
        odd: function(num) {
            return LNumber(num).isOdd();
        },
        // ------------------------------------------------------------------
        even: function(num) {
            return LNumber(num).isEvent();
        },
        // ------------------------------------------------------------------
        // math functions
        '*': function(...args) {
            if (args.length) {
                return args.reduce(function(a, b) {
                    return LNumber(a).mul(b);
                });
            }
        },
        // ------------------------------------------------------------------
        '+': function(...args) {
            if (args.length) {
                return args.reduce(function(a, b) {
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
        '-': function(...args) {
            if (args.length === 1) {
                return LNumber(args[0]).neg();
            }
            if (args.length) {
                return args.reduce(function(a, b) {
                    return LNumber(a).sub(b);
                });
            }
        },
        // ------------------------------------------------------------------
        '/': function(...args) {
            if (args.length) {
                return args.reduce(function(a, b) {
                    return LNumber(a).div(b);
                });
            }
        },
        // ------------------------------------------------------------------
        'abs': function(n) {
            return LNumber(n).abs();
        },
        // ------------------------------------------------------------------
        'sqrt': function(n) {
            if (n instanceof LNumber) {
                return Math.sqrt(n.valueOf());
            }
            return Math.sqrt(n);
        },
        // ------------------------------------------------------------------
        '**': function(a, b) {
            return LNumber(a).pow(b);
        },
        // ------------------------------------------------------------------
        '1+': function(number) {
            return LNumber(number).add(1);
        },
        // ------------------------------------------------------------------
        '1-': function(number) {
            return LNumber(number).sub(1);
        },
        // ------------------------------------------------------------------
        '++': new Macro('++', function(code) {
            var car = this.get(code.car);
            var value = LNumber(car).add(1);
            this.set(code.car, value);
            return value;
        }),
        // ------------------------------------------------------------------
        '--': new Macro('--', function(code) {
            var car = this.get(code.car);
            var value = LNumber(car).sub(1);
            this.set(code.car, value);
            return value;
        }),
        // ------------------------------------------------------------------
        '%': function(a, b) {
            return LNumber(a).mod(b);
        },
        // ------------------------------------------------------------------
        // Booleans
        '==': function(a, b) {
            return LNumber(a).cmp(b) === 0;
        },
        // ------------------------------------------------------------------
        '>': function(a, b) {
            return LNumber(a).cmp(b) === 1;
        },
        // ------------------------------------------------------------------
        '<': function(a, b) {
            return LNumber(a).cmp(b) === -1;
        },
        // ------------------------------------------------------------------
        '<=': function(a, b) {
            return [0, -1].includes(LNumber(a).cmp(b));
        },
        // ------------------------------------------------------------------
        '>=': function(a, b) {
            return [0, 1].includes(LNumber(a).cmp(b));
        },
        // ------------------------------------------------------------------
        'eq?': equal,
        // ------------------------------------------------------------------
        or: new Macro('or', function(code, { dynamic_scope, error }) {
            var args = this.get('list->array')(code);
            var self = this;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            return new Promise(function(resolve) {
                var result;
                (function loop() {
                    function next(value) {
                        result = value;
                        if (result) {
                            resolve(value);
                        } else {
                            loop();
                        }
                    }
                    var arg = args.shift();
                    if (typeof arg === 'undefined') {
                        if (result) {
                            resolve(result);
                        } else {
                            resolve(false);
                        }
                    } else {
                        var value = evaluate(arg, { env: self, dynamic_scope, error });
                        if (isPromise(value)) {
                            value.then(next);
                        } else {
                            next(value);
                        }
                    }
                })();
            });
        }),
        // ------------------------------------------------------------------
        and: new Macro('and', function(code, { dynamic_scope, error } = {}) {
            var args = this.get('list->array')(code);
            var self = this;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            return new Promise(function(resolve) {
                var result;
                (function loop() {
                    function next(value) {
                        result = value;
                        if (!result) {
                            resolve(false);
                        } else {
                            loop();
                        }
                    }
                    var arg = args.shift();
                    if (typeof arg === 'undefined') {
                        if (result) {
                            resolve(result);
                        } else {
                            resolve(false);
                        }
                    } else {
                        var value = evaluate(arg, { env: self, dynamic_scope, error });
                        if (isPromise(value)) {
                            value.then(next);
                        } else {
                            next(value);
                        }
                    }
                })();
            });
        }),
        // bit operations
        '|': function(a, b) {
            return LNumber(a).or(b);
        },
        '&': function(a, b) {
            return LNumber(a).and(b);
        },
        '~': function(a) {
            return LNumber(a).neg();
        },
        '>>': function(a, b) {
            return LNumber(a).shr(b);
        },
        '<<': function(a, b) {
            return LNumber(a).shl(b);
        },
        not: function(value) {
            if (isEmptyList(value)) {
                return true;
            }
            return !value;
        },
        '->': function(obj, name, ...args) {
            return obj[name](...args);
        }
    }, undefined, 'global');
    // ----------------------------------------------------------------------
    ['floor', 'round', 'ceil'].forEach(fn => {
        global_env.set(fn, function(value) {
            if (value instanceof LNumber) {
                return value[fn]();
            }
            throw new Error(`${typeof value} ${value.toString()} is not a number`);
        });
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
    combinations(['d', 'a'], 2, 5).forEach((spec) => {
        var chars = spec.split('').reverse();
        global_env.set('c' + spec + 'r', function(arg) {
            return chars.reduce(function(list, type) {
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
    function type(value) {
        if (typeof value === "string") {
            return "string";
        } else if (value instanceof LNumber) {
            return "number";
        } else if (value instanceof RegExp) {
            return "regex";
        } else if (typeof value === 'boolean') {
            return 'boolean';
        } else {
            return 'unknown type';
        }
    }
    // ----------------------------------------------------------------------
    // :; wrap tree of Promises with single Promise or return argument as is
    // :: if tree have no Promises
    // ----------------------------------------------------------------------
    function maybe_promise(arg) {
        var promises = [];
        traverse(arg);
        if (promises.length) {
            return resolve(arg);
        }
        return arg;
        function traverse(node) {
            if (isPromise(node)) {
                promises.push(node);
            } else if (node instanceof Pair) {
                traverse(node.car);
                traverse(node.cdr);
            } else if (node instanceof Array) {
                node.forEach(traverse);
            }
        }
        async function promise(node) {
            var pair = new Pair(
                await resolve(node.car),
                await resolve(node.cdr)
            );
            if (node.data) {
                pair.data = true;
            }
            return pair;
        }
        function resolve(node) {
            if (node instanceof Array) {
                return Promise.all(node.map(resolve));
            }
            if (node instanceof Pair && promises.length) {
                return promise(node);
            }
            return node;
        }
    }
    // ----------------------------------------------------------------------
    function get_function_args(rest, { env, dynamic_scope, error }) {
        var args = [];
        var node = rest;
        while (true) {
            if (node instanceof Pair) {
                var arg = evaluate(node.car, { env, dynamic_scope, error });
                if (dynamic_scope) {
                    if (isPromise(arg)) {
                        arg = arg.then(arg => {
                            if (typeof arg === 'function') {
                                return arg.bind(dynamic_scope);
                            }
                            return arg;
                        });
                    } else if (typeof arg === 'function') {
                        arg = arg.bind(dynamic_scope);
                    }
                }
                args.push(arg);
                node = node.cdr;
            } else {
                break;
            }
        }
        return maybe_promise(args);
    }
    // ----------------------------------------------------------------------
    function evaluate_macro(macro, code, eval_args) {
        if (code instanceof Pair) {
            code = code.clone();
        }
        var value = macro.invoke(code, eval_args);
        value = maybe_promise(value, true);
        function ret(value) {
            if (value && value.data || !(value instanceof Pair)) {
                return value;
            } else{
                return evaluate(value, eval_args);
            }
        }
        if (isPromise(value)) {
            return value.then(ret);
        }
        return ret(value);
    }
    // ----------------------------------------------------------------------
    function evaluate(code, { env, dynamic_scope, error = () => {} } = {}) {
        try {
            if (dynamic_scope === true) {
                env = dynamic_scope = env || global_env;
            } else if (env === true) {
                env = dynamic_scope = global_env;
            } else {
                env = env || global_env;
            }
            var eval_args = { env, dynamic_scope, error };
            var value;
            if (is_null(code)) {
                return code;
            }
            if (isEmptyList(code)) {
                return emptyList();
            }
            var first = code.car;
            var rest = code.cdr;
            if (first instanceof Pair) {
                value = maybe_promise(evaluate(first, eval_args));
                if (isPromise(value)) {
                    return value.then((value) => {
                        return evaluate(new Pair(value, code.cdr), eval_args);
                    });
                } else if (typeof value !== 'function') {
                    throw new Error(
                        env.get('string')(value) + ' is not a function' +
                            ' while evaluating ' + first.toString()
                    );
                }
            }
            if (first instanceof Symbol) {
                value = env.get(first);
                if (value instanceof Macro) {
                    return evaluate_macro(value, rest, eval_args);
                } else if (typeof value !== 'function') {
                    if (value) {
                        var msg = `${type(value)} \`${value}' is not a function`;
                        throw new Error(msg);
                    }
                    throw new Error(`Unknown function \`${first.name}'`);
                }
            } else if (typeof first === 'function') {
                value = first;
            }
            /*
            function debug(args, flag, scope) {
                if (false) {
                    window.args = window.args || {};
                    if (first instanceof Symbol) {
                        window.args[first.name] = window.args[first.name] || [];
                        window.args[first.name].push(args);
                        if (Symbol.is(first, /fn|\+|user-age/)) {
                            console.log({fn: first.name, flag, args});
                        }
                    }
                }
            }
            */
            if (typeof value === 'function') {
                var args = get_function_args(rest, eval_args);
                if (isPromise(args)) {
                    return args.then((args) => {
                        var scope = dynamic_scope || env;
                        //debug(first, 'async', scope);
                        return quote(maybe_promise(value.apply(scope, args)));
                    });
                }
                //debug(first, 'sync', dynamic_scope || env);
                return quote(maybe_promise(value.apply(dynamic_scope || env, args)));
            } else if (code instanceof Symbol) {
                value = env.get(code);
                if (value === 'undefined') {
                    throw new Error('Unbound variable `' + code.name + '\'');
                }
                return value;
            } else if (code instanceof Pair) {
                value = first && first.toString();
                throw new Error(`${type(first)} ${value} is not a function`);
            } else {
                return code;
            }
        } catch (e) {
            error && error(e, code);
        }
    }

    // ----------------------------------------------------------------------
    async function exec(string, env, dynamic_scope) {
        if (dynamic_scope === true) {
            env = dynamic_scope = env || global_env;
        } else if (env === true) {
            env = dynamic_scope = global_env;
        } else {
            env = env || global_env;
        }
        var list = parse(tokenize(string));
        var results = [];
        while (true) {
            var code = list.shift();
            if (!code) {
                return results;
            } else {
                var result = await evaluate(code, {
                    env,
                    dynamic_scope,
                    error: (e, code) => {
                        e.code = code.toString();
                        throw e;
                    }
                });
                results.push(result);
            }
        }
    }

    // ----------------------------------------------------------------------
    // create token matcher that work with string and object token
    // ----------------------------------------------------------------------
    function matchToken(re) {
        return function(token) {
            if (!token) {
                return false;
            }
            return (token.token || token).match(re);
        };
    }
    var isParen = matchToken(/[()]/);
    // ----------------------------------------------------------------------
    function balanced(code) {
        var tokens = typeof code === 'string' ? tokenize(code) : code;
        var parenthesis = tokens.filter(isParen);
        var open = parenthesis.filter(p => (p.token || p) === ')');
        var close = parenthesis.filter(p => (p.token || p) === '(');
        return open.length === close.length;
    }

    // ----------------------------------------------------------------------
    Pair.unDry = function(value) {
        return new Pair(value.car, value.cdr);
    };
    Pair.prototype.toDry = function() {
        return {
            value: {
                car: this.car,
                cdr: this.cdr
            }
        };
    };
    Nil.prototype.toDry = function() {
        return {
            value: null
        };
    };
    Nil.unDry = function() {
        return nil;
    };
    Symbol.prototype.toDry = function() {
        return {
            value: {
                name: this.name
            }
        };
    };
    Symbol.unDry = function(value) {
        return new Symbol(value.name);
    };
    // ----------------------------------------------------------------------
    function init() {
        var lips_mime = 'text/x-lips';
        if (window.document) {
            Array.from(document.querySelectorAll('script')).forEach((script) => {
                var type = script.getAttribute('type');
                if (type === lips_mime) {
                    var src = script.getAttribute('src');
                    if (src) {
                        root.fetch(src).then(res => res.text()).then(exec);
                    } else {
                        exec(script.innerHTML);
                    }
                } else if (type && type.match(/lips|lisp/)) {
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
                (function(old) {
                    window.onload = function() {
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
    load(function() {
        setTimeout(init, 0);
    });
    // --------------------------------------
    return {
        version: '{{VER}}',
        exec,
        parse,
        tokenize,
        evaluate,
        Environment,
        global_environment: global_env,
        env: global_env,
        balanced_parenthesis: balanced,
        Macro,
        quote,
        Pair,
        Formatter,
        nil,
        maybe_promise,
        Symbol,
        LNumber
    };
});
