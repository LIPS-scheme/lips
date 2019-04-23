/**@license
 * LIPS is Pretty Simple - simple scheme like lisp in JavaScript
 *
 * Copyright (c) 2018-2019 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 *
 * includes:
 *
 * unfetch by Jason Miller (@developit) MIT License
 *
 * includes:
 * contentloaded.js
 *
 * Author: Diego Perini (diego.perini at gmail.com)
 * Summary: cross-browser wrapper for DOMContentLoaded
 * Updated: 20101020
 * License: MIT
 * Version: 1.2
 *
 * URL:
 * http://javascript.nwbox.com/ContentLoaded/
 * http://javascript.nwbox.com/ContentLoaded/MIT-LICENSE
 */
/*
 * TODO: consider using exec in env.eval or use different maybe_async code
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
    /* istanbul ignore next */
    function contentLoaded(win, fn) {
        var done = false, top = true,

            doc = win.document,
            root = doc.documentElement,
            modern = doc.addEventListener,

            add = modern ? 'addEventListener' : 'attachEvent',
            rem = modern ? 'removeEventListener' : 'detachEvent',
            pre = modern ? '' : 'on',

            init = function(e) {
                if (e.type == 'readystatechange' && doc.readyState != 'complete') return;
                (e.type == 'load' ? win : doc)[rem](pre + e.type, init, false);
                if (!done && (done = true)) fn.call(win, e.type || e);
            },

            poll = function() {
                try { root.doScroll('left'); } catch(e) { setTimeout(poll, 50); return; }
                init('poll');
            };

        if (doc.readyState == 'complete') fn.call(win, 'lazy');
        else {
            if (!modern && root.doScroll) {
                try { top = !win.frameElement; } catch(e) { }
                if (top) poll();
            }
            doc[add](pre + 'DOMContentLoaded', init, false);
            doc[add](pre + 'readystatechange', init, false);
            win[add](pre + 'load', init, false);
        }
    }
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
    var string_re = /"(?:\\[\S\s]|[^"])*"/g;
    //var tokens_re = /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|\(|\)|'|"(?:\\[\S\s]|[^"])+|\n|(?:\\[\S\s]|[^"])*"|;.*|(?:[-+]?(?:(?:\.[0-9]+|[0-9]+\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\.)[0-9]|\.{2,}|\.|,@|,|#|`|[^(\s)]+)/gim;
    // ----------------------------------------------------------------------
    function makeTokenRe() {
        var tokens = Object.keys(specials).map(escapeRegex).join('|');
        return new RegExp(`("(?:\\\\[\\S\\s]|[^"])*"|\\/(?! )[^\\/\\\\]*(?:\\\\[\\S\\s][^\\/\\\\]*)*\\/[gimy]*(?=\\s|\\(|\\)|$)|\\(|\\)|'|"(?:\\\\[\\S\\s]|[^"])+|\\n|(?:\\\\[\\S\\s]|[^"])*"|;.*|(?:[-+]?(?:(?:\\.[0-9]+|[0-9]+\\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\\.)[0-9]|\\.{2,}|${tokens}|[^(\\s)]+)`, 'gim');
    }
    /* eslint-enable */
    // ----------------------------------------------------------------------
    function lastItem(array, n = 1) {
        return array[array.length - n];
    }
    // ----------------------------------------------------------------------
    function escapeRegex(str) {
        if (typeof str === 'string') {
            var special = /([-\\^$[\]()+{}?*.|])/g;
            return str.replace(special, '\\$1');
        }
    }
    // ----------------------------------------------------------------------
    function tokens(str) {
        var tokens_re = makeTokenRe();
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
                    var lastToken = lastItem(current_line);
                    if (lastToken.token.match(/\n/)) {
                        var last_line = lastToken.token.split('\n').pop();
                        col += last_line.length;
                    } else {
                        col += lastToken.token.length;
                    }
                    col += lastToken.col;
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
        ',@': new Symbol('unquote-splicing'),
        ',': new Symbol('unquote')
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
    function unpromise(value, fn = x => x) {
        if (isPromise(value)) {
            return value.then(fn);
        }
        return fn(value);
    }
    // ----------------------------------------------------------------------
    function matcher(name, arg) {
        if (arg instanceof RegExp) {
            return x => String(x).match(arg);
        } else if (typeof arg !== 'function') {
            throw new Error(`${name} argument need to be a function or RegExp`);
        } else {
            return arg;
        }
    }
    // ----------------------------------------------------------------------
    // :: documentaton decorator to LIPS functions if lines starts with :
    // :: they are ignored (not trim) otherwise it trim so
    // :: so you can have indent in source code
    // ----------------------------------------------------------------------
    function doc(fn, doc, dump) {
        if (doc) {
            if (dump) {
                fn.__doc__ = doc;
            } else {
                fn.__doc__ = trimLines(doc);
            }
        }
        return fn;
    }
    // ----------------------------------------------------------------------
    function trimLines(string) {
        return string.split('\n').map(line => {
            return line.trim();
        }).join('\n');
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
        } else {
            return 0;
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
            if (!node || node === nil || !(node instanceof Pair) ||
                 node.haveCycles('cdr')) {
                break;
            }
            len++;
            node = node.cdr;
        }
        return len;
    };

    // ----------------------------------------------------------------------
    Pair.prototype.clone = function() {
        var visited = new Map();
        function clone(node) {
            if (node instanceof Pair) {
                if (visited.has(node)) {
                    return visited.get(node);
                }
                var pair = new Pair();
                visited.set(node, pair);
                pair.car = clone(node.car);
                pair.cdr = clone(node.cdr);
                return pair;
            }
            return node;
        }
        return clone(this);
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
        if (this.haveCycles()) {
            throw new Error("You can't reverse list that have cycles");
        }
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
    function toString(value) {
        if (typeof value === 'function') {
            return '<#function ' + (value.name || 'anonymous') + '>';
        } else if (typeof value === 'string') {
            return JSON.stringify(value);
        } else if (isPromise(value)) {
            return '<#Promise>';
        } else if (value instanceof Symbol ||
                  value instanceof LNumber ||
                  value instanceof Pair ||
                  value === nil) {
            return value.toString();
        } else if (value instanceof Array) {
            return value.map(toString);
        } else if (typeof value === 'object') {
            if (value === null) {
                return 'null';
            }
            var name = value.constructor.name;
            if (name === 'Object') {
                return JSON.stringify(value);
            }
            return '<#object(' + value.constructor.name + ')>';
        } else if (typeof value !== 'undefined') {
            return value;
        }
    }

    // ----------------------------------------------------------------------------
    Pair.prototype.markCycles = function() {
        markCycles(this);
        return this;
    };

    // ----------------------------------------------------------------------------
    Pair.prototype.haveCycles = function(name = null) {
        if (!name) {
            return this.haveCycles('car') || this.haveCycles('cdr');
        }
        return !!(this.cycles && this.cycles[name]);
    };

    // ----------------------------------------------------------------------------
    function markCycles(pair) {
        var seenPairs = [];
        var cycles = [];
        function cycleName(pair) {
            if (pair instanceof Pair) {
                if (seenPairs.includes(pair)) {
                    if (!cycles.includes(pair)) {
                        cycles.push(pair);
                    }
                    return `#${cycles.length - 1}#`;
                }
            }
        }
        function detect(pair) {
            if (pair instanceof Pair) {
                seenPairs.push(pair);
                var cycles = {};
                var carCycle = cycleName(pair.car);
                var cdrCycle = cycleName(pair.cdr);
                if (carCycle) {
                    cycles['car'] = carCycle;
                } else {
                    detect(pair.car);
                }
                if (cdrCycle) {
                    cycles['cdr'] = cdrCycle;
                } else {
                    detect(pair.cdr);
                }
                if (carCycle || cdrCycle) {
                    pair.cycles = cycles;
                } else if (pair.cycles) {
                    delete pair.cycles;
                }
            }
        }
        detect(pair);
    }

    // ----------------------------------------------------------------------
    Pair.prototype.toString = function(cycle) {
        var arr = ['('];
        if (this.car !== undefined) {
            var value;
            if (this.cycles && this.cycles.car) {
                value = this.cycles.car;
            } else {
                value = toString(this.car);
            }
            if (value) {
                arr.push(value);
            }
            if (this.cdr instanceof Pair) {
                if (this.cycles && this.cycles.cdr) {
                    arr.push(' . ');
                    arr.push(this.cycles.cdr);
                } else {
                    var name;
                    if (this.cycles && this.cycles.cdr) {
                        name = this.cycles.cdr;
                    }
                    var cdr = this.cdr.toString(name).replace(/^\(|\)$/g, '');
                    arr.push(' ');
                    arr.push(cdr);
                }
            } else if (typeof this.cdr !== 'undefined' && this.cdr !== nil) {
                if (typeof this.cdr === 'string') {
                    arr = arr.concat([' . ', JSON.stringify(this.cdr)]);
                } else {
                    arr = arr.concat([' . ', toString(this.cdr)]);
                }
            }
        }
        arr.push(')');
        return arr.join('');
    };

    // ----------------------------------------------------------------------
    Pair.prototype.set = function(prop, value) {
        this[prop] = value;
        if (value instanceof Pair) {
            this.markCycles();
        }
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
                this.car = pair;
            }
        } else {
            while (true) {
                if (p instanceof Pair && p.cdr !== nil) {
                    p = p.cdr;
                } else {
                    break;
                }
            }
            if (pair instanceof Pair) {
                p.cdr = pair;
            } else if (pair !== nil) {
                p.cdr = new Pair(pair, nil);
            }
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
    function Macro(name, fn, doc) {
        if (typeof this !== 'undefined' && this.constructor !== Macro ||
            typeof this === 'undefined') {
            return new Macro(name, fn);
        }
        typecheck('Macro', name, 'string', 1);
        typecheck('Macro', fn, 'function', 2);
        this.__doc__ = doc;
        this.name = name;
        this.fn = fn;
    }
    // ----------------------------------------------------------------------
    Macro.defmacro = function(name, fn, doc) {
        var macro = new Macro(name, fn, doc);
        macro.defmacro = true;
        return macro;
    };
    // ----------------------------------------------------------------------
    Macro.prototype.invoke = function(code, { env, dynamic_scope, error }, macro_expand) {
        var args = {
            dynamic_scope,
            error,
            macro_expand
        };
        var result = this.fn.call(env, code, args, this.name);
        return macro_expand ? quote(result) : result;
    };
    // ----------------------------------------------------------------------
    Macro.prototype.toString = function() {
        return '#<Macro ' + this.name + '>';
    };
    // ----------------------------------------------------------------------
    var macro = 'define-macro';
    // ----------------------------------------------------------------------
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
                // CYCLE DETECT
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
    function isNull(value) {
        return typeof value === 'undefined' || value === nil || value === null;
    }
    // ----------------------------------------------------------------------
    function isNativeFunction(fn) {
        return typeof fn === 'function' &&
            fn.toString().match(/\{\s*\[native code\]\s*\}/) &&
            !fn.name.match(/^bound /);
    }
    // ----------------------------------------------------------------------
    function isPromise(o) {
        return o instanceof Promise ||
            (o && typeof o !== 'undefined' && typeof o.then === 'function');
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
        if (fn.__doc__) {
            binded.__doc__ = fn.__doc__;
        }
        if (fn.__code__) {
            binded.__code__ = fn.__code__;
        }
        binded.__bind = {
            args: fn.__bind ? fn.__bind.args.concat(args) : args,
            fn: fn.__bind ? fn.__bind.fn : fn
        };
        binded.toString = function() {
            return binded.__bind.fn.toString();
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
        return setFnLength(binded, binded.__bind.fn.length);
    }
    // ----------------------------------------------------------------------
    function unbind(obj) {
        if (typeof obj === 'function' && obj.__bind) {
            return obj.__bind.fn;
        }
        return obj;
    }
    // ----------------------------------------------------------------------
    // :: function bind fn with context but it also move all props
    // :: mostly used for Object function
    // ----------------------------------------------------------------------
    function filterFnNames(name) {
        return !['name', 'length'].includes(name);
    }
    // ----------------------------------------------------------------------
    function bindWithProps(fn, context) {
        const bound = fn.bind(context);
        const props = Object.getOwnPropertyNames(fn).filter(filterFnNames);
        props.forEach(prop => {
            bound[prop] = fn[prop];
        });
        return bound;
    }
    // ----------------------------------------------------------------------
    function setFnLength(fn, length) {
        try {
            Object.defineProperty(fn, 'length', {
                get: function() {
                    return length;
                }
            });
            return fn;
        } catch (e) {
            // hack that create function with specific length should work for browsers
            // that don't support Object.defineProperty like old IE
            var args = new Array(length).fill(0).map((_, i) => 'a' + i).join(',');
            var wrapper = new Function(`f`, `return function(${args}) {
                return f.apply(this, arguments);
            };`);
            return wrapper(fn);
        }
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
                    return unpromise(set(value), loop);
                }
            })();
        });
    }
    // ----------------------------------------------------------------------
    function guardMathCall(fn, ...args) {
        args.forEach(arg => {
            typecheck('', arg, 'number');
        });
        return fn(...args);
    }
    // ----------------------------------------------------------------------
    function pipe(...fns) {
        fns.forEach((fn, i) => {
            typecheck('pipe', fn, 'function', i + 1);
        });
        return function(...args) {
            return fns.reduce((args, f) => [f(...args)], args)[0];
        };
    }
    // ----------------------------------------------------------------------
    function compose(...fns) {
        fns.forEach((fn, i) => {
            typecheck('compose', fn, 'function', i + 1);
        });
        return pipe(...fns.reverse());
    }
    // ----------------------------------------------------------------------
    // :: fold functions generator
    // ----------------------------------------------------------------------
    function fold(name, fold) {
        var self = this;
        return function recur(fn, init, ...lists) {
            typecheck(name, fn, 'function');
            if (lists.some(l => isEmptyList(l) || isNull(l))) {
                if (typeof init === 'number') {
                    return LNumber(init);
                }
                return init;
            } else {
                return fold.call(self, recur, fn, init, ...lists);
            }
        };
    }
    // ----------------------------------------------------------------------
    function limitMathOp(n, fn) {
        // + 1 so it inlcude function in guardMathCall
        return limit(n + 1, curry(guardMathCall, fn));
    }
    // ----------------------------------------------------------------------
    var singleMathOp = curry(limitMathOp, 1);
    var binaryMathOp = curry(limitMathOp, 2);
    // ----------------------------------------------------------------------
    function reduceMathOp(fn) {
        return function(...args) {
            if (args.length) {
                return args.reduce(binaryMathOp(fn));
            }
        };
    }
    // ----------------------------------------------------------------------
    function curry(fn, ...init_args) {
        typecheck('curry', fn, 'function');
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
    }
    // ----------------------------------------------------------------------
    // return function with limited number of arguments
    function limit(n, fn) {
        typecheck('limit', fn, 'function', 2);
        return function(...args) {
            return fn(...args.slice(0, n));
        };
    }
    // ----------------------------------------------------------------------------
    var get = doc(function(obj, ...args) {
        if (typeof obj === 'function' && obj.__bind) {
            obj = obj.__bind.fn;
        }
        for (let arg of args) {
            var name = arg instanceof Symbol ? arg.name : arg;
            var value = obj[name];
            if (typeof value === 'function') {
                value = bindWithProps(value, obj);
            }
            obj = value;
        }
        return value;
    }, `(. obj . args)
        (get obj . args)

        Function use object as based and keep using arguments to get the
        property of JavaScript object. Arguments need to be a strings.
        e.g. \`(. console "log")\` if you use any function inside LIPS is
        will be weakly bind (can be rebind), so you can call this log function
        without problem unlike in JavaScript when you use
       \`var log = console.log\`.
       \`get\` is an alias because . don't work in every place, you can't
        pass it as argument`);
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
            throw new Error(`You can't create LNumber from ${type(n)}`);
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
            if (this.isBigNumber()) {
                return this.value % BigInt(2) === BigInt(1);
            }
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
    Environment.prototype.get = function(symbol, weak, context) {
        // we keep original environment as context for bind
        // so print will get user stdout
        context = context || this;
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
            if (value instanceof Pair) {
                return value.markCycles();
            }
            if (typeof value === 'function') {
                // bind only functions that are not binded for case:
                // (let ((x Object)) (. x 'keys))
                // second x access is already bound when accessing Object
                if (!value.name.match(/^bound /)) {
                    if (weak) {
                        return weakBind(value, context);
                    }
                    return value.bind(context);
                }
            }
            return value;
        }
        if (this.parent instanceof Environment) {
            return this.parent.get(symbol, weak, context);
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
                    if (isNativeFunction(root[name])) {
                        // hard bind of native functions with props for Object
                        // hard because of console.log
                        return bindWithProps(root[name], root);
                    }
                    return root[name];
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
        help: doc(function(obj) {
            return obj.__doc__;
        }, `(help object)

            Function returns documentation for function or macro.`),
        // ------------------------------------------------------------------
        cons: doc(function(car, cdr) {
            if (isEmptyList(cdr)) {
                cdr = nil;
            }
            return new Pair(car, cdr);
        }, `(cons left right)

            Function return new Pair out of two arguments.`),
        // ------------------------------------------------------------------
        car: doc(function(list) {
            typecheck('car', list, 'pair');
            return list.car;
        }, `(car pair)

            Function returns car (head) of the list/pair.`),
        // ------------------------------------------------------------------
        cdr: doc(function(list) {
            typecheck('cdr', list, 'pair');
            return list.cdr;
        }, `(cdr pair)

            Function returns cdr (tail) of the list/pair.`),
        // ------------------------------------------------------------------
        'set!': doc(new Macro('set!', function(code, { dynamic_scope, error } = {}) {
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            var value = evaluate(code.cdr.car, { env: this, dynamic_scope, error });
            value = resolvePromises(value);
            var ref;
            function set(key, value) {
                if (isPromise(key)) {
                    return key.then(key => set(key, value));
                }
                if (isPromise(value)) {
                    return value.then(value => set(key, value));
                }
                object[key] = value;
                return value;
            }
            if (code.car instanceof Pair && Symbol.is(code.car.car, '.')) {
                var second = code.car.cdr.car;
                var thrid = code.car.cdr.cdr.car;
                var object = evaluate(second, { env: this, dynamic_scope, error });
                var key = evaluate(thrid, { env: this, dynamic_scope, error });
                return set(key, value);
            }
            if (!(code.car instanceof Symbol)) {
                throw new Error('set! first argument need to be a symbol or ' +
                                'dot accessor that evaluate to object.');
            }
            ref = this.ref(code.car.name);
            if (!ref) {
                ref = this;
            }
            // we don't return value because we only care about sync of set value
            // when value is a promise
            return unpromise(value, value => {
                ref.set(code.car, value);
            });
        }), `(set! name value)

            Macro that can be used to set the value of the variable (mutate)
            it search the scope chain until it finds first non emtpy slot and set it.`),
        // ------------------------------------------------------------------
        'set-car!': doc(function(slot, value) {
            typecheck('set-car!', slot, 'pair');
            slot.car = value;
        }, `(set-car! obj value)

            Function that set car (head) of the list/pair to specified value.
            It can destroy the list. Old value is lost.`),
        // ------------------------------------------------------------------
        'set-cdr!': doc(function(slot, value) {
            typecheck('set-cdr!', slot, 'pair');
            slot.cdr = value;
        }, `(set-cdr! obj value)

            Function that set cdr (tail) of the list/pair to specified value.
            It can destroy the list. Old value is lost.`),
        // ------------------------------------------------------------------
        'empty?': doc(function(x) {
            return typeof x === 'undefined' || isEmptyList(x);
        }, `(empty? object)

            Function return true if value is undfined empty list.`),
        // ------------------------------------------------------------------
        assoc: doc(function(key, list) {
            if (key instanceof Pair && !(list instanceof Pair)) {
                throw new Error('First argument to assoc ned to be a key');
            }
            var node = list;
            while (true) {
                if (!(node instanceof Pair) || this.get('empty?')(node)) {
                    break;
                }
                var car = node.car.car;
                if (equal(car, key)) {
                    return node.car;
                } else if (!node.haveCycles('cdr')) {
                    node = node.cdr;
                }
            }
            return nil;
        }, `(assoc key alist)

            Function search Alist (list of pairs) until it find the one that
            have head set equal to key, and return found pair.`),
        // ------------------------------------------------------------------
        gensym: doc(
            gensym,
            `(gensym)

             Function generate unique symbol, to use with macros as meta name.`),
        // ------------------------------------------------------------------
        load: doc(function(file) {
            typecheck('load', file, 'string');
            return root.fetch(file).then(res => res.text()).then(exec);
        }, `(load filename)

            Function fetch the file and evaluate its content as LIPS code.`),
        // ------------------------------------------------------------------
        'while': doc(new Macro('while', function(code, { dynamic_scope, error }) {
            var self = this;
            var begin = new Pair(
                new Symbol('begin'),
                code.cdr
            );
            var result;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            return (function loop() {
                var cond = evaluate(code.car, {
                    env: self,
                    dynamic_scope,
                    error
                });
                function next(cond) {
                    if (cond && !isNull(cond) && !isEmptyList(cond)) {
                        result = evaluate(begin, {
                            env: self,
                            dynamic_scope,
                            error
                        });
                        if (isPromise(result)) {
                            return result.then(ret => {
                                result = ret;
                                return loop();
                            });
                        } else {
                            return loop();
                        }
                    } else {
                        return result;
                    }
                }
                return unpromise(cond, next);
            })();
        }), `(while cond . body)

            Macro that create a loop, it exectue body untill cond expression is false`),
        // ------------------------------------------------------------------
        'if': doc(new Macro('if', function(code, { dynamic_scope, error }) {
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            var env = this;
            var resolve = (cond) => {
                typecheck('if', cond, 'boolean');
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
            return unpromise(cond, resolve);
        }), `(if cond true-expr false-expr)

            Macro evaluate condition expression and if the value is true, it
            evaluate and return true expression if not it evaluate and return
            false expression`),
        // ------------------------------------------------------------------
        'let*': doc(
            let_macro(true),
            `(let* ((a value-a) (b value-b)) body)

             Macro that creates new environment, then evaluate and assign values to
             names and then evaluate the body in context of that environment.
             Values are evaluated sequentialy and next value can access to
             previous values/names.`),
        // ------------------------------------------------------------------
        'let': doc(
            let_macro(false),
            `(let ((a value-a) (b value-b)) body)

             Macro that creates new environment, then evaluate and assign values to
             names and then evaluate the body in context of that environment.
             Values are evaluated sequentialy but you can't access
             previous values/names when next are evaluated. You can only get them
             from body of let expression.`),
        // ------------------------------------------------------------------
        'begin': doc(new Macro('begin', function(code, { dynamic_scope, error }) {
            var arr = this.get('list->array')(code);
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            var env = this;
            var result;
            return (function loop() {
                if (arr.length) {
                    var code = arr.shift();
                    var ret = evaluate(code, { env, dynamic_scope, error });
                    return unpromise(ret, value => {
                        result = value;
                        return loop();
                    });
                } else {
                    return result;
                }
            })();
        }), `(begin . args)

             Macro runs list of expression and return valuate of the list one.
             It can be used in place where you can only have single exression,
             like if expression.`),
        nop: doc(function() {
        }, `(nop)

            Empty function you can pass list of exressions to the function.
            like every function each expression will be evaluated and it will
            not return any value. you can also put this function as last to
            let or begin. This function is usefull if you want to return
            undefined, like when you call function from terminal and don't
            want any output.`),
        // ------------------------------------------------------------------
        timer: doc(new Macro('timer', function(code, { dynamic_scope, error } = {}) {
            typecheck('timer', code.car, 'number');
            var env = this;
            if (dynamic_scope) {
                dynamic_scope = this;
            }
            return new Promise((resolve) => {
                setTimeout(() => {
                    resolve(evaluate(code.cdr.car, {
                        env,
                        dynamic_scope,
                        error
                    }));
                }, code.car);
            }).then(quote);
        }), `(timer time expression)

             Function return a promise, and it will be automatically evaluated
             after specific time passes. The return value of the function
             will be value of the timer exprssion. If you want to do side effect
             only expression you can wrap your expression in nol call.`),
        // ------------------------------------------------------------------
        define: doc(Macro.defmacro('define', function(code, eval_args) {
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
                unpromise(value, value => {
                    env.set(code.car, value);
                });
            }
        }), `(define name expression)
             (define (function-name . args) body)

             Macro for defining values. It can be used to define variables,
             or function. If first argument is list it will create function
             with name beeing first element of the list. The macro evalute
             code \`(define function (lambda args body))\``),
        // ------------------------------------------------------------------
        'set-obj!': doc(function(obj, key, value) {
            if (typeof obj !== 'object' || isNull(obj)) {
                throw new Error(typeErrorMessage('set-obj!', type(obj), 'object'));
            }
            obj[key] = value;
        }, `(set-obj! obj key value)

            Function set property of JavaScript object`),
        // ------------------------------------------------------------------
        'eval': doc(function(code) {
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
        }, `(eval list)

            Function evalute LIPS code as list structure.`),
        // ------------------------------------------------------------------
        lambda: new Macro('lambda', function(code, { dynamic_scope, error } = {}) {
            var self = this;
            var __doc__;
            if (code.cdr instanceof Pair &&
                typeof code.cdr.car === 'string' &&
                code.cdr.cdr !== nil) {
                __doc__ = code.cdr.car;
            }
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
                var rest = __doc__ ? code.cdr.cdr : code.cdr;
                var output = new Pair(new Symbol('begin'), rest);
                return evaluate(output, { env, dynamic_scope, error });
            }
            var length = code.car instanceof Pair ? code.car.length() : null;
            if (!(code.car instanceof Pair)) {
                return doc(lambda, __doc__, true); // variable arguments
            }
            lambda.__code__ = new Pair(new Symbol('lambda'), code);
            // wrap and decorate with __doc__
            return doc(setFnLength(lambda, length), __doc__, true);
        }, `(lambda (a b) body)
            (lambda args body)
            (lambda (a b . rest) body)

            Macro lambda create new anonymous function, if first element of the body
            is string and there is more elements it will be documentation, that can
            be read using (help fn)`),
        'macroexpand': new Macro('macro-expand', macro_expand()),
        'macroexpand-1': new Macro('macro-expand', macro_expand(true)),
        // ------------------------------------------------------------------
        'define-macro': doc(new Macro(macro, function(macro, { dynamic_scope, error }) {
            function clear(node) {
                if (node instanceof Pair) {
                    delete node.data;
                }
                return node;
            }
            if (macro.car instanceof Pair && macro.car.car instanceof Symbol) {
                var name = macro.car.car.name;
                var __doc__;
                if (typeof macro.cdr.car === 'string' &&
                    macro.cdr.cdr !== nil) {
                    __doc__ = macro.cdr.car;
                }
                this.env[name] = Macro.defmacro(name, function(code, { macro_expand }) {
                    var env = new Environment({}, this, 'defmacro');
                    var name = macro.car.cdr;
                    var arg = code;
                    while (true) {
                        if (name === nil) {
                            break;
                        }
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
                        var rest = __doc__ ? macro.cdr.cdr : macro.cdr;
                        var pair = rest.reduce(function(result, node) {
                            return evaluate(node, { env, dynamic_scope, error });
                        });
                        if (macro_expand) {
                            return pair;
                        }
                        // second evalute of code that is returned from macro
                        // need different env because we need to call it in scope
                        // were it was called
                        pair = evaluate(pair, { env: this, dynamic_scope, error });
                        return unpromise(pair, clear);
                    }
                }, __doc__);
            }
        }), `(define-macro (name . args) body)

             Meta macro, macro that create new macros, if return value is list structure
             it will be evaluated when macro is invoked. You can use quasiquote \` and
             unquote , and unquote-splicing ,@ inside to create expression that will be
             evaluated on runtime. Macros works like this: if you pass any expression to
             macro the arguments will not be evaluated unless macro itself evaluate it.
             Because of this macro can manipulate expression (arguments) as lists.`),
        // ------------------------------------------------------------------
        quote: doc(new Macro('quote', function(arg) {
            return quote(arg.car);
        }), `(quote expression)

             Macro that return single lips expression as data (it don't evaluate its
             argument). It will return list of pairs if put in front of lips code.
             And if put in fron of symbol it will return that symbol not value
             associated with that name.`),
        'unquote-splicing': doc(function() {
            throw new Error(`You can't call \`unquote-splicing\` outside of quasiquote`);
        }, `(unquote-splicing code)

            Special form to be used in quasiquote macro, parser is processing special
            characters ,@ and create call to this pseudo function. It can be used
            to evalute expression inside and return the value without parenthesis.
            the value will be joined to the output list structure.`),
        'unquote': doc(function() {
            throw new Error(`You can't call \`unquote\` outside of quasiquote`);
        }, `(unquote code)

            Special form to be used in quasiquote macro, parser is processing special
            characters , and create call to this pseudo function. It can be used
            to evalute expression inside and return the value, the output is inserted
            into list structure created by queasiquote.`),
        // ------------------------------------------------------------------
        quasiquote: doc(new Macro('quasiquote', function(arg, { dynamic_scope, error }) {
            var self = this;
            var max_unquote = 0;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            function isPair(value) {
                return value instanceof Pair;
            }
            function resolve_pair(pair, fn, test = isPair) {
                if (pair instanceof Pair && !isEmptyList(pair)) {
                    var car = pair.car;
                    var cdr = pair.cdr;
                    if (test(car)) {
                        car = fn(car);
                    }
                    if (test(cdr)) {
                        cdr = fn(cdr);
                    }
                    if (isPromise(car) || isPromise(cdr)) {
                        return Promise.all([car, cdr]).then(([car, cdr]) => {
                            return new Pair(car, cdr);
                        });
                    } else {
                        return new Pair(car, cdr);
                    }
                }
                return pair;
            }
            function join(eval_pair, value) {
                if (eval_pair instanceof Pair) {
                    eval_pair.append(value);
                } else {
                    eval_pair = new Pair(
                        eval_pair,
                        value
                    );
                }
                return eval_pair;
            }
            function recur(pair) {
                if (pair instanceof Pair && !isEmptyList(pair)) {
                    var eval_pair;
                    if (Symbol.is(pair.car.car, 'unquote-splicing')) {
                        eval_pair = evaluate(pair.car.cdr.car, {
                            env: self,
                            dynamic_scope,
                            error
                        });
                        return unpromise(eval_pair, function(eval_pair) {
                            if (!eval_pair instanceof Pair) {
                                throw new Error('Value of unquote-splicing need' +
                                                ' to be pair');
                            }
                            const value = recur(pair.cdr);
                            return unpromise(value, value => join(eval_pair, value));
                        });
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
                                return unpromise(recur(pair.cdr.cdr), function(value) {
                                    return new Pair(
                                        new Unquote(pair.cdr.car, unquote_count),
                                        value
                                    );
                                });
                            } else {
                                return new Unquote(pair.cdr.car, unquote_count);
                            }
                        } else if (parent.cdr.cdr !== nil) {
                            return unpromise(recur(parent.cdr.cdr), function(value) {
                                parent.car.cdr = new Pair(
                                    new Unquote(node, unquote_count),
                                    parent.cdr === nil ? nil : value
                                );
                                return head.car;
                            });
                        } else {
                            parent.car.cdr = new Unquote(node, unquote_count);
                        }
                        return head.car;
                    }
                    return resolve_pair(pair, recur);
                }
                return pair;
            }
            const unquoteTest = v => isPair(v) || v instanceof Unquote;
            function unquoting(node) {
                if (node instanceof Unquote) {
                    if (max_unquote === node.count) {
                        return evaluate(node.value, { env: self, dynamic_scope, error });
                    } else {
                        return unpromise(unquoting(node.value), function(value) {
                            return new Pair(
                                new Symbol('unquote'),
                                new Pair(
                                    value,
                                    nil
                                )
                            );
                        });
                    }
                }
                return resolve_pair(node, unquoting, unquoteTest);
            }
            return unpromise(recur(arg.car), value => {
                return unpromise(unquoting(value), quote);
            });
        }), `(quasiquote list ,value ,@value)

            Similar macro to \`quote\` but inside it you can use special
            expressions unquote abbreviated to , that will evaluate expresion inside
            and return its value or unquote-splicing abbreviated to ,@ that will
            evaluate expression but return value without parenthesis (it will join)
            the list with its value. Best used with macros but it can be used outside`),
        // ------------------------------------------------------------------
        clone: doc(function(list) {
            return list.clone();
        }, `(clone list)

            Function return clone of the list.`),
        // ------------------------------------------------------------------
        append: doc(function(list, item) {
            return this.get('append!')(list.clone(), item);
        }, `(append list item)

            Function will create new list with value appended to the end. It return
            New list.`),
        'append!': doc(function(list, item) {
            if (isNull(item) || isEmptyList(item)) {
                return list;
            }
            return list.append(item);
        }, `(append! name expression)

             Destructive version of append, it modify the list in place. It return
             original list.`),
        // ------------------------------------------------------------------
        reverse: doc(function(arg) {
            if (arg instanceof Pair) {
                var arr = this.get('list->array')(arg).reverse();
                return this.get('array->list')(arr);
            } else if (!(arg instanceof Array)) {
                throw new Error(typeErrorMessage('reverse', type(arg), 'array or pair'));
            } else {
                return arg.reverse();
            }
        }, `(reverse list)

            Function will reverse the list or array. If value is not a list
            or array it will throw exception.`),
        // ------------------------------------------------------------------
        nth: doc(function(index, obj) {
            if (obj instanceof Pair) {
                var node = obj;
                var count = 0;
                while (count < index) {
                    if (!node.cdr || node.cdr === nil || node.haveCycles('cdr')) {
                        return nil;
                    }
                    node = node.cdr;
                    count++;
                }
                return node.car;
            } else if (obj instanceof Array) {
                return obj[index];
            } else {
                throw new Error(typeErrorMessage('nth', type(obj), 'array or pair', 2));
            }
        }, `(nth index obj)

            Function return nth element of the list or array. If used with different
            value it will throw exception`),
        // ------------------------------------------------------------------
        list: doc(function() {
            return Pair.fromArray([].slice.call(arguments));
        }, `(list . args)

            Function create new list out of its arguments.`),
        // ------------------------------------------------------------------
        substring: doc(function(string, start, end) {
            return string.substring(start.valueOf(), end && end.valueOf());
        }, `(substring string start end)

            Function return part of the string starting at start ending with end.`),
        // ------------------------------------------------------------------
        concat: doc(function() {
            return [].join.call(arguments, '');
        }, `(concat . strings)

            Function create new string by joining its arguments`),
        // ------------------------------------------------------------------
        join: doc(function(separator, list) {
            return this.get('list->array')(list).join(separator);
        }, `(join separator list)

            Function return string by joining elements of the list`),
        // ------------------------------------------------------------------
        split: doc(function(separator, string) {
            return this.get('array->list')(string.split(separator));
        }, `(split separator string)

            Function create list by splitting string by separatar that can
            be a string or regular expression.`),
        // ------------------------------------------------------------------
        replace: doc(function(pattern, replacement, string) {
            return string.replace(pattern, replacement);
        }, `(replace pattern replacement string)

            Function change patter to replacement inside string.`),
        // ------------------------------------------------------------------
        match: doc(function(pattern, string) {
            return this.get('array->list')(string.match(pattern));
        }, `(match pattern string)

            function return match object from JavaScript as list.`),
        // ------------------------------------------------------------------
        search: doc(function(pattern, string) {
            return string.search(pattern);
        }, `(search pattern string)

            Function return first found index of the pattern inside a string`),
        // ------------------------------------------------------------------
        string: doc(function string(obj, quote) {
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
            if (root.HTMLElement && obj instanceof root.HTMLElement) {
                return `<#HTMLElement(${obj.tagName.toLowerCase()})>`;
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
        }, `(string obj)

            Function return string LIPS representation of an object as string.`),
        // ------------------------------------------------------------------
        env: doc(function(env) {
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
        }, `(env obj)

            Function return list values (functions and variables) inside environment.`),
        'new': doc(function(obj, ...args) {
            return new obj(...args);
        }, `(new obj . args)

            Function create new JavaScript instance of an object.`),
        // ------------------------------------------------------------------
        'get': get,
        '.': get,
        // ------------------------------------------------------------------
        'unbind': doc(
            unbind,
            `(unbind fn)

             Function remove bidning from function so you can get props from it.`),
        // ------------------------------------------------------------------
        type: doc(
            type,
            `(type object)

             Function return type of an object as string.`),
        // ------------------------------------------------------------------
        'instanceof': doc(function(type, obj) {
            return obj instanceof type;
        }, `(instanceof type obj)

            Function check of object is instance of object.`),
        'function?': doc(function(obj) {
            return typeof obj === 'function';
        }, `(function? expression)

            Function check if value is a function.`),
        // ------------------------------------------------------------------
        'number?': doc(
            LNumber.isNumber,
            `(number? expression)

             Function check if value is a number`),
        // ------------------------------------------------------------------
        'string?': doc(function(obj) {
            return typeof obj === 'string';
        }, `(string? expression)

            Function check if value is a string.`),
        // ------------------------------------------------------------------
        'pair?': doc(function(obj) {
            return obj instanceof Pair;
        }, `(pair? expression)

            Function check if value is a pair or list structure.`),
        // ------------------------------------------------------------------
        'regex?': doc(function(obj) {
            return obj instanceof RegExp;
        }, `(regex? expression)

            Function check if value is regular expression.`),
        // ------------------------------------------------------------------
        'null?': doc(function(obj) {
            return isNull(obj) || (obj instanceof Pair && obj.isEmptyList());
        }, `(null? expression)

            Function check if value is nulish.`),
        // ------------------------------------------------------------------
        'boolean?': doc(function(obj) {
            return typeof obj === 'boolean';
        }, `(boolean? expression)

            Function check if value is boolean.`),
        // ------------------------------------------------------------------
        'symbol?': doc(function(obj) {
            return obj instanceof Symbol;
        }, `(symbol? expression)

            Function check if value is LIPS symbol`),
        // ------------------------------------------------------------------
        'array?': doc(function(obj) {
            return obj instanceof Array;
        }, `(array? expression)

            Function check if value is an arrray.`),
        // ------------------------------------------------------------------
        'object?': doc(function(obj) {
            return obj !== null && typeof obj === 'object' && !(obj instanceof Array);
        }, `(object? expression)

            Function check if value is an object.`),
        // ------------------------------------------------------------------
        read: doc(function read(arg) {
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
        }, `(read [string])

            Function if used with string will parse the string and return
            list structure of LIPS code. If called without an argument it
            will read string from standard input (using browser prompt or
            user defined way) and call itself with that string (parse is)
            function can be used together with eval to evaluate code from
            string`),
        // ------------------------------------------------------------------
        print: doc(function(...args) {
            this.get('stdout').write(...args.map((arg) => {
                return this.get('string')(arg);
            }));
        }, `(print . args)

            Function convert each argument to string and print the result to
            standard output (by default it's console but it can be defined
            it user code)`),
        // ------------------------------------------------------------------
        error: doc(function(...args) {
            if (root.console) {
                if (root.console.error) {
                    root.console.error(...args);
                } else if (root.console.log) {
                    root.console.log(...args);
                }
            }
        }, `(error . args)

            Display error message.`),
        // ------------------------------------------------------------------
        flatten: doc(function(list) {
            return list.flatten();
        }, `(flatten list)

            Return shallow list from tree structure (pairs).`),
        // ------------------------------------------------------------------
        'array->list': doc(function(array) {
            return Pair.fromArray(array);
        }, `(array->list array)

            Function convert JavaScript array to LIPS list.`),
        // ------------------------------------------------------------------
        'list->array': doc(function(list) {
            if (list instanceof Pair && list.isEmptyList()) {
                return [];
            }
            var result = [];
            var node = list;
            while (true) {
                if (node instanceof Pair) {
                    if (node.haveCycles('cdr')) {
                        break;
                    }
                    result.push(node.car);
                    node = node.cdr;
                } else {
                    break;
                }
            }
            return result;
        }, `(list->array list)

            Function convert LIPS list into JavaScript array.`),
        // ------------------------------------------------------------------
        apply: doc(function(fn, args) {
            typecheck('call', fn, 'function', 1);
            typecheck('call', args, 'pair', 2);
            return fn(...this.get('list->array')(args));
        }, `(apply fn args)

            Function that call function with list of arguments.`),
        // ------------------------------------------------------------------
        'length': doc(function(obj) {
            if (!obj) {
                return LNumber(0);
            }
            if (obj instanceof Pair) {
                return LNumber(obj.length());
            }
            if ("length" in obj) {
                return LNumber(obj.length);
            }
        }, `(length expression)

            Function return length of the object, the object can be list
            or any object that have length property.`),
        // ------------------------------------------------------------------
        find: doc(function find(arg, list) {
            if (isNull(list)) {
                return nil;
            }
            var fn = matcher('find', arg);
            return unpromise(fn(list.car), function(value) {
                if (value) {
                    return list.car;
                }
                return find(arg, list.cdr);
            });
        }, `(Find fn list)

            Higher order Function find first value for which function
            return true.`),
        // ------------------------------------------------------------------
        'for-each': doc(function(fn, ...args) {
            typecheck('for-each', fn, 'function');
            // we need to use call(this because babel transpile this code into:
            // var ret = map.apply(void 0, [fn].concat(args));
            // it don't work with weakBind
            var ret = this.get('map')(fn, ...args);
            if (isPromise(ret)) {
                return ret.then(() => {});
            }
        }, `(for-each fn . args)

            Higher order function that call function \`fn\` by for each
            value of the argument. If you provide more then one list as argument
            it will take each value from each list and call \`fn\` function
            with that many argument as number of list arguments.`),
        // ------------------------------------------------------------------
        map: doc(function(fn, ...args) {
            typecheck('map', fn, 'function');
            var array = args.map(list => this.get('list->array')(list));
            var result = [];
            return (function loop(i) {
                function next(value) {
                    result.push(value);
                    return loop(++i);
                }
                if (i === array[0].length) {
                    return Pair.fromArray(result);
                }
                var item = array.map((_, j) => array[j][i]);
                return unpromise(fn(...item), next);
            })(0);
        }, `(map fn . args)

            Higher order function that call function \`fn\` by for each
            value of the argument. If you provide more then one list as argument
            it will take each value from each list and call \`fn\` function
            with that many argument as number of list arguments. The return
            values of the function call is acumulated in result list and
            returned by the call to map.`),
        // ------------------------------------------------------------------
        some: doc(function some(fn, list) {
            if (isNull(list)) {
                return false;
            } else {
                return unpromise(fn(list.car), (value) => {
                    return value || some(fn, list.cdr);
                });
            }
        }, `(some fn list)

            Higher order function that call argument on each element of the list.
            It stops when function fn return true for a value if so it will
            return true. If it don't find the value it will return false`),
        // ------------------------------------------------------------------
        fold: doc(fold('fold', function(fold, fn, init, ...lists) {
            const value = fold.call(this, fn, init, ...lists.map(l => l.cdr));
            return unpromise(value, value => {
                return fn(...lists.map(l => l.car), value);
            });
        }), `(fold fn init . lists)

             Function fold is reverse of the reduce. it call function \`fn\`
             on each elements on the list and return single value.
             e.g. it call (fn a1 b1 (fn a2 b2 (fn a3 b3 '())))
             for: (fold fn '() alist blist`),
        // ------------------------------------------------------------------
        reduce: doc(fold('reduce', function(reduce, fn, init, ...lists) {
            return unpromise(fn(...lists.map(l => l.car), init), (value) => {
                return reduce.call(this, fn, value, ...lists.map(l => l.cdr));
            });
        }), `(reduce fn init list . lists)

             Higher order function take each element of the list and call
             the function with result of previous call or init and next element
             on the list until each element is processed and return single value
             as result of last call to \`fn\` function.
             e.g. it call (fn a3 b3 (fn a2 b2 (fn a1 b1 init)))
             for (reduce fn init alist blist`),
        // ------------------------------------------------------------------
        filter: doc(function(arg, list) {
            var array = this.get('list->array')(list);
            var result = [];
            var fn = matcher('filter', arg);
            return (function loop(i) {
                function next(value) {
                    if (value) {
                        result.push(item);
                    }
                    return loop(++i);
                }
                if (i === array.length) {
                    return Pair.fromArray(result);
                }
                var item = array[i];
                return unpromise(fn(item, i), next);
            })(0);
        }, `(filter fn list)

            Higher order function that call \`fn\` for each element of the list
            and return list for only those elements for which funtion return
            true value.`),
        // ------------------------------------------------------------------
        range: doc(function(n) {
            if (n instanceof LNumber) {
                n = n.valueOf();
            }
            return Pair.fromArray(new Array(n).fill(0).map((_, i) => LNumber(i)));
        }, `(range n)

            Function return list of n numbers from 0 to n - 1`),
        // ------------------------------------------------------------------
        compose: doc(
            compose,
            `(compose . fns)

             Higher order function and create new function that apply all functions
             From right to left and return it's value. Reverse of compose.
             e.g.:
             ((compose (curry + 2) (curry * 3)) 3)
             11
            `),
        pipe: doc(
            pipe,
            `(pipe . fns)

             Higher order function and create new function that apply all functions
             From left to right and return it's value. Reverse of compose.
             e.g.:
             ((pipe (curry + 2) (curry * 3)) 3)
             15`),
        curry: doc(
            curry,
            `(curry fn . args)

             Higher order function that create curried version of the function.
             The result function will have parially applied arguments and it
             will keep returning functions until all arguments are added

             e.g.:
             (define (add a b c d) (+ a b c d))
             (define add1 (curry add 1))
             (define add12 (add 2))
             (print (add12 3 4))`),
        // ------------------------------------------------------------------
        odd: doc(singleMathOp(function(num) {
            return LNumber(num).isOdd();
        }), `(odd number)
             Function check if number os odd.`),
        // ------------------------------------------------------------------
        even: doc(singleMathOp(function(num) {
            return LNumber(num).isEvent();
        }), `(even number)

             Function check if number is even.`),
        // ------------------------------------------------------------------
        // math functions
        '*': doc(reduceMathOp(function(a, b) {
            return LNumber(a).mul(b);
        }), `(* . numbers)

             Multiplicate all numbers passed as arguments. If single value is passed
              it will return that value.`),
        // ------------------------------------------------------------------
        '+': doc(reduceMathOp(function(a, b) {
            return LNumber(a).add(b);
        }), `(+ . numbers)

             Sum all numbers passed as arguments. If single value is passed it will
             return that value.`),
        // ------------------------------------------------------------------
        '-': doc(function(...args) {
            if (args.length === 1) {
                return LNumber(args[0]).neg();
            }
            if (args.length) {
                return args.reduce(binaryMathOp(function(a, b) {
                    return LNumber(a).sub(b);
                }));
            }
        }, `(- . numbers)
            (- number)

            Substract number passed as argument. If only one argument is passed
            it will negate the value.`),
        // ------------------------------------------------------------------
        '/': doc(reduceMathOp(function(a, b) {
            return LNumber(a).div(b);
        }), `(/ . numbers)

             Divide number passed as arguments one by one. If single argument
             is passed it will return that value.`),
        // ------------------------------------------------------------------
        'abs': doc(singleMathOp(function(n) {
            return LNumber(n).abs();
        }), `(abs number)

             Function create absolute value from number.`),
        // ------------------------------------------------------------------
        'sqrt': doc(singleMathOp(function(n) {
            return Math.sqrt(n);
        }), `(sqrt number)

             Function return square root of the number.`),
        // ------------------------------------------------------------------
        '**': doc(binaryMathOp(function(a, b) {
            return LNumber(a).pow(b);
        }), `(** a b)

            Function calculate number a to to the power of b. It can throw
            exception when ** native operator is not supported.`),
        // ------------------------------------------------------------------
        '1+': doc(singleMathOp(function(number) {
            return LNumber(number).add(1);
        }), `(1+ number)

             Function add 1 to the number and return result.`),
        // ------------------------------------------------------------------
        '1-': doc(singleMathOp(function(number) {
            return LNumber(number).sub(1);
        }), `(1- number)

             Function substract 1 from the number and return result.`),
        // ------------------------------------------------------------------
        '++': doc(new Macro('++', function(code) {
            typecheck('++', code.car, 'symbol');
            var car = this.get(code.car);
            var value = LNumber(car).add(1);
            this.set(code.car, value);
            return value;
        }), `(++ variable)

             Macro that work only on variables and increment the value by one.`),
        // ------------------------------------------------------------------
        '--': doc(new Macro('--', function(code) {
            typecheck('--', code.car, 'symbol');
            var car = this.get(code.car);
            var value = LNumber(car).sub(1);
            this.set(code.car, value);
            return value;
        }), `(-- variable)

             Macro that decrement the value it work only on symbols`),
        // ------------------------------------------------------------------
        '%': doc(reduceMathOp(function(a, b) {
            return LNumber(a).mod(b);
        }), `(% . numbers)

             Function use modulo operation on each of the numbers. It return
             single number.`),
        // ------------------------------------------------------------------
        // Booleans
        '==': doc(function(a, b) {
            return LNumber(a).cmp(b) === 0;
        }, `(== a b)

            Function compare two numbers and check if they are equal.`),
        // ------------------------------------------------------------------
        '>': doc(function(a, b) {
            return LNumber(a).cmp(b) === 1;
        }, `(> a b)

            Function compare two numbers and check if first argument is greater
            than the second one`),
        // ------------------------------------------------------------------
        '<': doc(function(a, b) {
            return LNumber(a).cmp(b) === -1;
        }, `(< a b)

            Function compare two numbers and check if first argument is less
            than the second one`),
        // ------------------------------------------------------------------
        '<=': doc(function(a, b) {
            return [0, -1].includes(LNumber(a).cmp(b));
        }, `(<= a b)

            Function compare two numbers and check if first argument is less or
            equal to the second one`),
        // ------------------------------------------------------------------
        '>=': doc(function(a, b) {
            return [0, 1].includes(LNumber(a).cmp(b));
        }, `(>= a b)

            Function compare two numbers and check if first argument is less or
            equal to the second one`),
        // ------------------------------------------------------------------
        'eq?': doc(
            equal,
            `(eq? a b)

             Function compare two values if they are identical.`),
        // ------------------------------------------------------------------
        or: doc(new Macro('or', function(code, { dynamic_scope, error }) {
            var args = this.get('list->array')(code);
            var self = this;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            var result;
            return (function loop() {
                function next(value) {
                    result = value;
                    if (result) {
                        return result;
                    } else {
                        return loop();
                    }
                }
                var arg = args.shift();
                if (typeof arg === 'undefined') {
                    if (result) {
                        return result;
                    } else {
                        return false;
                    }
                } else {
                    var value = evaluate(arg, { env: self, dynamic_scope, error });
                    return unpromise(value, next);
                }
            })();
        }), `(or . expressions)

             Macro execute the values one by one and return the one that is truthy value.
             If there are no expression that evaluate to true it return false.`),
        // ------------------------------------------------------------------
        and: doc(new Macro('and', function(code, { dynamic_scope, error } = {}) {
            var args = this.get('list->array')(code);
            var self = this;
            if (dynamic_scope) {
                dynamic_scope = self;
            }
            if (!args.length) {
                return true;
            }
            var result;
            return (function loop() {
                function next(value) {
                    result = value;
                    if (!result) {
                        return false;
                    } else {
                        return loop();
                    }
                }
                var arg = args.shift();
                if (typeof arg === 'undefined') {
                    if (result) {
                        return result;
                    } else {
                        return false;
                    }
                } else {
                    var value = evaluate(arg, { env: self, dynamic_scope, error });
                    return unpromise(value, next);
                }
            })();
        }), `(and . expressions)

             Macro evalute each expression in sequence if any value return false it will
             return false. If each value return true it will return the last value.
             If it's called without arguments it will return true.`),
        // bit operations
        '|': doc(function(a, b) {
            return LNumber(a).or(b);
        }, `(& a b)

            Function calculate or bit operation.`),
        '&': doc(function(a, b) {
            return LNumber(a).and(b);
        }, `(& a b)

            Function calculate and bit operation.`),
        '~': doc(function(a) {
            return LNumber(a).neg();
        }, `(~ number)

            Function negate the value.`),
        '>>': doc(function(a, b) {
            return LNumber(a).shr(b);
        }, `(>> a b)

            Function right shit the value a by value b.`),
        '<<': doc(function(a, b) {
            return LNumber(a).shl(b);
        }, `(<< a b)

            Function left shit the value a by value b.`),
        not: doc(function(value) {
            if (isEmptyList(value)) {
                return true;
            }
            return !value;
        }, `(not object)

            Function return negation of the argument.`),
        '->': doc(function(obj, name, ...args) {
            return obj[name](...args);
        }, `(-> obj name . args)

            Function get function from object and call it with arguments.`)
    }, undefined, 'global');
    // ----------------------------------------------------------------------
    ['floor', 'round', 'ceil'].forEach(fn => {
        global_env.set(fn, doc(function(value) {
            if (value instanceof LNumber) {
                return value[fn]();
            }
            throw new Error(`${typeof value} ${value.toString()} is not a number`);
        }, `(${fn} number)

            Function calculate ${fn} of a number.`));
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
    combinations(['d', 'a'], 2, 5).forEach(spec => {
        const s = spec.split('');
        const chars = s.slice().reverse();
        const code = s.map(c => `(c${c}r`).join(' ') + ' arg' + ')'.repeat(s.length);
        const name = 'c' + spec + 'r';
        global_env.set(name, doc(function(arg) {
            return chars.reduce(function(list, type) {
                if (type === 'a') {
                    return list.car;
                } else {
                    return list.cdr;
                }
            }, arg);
        }, `(${name} arg)

            Function calculate ${code}`));
    });

    // ----------------------------------------------------------------------
    if (typeof global !== 'undefined') {
        global_env.set('global', global);
    } else if (typeof window !== 'undefined') {
        global_env.set('window', window);
    }
    // ----------------------------------------------------------------------
    function typeErrorMessage(fn, got, expected, position = null) {
        let postfix = fn ? ` in function \`${fn}\`` : '';
        if (position !== null) {
            postfix += ` argument ${position}`;
        }
        return `Expecting ${expected} got ${got}${postfix}`;
    }
    // ----------------------------------------------------------------------
    function typecheck(fn, arg, expected, position = null) {
        const arg_type = type(arg);
        if (arg_type !== expected) {
            throw new Error(typeErrorMessage(fn, arg_type, expected, position));
        }
    }
    // ----------------------------------------------------------------------
    function type(obj) {
        var mapping = {
            'pair': Pair,
            'symbol': Symbol,
            'macro': Macro,
            'array': Array,
            'native_symbol': root.Symbol
        };
        if (obj === nil) {
            return 'nil';
        }
        if (obj === null) {
            return 'null';
        }
        for (let [key, value] of Object.entries(mapping)) {
            if (obj instanceof value) {
                return key;
            }
        }
        if (obj instanceof LNumber) {
            return 'number';
        }
        if (obj instanceof RegExp) {
            return "regex";
        }
        if (typeof obj === 'object') {
            return obj.constructor.name;
        }
        return typeof obj;
    }
    // ----------------------------------------------------------------------
    // :; wrap tree of Promises with single Promise or return argument as is
    // :: if tree have no Promises
    // ----------------------------------------------------------------------
    function resolvePromises(arg) {
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
                if (!node.haveCycles('car')) {
                    traverse(node.car);
                }
                if (!node.haveCycles('cdr')) {
                    traverse(node.cdr);
                }
            } else if (node instanceof Array) {
                node.forEach(traverse);
            }
        }
        async function promise(node) {
            var pair = new Pair(
                node.haveCycles('car') ? node.car : await resolve(node.car),
                node.haveCycles('cdr') ? node.cdr : await resolve(node.cdr)
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
        markCycles(node);
        while (true) {
            if (node instanceof Pair && !isEmptyList(node)) {
                var arg = evaluate(node.car, { env, dynamic_scope, error });
                if (dynamic_scope) {
                    arg = unpromise(arg, arg => {
                        if (typeof arg === 'function' && isNativeFunction(arg)) {
                            return arg.bind(dynamic_scope);
                        }
                        return arg;
                    });
                }
                args.push(arg);
                if (node.haveCycles('cdr')) {
                    break;
                }
                node = node.cdr;
            } else {
                break;
            }
        }
        return resolvePromises(args);
    }
    // ----------------------------------------------------------------------
    function evaluate_macro(macro, code, eval_args) {
        if (code instanceof Pair) {
            //code = code.clone();
        }
        var value = macro.invoke(code, eval_args);
        value = resolvePromises(value);
        return unpromise(value, function ret(value) {
            if (value && value.data || !(value instanceof Pair)) {
                return value;
            } else {
                return evaluate(value, eval_args);
            }
        });
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
            if (isNull(code)) {
                return code;
            }
            if (isEmptyList(code)) {
                return emptyList();
            }
            var first = code.car;
            var rest = code.cdr;
            if (first instanceof Pair) {
                value = resolvePromises(evaluate(first, eval_args));
                if (isPromise(value)) {
                    return value.then((value) => {
                        return evaluate(new Pair(value, code.cdr), eval_args);
                    });
                    // else is later in code
                } else if (typeof value !== 'function') {
                    throw new Error(
                        type(value) + ' ' + env.get('string')(value) +
                            ' is not a function while evaluating ' + code.toString()
                    );
                }
            }
            if (first instanceof Symbol) {
                value = env.get(first, true);
                if (value instanceof Macro) {
                    return unpromise(evaluate_macro(value, rest, eval_args), result => {
                        if (result instanceof Pair) {
                            return result.markCycles();
                        }
                        return result;
                    });
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
            if (typeof value === 'function') {
                var args = get_function_args(rest, eval_args);
                return unpromise(args, function(args) {
                    var scope = dynamic_scope || env;
                    return unpromise(resolvePromises(value.apply(scope, args)), (result) => {
                        if (result instanceof Pair) {
                            return quote(result.markCycles());
                        }
                        return result;
                    });
                });
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
        // proper indent of multi line strings
        var tokens = tokenize(string, true).map(function(token) {
            if (token.token.match(string_re) && token.col) {
                // col + 1 because of open quote character
                var re = new RegExp(`^ {${token.col + 1}}`);
                return token.token.split('\n').map(line => {
                    return line.replace(re, '');
                }).join('\n');
            }
            return token.token.trim();
        }).filter(function(token) {
            return token && !token.match(/^;/);
        });
        var list = parse(tokens);
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
            var scripts = Array.from(document.querySelectorAll('script'));
            return (function loop() {
                var script = scripts.shift();
                if (script) {
                    var type = script.getAttribute('type');
                    if (type === lips_mime) {
                        var src = script.getAttribute('src');
                        if (src) {
                            return root.fetch(src).then(res => res.text())
                                .then(exec).then(loop);
                        } else {
                            return exec(script.innerHTML).then(loop);
                        }
                    } else if (type && type.match(/lips|lisp/)) {
                        console.warn('Expecting ' + lips_mime + ' found ' + type);
                    }
                    return loop();
                }
            })();
        }
    }
    // ----------------------------------------------------------------------
    if (typeof window !== 'undefined') {
        contentLoaded(window, init);
    }
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
        specials,
        nil,
        resolvePromises,
        Symbol,
        LNumber
    };
});
