/**@license
 *   __ __                          __
 *  / / \ \       _    _  ___  ___  \ \
 * | |   \ \     | |  | || . \/ __>  | |
 * | |    > \    | |_ | ||  _/\__ \  | |
 * | |   / ^ \   |___||_||_|  <___/  | |
 *  \_\ /_/ \_\                     /_/
 *
 * <https://lips.js.org>
 *
 * LIPS is Pretty Simple - Scheme based powerful LISP in JavaScript
 *
 * Copyright (c) 2018-2024 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 *
 * Includes:
 *
 * ucs2decode function from Punycode v 2.1.1 by Mathias Bynens MIT License
 *
 * Author: Diego Perini (diego.perini at gmail.com)
 * Summary: cross-browser wrapper for DOMContentLoaded
 * Updated: 20101020
 * License: MIT
 * Version: 1.2
 *
 * contentloaded.js
 *
 * URL:
 * http://javascript.nwbox.com/ContentLoaded/
 * http://javascript.nwbox.com/ContentLoaded/MIT-LICENSE
 *
 * The rationalize algorithm is by Per M.A. Bothner, Alan Bawden and Marc Feeley.
 * source: Kawa, C-Gambit
 *
 * Build time: {{DATE}}
 */
/*
 * TODO: consider using exec in env.eval or use different maybe_async code
 */
/* global jQuery, BigInt, Map, WeakMap, Set, Symbol, importScripts, Uint8Array */
"use strict";

const root = typeof global !== 'undefined' ? global : self;

import { addExtension, Encoder } from 'cbor-x';
import { pack, unpack } from 'lzjb-pack';
import unfetch from 'unfetch';

/* c8 ignore next 3 */
if (!root.fetch) {
    root.fetch = unfetch;
}

// -------------------------------------------------------------------------
// :: typechecking maps
// -------------------------------------------------------------------------
const type_mapping = {
    'pair': Pair,
    'symbol': LSymbol,
    'number': LNumber,
    'array': Array,
    'nil': Nil,
    'character': LCharacter,
    'values': Values,
    'input-port': InputPort,
    'output-port': OutputPort,
    'regex': RegExp,
    'syntax': Syntax,
    'eof': EOF,
    'macro': Macro,
    'string': LString,
    'native-symbol': Symbol
};
const type_constants = new Map([
    [NaN, 'NaN'],
    [null, 'null']
]);
// -------------------------------------------------------------------------

let fs, path, nodeRequire;

const BN = root.BN;

/* eslint-disable */
/* c8 ignore next 32 */
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
// -------------------------------------------------------------------------
/* eslint-disable */
/* c8 ignore next 13 */
function log(x, ...args) {
    if (is_debug()) {
        if (is_plain_object(x)) {
            console.log(map_object(x, function(value) {
                return toString(value, true);
            }));
        } else {
            console.log(toString(x, true), ...args.map(item => {
                return toString(item, true);
            }));
        }
    }
}
// ----------------------------------------------------------------------
/* c8 ignore next */
function is_debug(n = null) {
    const debug = user_env && user_env.get('DEBUG', { throwError: false });
    if (n === null) {
        return debug === true;
    }
    return debug?.valueOf() === n.valueOf();
}
/* eslint-enable */
/* eslint-disable max-len */
// functions generate regexes to match number rational, integer, complex, complex+rational
function num_mnemicic_re(mnemonic) {
    return mnemonic ? `(?:#${mnemonic}(?:#[ie])?|#[ie]#${mnemonic})` : '(?:#[ie])?';
}
function gen_rational_re(mnemonic, range) {
    return `${num_mnemicic_re(mnemonic)}[+-]?${range}+/${range}+`;
}
// TODO: float complex
function gen_complex_re(mnemonic, range) {
    // [+-]i have (?=..) so it don't match +i from +inf.0
    return `${num_mnemicic_re(mnemonic)}(?:[+-]?(?:${range}+/${range}+|nan.0|inf.0|${range}+))?(?:[+-]i|[+-]?(?:${range}+/${range}+|${range}+|nan.0|inf.0)i)(?=[()[\\]\\s]|$)`;
}
function gen_integer_re(mnemonic, range) {
    return `${num_mnemicic_re(mnemonic)}[+-]?${range}+`;
}
var re_re = /^#\/((?:\\\/|[^/]|\[[^\]]*\/[^\]]*\])+)\/([gimyus]*)$/;
var float_stre = '(?:[-+]?(?:[0-9]+(?:[eE][-+]?[0-9]+)|(?:\\.[0-9]+|[0-9]+\\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\\.)';
// TODO: extend to ([+-]1/2|float)([+-]1/2|float)
var complex_float_stre = `(?:#[ie])?(?:[+-]?(?:[0-9][0-9_]*/[0-9][0-9_]*|nan.0|inf.0|${float_stre}|[+-]?[0-9]+))?(?:${float_stre}|[+-](?:[0-9]+/[0-9]+|[0-9]+|nan.0|inf.0))i`;
var float_re = new RegExp(`^(#[ie])?${float_stre}$`, 'i');
function make_complex_match_re(mnemonic, range) {
    // complex need special treatment of 10e+1i when it's hex or decimal
    var neg = mnemonic === 'x' ? `(?!\\+|${range})` : `(?!\\.|${range})`;
    var fl = '';
    if (mnemonic === '') {
        fl = '(?:[-+]?(?:[0-9]+(?:[eE][-+]?[0-9]+)|(?:\\.[0-9]+|[0-9]+\\.[0-9]+(?![0-9]))(?:[eE][-+]?[0-9]+)?))';
    }
    return new RegExp(`^((?:(?:${fl}|[-+]?inf.0|[-+]?nan.0|[+-]?${range}+/${range}+(?!${range})|[+-]?${range}+)${neg})?)(${fl}|[-+]?inf.0|[-+]?nan.0|[+-]?${range}+/${range}+|[+-]?${range}+|[+-])i$`, 'i');
}
var complex_list_re = (function() {
    var result = {};
    [
        [10, '', '[0-9]'],
        [16, 'x', '[0-9a-fA-F]'],
        [8, 'o', '[0-7]'],
        [2, 'b', '[01]']
    ].forEach(([radix, mnemonic, range]) => {
        result[radix] = make_complex_match_re(mnemonic, range);
    });
    return result;
})();
const characters = {
    'alarm': '\x07',
    'backspace': '\x08',
    'delete': '\x7F',
    'escape': '\x1B',
    'newline': '\n',
    'null': '\x00',
    'return': '\r',
    'space': ' ',
    'tab': '\t',
    // new symbols from ASCII table in SRFI-175
    'dle': '\x10',
    'soh': '\x01',
    'dc1': '\x11',
    'stx': '\x02',
    'dc2': '\x12',
    'etx': '\x03',
    'dc3': '\x13',
    'eot': '\x04',
    'dc4': '\x14',
    'enq': '\x05',
    'nak': '\x15',
    'ack': '\x06',
    'syn': '\x16',
    'bel': '\x07',
    'etb': '\x17',
    'bs': '\x08',
    'can': '\x18',
    'ht': '\x09',
    'em': '\x19',
    'lf': '\x0a',
    'sub': '\x1a',
    'vt': '\x0b',
    'esc': '\x1b',
    'ff': '\x0c',
    'fs': '\x1c',
    'cr': '\x0d',
    'gs': '\x1d',
    'so': '\x0e',
    'rs': '\x1e',
    'si': '\x0f',
    'us': '\x1f',
    'del': '\x7f'
};
// -------------------------------------------------------------------------
// :: ref: https://github.com/bestiejs/punycode.js/blob/master/punycode.js
// -------------------------------------------------------------------------
function ucs2decode(string) {
    const output = [];
    let counter = 0;
    const length = string.length;
    while (counter < length) {
        const value = string.charCodeAt(counter++);
        if (value >= 0xD800 && value <= 0xDBFF && counter < length) {
            // It's a high surrogate, and there is a next character.
            const extra = string.charCodeAt(counter++);
            if ((extra & 0xFC00) === 0xDC00) { // Low surrogate.
                output.push(((value & 0x3FF) << 10) + (extra & 0x3FF) + 0x10000);
            } else {
                // It's an unmatched surrogate; only append this code unit, in case the
                // next code unit is the high surrogate of a surrogate pair.
                output.push(value);
                counter--;
            }
        } else {
            output.push(value);
        }
    }
    return output;
}
// -------------------------------------------------------------------------
const character_symbols = Object.keys(characters).join('|');
const char_sre_re = `#\\\\(?:x[0-9a-f]+|${character_symbols}|[\\s\\S])`;
const char_re = new RegExp(`^${char_sre_re}$`, 'i');
// Complex with (int) (float) (rational)
function make_num_stre(fn) {
    const ranges = [
        ['o', '[0-7]'],
        ['x', '[0-9a-fA-F]'],
        ['b', '[01]'],
        ['d', '[0-9]'],
        ['', '[0-9]']
    ];
    // float exception that don't accept mnemonics
    let result = ranges.map(([m, range]) => fn(m, range)).join('|');
    if (fn === gen_complex_re) {
        result = complex_float_stre + '|' + result;
    }
    return result;
}
function make_type_re(fn) {
    return new RegExp('^(?:' + make_num_stre(fn) + ')$', 'i');
}
const complex_re = make_type_re(gen_complex_re);
const rational_re = make_type_re(gen_rational_re);
const int_re = make_type_re(gen_integer_re);

// regexes with full range but without mnemonics for string->number
const int_bare_re = new RegExp('^(?:' + gen_integer_re('', '[0-9a-f]') + ')$', 'i');
const rational_bare_re = new RegExp('^(?:' + gen_rational_re('', '[0-9a-f]') + ')$', 'i');
const complex_bare_re = new RegExp('^(?:' + gen_complex_re('', '[0-9a-f]') + ')$', 'i');

const complex_bare_match_re = make_complex_match_re('', '[0-9a-fA-F]');

const pre_num_parse_re = /((?:#[xodbie]){0,2})(.*)/i;
/* eslint-enable */
function num_pre_parse(arg) {
    var parts = arg.match(pre_num_parse_re);
    var options = {};
    if (parts[1]) {
        var type = parts[1].replace(/#/g, '').toLowerCase().split('');
        if (type.includes('x')) {
            options.radix = 16;
        } else if (type.includes('o')) {
            options.radix = 8;
        } else if (type.includes('b')) {
            options.radix = 2;
        } else if (type.includes('d')) {
            options.radix = 10;
        }
        if (type.includes('i')) {
            options.inexact = true;
        }
        if (type.includes('e')) {
            options.exact = true;
        }
    }
    options.number = parts[2];
    return options;
}
// ----------------------------------------------------------------------
function parse_rational(arg, radix = 10) {
    var parse = num_pre_parse(arg);
    var parts = parse.number.split('/');
    var num = LRational({
        num: LNumber([parts[0], parse.radix || radix]),
        denom: LNumber([parts[1], parse.radix || radix])
    });
    if (parse.inexact) {
        return num.valueOf();
    } else {
        return num;
    }
}
// ----------------------------------------------------------------------
function parse_integer(arg, radix = 10) {
    var parse = num_pre_parse(arg);
    if (parse.inexact) {
        return LFloat(parseInt(parse.number, parse.radix || radix), true);
    }
    return LNumber([parse.number, parse.radix || radix]);
}
// ----------------------------------------------------------------------
function parse_character(arg) {
    var m = arg.match(/#\\x([0-9a-f]+)$/i);
    var char;
    if (m) {
        var ord = parseInt(m[1], 16);
        char = String.fromCodePoint(ord);
    } else {
        m = arg.match(/#\\([\s\S]+)$/);
        if (m) {
            char = m[1];
        }
    }
    if (char) {
        return LCharacter(char);
    }
    throw new Error('Parse: invalid character');
}
// ----------------------------------------------------------------------
function parse_complex(arg, radix = 10) {
    function parse_num(n) {
        var value;
        if (n === '+') {
            value = LNumber(1);
        } else if (n === '-') {
            value = LNumber(-1);
        } else if (n.match(int_bare_re)) {
            value = LNumber([n, radix]);
        } else if (n.match(rational_bare_re)) {
            var parts = n.split('/');
            value = LRational({
                num: LNumber([parts[0], radix]),
                denom: LNumber([parts[1], radix])
            });
        } else if (n.match(float_re)) {
            var float = parse_float(n);
            if (parse.exact) {
                return float.toRational();
            }
            return float;
        } else if (n.match(/nan.0$/)) {
            return LNumber(NaN);
        } else if (n.match(/inf.0$/)) {
            if (n[0] === '-') {
                return LNumber(Number.NEGATIVE_INFINITY);
            }
            return LNumber(Number.POSITIVE_INFINITY);
        } else {
            throw new Error('Internal Parser Error');
        }
        if (parse.inexact) {
            return LFloat(value.valueOf(), true);
        }
        return value;
    }
    var parse = num_pre_parse(arg);
    radix = parse.radix || radix;
    var parts;
    var bare_match = parse.number.match(complex_bare_match_re);
    if (radix !== 10 && bare_match) {
        parts = bare_match;
    } else {
        parts = parse.number.match(complex_list_re[radix]);
    }
    var re, im;
    im = parse_num(parts[2]);
    if (parts[1]) {
        re = parse_num(parts[1]);
    } else if (im instanceof LFloat) {
        re = LFloat(0);
    } else {
        re = LNumber(0);
    }
    if (im.cmp(0) === 0 && im.__type__ === 'bigint') {
        return re;
    }
    return LComplex({ im, re });
}
// ----------------------------------------------------------------------
function is_int(value) {
    return parseInt(value.toString(), 10) === value;
}
// ----------------------------------------------------------------------
function parse_big_int(str) {
    var num_match = str.match(/^(([-+]?[0-9]*)(?:\.([0-9]+))?)e([-+]?[0-9]+)/i);
    if (num_match) {
        var exponent = parseInt(num_match[4], 10);
        var mantisa;// = parseFloat(num_match[1]);
        var digits = num_match[1].replace(/[-+]?([0-9]*)\..+$/, '$1').length;
        var decimal_points = num_match[3] && num_match[3].length;
        if (digits < Math.abs(exponent)) {
            mantisa = LNumber([num_match[1].replace(/\./, ''), 10]);
            if (decimal_points) {
                exponent -= decimal_points;
            }
        }
    }
    return { exponent, mantisa };
}

// ----------------------------------------------------------------------
function string_to_float(str) {
    if (str.match(/e/i)) {
        const [coefficient, exponent] = str.split('e');
        const decimal_places = Math.abs(parseInt(exponent));
        if (decimal_places < 7 && exponent < 0) {
            const zeros = '0'.repeat(decimal_places - 1);
            const sign = coefficient[0] === '-' ? '-' : '+';
            const digits = coefficient.replace(/(^[-+])|\./g, '');
            const float_str = `${sign}0.${zeros}${digits}`;
            return parseFloat(float_str);
        }
    }
    return parseFloat(str);
}

// ----------------------------------------------------------------------
function parse_float(arg) {
    const parse = num_pre_parse(arg);
    let value = string_to_float(parse.number);
    const simple_number = (parse.number.match(/\.0$/) ||
                         !parse.number.match(/\./)) && !parse.number.match(/e/i);
    if (!parse.inexact) {
        if (parse.exact && simple_number) {
            return LNumber(value);
        }
        // positive big num that eval to int e.g.: 1.2e+20
        if (is_int(value) &&
            Number.isSafeInteger(value) &&
            parse.number.match(/e\+?[0-9]/i)) {
            return LNumber(value);
        }
        // calculate big int and big fraction by hand - it don't fit into JS float
        const { mantisa, exponent } = parse_big_int(parse.number);
        if (mantisa !== undefined && exponent !== undefined) {
            const factor = LNumber(10).pow(LNumber(Math.abs(exponent)));
            if (parse.exact && exponent < 0) {
                return LRational({ num: mantisa, denom: factor });
            } else if (exponent > 0 && (parse.exact || !parse.number.match(/\./))) {
                return LNumber(mantisa).mul(factor);
            }
        }
    }
    value = LFloat(value, true);
    if (parse.exact) {
        return value.toRational();
    }
    return value;
}
// ----------------------------------------------------------------------
function parse_string(string) {
    // handle non JSON escapes and skip unicode escape \u (even partial)
    string = string.replace(/\\x([0-9a-f]+);/ig, function(_, hex) {
        return "\\u" + hex.padStart(4, '0');
    }).replace(/\n/g, '\\n'); // in LIPS strings can be multiline
    var m = string.match(/(\\*)(\\x[0-9A-F])/i);
    if (m && m[1].length % 2 === 0) {
        throw new Error(`Invalid string literal, unclosed ${m[2]}`);
    }
    try {
        const str =  LString(JSON.parse(string));
        str.freeze();
        return str;
    } catch (e) {
        const msg = e.message.replace(/in JSON /, '').replace(/.*Error: /, '');
        throw new Error(`Invalid string literal: ${msg}`);
    }
}
// ----------------------------------------------------------------------
function parse_symbol(arg) {
    if (arg.match(/^\|.*\|$/)) {
        arg = arg.replace(/(^\|)|(\|$)/g, '');
        var chars = {
            t: '\t',
            r: '\r',
            n: '\n'
        };
        arg = arg.replace(/\\(x[^;]+);/g, function(_, chr) {
            return String.fromCharCode(parseInt('0' + chr, 16));
        }).replace(/\\(.)/g, function(_, chr) {
            return chars[chr] || chr;
        });
    }
    return new LSymbol(arg);
}
// ----------------------------------------------------------------------
function parse_argument(arg) {
    if (constants.hasOwnProperty(arg)) {
        return constants[arg];
    }
    if (arg.match(/^"[\s\S]*"$/)) {
        return parse_string(arg);
    } else if (arg[0] === '#') {
        var regex = arg.match(re_re);
        if (regex) {
            return new RegExp(regex[1], regex[2]);
        } else if (arg.match(char_re)) {
            return parse_character(arg);
        }
        // characters with more than one codepoint
        var m = arg.match(/#\\(.+)/);
        if (m && ucs2decode(m[1]).length === 1) {
            return parse_character(arg);
        }
    }
    if (arg.match(/[0-9a-f]|[+-]i/i)) {
        if (arg.match(int_re)) {
            return parse_integer(arg);
        } else if (arg.match(float_re)) {
            return parse_float(arg);
        } else if (arg.match(rational_re)) {
            return parse_rational(arg);
        } else if (arg.match(complex_re)) {
            return parse_complex(arg);
        }
    }
    if (arg.match(/^#[iexobd]/)) {
        throw new Error('Invalid numeric constant: ' + arg);
    }
    return parse_symbol(arg);
}
// ----------------------------------------------------------------------
function is_atom_string(str) {
    return !(['(', ')', '[', ']'].includes(str) ||
             specials.names().includes(str));
}
// ----------------------------------------------------------------------
function is_symbol_string(str) {
    return is_atom_string(str) &&
        !(str.match(re_re) ||
          str.match(/^"[\s\S]*"$/) || str.match(int_re) ||
          str.match(float_re) || str.match(complex_re) ||
          str.match(rational_re) || str.match(char_re) ||
          ['#t', '#f', 'nil'].includes(str));
}
// ----------------------------------------------------------------------
var string_re = /"(?:\\[\S\s]|[^"])*"?/g;
// ----------------------------------------------------------------------
function escape_regex(str) {
    if (typeof str === 'string') {
        var special = /([-\\^$[\]()+{}?*.|])/g;
        return str.replace(special, '\\$1');
    }
    return str;
}
// ----------------------------------------------------------------------
// Stack used in balanced function
// TODO: use it in parser
// ----------------------------------------------------------------------
function Stack() {
    this.data = [];
}
Stack.prototype.push = function(item) {
    this.data.push(item);
};
Stack.prototype.top = function() {
    return this.data[this.data.length - 1];
};
Stack.prototype.pop = function() {
    return this.data.pop();
};
Stack.prototype.is_empty = function() {
    return !this.data.length;
};
// ----------------------------------------------------------------------
function tokens(str) {
    if (str instanceof LString) {
        str = str.valueOf();
    }
    var lexer = new Lexer(str, { whitespace: true });
    var result = [];
    while (true) {
        const token = lexer.peek(true);
        if (token === eof) {
            break;
        }
        result.push(token);
        lexer.skip();
    }
    return result;
}
// ----------------------------------------------------------------------
function multiline_formatter(meta) {
    var { token, ...rest } = meta;
    if (token.match(/^"[\s\S]*"$/) && token.match(/\n/)) {
        var re = new RegExp('^ {1,' + (meta.col + 1) + '}', 'mg');
        token = token.replace(re, '');
    }
    return {
        token,
        ...rest
    };
}
// ----------------------------------------------------------------------
function Thunk(fn, cont = () => {}) {
    this.fn = fn;
    this.cont = cont;
}
// ----------------------------------------------------------------------
Thunk.prototype.toString = function() {
    return '#<Thunk>';
};
// ----------------------------------------------------------------------
function trampoline(fn) {
    return function(...args) {
        return unwind(fn.apply(this, args));
    };
}
// ----------------------------------------------------------------------
function unwind(result) {
    while (result instanceof Thunk) {
        const thunk = result;
        result = result.fn();
        if (!(result instanceof Thunk)) {
            thunk.cont();
        }
    }
    return result;
}
// ----------------------------------------------------------------------
function tokenize(str, meta = false) {
    if (str instanceof LString) {
        str = str.toString();
    }
    if (meta) {
        return tokens(str);
    } else {
        var result = tokens(str).map(function(token) {
            // we don't want literal space character to be trimmed
            if (token.token === '#\\ ' || token.token == '#\\\n') {
                return token.token;
            }
            return token.token.trim();
        }).filter(function(token) {
            return token && !token.match(/^;/) && !token.match(/^#\|[\s\S]*\|#$/);
        });
        return strip_s_comments(result);
    }
}
// ----------------------------------------------------------------------
function strip_s_comments(tokens) {
    var s_count = 0;
    var s_start = null;
    var remove_list = [];
    for (let i = 0; i < tokens.length; ++i) {
        const token = tokens[i];
        if (token === '#;') {
            if (['(', '['].includes(tokens[i + 1])) {
                s_count = 1;
                s_start = i;
            } else {
                remove_list.push([i, i + 2]);
            }
            i += 1;
            continue;
        }
        if (s_start !== null) {
            if ([')', ']'].includes(token)) {
                s_count--;
            } else if (['(', '['].includes(token)) {
                s_count++;
            }
            if (s_count === 0) {
                remove_list.push([s_start, i + 1]);
                s_start = null;
            }
        }
    }
    tokens = tokens.slice();
    remove_list.reverse();
    for (const [begin, end] of remove_list) {
        tokens.splice(begin, end - begin);
    }
    return tokens;
}
// ----------------------------------------------------------------------
// Detect if object is ES6 Symbol that work with polyfills
// ----------------------------------------------------------------------
function isSymbol(x) {
    return typeof x === 'symbol' ||
        typeof x === 'object' &&
        Object.prototype.toString.call(x) === '[object Symbol]';
}
// ----------------------------------------------------------------------
// :: LSymbol constructor
// ----------------------------------------------------------------------
function LSymbol(name) {
    if (name instanceof LString) {
        name = name.valueOf();
    }
    if (LSymbol.list[name] instanceof LSymbol) {
        return LSymbol.list[name];
    }
    if (typeof this !== 'undefined' && this.constructor !== LSymbol ||
        typeof this === 'undefined') {
        return new LSymbol(name);
    }
    this.__name__ = name;
    if (typeof name === 'string') {
        LSymbol.list[name] = this;
    }
}
LSymbol.list = {};
LSymbol.literal = Symbol.for('__literal__');
LSymbol.object = Symbol.for('__object__');
// ----------------------------------------------------------------------
LSymbol.is = function(symbol, name) {
    return symbol instanceof LSymbol &&
        ((name instanceof LSymbol && symbol.__name__ === name.__name__) ||
         (typeof name === 'string' && symbol.__name__ === name) ||
         (name instanceof RegExp && name.test(symbol.__name__)));
};
// ----------------------------------------------------------------------
LSymbol.prototype.toString = function(quote) {
    //return '#<symbol \'' + this.name + '\'>';
    if (isSymbol(this.__name__)) {
        return symbol_to_string(this.__name__);
    }
    var str = this.valueOf();
    // those special characters can be normal symbol when printed
    if (quote && str.match(/(^;|[\s()[\]'])/)) {
        return `|${str}|`;
    }
    return str;
};
LSymbol.prototype.literal = function() {
    if (this.is_gensym()) {
        return this[LSymbol.literal];
    }
    return this.valueOf();
};
LSymbol.prototype.serialize = function() {
    if (LString.isString(this.__name__)) {
        return this.__name__;
    }
    return [symbol_to_string(this.__name__)];
};
LSymbol.prototype.valueOf = function() {
    return this.__name__.valueOf();
};
// -------------------------------------------------------------------------
LSymbol.prototype.is_gensym = function() {
    return is_gensym(this.__name__);
};
// -------------------------------------------------------------------------
function symbol_to_string(obj) {
    return obj.toString().replace(/^Symbol\(([^)]+)\)/, '$1');
}
// -------------------------------------------------------------------------
function is_gensym(symbol) {
    if (typeof symbol === 'symbol') {
        return !!symbol.toString().match(/^Symbol\(#:/);
    }
    return false;
}
// -------------------------------------------------------------------------
const gensym = (function() {
    var count = 0;
    function with_props(name, sym) {
        var symbol = new LSymbol(sym);
        hidden_prop(symbol, '__literal__', name);
        return symbol;
    }
    return function(name = null) {
        if (name instanceof LSymbol) {
            if (name.is_gensym()) {
                return name;
            }
            name = name.valueOf();
        }
        if (is_gensym(name)) {
            // don't do double gynsyms in nested syntax-rules
            return LSymbol(name);
        }
        // use ES6 symbol as name for lips symbol (they are unique)
        if (name !== null) {
            return with_props(name, Symbol(`#:${name}`));
        }
        count++;
        return with_props(count, Symbol(`#:g${count}`));
    };
})();
// ----------------------------------------------------------------------
// Class used to escape promises: feature #54
// ----------------------------------------------------------------------
function QuotedPromise(promise) {
    var internal = {
        pending: true,
        rejected: false,
        fulfilled: false,
        reason: undefined,
        type: undefined
    };
    // then added to __promise__ is needed otherwise rejection
    // will give UnhandledPromiseRejectionWarning in Node.js
    promise = promise.then(v => {
        internal.type = type(v);
        internal.fulfilled = true;
        internal.pending = false;
        return v;
    });
    // promise without catch, used for valueOf - for rejecting
    // that should throw an error when used with await
    read_only(this, '_promise', promise, { hidden: true });
    if (is_function(promise.catch)) {
        // prevent exception on unhandled rejecting when using
        // '>(Promise.reject (new Error "zonk")) in REPL
        promise = promise.catch((err) => {
            internal.rejected = true;
            internal.pending = false;
            internal.reason = err;
        });
    }
    Object.keys(internal).forEach(name => {
        Object.defineProperty(this, `__${name}__`, {
            enumerable: true,
            get: () => internal[name]
        });
    });
    read_only(this, '__promise__', promise);
    // prevent resolving when returned from real promise #153
    this.then = false;
}
// ----------------------------------------------------------------------
QuotedPromise.prototype.then = function(fn) {
    return new QuotedPromise(this.valueOf().then(fn));
};
// ----------------------------------------------------------------------
QuotedPromise.prototype.catch = function(fn) {
    return new QuotedPromise(this.valueOf().catch(fn));
};
// ----------------------------------------------------------------------
QuotedPromise.prototype.valueOf = function() {
    if (!this._promise) {
        throw new Error('QuotedPromise: invalid promise created');
    }
    return this._promise;
};
// ----------------------------------------------------------------------
QuotedPromise.prototype.toString = function() {
    if (this.__pending__) {
        return QuotedPromise.pending_str;
    }
    if (this.__rejected__) {
        return QuotedPromise.rejected_str;
    }
    return `#<js-promise resolved (${this.__type__})>`;
};
QuotedPromise.pending_str = '#<js-promise (pending)>';
QuotedPromise.rejected_str = '#<js-promise (rejected)>';
// ----------------------------------------------------------------------
// wrapper over Promise.all that ignore quoted promises
// ----------------------------------------------------------------------
function promise_all(arg) {
    if (Array.isArray(arg)) {
        return Promise.all(escape_quoted_promises(arg))
            .then(unescape_quoted_promises);
    }
    return arg;
}
// ----------------------------------------------------------------------
function escape_quoted_promises(array) {
    // using loops for performance
    var escaped = new Array(array.length), i = array.length;
    while (i--) {
        const value = array[i];
        if (value instanceof QuotedPromise) {
            escaped[i] = new Value(value);
        } else {
            escaped[i] = value;
        }
    }
    return escaped;
}
// ----------------------------------------------------------------------
function unescape_quoted_promises(array) {
    var unescaped = new Array(array.length), i = array.length;
    while (i--) {
        var value = array[i];
        if (value instanceof Value) {
            unescaped[i] = value.valueOf();
        } else {
            unescaped[i] = value;
        }
    }
    return unescaped;
}
// ----------------------------------------------------------------------
// :: Parser macros transformers
// ----------------------------------------------------------------------
var specials = {
    LITERAL: Symbol.for('literal'),
    SPLICE: Symbol.for('splice'),
    SYMBOL: Symbol.for('symbol'),
    names: function() {
        return Object.keys(this.__list__);
    },
    type: function(name) {
        try {
            return this.get(name).type;
        } catch(e) {
            console.log({name});
            console.log(e);
            return null;
        }
    },
    get: function(name) {
        return this.__list__[name];
    },
    // events are used in Lexer dynamic rules
    off: function(name, fn = null) {
        if (Array.isArray(name)) {
            name.forEach(name => this.off(name, fn));
        } else if (fn === null) {
            delete this.__events__[name];
        } else {
            this.__events__ = this.__events__.filter(test => test !== fn);
        }
    },
    on: function(name, fn) {
        if (Array.isArray(name)) {
            name.forEach(name => this.on(name, fn));
        } else if (!this.__events__[name]) {
            this.__events__[name] = [fn];
        } else {
            this.__events__[name].push(fn);
        }
    },
    trigger: function(name, ...args) {
        if (this.__events__[name]) {
            this.__events__[name].forEach(fn => fn(...args));
        }
    },
    remove: function(name) {
        delete this.__list__[name];
        this.trigger('remove');
    },
    append: function(name, value, type) {
        this.__list__[name] = {
            seq: name,
            symbol: value,
            type
        };
        this.trigger('append');
    },
    __events__: {},
    __list__: {}
};
function is_special(token) {
    return specials.names().includes(token);
}
function is_builtin(token) {
    return specials.__builtins__.includes(token);
}
function is_literal(special) {
    return specials.type(special) === specials.LITERAL;
}
function is_symbol_extension(special) {
    return specials.type(special) === specials.SYMBOL;
}
// ----------------------------------------------------------------------
const defined_specials = [
    ["'", new LSymbol('quote'), specials.LITERAL],
    ['`', new LSymbol('quasiquote'), specials.LITERAL],
    [',@', new LSymbol('unquote-splicing'), specials.LITERAL],
    [',', new LSymbol('unquote'), specials.LITERAL],
    ["'>", new LSymbol('quote-promise'), specials.LITERAL]
];

const builtins = defined_specials.map(arr => arr[0]);
Object.freeze(builtins);

Object.defineProperty(specials, '__builtins__', {
    writable: false,
    value: builtins
});
defined_specials.forEach(([seq, symbol, type]) => {
    specials.append(seq, symbol, type);
});
// ----------------------------------------------------------------------
// :: Finite State Machine based incremental Lexer
// ----------------------------------------------------------------------
/* Lexer debugger
   var DEBUG = false;
   function log(...args) {
   if (DEBUG) {
   console.log(...args);
   }
   }
*/
class Lexer {
    constructor(input, { whitespace = false } = {}) {
        read_only(this, '__input__', input.replace(/\r/g, ''));
        var internals = {};
        // hide internals from introspection
        [
            '_i', '_whitespace', '_col', '_newline', '_line',
            '_state', '_next', '_token', '_prev_char'
        ].forEach(name => {
            Object.defineProperty(this, name, {
                configurable: false,
                enumerable: false,
                get() {
                    return internals[name];
                },
                set(value) {
                    internals[name] = value;
                }
            });
        });
        this._whitespace = whitespace;
        this._i = this._line = this._col = this._newline = 0;
        this._state = this._next = this._token = null;
        this._prev_char = '';
    }
    get(name) {
        return this.__internal[name];
    }
    set(name, value) {
        this.__internal[name] = value;
    }
    token(meta = false) {
        if (meta) {
            let line = this._line;
            if (this._whitespace && this._token === '\n') {
                --line;
            }
            return {
                token: this._token,
                col: this._col,
                offset: this._i,
                line
            };
        }
        return this._token;
    }
    peek(meta = false) {
        if (this._i >= this.__input__.length) {
            return eof;
        }
        if (this._token) {
            read_only(this, '__token__', this.token(true));
            return this.token(meta);
        }
        var found = this.next_token();
        if (found) {
            this._token = this.__input__.substring(this._i, this._next);
            return this.token(meta);
        }
        return eof;
    }
    skip() {
        if (this._next !== null) {
            this._token = null;
            this._i = this._next;
        }
    }
    read_line() {
        var len = this.__input__.length;
        if (this._i >= len) {
            return eof;
        }
        for (let i = this._i; i < len; ++i) {
            var char = this.__input__[i];
            if (char === '\n') {
                const line = this.__input__.substring(this._i, i);
                this._i = i + 1;
                ++this._line;
                return line;
            }
        }
        return this.read_rest();
    }
    read_rest() {
        const i = this._i;
        this._i = this.__input__.length;
        return this.__input__.substring(i);
    }
    read_string(num) {
        const len = this.__input__.length;
        if (this._i >= len) {
            return eof;
        }
        if (num + this._i >= len) {
            return this.read_rest();
        }
        const end = this._i + num;
        const result = this.__input__.substring(this._i, end);
        const found = result.match(/\n/g);
        if (found) {
            this._line += found.length;
        }
        this._i = end;
        return result;
    }
    peek_char() {
        if (this._i >= this.__input__.length) {
            return eof;
        }
        return LCharacter(this.__input__[this._i]);
    }
    read_char() {
        const char = this.peek_char();
        this.skip_char();
        return char;
    }
    skip_char() {
        if (this._i < this.__input__.length) {
            ++this._i;
            this._token = null;
        }
    }
    match_rule(rule, { prev_char, char, next_char } = {}) {
        var [ re, prev_re, next_re, state ] = rule;
        if (rule.length !== 5) {
            throw new Error(`Lexer: Invalid rule of length ${rule.length}`);
        }
        if (is_string(re)) {
            if (re !== char) {
                return false;
            }
        } else if (!char.match(re)) {
            return false;
        }
        if (!match_or_null(prev_re, prev_char)) {
            return false;
        }
        if (!match_or_null(next_re, next_char)) {
            return false;
        }
        if (state !== this._state) {
            return false;
        }
        return true;
    }
    next_token() {
        if (this._i >= this.__input__.length) {
            return false;
        }
        let start = true;
        loop:
        for (let i = this._i, len = this.__input__.length; i < len; ++i) {
            const char = this.__input__[i];
            const prev_char = this.__input__[i - 1] || '';
            const next_char = this.__input__[i + 1] || '';
            if (char === '\n') {
                ++this._line;
                const newline = this._newline;
                if (this._state === null) {
                    // keep beginning of the newline to calculate col
                    // we don't want to check inside the token (e.g. strings)
                    this._newline = i + 1;
                }
                if (this._whitespace && this._state === null) {
                    this._next = i + 1;
                    this._col = this._i - newline;
                    return true;
                }
            }
            // skip leading spaces
            if (start && this._state === null && char.match(/\s/)) {
                if (this._whitespace) {
                    if (!next_char.match(/\s/)) {
                        this._next = i + 1;
                        this._col = this._i - this._newline;
                        return true;
                    } else {
                        continue;
                    }
                } else {
                    this._i = i + 1;
                    continue;
                }
            }
            start = false;
            for (let rule of Lexer.rules) {
                if (this.match_rule(rule, { prev_char, char, next_char })) {
                    // change state to null if end of the token
                    var next_state = rule[rule.length - 1];
                    this._state = next_state;
                    if (this._state === null) {
                        this._next = i + 1;
                        this._col = this._i - this._newline;
                        return true;
                    }
                    // token is activated
                    continue loop;
                }
            }
            if (this._state !== null) {
                // collect char in token
                continue loop;
            }
            // no rule for token
            const line = this.__input__.split('\n')[this._line];
            throw new Error(`Invalid Syntax at line ${this._line + 1}\n${line}`);
        }
        // we need to ignore comments because they can be the last expression in code
        // without extra newline at the end
        if (![null, Lexer.comment].includes(this._state)) {
            const line_number = this.__input__.substring(0, this._newline).match(/\n/g)?.length ?? 0;
            const line = this.__input__.substring(this._newline);
            if (this.__input__[this._i] === '#') {
                const expr = this.__input__.substring(this._i).replace(/^([^\s()\[\]]+).*/, '$1');
                throw new Error(`Invalid Syntax at line ${line_number + 1}: invalid token ${expr}`);
            }
            throw new Unterminated(`Invalid Syntax at line ${line_number + 1}: Unterminated expression ${line}`);
        }
    }
}
// ----------------------------------------------------------------------
// TODO: cache the rules creation or whole list
// ----------------------------------------------------------------------
// State rule for literal symbol
// ----------------------------------------------------------------------
Lexer.literal_rule = function literal_rule(string, symbol, p_re = null, n_re = null) {
    if (string.length === 0) {
        throw new Error('Lexer: invalid literal rule');
    }
    if (string.length === 1) {
        return [[string, p_re, n_re, null, null]];
    }
    var rules = [];
    for (let i = 0, len = string.length; i < len; ++i) {
        const rule = [];
        rule.push(string[i]);
        rule.push(string[i - 1] || p_re);
        rule.push(string[i + 1] || n_re);
        if (i === 0) {
            rule.push(null);
            rule.push(symbol);
        } else if (i === len - 1) {
            rule.push(symbol);
            rule.push(null);
        } else {
            rule.push(symbol);
            rule.push(symbol);
        }
        rules.push(rule);
    }
    return rules;
};
// ----------------------------------------------------------------------
Lexer.string = Symbol.for('string');
Lexer.string_escape = Symbol.for('string_escape');
Lexer.symbol = Symbol.for('symbol');
Lexer.comment = Symbol.for('comment');
Lexer.regex = Symbol.for('regex');
Lexer.regex_init = Symbol.for('regex_init');
Lexer.regex_class = Symbol.for('regex_class');
Lexer.character = Symbol.for('character');
Lexer.bracket = Symbol.for('bracket');
Lexer.b_symbol = Symbol.for('b_symbol');
Lexer.b_comment = Symbol.for('b_comment');
Lexer.i_comment = Symbol.for('i_comment');
Lexer.l_datum = Symbol.for('l_datum');
Lexer.dot = Symbol.for('dot');
// ----------------------------------------------------------------------
Lexer.boundary = /^$|[\s()[\]']/;
// ----------------------------------------------------------------------
Lexer._rules = [
    // char_re prev_re next_re from_state to_state
    // null as to_state mean that is single char token
    // string
    [/"/, null, null, Lexer.string, null],
    [/"/, null, null, null, Lexer.string],
    [/"/, null, null, Lexer.string_escape, Lexer.string],
    [/\\/, null, null, Lexer.string, Lexer.string_escape],
    [/./, /\\/, null, Lexer.string_escape, Lexer.string],

    // hash special symbols, lexer don't need to distinguish those
    // we only care if it's not pick up by vectors literals
    [/#/, null, /[bdxoei]/i, null, Lexer.symbol],

    // characters
    [/#/, null, /\\/, null, Lexer.character],
    [/\\/, /#/, /\s/, Lexer.character, Lexer.character],
    [/\\/, /#/, /[()[\]]/, Lexer.character, Lexer.character],
    [/\s/, /\\/, null, Lexer.character, null],
    [/\S/, null, Lexer.boundary, Lexer.character, null],

    // regex
    [/#/, Lexer.boundary, /\//, null, Lexer.regex_init],
    [/./, /\//, null, Lexer.regex_init, Lexer.regex],
    [/[ \t]/, null, null, Lexer.regex, Lexer.regex],
    [/\[/, /[^\\]/, null, Lexer.regex, Lexer.regex_class],
    [/\]/, /[^\\]/, null, Lexer.regex_class, Lexer.regex],
    [/[()[\]]/, null, null, Lexer.regex, Lexer.regex],
    [/\//, /\\/, null, Lexer.regex, Lexer.regex],
    [/\//, null, Lexer.boundary, Lexer.regex, null],
    [/[gimyus]/, /\//, Lexer.boundary, Lexer.regex, null],
    [/[gimyus]/, /\//, /[gimyus]/, Lexer.regex, Lexer.regex],
    [/[gimyus]/, /[gimyus]/, Lexer.boundary, Lexer.regex, null],

    // comment
    [/;/, /^$|[^#]/, null, null, Lexer.comment],
    [/\n/, ';', null, Lexer.comment, null],
    [/[\s\S]/, null, /\n/, Lexer.comment, null],
    [/\s/, null, null, Lexer.comment, Lexer.comment],

    // block comment
    [/#/, null, /\|/, null, Lexer.b_comment],
    [/\s/, null, null, Lexer.b_comment, Lexer.b_comment],
    [/#/, /\|/, null, Lexer.b_comment, null],

    // inline commentss
    [/#/, null, /;/, null, Lexer.i_comment],
    [/;/, /#/, null, Lexer.i_comment, null],

    // datum label
    [/#/, null, /[0-9]/, null, Lexer.l_datum],
    [/=/, /[0-9]/, null, Lexer.l_datum, null],
    [/#/, /[0-9]/, null, Lexer.l_datum, null],

    // for dot comma `(a .,b)
    [/\./, Lexer.boundary, /,/, null, null],

    // block symbols
    [/\|/, null, null, null, Lexer.b_symbol],
    [/\s/, null, null, Lexer.b_symbol, Lexer.b_symbol],
    [/\|/, null, Lexer.boundary, Lexer.b_symbol, null]
];
// ----------------------------------------------------------------------
Lexer._brackets = [
    [/[()[\]]/, null, null, null, null]
];
// ----------------------------------------------------------------------
// :: symbols should be matched last
// ----------------------------------------------------------------------
Lexer._symbol_rules = [
    [/\S/, Lexer.boundary, Lexer.boundary, null, null],
    [/\S/, Lexer.boundary, null, null, Lexer.symbol],
    [/\S/, null, Lexer.boundary, null, null],
    [/\S/, null, null, null, Lexer.symbol],
    [/\S/, null, Lexer.boundary, Lexer.symbol, null]
];
// ----------------------------------------------------------------------
// :: Dynamic getter or Lexer state rules, parser uses this
// :: so user code can modify Lexer using syntax extensions
// ----------------------------------------------------------------------
Lexer._cache = {
    valid: false,
    rules: null
};
// ----------------------------------------------------------------------
specials.on(['remove', 'append'], function() {
    Lexer._cache.valid = false;
    Lexer._cache.rules = null;
});
// those constants need to be add as rules to the Lexer to work with vector literals
const parsable_contants = {
    '#null': null,
    '#void': undefined
};
const directives = [
    '#!fold-case',
    '#!no-fold-case'
];
const hash_literals = ['#t', '#f'];
// ----------------------------------------------------------------------
Object.defineProperty(Lexer, 'rules', {
    get() {
        if (Lexer._cache.valid) {
            return Lexer._cache.rules;
        }
        const parsable = Object.keys(parsable_contants).concat(directives, hash_literals);
        const tokens = specials.names().concat(parsable).sort((a, b) => {
            return b.length - a.length || a.localeCompare(b);
        });

        // syntax-extensions tokens that share the same first character after hash
        // should have same symbol, but because tokens are sorted, the longer
        // tokens are always process first.
        const special_rules = tokens.reduce((acc, token) => {
            let symbol;
            let after = null;
            if (token[0] === '#') {
                if (token.length === 1) {
                    symbol = Symbol.for(token);
                } else {
                    if (hash_literals.includes(token)) {
                        after = Lexer.boundary;
                    }
                    symbol = Symbol.for(token[1]);
                }
            } else {
                symbol = Symbol.for(token);
            }
            const rules = Lexer.literal_rule(token, symbol, null, after);
            return acc.concat(rules);
        }, []);

        Lexer._cache.rules = Lexer._rules.concat(
            Lexer._brackets,
            special_rules,
            Lexer._symbol_rules
        );

        Lexer._cache.valid = true;
        return Lexer._cache.rules;
    }
});
// ----------------------------------------------------------------------
function match_or_null(re, char) {
    return re === null || char.match(re);
}
// ----------------------------------------------------------------------
// :: Parser inspired by BiwaScheme
// :: ref: https://github.com/biwascheme/biwascheme/blob/master/src/system/parser.js
// ----------------------------------------------------------------------
class Parser {
    constructor({ env, meta = false, formatter = multiline_formatter } = {}) {
        read_only(this, '_formatter', formatter, { hidden: true });
        read_only(this, '__env__', env);
        read_only(this, '_meta', meta, { hidden: true });
        // datum labels
        read_only(this, '_refs', [], { hidden: true });
        read_only(this, '_state', {
            parentheses: 0,
            fold_case: false
        }, { hidden: true });
    }
    _with_syntax_scope(fn) {
        // expose parser and change stdin so parser extension can use current-input
        // to read data from the parser stream #150
        const internal = get_internal(this.__env__);
        const stdin = internal.get('stdin');
        this.__env__.set('lips',  { ...lips, __parser__: this });
        internal.set('stdin', new ParserInputPort(this, this.__env__));
        const cleanup = () => {
            this.__env__.set('lips', lips);
            internal.set('stdin', stdin);
        }
        return unpromise(fn(), (result) => {
            cleanup();
            return result;
        }, cleanup);
    }
    parse(arg) {
        if (arg instanceof LString) {
            arg = arg.toString();
        }
        read_only(this, '__lexer__', new Lexer(arg));
    }
    resolve(name) {
        return this.__env__ && this.__env__.get(name, { throwError: false });
    }
    async peek() {
        let token;
        while (true) {
            token = this.__lexer__.peek(true);
            if (token === eof) {
                return eof;
            }
            if (this.is_comment(token.token)) {
                this.skip();
                continue;
            }
            if (is_directive(token.token)) {
                this.skip();
                if (token.token === '#!fold-case') {
                    this._state.fold_case = true;
                } else if (token.token === '#!no-fold-case') {
                    this._state.fold_case = false;
                }
                continue;
            }
            if (token.token === '#;') {
                this.skip();
                if (this.__lexer__.peek() === eof) {
                    throw new Error('Lexer: syntax error eof found after comment');
                }
                await this._read_object();
                continue;
            }
            break;
        }
        token = this._formatter(token);
        if (this._state.fold_case) {
            token.token = foldcase_string(token.token);
        }
        if (this._meta) {
            return token;
        }
        return token.token;
    }
    reset() {
        this._refs.length = 0;
    }
    skip() {
        this.__lexer__.skip();
    }
    async read() {
        const token = await this.peek();
        this.skip();
        return token;
    }
    match_datum_label(token) {
        var m = token.match(/^#([0-9]+)=$/);
        return m && m[1];
    }
    match_datum_ref(token) {
        var m = token.match(/^#([0-9]+)#$/);
        return m && m[1];
    }
    is_open(token) {
        return ['(', '['].includes(token);
    }
    is_close(token) {
        return [')', ']'].includes(token);
    }
    async read_list() {
        let head = nil, prev = head, dot;
        while (true) {
            const token = await this.peek();
            if (token === eof) {
                break;
            }
            if (this.is_close(token)) {
                --this._state.parentheses;
                this.skip();
                break;
            }
            if (token === '.' && !is_nil(head)) {
                this.skip();
                prev.cdr = await this._read_object();
                dot = true;
            } else if (dot) {
                throw new Error('Parser: syntax error more than one element after dot');
            } else {
                const node = await this._read_object();
                const cur = new Pair(node, nil);
                if (is_nil(head)) {
                    head = cur;
                } else {
                    prev.cdr = cur;
                }
                prev = cur;
            }
        }
        return head;
    }
    async read_value() {
        let token = await this.read();
        if (token === eof) {
            throw new Error('Parser: Expected token eof found');
        }
        return parse_argument(token);
    }
    is_comment(token) {
        return token.match(/^;/) || (token.match(/^#\|/) && token.match(/\|#$/));
    }
    evaluate(code) {
        return evaluate(code, { env: this.__env__, error: (e) => {
            throw e;
        } });
    }
    // public API that handle R7RS datum labels
    async read_object() {
        this.reset();
        var object = await this._read_object();
        if (object instanceof DatumReference) {
            object = object.valueOf();
        }
        if (this._refs.length) {
            return unpromise(this._resolve_object(object), object => {
                if (is_pair(object)) {
                    // mark cycles on parser level
                    object.mark_cycles();
                }
                return object;
            });
        }
        return object;
    }
    balanced() {
        return this._state.parentheses === 0;
    }
    ballancing_error(expr, prev) {
        const count = this._state.parentheses;
        let e;
        if (count < 0) {
            e = new Error('Parser: unexpected parenthesis');
            e.__code__ = [prev.toString() + ')'];
        } else {
            e = new Error('Parser: expected parenthesis but eof found');
            const re = new RegExp(`\\){${count}}$`);
            e.__code__ = [expr.toString().replace(re, '')];
        }
        throw e;
    }
    // TODO: Cover This function (array and object branch)
    async _resolve_object(object) {

        if (Array.isArray(object)) {
            return object.map(item => this._resolve_object(item));
        }
        if (is_plain_object(object)) {
            var result = {};
            Object.keys(object).forEach(key => {
                result[key] = this._resolve_object(object[key]);
            });
            return result;
        }
        if (is_pair(object)) {
            return this._resolve_pair(object);
        }
        return object;
    }
    async _resolve_pair(pair) {
        if (is_pair(pair)) {
            if (pair.car instanceof DatumReference) {
                pair.car = await pair.car.valueOf();
            } else {
                this._resolve_pair(pair.car);
            }
            if (pair.cdr instanceof DatumReference) {
                pair.cdr = await pair.cdr.valueOf();
            } else {
                this._resolve_pair(pair.cdr);
            }
        }
        return pair;
    }
    async _read_object() {
        const token = await this.peek();
        if (token === eof) {
            return token;
        }
        if (is_special(token)) {
            // Built-in parser extensions are mapping short symbols to longer symbols
            // that can be function or macro. Parser doesn't care
            // if it's not built-in and the extension can be macro or function.
            // FUNCTION: when it's used, it gets arguments like FEXPR and the
            // result is returned by parser as is the macro.
            // MACRO: if macro is used, then it is evaluated in place and the
            // result is returned by parser and it is quoted.
            const special = specials.get(token);
            const builtin = is_builtin(token);
            this.skip();
            let expr, extension;
            const is_symbol = is_symbol_extension(token);
            const was_close_paren = this.is_close(await this.peek());
            const object = is_symbol ? undefined : await this._read_object();
            if (object === eof) {
                throw new Unterminated('Expecting expression eof found');
            }
            if (!builtin) {
                extension = this.__env__.get(special.symbol);
                if (typeof extension === 'function') {
                    let args;
                    if (is_literal(token)) {
                        args = [object];
                    } else if (is_nil(object)) {
                        args = [];
                    } else if (is_pair(object)) {
                        args = object.to_array(false);
                    }
                    if (args || is_symbol) {
                        return this._with_syntax_scope(() => {
                            return call_function(extension, is_symbol ? [] : args, {
                                env: this.__env__,
                                dynamic_env: this.__env__,
                                use_dynamic: false
                            });
                        });
                    }
                    throw new Error('Parse Error: Invalid parser extension ' +
                                    `invocation ${special.symbol}`);
                }
            }
            if (is_literal(token)) {
                if (was_close_paren) {
                    throw new Error('Parse Error: expecting datum');
                }
                expr = new Pair(
                    special.symbol,
                    new Pair(
                        object,
                        nil
                    )
                );
            } else {
                expr = new Pair(
                    special.symbol,
                    object
                );
            }
            // Built-in parser extensions just expand into lists like 'x ==> (quote x)
            if (builtin) {
                return expr;
            }
            // Evaluate parser extension at parse time
            if (extension instanceof Macro) {
                var result = await this._with_syntax_scope(() => {
                    return this.evaluate(expr);
                });
                // We need literal quotes to make that macro's return pairs works
                // because after the parser returns the value it will be evaluated again
                // by the interpreter, so we create quoted expressions.
                if (is_pair(result) || result instanceof LSymbol) {
                    return Pair.fromArray([LSymbol('quote'), result]);
                }
                return result;
            } else {
                throw new Error('Parse Error: invalid parser extension: ' +
                                special.symbol);
            }
        }
        var ref = this.match_datum_ref(token);
        if (ref !== null) {
            this.skip();
            if (this._refs[ref]) {
                return new DatumReference(ref, this._refs[ref]);
            }
            throw new Error(`Parse Error: invalid datum label #${ref}#`);
        }
        var ref_label = this.match_datum_label(token);
        if (ref_label !== null) {
            this.skip();
            this._refs[ref_label] = this._read_object();
            return this._refs[ref_label];
        } else if (this.is_close(token)) {
            --this._state.parentheses;
            this.skip();
            // invalid state, we don't need to return anything
        } else if (this.is_open(token)) {
            ++this._state.parentheses;
            this.skip();
            return this.read_list();
        } else {
            return this.read_value();
        }
    }
}
class Unterminated extends Error { }
Parser.Unterminated = Unterminated;
// ----------------------------------------------------------------------
// :: Parser helper that handles circular list structures
// :: using datum labels
// ----------------------------------------------------------------------
class DatumReference {
    constructor(name, data) {
        this.name = name;
        this.data = data;
    }
    valueOf() {
        return this.data;
    }
}
// ----------------------------------------------------------------------
// :: Tokens are the array of strings from tokenizer
// :: the return value is an array of lips code created out of Pair class.
// :: env is needed for parser extensions that will invoke the function
// :: or macro assigned to symbol, this function is async because
// :: it evaluates the code, from parser extensions, that may return a promise.
// ----------------------------------------------------------------------
async function* _parse(arg, env) {
    if (!env) {
        if (global_env) {
            env = global_env.get('**interaction-environment**', {
                throwError: false
            });
        } else {
            env = user_env;
        }
    }
    let parser;
    if (arg instanceof Parser) {
        parser = arg;
    } else {
        parser = new Parser({ env });
        parser.parse(arg);
    }
    let prev;
    while (true) {
        const expr = await parser.read_object();
        if (!parser.balanced()) {
            parser.ballancing_error(expr, prev);
        }
        if (expr === eof) {
            break;
        }
        prev = expr;
        yield expr;
    }
}
// ----------------------------------------------------------------------
function unpromise(value, fn = x => x, error = null) {
    if (is_promise(value)) {
        var ret = value.then(fn);
        if (error === null) {
            return ret;
        } else {
            return ret.catch(error);
        }
    }
    if (value instanceof Array) {
        return unpromise_array(value, fn, error);
    }
    if (is_plain_object(value)) {
        return unpromise_object(value, fn, error);
    }
    return fn(value);
}
// ----------------------------------------------------------------------
function unpromise_array(array, fn, error) {
    if (array.find(is_promise)) {
        return unpromise(promise_all(array), (arr) => {
            if (Object.isFrozen(array)) {
                Object.freeze(arr);
            }
            return fn(arr);
        }, error);
    }
    return fn(array);
}
// ----------------------------------------------------------------------
function unpromise_object(object, fn, error) {
    const keys = Object.keys(object);
    const values = [], anyPromise = [];
    let i = keys.length;
    while (i--) {
        const key = keys[i];
        const value = object[key];
        values[i] = value;
        if (is_promise(value)) {
            anyPromise.push(value);
        }
    }
    if (anyPromise.length) {
        return unpromise(promise_all(values), (values) => {
            const result = {};
            values.forEach((value, i) => {
                const key = keys[i];
                result[key] = value;
            });
            if (Object.isFrozen(object)) {
                Object.freeze(result);
            }
            return result;
        }, error);
    }
    return fn(object);
}
// ----------------------------------------------------------------------
function read_only(object, property, value, { hidden = false } = {}) {
    Object.defineProperty(object, property, {
        value,
        configurable: true,
        enumerable: !hidden
    });
}
// ----------------------------------------------------------------------
// :: Function similar to Array.from that work on async iterators
// ----------------------------------------------------------------------
async function uniterate_async(object) {
    const result = [];
    for await (let item of object) {
        result.push(item);
    }
    return result;
}
// ----------------------------------------------------------------------
// :: Function that return matcher function that match string against string
// ----------------------------------------------------------------------
function matcher(name, arg) {
    if (arg instanceof RegExp) {
        return x => String(x).match(arg);
    } else if (is_function(arg)) {
        // it will always be function
        return arg;
    }
    throw new Error('Invalid matcher');
}
// ----------------------------------------------------------------------
// :: Documentation decorator to LIPS functions if lines starts with :
// :: they are ignored (not trimmed) otherwise it trims so
// :: so you can have indent in source code
// ----------------------------------------------------------------------
function doc(name, fn, doc, dump) {
    if (typeof name !== 'string') {
        fn = arguments[0];
        doc = arguments[1];
        dump = arguments[2];
        name = null;
    }
    if (doc) {
        if (dump) {
            fn.__doc__ = doc;
        } else {
            fn.__doc__ = trim_lines(doc);
        }
    }
    if (name) {
        fn.__name__ = name;
    } else if (fn.name && !is_lambda(fn)) {
        fn.__name__ = fn.name;
    }
    return fn;
}
// ----------------------------------------------------------------------
function trim_lines(string) {
    return string.split('\n').map(line => {
        return line.trim();
    }).join('\n');
}
// ----------------------------------------------------------------------
// return last S-Expression
// @param tokens - array of tokens (objects from tokenizer or strings)
// @param sexp - number of expression to look behind
// ----------------------------------------------------------------------
function previousSexp(tokens, sexp = 1) {
    var i = tokens.length;
    if (sexp <= 0) {
        throw Error(`previousSexp: Invalid argument sexp = ${sexp}`);
    }
    outer: while (sexp-- && i >= 0) {
        var count = 1;
        while (count > 0) {
            var token = tokens[--i];
            if (!token) {
                break outer;
            }
            if (token === '(' || token.token === '(') {
                count--;
            } else if (token === ')' || token.token === ')') {
                count++;
            }
        }
        i--;
    }
    return tokens.slice(i + 1);
}
// ----------------------------------------------------------------------
// :: Find the number of spaces in line
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
// :: Token based pattern matching (used by formatter)
// ----------------------------------------------------------------------
/*
  Function nested_pattern(pattern) {
  return pattern instanceof Array ||
  pattern instanceof Pattern;
  }
*/
// ----------------------------------------------------------------------
function match(pattern, input) {
    return inner_match(pattern, input) === input.length;
    function inner_match(pattern, input) {
        /*
          function empty_match() {
          if (p <= 0 && i <= 0) {
          return false;
          }
          var prev_pattern = pattern[p - 1];
          if (!nested_pattern(prev_pattern)) {
          prev_pattern = [prev_pattern];
          }
          var next_pattern = pattern[p + 1];
          if (next_pattern && !nested_pattern(next_pattern)) {
          next_pattern = [next_pattern];
          }
          return match(prev_pattern, [input[i - 1]]) &&
          (!next_pattern || match(next_pattern, [input[i]]));
          }
        */
        function get_first_match(patterns, input) {
            for (let p of patterns) {
                const m = inner_match(p, input);
                if (m !== -1) {
                    return m;
                }
            }
            return -1;
        }
        function not_symbol_match() {
            return pattern[p] === Symbol.for('symbol') && !is_symbol_string(input[i]);
        }
        function match_next() {
            var next_pattern = pattern[p + 1];
            var next_input = input[i + 1];
            if (next_pattern !== undefined && next_input !== undefined) {
                return inner_match([next_pattern], [next_input]);
            }
        }
        var p = 0;
        var glob = {};
        for (var i = 0; i < input.length; ++i) {
            if (typeof pattern[p] === 'undefined') {
                return i;
            }
            if (pattern[p] instanceof Pattern) {
                var m;
                if (['+', '*'].includes(pattern[p].flag)) {
                    while (i < input.length) {
                        m = get_first_match(pattern[p].patterns, input.slice(i));
                        if (m === -1) {
                            break;
                        }
                        i += m;
                    }
                    i -= 1;
                    p++;
                    continue;
                } else if (pattern[p].flag === '?') {
                    m = get_first_match(pattern[p].patterns, input.slice(i));
                    if (m === -1) {
                        i -= 2; // if not found use same test on same input again
                    } else {
                        p++;
                    }
                    continue;
                }
            } else if (pattern[p] instanceof RegExp) {
                if (!input[i].match(pattern[p])) {
                    return -1;
                }
            } else if (lips.LString.isString(pattern[p])) {
                if (pattern[p].valueOf() !== input[i]) {
                    return -1;
                }
            } else if (typeof pattern[p] === 'symbol') {
                if (pattern[p] === Symbol.for('*')) {
                    // ignore S-expressions inside for case when next pattern is )
                    glob[p] = glob[p] || 0;
                    //var zero_match = empty_match();
                    if (['(', '['].includes(input[i])) {
                        glob[p]++;
                    } else if ([')', ']'].includes(input[i])) {
                        glob[p]--;
                    }
                    if ((typeof pattern[p + 1] !== 'undefined' &&
                         glob[p] === 0 && match_next() === -1) ||
                        glob[p] > 0) {
                        continue;
                    }
                } else if (not_symbol_match()) {
                    return -1;
                }
            } else if (pattern[p] instanceof Array) {
                var inc = inner_match(pattern[p], input.slice(i));
                if (inc === -1 || inc + i > input.length) {
                    // if no more input it's not match
                    return -1;
                }
                i += inc - 1;
                p++;
                continue;
            } else {
                return -1;
            }
            p++;
        }
        if (pattern.length !== p) {
            // if there are still patterns it's not match
            return -1;
        }
        return input.length;
    }
}
// ----------------------------------------------------------------------
// :: Code formatter class
// :: based on http://community.schemewiki.org/?scheme-style
// :: and GNU Emacs scheme mode
// :: it rely on meta data from tokenizer function
// ----------------------------------------------------------------------
function Formatter(code) {
    this.__code__ = code.replace(/\r/g, '');
}
// ----------------------------------------------------------------------
Formatter.defaults = {
    offset: 0,
    indent: 2,
    exceptions: {
        specials: [
            /* eslint-disable max-len */
            /^(?:#:)?(?:define(?:-values|-syntax|-macro|-class|-record-type)?|(?:call-with-(?:input-file|output-file|port))|lambda|let-env|try|catch|when|unless|while|syntax-rules|(let|letrec)(-syntax|\*?-values|\*)?)$/
            /* eslint-enable */
        ],
        shift: {
            1: ['&', '#']
        }
    }
};
Formatter.match = match;
// ----------------------------------------------------------------------
// :: Return indent for next line
// ----------------------------------------------------------------------
Formatter.prototype._options = function _options(options) {
    var defaults = Formatter.defaults;
    if (typeof options === 'undefined') {
        return Object.assign({}, defaults);
    }
    var exceptions = options && options.exceptions || {};
    var specials = exceptions.specials || [];
    var shift = exceptions.shift || { 1: [] };
    return {
        ...defaults,
        ...options,
        exceptions: {
            specials: [...defaults.exceptions.specials, ...specials],
            shift: {
                ...shift,
                1: [...defaults.exceptions.shift[1], ...shift[1]]
            }
        }
    };
};
// ----------------------------------------------------------------------
Formatter.prototype.indent = function indent(options) {
    var tokens = tokenize(this.__code__, true);
    return this._indent(tokens, options);
};
// ----------------------------------------------------------------------
Formatter.exception_shift = function(token, settings) {
    function match(list) {
        if (!list.length) {
            return false;
        }
        if (list.indexOf(token) !== -1) {
            return true;
        } else {
            var regexes = list.filter(s => s instanceof RegExp);
            if (!regexes.length) {
                return false;
            }
            for (let re of regexes) {
                if (token.match(re)) {
                    return true;
                }
            }
        }
        return false;
    }
    if (match(settings.exceptions.specials)) {
        return settings.indent;
    }
    var shift = settings.exceptions.shift;
    for (var [indent, tokens] of Object.entries(shift)) {
        if (match(tokens)) {
            return +indent;
        }
    }
    return -1;
};
// ----------------------------------------------------------------------
Formatter.prototype._indent = function _indent(tokens, options) {
    var settings = this._options(options);
    var spaces = lineIndent(tokens);
    var sexp = previousSexp(tokens);
    // one character before S-Expression
    var before_sexpr = tokens[tokens.length - sexp.length - 1];
    var last = tokens[tokens.length - 1];
    if (last.token.match(/^"[\S\s]+[^"]$/)) {
        return spaces + settings.indent;
    }
    if (sexp && sexp.length) {
        if (sexp[0].line > 0) {
            settings.offset = 0;
        }
        if (sexp.toString() === tokens.toString() && balanced(sexp)) {
            return settings.offset + sexp[0].col;
        } else if (sexp.length === 1) {
            return settings.offset + sexp[0].col + 1;
        } else {
            // search for token before S-Expression for case like #(10 or &(:x
            var exception = -1;
            if (before_sexpr) {
                var shift = Formatter.exception_shift(before_sexpr.token, settings);
                if (shift !== -1) {
                    exception = shift;
                }
            }
            if (exception === -1) {
                exception = Formatter.exception_shift(sexp[1].token, settings);
            }
            if (exception !== -1) {
                return settings.offset + sexp[0].col + exception;
            } else if (sexp[0].line < sexp[1].line) {
                return settings.offset + sexp[0].col + 1;
            } else if (sexp.length > 3 && sexp[1].line === sexp[3].line) {
                if (sexp[1].token === '(' || sexp[1].token === '[') {
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
    } else {
        return 0;
    }
    return spaces + settings.indent;
};
// ----------------------------------------------------------------------
function Ahead(pattern) {
    this.pattern = pattern;
}
// TODO: make it print
Ahead.prototype.toString = function() {
    return `#<pattern(${this.pattern})>`;
};
// ----------------------------------------------------------------------
Ahead.prototype.match = function(string) {
    return string.match(this.pattern);
};
// ----------------------------------------------------------------------
// Pattern has any number of patterns that it matches using OR operator
// Pattern is in form of array with regular expressions
// ----------------------------------------------------------------------
function Pattern(...args) {
    var flag = args.pop();
    this.patterns = args;
    this.flag = flag;
}
Pattern.prototype.toString = function() {
    var patterns = this.patterns.map(x => toString(x)).join('|');
    return `#<pattern(${patterns} ${this.flag})>`;
};
// ----------------------------------------------------------------------
Formatter.Pattern = Pattern;
Formatter.Ahead = Ahead;
var p_o = /^[[(]$/;
var p_e = /^[\])]$/;
var not_p = /[^()[\]]/;
const not_close = new Ahead(/[^)\]]/);
//const open = new Ahead(/[([]/);
const glob = Symbol.for('*');
const sexp_or_atom = new Pattern([p_o, glob, p_e], [not_p], '+');
const sexp = new Pattern([p_o, glob, p_e], '+');
const symbol = new Pattern([Symbol.for('symbol')], '?');
const symbols = new Pattern([Symbol.for('symbol')], '*');
const identifiers = [p_o, symbols, p_e];
const let_value = new Pattern([p_o, Symbol.for('symbol'), glob, p_e], '+');
const syntax_rules = keywords_re('syntax-rules');
// rules for breaking S-Expressions into lines
var def_lambda_re = keywords_re('define', 'lambda', 'define-macro', 'syntax-rules');
/* eslint-disable max-len */
var non_def = /^(?!.*\b(?:[()[\]]|define(?:-macro)?|let(?:\*|rec|-env|-syntax|)?|lambda|syntax-rules)\b).*$/;
/* eslint-enable */
var let_re = /^(?:#:)?(let(?:\*|rec|-env|-syntax)?)$/;
// match keyword if it's normal token or gensym (prefixed with #:)
function keywords_re(...args) {
    return new RegExp(`^(?:#:)?(?:${args.join('|')})$`);
}
// line breaking rules
Formatter.rules = [
    [[sexp], 0, not_close],
    [[p_o, keywords_re('begin', 'cond-expand')], 1],
    [[p_o, let_re, symbol, p_o, let_value, p_e], 1],
    [[p_o, let_re, symbol, sexp_or_atom], 1, not_close],
    [[p_o, let_re, p_o, let_value], 1, not_close],
    [[p_o, keywords_re('define-syntax'), /.+/], 1],
    [[p_o, syntax_rules, not_p, identifiers], 1],
    [[p_o, syntax_rules, not_p, identifiers, sexp], 1, not_close],
    [[p_o, syntax_rules, identifiers], 1],
    [[p_o, syntax_rules, identifiers, sexp], 1, not_close],
    [[p_o, non_def, new Pattern([/[^()[\]]/], '+'), sexp], 1, not_close],
    [[p_o, sexp], 1, not_close],
    [[p_o, not_p, sexp], 1, not_close],
    [[p_o, keywords_re('lambda', 'if'), not_p], 1, not_close],
    [[p_o, keywords_re('while'), not_p, sexp], 1, not_close],
    [[p_o, keywords_re('if'), not_p, glob], 1],
    [[p_o, def_lambda_re, identifiers], 0, not_close],
    [[p_o, def_lambda_re, identifiers, string_re], 0, not_close],
    [[p_o, def_lambda_re, identifiers, string_re, sexp], 0, not_close],
    [[p_o, def_lambda_re, identifiers, sexp], 0, not_close]
];
// ----------------------------------------------------------------------
Formatter.prototype.break = function() {
    var code = this.__code__.replace(/\n[ \t]*/g, '\n ').replace(/^\s+/, '');
    // function that work when calling tokenize with meta data or not
    const token = t => {
        if (t.token.match(string_re) || t.token.match(re_re)) {
            return t.token;
        } else {
            return t.token.replace(/\s+/, ' ');
        }
    };
    const first_token_index = tokens => {
        for (let i = tokens.length; i--;) {
            const token = tokens[i];
            if (token.trim() && !is_special(token)) {
                return tokens.length - i - 1;
            }
        }
    };
    // Tokenize is part of the parser/lexer that split code into tokens and includes
    // meta data like number of column or line
    var tokens = tokenize(code, true).map(token).filter(t => t !== '\n');
    const { rules } = Formatter;
    outer: for (let i = 1; i < tokens.length; ++i) {
        if (!tokens[i].trim()) {
            continue;
        }
        var sub = tokens.slice(0, i);
        var sexp = {};
        rules.map(b => b[1]).forEach(count => {
            count = count.valueOf();
            // some patterns require to check what was before like
            // if inside let binding
            if (count > 0 && !sexp[count]) {
                sexp[count] = previousSexp(sub, count);
            }
        });
        for (let [pattern, count, ext] of rules) {
            count = count.valueOf();
            // 0 count mean ignore the previous S-Expression
            var test_sexp = count > 0 ? sexp[count] : sub;
            const input = test_sexp.filter(t => t.trim() && !is_special(t));
            const inc = first_token_index(test_sexp);
            var m = match(pattern, input);
            var next = tokens.slice(i).find(t => t.trim() && !is_special(t));
            if (m && (ext instanceof Ahead && ext.match(next) || !ext)) {
                const index = i - inc;
                if (tokens[index] !== '\n') {
                    if (!tokens[index].trim()) {
                        tokens[index] = '\n';
                    } else {
                        tokens.splice(index, 0, '\n');
                        i++;
                    }
                }
                i += inc;
                continue outer;
            }
        }
    }
    this.__code__ = tokens.join('');
    return this;
};
// ----------------------------------------------------------------------
Formatter.prototype._spaces = function(i) {
    return ' '.repeat(i);
};
// ----------------------------------------------------------------------
// :: Auto formatting of code, it requires to have newlines
// ----------------------------------------------------------------------
Formatter.prototype.format = function format(options) {
    // prepare code with single space after newline
    // so we have space token to align
    var code = this.__code__.replace(/[ \t]*\n[ \t]*/g, '\n ');
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
    return tokens.map(token => {
        if (token.token.match(string_re)) {
            if (token.token.match(/\n/)) {
                var spaces = ' '.repeat(token.col);
                var lines = token.token.split('\n');
                token.token = [lines[0]].concat(lines.slice(1).map(line => {
                    return spaces + line;
                })).join('\n');
            }
        }
        return token.token;
    }).join('');
};
// ----------------------------------------------------------------------
// :: the Unicode folding case mapping generated scripts/fold.scm to get
// :: latest version of the file use `make fold`
// ----------------------------------------------------------------------
const fold_case_mapping = {
    "65": 97, "66": 98, "67": 99, "68": 100,
    "69": 101, "70": 102, "71": 103, "72": 104,
    "73": 105, "74": 106, "75": 107, "76": 108,
    "77": 109, "78": 110, "79": 111, "80": 112,
    "81": 113, "82": 114, "83": 115, "84": 116,
    "85": 117, "86": 118, "87": 119, "88": 120,
    "89": 121, "90": 122, "181": 956, "192": 224,
    "193": 225, "194": 226, "195": 227, "196": 228,
    "197": 229, "198": 230, "199": 231, "200": 232,
    "201": 233, "202": 234, "203": 235, "204": 236,
    "205": 237, "206": 238, "207": 239, "208": 240,
    "209": 241, "210": 242, "211": 243, "212": 244,
    "213": 245, "214": 246, "216": 248, "217": 249,
    "218": 250, "219": 251, "220": 252, "221": 253,
    "222": 254, "256": 257, "258": 259, "260": 261,
    "262": 263, "264": 265, "266": 267, "268": 269,
    "270": 271, "272": 273, "274": 275, "276": 277,
    "278": 279, "280": 281, "282": 283, "284": 285,
    "286": 287, "288": 289, "290": 291, "292": 293,
    "294": 295, "296": 297, "298": 299, "300": 301,
    "302": 303, "306": 307, "308": 309, "310": 311,
    "313": 314, "315": 316, "317": 318, "319": 320,
    "321": 322, "323": 324, "325": 326, "327": 328,
    "330": 331, "332": 333, "334": 335, "336": 337,
    "338": 339, "340": 341, "342": 343, "344": 345,
    "346": 347, "348": 349, "350": 351, "352": 353,
    "354": 355, "356": 357, "358": 359, "360": 361,
    "362": 363, "364": 365, "366": 367, "368": 369,
    "370": 371, "372": 373, "374": 375, "376": 255,
    "377": 378, "379": 380, "381": 382, "383": 115,
    "385": 595, "386": 387, "388": 389, "390": 596,
    "391": 392, "393": 598, "394": 599, "395": 396,
    "398": 477, "399": 601, "400": 603, "401": 402,
    "403": 608, "404": 611, "406": 617, "407": 616,
    "408": 409, "412": 623, "413": 626, "415": 629,
    "416": 417, "418": 419, "420": 421, "422": 640,
    "423": 424, "425": 643, "428": 429, "430": 648,
    "431": 432, "433": 650, "434": 651, "435": 436,
    "437": 438, "439": 658, "440": 441, "444": 445,
    "452": 454, "453": 454, "455": 457, "456": 457,
    "458": 460, "459": 460, "461": 462, "463": 464,
    "465": 466, "467": 468, "469": 470, "471": 472,
    "473": 474, "475": 476, "478": 479, "480": 481,
    "482": 483, "484": 485, "486": 487, "488": 489,
    "490": 491, "492": 493, "494": 495, "497": 499,
    "498": 499, "500": 501, "502": 405, "503": 447,
    "504": 505, "506": 507, "508": 509, "510": 511,
    "512": 513, "514": 515, "516": 517, "518": 519,
    "520": 521, "522": 523, "524": 525, "526": 527,
    "528": 529, "530": 531, "532": 533, "534": 535,
    "536": 537, "538": 539, "540": 541, "542": 543,
    "544": 414, "546": 547, "548": 549, "550": 551,
    "552": 553, "554": 555, "556": 557, "558": 559,
    "560": 561, "562": 563, "570": 11365, "571": 572,
    "573": 410, "574": 11366, "577": 578, "579": 384,
    "580": 649, "581": 652, "582": 583, "584": 585,
    "586": 587, "588": 589, "590": 591, "837": 953,
    "880": 881, "882": 883, "886": 887, "895": 1011,
    "902": 940, "904": 941, "905": 942, "906": 943,
    "908": 972, "910": 973, "911": 974, "913": 945,
    "914": 946, "915": 947, "916": 948, "917": 949,
    "918": 950, "919": 951, "920": 952, "921": 953,
    "922": 954, "923": 955, "924": 956, "925": 957,
    "926": 958, "927": 959, "928": 960, "929": 961,
    "931": 963, "932": 964, "933": 965, "934": 966,
    "935": 967, "936": 968, "937": 969, "938": 970,
    "939": 971, "962": 963, "975": 983, "976": 946,
    "977": 952, "981": 966, "982": 960, "984": 985,
    "986": 987, "988": 989, "990": 991, "992": 993,
    "994": 995, "996": 997, "998": 999, "1000": 1001,
    "1002": 1003, "1004": 1005, "1006": 1007, "1008": 954,
    "1009": 961, "1012": 952, "1013": 949, "1015": 1016,
    "1017": 1010, "1018": 1019, "1021": 891, "1022": 892,
    "1023": 893, "1024": 1104, "1025": 1105, "1026": 1106,
    "1027": 1107, "1028": 1108, "1029": 1109, "1030": 1110,
    "1031": 1111, "1032": 1112, "1033": 1113, "1034": 1114,
    "1035": 1115, "1036": 1116, "1037": 1117, "1038": 1118,
    "1039": 1119, "1040": 1072, "1041": 1073, "1042": 1074,
    "1043": 1075, "1044": 1076, "1045": 1077, "1046": 1078,
    "1047": 1079, "1048": 1080, "1049": 1081, "1050": 1082,
    "1051": 1083, "1052": 1084, "1053": 1085, "1054": 1086,
    "1055": 1087, "1056": 1088, "1057": 1089, "1058": 1090,
    "1059": 1091, "1060": 1092, "1061": 1093, "1062": 1094,
    "1063": 1095, "1064": 1096, "1065": 1097, "1066": 1098,
    "1067": 1099, "1068": 1100, "1069": 1101, "1070": 1102,
    "1071": 1103, "1120": 1121, "1122": 1123, "1124": 1125,
    "1126": 1127, "1128": 1129, "1130": 1131, "1132": 1133,
    "1134": 1135, "1136": 1137, "1138": 1139, "1140": 1141,
    "1142": 1143, "1144": 1145, "1146": 1147, "1148": 1149,
    "1150": 1151, "1152": 1153, "1162": 1163, "1164": 1165,
    "1166": 1167, "1168": 1169, "1170": 1171, "1172": 1173,
    "1174": 1175, "1176": 1177, "1178": 1179, "1180": 1181,
    "1182": 1183, "1184": 1185, "1186": 1187, "1188": 1189,
    "1190": 1191, "1192": 1193, "1194": 1195, "1196": 1197,
    "1198": 1199, "1200": 1201, "1202": 1203, "1204": 1205,
    "1206": 1207, "1208": 1209, "1210": 1211, "1212": 1213,
    "1214": 1215, "1216": 1231, "1217": 1218, "1219": 1220,
    "1221": 1222, "1223": 1224, "1225": 1226, "1227": 1228,
    "1229": 1230, "1232": 1233, "1234": 1235, "1236": 1237,
    "1238": 1239, "1240": 1241, "1242": 1243, "1244": 1245,
    "1246": 1247, "1248": 1249, "1250": 1251, "1252": 1253,
    "1254": 1255, "1256": 1257, "1258": 1259, "1260": 1261,
    "1262": 1263, "1264": 1265, "1266": 1267, "1268": 1269,
    "1270": 1271, "1272": 1273, "1274": 1275, "1276": 1277,
    "1278": 1279, "1280": 1281, "1282": 1283, "1284": 1285,
    "1286": 1287, "1288": 1289, "1290": 1291, "1292": 1293,
    "1294": 1295, "1296": 1297, "1298": 1299, "1300": 1301,
    "1302": 1303, "1304": 1305, "1306": 1307, "1308": 1309,
    "1310": 1311, "1312": 1313, "1314": 1315, "1316": 1317,
    "1318": 1319, "1320": 1321, "1322": 1323, "1324": 1325,
    "1326": 1327, "1329": 1377, "1330": 1378, "1331": 1379,
    "1332": 1380, "1333": 1381, "1334": 1382, "1335": 1383,
    "1336": 1384, "1337": 1385, "1338": 1386, "1339": 1387,
    "1340": 1388, "1341": 1389, "1342": 1390, "1343": 1391,
    "1344": 1392, "1345": 1393, "1346": 1394, "1347": 1395,
    "1348": 1396, "1349": 1397, "1350": 1398, "1351": 1399,
    "1352": 1400, "1353": 1401, "1354": 1402, "1355": 1403,
    "1356": 1404, "1357": 1405, "1358": 1406, "1359": 1407,
    "1360": 1408, "1361": 1409, "1362": 1410, "1363": 1411,
    "1364": 1412, "1365": 1413, "1366": 1414, "4256": 11520,
    "4257": 11521, "4258": 11522, "4259": 11523, "4260": 11524,
    "4261": 11525, "4262": 11526, "4263": 11527, "4264": 11528,
    "4265": 11529, "4266": 11530, "4267": 11531, "4268": 11532,
    "4269": 11533, "4270": 11534, "4271": 11535, "4272": 11536,
    "4273": 11537, "4274": 11538, "4275": 11539, "4276": 11540,
    "4277": 11541, "4278": 11542, "4279": 11543, "4280": 11544,
    "4281": 11545, "4282": 11546, "4283": 11547, "4284": 11548,
    "4285": 11549, "4286": 11550, "4287": 11551, "4288": 11552,
    "4289": 11553, "4290": 11554, "4291": 11555, "4292": 11556,
    "4293": 11557, "4295": 11559, "4301": 11565, "5112": 5104,
    "5113": 5105, "5114": 5106, "5115": 5107, "5116": 5108,
    "5117": 5109, "7296": 1074, "7297": 1076, "7298": 1086,
    "7299": 1089, "7300": 1090, "7301": 1090, "7302": 1098,
    "7303": 1123, "7304": 42571, "7312": 4304, "7313": 4305,
    "7314": 4306, "7315": 4307, "7316": 4308, "7317": 4309,
    "7318": 4310, "7319": 4311, "7320": 4312, "7321": 4313,
    "7322": 4314, "7323": 4315, "7324": 4316, "7325": 4317,
    "7326": 4318, "7327": 4319, "7328": 4320, "7329": 4321,
    "7330": 4322, "7331": 4323, "7332": 4324, "7333": 4325,
    "7334": 4326, "7335": 4327, "7336": 4328, "7337": 4329,
    "7338": 4330, "7339": 4331, "7340": 4332, "7341": 4333,
    "7342": 4334, "7343": 4335, "7344": 4336, "7345": 4337,
    "7346": 4338, "7347": 4339, "7348": 4340, "7349": 4341,
    "7350": 4342, "7351": 4343, "7352": 4344, "7353": 4345,
    "7354": 4346, "7357": 4349, "7358": 4350, "7359": 4351,
    "7680": 7681, "7682": 7683, "7684": 7685, "7686": 7687,
    "7688": 7689, "7690": 7691, "7692": 7693, "7694": 7695,
    "7696": 7697, "7698": 7699, "7700": 7701, "7702": 7703,
    "7704": 7705, "7706": 7707, "7708": 7709, "7710": 7711,
    "7712": 7713, "7714": 7715, "7716": 7717, "7718": 7719,
    "7720": 7721, "7722": 7723, "7724": 7725, "7726": 7727,
    "7728": 7729, "7730": 7731, "7732": 7733, "7734": 7735,
    "7736": 7737, "7738": 7739, "7740": 7741, "7742": 7743,
    "7744": 7745, "7746": 7747, "7748": 7749, "7750": 7751,
    "7752": 7753, "7754": 7755, "7756": 7757, "7758": 7759,
    "7760": 7761, "7762": 7763, "7764": 7765, "7766": 7767,
    "7768": 7769, "7770": 7771, "7772": 7773, "7774": 7775,
    "7776": 7777, "7778": 7779, "7780": 7781, "7782": 7783,
    "7784": 7785, "7786": 7787, "7788": 7789, "7790": 7791,
    "7792": 7793, "7794": 7795, "7796": 7797, "7798": 7799,
    "7800": 7801, "7802": 7803, "7804": 7805, "7806": 7807,
    "7808": 7809, "7810": 7811, "7812": 7813, "7814": 7815,
    "7816": 7817, "7818": 7819, "7820": 7821, "7822": 7823,
    "7824": 7825, "7826": 7827, "7828": 7829, "7835": 7777,
    "7840": 7841, "7842": 7843, "7844": 7845, "7846": 7847,
    "7848": 7849, "7850": 7851, "7852": 7853, "7854": 7855,
    "7856": 7857, "7858": 7859, "7860": 7861, "7862": 7863,
    "7864": 7865, "7866": 7867, "7868": 7869, "7870": 7871,
    "7872": 7873, "7874": 7875, "7876": 7877, "7878": 7879,
    "7880": 7881, "7882": 7883, "7884": 7885, "7886": 7887,
    "7888": 7889, "7890": 7891, "7892": 7893, "7894": 7895,
    "7896": 7897, "7898": 7899, "7900": 7901, "7902": 7903,
    "7904": 7905, "7906": 7907, "7908": 7909, "7910": 7911,
    "7912": 7913, "7914": 7915, "7916": 7917, "7918": 7919,
    "7920": 7921, "7922": 7923, "7924": 7925, "7926": 7927,
    "7928": 7929, "7930": 7931, "7932": 7933, "7934": 7935,
    "7944": 7936, "7945": 7937, "7946": 7938, "7947": 7939,
    "7948": 7940, "7949": 7941, "7950": 7942, "7951": 7943,
    "7960": 7952, "7961": 7953, "7962": 7954, "7963": 7955,
    "7964": 7956, "7965": 7957, "7976": 7968, "7977": 7969,
    "7978": 7970, "7979": 7971, "7980": 7972, "7981": 7973,
    "7982": 7974, "7983": 7975, "7992": 7984, "7993": 7985,
    "7994": 7986, "7995": 7987, "7996": 7988, "7997": 7989,
    "7998": 7990, "7999": 7991, "8008": 8000, "8009": 8001,
    "8010": 8002, "8011": 8003, "8012": 8004, "8013": 8005,
    "8025": 8017, "8027": 8019, "8029": 8021, "8031": 8023,
    "8040": 8032, "8041": 8033, "8042": 8034, "8043": 8035,
    "8044": 8036, "8045": 8037, "8046": 8038, "8047": 8039,
    "8120": 8112, "8121": 8113, "8122": 8048, "8123": 8049,
    "8126": 953, "8136": 8050, "8137": 8051, "8138": 8052,
    "8139": 8053, "8152": 8144, "8153": 8145, "8154": 8054,
    "8155": 8055, "8168": 8160, "8169": 8161, "8170": 8058,
    "8171": 8059, "8172": 8165, "8184": 8056, "8185": 8057,
    "8186": 8060, "8187": 8061, "8486": 969, "8490": 107,
    "8491": 229, "8498": 8526, "8544": 8560, "8545": 8561,
    "8546": 8562, "8547": 8563, "8548": 8564, "8549": 8565,
    "8550": 8566, "8551": 8567, "8552": 8568, "8553": 8569,
    "8554": 8570, "8555": 8571, "8556": 8572, "8557": 8573,
    "8558": 8574, "8559": 8575, "8579": 8580, "9398": 9424,
    "9399": 9425, "9400": 9426, "9401": 9427, "9402": 9428,
    "9403": 9429, "9404": 9430, "9405": 9431, "9406": 9432,
    "9407": 9433, "9408": 9434, "9409": 9435, "9410": 9436,
    "9411": 9437, "9412": 9438, "9413": 9439, "9414": 9440,
    "9415": 9441, "9416": 9442, "9417": 9443, "9418": 9444,
    "9419": 9445, "9420": 9446, "9421": 9447, "9422": 9448,
    "9423": 9449, "11264": 11312, "11265": 11313, "11266": 11314,
    "11267": 11315, "11268": 11316, "11269": 11317, "11270": 11318,
    "11271": 11319, "11272": 11320, "11273": 11321, "11274": 11322,
    "11275": 11323, "11276": 11324, "11277": 11325, "11278": 11326,
    "11279": 11327, "11280": 11328, "11281": 11329, "11282": 11330,
    "11283": 11331, "11284": 11332, "11285": 11333, "11286": 11334,
    "11287": 11335, "11288": 11336, "11289": 11337, "11290": 11338,
    "11291": 11339, "11292": 11340, "11293": 11341, "11294": 11342,
    "11295": 11343, "11296": 11344, "11297": 11345, "11298": 11346,
    "11299": 11347, "11300": 11348, "11301": 11349, "11302": 11350,
    "11303": 11351, "11304": 11352, "11305": 11353, "11306": 11354,
    "11307": 11355, "11308": 11356, "11309": 11357, "11310": 11358,
    "11311": 11359, "11360": 11361, "11362": 619, "11363": 7549,
    "11364": 637, "11367": 11368, "11369": 11370, "11371": 11372,
    "11373": 593, "11374": 625, "11375": 592, "11376": 594,
    "11378": 11379, "11381": 11382, "11390": 575, "11391": 576,
    "11392": 11393, "11394": 11395, "11396": 11397, "11398": 11399,
    "11400": 11401, "11402": 11403, "11404": 11405, "11406": 11407,
    "11408": 11409, "11410": 11411, "11412": 11413, "11414": 11415,
    "11416": 11417, "11418": 11419, "11420": 11421, "11422": 11423,
    "11424": 11425, "11426": 11427, "11428": 11429, "11430": 11431,
    "11432": 11433, "11434": 11435, "11436": 11437, "11438": 11439,
    "11440": 11441, "11442": 11443, "11444": 11445, "11446": 11447,
    "11448": 11449, "11450": 11451, "11452": 11453, "11454": 11455,
    "11456": 11457, "11458": 11459, "11460": 11461, "11462": 11463,
    "11464": 11465, "11466": 11467, "11468": 11469, "11470": 11471,
    "11472": 11473, "11474": 11475, "11476": 11477, "11478": 11479,
    "11480": 11481, "11482": 11483, "11484": 11485, "11486": 11487,
    "11488": 11489, "11490": 11491, "11499": 11500, "11501": 11502,
    "11506": 11507, "42560": 42561, "42562": 42563, "42564": 42565,
    "42566": 42567, "42568": 42569, "42570": 42571, "42572": 42573,
    "42574": 42575, "42576": 42577, "42578": 42579, "42580": 42581,
    "42582": 42583, "42584": 42585, "42586": 42587, "42588": 42589,
    "42590": 42591, "42592": 42593, "42594": 42595, "42596": 42597,
    "42598": 42599, "42600": 42601, "42602": 42603, "42604": 42605,
    "42624": 42625, "42626": 42627, "42628": 42629, "42630": 42631,
    "42632": 42633, "42634": 42635, "42636": 42637, "42638": 42639,
    "42640": 42641, "42642": 42643, "42644": 42645, "42646": 42647,
    "42648": 42649, "42650": 42651, "42786": 42787, "42788": 42789,
    "42790": 42791, "42792": 42793, "42794": 42795, "42796": 42797,
    "42798": 42799, "42802": 42803, "42804": 42805, "42806": 42807,
    "42808": 42809, "42810": 42811, "42812": 42813, "42814": 42815,
    "42816": 42817, "42818": 42819, "42820": 42821, "42822": 42823,
    "42824": 42825, "42826": 42827, "42828": 42829, "42830": 42831,
    "42832": 42833, "42834": 42835, "42836": 42837, "42838": 42839,
    "42840": 42841, "42842": 42843, "42844": 42845, "42846": 42847,
    "42848": 42849, "42850": 42851, "42852": 42853, "42854": 42855,
    "42856": 42857, "42858": 42859, "42860": 42861, "42862": 42863,
    "42873": 42874, "42875": 42876, "42877": 7545, "42878": 42879,
    "42880": 42881, "42882": 42883, "42884": 42885, "42886": 42887,
    "42891": 42892, "42893": 613, "42896": 42897, "42898": 42899,
    "42902": 42903, "42904": 42905, "42906": 42907, "42908": 42909,
    "42910": 42911, "42912": 42913, "42914": 42915, "42916": 42917,
    "42918": 42919, "42920": 42921, "42922": 614, "42923": 604,
    "42924": 609, "42925": 620, "42926": 618, "42928": 670,
    "42929": 647, "42930": 669, "42931": 43859, "42932": 42933,
    "42934": 42935, "42936": 42937, "42938": 42939, "42940": 42941,
    "42942": 42943, "42944": 42945, "42946": 42947, "42948": 42900,
    "42949": 642, "42950": 7566, "42951": 42952, "42953": 42954,
    "42960": 42961, "42966": 42967, "42968": 42969, "42997": 42998,
    "43888": 5024, "43889": 5025, "43890": 5026, "43891": 5027,
    "43892": 5028, "43893": 5029, "43894": 5030, "43895": 5031,
    "43896": 5032, "43897": 5033, "43898": 5034, "43899": 5035,
    "43900": 5036, "43901": 5037, "43902": 5038, "43903": 5039,
    "43904": 5040, "43905": 5041, "43906": 5042, "43907": 5043,
    "43908": 5044, "43909": 5045, "43910": 5046, "43911": 5047,
    "43912": 5048, "43913": 5049, "43914": 5050, "43915": 5051,
    "43916": 5052, "43917": 5053, "43918": 5054, "43919": 5055,
    "43920": 5056, "43921": 5057, "43922": 5058, "43923": 5059,
    "43924": 5060, "43925": 5061, "43926": 5062, "43927": 5063,
    "43928": 5064, "43929": 5065, "43930": 5066, "43931": 5067,
    "43932": 5068, "43933": 5069, "43934": 5070, "43935": 5071,
    "43936": 5072, "43937": 5073, "43938": 5074, "43939": 5075,
    "43940": 5076, "43941": 5077, "43942": 5078, "43943": 5079,
    "43944": 5080, "43945": 5081, "43946": 5082, "43947": 5083,
    "43948": 5084, "43949": 5085, "43950": 5086, "43951": 5087,
    "43952": 5088, "43953": 5089, "43954": 5090, "43955": 5091,
    "43956": 5092, "43957": 5093, "43958": 5094, "43959": 5095,
    "43960": 5096, "43961": 5097, "43962": 5098, "43963": 5099,
    "43964": 5100, "43965": 5101, "43966": 5102, "43967": 5103,
    "65313": 65345, "65314": 65346, "65315": 65347, "65316": 65348,
    "65317": 65349, "65318": 65350, "65319": 65351, "65320": 65352,
    "65321": 65353, "65322": 65354, "65323": 65355, "65324": 65356,
    "65325": 65357, "65326": 65358, "65327": 65359, "65328": 65360,
    "65329": 65361, "65330": 65362, "65331": 65363, "65332": 65364,
    "65333": 65365, "65334": 65366, "65335": 65367, "65336": 65368,
    "65337": 65369, "65338": 65370, "66560": 66600, "66561": 66601,
    "66562": 66602, "66563": 66603, "66564": 66604, "66565": 66605,
    "66566": 66606, "66567": 66607, "66568": 66608, "66569": 66609,
    "66570": 66610, "66571": 66611, "66572": 66612, "66573": 66613,
    "66574": 66614, "66575": 66615, "66576": 66616, "66577": 66617,
    "66578": 66618, "66579": 66619, "66580": 66620, "66581": 66621,
    "66582": 66622, "66583": 66623, "66584": 66624, "66585": 66625,
    "66586": 66626, "66587": 66627, "66588": 66628, "66589": 66629,
    "66590": 66630, "66591": 66631, "66592": 66632, "66593": 66633,
    "66594": 66634, "66595": 66635, "66596": 66636, "66597": 66637,
    "66598": 66638, "66599": 66639, "66736": 66776, "66737": 66777,
    "66738": 66778, "66739": 66779, "66740": 66780, "66741": 66781,
    "66742": 66782, "66743": 66783, "66744": 66784, "66745": 66785,
    "66746": 66786, "66747": 66787, "66748": 66788, "66749": 66789,
    "66750": 66790, "66751": 66791, "66752": 66792, "66753": 66793,
    "66754": 66794, "66755": 66795, "66756": 66796, "66757": 66797,
    "66758": 66798, "66759": 66799, "66760": 66800, "66761": 66801,
    "66762": 66802, "66763": 66803, "66764": 66804, "66765": 66805,
    "66766": 66806, "66767": 66807, "66768": 66808, "66769": 66809,
    "66770": 66810, "66771": 66811, "66928": 66967, "66929": 66968,
    "66930": 66969, "66931": 66970, "66932": 66971, "66933": 66972,
    "66934": 66973, "66935": 66974, "66936": 66975, "66937": 66976,
    "66938": 66977, "66940": 66979, "66941": 66980, "66942": 66981,
    "66943": 66982, "66944": 66983, "66945": 66984, "66946": 66985,
    "66947": 66986, "66948": 66987, "66949": 66988, "66950": 66989,
    "66951": 66990, "66952": 66991, "66953": 66992, "66954": 66993,
    "66956": 66995, "66957": 66996, "66958": 66997, "66959": 66998,
    "66960": 66999, "66961": 67000, "66962": 67001, "66964": 67003,
    "66965": 67004, "68736": 68800, "68737": 68801, "68738": 68802,
    "68739": 68803, "68740": 68804, "68741": 68805, "68742": 68806,
    "68743": 68807, "68744": 68808, "68745": 68809, "68746": 68810,
    "68747": 68811, "68748": 68812, "68749": 68813, "68750": 68814,
    "68751": 68815, "68752": 68816, "68753": 68817, "68754": 68818,
    "68755": 68819, "68756": 68820, "68757": 68821, "68758": 68822,
    "68759": 68823, "68760": 68824, "68761": 68825, "68762": 68826,
    "68763": 68827, "68764": 68828, "68765": 68829, "68766": 68830,
    "68767": 68831, "68768": 68832, "68769": 68833, "68770": 68834,
    "68771": 68835, "68772": 68836, "68773": 68837, "68774": 68838,
    "68775": 68839, "68776": 68840, "68777": 68841, "68778": 68842,
    "68779": 68843, "68780": 68844, "68781": 68845, "68782": 68846,
    "68783": 68847, "68784": 68848, "68785": 68849, "68786": 68850,
    "71840": 71872, "71841": 71873, "71842": 71874, "71843": 71875,
    "71844": 71876, "71845": 71877, "71846": 71878, "71847": 71879,
    "71848": 71880, "71849": 71881, "71850": 71882, "71851": 71883,
    "71852": 71884, "71853": 71885, "71854": 71886, "71855": 71887,
    "71856": 71888, "71857": 71889, "71858": 71890, "71859": 71891,
    "71860": 71892, "71861": 71893, "71862": 71894, "71863": 71895,
    "71864": 71896, "71865": 71897, "71866": 71898, "71867": 71899,
    "71868": 71900, "71869": 71901, "71870": 71902, "71871": 71903,
    "93760": 93792, "93761": 93793, "93762": 93794, "93763": 93795,
    "93764": 93796, "93765": 93797, "93766": 93798, "93767": 93799,
    "93768": 93800, "93769": 93801, "93770": 93802, "93771": 93803,
    "93772": 93804, "93773": 93805, "93774": 93806, "93775": 93807,
    "93776": 93808, "93777": 93809, "93778": 93810, "93779": 93811,
    "93780": 93812, "93781": 93813, "93782": 93814, "93783": 93815,
    "93784": 93816, "93785": 93817, "93786": 93818, "93787": 93819,
    "93788": 93820, "93789": 93821, "93790": 93822, "93791": 93823,
    "125184": 125218, "125185": 125219, "125186": 125220, "125187": 125221,
    "125188": 125222, "125189": 125223, "125190": 125224, "125191": 125225,
    "125192": 125226, "125193": 125227, "125194": 125228, "125195": 125229,
    "125196": 125230, "125197": 125231, "125198": 125232, "125199": 125233,
    "125200": 125234, "125201": 125235, "125202": 125236, "125203": 125237,
    "125204": 125238, "125205": 125239, "125206": 125240, "125207": 125241,
    "125208": 125242, "125209": 125243, "125210": 125244, "125211": 125245,
    "125212": 125246, "125213": 125247, "125214": 125248, "125215": 125249,
    "125216": 125250, "125217": 125251
};
// ----------------------------------------------------------------------
function foldcase_string(string) {
    string = string.valueOf();
    return Array.from(string).map(str => {
        const ord = str.codePointAt(0);
        const output = fold_case_mapping[ord];
        if (output) {
            return String.fromCodePoint(output);
        }
        return str;
    }).join('');
}
// ----------------------------------------------------------------------
// :: Flatten nested arrays
// :: ref: https://stackoverflow.com/a/27282907/387194
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
// :: Fisher-Yates (aka Knuth) Shuffle
// :: ref: https://stackoverflow.com/a/2450976/387194
// ----------------------------------------------------------------------
function shuffle(array, random) {
  let currentIndex = array.length,  randomIndex;

  // While there remain elements to shuffle.
  while (currentIndex > 0) {

    // Pick a remaining element.
    randomIndex = Math.floor(random() * currentIndex);
    currentIndex--;

    // And swap it with the current element.
    [array[currentIndex], array[randomIndex]] = [
      array[randomIndex], array[currentIndex]];
  }

  return array;
}
// ----------------------------------------------------------------------
// :: Nil constructor with only once instance
// ----------------------------------------------------------------------
function Nil() {}
Nil.prototype.toString = function() {
    return '()';
};
Nil.prototype.valueOf = function() {
    return undefined;
};
Nil.prototype.serialize = function() {
    return 0;
};
Nil.prototype.to_object = function() {
    return {};
};
Nil.prototype.append = function(x) {
    return new Pair(x, nil);
};
Nil.prototype.to_array = function() {
    return [];
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
function to_array(name, deep) {
    return function recur(list) {
        typecheck(name, list, ['pair', 'nil']);
        if (is_nil(list)) {
            return [];
        }
        var result = [];
        var node = list;
        while (true) {
            if (is_pair(node)) {
                if (node.have_cycles('cdr')) {
                    break;
                }
                var car = node.car;
                if (deep && is_pair(car)) {
                    car = this.get(name).call(this, car);
                }
                result.push(car);
                node = node.cdr;
            } else if (is_nil(node)) {
                break;
            } else {
                throw new Error(`${name}: can't convert improper list`);
            }
        }
        return result;
    };
}
// ----------------------------------------------------------------------
Pair.prototype.flatten = function() {
    return Pair.fromArray(flatten(this.to_array()));
};
// ----------------------------------------------------------------------
Pair.prototype.length = function() {
    var len = 0;
    var node = this;
    while (true) {
        if (!node || is_nil(node) || !is_pair(node) ||
            node.have_cycles('cdr')) {
            break;
        }
        len++;
        node = node.cdr;
    }
    return len;
};
// ----------------------------------------------------------------------
Pair.match = function(obj, item) {
    if (obj instanceof LSymbol) {
        return LSymbol.is(obj, item);
    } else if (is_pair(obj)) {
        return Pair.match(obj.car, item) || Pair.match(obj.cdr, item);
    } else if (Array.isArray(obj)) {
        return obj.some(x => {
            return Pair.match(x, item);
        });
    } else if (is_plain_object(obj)) {
        return Object.values(obj).some(x => {
            return Pair.match(x, item);
        });
    }
    return false;
};
// ----------------------------------------------------------------------
Pair.prototype.find = function(item) {
    return Pair.match(this, item);
};

// ----------------------------------------------------------------------
Pair.prototype.clone = function(deep = true) {
    var visited = new Map();
    function clone(node) {
        if (is_pair(node)) {
            if (visited.has(node)) {
                return visited.get(node);
            }
            var pair = new Pair();
            visited.set(node, pair);
            if (deep) {
                pair.car = clone(node.car);
            } else {
                pair.car = node.car;
            }
            pair.cdr = clone(node.cdr);
            pair[__cycles__] = node[__cycles__];
            return pair;
        }
        return node;
    }
    return clone(this);
};

// ----------------------------------------------------------------------
Pair.prototype.last_pair = function() {
    let node = this;
    while (true) {
        if (!is_pair(node.cdr)) {
            return node;
        }
        if (node.have_cycles('cdr')) {
            break;
        }
        node = node.cdr;
    }
};

// ----------------------------------------------------------------------
Pair.prototype.to_array = function(deep = true) {
    var result = [];
    if (is_pair(this.car)) {
        if (deep) {
            result.push(this.car.to_array());
        } else {
            result.push(this.car);
        }
    } else {
        result.push(this.car.valueOf());
    }
    if (is_pair(this.cdr)) {
        result = result.concat(this.cdr.to_array(deep));
    }
    return result;
};

// ----------------------------------------------------------------------
// :: TODO: change to Pair.from_array
// ----------------------------------------------------------------------
Pair.fromArray = function(array, deep = true, quote = false) {
    if (is_pair(array) || quote && array instanceof Array && array[__data__]) {
        return array;
    }
    if (deep === false) {
        var list = nil;
        for (let i = array.length; i--;) {
            list = new Pair(array[i], list);
        }
        return list;
    }
    if (array.length && !(array instanceof Array)) {
        array = [...array];
    }
    var result = nil;
    var i = array.length;
    while (i--) {
        let car = array[i];
        if (car instanceof Array) {
            car = Pair.fromArray(car, deep, quote);
        } else if (typeof car === 'string') {
            car = LString(car);
        } else if (typeof car === 'number' && !Number.isNaN(car)) {
            car = LNumber(car);
        }
        result = new Pair(car, result);
    }
    return result;
};

// ----------------------------------------------------------------------
// By default to_object was created to create JavaScript objects,
// so it uses valueOf to get native values.
// Literal parameter was a hack to allow creating LComplex from LIPS code
// ----------------------------------------------------------------------
Pair.prototype.to_object = function(literal = false) {
    var node = this;
    var result = {};
    while (true) {
        if (is_pair(node) && is_pair(node.car)) {
            var pair = node.car;
            var name = pair.car;
            if (name instanceof LSymbol) {
                name = name.__name__;
            }
            if (name instanceof LString) {
                name = name.valueOf();
            }
            var cdr = pair.cdr;
            if (is_pair(cdr)) {
                cdr = cdr.to_object(literal);
            }
            if (is_native(cdr)) {
                if (!literal) {
                    cdr = cdr.valueOf();
                }
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
                new LSymbol(pair[0]),
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
        if (!is_nil(node)) {
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
    if (this.have_cycles()) {
        throw new Error("You can't reverse list that have cycles");
    }
    var node = this;
    var prev = nil;
    while (!is_nil(node)) {
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
        if (is_pair(pair)) {
            if (pair.replace) {
                delete pair.replace;
                return pair;
            }
            var car = fn(pair.car);
            if (is_pair(car)) {
                car = recur(car);
                visited.push(car);
            }
            var cdr = fn(pair.cdr);
            if (is_pair(cdr)) {
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
        return new Pair(fn(this.car), is_nil(this.cdr) ? nil : this.cdr.map(fn));
    } else {
        return nil;
    }
};
var repr = new Map();
// ----------------------------------------------------------------------
function is_plain_object(object) {
    return object && typeof object === 'object' && object.constructor === Object;
}
// ----------------------------------------------------------------------
var props = Object.getOwnPropertyNames(Array.prototype);
var array_methods = [];
props.forEach(x => {
    array_methods.push(Array[x], Array.prototype[x]);
});
// ----------------------------------------------------------------------
function is_array_method(x) {
    x = unbind(x);
    return array_methods.includes(x);
}
// ----------------------------------------------------------------------
function is_lips_function(x) {
    return is_function(x) && (is_lambda(x) || x.__doc__);
}
// ----------------------------------------------------------------------
function user_repr(obj) {
    var constructor = obj.constructor || Object;
    var plain_object = is_plain_object(obj);
    var iterator = is_function(obj[Symbol.asyncIterator]) ||
        is_function(obj[Symbol.iterator]);
    var fn;
    if (repr.has(constructor)) {
        fn = repr.get(constructor);
    } else {
        repr.forEach(function(value, key) {
            key = unbind(key);
            // if key is Object it should only work for plain_object
            // because otherwise it will match every object
            // we don't use instanceof so it don't work for subclasses
            if (obj instanceof key &&
                (key === Object && plain_object && !iterator || key !== Object)) {
                fn = value;
            }
        });
    }
    return fn;
}
// ----------------------------------------------------------------------
var str_mapping = new Map();
[
    [true, '#t'],
    [false, '#f'],
    [null, '#null'],
    [undefined, '#void']
].forEach(([key, value]) => {
    str_mapping.set(key, value);
});
// ----------------------------------------------------------------------
// :: Debug function that can be used with JSON.stringify
// :: that will show symbols
// ----------------------------------------------------------------------
/* c8 ignore next 22 */
function symbolize(obj) {
    if (obj && typeof obj === 'object') {
        var result = {};
        const symbols = Object.getOwnPropertySymbols(obj);
        symbols.forEach((key) => {
            const name = key.toString()
                .replace(/Symbol\(([^)]+)\)/, '$1');
            result[name] = toString(obj[key]);
        });
        const props = Object.getOwnPropertyNames(obj);
        props.forEach(key => {
            const o = obj[key];
            if (o && typeof o === 'object' && o.constructor === Object) {
                result[key] = symbolize(o);
            } else {
                result[key] = toString(o);
            }
        });
        return result;
    }
    return obj;
}
// ----------------------------------------------------------------------
function get_props(obj) {
    return Object.keys(obj).concat(Object.getOwnPropertySymbols(obj));
}
// ----------------------------------------------------------------------
function has_own_function(obj, name) {
    return obj.hasOwnProperty(name) && is_function(obj.toString);
}
// ----------------------------------------------------------------------
function function_to_string(fn) {
    if (is_native_function(fn)) {
        return '#<procedure(native)>';
    }
    const constructor = fn.prototype && fn.prototype.constructor;
    if (is_function(constructor) && is_lambda(constructor)) {
        if (fn[__class__] && constructor.hasOwnProperty('__name__')) {
            let name = constructor.__name__;
            if (LString.isString(name)) {
                name = name.toString();
                return `#<class:${name}>`;
            }
            return '#<class>';
        }
    }
    if (fn.hasOwnProperty('__name__')) {
        let name = fn.__name__;
        if (typeof name === 'symbol') {
            name = symbol_to_string(name);
        }
        if (typeof name === 'string') {
            return `#<procedure:${name}>`;
        }
    }
    if (has_own_function(fn, 'toString')) {
        return fn.toString();
    } else if (fn.name && !is_lambda(fn)) {
        return `#<procedure:${fn.name.trim()}>`;
    } else {
        return '#<procedure>';
    }
}
// ----------------------------------------------------------------------
// Instances extracted to make cyclomatic complexity of toString smaller
const instances = new Map();
// ----------------------------------------------------------------------
[
    [Error, function(e) {
        return e.message;
    }],
    [Pair, function(pair, { quote, skip_cycles, pair_args }) {
        // make sure that repr directly after update set the cycle ref
        if (!skip_cycles) {
            pair.mark_cycles();
        }
        return pair.toString(quote, ...pair_args);
    }],
    [LCharacter, function(chr, { quote }) {
        if (quote) {
            return chr.toString();
        }
        return chr.valueOf();
    }],
    [LString, function(str, { quote }) {
        str = str.toString();
        if (quote) {
            return JSON.stringify(str).replace(/\\n/g, '\n');
        }
        return str;
    }],
    [RegExp, function(re) {
        return '#' + re.toString();
    }]
].forEach(([cls, fn]) => {
    instances.set(cls, fn);
});
// ----------------------------------------------------------------------
const native_types = [
    LSymbol,
    Macro,
    Values,
    InputPort,
    OutputPort,
    Environment,
    QuotedPromise
];
// ----------------------------------------------------------------------
function toString(obj, quote, skip_cycles, ...pair_args) {
    if (typeof jQuery !== 'undefined' &&
        obj instanceof jQuery.fn.init) {
        return '#<jQuery(' + obj.length + ')>';
    }
    if (str_mapping.has(obj)) {
        return str_mapping.get(obj);
    }
    if (is_prototype(obj)) {
        return '#<prototype>';
    }
    if (obj) {
        var cls = obj.constructor;
        if (instances.has(cls)) {
            return instances.get(cls)(obj, { quote, skip_cycles, pair_args });
        }
    }
    // standard objects that have toString
    for (let type of native_types) {
        if (obj instanceof type) {
            return obj.toString(quote);
        }
    }
    if (obj instanceof LNumber) {
        return obj.toString();
    }
    // constants
    if ([nil, eof].includes(obj)) {
        return obj.toString();
    }
    if (obj === root) {
        return '#<js:global>';
    }
    if (obj === null) {
        return 'null';
    }
    if (is_function(obj)) {
        if (is_function(obj.toString) && obj.hasOwnProperty('toString')) {
            // promises
            return obj.toString().valueOf();
        }
        return function_to_string(obj);
    }
    if (typeof obj === 'object') {
        var constructor = obj.constructor;
        if (!constructor) {
            // This is case of fs.constants in Node.js that is null constructor object.
            // This object can be handled like normal objects that have properties
            constructor = Object;
        }
        var name;
        if (typeof constructor.__class__ === 'string') {
            name = constructor.__class__;
        } else {
            var fn = user_repr(obj);
            if (fn) {
                if (is_function(fn)) {
                    return fn(obj, quote);
                } else {
                    throw new Error('toString: Invalid repr value');
                }
            }
            name = constructor.name;
        }
        // user defined representation
        if (is_function(obj.toString) && obj.hasOwnProperty('toString')) {
            return obj.toString().valueOf();
        }
        if (type(obj) === 'instance') {
            if (is_lambda(constructor) && constructor.__name__) {
                name = constructor.__name__.valueOf();
            } else if (!is_native_function(constructor)) {
                name = 'instance';
            }
        }
        if (is_iterator(obj, Symbol.iterator)) {
            if (name) {
                return `#<iterator(${name})>`;
            }
            return '#<iterator>';
        }
        if (is_iterator(obj, Symbol.asyncIterator)) {
            if (name) {
                return `#<asyncIterator(${name})>`;
            }
            return '#<asyncIterator>';
        }
        if (name !== '') {
            return '#<' + name + '>';
        }
        return '#<Object>';
    }
    if (typeof obj !== 'string') {
        return obj.toString();
    }
    return obj;
}

// ----------------------------------------------------------------------------
Pair.prototype.mark_cycles = function() {
    mark_cycles(this);
    return this;
};

// ----------------------------------------------------------------------------
Pair.prototype.have_cycles = function(name = null) {
    if (!name) {
        return this.have_cycles('car') || this.have_cycles('cdr');
    }
    return !!(this[__cycles__] && this[__cycles__][name]);
};

// ----------------------------------------------------------------------------
Pair.prototype.is_cycle = function() {
    return is_cycle(this);
};

// ----------------------------------------------------------------------------
function is_cycle(pair) {
    if (!is_pair(pair)) {
        return false;
    }
    if (pair.have_cycles()) {
        return true;
    }
    return is_cycle(pair.car, fn) || is_cycle(pair.cdr, fn);
}

// ----------------------------------------------------------------------------
function mark_cycles(pair) {
    var seen_pairs = [];
    var cycles = [];
    var refs = [];
    function visit(pair) {
        if (!seen_pairs.includes(pair)) {
            seen_pairs.push(pair);
        }
    }
    function set(node, type, child, parents) {
        if (is_pair(child)) {
            if (parents.includes(child)) {
                if (!refs.includes(child)) {
                    refs.push(child);
                }
                if (!node[__cycles__]) {
                    node[__cycles__] = {};
                }
                node[__cycles__][type] = child;
                if (!cycles.includes(node)) {
                    cycles.push(node);
                }
                return true;
            }
        }
    }
    const detect = trampoline(function detect_thunk(pair, parents) {
        if (is_pair(pair)) {
            delete pair[__ref__];
            delete pair[__cycles__];
            visit(pair);
            parents.push(pair);
            var car = set(pair, 'car', pair.car, parents);
            var cdr = set(pair, 'cdr', pair.cdr, parents);
            if (!car) {
                detect(pair.car, parents.slice());
            }
            if (!cdr) {
                return new Thunk(() => {
                    return detect_thunk(pair.cdr, parents.slice());
                });
            }
        }
    });
    function mark_node(node, type) {
        if (is_pair(node[__cycles__][type])) {
            const count = ref_nodes.indexOf(node[__cycles__][type]);
            node[__cycles__][type] = `#${count}#`;
        }
    }
    detect(pair, []);
    var ref_nodes = seen_pairs.filter(node => refs.includes(node));
    ref_nodes.forEach((node, i) => {
        node[__ref__] = `#${i}=`;
    });
    cycles.forEach(node => {
        mark_node(node, 'car');
        mark_node(node, 'cdr');
    });
}

// ----------------------------------------------------------------------
// Trampoline based recursive pair to string that don't overflow the stack
// ----------------------------------------------------------------------
/* eslint-disable no-unused-vars */
/* c8 ignore next */
const pair_to_string = (function() {
    const prefix = (pair, nested) => {
        var result = [];
        if (pair[__ref__]) {
            result.push(pair[__ref__] + '(');
        } else if (!nested) {
            result.push('(');
        }
        return result;
    };
    const postfix = (pair, nested) => {
        if (!nested || pair[__ref__]) {
            return [')'];
        }
        return [];
    };
    return trampoline(function pairToString(pair, quote, extra = {}) {
        const {
            nested = false,
            result = [],
            cont = () => {
                result.push(...postfix(pair, nested));
            }
        } = extra;
        result.push(...prefix(pair, nested));
        let car;
        if (pair[__cycles__] && pair[__cycles__].car) {
            car = pair[__cycles__].car;
        } else {
            car = toString(pair.car, quote, true, { result, cont });
        }
        if (car !== undefined) {
            result.push(car);
        }
        return new Thunk(() => {
            if (is_pair(pair.cdr)) {
                if (pair[__cycles__] && pair[__cycles__].cdr) {
                    result.push(' . ');
                    result.push(pair[__cycles__].cdr);
                } else {
                    if (pair.cdr[__ref__]) {
                        result.push(' . ');
                    } else {
                        result.push(' ');
                    }
                    return pairToString(pair.cdr, quote, {
                        nested: true,
                        result,
                        cont
                    });
                }
            } else if (!is_nil(pair.cdr)) {
                result.push(' . ');
                result.push(toString(pair.cdr, quote));
            }
        }, cont);
    });
})();

// ----------------------------------------------------------------------
Pair.prototype.toString = function(quote, { nested = false } = {}) {
    var arr = [];
    if (this[__ref__]) {
        arr.push(this[__ref__] + '(');
    } else if (!nested) {
        arr.push('(');
    }
    var value;
    if (this[__cycles__] && this[__cycles__].car) {
        value = this[__cycles__].car;
    } else {
        value = toString(this.car, quote, true);
    }
    if (value !== undefined) {
        arr.push(value);
    }
    if (is_pair(this.cdr)) {
        if (this[__cycles__] && this[__cycles__].cdr) {
            arr.push(' . ');
            arr.push(this[__cycles__].cdr);
        } else {
            if (this.cdr[__ref__]) {
                arr.push(' . ');
            } else {
                arr.push(' ');
            }
            const cdr = this.cdr.toString(quote, { nested: true });
            arr.push(cdr);
        }
    } else if (!is_nil(this.cdr)) {
        arr = arr.concat([' . ', toString(this.cdr, quote, true)]);
    }
    if (!nested || this[__ref__]) {
        arr.push(')');
    }
    return arr.join('');
};

// ----------------------------------------------------------------------
Pair.prototype.set = function(prop, value) {
    this[prop] = value;
    if (is_pair(value)) {
        this.mark_cycles();
    }
};

// ----------------------------------------------------------------------
Pair.prototype.append = function(arg) {
    if (arg instanceof Array) {
        return this.append(Pair.fromArray(arg));
    }
    var p = this;
    if (p.car === undefined) {
        if (is_pair(arg)) {
            this.car = arg.car;
            this.cdr = arg.cdr;
        } else {
            this.car = arg;
        }
    } else if (!is_nil(arg)) {
        while (true) {
            if (is_pair(p) && !is_nil(p.cdr)) {
                p = p.cdr;
            } else {
                break;
            }
        }
        p.cdr = arg;
    }
    return this;
};
// ----------------------------------------------------------------------
Pair.prototype.serialize = function() {
    return [
        this.car,
        this.cdr
    ];
};
// ----------------------------------------------------------------------
// :: List iterator (for do-iterator macro)
// ----------------------------------------------------------------------
Pair.prototype[Symbol.iterator] = function() {
    var node = this;
    return {
        next: function() {
            var cur = node;
            node = cur.cdr;
            if (is_nil(cur)) {
                return { value: undefined, done: true };
            } else {
                return { value: cur.car, done: false };
            }
        }
    };
};
// ----------------------------------------------------------------------
// :: abs that work on BigInt
// ----------------------------------------------------------------------
function abs(x) {
    return x < 0 ? -x : x;
}
// ----------------------------------------------------------------------
function seq_compare(fn, args) {
    var [a, ...rest] = args;
    while (rest.length > 0) {
        var [b] = rest;
        if (!fn(a, b)) {
            return false;
        }
        [a, ...rest] = rest;
    }
    return true;
}

// ----------------------------------------------------------------------
function equal(x, y) {
    if (is_function(x)) {
        return is_function(y) && unbind(x) === unbind(y);
    } else if (x instanceof LNumber) {
        if (!(y instanceof LNumber)) {
            return false;
        }
        let type;
        if (x.__type__ === y.__type__) {
            if (x.__type__ === 'complex') {
                type = x.__im__.__type__ === y.__im__.__type__ &&
                    x.__re__.__type__ === y.__re__.__type__;
            } else {
                type = true;
            }
            if (type && x.cmp(y) === 0) {
                if (x.valueOf() === 0) {
                    return Object.is(x.valueOf(), y.valueOf());
                }
                return true;
            }
        }
        return false;
    } else if (typeof x === 'number') {
        if (typeof y !== 'number') {
            return false;
        }
        if (Number.isNaN(x)) {
            return Number.isNaN(y);
        }
        if (x === Number.NEGATIVE_INFINITY) {
            return y === Number.NEGATIVE_INFINITY;
        }
        if (x === Number.POSITIVE_INFINITY) {
            return y === Number.POSITIVE_INFINITY;
        }
        return equal(LNumber(x), LNumber(y));
    } else if (x instanceof LCharacter) {
        if (!(y instanceof LCharacter)) {
            return false;
        }
        return x.__char__ === y.__char__;
    } else {
        return x === y;
    }
}
// ----------------------------------------------------------------------
function same_atom(a, b) {
    if (type(a) !== type(b)) {
        return false;
    }
    if (!is_atom(a)) {
        return false;
    }
    if (a instanceof RegExp) {
        return a.source === b.source;
    }
    if (a instanceof LString) {
        return a.valueOf() === b.valueOf();
    }
    return equal(a, b);
}
// ----------------------------------------------------------------------
function is_atom(obj) {
    return obj instanceof LSymbol ||
        LString.isString(obj) ||
        is_nil(obj) ||
        obj === null ||
        obj instanceof LCharacter ||
        obj instanceof LNumber ||
        obj === true ||
        obj === false;
}
// ----------------------------------------------------------------------
var truncate = (function() {
    if (Math.trunc) {
        return Math.trunc;
    } else {
        return function(x) {
            if (x === 0) {
                return 0;
            } else if (x < 0) {
                return Math.ceil(x);
            } else {
                return Math.floor(x);
            }
        };
    }
})();
// ----------------------------------------------------------------------
// :: Macro constructor
// ----------------------------------------------------------------------
function Macro(name, fn, doc, dump) {
    if (typeof this !== 'undefined' && this.constructor !== Macro ||
        typeof this === 'undefined') {
        return new Macro(name, fn);
    }
    typecheck('Macro', name, 'string', 1);
    typecheck('Macro', fn, 'function', 2);
    if (doc) {
        if (dump) {
            this.__doc__ = doc;
        } else {
            this.__doc__ = trim_lines(doc);
        }
    }
    this.__name__ = name;
    this.__fn__ = fn;
}
// ----------------------------------------------------------------------
Macro.defmacro = function(name, fn, doc, dump) {
    var macro = new Macro(name, fn, doc, dump);
    macro.__defmacro__ = true;
    return macro;
};
// ----------------------------------------------------------------------
Macro.prototype.invoke = function(code, { env, ...rest }, macro_expand) {
    var args = {
        ...rest,
        macro_expand
    };
    var result = this.__fn__.call(env, code, args, this.__name__);
    return result;
    //return macro_expand ? quote(result) : result;
};
// ----------------------------------------------------------------------
Macro.prototype.toString = function() {
    return `#<macro:${this.__name__}>`;
};
// ----------------------------------------------------------------------
const macro = 'define-macro';
// ----------------------------------------------------------------------
const recur_guard = -10000;
function macro_expand(single) {
    return async function(code, args) {
        var env = args['env'] = this;
        var bindings = [];
        var let_macros = ['let', 'let*', 'letrec'];
        var lambda = global_env.get('lambda');
        var define = global_env.get('define');
        function is_let_macro(symbol) {
            var name = symbol.valueOf();
            return let_macros.includes(name);
        }
        function is_procedure(value, node) {
            return value === define && is_pair(node.cdr.car);
        }
        function is_lambda(value) {
            return value === lambda;
        }
        function proc_bindings(node) {
            var names = [];
            while (true) {
                if (!is_nil(node)) {
                    if (node instanceof LSymbol) {
                        names.push(node.valueOf());
                        break;
                    }
                    names.push(node.car.valueOf());
                    node = node.cdr;
                } else {
                    break;
                }
            }
            return [...bindings, ...names];
        }
        function let_binding(node) {
            return [...bindings, ...node.to_array(false).map(function(node) {
                if (is_pair(node)) {
                    return node.car.valueOf();
                }
                const t = type(node);
                const msg = `macroexpand: Invalid let binding expectig pair got ${t}`;
                throw new Error(msg);
            })];
        }
        function is_macro(name, value) {
            return value instanceof Macro &&
                value.__defmacro__ &&
                !bindings.includes(name);
        }
        async function expand_let_binding(node, n) {
            if (is_nil(node)) {
                return nil;
            }
            var pair = node.car;
            return new Pair(
                new Pair(
                    pair.car,
                    await traverse(pair.cdr, n, env)
                ),
                await expand_let_binding(node.cdr)
            );
        }
        async function traverse(node, n, env) {
            if (is_pair(node) && node.car instanceof LSymbol) {
                if (node[__data__]) {
                    return node;
                }
                var name = node.car.valueOf();
                var value = env.get(node.car, { throwError: false });
                var is_let = is_let_macro(node.car);

                var is_binding = is_let ||
                    is_procedure(value, node) ||
                    is_lambda(value);

                if (is_binding && is_pair(node.cdr.car)) {
                    var second;
                    if (is_let) {
                        bindings = let_binding(node.cdr.car);
                        second = await expand_let_binding(node.cdr.car, n);
                    } else {
                        bindings = proc_bindings(node.cdr.car);
                        second = node.cdr.car;
                    }
                    return new Pair(
                        node.car,
                        new Pair(
                            second,
                            await traverse(node.cdr.cdr, n, env)
                        )
                    );
                } else if (is_macro(name, value)) {
                    var code = value instanceof Syntax ? node : node.cdr;
                    var result = await value.invoke(code, { ...args, env }, true);
                    if (value instanceof Syntax) {
                        const { expr, scope } = result;
                        if (is_pair(expr)) {
                            if (n !== -1 && n <= 1 || n < recur_guard) {
                                return expr;
                            }
                            if (n !== -1) {
                                n = n - 1;
                            }
                            return traverse(expr, n, scope);
                        }
                        result = expr;
                    }
                    if (result instanceof LSymbol) {
                        return quote(result);
                    }
                    if (is_pair(result)) {
                        if (n !== -1 && n <= 1 || n < recur_guard) {
                            return result;
                        }
                        if (n !== -1) {
                            n = n - 1;
                        }
                        return traverse(result, n, env);
                    }
                    if (is_atom(result)) {
                        return result;
                    }
                }
            }
            // TODO: CYCLE DETECT
            var car = node.car;
            if (is_pair(car)) {
                car = await traverse(car, n, env);
            }
            var cdr = node.cdr;
            if (is_pair(cdr)) {
                cdr = await traverse(cdr, n, env);
            }
            var pair = new Pair(car, cdr);
            return pair;
        }
        //var this.__code__ = code;
        if (is_pair(code.cdr) && LNumber.isNumber(code.cdr.car)) {
            return quote((await traverse(code, code.cdr.car.valueOf(), env)).car);
        }
        if (single) {
            return quote((await traverse(code, 1, env)).car);
        }
        return quote((await traverse(code, -1, env)).car);
    };
}
// ----------------------------------------------------------------------
// TODO: Don't put Syntax as Macro they are not runtime
// ----------------------------------------------------------------------
function Syntax(fn, env) {
    this.__env__ = env;
    this.__fn__ = fn;
    // allow macroexpand
    this.__defmacro__ = true;
}
Syntax.__merge_env__ = Symbol.for('merge');
// ----------------------------------------------------------------------
Syntax.prototype = Object.create(Macro.prototype);
Syntax.prototype.invoke = function(code, { error, env, use_dynamic }, macro_expand) {
    var args = {
        error,
        env,
        use_dynamic,
        dynamic_env: this.__env__,
        macro_expand
    };
    return this.__fn__.call(env, code, args, this.__name__ || 'syntax');
};
Syntax.prototype.constructor = Syntax;
Syntax.prototype.toString = function() {
    if (this.__name__) {
        return `#<syntax:${this.__name__}>`;
    }
    return '#<syntax>';
};
// ----------------------------------------------------------------------
// :: SRFI-139
// ----------------------------------------------------------------------
class SyntaxParameter {
    constructor(syntax) {
        read_only(this, '_syntax', syntax, { hidden: true });
        read_only(this._syntax, '_param', true, { hidden: true });
    }
}
Syntax.Parameter = SyntaxParameter;
// ----------------------------------------------------------------------
// :: for usage in syntax-rule when pattern match it will return
// :: list of bindings from code that match the pattern
// :: TODO detect cycles
// ----------------------------------------------------------------------
function extract_patterns(pattern, code, symbols, ellipsis_symbol, scope = {}) {
    var bindings = {
        '...': {
            symbols: { }, // symbols ellipsis (x ...)
            lists: [ ]
        },
        symbols: { }
    };
    const { expansion, define } = scope;
    // pattern_names parameter is used to distinguish
    // multiple matches of ((x ...) ...) against ((1 2 3) (1 2 3))
    // in loop we add x to the list so we know that this is not
    // duplicated ellipsis symbol
    log(symbols);
    /* eslint-disable complexity */
    function traverse(pattern, code, pattern_names = [], ellipsis = false) {
        log({
            code,
            pattern
        });
        if (is_atom(pattern) && !(pattern instanceof LSymbol)) {
            return same_atom(pattern, code);
        }
        if (pattern instanceof LSymbol) {
            const literal = pattern.literal(); // TODO: literal() may be SLOW
            if (symbols.includes(literal)) {
                if (!LSymbol.is(code, literal) && !LSymbol.is(pattern, code)) {
                    return false;
                }
                const ref = expansion.ref(literal);
                return !ref || ref === define || ref === global_env;
            }
        }
        if (Array.isArray(pattern) && Array.isArray(code)) {
            log('<<< a 1');
            if (pattern.length === 0 && code.length === 0) {
                return true;
            }
            if (LSymbol.is(pattern[1], ellipsis_symbol)) {
                if (pattern[0] instanceof LSymbol) {
                    const name = pattern[0].valueOf();
                    log('<<< a 2 ' + ellipsis);
                    if (ellipsis) {
                        const count = code.length - 2;
                        const array_head = count > 0 ? code.slice(0, count) : code;
                        const as_list = Pair.fromArray(array_head, false);
                        if (!bindings['...'].symbols[name]) {
                            bindings['...'].symbols[name] = new Pair(as_list, nil);
                        } else {
                            bindings['...'].symbols[name].append(new Pair(as_list, nil));
                        }
                    } else {
                        bindings['...'].symbols[name] = Pair.fromArray(code, false);
                    }
                } else if (Array.isArray(pattern[0])) {
                    log('<<< a 3');
                    const names = [...pattern_names];
                    let node = code;
                    if (!code.every(node => traverse(pattern[0], node, names, true))) {
                        return false;
                    }
                }
                if (pattern.length > 2) {
                    const pat = pattern.slice(2);
                    return traverse(pat, code.slice(-pat.length), pattern_names, ellipsis);
                }
                return true;
            }
            const first = traverse(pattern[0], code[0], pattern_names, ellipsis);
            log({first, pattern: pattern[0], code: code[0]});
            const rest = traverse(pattern.slice(1), code.slice(1), pattern_names, ellipsis);
            log({first, rest});
            return first && rest;
        }
        // pattern (a b (x ...)) and (x ...) match nil
        if (is_pair(pattern) &&
            is_pair(pattern.car) &&
            is_pair(pattern.car.cdr) &&
            LSymbol.is(pattern.car.cdr.car, ellipsis_symbol)) {
            log('>> 0');
            if (is_nil(code)) {
                log({ pattern });
                if (pattern.car.car instanceof LSymbol) {
                    let name = pattern.car.car.valueOf();
                    if (bindings['...'].symbols[name]) {
                        throw new Error('syntax: named ellipsis can only ' +
                                        'appear onces');
                    }
                    bindings['...'].symbols[name] = code;
                }
            }
        }
        if (is_pair(pattern) &&
            is_pair(pattern.cdr) &&
            LSymbol.is(pattern.cdr.car, ellipsis_symbol)) {
            // pattern (... ???) - SRFI-46
            if (!is_nil(pattern.cdr.cdr)) {
                if (is_pair(pattern.cdr.cdr)) {
                    // if we have (x ... a b) we need to remove two from the end
                    const list_len = pattern.cdr.cdr.length();
                    if (!is_pair(code)) {
                        return false;
                    }
                    let code_len = code.length();
                    let list = code;
                    while (code_len - 1 > list_len) {
                        list = list.cdr;
                        code_len--;
                    }
                    const rest = list.cdr;
                    list.cdr = nil;
                    if (!traverse(pattern.cdr.cdr, rest, pattern_names, ellipsis)) {
                        return false;
                    }
                }
            }
            if (pattern.car instanceof LSymbol) {
                let name = pattern.car.__name__;
                if (bindings['...'].symbols[name] &&
                    !pattern_names.includes(name) && !ellipsis) {
                    throw new Error('syntax: named ellipsis can only appear onces');
                }
                log('>> 1');
                if (is_nil(code)) {
                    log('>> 2');
                    if (ellipsis) {
                        log('NIL');
                        bindings['...'].symbols[name] = nil;
                    } else {
                        log('NULL');
                        bindings['...'].symbols[name] = null;
                    }
                } else if (is_pair(code) && (is_pair(code.car) || is_nil(code.car))) {
                    log('>> 3 ' + ellipsis);
                    if (ellipsis) {
                        if (bindings['...'].symbols[name]) {
                            let node = bindings['...'].symbols[name];
                            if (is_nil(node)) {
                                node = new Pair(nil, new Pair(code, nil));
                            } else {
                                node = node.append(new Pair(code, nil));
                            }
                            bindings['...'].symbols[name] = node;
                        } else {
                            bindings['...'].symbols[name] = new Pair(code, nil);
                        }
                    } else {
                        log('>> 4');
                        bindings['...'].symbols[name] = new Pair(code, nil);
                    }
                } else {
                    log('>> 6');
                    if (is_pair(code)) {
                        // cons (a . b) => (var ... . x)
                        if (!is_pair(code.cdr) && !is_nil(code.cdr)) {
                            log('>> 7 (b)');
                            if (is_nil(pattern.cdr.cdr)) {
                                return false;
                            } else if (!bindings['...'].symbols[name]) {
                                bindings['...'].symbols[name] = new Pair(code.car, nil);
                                return traverse(pattern.cdr.cdr, code.cdr);
                            }
                        }
                        // code as improper list
                        const last_pair = code.last_pair();
                        if (!is_nil(last_pair.cdr)) {
                            if (is_nil(pattern.cdr.cdr)) {
                                // case (a ...) for (a b . x)
                                return false;
                            } else {
                                // case (a ... . b) for (a b . x)
                                const copy = code.clone();
                                copy.last_pair().cdr = nil;
                                bindings['...'].symbols[name] = copy;
                                return traverse(pattern.cdr.cdr, last_pair.cdr);
                            }
                        }
                        log('>> 7 ' + ellipsis);
                        pattern_names.push(name);
                        if (!bindings['...'].symbols[name]) {
                            bindings['...'].symbols[name] = new Pair(
                                code,
                                nil
                            );
                        } else {
                            const node = bindings['...'].symbols[name];
                            bindings['...'].symbols[name] = node.append(
                                new Pair(
                                    code,
                                    nil
                                )
                            );
                        }
                        log({ IIIIII: bindings['...'].symbols[name] });
                    } else if (pattern.car instanceof LSymbol &&
                               is_pair(pattern.cdr) &&
                               LSymbol.is(pattern.cdr.car, ellipsis_symbol)) {
                        // empty ellipsis with rest  (a b ... . d) #290
                        log('>> 8');
                        bindings['...'].symbols[name] = null;
                        return traverse(pattern.cdr.cdr, code);
                    } else {
                        log('>> 9');
                        return false;
                        //bindings['...'].symbols[name] = code;
                    }
                }
                return true;
            } else if (is_pair(pattern.car)) {
                var names = [...pattern_names];
                if (is_nil(code)) {
                    log('>> 10');
                    bindings['...'].lists.push(nil);
                    return true;
                }
                log('>> 11');
                let node = code;
                while (is_pair(node)) {
                    if (!traverse(pattern.car, node.car, names, true)) {
                        return false;
                    }
                    node = node.cdr;
                }
                return true;
            } if (Array.isArray(pattern.car) ) {
                var names = [...pattern_names];
                let node = code;
                while (is_pair(node)) {
                    if (!traverse(pattern.car, node.car, names, true)) {
                        return false;
                    }
                    node = node.cdr;
                }
                return true;
            }
            return false;
        }
        if (pattern instanceof LSymbol) {
            if (LSymbol.is(pattern, ellipsis_symbol)) {
                throw new Error('syntax: invalid usage of ellipsis');
            }
            log('>> 12');
            const name = pattern.__name__;
            if (symbols.includes(name)) {
                return true;
            }
            if (ellipsis) {
                log(bindings['...'].symbols[name]);
                bindings['...'].symbols[name] ??= [];
                bindings['...'].symbols[name].push(code);
            } else {
                bindings.symbols[name] = code;
            }
            return true;
        }
        if (is_pair(pattern) && is_pair(code)) {
            log('>> 13');
            log({
                a: 13,
                code,
                pattern
            });
            if (is_nil(code.cdr)) {
                log('>> 13 (a)');
                // last item in in call using in recursive calls on
                // last element of the list
                // case of pattern (p . rest) and code (0)
                var rest_pattern = pattern.car instanceof LSymbol &&
                    pattern.cdr instanceof LSymbol;
                if (rest_pattern) {
                    // fix for SRFI-26 in recursive call of (b) ==> (<> . x)
                    // where <> is symbol
                    if (!traverse(pattern.car, code.car, pattern_names, ellipsis)) {
                        return false;
                    }
                    log('>> 14');
                    let name = pattern.cdr.valueOf();
                    if (!(name in bindings.symbols)) {
                        bindings.symbols[name] = nil;
                    }
                    name = pattern.car.valueOf();
                    if (!(name in bindings.symbols)) {
                        bindings.symbols[name] = code.car;
                    }
                    return true;
                }
            }
            log({
                pattern,
                code
            });
            // case (x y) ===> (var0 var1 ... warn) where var1 match nil
            if (is_pair(pattern.cdr) &&
                is_pair(pattern.cdr.cdr) &&
                pattern.cdr.car instanceof LSymbol &&
                LSymbol.is(pattern.cdr.cdr.car, ellipsis_symbol) &&
                is_pair(pattern.cdr.cdr.cdr) &&
                !LSymbol.is(pattern.cdr.cdr.cdr.car, ellipsis_symbol) &&
                traverse(pattern.car, code.car, pattern_names, ellipsis) &&
                traverse(pattern.cdr.cdr.cdr, code.cdr, pattern_names, ellipsis)) {
                const name = pattern.cdr.car.__name__;
                log({
                    pattern,
                    code,
                    name
                });
                if (symbols.includes(name)) {
                    return true;
                }
                bindings['...'].symbols[name] = null;
                return true;
            }
            log('recur');
            log({
                pattern,
                code
            });
            const car = traverse(pattern.car, code.car, pattern_names, ellipsis);
            const cdr = traverse(pattern.cdr, code.cdr, pattern_names, ellipsis);
            log({
                $car_code: code.car,
                $car_pattern: pattern.car,
                car,
                $cdr_code: code.cdr,
                $cdr_pattern: pattern.cdr,
                cdr
            });
            if (car && cdr) {
                return true;
            }
        } else if (is_nil(pattern) && (is_nil(code) || code === undefined)) {
            // undefined is case when you don't have body ...
            // and you do recursive call
            return true;
        } else if (is_pair(pattern.car) &&
                   LSymbol.is(pattern.car.car, ellipsis_symbol)) {
            // pattern (...)
            throw new Error('syntax: invalid usage of ellipsis');
        } else {
            return false;
        }
    }
    /* eslint-enable complexity */
    if (traverse(pattern, code)) {
        return bindings;
    }
}
// ----------------------------------------------------------------------
// :: This function is called after syntax-rules macro is evaluated
// :: and if there are any gensyms added by macro they need to restored
// :: to original symbols
// ----------------------------------------------------------------------
function clear_gensyms(node, gensyms) {
    function traverse(node) {
        if (is_pair(node)) {
            if (!gensyms.length) {
                return node;
            }
            const car = traverse(node.car);
            const cdr = traverse(node.cdr);
            // TODO: check if it's safe to modify the list
            //       some funky modify of code can happen in macro
            return new Pair(car, cdr);
        } else if (node instanceof LSymbol) {
            var replacement = gensyms.find((gensym) => {
                return gensym.gensym === node;
            });
            if (replacement) {
                return LSymbol(replacement.name);
            }
            return node;
        } else {
            return node;
        }
    }
    return traverse(node);
}
// ----------------------------------------------------------------------
function transform_syntax(options = {}) {
    const {
        bindings,
        expr,
        scope,
        symbols,
        names,
        ellipsis: ellipsis_symbol } = options;
    const gensyms = {};
    function valid_symbol(symbol) {
        if (symbol instanceof LSymbol) {
            return true;
        }
        return ['string', 'symbol'].includes(typeof symbol);
    }
    function transform(symbol) {
        if (!valid_symbol(symbol)) {
            const t = type(symbol);
            throw new Error(`syntax: internal error, need symbol got ${t}`);
        }
        const name = symbol.valueOf();
        if (name === ellipsis_symbol) {
            throw new Error('syntax: internal error, ellipis not transformed');
        }
        // symbols are gensyms from nested syntax-rules
        var n_type = typeof name;
        if (['string', 'symbol'].includes(n_type)) {
            if (name in bindings.symbols) {
                return bindings.symbols[name];
            } else if (n_type === 'string' && name.match(/\./)) {
                // calling method on pattern symbol #83
                const parts = name.split('.');
                const first = parts[0];
                if (first in bindings.symbols) {
                    return Pair.fromArray([
                        LSymbol('.'),
                        bindings.symbols[first]
                    ].concat(parts.slice(1).map(x => LString(x))));
                }
            }
        }
        if (symbols.includes(name)) {
            return symbol;
        }
        return rename(name, symbol);
    }
    function rename(name, symbol) {
        if (!gensyms[name]) {
            const ref = scope.ref(name);
            // nested syntax-rules needs original symbol to get renamed again
            if (typeof name === 'symbol' && !ref) {
                name = symbol.literal();
            }
            if (gensyms[name]) {
                return gensyms[name];
            }
            const gensym_name = gensym(name);
            if (ref) {
                const value = scope.get(name);
                scope.set(gensym_name, value);
            } else {
                const value = scope.get(name, { throwError: false });
                // value is not in scope, but it's JavaScript object
                if (typeof value !== 'undefined') {
                    scope.set(gensym_name, value);
                }
            }
            // keep names so they can be restored after evaluation
            // if there are free symbols as output
            // kind of hack
            names.push({
                name, gensym: gensym_name
            });
            gensyms[name] = gensym_name;
            // we need to check if name is a string, because it can be
            // gensym from nested syntax-rules
            if (typeof name === 'string' && name.match(/\./)) {
                const [first, ...rest] = name.split('.').filter(Boolean);
                // save JavaScript dot notation for Env::get
                if (gensyms[first]) {
                    hidden_prop(gensym_name, '__object__', [gensyms[first], ...rest]);
                }
            }
        }
        return gensyms[name];
    }
    function transform_ellipsis_expr(expr, bindings, state, next = () => {}) {
        const { nested } = state;
        log({ bindings, expr });
        if (Array.isArray(expr) && !expr.length) {
            return expr;
        }
        if (expr instanceof LSymbol) {
            let name = expr.valueOf();
            if (is_gensym(expr) && !bindings[name]) {
               // name = expr.literal();
            }
            log('[t 1');
            if (bindings[name]) {
                if (is_pair(bindings[name])) {
                    const { car, cdr } = bindings[name];
                    if (nested) {
                        const { car: caar, cdr: cadr } = car;
                        if (!is_nil(cadr)) {
                            next(name, new Pair(cadr, nil));
                        }
                        return caar;
                    }
                    if (!is_nil(cdr)) {
                        next(name, cdr);
                    }
                    return car;
                } else if (bindings[name] instanceof Array) {
                    next(name, bindings[name].slice(1));
                    return bindings[name][0];
                }
            }
            return transform(expr);
        }
        const is_array = Array.isArray(expr);
        if (is_pair(expr) || is_array) {
            const first = is_array ? expr[0] : expr.car;
            const second = is_array ? expr[1] : is_pair(expr.cdr) && expr.cdr.car;
            if (first instanceof LSymbol &&
                LSymbol.is(second, ellipsis_symbol)) {
                let rest = is_array ? expr.slice(2) : expr.cdr.cdr;
                log('[t 2');
                const name = first.valueOf();
                const item = bindings[name];
                if (item === null) {
                    return;
                } else if (item) {
                    log({ name, binding: bindings[name] });
                    if (is_pair(item)) {
                        log('[t 2 Pair ' + nested);
                        const { car, cdr } = item;
                        const rest_expr = is_array ? expr.slice(2) : expr.cdr.cdr;
                        if (nested) {
                            if (!is_nil(cdr)) {
                                log('|| next 1');
                                next(name, cdr);
                            }
                            if ((is_array && rest_expr.length) || (!is_nil(rest_expr) && !is_array)) {
                                const rest = transform_ellipsis_expr(rest_expr, bindings, state, next);
                                if (is_array) {
                                    return car.concat(rest);
                                } else if (is_pair(car)) {
                                    return car.append(rest);
                                } else {
                                    log('UNKNOWN');
                                }
                            }
                            return car;
                        } else if (is_pair(car)) {
                            if (!is_nil(car.cdr)) {
                                log('|| next 2');
                                next(name, new Pair(car.cdr, cdr));
                            }
                            return car.car;
                        } else if (is_nil(cdr)) {
                            return car;
                        } else {
                            const last_pair = expr.last_pair();
                            if (last_pair.cdr instanceof LSymbol) {
                                log('|| next 3');
                                next(name, item.last_pair());
                                return car;
                            }
                        }
                    } else if (item instanceof Array) {
                        log('[t 2 Array ' + nested);
                        if (nested) {
                            next(name, item.slice(1));
                            return Pair.fromArray(item);
                        } else {
                            const rest = item.slice(1);
                            if (rest.length) {
                                next(name, rest);
                            }
                            return item[0];
                        }
                    } else {
                        return item;
                    }
                }
            }
            log('[t 3 recur ', expr);
            const rest_expr = is_array ? expr.slice(1) : expr.cdr;
            const head = transform_ellipsis_expr(first, bindings, state, next);
            const rest = transform_ellipsis_expr(rest_expr, bindings, state, next);
            log( { head, rest });
            if (is_array) {
                return [head].concat(rest);
            }
            return new Pair(
                head,
                rest
            );
        }
        return expr;
    }
    function have_binding(biding, skip_nulls) {
        const values = Object.values(biding);
        const symbols = Object.getOwnPropertySymbols(biding);
        if (symbols.length) {
            values.push(...symbols.map(x => biding[x]));
        }
        return values.length && values.every(x => {
            if (x === null) {
                return !skip_nulls;
            }
            return is_pair(x) || is_nil(x) || (Array.isArray(x) && x.length);
        });
    }
    function get_names(object) {
        return Object.keys(object).concat(Object.getOwnPropertySymbols(object));
    }
    /* eslint-disable complexity */
    function traverse(expr, { disabled } = {}) {
        log('traverse>> ', expr);
        const is_array = Array.isArray(expr);
        if (is_array && expr.length === 0) {
            return expr;
        }
        if (is_pair(expr) || is_array) {
            log('>> 0');
            const first = is_array ? expr[0] : expr.car;
            let second, rest_second;
            if (is_array) {
                second = expr[1];
                rest_second = expr.slice(2);
            } else if (is_pair(expr.cdr)) {
                second = expr.cdr.car;
                rest_second = expr.cdr.cdr;
            }
            log({ first, second, rest_second });
            // escape ellispsis from R7RS e.g. (... ...)
            if (!disabled && is_pair(first) &&
                LSymbol.is(first.car, ellipsis_symbol)) {
                return new Pair(
                    first.cdr.car,
                    traverse(expr.cdr)
                );
            }
            if (second && LSymbol.is(second, ellipsis_symbol) && !disabled) {
                log('>> 1');
                const symbols = bindings['...'].symbols;
                // skip expand list of pattern was (x y ... z)
                // and code was (x z) so y == null
                const values = Object.values(symbols);
                if (values.length && values.every(x => x === null)) {
                    log('>>> 1 (a)');
                    return traverse(rest_second, { disabled });
                }
                var keys = get_names(symbols);
                // case of list as first argument ((x . y) ...) or (x ... ...)
                // we need to recursively process the list
                // if we have pattern (_ (x y z ...) ...) and code (foo (1 2) (1 2))
                // x an y will be arrays of [1 1] and [2 2] and z will be array
                // of rest, x will also have it's own mapping to 1 and y to 2
                // in case of usage outside of ellipsis list e.g.: (x y)
                var is_spread = first instanceof LSymbol &&
                    LSymbol.is(rest_second.car, ellipsis_symbol);
                if (is_pair(first) || is_spread) {
                    log('>>> 1 (b)');
                    // lists is free ellipsis on pairs ((???) ...)
                    // TODO: will this work in every case? Do we need to handle
                    // nesting here?
                    if (is_nil(bindings['...'].lists[0])) {
                        if (!is_spread) {
                            return traverse(rest_second, { disabled });
                        }
                        log(rest_second);
                        return nil;
                    }
                    var new_expr = first;
                    if (is_spread) {
                        log('>>> 1 (c)'); // TODO: array
                        new_expr = new Pair(
                            first,
                            new Pair(
                                second,
                                nil
                            )
                        );
                    }
                    log('>> 2');
                    let result;
                    if (keys.length) {
                        log('>> 2 (a)');
                        let bind = { ...symbols };
                        result = is_array ? [] : nil;
                        while (true) {
                            log({ bind });
                            if (!have_binding(bind)) {
                                break;
                            }
                            const new_bind = {};
                            const next = (key, value) => {
                                // ellipsis decide if what should be the next value
                                // there are two cases ((a . b) ...) and (a ...)
                                new_bind[key] = value;
                            };
                            const car = transform_ellipsis_expr(
                                new_expr,
                                bind,
                                { nested: true },
                                next
                            );
                            // undefined can be null caused by null binding
                            // on empty ellipsis
                            if (car !== undefined) {
                                if (is_spread) {
                                    if (is_array) {
                                        if (Array.isArray(car)) {
                                            result.push(...car);
                                        } else {
                                            log('ZONK {1}');
                                        }
                                    } else {
                                        if (is_nil(result)) {
                                            result = car;
                                        } else {
                                            result = result.append(car);
                                        }
                                    }
                                } else if (is_array) {
                                    result.push(car);
                                } else {
                                    result = new Pair(
                                        car,
                                        result
                                    );
                                }
                            }
                            bind = new_bind;
                        }
                        if (!is_nil(result) && !is_spread && !is_array) {
                            result = result.reverse();
                        }
                        // case of (list) ... (rest code)
                        if (is_array) {
                            if (rest_second) {
                                log({ rest_second, expr });
                                const rest = traverse(rest_second, { disabled });
                                return result.concat(rest);
                            }
                            return result;
                        }
                        if (!is_nil(expr.cdr.cdr) &&
                            !LSymbol.is(expr.cdr.cdr.car, ellipsis_symbol)) {
                            const rest = traverse(expr.cdr.cdr, { disabled });
                            return result.append(rest);
                        }
                        return result;
                    } else {
                        log('>> 3');
                        const car = transform_ellipsis_expr(first, symbols, {
                            nested: true
                        });
                        if (car) {
                            return new Pair(
                                car,
                                nil
                            );
                        }
                        return nil;
                    }
                } else if (first instanceof LSymbol) {
                    log('>> 4');
                    if (LSymbol.is(rest_second.car, ellipsis_symbol)) {
                        // case (x ... ...)
                        log('>> 4 (a)');
                    } else {
                        log('>> 4 (b)');
                    }
                    // case: (x ...)
                    let name = first.__name__;
                    let bind = { [name]: symbols[name] };
                    log({bind});
                    const is_null = symbols[name] === null;
                    let result = is_array ? [] : nil;
                    while (true) {
                        if (!have_binding(bind, true)) {
                            log({ bind });
                            break;
                        }
                        const new_bind = {};
                        const next = (key, value) => {
                            new_bind[key] = value;
                        };
                        const value = transform_ellipsis_expr(
                            expr,
                            bind,
                            { nested: false },
                            next
                        );
                        log({ value });
                        if (typeof value !== 'undefined') {
                            if (is_array) {
                                result.push(value);
                            } else {
                                result = new Pair(
                                    value,
                                    result
                                );
                            }
                        }
                        bind = new_bind;
                    }
                    if (!is_nil(result) && !is_array) {
                        result = result.reverse();
                    }
                    // case if (x ... y ...) second spread is not processed
                    // and (??? . x) last symbol
                    // by ellipsis transformation
                    if (is_pair(expr.cdr)) {
                        if (is_pair(expr.cdr.cdr) ||
                            expr.cdr.cdr instanceof LSymbol) {
                            const node = traverse(expr.cdr.cdr, { disabled });
                            log({node});
                            if (is_null) {
                                return node;
                            }
                            if (is_nil(result)) {
                                result = node;
                            } else {
                                result.append(node);
                            }
                            log({result, node});
                        }
                    }
                    log('<<<< 2');
                    return result;
                }
            }
            const head = traverse(first, { disabled });
            let rest;
            let is_syntax;
            if ((first instanceof LSymbol)) {
                const value = scope.get(first, { throwError: false });
                is_syntax = value instanceof Macro &&
                    value.__name__ === 'syntax-rules';
            }
            if (is_syntax) {
                if (expr.cdr.car instanceof LSymbol) {
                    rest = new Pair(
                        traverse(expr.cdr.car, { disabled }),
                        new Pair(
                            expr.cdr.cdr.car,
                            traverse(expr.cdr.cdr.cdr, { disabled })
                        )
                    );
                } else {
                    rest = new Pair(
                        expr.cdr.car,
                        traverse(expr.cdr.cdr, { disabled })
                    );
                }
                log('REST >>>> ', rest);
            } else {
                rest = traverse(expr.cdr, { disabled });
            }
            log({
                a: true,
                car: toString(expr.car),
                cdr: toString(expr.cdr),
                head: toString(head),
                rest: toString(rest)
            });
            return new Pair(
                head,
                rest
            );
        }
        if (expr instanceof LSymbol) {
            if (disabled && LSymbol.is(expr, ellipsis_symbol)) {
                return expr;
            }
            const symbols = Object.keys(bindings['...'].symbols);
            const name = expr.literal(); // TODO: slow
            if (symbols.includes(name)) {
                const msg = `missing ellipsis symbol next to name \`${name}'`;
                throw new Error(`syntax-rules: ${msg}`);
            }
            const value = transform(expr);
            if (typeof value !== 'undefined') {
                return value;
            }
        }
        return expr;
    }
    return traverse(expr, {});
}
// ----------------------------------------------------------------------
// :: Check for nullish values
// ----------------------------------------------------------------------
function is_null(value) {
    return is_undef(value) || is_nil(value) || value === null;
}
// ----------------------------------------------------------------------
function is_nil(value) {
    return value === nil;
}
// ----------------------------------------------------------------------
function is_function(o) {
    return typeof o === 'function' && typeof o.bind === 'function';
}
// ----------------------------------------------------------------------------
function is_directive(token) {
    return directives.includes(token);
}
// ----------------------------------------------------------------------------
function is_false(o) {
    return o === false || o === null;
}
// ----------------------------------------------------------------------------
function is_string(o) {
    return typeof o === 'string';
}
// ----------------------------------------------------------------------------
function is_prototype(obj) {
    return obj &&
        typeof obj === 'object' &&
        obj.hasOwnProperty &&
        obj.hasOwnProperty("constructor") &&
        typeof obj.constructor === "function" &&
        obj.constructor.prototype === obj;
}
// ----------------------------------------------------------------------
function is_continuation(o) {
    return o instanceof Continuation;
}
// ----------------------------------------------------------------------
function is_context(o) {
    return o instanceof LambdaContext;
}
// ----------------------------------------------------------------------
function is_parameter(o) {
    return o instanceof Parameter;
}
// ----------------------------------------------------------------------
function is_pair(o) {
    return o instanceof Pair;
}
// ----------------------------------------------------------------------
function is_env(o) {
    return o instanceof Environment;
}
// ----------------------------------------------------------------------
function is_callable(o) {
    return is_function(o) || is_continuation(o) || is_parameter(o) || is_macro(o);s
}
// ----------------------------------------------------------------------
function is_macro(o) {
    return o instanceof Macro || o instanceof SyntaxParameter;
}
// ----------------------------------------------------------------------
function is_promise(o) {
    if (o instanceof QuotedPromise) {
        return false;
    }
    if (o instanceof Promise) {
        return true;
    }
    return !!o && is_function(o.then);
}
// ----------------------------------------------------------------------
function is_undef(value) {
    return typeof value === 'undefined';
}
// -------------------------------------------------------------------------
function is_iterator(obj, symbol) {
    if (has_own_symbol(obj, symbol) || has_own_symbol(obj.__proto__, symbol)) {
        return is_function(obj[symbol]);
    }
}
// -------------------------------------------------------------------------
function is_instance(obj) {
    if (!obj) {
        return false;
    }
    if (typeof obj !== 'object') {
        return false;
    }
    // __instance__ is read only for instances
    if (obj.__instance__) {
        obj.__instance__ = false;
        return obj.__instance__;
    }
    return false;
}
// -------------------------------------------------------------------------
function self_evaluated(obj) {
    var type = typeof obj;
    return ['string', 'function'].includes(type) ||
        typeof obj === 'symbol' ||
        obj instanceof QuotedPromise ||
        obj instanceof LSymbol ||
        obj instanceof LNumber ||
        obj instanceof LString ||
        obj instanceof RegExp;
}
// -------------------------------------------------------------------------
function is_native(obj) {
    return obj instanceof LNumber ||
        obj instanceof LString ||
        obj instanceof LCharacter;
}
// -------------------------------------------------------------------------
function has_own_symbol(obj, symbol) {
    if (obj === null) {
        return false;
    }
    return typeof obj === 'object' &&
        symbol in Object.getOwnPropertySymbols(obj);
}
// ----------------------------------------------------------------------
// :: Function utilities
// ----------------------------------------------------------------------
function box(object) {
    // We only need to box lips data and arrays. Object don't need
    // to be boxed, but values from objects will be boxed when accessed.
    switch (typeof object) {
        case 'string':
            return LString(object);
        case 'bigint':
            return LNumber(object);
        case 'number':
            if (Number.isNaN(object)) {
                return nan;
            } else {
                return LNumber(object);
            }
    }
    return object;
}
// ----------------------------------------------------------------------
function map_object(object, fn) {
    const props = Object.getOwnPropertyNames(object);
    const symbols = Object.getOwnPropertySymbols(object);
    const result = {};
    props.concat(symbols).forEach(key => {
        const value = fn(object[key]);
        result[key] = value;
    });
    return result;
}
// ----------------------------------------------------------------------
function unbox(object) {
    var lips_type = [LString, LNumber, LCharacter].some(x => object instanceof x);
    if (lips_type) {
        return object.valueOf();
    }
    if (object instanceof Array) {
        return object.map(unbox);
    }
    if (object instanceof QuotedPromise) {
        delete object.then;
    }
    if (is_plain_object(object)) {
        return map_object(object, unbox);
    }
    return object;
}
// ----------------------------------------------------------------------
function patch_value(value, context) {
    if (is_pair(value)) {
        value.mark_cycles();
        return quote(value);
    }
    if (is_function(value)) {
        // original function can be restored using unbind function
        // only real JS function require to be bound
        if (context) {
            return bind(value, context);
        }
    }
    return box(value);
}
// ----------------------------------------------------------------------
// :: Function gets original function that was binded with props
// ----------------------------------------------------------------------
function unbind(obj) {
    if (is_bound(obj)) {
        return obj[__fn__];
    }
    return obj;
}
// ----------------------------------------------------------------------
// :: Function binds with context that can be optionally unbind
// :: get original function with unbind
// ----------------------------------------------------------------------
function bind(fn, context) {
    if (fn[Symbol.for('__bound__')]) {
        return fn;
    }
    const bound = fn.bind(context);
    const props = Object.getOwnPropertyNames(fn);
    for (var prop of props) {
        if (filter_fn_names(prop)) {
            try {
                bound[prop] = fn[prop];
            } catch (e) {
                // ignore error from express.js while accessing bodyParser
            }
        }
    }
    hidden_prop(bound, '__fn__', fn);
    hidden_prop(bound, '__context__', context);
    hidden_prop(bound, '__bound__', true);
    if (is_native_function(fn)) {
        hidden_prop(bound, '__native__', true);
    }
    if (is_plain_object(context) && is_lambda(fn)) {
        hidden_prop(bound, '__method__', true);
    }
    bound.valueOf = function() {
        return fn;
    };
    return bound;
}
// ----------------------------------------------------------------------
// Function used to check if function should not get unboxed arguments,
// so you can call Object.getPrototypeOf for lips data types
// this is case, see dir function and #73
// ----------------------------------------------------------------------
function is_object_bound(obj) {
    return is_bound(obj) && obj[Symbol.for('__context__')] === Object;
}
// ----------------------------------------------------------------------
function is_bound(obj) {
    return !!(is_function(obj) && obj[__fn__]);
}
// ----------------------------------------------------------------------
function lips_context(obj) {
    if (is_function(obj)) {
        var context = obj[__context__];
        if (context && (context === lips ||
                        (context.constructor &&
                         context.constructor.__class__))) {
            return true;
        }
    }
    return false;
}
// ----------------------------------------------------------------------
function is_port(obj) {
    return obj instanceof InputPort || obj instanceof OutputPort;
}
// ----------------------------------------------------------------------
function is_port_method(obj) {
    if (is_function(obj)) {
        if (is_port(obj[__context__])) {
            return true;
        }
    }
    return false;
}
// ----------------------------------------------------------------------
// Hidden props
// ----------------------------------------------------------------------
const __context__ = Symbol.for('__context__');
const __fn__ = Symbol.for('__fn__');
const __data__ = Symbol.for('__data__');
const __ref__ = Symbol.for('__ref__');
const __cycles__ = Symbol.for('__cycles__');
const __class__ = Symbol.for('__class__');
const __method__ = Symbol.for('__method__');
const __prototype__ = Symbol.for('__prototype__');
const __lambda__ = Symbol.for('__lambda__');
// ----------------------------------------------------------------------
// :: Function bind fn with context but it also move all props
// :: mostly used for Object function
// ----------------------------------------------------------------------
var exluded_names = ['name', 'length', 'caller', 'callee', 'arguments', 'prototype'];
function filter_fn_names(name) {
    return !exluded_names.includes(name);
}
// ----------------------------------------------------------------------
function hidden_prop(obj, name, value) {
    Object.defineProperty(obj, Symbol.for(name), {
        get: () => value,
        set: () => {},
        configurable: false,
        enumerable: false
    });
}
// ----------------------------------------------------------------------
function set_fn_length(fn, length) {
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
function is_lambda(obj) {
    return obj && obj[__lambda__];
}
// ----------------------------------------------------------------------
function is_method(obj) {
    return obj && obj[__method__];
}
// ----------------------------------------------------------------------
function is_raw_lambda(fn) {
    return is_lambda(fn) && !fn[__prototype__] &&
        !is_method(fn) && !is_port_method(fn);
}
// ----------------------------------------------------------------------
function is_native_function(fn) {
    var native = Symbol.for('__native__');
    return is_function(fn) &&
        fn.toString().match(/\{\s*\[native code\]\s*\}/) &&
        ((fn.name.match(/^bound /) && fn[native] === true) ||
         (!fn.name.match(/^bound /) && !fn[native]));
}
// ----------------------------------------------------------------------
// :: function that return macro for let, let* and letrec
// ----------------------------------------------------------------------
function let_macro(symbol) {
    var name;
    switch (symbol) {
        case Symbol.for('letrec'):
            name = 'letrec';
            break;
        case Symbol.for('let'):
            name = 'let';
            break;
        case Symbol.for('let*'):
            name = 'let*';
            break;
        default:
            throw new Error('Invalid let_macro value');
    }
    return Macro.defmacro(name, function(code, options) {
        let { dynamic_env } = options;
        const { error, macro_expand, use_dynamic } = options
        var args;
        // named let:
        // (let loop ((x 10)) (iter (- x 1))) -> (letrec ((loop (lambda (x) ...
        if (code.car instanceof LSymbol) {
            if (!(is_pair(code.cdr.car) || is_nil(code.cdr.car))) {
                throw new Error('let require list of pairs');
            }
            var params;
            if (is_nil(code.cdr.car)) {
                args = nil;
                params = nil;
            } else {
                params = code.cdr.car.map(pair => pair.car);
                args = code.cdr.car.map(pair => pair.cdr.car);
            }
            const args_name = gensym('args');
            return Pair.fromArray([
                LSymbol('let'),
                [[args_name, Pair(LSymbol('list'), args)]],
                [LSymbol('letrec'),
                 [[code.car, Pair(
                     LSymbol('lambda'),
                     Pair(params, code.cdr.cdr))]],
                 [LSymbol('apply'), code.car, args_name]]
            ]);
        } else if (macro_expand) {
            // Macro.defmacro are special macros that should return lips code
            // here we use evaluate, so we need to check special flag set by
            // macroexpand to prevent evaluation of code in normal let
            return;
        }
        var self = this;
        args = global_env.get('list->array')(code.car);
        var env = self.inherit(name);
        var values, var_body_env;
        if (name === 'let*') {
            var_body_env = env;
        } else if (name === 'let') {
            values = []; // collect potential promises
        }
        var i = 0;
        function exec() {
            var output = new Pair(new LSymbol('begin'), code.cdr);
            return evaluate(output, {
                env,
                dynamic_env: env,
                use_dynamic,
                error
            });
        }
        function check_duplicates(name) {
            if (name in env.__env__) {
                throw new Error(`Duplicated let variable ${name}`);
            }
        }
        return (function loop() {
            var pair = args[i++];
            dynamic_env = name === 'let*' ? env : self;
            if (!pair) {
                if (values && values.length) {
                    var v = values.map(x => x.value);
                    // resolve all promises
                    var promises = v.filter(is_promise);
                    if (promises.length) {
                        return promise_all(v).then((arr) => {
                            for (var i = 0, len = arr.length; i < len; ++i) {
                                const name = values[i].name;
                                check_duplicates(name);
                                env.set(name, arr[i]);
                            }
                        }).then(exec);
                    } else {
                        for (let { name, value } of values) {
                            check_duplicates(name);
                            env.set(name, value);
                        }
                    }
                }
                return exec();
            } else {
                if (name === 'let') {
                    var_body_env = self;
                } else if (name === 'letrec') {
                    var_body_env = env;
                }
                var value = evaluate(pair.cdr.car, {
                    env: var_body_env,
                    dynamic_env,
                    use_dynamic,
                    error
                });
                if (name === 'let*') {
                    var_body_env = env = var_body_env.inherit('let*[' + i + ']');
                }
                if (values) {
                    values.push({ name: pair.car, value });
                    return loop();
                } else {
                    return unpromise(value, function(value) {
                        env.set(pair.car, value);
                        return loop();
                    });
                }
            }
        })();
    });
}
// -------------------------------------------------------------------------
function parallel(name, fn) {
    return new Macro(name, function(code, { use_dynamic, error } = {}) {
        const env = this;
        const dynamic_env = this;
        const results = [];
        let node = code;
        while (is_pair(node)) {
            results.push(evaluate(node.car, { env, dynamic_env, use_dynamic, error }));
            node = node.cdr;
        }
        var havePromises = results.filter(is_promise).length;
        if (havePromises) {
            return promise_all(results).then(fn.bind(this));
        } else {
            return fn.call(this, results);
        }
    });
}
// -------------------------------------------------------------------------
function guard_math_call(fn, ...args) {
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
    return (...args) => {
        return fns.reduce((args, f) => [f.apply(this, args)], args)[0];
    };
}
// -------------------------------------------------------------------------
function compose(...fns) {
    fns.forEach((fn, i) => {
        typecheck('compose', fn, 'function', i + 1);
    });
    return pipe(...fns.reverse());
}
// -------------------------------------------------------------------------
// :: fold functions generator
// -------------------------------------------------------------------------
function fold(name, fold) {
    var self = this;
    return function recur(fn, init, ...lists) {
        typecheck(name, fn, 'function');
        if (lists.some(is_null)) {
            if (typeof init === 'number') {
                return LNumber(init);
            }
            return init;
        } else {
            return fold.call(self, recur, fn, init, ...lists);
        }
    };
}
// -------------------------------------------------------------------------
function limit_math_op(n, fn) {
    // + 1 so it include function in guard_math_call
    return limit(n + 1, curry(guard_math_call, fn));
}
// -------------------------------------------------------------------------
// :: some functional magic
// -------------------------------------------------------------------------
var single_math_op = curry(limit_math_op, 1);
var binary_math_op = curry(limit_math_op, 2);
// -------------------------------------------------------------------------
function reduce_math_op(fn, init = null) {
    return function(...args) {
        if (init !== null) {
            args = [init, ...args];
        }
        return args.reduce(binary_math_op(fn));
    };
}
// -------------------------------------------------------------------------
function curry(fn, ...init_args) {
    typecheck('curry', fn, 'function');
    var len = fn.length;
    return function() {
        var args = init_args.slice();
        // HACK: we use IIFE here to get rid of the name of the function.
        // The JavaScript is smart and add name property to a function
        // if it's assigned to a variable, with IIFE we can get rid of it.
        // we need this so the curried function display as #<procedure>
        const curried = (() => {
            return (...more_args) => {
                args = args.concat(more_args);
                if (args.length >= len) {
                    return fn.apply(this, args);
                } else {
                    return curried;
                }
            };
        })()
        return curried(...arguments);
    };
}
// -------------------------------------------------------------------------
// return function with limited number of arguments
function limit(n, fn) {
    typecheck('limit', fn, 'function', 2);
    return function(...args) {
        return fn(...args.slice(0, n));
    };
}
// -------------------------------------------------------------------------
// :: Character object representation
// -------------------------------------------------------------------------
function LCharacter(char) {
    if (typeof this !== 'undefined' && !(this instanceof LCharacter) ||
        typeof this === 'undefined') {
        return new LCharacter(char);
    }
    if (char instanceof LString) {
        char = char.valueOf();
    }
    var name;
    if (Array.from(char).length > 1) {
        // this is name
        char = char.toLowerCase();
        if (LCharacter.__names__[char]) {
            name = char;
            char = LCharacter.__names__[char];
        } else {
            // this should never happen
            // parser don't allow not defined named characters
            throw new Error('Internal: Unknown named character');
        }
    } else {
        name = LCharacter.__rev_names__[char];
    }
    Object.defineProperty(this, '__char__', {
        value: char,
        enumerable: true
    });
    if (name) {
        Object.defineProperty(this, '__name__', {
            value: name,
            enumerable: true
        });
    }
}
LCharacter.__names__ = characters;
LCharacter.__rev_names__ = {};
Object.keys(LCharacter.__names__).forEach(key => {
    var value = LCharacter.__names__[key];
    LCharacter.__rev_names__[value] = key;
});
LCharacter.prototype.toUpperCase = function() {
    return LCharacter(this.__char__.toUpperCase());
};
LCharacter.prototype.toLowerCase = function() {
    return LCharacter(this.__char__.toLowerCase());
};
LCharacter.prototype.toString = function() {
    return '#\\' + (this.__name__ || this.__char__);
};
LCharacter.prototype.valueOf = LCharacter.prototype.serialize = function() {
    return this.__char__;
};
// -------------------------------------------------------------------------
// :: String wrapper that handle copy and in place change
// -------------------------------------------------------------------------
function LString(string) {
    if (typeof this !== 'undefined' && !(this instanceof LString) ||
        typeof this === 'undefined') {
        return new LString(string);
    }
    if (string instanceof Array) {
        this.__string__ = string.map((x, i) => {
            typecheck('LString', x, 'character', i + 1);
            return x.toString();
        }).join('');
    } else {
        this.__string__ = string.valueOf();
    }
}
{
    const ignore = ['length', 'constructor'];
    const _keys = Object.getOwnPropertyNames(String.prototype).filter(name => {
        return !ignore.includes(name);
    });
    const wrap = (fn) => function(...args) {
        return fn.apply(this.__string__, args);
    };
    for (let key of _keys) {
        LString.prototype[key] = wrap(String.prototype[key]);
    }
}
LString.prototype[Symbol.iterator] = function*() {
    const chars = Array.from(this.__string__);
    for (const char of chars) {
        yield LCharacter(char);
    }
};
LString.prototype.serialize = function() {
    return this.valueOf();
};
LString.isString = function(x) {
    return x instanceof LString || typeof x === 'string';
};
LString.prototype.freeze = function() {
    const string = this.__string__;
    delete this.__string__;
    read_only(this, '__string__', string);
};
LString.prototype.get = function(n) {
    typecheck('LString::get', n, 'number');
    return Array.from(this.__string__)[n.valueOf()];
};
LString.prototype.cmp = function(string) {
    typecheck('LString::cmp', string, 'string');
    var a = this.valueOf();
    var b = string.valueOf();
    if (a < b) {
        return -1;
    } else if (a === b) {
        return 0;
    } else {
        return 1;
    }
};
LString.prototype.lower = function() {
    return LString(this.__string__.toLowerCase());
};
LString.prototype.upper = function() {
    return LString(this.__string__.toUpperCase());
};
LString.prototype.set = function(n, char) {
    typecheck('LString::set', n, 'number');
    typecheck('LString::set', char, ['string', 'character']);
    n = n.valueOf();
    if (char instanceof LCharacter) {
        char = char.__char__;
    }
    var string = [];
    if (n > 0) {
        string.push(this.__string__.substring(0, n));
    }
    string.push(char);
    if (n < this.__string__.length - 1) {
        string.push(this.__string__.substring(n + 1));
    }
    this.__string__ = string.join('');
};
Object.defineProperty(LString.prototype, "length", {
    get: function() {
        return this.__string__.length;
    }
});
LString.prototype.clone = function() {
    return LString(this.valueOf());
};
LString.prototype.fill = function(char) {
    typecheck('LString::fill', char, ['string', 'character']);
    if (char instanceof LCharacter) {
        char = char.valueOf();
    }
    var len = this.__string__.length;
    this.__string__ = char.repeat(len);
};
// -------------------------------------------------------------------------
// :: Number wrapper that handle BigNumbers
// -------------------------------------------------------------------------
function LNumber(n, force = false) {
    if (n instanceof LNumber) {
        return n;
    }
    if (typeof this !== 'undefined' && !(this instanceof LNumber) ||
        typeof this === 'undefined') {
        return new LNumber(n, force);
    }
    if (typeof n === 'undefined') {
        throw new Error('Invalid LNumber constructor call');
    }
    var _type = LNumber.getType(n);
    if (LNumber.types[_type]) {
        return LNumber.types[_type](n, force);
    }
    var parsable = n instanceof Array && LString.isString(n[0]) &&
        LNumber.isNumber(n[1]);
    if (n instanceof LNumber) {
        return LNumber(n.value);
    }
    if (!LNumber.isNumber(n) && !parsable) {
        throw new Error(`You can't create LNumber from ${type(n)}`);
    }
    // prevent infinite loop https://github.com/indutny/bn.js/issues/186
    if (n === null) {
        n = 0;
    }
    var value;
    if (parsable) {
        var [str, radix] = n;
        if (str instanceof LString) {
            str = str.valueOf();
        }
        if (radix instanceof LNumber) {
            radix = radix.valueOf();
        }
        var sign = str.match(/^([+-])/);
        var minus = false;
        if (sign) {
            str = str.replace(/^[+-]/, '');
            if (sign[1] === '-') {
                minus = true;
            }
        }
    }
    if (Number.isNaN(n)) {
        return LFloat(n);
    } else if (parsable && Number.isNaN(parseInt(str, radix))) {
        return nan;
    } else if (typeof BigInt !== 'undefined') {
        if (typeof n !== 'bigint') {
            if (parsable) {
                let prefix;
                // default number base (radix) supported by BigInt constructor
                switch (radix) {
                    case 8:
                        prefix = '0o';
                        break;
                    case 16:
                        prefix = '0x';
                        break;
                    case 2:
                        prefix = '0b';
                        break;
                    case 10:
                        prefix = '';
                        break;
                }
                if (typeof prefix === 'undefined') {
                    // non standard radix we convert by hand
                    var n_radix = BigInt(radix);
                    value = [...str].map((x, i) => {
                        return BigInt(parseInt(x, radix)) * pow(n_radix, BigInt(i));
                    }).reduce((a, b) => a + b);
                } else {
                    value = BigInt(prefix + str);
                }
            } else {
                value = BigInt(n);
            }
            if (minus) {
                value *= BigInt(-1);
            }
        } else {
            value = n;
        }
        return LBigInteger(value, true);
    } else if (typeof BN !== 'undefined' && !(n instanceof BN)) {
        if (n instanceof Array) {
            return LBigInteger(new BN(...n));
        }
        return LBigInteger(new BN(n));
    } else if (parsable) {
        this.constant(parseInt(str, radix), 'integer');
    } else {
        this.constant(n, 'integer');
    }
}
// -------------------------------------------------------------------------
LNumber.prototype.constant = function(value, type) {
    Object.defineProperty(this, '__value__', {
        value,
        enumerable: true
    });
    Object.defineProperty(this, '__type__', {
        value: type,
        enumerable: true
    });
};
// -------------------------------------------------------------------------
LNumber.types = {
    float: function(n, force = false) {
        return new LFloat(n, force);
    },
    complex: function(n, force = false) {
        if (!LNumber.isComplex(n)) {
            n = { im: 0, re: n };
        }
        return new LComplex(n, force);
    },
    rational: function(n, force = false) {
        if (!LNumber.isRational(n)) {
            n = { num: n, denom: 1 };
        }
        return new LRational(n, force);
    }
};
// -------------------------------------------------------------------------
LNumber.prototype.serialize = function() {
    return this.__value__;
};
// -------------------------------------------------------------------------
LNumber.prototype.isNaN = function() {
    return Number.isNaN(this.__value__);
};
// -------------------------------------------------------------------------
LNumber.prototype.gcd = function(b) {
    // ref: https://rosettacode.org/wiki/Greatest_common_divisor#JavaScript
    var a = this.abs();
    b = b.abs();
    if (b.cmp(a) === 1) {
        var temp = a;
        a = b;
        b = temp;
    }
    while (true) {
        a = a.rem(b);
        if (a.cmp(0) === 0) {
            return b;
        }
        b = b.rem(a);
        if (b.cmp(0) === 0) {
            return a;
        }
    }
};
// -------------------------------------------------------------------------
LNumber.isFloat = function isFloat(n) {
    return n instanceof LFloat || (Number(n) === n && n % 1 !== 0);
};
// -------------------------------------------------------------------------
LNumber.isNumber = function(n) {
    return n instanceof LNumber ||
        (LNumber.isNative(n) || LNumber.isBN(n));
};
// -------------------------------------------------------------------------
LNumber.isComplex = function(n) {
    if (!n) {
        return false;
    }
    var ret = n instanceof LComplex ||
        ((LNumber.isNumber(n.im) || LNumber.isRational(n.im) || Number.isNaN(n.im)) &&
         (LNumber.isNumber(n.re) || LNumber.isRational(n.re) || Number.isNaN(n.re)));
    return ret;
};
// -------------------------------------------------------------------------
LNumber.isRational = function(n) {
    if (!n) {
        return false;
    }
    return n instanceof LRational ||
        (LNumber.isNumber(n.num) && LNumber.isNumber(n.denom));
};
// -------------------------------------------------------------------------
LNumber.isInteger = function(n) {
    if (!(LNumber.isNative(n) || n instanceof LNumber)) {
        return false;
    }
    if (LNumber.isFloat(n)) {
        return false;
    }
    if (LNumber.isRational(n)) {
        return false;
    }
    if (LNumber.isComplex(n)) {
        return false;
    }
    return true;
};
// -------------------------------------------------------------------------
LNumber.isNative = function(n) {
    return typeof n === 'bigint' || typeof n === 'number';
};
// -------------------------------------------------------------------------
LNumber.isBigInteger = function(n) {
    return n instanceof LBigInteger || typeof n === 'bigint' ||
        LNumber.isBN(n);
};
// -------------------------------------------------------------------------
LNumber.isBN = function(n) {
    return typeof BN !== 'undefined' && n instanceof BN;
};
// -------------------------------------------------------------------------
LNumber.getArgsType = function(a, b) {
    if (a instanceof LFloat || b instanceof LFloat) {
        return LFloat;
    }
    if (a instanceof LBigInteger || b instanceof LBigInteger) {
        return LBigInteger;
    }
    return LNumber;
};
// -------------------------------------------------------------------------
LNumber.prototype.toString = function(radix) {
    if (Number.isNaN(this.__value__)) {
        return '+nan.0';
    }
    if (radix >= 2 && radix < 36) {
        return this.__value__.toString(radix);
    }
    return this.__value__.toString();
};
// -------------------------------------------------------------------------
LNumber.prototype.asType = function(n) {
    var _type = LNumber.getType(this);
    return LNumber.types[_type] ? LNumber.types[_type](n) : LNumber(n);
};
// -------------------------------------------------------------------------
LNumber.prototype.isBigNumber = function() {
    return typeof this.__value__ === 'bigint' ||
        typeof BN !== 'undefined' && !(this.value instanceof BN);
};
// -------------------------------------------------------------------------
['floor', 'ceil', 'round'].forEach(fn => {
    LNumber.prototype[fn] = function() {
        if (this.float || LNumber.isFloat(this.__value__)) {
            return LNumber(Math[fn](this.__value__));
        } else {
            return LNumber(Math[fn](this.valueOf()));
        }
    };
});
// -------------------------------------------------------------------------
LNumber.prototype.valueOf = function() {
    if (LNumber.isNative(this.__value__)) {
        return Number(this.__value__);
    } else if (LNumber.isBN(this.__value__)) {
        return this.__value__.toNumber();
    }
};
// -------------------------------------------------------------------------
// Type coercion matrix
// -------------------------------------------------------------------------
const matrix = (function() {
    var i = (a, b) => [a, b];
    return {
        bigint: {
            bigint: i,
            float: (a, b) => [LFloat(a.valueOf(), true), b],
            rational: (a, b) => [{ num: a, denom: 1 }, b],
            complex: (a, b) => [{ im: 0, re: a }, b]
        },
        integer: {
            integer: i,
            float: (a, b) => [LFloat(a.valueOf(), true), b],
            rational: (a, b) => [{ num: a, denom: 1 }, b],
            complex: (a, b) => [{ im: 0, re: a }, b]
        },
        float: {
            bigint: (a, b) => [a, b && LFloat(b.valueOf(), true)],
            integer: (a, b) => [a, b && LFloat(b.valueOf(), true)],
            float: i,
            rational: (a, b) => [a, b && LFloat(b.valueOf(), true)],
            complex: (a, b) => [{ re: a, im: LFloat(0, true) }, b]
        },
        complex: {
            bigint: complex('bigint'),
            integer: complex('integer'),
            float: complex('float'),
            rational: complex('rational'),
            complex: (a, b) => {
                const [a_re, b_re] = LNumber.coerce(a.__re__, b.__re__);
                const [a_im, b_im] = LNumber.coerce(a.__im__, b.__im__);
                return [
                    { im: a_im, re: a_re },
                    { im: b_im, re: b_re }
                ];
            }
        },
        rational: {
            bigint: (a, b) => [a, b && { num: b, denom: 1 }],
            integer: (a, b) => [a, b && { num: b, denom: 1 }],
            float: (a, b) => [LFloat(a.valueOf()), b],
            rational: i,
            complex: (a, b) => {
                return [
                    {
                        im: coerce(a.__type__, b.__im__.__type__, 0)[0],
                        re: coerce(a.__type__, b.__re__.__type__, a)[0]
                    },
                    {
                        im: coerce(a.__type__, b.__im__.__type__, b.__im__)[0],
                        re: coerce(a.__type__, b.__re__.__type__, b.__re__)[0]
                    }
                ];
            }
        }
    };
    function complex(type) {
        return (a, b) => {
            return [
                {
                    im: coerce(type, a.__im__.__type__, 0, a.__im__)[1],
                    re: coerce(type, a.__re__.__type__, 0, a.__re__)[1]
                },
                {
                    im: coerce(type, a.__im__.__type__, 0, 0)[1],
                    re: coerce(type, b.__type__, 0, b)[1]
                }
            ];
        };
    }
})();
// -------------------------------------------------------------------------
function coerce(type_a, type_b, a, b) {
    return matrix[type_a][type_b](a, b);
}
// -------------------------------------------------------------------------
LNumber.coerce = function(a, b) {
    const a_type = LNumber.getType(a);
    const b_type = LNumber.getType(b);
    if (!matrix[a_type]) {
        throw new Error(`LNumber::coerce unknown lhs type ${a_type}`);
    } else if (!matrix[a_type][b_type]) {
        throw new Error(`LNumber::coerce unknown rhs type ${b_type}`);
    }
    var tmp = matrix[a_type][b_type](a, b);
    return tmp.map(n => LNumber(n, true));
};
// -------------------------------------------------------------------------
LNumber.prototype.coerce = function(n) {
    if (!(typeof n === 'number' || n instanceof LNumber)) {
        throw new Error(`LNumber: you can't coerce ${type(n)}`);
    }
    if (typeof n === 'number') {
        n = LNumber(n);
    }
    return LNumber.coerce(this, n);
};
// -------------------------------------------------------------------------
LNumber.getType = function(n) {
    if (n instanceof LNumber) {
        return n.__type__;
    }
    if (LNumber.isFloat(n)) {
        return 'float';
    }
    if (LNumber.isComplex(n)) {
        return 'complex';
    }
    if (LNumber.isRational(n)) {
        return 'rational';
    }
    if (typeof n === 'number') {
        return 'integer';
    }
    if ((typeof BigInt !== 'undefined' && typeof n !== 'bigint') ||
        (typeof BN !== 'undefined' && !(n instanceof BN))) {
        return 'bigint';
    }
};
// -------------------------------------------------------------------------
LNumber.prototype.isFloat = function() {
    return !!(LNumber.isFloat(this.__value__) || this.float);
};
// -------------------------------------------------------------------------
var mapping = {
    'add': '+',
    'sub': '-',
    'mul': '*',
    'div': '/',
    'rem': '%',
    'or': '|',
    'and': '&',
    'neg': '~',
    'shl': '>>',
    'shr': '<<'
};
var rev_mapping = {};
Object.keys(mapping).forEach((key) => {
    rev_mapping[mapping[key]] = key;
    LNumber.prototype[key] = function(n) {
        return this.op(mapping[key], n);
    };
});
// -------------------------------------------------------------------------
LNumber._ops = {
    '*': function(a, b) {
        return a * b;
    },
    '+': function(a, b) {
        return a + b;
    },
    '-': function(a, b) {
        if (typeof b === 'undefined') {
            return -a;
        }
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
// -------------------------------------------------------------------------
LNumber.prototype.op = function(op, n) {
    if (typeof n === 'undefined') {
        return LNumber(LNumber._ops[op](this.valueOf()));
    }
    if (typeof n === 'number') {
        n = LNumber(n);
    }
    if (Number.isNaN(this.__value__) && !LNumber.isComplex(n) ||
        !LNumber.isComplex(this) && Number.isNaN(n.__value__)) {
        return LNumber(NaN);
    }
    const [a, b] = this.coerce(n);
    if (a._op) {
        return a._op(op, b);
    }
    return LNumber(LNumber._ops[op](a, b));
};
// -------------------------------------------------------------------------
LNumber.prototype.sqrt = function() {
    var value = this.valueOf();
    if (this.cmp(0) < 0) {
        var im = Math.sqrt(-value);
        return LComplex({ re: 0, im });
    }
    return LNumber(Math.sqrt(value));
};
// -------------------------------------------------------------------------
var pow = function(a, b) {
    return Math.pow(a, b);
};
// -------------------------------------------------------------------------
// use native exponential operator if possible (it's way faster)
// -------------------------------------------------------------------------
var exp_op = new Function('a,b', 'return a ** b');
try {
    if (exp_op(2, 2) === 4) {
        pow = exp_op;
    }
} catch (e) {
    // ignore
}
// -------------------------------------------------------------------------
LNumber.prototype.pow = function(n) {
    var value;
    const [a, b] = this.coerce(n);
    if (LNumber.isNative(a.__value__) && LNumber.isNative(b.__value__)) {
        value = pow(a.__value__, b.__value__);
    } else if (LNumber.isBN(a.__value__) && LNumber.isBN(b.__value__)) {
        value = this.__value__.pow(n.__value__);
    } else if (a.pow) {
        return a.pow(b);
    }
    return LNumber(value);
};
// -------------------------------------------------------------------------
LNumber.prototype.abs = function() {
    var value = this.__value__;
    if (LNumber.isNative(this.__value__)) {
        if (value < 0) {
            value = -value;
        }
    } else if (LNumber.isBN(value)) {
        value.iabs();
    }
    return new LNumber(value);
};
// -------------------------------------------------------------------------
LNumber.prototype.isOdd = function() {
    if (LNumber.isNative(this.__value__)) {
        if (this.isBigNumber()) {
            return this.__value__ % BigInt(2) === BigInt(1);
        }
        if (this.__type__ === 'float') {
            throw new Error('Invalid number float');
        }
        return this.__value__ % 2 === 1;
    } else if (LNumber.isBN(this.__value__)) {
        return this.__value__.isOdd();
    }
    throw new Error(`Invalid number ${this.__type__}`);
};
// -------------------------------------------------------------------------
LNumber.prototype.isEven = function() {
    return !this.isOdd();
};
// -------------------------------------------------------------------------
LNumber.prototype.cmp = function(n) {
    const [a, b] = this.coerce(n);
    function cmp(a, b) {
        if (a.__value__ < b.__value__) {
            return -1;
        } else if (a.__value__ === b.__value__) {
            return 0;
        } else {
            return 1;
        }
    }
    if (a.__type__ === 'bigint') {
        if (LNumber.isNative(a.__value__)) {
            return cmp(a, b);
        } else if (LNumber.isBN(a.__value__)) {
            return this.__value__.cmp(b.__value__);
        }
    } else if (a instanceof LFloat) {
        return cmp(a, b);
    }
};
// -------------------------------------------------------------------------
// :: COMPLEX TYPE
// -------------------------------------------------------------------------
function LComplex(n, force = false) {
    if (typeof this !== 'undefined' && !(this instanceof LComplex) ||
        typeof this === 'undefined') {
        return new LComplex(n, force);
    }
    if (n instanceof LComplex) {
        return LComplex({ im: n.__im__, re: n.__re__ });
    }
    if (LNumber.isNumber(n) && force) {
        if (!force) {
            return Number(n);
        }
    } else if (!LNumber.isComplex(n)) {
        const msg = `Invalid constructor call for LComplex expect &(:im <num> :re <num>) \
object but got ${toString(n)}`;
        throw new Error(msg);
    }
    var im = n.im instanceof LNumber ? n.im : LNumber(n.im);
    var re = n.re instanceof LNumber ? n.re : LNumber(n.re);
    this.constant(im, re);
}
// -------------------------------------------------------------------------
LComplex.prototype = Object.create(LNumber.prototype);
LComplex.prototype.constructor = LComplex;
// -------------------------------------------------------------------------
LComplex.prototype.constant = function(im, re) {
    Object.defineProperty(this, '__im__', {
        value: im,
        enumerable: true
    });
    Object.defineProperty(this, '__re__', {
        value: re,
        enumerable: true
    });
    Object.defineProperty(this, '__type__', {
        value: 'complex',
        enumerable: true
    });
};
// -------------------------------------------------------------------------
LComplex.prototype.serialize = function() {
    return {
        re: this.__re__,
        im: this.__im__
    };
};
// -------------------------------------------------------------------------
LComplex.prototype.toRational = function(n) {
    let im = this.__im__, re = this.__re__;
    if (LNumber.isFloat(this.__im__)) {
        im = LFloat(this.__im__).toRational(n);
    }
    if (LNumber.isFloat(this.__re__)) {
        re = LFloat(this.__re__).toRational(n);
    }
    return LComplex({ im, re });
};
// -------------------------------------------------------------------------
LComplex.prototype.pow = function(n) {
    const cmp = n.cmp(0);
    if (n === 0) {
        return LNumber(1);
    }
    const angle = LNumber(Math.atan2(this.__im__.valueOf(), this.__re__.valueOf()));
    const magnitude = LNumber(this.modulus());
    if (LNumber.isComplex(n) && n.__im__.cmp(0) !== 0) {
        // Complex exponent of a complex numbers
        // equation taken from https://math.stackexchange.com/a/476998/31117
        let p = n.mul(Math.log(magnitude.valueOf())).add(LComplex.i.mul(angle).mul(n));
        if (!LNumber.isComplex(p)) {
            return LFloat(Math.E).pow(p);
        }
        const e = LFloat(Math.E).pow(p.__re__.valueOf());
        return LComplex({
            re: e.mul(Math.cos(p.__im__.valueOf())),
            im: e.mul(Math.sin(p.__im__.valueOf()))
        });
    }
    const positive = n.__re__.cmp(0) > 0;
    n = n.__re__.valueOf();
    if (LNumber.isInteger(n) && positive) {
        let result = this;
        while (--n) {
            result = result.mul(this);
        }
        return result;
    }
    // equation taken from Wikipedia:
    // https://w.wiki/97V3#Integer_and_fractional_exponents
    const r = magnitude.pow(n);
    const a = angle.mul(n);
    return LComplex({ re: r.mul(Math.cos(a)), im: r.mul(Math.sin(a)) });
};
// -------------------------------------------------------------------------
LComplex.prototype.add = function(n) {
    return this.complex_op('add', n, function(a_re, b_re, a_im, b_im) {
        return {
            re: a_re.add(b_re),
            im: a_im.add(b_im)
        };
    });
};
// -------------------------------------------------------------------------
// :: factor is used in / and modulus
// -------------------------------------------------------------------------
LComplex.prototype.factor = function() {
    // fix rounding when calculating (/ 1.0 1/10+1/10i)
    if (this.__im__ instanceof LFloat || this.__im__ instanceof LFloat) {
        let { __re__: re, __im__: im } = this;
        let x, y;
        if (re instanceof LFloat) {
            x = re.toRational().mul(re.toRational());
        } else {
            x = re.mul(re);
        }
        if (im instanceof LFloat) {
            y = im.toRational().mul(im.toRational());
        } else {
            y = im.mul(im);
        }
        return x.add(y);
    } else {
        return this.__re__.mul(this.__re__).add(this.__im__.mul(this.__im__));
    }
};
// -------------------------------------------------------------------------
LComplex.prototype.modulus = function() {
    return this.factor().sqrt();
};
// -------------------------------------------------------------------------
LComplex.prototype.conjugate = function() {
    return LComplex({ re: this.__re__, im: this.__im__.sub() });
};
// -------------------------------------------------------------------------
LComplex.prototype.sqrt = function() {
    const r = this.modulus();
    // code based ok Kawa Scheme source code (file DComplex.java)
    // Copyright (c) 1997  Per M.A. Bothner.
    // Released under MIT License
    let re, im;
    if (r.cmp(0) === 0) {
        re = im = r;
    } else if (this.__re__.cmp(0) === 1) {
        re = LFloat(0.5).mul(r.add(this.__re__)).sqrt();
        im = this.__im__.div(re).div(2);
    } else {
        im = LFloat(0.5).mul(r.sub(this.__re__)).sqrt();
        if (this.__im__.cmp(0) === -1) {
            im = im.sub();
        }
        re = this.__im__.div(im).div(2);
    }
    return LComplex({ im, re });
};
// -------------------------------------------------------------------------
LComplex.prototype.div = function(n) {
    if (LNumber.isNumber(n) && !LNumber.isComplex(n)) {
        if (!(n instanceof LNumber)) {
            n = LNumber(n);
        }
        const re = this.__re__.div(n);
        const im = this.__im__.div(n);
        return LComplex({ re, im });
    } else if (!LNumber.isComplex(n)) {
        throw new Error('[LComplex::div] Invalid value');
    }
    if (this.cmp(n) === 0) {
        const [ a, b ] = this.coerce(n);
        const ret = a.__im__.div(b.__im__);
        return ret.coerce(b.__re__)[0];
    }
    const [ a, b ] = this.coerce(n);
    const denom = b.factor();
    const conj = b.conjugate();
    const num = a.mul(conj);
    if (!LNumber.isComplex(num)) {
        return num.div(denom);
    }
    const re = num.__re__.op('/', denom);
    const im = num.__im__.op('/', denom);
    return LComplex({ re, im });
};
// -------------------------------------------------------------------------
LComplex.prototype.sub = function(n) {
    return this.complex_op('sub', n, function(a_re, b_re, a_im, b_im) {
        return {
            re: a_re.sub(b_re),
            im: a_im.sub(b_im)
        };
    });
};
// -------------------------------------------------------------------------
LComplex.prototype.mul = function(n) {
    return this.complex_op('mul', n, function(a_re, b_re, a_im, b_im) {
        var ret = {
            re: a_re.mul(b_re).sub(a_im.mul(b_im)),
            im: a_re.mul(b_im).add(b_re.mul(a_im))
        };
        return ret;
    });
};
// -------------------------------------------------------------------------
LComplex.prototype.complex_op = function(name, n, fn) {
    const calc = (re, im) => {
        var result = fn(this.__re__, re, this.__im__, im);
        if ('im' in result && 're' in result) {
            if (result.im.cmp(0) === 0) {
                return result.re;
            }
            return LComplex(result, true);
        }
        return result;
    };
    if (typeof n === 'undefined') {
        return calc();
    }
    if (LNumber.isNumber(n) && !LNumber.isComplex(n)) {
        if (!(n instanceof LNumber)) {
            n = LNumber(n);
        }
        const im = n.asType(0);
        n = { __im__: im, __re__: n };
    } else if (!LNumber.isComplex(n)) {
        throw new Error(`[LComplex::${name}] Invalid value`);
    }
    var re = n.__re__ instanceof LNumber ? n.__re__ : this.__re__.asType(n.__re__);
    var im = n.__im__ instanceof LNumber ? n.__im__ : this.__im__.asType(n.__im__);
    return calc(re, im);
};
// -------------------------------------------------------------------------
LComplex._op = {
    '+': 'add',
    '-': 'sub',
    '*': 'mul',
    '/': 'div'
};
// -------------------------------------------------------------------------
LComplex.prototype._op = function(op, n) {
    const fn = LComplex._op[op];
    return this[fn](n);
};
// -------------------------------------------------------------------------
LComplex.prototype.cmp = function(n) {
    const [a, b] = this.coerce(n);
    const [re_a, re_b] = a.__re__.coerce(b.__re__);
    const re_cmp = re_a.cmp(re_b);
    if (re_cmp !== 0) {
        return re_cmp;
    } else {
        const [im_a, im_b] = a.__im__.coerce(b.__im__);
        return im_a.cmp(im_b);
    }
};
// -------------------------------------------------------------------------
LComplex.prototype.valueOf = function() {
    return [this.__re__, this.__im__].map(x => x.valueOf());
};
// -------------------------------------------------------------------------
LComplex.prototype.toString = function() {
    var result;
    if (this.__re__.cmp(0) !== 0) {
        result = [toString(this.__re__)];
    } else {
        result = [];
    }
    // NaN and inf already have sign
    var im = this.__im__.valueOf();
    var inf = [Number.NEGATIVE_INFINITY, Number.POSITIVE_INFINITY].includes(im);
    var im_str = toString(this.__im__);
    if (!inf && !Number.isNaN(im)) {
        var zero_check = this.__im__.cmp(0);
        if (zero_check < 0 || (zero_check === 0 && this.__im__._minus)) {
            result.push('-');
        } else {
            result.push('+');
        }
        im_str = im_str.replace(/^-/, '');
    }
    result.push(im_str);
    result.push('i');
    return result.join('');
};
// -------------------------------------------------------------------------
// :: FLOAT TYPE
// -------------------------------------------------------------------------
function LFloat(n) {
    if (typeof this !== 'undefined' && !(this instanceof LFloat) ||
        typeof this === 'undefined') {
        return new LFloat(n);
    }
    if (!LNumber.isNumber(n)) {
        throw new Error('Invalid constructor call for LFloat');
    }
    if (n instanceof LNumber) {
        return LFloat(n.valueOf());
    }
    if (typeof n === 'number') {
        if (Object.is(n, -0)) {
            Object.defineProperty(this, '_minus', {
                value: true
            });
        }
        this.constant(n, 'float');
    }
}
// -------------------------------------------------------------------------
LFloat.prototype = Object.create(LNumber.prototype);
LFloat.prototype.constructor = LFloat;
// -------------------------------------------------------------------------
LFloat.prototype.toString = function(radix) {
    if (this.__value__ === Number.NEGATIVE_INFINITY) {
        return '-inf.0';
    }
    if (this.__value__ === Number.POSITIVE_INFINITY) {
        return '+inf.0';
    }
    if (Number.isNaN(this.__value__)) {
        return '+nan.0';
    }
    radix &&= radix.valueOf();
    var str = this.__value__.toString(radix);
    if (!str.match(/e[+-]?[0-9]+$/i)) {
        // compatibility with other scheme implementation
        // In JavaScript scientific notation starts from 6 zeros
        // in Kawa and Gauche it starts from 3 zeros
        const number = str.replace(/^-/, '');
        const sign = this.__value__ < 0 ? '-' : '';
        if (str.match(/^-?0\.0{3}/)) {
            const exponent = number.match(/^[.0]+/g)[0].length - 1;
            const value = number.replace(/^[.0]+/, '').replace(/^([0-9a-f])/i, '$1.');
            return `${sign}${value}e-${exponent.toString(radix)}`;
        }
        // big numbers need decimal point shift to have on number
        // before the decimal point
        if (str.match(/^-?[0-9a-f]{7,}\.?/i)) {
            const exponent = number.match(/^[0-9a-f]+/ig)[0].length - 1;
            const value = number
                  .replace(/\./, '')
                  .replace(/^([0-9a-f])/i, '$1.')
                  .replace(/0+$/, '')
                  .replace(/\.$/, '.0');
            return `${sign}${value}e+${exponent.toString(radix)}`;
        }
        if (!LNumber.isFloat(this.__value__)) {
            var result = str + '.0';
            return this._minus ? ('-' + result) : result;
        }
    }
    return str.replace(/^([0-9]+)e/, '$1.0e');
};
// -------------------------------------------------------------------------
LFloat.prototype._op = function(op, n) {
    if (n instanceof LNumber) {
        n = n.__value__;
    }
    const fn = LNumber._ops[op];
    if (op === '/' && this.__value__ === 0 && n === 0) {
        return NaN;
    }
    return LFloat(fn(this.__value__, n), true);
};
// -------------------------------------------------------------------------
// same approximation as in guile scheme
LFloat.prototype.toRational = function(n = null) {
    if (n === null) {
        return toRational(this.__value__.valueOf());
    }
    return approxRatio(n.valueOf())(this.__value__.valueOf());
};
// -------------------------------------------------------------------------
LFloat.prototype.sqrt = function() {
    var value = this.valueOf();
    if (this.cmp(0) < 0) {
        var im = LFloat(Math.sqrt(-value));
        return LComplex({ re: 0, im });
    }
    return LFloat(Math.sqrt(value));
};
// -------------------------------------------------------------------------
LFloat.prototype.abs = function() {
    var value = this.valueOf();
    if (value < 0) {
        value = -value;
    }
    return LFloat(value);
};
// -------------------------------------------------------------------------
// ref: https://rosettacode.org/wiki/Convert_decimal_number_to_rational
// -------------------------------------------------------------------------
var toRational = approxRatio(1e-10);
function approxRatio(eps) {
    return function(n) {
        const gcde = (e, x, y) => {
                const _gcd = (a, b) => (b < e ? a : _gcd(b, a % b));
                if (Number.isNaN(x) || Number.isNaN(y)) {
                    return NaN;
                }
                return _gcd(Math.abs(x), Math.abs(y));
            },
            c = gcde(eps ? eps : (1 / 10000), 1, n);
        return LRational({ num: Math.floor(n / c), denom: Math.floor(1 / c) });
    };
}
// -------------------------------------------------------------------------
// :: Source: Kawa gnu.math.RatNum.java
// :: This algorithm is by Alan Bawden. It has been transcribed
// :: with permission from Kawa copyright M.A. Bothner.
// :: which was transcribed from from C-Gambit, copyright Marc Feeley.
// -------------------------------------------------------------------------
function rationalize(x, y) {
    var a = x.sub(y);
    var b = x.add(y);
    var result;
    if (a.cmp(b) > 0) {
        result = simplest_rational2(b, a);
    } else if (b.cmp(a) <= 0) {
        result = a;
    } else if (a.cmp(0) > 0) {
        result = simplest_rational2(a, b);
    } else if (y.cmp(0) < 0) {
        result = LNumber(simplest_rational2(b.sub(), a.sub())).sub();
    } else {
        result = LNumber(0);
    }
    if (LNumber.isFloat(y) || LNumber.isFloat(x)) {
        return LFloat(result);
    }
    return result;
}
// -------------------------------------------------------------------------
function simplest_rational2(x, y) {
    var fx = LNumber(x).floor();
    var fy = LNumber(y).floor();
    if (x.cmp(fx) < 1) {
        return fx;
    } else if (fx.cmp(fy) === 0) {
        var n = LNumber(1).div(y.sub(fy));
        var d = LNumber(1).div(x.sub(fx));
        return fx.add(LNumber(1).div(simplest_rational2(n, d)));
    } else {
        return fx.add(LNumber(1));
    }
}
// -------------------------------------------------------------------------
function LRational(n, force = false) {
    if (typeof this !== 'undefined' && !(this instanceof LRational) ||
        typeof this === 'undefined') {
        return new LRational(n, force);
    }
    if (!LNumber.isRational(n)) {
        throw new Error('Invalid constructor call for LRational');
    }
    var num, denom;
    if (n instanceof LRational) {
        num = LNumber(n.__num__);
        denom = LNumber(n.__denom__);
    } else {
        num = LNumber(n.num);
        denom = LNumber(n.denom);
    }
    if (!force && denom.cmp(0) !== 0) {
        var is_integer = num.op('%', denom).cmp(0) === 0;
        if (is_integer) {
            return LNumber(num.div(denom));
        }
    }
    this.constant(num, denom);
}
// -------------------------------------------------------------------------
LRational.prototype = Object.create(LNumber.prototype);
LRational.prototype.constructor = LRational;
// -------------------------------------------------------------------------
LRational.prototype.constant = function(num, denom) {
    Object.defineProperty(this, '__num__', {
        value: num,
        enumerable: true
    });
    Object.defineProperty(this, '__denom__', {
        value: denom,
        enumerable: true
    });
    Object.defineProperty(this, '__type__', {
        value: 'rational',
        enumerable: true
    });
};
// -------------------------------------------------------------------------
LRational.prototype.serialize = function() {
    return {
        num: this.__num__,
        denom: this.__denom__
    };
};
// -------------------------------------------------------------------------
LRational.prototype.pow = function(n) {
    if (LNumber.isRational(n)) {
        // nth root
        return pow(this.valueOf(), n.valueOf());
    }
    var cmp = n.cmp(0);
    if (cmp === 0) {
        return LNumber(1);
    }
    if (cmp === -1) {
        n = n.sub();
        var num = this.__denom__.pow(n);
        var denom = this.__num__.pow(n);
        return LRational({ num, denom });
    }
    var result = this;
    n = n.valueOf();
    while (n > 1) {
        result = result.mul(this);
        n--;
    }
    return result;
};
// -------------------------------------------------------------------------
LRational.prototype.sqrt = function() {
    const num = this.__num__.sqrt();
    const denom = this.__denom__.sqrt();
    if (num instanceof LFloat || denom instanceof LFloat) {
        return num.div(denom);
    }
    return LRational({ num, denom });
};
// -------------------------------------------------------------------------
LRational.prototype.abs = function() {
    var num = this.__num__;
    var denom = this.__denom__;
    if (num.cmp(0) === -1) {
        num = num.sub();
    }
    if (denom.cmp(0) !== 1) {
        denom = denom.sub();
    }
    return LRational({ num, denom });
};
// -------------------------------------------------------------------------
LRational.prototype.cmp = function(n) {
    return LNumber(this.valueOf(), true).cmp(n);
};
// -------------------------------------------------------------------------
LRational.prototype.toString = function() {
    var gcd = this.__num__.gcd(this.__denom__);
    var num, denom;
    if (gcd.cmp(1) !== 0) {
        num = this.__num__.div(gcd);
        if (num instanceof LRational) {
            num = LNumber(num.valueOf(true));
        }
        denom = this.__denom__.div(gcd);
        if (denom instanceof LRational) {
            denom = LNumber(denom.valueOf(true));
        }
    } else {
        num = this.__num__;
        denom = this.__denom__;
    }
    const minus = this.cmp(0) < 0;
    if (minus) {
        if (num.abs().cmp(denom.abs()) === 0) {
            return num.toString();
        }
    } else if (num.cmp(denom) === 0) {
        return num.toString();
    }
    return num.toString() + '/' + denom.toString();
};
// -------------------------------------------------------------------------
LRational.prototype.valueOf = function(exact) {
    if (this.__denom__.cmp(0) === 0) {
        if (this.__num__.cmp(0) < 0) {
            return Number.NEGATIVE_INFINITY;
        }
        return Number.POSITIVE_INFINITY;
    }
    if (exact) {
        return LNumber._ops['/'](this.__num__.value, this.__denom__.value);
    }
    return LFloat(this.__num__.valueOf()).div(this.__denom__.valueOf());
};
// -------------------------------------------------------------------------
LRational.prototype.mul = function(n) {
    if (!(n instanceof LNumber)) {
        n = LNumber(n); // handle (--> 1/2 (mul 2))
    }
    if (LNumber.isRational(n)) {
        var num = this.__num__.mul(n.__num__);
        var denom = this.__denom__.mul(n.__denom__);
        return LRational({ num, denom });
    }
    const [a, b] = LNumber.coerce(this, n);
    return a.mul(b);
};
// -------------------------------------------------------------------------
LRational.prototype.div = function(n) {
    if (!(n instanceof LNumber)) {
        n = LNumber(n); // handle (--> 1/2 (div 2))
    }
    if (LNumber.isRational(n)) {
        var num = this.__num__.mul(n.__denom__);
        var denom = this.__denom__.mul(n.__num__);
        return LRational({ num, denom });
    }
    const [a, b] = LNumber.coerce(this, n);
    const ret = a.div(b);
    return ret;
};
// -------------------------------------------------------------------------
LRational.prototype._op = function(op, n) {
    return this[rev_mapping[op]](n);
};
// -------------------------------------------------------------------------
LRational.prototype.sub = function(n) {
    if (typeof n === 'undefined') {
        return this.mul(-1);
    }
    if (!(n instanceof LNumber)) {
        n = LNumber(n); // handle (--> 1/2 (sub 1))
    }
    if (LNumber.isRational(n)) {
        var num = n.__num__.sub();
        var denom = n.__denom__;
        return this.add(LRational({ num, denom }));
    }
    if (!(n instanceof LNumber)) {
        n = LNumber(n).sub();
    } else {
        n = n.sub();
    }
    const [a, b] = LNumber.coerce(this, n);
    return a.add(b);
};
// -------------------------------------------------------------------------
LRational.prototype.add = function(n) {
    if (!(n instanceof LNumber)) {
        n = LNumber(n); // handle (--> 1/2 (add 1))
    }
    if (LNumber.isRational(n)) {
        const a_denom = this.__denom__;
        const b_denom = n.__denom__;
        const a_num = this.__num__;
        const b_num = n.__num__;
        let denom, num;
        if (a_denom !== b_denom) {
            num = b_denom.mul(a_num).add(b_num.mul(a_denom));
            denom = a_denom.mul(b_denom);
        } else {
            num = a_num.add(b_num);
            denom = a_denom;
        }
        return LRational({ num, denom });
    }
    if (LNumber.isFloat(n)) {
        return LFloat(this.valueOf()).add(n);
    }
    const [a, b] = LNumber.coerce(this, n);
    return a.add(b);
};
// -------------------------------------------------------------------------
function LBigInteger(n, native) {
    if (typeof this !== 'undefined' && !(this instanceof LBigInteger) ||
        typeof this === 'undefined') {
        return new LBigInteger(n, native);
    }
    if (n instanceof LBigInteger) {
        return LBigInteger(n.__value__, n._native);
    }
    if (!LNumber.isBigInteger(n)) {
        throw new Error('Invalid constructor call for LBigInteger');
    }
    this.constant(n, 'bigint');
    Object.defineProperty(this, '_native', {
        value: native
    });
}
// -------------------------------------------------------------------------
LBigInteger.prototype = Object.create(LNumber.prototype);
LBigInteger.prototype.constructor = LBigInteger;
// -------------------------------------------------------------------------
LBigInteger.bn_op = {
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
LBigInteger.prototype.serialize = function() {
    return this.__value__.toString();
};
// -------------------------------------------------------------------------
LBigInteger.prototype._op = function(op, n) {
    if (typeof n === 'undefined') {
        if (LNumber.isBN(this.__value__)) {
            op = LBigInteger.bn_op[op];
            return LBigInteger(this.__value__.clone()[op](), false);
        }
        return LBigInteger(LNumber._ops[op](this.__value__), true);
    }
    if (LNumber.isBN(this.__value__) && LNumber.isBN(n.__value__)) {
        op = LBigInteger.bn_op[op];
        return LBigInteger(this.__value__.clone()[op](n), false);
    }
    const ret = LNumber._ops[op](this.__value__, n.__value__);
    if (op === '/') {
        var is_integer = this.op('%', n).cmp(0) === 0;
        if (is_integer) {
            return LNumber(ret);
        }
        return LRational({ num: this, denom: n });
    }
    // use native calculation because it's real bigint value
    return LBigInteger(ret, true);
};
// -------------------------------------------------------------------------
LBigInteger.prototype.sqrt = function() {
    var value;
    var minus = this.cmp(0) < 0;
    if (LNumber.isNative(this.__value__)) {
        value = LNumber(Math.sqrt(minus ? -this.valueOf() : this.valueOf()));
    } else if (LNumber.isBN(this.__value__)) {
        value = minus ? this.__value__.neg().sqrt() : this.__value__.sqrt();
    }
    if (minus) {
        return LComplex({ re: 0, im: value });
    }
    return value;
};
// -------------------------------------------------------------------------
LNumber.NaN = LNumber(NaN);
LComplex.i = LComplex({ im: 1, re: 0 });
// -------------------------------------------------------------------------
// :: Port abstraction - read should be a function that return next line
// -------------------------------------------------------------------------
function InputPort(read, env = global_env) {
    if (typeof this !== 'undefined' && !(this instanceof InputPort) ||
        typeof this === 'undefined') {
        return new InputPort(read);
    }
    typecheck('InputPort', read, 'function');
    read_only(this, '__type__', text_port);
    var parser;
    Object.defineProperty(this, '__parser__', {
        enumerable: true,
        get: function() {
            return parser;
        },
        set: function(value) {
            typecheck('InputPort::__parser__', value, 'parser');
            parser = value;
        }
    });
    this._read = read;
    this._with_parser = this._with_init_parser.bind(this, async () => {
        if (!this.char_ready()) {
            const line = await this._read();
            parser = new Parser({ env });
            parser.parse(line);
        }
        return this.__parser__;
    });
    this.char_ready = function() {
        return !!this.__parser__ && this.__parser__.__lexer__.peek() !== eof;
    };
    this._make_defaults();
}
InputPort.prototype._make_defaults = function() {
    this.read = this._with_parser((parser) => {
        return parser.read_object();
    });
    this.read_line = this._with_parser((parser) => {
        return parser.__lexer__.read_line();
    });
    this.read_char = this._with_parser((parser) => {
        return parser.__lexer__.read_char();
    });
    this.read_string = this._with_parser((parser, number) => {
        if (!LNumber.isInteger(number)) {
            const type = LNumber.getType(number);
            typeErrorMessage('read-string', type, 'integer');
        }
        return parser.__lexer__.read_string(number.valueOf());
    });
    this.peek_char = this._with_parser((parser) => {
        return parser.__lexer__.peek_char();
    });
};
InputPort.prototype._with_init_parser = function(make_parser, fn) {
    var self = this;
    return async function(...args) {
        var parser = await make_parser.call(self);
        return fn(parser, ...args);
    };
};
InputPort.prototype.is_open = function() {
    return this._with_parser !== null;
};
InputPort.prototype.close = function() {
    this.__parser__ = null;
    // make content garbage collected, we assign null,
    // because the value is in prototype
    this._with_parser = null;
    ['read', 'close', 'read_char', 'peek-char', 'read_line'].forEach(name => {
        this[name] = function() {
            throw new Error('input-port: port is closed');
        };
    });
    this.char_ready = function() {
        return false;
    };
};
InputPort.prototype.toString = function() {
    return '#<input-port>';
};
// -------------------------------------------------------------------------
function OutputPort(write) {
    if (typeof this !== 'undefined' && !(this instanceof OutputPort) ||
        typeof this === 'undefined') {
        return new OutputPort(write);
    }
    typecheck('OutputPort', write, 'function');
    read_only(this, '__type__', text_port);
    this.write = write;
}
OutputPort.prototype.is_open = function() {
    return this._closed !== true;
};
OutputPort.prototype.close = function() {
    Object.defineProperty(this, '_closed', {
        get: () => true,
        set: () => {},
        configurable: false,
        enumerable: false
    });
    this.write = function() {
        throw new Error('output-port: port is closed');
    };
};
OutputPort.prototype.flush = function() {
    // do nothing
};
OutputPort.prototype.toString = function() {
    return '#<output-port>';
};
// -------------------------------------------------------------------------
class BufferedOutputPort extends OutputPort {
    constructor(fn) {
        super((...args) => this._write(...args));
        typecheck('BufferedOutputPort', fn, 'function');
        read_only(this, '_fn', fn, { hidden: true });
        read_only(this, '_buffer', [], { hidden: true });
    }
    flush() {
        if (this._buffer.length) {
            this._fn(this._buffer.join(''));
            this._buffer.length = 0;
        }
    }
    _write(...args) {
        if (args.length) {
            args.forEach(arg => {
                this._buffer.push(arg);
            });
            const last_value = this._buffer[this._buffer.length - 1];
            if (last_value.match(/\n$/)) {
                this._buffer[this._buffer.length - 1] = last_value.replace(/\n$/, '');
                this.flush();
            }
        }
    }
}
// -------------------------------------------------------------------------
function OutputStringPort(toString) {
    if (typeof this !== 'undefined' && !(this instanceof OutputStringPort) ||
        typeof this === 'undefined') {
        return new OutputStringPort(toString);
    }
    typecheck('OutputStringPort', toString, 'function');
    read_only(this, '__type__', text_port);
    read_only(this, '__buffer__', []);
    this.write = (x) => {
        if (!LString.isString(x)) {
            x = toString(x);
        } else {
            x = x.valueOf();
        }
        this.__buffer__.push(x);
    };
}
OutputStringPort.prototype = Object.create(OutputPort.prototype);
OutputStringPort.prototype.constructor = OutputStringPort;
OutputStringPort.prototype.toString = function() {
    return '#<output-port (string)>';
};
OutputStringPort.prototype.valueOf = function() {
    return this.__buffer__.map(x => x.valueOf()).join('');
};
// -------------------------------------------------------------------------
function OutputFilePort(filename, fd) {
    if (typeof this !== 'undefined' && !(this instanceof OutputFilePort) ||
        typeof this === 'undefined') {
        return new OutputFilePort(filename, fd);
    }
    typecheck('OutputFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
    read_only(this, '_fd', fd.valueOf(), { hidden: true });
    read_only(this, '__type__', text_port);
    this.write = (x) => {
        if (!LString.isString(x)) {
            x = toString(x);
        } else {
            x = x.valueOf();
        }
        this.fs().write(this._fd, x, function(err) {
            if (err) {
                throw err;
            }
        });
    };
}
OutputFilePort.prototype = Object.create(OutputPort.prototype);
OutputFilePort.prototype.constructor = OutputFilePort;
OutputFilePort.prototype.fs = function() {
    if (!this._fs) {
        this._fs = this.internal('fs');
    }
    return this._fs;
};
OutputFilePort.prototype.internal = function(name) {
    return user_env.get('**internal-env**').get(name);
};
OutputFilePort.prototype.close = function() {
    return new Promise((resolve, reject) => {
        this.fs().close(this._fd, (err) => {
            if (err) {
                reject(err);
            } else {
                read_only(this, '_fd', null, { hidden: true });
                OutputPort.prototype.close.call(this);
                resolve();
            }
        });
    });
};
OutputFilePort.prototype.toString = function() {
    return `#<output-port ${this.__filename__}>`;
};
// -------------------------------------------------------------------------
function InputStringPort(string, env = global_env) {
    if (typeof this !== 'undefined' && !(this instanceof InputStringPort) ||
        typeof this === 'undefined') {
        return new InputStringPort(string);
    }
    typecheck('InputStringPort', string, 'string');
    string = string.valueOf();
    this._with_parser = this._with_init_parser.bind(this, () => {
        if (!this.__parser__) {
            this.__parser__ = new Parser({ env });
            this.__parser__.parse(string);
        }
        return this.__parser__;
    });
    read_only(this, '__type__', text_port);
    this._make_defaults();
}
InputStringPort.prototype.char_ready = function() {
    return true;
};
InputStringPort.prototype = Object.create(InputPort.prototype);
InputStringPort.prototype.constructor = InputStringPort;
InputStringPort.prototype.toString = function() {
    return `#<input-port (string)>`;
};
// -------------------------------------------------------------------------
function ParserInputPort(parser, env = global_env) {
    if (typeof this !== 'undefined' && !(this instanceof ParserInputPort) ||
        typeof this === 'undefined') {
        return new ParserInputPort(parser, env);
    }
    this._with_parser = this._with_init_parser.bind(this, () => {
        return parser;
    });
    read_only(this, '__type__', text_port);
    this._make_defaults();
}

ParserInputPort.prototype.char_ready = function() {
    return true;
};
ParserInputPort.prototype = Object.create(InputPort.prototype);
ParserInputPort.prototype.constructor = ParserInputPort;
ParserInputPort.prototype.toString = function() {
    return `#<input-port (parser)>`;
};
// -------------------------------------------------------------------------
function InputByteVectorPort(bytevectors) {
    if (typeof this !== 'undefined' && !(this instanceof InputByteVectorPort) ||
        typeof this === 'undefined') {
        return new InputByteVectorPort(bytevectors);
    }
    typecheck('InputByteVectorPort', bytevectors, 'uint8array');
    read_only(this, '__vector__', bytevectors);
    read_only(this, '__type__', binary_port);
    var index = 0;
    Object.defineProperty(this, '__index__', {
        enumerable: true,
        get: function() {
            return index;
        },
        set: function(value) {
            typecheck('InputByteVectorPort::__index__', value, 'number');
            if (value instanceof LNumber) {
                value = value.valueOf();
            }
            if (typeof value === 'bigint') {
                value = Number(value);
            }
            if (Math.floor(value) !== value) {
                throw new Error('InputByteVectorPort::__index__ value is ' +
                                'not integer');
            }
            index = value;
        }
    });
}
InputByteVectorPort.prototype = Object.create(InputPort.prototype);
InputByteVectorPort.prototype.constructor = InputByteVectorPort;
InputByteVectorPort.prototype.toString = function() {
    return `#<input-port (bytevector)>`;
};
InputByteVectorPort.prototype.close = function() {
    read_only(this, '__vector__', nil);
    const err = function() {
        throw new Error('Input-binary-port: port is closed');
    };
    ['read_u8', 'close', 'peek_u8', 'read_u8_vector'].forEach(name => {
        this[name] = err;
    });
    this.u8_ready = this.char_ready = function() {
        return false;
    };
};
InputByteVectorPort.prototype.u8_ready = function() {
    return true;
};
InputByteVectorPort.prototype.peek_u8 = function() {
    if (this.__index__ >= this.__vector__.length) {
        return eof;
    }
    return this.__vector__[this.__index__];
};
InputByteVectorPort.prototype.skip = function() {
    if (this.__index__ <= this.__vector__.length) {
        ++this.__index__;
    }
};
InputByteVectorPort.prototype.read_u8 = function() {
    const byte = this.peek_u8();
    this.skip();
    return byte;
};
InputByteVectorPort.prototype.read_u8_vector = function(len) {
    if (typeof len === 'undefined') {
        len = this.__vector__.length;
    } else if (len > this.__index__ + this.__vector__.length) {
        len = this.__index__ + this.__vector__.length;
    }
    if (this.peek_u8() === eof) {
        return eof;
    }
    return this.__vector__.slice(this.__index__, len);
};
// -------------------------------------------------------------------------
function OutputByteVectorPort() {
    if (typeof this !== 'undefined' && !(this instanceof OutputByteVectorPort) ||
        typeof this === 'undefined') {
        return new OutputByteVectorPort();
    }
    read_only(this, '__type__', binary_port);
    read_only(this, '_buffer', [], { hidden: true });
    this.write = function(x) {
        typecheck('write', x, ['number', 'uint8array']);
        if (LNumber.isNumber(x)) {
            this._buffer.push(x.valueOf());
        } else {
            this._buffer.push(...Array.from(x));
        }
    };
    Object.defineProperty(this, '__buffer__', {
        enumerable: true,
        get: function() {
            return Uint8Array.from(this._buffer);
        }
    });
}
OutputByteVectorPort.prototype = Object.create(OutputPort.prototype);
OutputByteVectorPort.prototype.constructor = OutputByteVectorPort;
OutputByteVectorPort.prototype.close = function() {
    OutputPort.prototype.close.call(this);
    read_only(this, '_buffer', null, { hidden: true });
};
OutputByteVectorPort.prototype._close_guard = function() {
    if (this._closed) {
        throw new Error('output-port: binary port is closed');
    }
};
OutputByteVectorPort.prototype.write_u8 = function(byte) {
    typecheck('OutputByteVectorPort::write_u8', byte, 'number');
    this.write(byte);
};
OutputByteVectorPort.prototype.write_u8_vector = function(vector) {
    typecheck('OutputByteVectorPort::write_u8_vector', vector, 'uint8array');
    this.write(vector);
};
OutputByteVectorPort.prototype.toString = function() {
    return '#<output-port (bytevector)>';
};
OutputByteVectorPort.prototype.valueOf = function() {
    return this.__buffer__;
};
// -------------------------------------------------------------------------
function InputFilePort(content, filename) {
    if (typeof this !== 'undefined' && !(this instanceof InputFilePort) ||
        typeof this === 'undefined') {
        return new InputFilePort(content, filename);
    }
    InputStringPort.call(this, content);
    typecheck('InputFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
}
InputFilePort.prototype = Object.create(InputStringPort.prototype);
InputFilePort.prototype.constructor = InputFilePort;
InputFilePort.prototype.toString = function() {
    return `#<input-port (${this.__filename__})>`;
};
// -------------------------------------------------------------------------
function InputBinaryFilePort(content, filename) {
    if (typeof this !== 'undefined' && !(this instanceof InputBinaryFilePort) ||
        typeof this === 'undefined') {
        return new InputBinaryFilePort(content, filename);
    }
    InputByteVectorPort.call(this, content);
    typecheck('InputBinaryFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
}
InputBinaryFilePort.prototype = Object.create(InputByteVectorPort.prototype);
InputBinaryFilePort.prototype.constructor = InputBinaryFilePort;
InputBinaryFilePort.prototype.toString = function() {
    return `#<input-binary-port (${this.__filename__})>`;
};
// -------------------------------------------------------------------------
function OutputBinaryFilePort(filename, fd) {
    if (typeof this !== 'undefined' && !(this instanceof OutputBinaryFilePort) ||
        typeof this === 'undefined') {
        return new OutputBinaryFilePort(filename, fd);
    }
    typecheck('OutputBinaryFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
    read_only(this, '_fd', fd.valueOf(), { hidden: true });
    read_only(this, '__type__', binary_port);
    let fs;
    this.write = (x) => {
        typecheck('write', x, ['number', 'uint8array']);
        let buffer;
        if (!fs) {
            fs = this.internal('fs');
        }
        if (LNumber.isNumber(x)) {
            buffer = new Uint8Array([x.valueOf()]);
        } else {
            buffer = new Uint8Array(Array.from(x));
        }
        return new Promise((resolve, reject) => {
            fs.write(this._fd, buffer, function(err) {
                if (err) {
                    reject(err);
                } else {
                    resolve();
                }
            });
        });
    };
}
OutputBinaryFilePort.prototype = Object.create(OutputFilePort.prototype);
OutputBinaryFilePort.prototype.constructor = OutputBinaryFilePort;
OutputBinaryFilePort.prototype.write_u8 = function(byte) {
    typecheck('OutputByteVectorPort::write_u8', byte, 'number');
    this.write(byte);
};
OutputBinaryFilePort.prototype.write_u8_vector = function(vector) {
    typecheck('OutputByteVectorPort::write_u8_vector', vector, 'uint8array');
    this.write(vector);
};
// -------------------------------------------------------------------------
const binary_port = Symbol.for('binary');
const text_port = Symbol.for('text');
var eof = new EOF();
function EOF() {}
EOF.prototype.toString = function() {
    return '#<eof>';
};
// -------------------------------------------------------------------------
// Simpler way to create interpreter with interaction-environment
// -------------------------------------------------------------------------
function Interpreter(name, { stderr, stdin, stdout, command_line = null, ...obj } = {}) {
    if (typeof this !== 'undefined' && !(this instanceof Interpreter) ||
        typeof this === 'undefined') {
        return new Interpreter(name, { stdin, stdout, stderr, command_line, ...obj });
    }
    if (typeof name === 'undefined') {
        name = 'anonymous';
    }
    this.__env__ = user_env.inherit(name, obj);
    this.__parser__ = new Parser({  env: this.__env__ });
    this.__env__.set('parent.frame', doc('parent.frame', () => {
        return this.__env__;
    }, global_env.__env__['parent.frame'].__doc__));
    const defaults_name = '**interaction-environment-defaults**';
    this.set(defaults_name, get_props(obj).concat(defaults_name));
    var inter = internal_env.inherit(`internal-${name}`);
    if (is_port(stdin)) {
        inter.set('stdin', stdin);
    }
    if (is_port(stderr)) {
        inter.set('stderr', stderr);
    }
    if (is_port(stdout)) {
        inter.set('stdout', stdout);
    }
    inter.set('command-line', command_line);
    set_interaction_env(this.__env__, inter);
}
// -------------------------------------------------------------------------
Interpreter.prototype.exec = async function(arg, options = {}) {
    let {
        use_dynamic = false,
        dynamic_env,
        env
    } = options;
    typecheck('Interpreter::exec', arg, ['string', 'array'], 1);
    typecheck('Interpreter::exec', use_dynamic, 'boolean', 2);
    // simple solution to overwrite this variable in each interpreter
    // before evaluation of user code
    if (!env) {
        env = this.__env__;
    }
    if (!dynamic_env) {
        dynamic_env = env;
    }
    global_env.set('**interaction-environment**', this.__env__);
    if (Array.isArray(arg)) {
        return exec(arg, { env, dynamic_env, use_dynamic });
    } else {
        this.__parser__.parse(arg);
        return exec(this.__parser__, { env, dynamic_env, use_dynamic });
    }
};
// -------------------------------------------------------------------------
Interpreter.prototype.get = function(value) {
    const result = this.__env__.get(value);
    if (is_function(result)) {
        const context = new LambdaContext({
            env: this.__env__
        });
        return result.bind(context);
    }
    return result;
};
// -------------------------------------------------------------------------
Interpreter.prototype.set = function(name, value) {
    return this.__env__.set(name, value);
};
// -------------------------------------------------------------------------
Interpreter.prototype.constant = function(name, value) {
    return this.__env__.constant(name, value);
};
// -------------------------------------------------------------------------
// Lips Exception used in error function
// -------------------------------------------------------------------------
function LipsError(message, args) {
    this.name = 'LipsError';
    this.message = message;
    this.args = args;
    this.stack = (new Error()).stack;
}
LipsError.prototype = new Error();
LipsError.prototype.constructor = LipsError;
// -------------------------------------------------------------------------
// :: Fake exception to handle try catch to break the execution
// :: of body expression #163
// -------------------------------------------------------------------------
class IgnoreException extends Error { }
// -------------------------------------------------------------------------
// :: Environment constructor (parent and name arguments are optional)
// -------------------------------------------------------------------------
function Environment(obj, parent, name) {
    if (arguments.length === 1) {
        if (typeof arguments[0] === 'object') {
            obj = arguments[0];
            parent = null;
        } else if (typeof arguments[0] === 'string') {
            obj = {};
            parent = null;
            name = arguments[0];
        }
    }
    this.__docs__ = new Map();
    this.__env__ = obj;
    this.__parent__ = parent;
    this.__name__ = name || 'anonymous';
}
// -------------------------------------------------------------------------
Environment.prototype.list = function() {
    return get_props(this.__env__);
};
// -------------------------------------------------------------------------
Environment.prototype.fs = function() {
    return this.get('**fs**');
};
// -------------------------------------------------------------------------
Environment.prototype.unset = function(name) {
    if (name instanceof LSymbol) {
        name = name.valueOf();
    }
    if (name instanceof LString) {
        name = name.valueOf();
    }
    delete this.__env__[name];
};
// -------------------------------------------------------------------------
Environment.prototype.inherit = function(name, obj = {}) {
    if (typeof name === "object") {
        obj = name;
    }
    if (!name || typeof name === "object") {
        name = 'child of ' + (this.__name__ || 'unknown');
    }
    return new Environment(obj || {}, this, name);
};
// -------------------------------------------------------------------------
// :: Lookup function for variable doc strings
// -------------------------------------------------------------------------
Environment.prototype.doc = function(name, value = null, dump = false) {
    if (name instanceof LSymbol) {
        name = name.__name__;
    }
    if (name instanceof LString) {
        name = name.valueOf();
    }
    if (value) {
        if (!dump) {
            value = trim_lines(value);
        }
        this.__docs__.set(name, value);
        return this;
    }
    if (this.__docs__.has(name)) {
        return this.__docs__.get(name);
    }
    if (this.__parent__) {
        return this.__parent__.doc(name);
    }
};
// -------------------------------------------------------------------------
// :: Function creates frame environment for usage in functions
// :: frames are used to it's easier to find environments of the functions
// :: in scope chain, they are dummy environments just for lookup
// -------------------------------------------------------------------------
Environment.prototype.new_frame = function(fn, args) {
    var frame = this.inherit('__frame__');
    frame.set('parent.frame', doc('parent.frame', function(n = 1) {
        n = n.valueOf();
        var scope = frame.__parent__;
        if (!is_env(scope)) {
            return nil;
        }
        if (n <= 0) {
            return scope;
        }
        var parent_frame = scope.get('parent.frame');
        return parent_frame(n - 1);
    }, global_env.__env__['parent.frame'].__doc__));
    args.callee = fn;
    frame.set('arguments', args);
    return frame;
};
// -------------------------------------------------------------------------
Environment.prototype._lookup = function(symbol) {
    if (symbol instanceof LSymbol) {
        symbol = symbol.__name__;
    }
    if (symbol instanceof LString) {
        symbol = symbol.valueOf();
    }
    if (this.__env__.hasOwnProperty(symbol)) {
        return Value(this.__env__[symbol]);
    }
    if (this.__parent__) {
        return this.__parent__._lookup(symbol);
    }
};
// -------------------------------------------------------------------------
Environment.prototype.toString = function() {
    return '#<environment:' + this.__name__ + '>';
};
// -------------------------------------------------------------------------
Environment.prototype.clone = function() {
    // duplicate refs
    var env = {};
    // TODO: duplicated Symbols
    Object.keys(this.__env__).forEach(key => {
        env[key] = this.__env__[key];
    });
    return new Environment(env, this.__parent__, this.__name__);
};
// -------------------------------------------------------------------------
Environment.prototype.merge = function(env, name = 'merge') {
    typecheck('Environment::merge', env, 'environment');
    return this.inherit(name, env.__env__);
};
// -------------------------------------------------------------------------
// Value returned in lookup if found value in env and in promise_all
// -------------------------------------------------------------------------
function Value(value) {
    if (typeof this !== 'undefined' && !(this instanceof Value) ||
        typeof this === 'undefined') {
        return new Value(value);
    }
    this.value = value;
}
// -------------------------------------------------------------------------
Value.isUndefined = function(x) {
    return x instanceof Value && typeof x.value === 'undefined';
};
// -------------------------------------------------------------------------
Value.prototype.valueOf = function() {
    return this.value;
};
// -------------------------------------------------------------------------
// :: Different object than value used as object for (values)
// -------------------------------------------------------------------------
function Values(values) {
    if (!values.length) {
        return;
    }
    if (values.length === 1) {
        return values[0];
    }
    if (typeof this !== 'undefined' && !(this instanceof Values) ||
        typeof this === 'undefined') {
        return new Values(values);
    }
    this.__values__ = values;
}
Values.prototype.toString = function() {
    return this.__values__.map(x => toString(x)).join('\n');
};
Values.prototype.valueOf = function() {
    return this.__values__;
};
// -------------------------------------------------------------------------
Environment.prototype.get = function(symbol, options = {}) {
    // we keep original environment as context for bind
    // so print will get user stdout
    typecheck('Environment::get', symbol, ['symbol', 'string']);
    const { throwError = true } = options;
    var name = symbol;
    if (name instanceof LSymbol || name instanceof LString) {
        name = name.valueOf();
    }
    var value = this._lookup(name);
    if (value instanceof Value) {
        if (Value.isUndefined(value)) {
            return undefined;
        }
        return patch_value(value.valueOf());
    }
    var parts;
    if (symbol instanceof LSymbol && symbol[LSymbol.object]) {
        // dot notation symbols from syntax-rules that are gensyms
        parts = symbol[LSymbol.object];
    } else if (typeof name === 'string') {
        parts = name.split('.').filter(Boolean);
    }
    if (parts && parts.length > 0) {
        var [first, ...rest] = parts;
        value = this._lookup(first);
        if (rest.length) {
            try {
                if (value instanceof Value) {
                    value = value.valueOf();
                } else {
                    value = get(root, first);
                    if (is_function(value)) {
                        value = unbind(value);
                    }
                }
                if (typeof value !== 'undefined') {
                    // object accessor
                    return get(value, ...rest);
                }
            } catch (e) {
                throw e;
            }
        } else if (value instanceof Value) {
            return patch_value(value.valueOf());
        }
        value = get(root, name);
    }
    if (typeof value !== 'undefined') {
        return value;
    }
    if (throwError) {
        throw new Error("Unbound variable `" + name.toString() + "'");
    }
};
// -------------------------------------------------------------------------
Environment.prototype.set = function(name, value, doc = null) {
    typecheck('Environment::set', name, ['string', 'symbol']);
    if (LNumber.isNumber(value)) {
        value = LNumber(value);
    }
    if (name instanceof LSymbol) {
        name = name.__name__;
    }
    if (name instanceof LString) {
        name = name.valueOf();
    }
    this.__env__[name] = value;
    if (doc) {
        this.doc(name, doc, true);
    }
    return this;
};
// -------------------------------------------------------------------------
// For internal use only
// -------------------------------------------------------------------------
Environment.prototype.constant = function(name, value) {
    if (this.__env__.hasOwnProperty(name)) {
        throw new Error(`Environment::constant: ${name} already exists`);
    }
    if (arguments.length === 1 && is_plain_object(arguments[0])) {
        var obj = arguments[0];
        Object.keys(obj).forEach(key => {
            this.constant(name, obj[key]);
        });
    } else {
        Object.defineProperty(this.__env__, name, {
            value,
            enumerable: true
        });
    }
    return this;
};
// -------------------------------------------------------------------------
Environment.prototype.has = function(name) {
    return this.__env__.hasOwnProperty(name);
};
// -------------------------------------------------------------------------
Environment.prototype.ref = function(name) {
    var env = this;
    while (true) {
        if (!env) {
            break;
        }
        if (env.has(name)) {
            return env;
        }
        env = env.__parent__;
    }
};
// -------------------------------------------------------------------------
Environment.prototype.parents = function() {
    var env = this;
    var result = [];
    while (env) {
        result.unshift(env);
        env = env.__parent__;
    }
    return result;
};
// -------------------------------------------------------------------------
// :: Quote function used to pause evaluation from Macro
// -------------------------------------------------------------------------
function quote(value) {
    if (is_promise(value)) {
        return value.then(quote);
    }
    if (is_pair(value) || value instanceof LSymbol) {
        value[__data__] = true;
    }
    return value;
}
// -------------------------------------------------------------------------
// :: Unquote is used for multiple backticks and unquote
// -------------------------------------------------------------------------
function Unquote(value, count, max) {
    this.value = value;
    this.count = count;
    this.max = max;
}
Unquote.prototype.toString = function() {
    return '#<unquote[' + this.count + '] ' + this.value + '>';
};
// -------------------------------------------------------------------------------
const native_lambda = _parse(tokenize(`(lambda ()
                                        "[native code]"
                                        (throw "Invalid Invocation"))`))[0];
// -------------------------------------------------------------------------------
var get = doc('get', function get(object, ...args) {
    var value;
    var len = args.length;
    while (args.length) {
        // if arg is symbol someone probably want to get __fn__ from binded function
        if (is_function(object) && typeof args[0] !== 'symbol') {
            object = unbind(object);
        }
        var arg = args.shift();
        var name = unbox(arg);
        // the value was set to false to prevent resolving
        // by Real Promises #153
        if (name === 'then' && object instanceof QuotedPromise) {
            value = QuotedPromise.prototype.then;
        } else if (name === '__code__' && is_function(object) &&
                   typeof object.__code__ === 'undefined') {
            value = native_lambda;
        } else {
            value = object[name];
        }
        if (typeof value === 'undefined') {
            if (args.length) {
                throw new Error(`Try to get ${args[0]} from undefined`);
            }
            return value;
        } else {
            var context;
            if (args.length - 1 < len) {
                context = object;
            }
            value = patch_value(value, context);
        }
        object = value;
    }
    return value;
}, `(. obj . args)
    (get obj . args)

    This function uses an object as a base and keeps using arguments to get the
    property of JavaScript object. Arguments need to be a strings.
    e.g. \`(. console "log")\` if you use any function inside LIPS it
    will be weakly bound (can be rebound), so you can call this log function
    without problem unlike in JavaScript when you use
    \`var log = console.log\`.
    \`get\` is an alias because . doesn't work everywhere, e.g. you can't
    pass it as an argument.`);
// -------------------------------------------------------------------------
// Function gets internal protected data
// -------------------------------------------------------------------------
function get_internal(env) {
    return interaction(env, '**internal-env**');
}
// -------------------------------------------------------------------------
function internal(env, name) {
    const internal_env = get_internal(env);
    return internal_env.get(name);
}
// -------------------------------------------------------------------------
// Get variable from interaction environment
// -------------------------------------------------------------------------
function interaction(env, name) {
    var interaction_env = env.get('**interaction-environment**');
    return interaction_env.get(name);
}
// -------------------------------------------------------------------------
var internal_env = new Environment({
    stdout: new BufferedOutputPort(function(...args) {
        console.log(...args);
    }),
    // ------------------------------------------------------------------
    stderr: new BufferedOutputPort(function(...args) {
        console.error(...args);
    }),
    'command-line': [],
    // ------------------------------------------------------------------
    stdin: InputPort(function() {
        return Promise.resolve(prompt(''));
    }),
    // those will be compiled by babel regex plugin
    'letter-unicode-regex': /\p{L}/u,
    'numeral-unicode-regex': /\p{N}/u,
    'space-unicode-regex': /\s/u
}, undefined, 'internal');
// ----------------------------------------------------------------------
var nan = LNumber(NaN);
var constants = {
    '#t': true,
    '#f': false,
    '#true': true,
    '#false': false,
    '+inf.0': Number.POSITIVE_INFINITY,
    '-inf.0': Number.NEGATIVE_INFINITY,
    '+nan.0': nan,
    '-nan.0': nan,
    ...parsable_contants
};
// -------------------------------------------------------------------------
var global_env = new Environment({
    eof,
    undefined, // undefined as parser constant breaks most of the unit tests
    // ---------------------------------------------------------------------
    'peek-char': doc('peek-char', function(port = null) {
        if (port === null) {
            port = internal(this, 'stdin');
        }
        typecheck_text_port('peek-char', port, 'input-port');
        return port.peek_char();
    }, `(peek-char port)

        This function reads and returns a character from the string
        port, or, if there is no more data in the string port, it
        returns an EOF.`),
    // ------------------------------------------------------------------
    'read-line': doc('read-line', function(port = null) {
        if (port === null) {
            port = internal(this, 'stdin');
        }
        typecheck_text_port('read-line', port, 'input-port');
        return port.read_line();
    }, `(read-line port)

        This function reads and returns the next line from the input
        port.`),
    // ------------------------------------------------------------------
    'read-char': doc('read-char', function(port = null) {
        if (port === null) {
            port = internal(this, 'stdin');
        }
        typecheck_text_port('read-char', port, 'input-port');
        return port.read_char();
    }, `(read-char port)

        This function reads and returns the next character from the
        input port.`),
    // ------------------------------------------------------------------
    read: doc('read', async function read(arg = null) {
        const { env } = this;
        var port;
        if (arg === null) {
            port = internal(env, 'stdin');
        } else {
            port = arg;
        }
        typecheck_text_port('read', port, 'input-port');
        return port.read.call(env);
    }, `(read [port])

        This function, if called with a port, it will parse the next
        item from the port. If called without an input, it will read
        a string from standard input (using the browser's prompt or
        a user defined input method) and parse it. This function can be
        used together with \`eval\` to evaluate code from port.`),
    // ------------------------------------------------------------------
    pprint: doc('pprint', function pprint(arg) {
        if (is_pair(arg)) {
            arg = new lips.Formatter(arg.toString(true)).break().format();
            global_env.get('display').call(global_env, arg);
        } else {
            global_env.get('write').call(global_env, arg);
        }
        global_env.get('newline').call(global_env);
    }, `(pprint expression)

        This function will pretty print its input to stdout. If it is called
        with a non-list, it will just call the print function on its
        input.`),
    // ------------------------------------------------------------------
    print: doc('print', function print(...args) {
        const display = global_env.get('display');
        const newline = global_env.get('newline');
        const { use_dynamic } = this;
        const env = global_env;
        const dynamic_env = global_env;
        args.forEach(arg => {
            call_function(display, [arg], { env, dynamic_env, use_dynamic });
            call_function(newline, [], { env, dynamic_env, use_dynamic });
        });
    }, `(print . args)

        This function converts each input into a string and prints
        the result to the standard output (by default it's the
        console but it can be defined in user code). This function
        calls \`(newline)\` after printing each input.`),
    // ------------------------------------------------------------------
    format: doc('format', function format(str, ...args) {
        typecheck('format', str, 'string');
        const re = /(~[as%~])/g;
        let m = str.match(/(~[as])/g);
        if (m && m.length > args.length) {
            throw new Error('Not enough arguments');
        }
        var i = 0;
        var repr = global_env.get('repr');
        str = str.replace(re, (x) => {
            const chr = x[1];
            if (chr === '~') {
                return '~';
            } else if (chr === '%') {
                return '\n';
            } else {
                const arg = args[i++];
                if (chr === 'a') {
                    return repr(arg);
                } else {
                    return repr(arg, true);
                }
            }
        });
        m = str.match(/~([\S])/);
        if (m) {
            throw new Error(`format: Unrecognized escape sequence ${m[1]}`);
        }
        return str;
    }, `(format string n1 n2 ...)

        This function accepts a string template and replaces any
        escape sequences in its inputs:

        * ~a value as if printed with \`display\`
        * ~s value as if printed with \`write\`
        * ~% newline character
        * ~~ literal tilde '~'

        If there are missing inputs or other escape characters it
        will error.`),
    // ------------------------------------------------------------------
    newline: doc('newline', function newline(port = null) {
        const display = global_env.get('display');
        const { use_dynamic } = this;
        const env = global_env;
        const dynamic_env = global_env;
        call_function(display, ['\n', port], { env, dynamic_env, use_dynamic });
    }, `(newline [port])

        Write newline character to standard output or given port`),
    // ------------------------------------------------------------------
    display: doc('display', function display(arg, port = null) {
        if (port === null) {
            port = internal(this, 'stdout');
        } else {
            typecheck('display', port, 'output-port');
        }
        let value = arg;
        if (!(port instanceof OutputBinaryFilePort)) {
            value = global_env.get('repr')(arg);
        }
        port.write.call(global_env, value);
    }, `(display string [port])

        This function outputs the string to the standard output or
        the port if given. No newline.`),
    // ------------------------------------------------------------------
    'display-error': doc('display-error', function error(...args) {
        const port = internal(this, 'stderr');
        const repr = global_env.get('repr');
        const value = args.map(repr).join(' ');
        port.write.call(global_env, value);
        global_env.get('newline')(port);
    }, `(display-error . args)

        Display an error message on stderr.`),
    // ------------------------------------------------------------------
    '%foldcase-string': doc(
        '%foldcase-string',
        foldcase_string,
        `(%foldcase-string string)

         Same as string-foldcase but without typechecking`),
    // ------------------------------------------------------------------
    '%same-functions': doc('%same-functions', function(a, b) {
        if (!is_function(a)) {
            return false;
        }
        if (!is_function(b)) {
            return false;
        }
        return unbind(a) === unbind(b);
    }, `(%same-functions a b)

        A helper function that checks if the two input functions are
        the same.`),
    // ------------------------------------------------------------------
    help: doc(new Macro('help', function(code, { dynamic_env, use_dynamic, error }) {
        var symbol;
        if (code.car instanceof LSymbol) {
            symbol = code.car;
        } else if (is_pair(code.car) && code.car.car instanceof LSymbol) {
            symbol = code.car.car;
        } else {
            var env = this;
            dynamic_env = this;
            var ret = evaluate(code.car, { env, error, dynamic_env, use_dynamic });
            if (ret && ret.__doc__) {
                return ret.__doc__;
            }
            return;
        }
        var __doc__;
        var value = this.get(symbol);
        __doc__ = value && value.__doc__;
        if (__doc__) {
            return __doc__;
        }
        var ref = this.ref(symbol);
        if (ref) {
            __doc__ = ref.doc(symbol);
            if (__doc__) {
                return __doc__;
            }
        }
    }), `(help object)

         This macro returns documentation for a function or macro.
         You can save the function or macro in a variable and use it
         here. But getting help for a variable requires passing the
         variable in a \`quote\`.`),
    // ------------------------------------------------------------------
    cons: doc('cons', function cons(car, cdr) {
        return new Pair(car, cdr);
    }, `(cons left right)

        This function returns a new list with the first appended
        before the second. If the second is not a list cons will
        return a dotted pair.`),
    // ------------------------------------------------------------------
    car: doc('car', function car(list) {
        typecheck('car', list, 'pair');
        return list.car;
    }, `(car pair)

        This function returns the car (item 1) of the list.`),
    // ------------------------------------------------------------------
    cdr: doc('cdr', function cdr(list) {
        typecheck('cdr', list, 'pair');
        return list.cdr;
    }, `(cdr pair)

        This function returns the cdr (all but first) of the list.`),
    // ------------------------------------------------------------------
    'set!': doc(new Macro('set!', function(code, { use_dynamic, ...rest } = {}) {
        const dynamic_env = this;
        const env = this;
        let ref;
        const eval_args = { ...rest, env: this, dynamic_env, use_dynamic };
        let value = evaluate(code.cdr.car, eval_args);
        value = resolve_promises(value);
        function set(object, key, value) {
            if (is_promise(object)) {
                return object.then(key => set(object, key, value));
            }
            if (is_promise(key)) {
                return key.then(key => set(object, key, value));
            }
            if (is_promise(value)) {
                return value.then(value => set(object, key, value));
            }
            env.get('set-obj!').call(env, object, key, value);
            return value;
        }
        if (is_pair(code.car) && LSymbol.is(code.car.car, '.')) {
            var second = code.car.cdr.car;
            var third = code.car.cdr.cdr.car;
            var object = evaluate(second, eval_args);
            var key = evaluate(third, eval_args);
            return set(object, key, value);
        }
        if (!(code.car instanceof LSymbol)) {
            throw new Error('set! first argument need to be a symbol or ' +
                            'dot accessor that evaluate to object.');
        }
        var symbol = code.car.valueOf();
        ref = this.ref(code.car.__name__);
        // we don't return value because we only care about sync of set value
        // when value is a promise
        return unpromise(value, value => {
            if (!ref) {
                // case (set! fn.toString (lambda () "xxx"))
                var parts = symbol.split('.');
                if (parts.length > 1) {
                    var key = parts.pop();
                    var name = parts.join('.');
                    var obj = this.get(name, { throwError: false });
                    if (obj) {
                        set(obj, key, value);
                        return;
                    }
                }
                throw new Error('Unbound variable `' + symbol + '\'');
            }
            ref.set(symbol, value);
        });
    }), `(set! name value)

         Macro that can be used to set the value of the variable or slot (mutate it).
         set! searches the scope chain until it finds first non empty slot and sets it.`),
    // ------------------------------------------------------------------
    'unset!': doc(new Macro('set!', function(code) {
        if (!(code.car instanceof LSymbol)) {
            throw new Error('unset! first argument need to be a symbol or ' +
                            'dot accessor that evaluate to object.');
        }
        const symbol = code.car;
        var ref = this.ref(symbol);
        if (ref) {
            delete ref.__env__[symbol.__name__];
        }
    }), `(unset! name)

         Function to delete the specified name from environment.
         Trying to access the name afterwards will error.`),
    // ------------------------------------------------------------------
    'set-car!': doc('set-car!', function(slot, value) {
        typecheck('set-car!', slot, 'pair');
        slot.car = value;
    }, `(set-car! obj value)

         Function that sets the car (first item) of the list/pair to specified value.
         The old value is lost.`),
    // ------------------------------------------------------------------
    'set-cdr!': doc('set-cdr!', function(slot, value) {
        typecheck('set-cdr!', slot, 'pair');
        slot.cdr = value;
    }, `(set-cdr! obj value)

         Function that sets the cdr (tail) of the list/pair to specified value.
         It will destroy the list. The old tail is lost.`),
    // ------------------------------------------------------------------
    'empty?': doc('empty?', function(x) {
        return typeof x === 'undefined' || is_nil(x);
    }, `(empty? object)

         Function that returns #t if value is nil (an empty list) or undefined.`),
    // ------------------------------------------------------------------
    gensym: doc(
        'gensym',
        gensym,
        `(gensym)

         Generates a unique symbol that is not bound anywhere,
         to use with macros as meta name.`),
    // ------------------------------------------------------------------
    load: doc('load', function load(file, env) {
        typecheck('load', file, 'string');
        var g_env = this;
        if (g_env.__name__ === '__frame__') {
            g_env = g_env.__parent__;
        }
        if (!(env instanceof Environment)) {
            if (g_env === global_env) {
                // this is used for let-env + load
                // this may be obsolete when there is env arg
                env = g_env;
            } else {
                env = this.get('**interaction-environment**');
            }
        }
        const package_name = '@lips';
        const has_package = file.startsWith(package_name);
        // TODO: move **module-path** to internal env
        const PATH = '**module-path**';
        var module_path = global_env.get(PATH, { throwError: false });
        file = file.valueOf();
        if (!file.match(/.[^.]+$/)) {
            file += '.scm';
        }
        const IS_BIN = file.match(/\.xcb$/);
        function run(code) {
            if (IS_BIN) {
                code = unserialize_bin(code);
            } else {
                if (type(code) === 'buffer') {
                    code = code.toString();
                }
                code = code.replace(/^(#!.*)/, function(_, shebang) {
                    if (is_directive(shebang)) {
                        return shebang;
                    }
                    return '';
                });
                if (code.match(/^\{/)) {
                    code = unserialize(code);
                }
            }
            return exec(code, { env });
        }
        function fetch(file) {
            return root.fetch(file)
                .then(res => IS_BIN ? res.arrayBuffer() : res.text())
                .then((code) => {
                    if (IS_BIN) {
                        code = new Uint8Array(code);
                    }
                    return code;
                });
        }
        function get_root_dir() {
            const __dirname = global_env.get('__dirname');
            return __dirname.replace(/[^/]+$/, '');
        }
        if (is_node()) {
            return new Promise(async (resolve, reject) => {
                try {
                    await node_ready;
                    const path = nodeRequire('path');
                    const fs = nodeRequire('fs');
                    let cwd;
                    const root_dir = get_root_dir();
                    if (has_package) {
                        file = file.replace(package_name, root_dir);
                    }
                    if (module_path) {
                        module_path = module_path.valueOf();
                        if (!file.startsWith('/')) {
                            file = path.join(module_path, file);
                        }
                    } else if (!file.startsWith('/')) {
                        const cmd = g_env.get('command-line', { throwError: false });
                        let args;
                        if (cmd) {
                            args = await cmd();
                        }
                        if (args && !is_nil(args)) {
                            cwd = process.cwd();
                            file = path.join(path.dirname(args.car.valueOf()), file);
                        }
                    }
                    global_env.set(PATH, path.dirname(file));
                    fs.readFile(file, function(err, data) {
                        if (err) {
                            reject(err);
                            global_env.set(PATH, module_path);
                        } else {
                            try {
                                run(data).then(() => {
                                    resolve();
                                    global_env.set(PATH, module_path);
                                }).catch(reject);
                            } catch (e) {
                                reject(e);
                            }
                        }
                    });
                } catch(e) {
                    console.error(e);
                }
            });
        }
        if (has_package) {
            let path = global_env.get('__dirname', { throwError: false }) ?? current_script;
            path ??= current_script;
            const root = path.replace(/dist\/?[^\/]*$/, '');
            file = file.replace(package_name, root);
        }
        if (module_path) {
            module_path = module_path.valueOf();
            if (!file.startsWith('/')) {
                file = module_path + '/' + file.replace(/^\.?\/?/, '');
            }
        }
        return fetch(file).then(code => {
            global_env.set(PATH, file.replace(/\/[^/]*$/, ''));
            return run(code);
        }).then(() => {}).finally(() => {
            global_env.set(PATH, module_path);
        });
    }, `(load filename)
        (load filename environment)

        Fetches the file (from disk or network) and evaluates its content as LIPS code.
        If the second argument is provided and it's an environment the evaluation
        will happen in that environment.`),
    // ------------------------------------------------------------------
    'while': doc(new Macro('while', function(code, args) {
        const test = code.car;
        const eval_args = { ...args, env: this };
        const body = new Pair(new LSymbol('begin'), code.cdr);
        return (function loop() {
            return unpromise(evaluate(test, eval_args), test => {
                if (test) {
                    return unpromise(evaluate(body, eval_args), loop);
                }
            });
        })();
    }), `(while cond body)

         Creates a loop, it executes cond and body until cond expression is false.`),
    // ------------------------------------------------------------------
    'do': doc(new Macro('do', async function(code, { use_dynamic, error }) {
        const self = this;
        const dynamic_env = self;
        let scope = self.inherit('do');
        const vars = code.car;
        const test = code.cdr.car;
        let body = code.cdr.cdr;
        if (!is_nil(body)) {
            body = new Pair(LSymbol('begin'), body);
        }
        let eval_args = { env: self, dynamic_env, use_dynamic, error };
        let node = vars;
        // init variables
        while (!is_nil(node)) {
            const item = node.car;
            scope.set(item.car, await evaluate(item.cdr.car, eval_args));
            node = node.cdr;
        }
        eval_args = { env: scope, dynamic_env, error };
        while ((await evaluate(test.car, eval_args)) === false) {
            if (!is_nil(body)) {
                await lips.evaluate(body, eval_args);
            }
            let node = vars;
            const next = {};
            // next value of variables
            while (!is_nil(node)) {
                const item = node.car;
                if (!is_nil(item.cdr.cdr)) {
                    const value = await evaluate(item.cdr.cdr.car, eval_args);
                    next[item.car.valueOf()] = value;
                }
                node = node.cdr;
            }
            const symbols = Object.getOwnPropertySymbols(next);
            // new scope for new iteration
            eval_args.env = scope = self.inherit('do');
            Object.keys(next).concat(symbols).forEach(key => {
                scope.set(key, next[key]);
            });
        }
        if (!is_nil(test.cdr)) {
            return await evaluate(test.cdr.car, eval_args);
        }
    }), `(do ((<var> <init> <next>)) (test return) . body)

         Iteration macro that evaluates the expression body in scope of the variables.
         On each loop it changes the variables according to the <next> expression and runs
         test to check if the loop should continue. If test is a single value, the macro
         will return undefined. If the test is a pair of expressions the macro will
         evaluate and return the second expression after the loop exits.`),
    // ------------------------------------------------------------------
    'if': doc(new Macro('if', function(code, { error, use_dynamic }) {
        const dynamic_env = this;
        const env = this;
        const eval_args = { env, dynamic_env, use_dynamic, error };
        const resolve = (cond) => {
            if (is_false(cond)) {
                return evaluate(code.cdr.cdr.car, eval_args);
            } else {
                return evaluate(code.cdr.car, eval_args);
            }
        };
        if (is_nil(code)) {
            throw new Error('too few expressions for `if`');
        }
        const cond = evaluate(code.car, eval_args);
        return unpromise(cond, resolve);
    }), `(if cond true-expr false-expr)

         Macro that evaluates cond expression and if the value is true, it
         evaluates and returns true-expression, if not it evaluates and returns
         false-expression.`),
    // ------------------------------------------------------------------
    'let-env': new Macro('let-env', function(code, options = {}) {
        const { dynamic_env, use_dynamic, error } = options;
        typecheck('let-env', code, 'pair');
        const ret = evaluate(code.car, { env: this, dynamic_env, error, use_dynamic });
        return unpromise(ret, function(value) {
            typecheck('let-env', value, 'environment');
            return evaluate(Pair(LSymbol('begin'), code.cdr), {
                env: value, dynamic_env, error
            });
        });
    }, `(let-env env . body)

        Special macro that evaluates body in context of given environment
        object.`),
    // ------------------------------------------------------------------
    'letrec': doc(
        let_macro(Symbol.for('letrec')),
        `(letrec ((a value-a) (b value-b) ...) . body)

         Macro that creates a new environment, then evaluates and assigns values to
         names and then evaluates the body in context of that environment.
         Values are evaluated sequentially and the next value can access the
         previous values/names.`),
    // ---------------------------------------------------------------------
    'letrec*': doc(
        let_macro(Symbol.for('letrec')),
        `(letrec* ((a value-a) (b value-b) ...) . body)

         Same as letrec but the order of execution of the binding is guaranteed,
         so you can use recursive code as well as referencing the previous binding.

         In LIPS both letrec and letrec* behave the same.`),
    // ---------------------------------------------------------------------
    'let*': doc(
        let_macro(Symbol.for('let*')),
        `(let* ((a value-a) (b value-b) ...) . body)

         Macro similar to \`let\`, but the subsequent bindings after the first
         are evaluated in the environment including the previous let variables,
         so you can define one variable, and use it in the next's definition.`),
    // ---------------------------------------------------------------------
    'let': doc(
        let_macro(Symbol.for('let')),
        `(let ((a value-a) (b value-b) ...) . body)

         Macro that creates a new environment, then evaluates and assigns values to names,
         and then evaluates the body in context of that environment.  Values are evaluated
         sequentially but you can't access previous values/names when the next are
         evaluated. You can only get them in the body of the let expression.  (If you want
         to define multiple variables and use them in each other's definitions, use
         \`let*\`.)`),
    // ------------------------------------------------------------------
    'begin*': doc(parallel('begin*', function(values) {
        return values.pop();
    }), `(begin* . body)

         This macro is a parallel version of begin. It evaluates each expression
         in the body and if it's a promise it will await it in parallel and return
         the value of the last expression (i.e. it uses Promise.all()).`),
    // ------------------------------------------------------------------
    shuffle: doc('shuffle', function(arg) {
        typecheck('shuffle', arg, ['pair', 'nil', 'array']);
        const random = global_env.get('random')
        if (is_nil(arg)) {
            return nil;
        }
        if (Array.isArray(arg)) {
            return shuffle(arg.slice(), random);
        }
        let arr = global_env.get('list->array')(arg);
        arr = shuffle(arr, random);

        return global_env.get('array->list')(arr);
    }, `(shuffle obj)

        Order items in vector or list in random order.`),
    // ------------------------------------------------------------------
    begin: doc(new Macro('begin', function(code, options) {
        const eval_args = {...options, env: this };
        const arr = global_env.get('list->array')(code);
        let result;
        return (function loop() {
            if (arr.length) {
                const code = arr.shift();
                const ret = evaluate(code, eval_args);
                return unpromise(ret, value => {
                    result = value;
                    return loop();
                });
            } else {
                return result;
            }
        })();
    }), `(begin . args)

         Macro that runs a list of expressions in order and returns the value
         of the last one. It can be used in places where you can only have a
         single expression, like (if).`),
    // ------------------------------------------------------------------
    'ignore': new Macro('ignore', function(code, options) {
        const eval_args = { ...options, env: this, dynamic_env: this };
        evaluate(new Pair(new LSymbol('begin'), code), eval_args);
    }, `(ignore . body)

        Macro that will evaluate the expression and swallow any promises that may
        be created. It will discard any value that may be returned by the last body
        expression. The code should have side effects and/or when it's promise
        it should resolve to undefined.`),
    // ------------------------------------------------------------------
    'call/cc': doc(Macro.defmacro('call/cc', function(code, eval_args = {}) {
        const args = {
            env: this,
            ...eval_args
        };
        return unpromise(evaluate(code.car, args), (result) => {
            if (is_function(result)) {
                return result(new Continuation(null));
            }
        });
    }), `(call/cc proc)

         Call-with-current-continuation.

         NOT SUPPORTED BY LIPS RIGHT NOW`),
    // ------------------------------------------------------------------
    parameterize: doc(new Macro('parameterize', function(code, options) {
        const { dynamic_env } = options;
        const env = dynamic_env.inherit('parameterize').new_frame(null, {});
        const eval_args = { ...options, env: this };
        let params = code.car;
        if (!is_pair(params)) {
            const t = type(params);
            throw new Error(`Invalid syntax for parameterize expecting pair got ${t}`);
        }
        function next() {
            const body = new Pair(new LSymbol('begin',), code.cdr);
            return evaluate(body, { ...eval_args, dynamic_env: env });
        }
        return (function loop() {
            const pair = params.car;
            const name = pair.car.valueOf();
            return unpromise(evaluate(pair.cdr.car, eval_args), function(value) {
                const param = dynamic_env.get(name, { throwError: false });
                if (!is_parameter(param)) {
                    throw new Error(`Unknown parameter ${name}`);
                }
                env.set(name, param.inherit(value));
                if (!is_null(params.cdr)) {
                    params = params.cdr;
                    return loop();
                } else {
                    return next();
                }
            });
        })();
    }), `(parameterize ((name value) ...)

         Macro that change the dynamic variable created by make-parameter.`),
    // ------------------------------------------------------------------
    'make-parameter': doc(new Macro('make-parameter', function(code, eval_args) {
        const dynamic_env = eval_args.dynamic_env;
        const init = evaluate(code.car, eval_args);
        let fn;
        if (is_pair(code.cdr.car)) {
            fn = evaluate(code.cdr.car, eval_args);
        }
        return new Parameter(init, fn);
    }), `(make-parameter init converter)

    Function creates new dynamic variable that can be custimized with parameterize
    macro. The value should be assigned to a variable e.g.:

    (define radix (make-parameter 10))

    The result value is a procedure that return the value of dynamic variable.`),
    // ------------------------------------------------------------------
    'define-syntax-parameter': doc(new Macro('define-syntax-parameter', function(code, eval_args) {
        const name = code.car;
        const env = this;
        if (!(name instanceof LSymbol)) {
            throw new Error(`define-syntax-parameter: invalid syntax expecting symbol got ${type(name)}`);
        }
        const syntax = evaluate(code.cdr.car, { env, ...eval_args });
        typecheck('define-syntax-parameter', syntax, 'syntax', 2);
        syntax.__name__ = name.valueOf();
        if (syntax.__name__ instanceof LString) {
            syntax.__name__ = syntax.__name__.valueOf();
        }
        let __doc__;
        if (is_pair(code.cdr.cdr) &&
            LString.isString(code.cdr.cdr.car)) {
            __doc__ = code.cdr.cdr.car.valueOf();
        }
        env.set(code.car, new SyntaxParameter(syntax), __doc__, true);
    }), `(define-syntax-parameter name syntax [__doc__])

         Binds <keyword> to the transformer obtained by evaluating <transformer spec>.
         The transformer provides the default expansion for the syntax parameter,
         and in the absence of syntax-parameterize, is functionally equivalent to
         define-syntax.`),
    // ------------------------------------------------------------------
    'syntax-parameterize': doc(new Macro('syntax-parameterize', function(code, eval_args) {
        const args = global_env.get('list->array')(code.car);
        const env = this.inherit('syntax-parameterize');
        while (args.length) {
            const pair = args.shift();
            if (!(is_pair(pair) || pair.car instanceof LSymbol)) {
                const msg = `invalid syntax for syntax-parameterize: ${repr(code, true)}`;
                throw new Error(`syntax-parameterize: ${msg}`);
            }
            let syntax = evaluate(pair.cdr.car, { ...eval_args, env: this });
            const name = pair.car;
            typecheck('syntax-parameterize', syntax, ['syntax']);
            typecheck('syntax-parameterize', name, 'symbol');
            syntax.__name__ = name.valueOf();
            if (syntax.__name__ instanceof LString) {
                syntax.__name__ = syntax.__name__.valueOf();
            }
            const parameter = new SyntaxParameter(syntax);
            // used inside syntax-rules
            if (name.is_gensym()) {
                const symbol = name.literal();
                const parent = this.get(symbol, { throwError: false });
                if (parent instanceof SyntaxParameter) {
                    // create anaphoric binding for literal symbol
                    env.set(symbol, parameter);
                }
            }
            env.set(name, parameter);
        }
        const body = new Pair(new LSymbol('begin'), code.cdr);
        return evaluate(body, { ...eval_args, env });
    }), `(syntax-parameterize (bindings) body)

         Macro work similar to let-syntax but the the bindnds will be exposed to the user.
         With syntax-parameterize you can define anaphoric macros.`),
    // ------------------------------------------------------------------
    define: doc(Macro.defmacro('define', function(code, eval_args) {
        var env = this;
        if (is_pair(code.car) &&
            code.car.car instanceof LSymbol) {
            var new_code = new Pair(
                new LSymbol("define"),
                new Pair(
                    code.car.car,
                    new Pair(
                        new Pair(
                            new LSymbol("lambda"),
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
        eval_args.dynamic_env = this;
        eval_args.env = env;
        var value = code.cdr.car;
        let new_expr;
        if (is_pair(value)) {
            value = evaluate(value, eval_args);
            new_expr = true;
        } else if (value instanceof LSymbol) {
            value = env.get(value);
        }
        typecheck('define', code.car, 'symbol');
        return unpromise(value, value => {
            if (env.__name__ === Syntax.__merge_env__) {
                env = env.__parent__;
            }
            if (new_expr &&
                ((is_function(value) && is_lambda(value)) ||
                 (value instanceof Syntax) || is_parameter(value))) {
                value.__name__ = code.car.valueOf();
                if (value.__name__ instanceof LString) {
                    value.__name__ = value.__name__.valueOf();
                }
            }
            let __doc__;
            if (is_pair(code.cdr.cdr) &&
                LString.isString(code.cdr.cdr.car)) {
                __doc__ = code.cdr.cdr.car.valueOf();
            }
            env.set(code.car, value, __doc__, true);
        });
    }), `(define name expression)
         (define name expression "doc string")
         (define (function-name . args) . body)

         Macro for defining values. It can be used to define variables,
         or functions. If the first argument is list it will create a function
         with name being first element of the list. This form expands to
         \`(define function-name (lambda args body))\``),
    // ------------------------------------------------------------------
    'set-obj!': doc('set-obj!', function(obj, key, value, options = null) {
        var obj_type = typeof obj;
        if (is_null(obj) || (obj_type !== 'object' && obj_type !== 'function')) {
            var msg = typeErrorMessage('set-obj!', type(obj), ['object', 'function']);
            throw new Error(msg);
        }
        typecheck('set-obj!', key, ['string', 'symbol', 'number']);
        obj = unbind(obj);
        key = key.valueOf();
        if (arguments.length === 2) {
            delete obj[key];
        } else if (is_prototype(obj) && is_function(value)) {
            obj[key] = unbind(value);
            obj[key][__prototype__] = true;
        } else if (is_function(value) || is_native(value) || is_nil(value)) {
            obj[key] = value;
        } else {
            obj[key] = value && !is_prototype(value) ? value.valueOf() : value;
        }
        if (props) {
            const value = obj[key];
            Object.defineProperty(obj, key, { ...options, value });
        }
    }, `(set-obj! obj key value)
        (set-obj! obj key value props)

        Function set a property of a JavaScript object. props should be a vector of pairs,
        passed to Object.defineProperty.`),
    // ------------------------------------------------------------------
    'null-environment': doc('null-environment', function() {
        return global_env.inherit('null');
    }, `(null-environment)

        Returns a clean environment with only the standard library.`),
    // ------------------------------------------------------------------
    'values': doc('values', function values(...args) {
        return Values(args);
    }, `(values a1 a2 ...)

        If called with more then one element it will create a special
        Values object that can be used in the call-with-values function.`),
    // ------------------------------------------------------------------
    'call-with-values': doc('call-with-values', function(producer, consumer) {
        typecheck('call-with-values', producer, 'function', 1);
        typecheck('call-with-values', consumer, 'function', 2);
        var maybe = producer.apply(this);
        if (maybe instanceof Values) {
            return consumer.apply(this, maybe.valueOf());
        }
        return consumer.call(this, maybe);
    }, `(call-with-values producer consumer)

        Calls the producer procedure with no arguments, then calls the
        consumer procedure with the returned value as an argument -- unless
        the returned value is a special Values object created by (values), if it is
        the values are unpacked and the consumer is called with multiple arguments.`),
    // ------------------------------------------------------------------
    'current-environment': doc('current-environment', function() {
        if (this.__name__ === '__frame__') {
            return this.__parent__;
        }
        return this;
    }, `(current-environment)

        Function that returns the current environment (they're first-class objects!)`),
    // ------------------------------------------------------------------
    'parent.frame': doc('parent.frame', function() {
        return user_env;
    }, `(parent.frame)

        Returns the parent environment if called from inside a function.
        If no parent frame can be found it returns nil.`),
    // ------------------------------------------------------------------
    'eval': doc('eval', function(code, env) {
        env = env || this.get('interaction-environment').call(this);
        return evaluate(code, {
            env,
            dynamic_env: env,
            error: e => {
                var error = global_env.get('display-error');
                error.call(this, e.message);
                if (e.code) {
                    var stack = e.code.map((line, i) => {
                        return `[${i + 1}]: ${line}`;
                    }).join('\n');
                    error.call(this, stack);
                }
            }
        });
    }, `(eval expr)
        (eval expr environment)

        Function that evaluates LIPS Scheme code. If the second argument is provided
        it will be the environment that the code is evaluated in.`),
    // ------------------------------------------------------------------
    lambda: new Macro('lambda', function(code, { use_dynamic, error } = {}) {
        var self = this;
        var __doc__;
        if (is_pair(code.cdr) &&
            LString.isString(code.cdr.car) &&
            !is_nil(code.cdr.cdr)) {
            __doc__ = code.cdr.car.valueOf();
        }
        function lambda(...args) {
            // lambda got scopes as context in apply
            let { dynamic_env } = is_context(this) ? this : { dynamic_env: self };
            const env = self.inherit('lambda');
            dynamic_env = dynamic_env.inherit('lambda');
            if (this && !is_context(this)) {
                if (this && !this.__instance__) {
                    Object.defineProperty(this, '__instance__', {
                        enumerable: false,
                        get: () => true,
                        set: () => {},
                        configurable: false
                    });
                }
                env.set('this', this);
            }
            // arguments and arguments.callee inside lambda function
            if (this instanceof LambdaContext) {
                const options = { throwError: false };
                env.set('arguments', this.env.get('arguments', options));
                env.set('parent.frame', this.env.get('parent.frame', options));
            } else {
                // this case is for lambda as callback function in JS; e.g. setTimeout
                var _args = args.slice();
                _args.callee = lambda;
                _args.env = env;
                env.set('arguments', _args);
            }
            function set(name, value) {
                env.__env__[name.__name__] = value;
                dynamic_env.__env__[name.__name__] = value;
            }
            let name = code.car;
            let i = 0;
            if (name instanceof LSymbol || !is_nil(name)) {
                while (true) {
                    if (!is_nil(name.car)) {
                        if (name instanceof LSymbol) {
                            // rest argument,  can also be first argument
                            const value = quote(Pair.fromArray(args.slice(i), false));
                            set(name, value);
                            break;
                        } else if (is_pair(name)) {
                            const value = args[i];
                            set(name.car, value);
                        }
                    }
                    if (is_nil(name.cdr)) {
                        break;
                    }
                    i++;
                    name = name.cdr;
                }
            }
            var rest = __doc__ ? code.cdr.cdr : code.cdr;
            var output = new Pair(new LSymbol('begin'), rest);
            const eval_args = {
                env,
                dynamic_env,
                use_dynamic,
                error
            }
            return evaluate(output, eval_args);
        }
        var length = is_pair(code.car) ? code.car.length() : null;
        lambda.__code__ = new Pair(new LSymbol('lambda'), code);
        lambda[__lambda__] = true;
        if (!is_pair(code.car)) {
            return doc(lambda, __doc__, true); // variable arguments
        }
        // wrap and decorate with __doc__
        return doc(set_fn_length(lambda, length), __doc__, true);
    }, `(lambda (a b) body)
        (lambda args body)
        (lambda (a b . rest) body)

        The lambda macro creates a new anonymous function. If the first element of
        the body is a string and there is more elements the string is used as the
        documentation string, that can be read using (help fn).`),
    // ------------------------------------------------------------------
    'macroexpand': doc(
        new Macro('macroexpand', macro_expand()),
        `(macroexpand expr)

         Macro that expand all macros inside and return single expression as output.`),
    // ------------------------------------------------------------------
    'macroexpand-1': doc(
        new Macro('macroexpand-1', macro_expand(true)),
        `(macroexpand-1 expr)

         Macro similar to macroexpand but it expand macros only one level
         and return single expression as output.`),
    // ------------------------------------------------------------------
    'define-macro': doc(new Macro(macro, function(macro, { use_dynamic, error }) {
        if (is_pair(macro.car) && macro.car.car instanceof LSymbol) {
            var name = macro.car.car.__name__;
            var __doc__;
            if (LString.isString(macro.cdr.car) && is_pair(macro.cdr.cdr)) {
                __doc__ = macro.cdr.car.valueOf();
            }
            var makro_instance = Macro.defmacro(name, function(code) {
                var env = new Environment({}, this, 'defmacro');
                var name = macro.car.cdr;
                var arg = code;
                while (true) {
                    if (is_nil(name)) {
                        break;
                    }
                    if (name instanceof LSymbol) {
                        env.__env__[name.__name__] = arg;
                        break;
                    } else if (!is_nil(name.car)) {
                        if (is_nil(arg)) {
                            env.__env__[name.car.__name__] = nil;
                        } else {
                            if (is_pair(arg.car)) {
                                arg.car[__data__] = true;
                            }
                            env.__env__[name.car.__name__] = arg.car;
                        }
                    }
                    if (is_nil(name.cdr)) {
                        break;
                    }
                    if (!is_nil(arg)) {
                        arg = arg.cdr;
                    }
                    name = name.cdr;
                }
                var eval_args = {
                    env,
                    dynamic_env: env,
                    use_dynamic,
                    error
                };
                // evaluate macro
                if (is_pair(macro.cdr)) {
                    // this eval will return lips code
                    var rest = __doc__ ? macro.cdr.cdr : macro.cdr;
                    var result = rest.reduce(function(result, node) {
                        return evaluate(node, eval_args);
                    });
                    return unpromise(result, function(result) {
                        if (typeof result === 'object') {
                            delete result[__data__];
                        }
                        return result;
                    });
                }
            }, __doc__, true);
            makro_instance.__code__ = new Pair(new LSymbol('define-macro'), macro);
            this.set(name, makro_instance);
        }
    }), `(define-macro (name . args) body)

         The meta-macro, that creates new macros. If the return value is a list structure
         it will be evaluated where the macro is invoked from. You can use quasiquote \`
         and unquote , and unquote-splicing ,@ inside to create an expression that will be
         evaluated at runtime. Macros works like this: if you pass any expression to a
         macro the arguments will not be evaluated unless the macro's body explicitly
         calls (eval) on it. Because of this a macro can manipulate the expression
         (arguments) as lists.`),
    // ------------------------------------------------------------------
    'syntax-rules': new Macro('syntax-rules', function(macro, options) {
        var { use_dynamic, error } = options;
        // TODO: find identifiers and freeze the scope when defined #172
        var env = this;
        function get_identifiers(node) {
            let symbols = [];
            while (!is_nil(node)) {
                const x = node.car;
                symbols.push(x.valueOf());
                node = node.cdr;
            }
            return symbols;
        }
        function validate_identifiers(node) {
            while (!is_nil(node)) {
                const x = node.car;
                if (!(x instanceof LSymbol)) {
                    throw new Error('syntax-rules: wrong identifier');
                }
                node = node.cdr;
            }
        }
        if (macro.car instanceof LSymbol) {
            validate_identifiers(macro.cdr.car);
        } else {
            validate_identifiers(macro.car);
        }
        const syntax = new Syntax(function(code, { macro_expand }) {
            log('>> SYNTAX');
            log(code);
            log(macro);
            const scope = env.inherit('syntax');
            const dynamic_env = scope;
            let var_scope = this;
            // for macros that define variables used in macro (2 levels nestting)
            if (var_scope.__name__ === Syntax.__merge_env__) {
                // copy refs for defined gynsyms
                const props = Object.getOwnPropertySymbols(var_scope.__env__);
                props.forEach(symbol => {
                    var_scope.__parent__.set(symbol, var_scope.__env__[symbol]);
                });
                var_scope = var_scope.__parent__;
            }
            var eval_args = { env: scope, dynamic_env, use_dynamic, error };
            let ellipsis, rules, symbols;
            if (macro.car instanceof LSymbol) {
                ellipsis = macro.car;
                symbols = get_identifiers(macro.cdr.car);
                rules = macro.cdr.cdr;
            } else {
                ellipsis = '...';
                symbols = get_identifiers(macro.car);
                rules = macro.cdr;
            }
            try {
                while (!is_nil(rules)) {
                    var rule = rules.car.car;
                    var expr = rules.car.cdr.car;
                    log('[[[ RULE');
                    log(rule);
                    var bindings = extract_patterns(rule, code, symbols, ellipsis, {
                        expansion: this, define: env
                    });
                    if (bindings) {
                        /* c8 ignore next 5 */
                        if (is_debug()) {
                            console.log(JSON.stringify(symbolize(bindings), true, 2));
                            console.log('PATTERN: ' + rule.toString(true));
                            console.log('MACRO: ' + code.toString(true));
                        }
                        // name is modified in transform_syntax
                        var names = [];
                        const new_expr = transform_syntax({
                            bindings,
                            expr,
                            symbols,
                            scope,
                            lex_scope: var_scope,
                            names,
                            ellipsis
                        });
                        log('OUPUT>>> ', new_expr);
                        // TODO: if expression is undefined throw an error
                        if (new_expr) {
                            expr = new_expr;
                        }
                        var new_env = var_scope.merge(scope, Syntax.__merge_env__);
                        if (macro_expand) {
                            return { expr, scope: new_env };
                        }
                        var result = evaluate(expr, { ...eval_args, env: new_env });
                        // Hack: update the result if there are generated
                        //       gensyms that should be literal symbols
                        // TODO: maybe not the part move when literal elisps may
                        //       be generated, maybe they will need to be mark somehow
                        return clear_gensyms(result, names);
                    }
                    rules = rules.cdr;
                }
            } catch (e) {
                e.message += `\nin macro:\n  ${macro.toString(true)}`;
                throw e;
            }
            throw new Error(`syntax-rules: no matching syntax in macro ${code.toString(true)}`);
        }, env);
        syntax.__code__ = macro;
        return syntax;
    }, `(syntax-rules () (pattern expression) ...)

        Base of hygienic macros, it will return a new syntax expander
        that works like Lisp macros.`),
    // ------------------------------------------------------------------
    quote: doc(new Macro('quote', function(arg) {
        return quote(arg.car);
    }), `(quote expression) or 'expression

         Macro that returns a single LIPS expression as data (it won't evaluate the
         argument). It will return a list if put in front of LIPS code.
         And if put in front of a symbol it will return the symbol itself, not the value
         bound to that name.`),
    'unquote-splicing': doc('unquote-splicing', function() {
        throw new Error(`You can't call \`unquote-splicing\` outside of quasiquote`);
    }, `(unquote-splicing code) or ,@code

        Special form used in the quasiquote macro. It evaluates the expression inside and
        splices the list into quasiquote's result. If it is not the last element of the
        expression, the computed value must be a pair.`),
    'unquote': doc('unquote', function() {
        throw new Error(`You can't call \`unquote\` outside of quasiquote`);
    }, `(unquote code) or ,code

        Special form used in the quasiquote macro. It evaluates the expression inside and
        substitutes the value into quasiquote's result.`),
    // ------------------------------------------------------------------
    quasiquote: Macro.defmacro('quasiquote', function(arg, env) {
        const { use_dynamic, error } = env;
        const self = this;
        //var max_unquote = 1;
        const dynamic_env = self;
        // -----------------------------------------------------------------
        function is_struct(value) {
            return is_pair(value) ||
                is_plain_object(value) ||
                Array.isArray(value);
        }
        // -----------------------------------------------------------------
        function resolve_pair(pair, fn, test = is_struct) {
            if (is_pair(pair)) {
                var car = pair.car;
                var cdr = pair.cdr;
                if (test(car)) {
                    car = fn(car);
                }
                if (test(cdr)) {
                    cdr = fn(cdr);
                }
                if (is_promise(car) || is_promise(cdr)) {
                    return promise_all([car, cdr]).then(([car, cdr]) => {
                        return new Pair(car, cdr);
                    });
                } else {
                    return new Pair(car, cdr);
                }
            }
            return pair;
        }
        // -----------------------------------------------------------------
        function join(eval_pair, value) {
            if (is_nil(eval_pair) && is_nil(value)) {
                //return nil;
            }
            if (is_pair(eval_pair)) {
                if (!is_nil(value)) {
                    eval_pair.append(value);
                }
            } else {
                eval_pair = new Pair(
                    eval_pair,
                    value
                );
            }
            return eval_pair;
        }
        // -----------------------------------------------------------------
        function unquoted_arr(arr) {
            return !!arr.filter(value => {
                return is_pair(value) &&
                    LSymbol.is(value.car, /^(unquote|unquote-splicing)$/);
            }).length;
        }
        // -----------------------------------------------------------------
        function quote_vector(arr, unquote_cnt, max_unq) {
            return arr.reduce((acc, x) => {
                if (!is_pair(x)) {
                    acc.push(x);
                    return acc;
                }
                if (LSymbol.is(x.car, 'unquote-splicing')) {
                    let result;
                    if (unquote_cnt + 1 < max_unq) {
                        result = recur(x.cdr, unquote_cnt + 1, max_unq);
                    } else {
                        result = evaluate(x.cdr.car, {
                            env: self,
                            use_dynamic,
                            dynamic_env,
                            error
                        });
                    }
                    if (!is_pair(result)) {
                        throw new Error(`Expecting list ${type(x)} found`);
                    }
                    return acc.concat(result.to_array());
                }
                acc.push(recur(x, unquote_cnt, max_unq));
                return acc;
            }, []);
        }
        // -----------------------------------------------------------------
        function quote_object(object, unquote_cnt, max_unq) {
            const result = {};
            unquote_cnt++;
            Object.keys(object).forEach(key => {
                const value = object[key];
                if (is_pair(value)) {
                    if (LSymbol.is(value.car, 'unquote-splicing')) {
                        throw new Error("You can't call `unquote-splicing` " +
                                        "inside object");
                    }
                    let output;
                    if (unquote_cnt < max_unq) {
                        output = recur(value.cdr.car, unquote_cnt, max_unq);
                    } else {
                        output = evaluate(value.cdr.car, {
                            env: self,
                            dynamic_env,
                            use_dynamic,
                            error
                        });
                    }
                    result[key] = output;
                } else {
                    result[key] = value;
                }
            });
            if (Object.isFrozen(object)) {
                Object.freeze(result);
            }
            return result;
        }
        // -----------------------------------------------------------------
        function unquote_splice(pair, unquote_cnt, max_unq) {
            if (unquote_cnt < max_unq) {
                return new Pair(
                    new Pair(
                        pair.car.car,
                        recur(pair.car.cdr, unquote_cnt, max_unq)
                    ),
                    nil
                );
            }
            var lists = [];
            return (function next(node) {
                var value = evaluate(node.car, {
                    env: self,
                    dynamic_env,
                    use_dynamic,
                    error
                });
                lists.push(value);
                if (is_pair(node.cdr)) {
                    return next(node.cdr);
                }
                return unpromise(lists, function(arr) {
                    if (arr.some(x => !is_pair(x))) {
                        if (is_pair(pair.cdr) &&
                            LSymbol.is(pair.cdr.car, '.') &&
                            is_pair(pair.cdr.cdr) &&
                            is_nil(pair.cdr.cdr.cdr)) {
                            return pair.cdr.cdr.car;
                        }
                        if (!(is_nil(pair.cdr) || is_pair(pair.cdr))) {
                            const msg = "You can't splice atom inside list";
                            throw new Error(msg);
                        }
                        if (arr.length > 1) {
                            const msg = "You can't splice multiple atoms inside list";
                            throw new Error(msg);
                        }
                        if (!(is_pair(pair.cdr) && is_nil(arr[0]))) {
                            return arr[0];
                        }
                    }
                    // don't create Cycles
                    arr = arr.map(eval_pair => {
                        if (splices.has(eval_pair)) {
                            return eval_pair.clone();
                        } else {
                            splices.add(eval_pair);
                            return eval_pair;
                        }
                    });
                    const value = recur(pair.cdr, 0, 1);
                    if (is_nil(value) && is_nil(arr[0])) {
                        return undefined;
                    }
                    return unpromise(value, value => {
                        if (is_nil(arr[0])) {
                            return value;
                        }
                        if (arr.length === 1) {
                            return join(arr[0], value);
                        }
                        var result = arr.reduce((result, eval_pair) => {
                            return join(result, eval_pair);
                        });
                        return join(result, value);
                    });
                });
            })(pair.car.cdr);
        }
        // -----------------------------------------------------------------
        var splices = new Set();
        function recur(pair, unquote_cnt, max_unq) {
            if (is_pair(pair)) {
                if (is_pair(pair.car)) {
                    if (LSymbol.is(pair.car.car, 'unquote-splicing')) {
                        return unquote_splice(pair, unquote_cnt + 1, max_unq);
                    }
                    if (LSymbol.is(pair.car.car, 'unquote')) {
                        // + 2 - one for unquote and one for unquote splicing
                        if (unquote_cnt + 2 === max_unq &&
                            is_pair(pair.car.cdr) &&
                            is_pair(pair.car.cdr.car) &&
                            LSymbol.is(pair.car.cdr.car.car, 'unquote-splicing')) {
                            const rest = pair.car.cdr;
                            return new Pair(
                                new Pair(
                                    new LSymbol('unquote'),
                                    unquote_splice(rest, unquote_cnt + 2, max_unq)
                                ),
                                nil
                            );
                        } else if (is_pair(pair.car.cdr) &&
                                   !is_nil(pair.car.cdr.cdr)) {
                            if (is_pair(pair.car.cdr.car)) {
                                // values inside unquote are lists
                                const result = [];
                                return (function recur(node) {
                                    if (is_nil(node)) {
                                        return Pair.fromArray(result);
                                    }
                                    return unpromise(evaluate(node.car, {
                                        env: self,
                                        dynamic_env,
                                        use_dynamic,
                                        error
                                    }), function(next) {
                                        result.push(next);
                                        return recur(node.cdr);
                                    });
                                })(pair.car.cdr);
                            } else {
                                // same as in guile if (unquote 1 2 3) it should be
                                // spliced - scheme spec say it's unspecify but it
                                // work like in CL
                                return pair.car.cdr;
                            }
                        }
                    }
                }
                if (LSymbol.is(pair.car, 'quasiquote')) {
                    var cdr = recur(pair.cdr, unquote_cnt, max_unq + 1);
                    return new Pair(pair.car, cdr);
                }
                if (LSymbol.is(pair.car, 'quote')) {
                    return new Pair(
                        pair.car,
                        recur(pair.cdr, unquote_cnt, max_unq)
                    );
                }
                if (LSymbol.is(pair.car, 'unquote')) {
                    unquote_cnt++;
                    if (unquote_cnt < max_unq) {
                        return new Pair(
                            new LSymbol('unquote'),
                            recur(pair.cdr, unquote_cnt, max_unq)
                        );
                    }
                    if (unquote_cnt > max_unq) {
                        throw new Error("You can't call `unquote` outside " +
                                        "of quasiquote");
                    }
                    if (is_pair(pair.cdr)) {
                        if (!is_nil(pair.cdr.cdr)) {
                            if (is_pair(pair.cdr.car)) {
                                // TODO: test if this part is needed
                                // this part was duplicated in previous section
                                // if (LSymbol.is(pair.car.car, 'unquote')) {
                                // so this probably can be removed
                                const result = [];
                                // evaluate all values in unquote
                                return (function recur(node) {
                                    if (is_nil(node)) {
                                        return Pair.fromArray(result);
                                    }
                                    return unpromise(evaluate(node.car, {
                                        env: self,
                                        dynamic_env,
                                        use_dynamic,
                                        error
                                    }), function(next) {
                                        result.push(next);
                                        return recur(node.cdr);
                                    });
                                })(pair.cdr);
                            } else {
                                return pair.cdr;
                            }
                        } else {
                            return evaluate(pair.cdr.car, {
                                env: self,
                                dynamic_env,
                                error
                            });
                        }
                    } else {
                        return pair.cdr;
                    }
                }
                return resolve_pair(pair, (pair) => {
                    return recur(pair, unquote_cnt, max_unq);
                });
            } else if (is_plain_object(pair)) {
                return quote_object(pair, unquote_cnt, max_unq);
            } else if (pair instanceof Array) {
                return quote_vector(pair, unquote_cnt, max_unq);
            }
            return pair;
        }
        // -----------------------------------------------------------------
        function clear(node) {
            if (is_pair(node)) {
                delete node[__data__];
                if (!node.have_cycles('car')) {
                    clear(node.car);
                }
                if (!node.have_cycles('cdr')) {
                    clear(node.cdr);
                }
            }
        }
        // -----------------------------------------------------------------
        if (is_plain_object(arg.car) && !unquoted_arr(Object.values(arg.car))) {
            return quote(arg.car);
        }
        if (Array.isArray(arg.car) && !unquoted_arr(arg.car)) {
            return quote(arg.car);
        }
        if (is_pair(arg.car) &&
            !arg.car.find('unquote') &&
            !arg.car.find('unquote-splicing') &&
            !arg.car.find('quasiquote')) {
            return quote(arg.car);
        }
        var x = recur(arg.car, 0, 1);
        return unpromise(x, value => {
            // clear nested data for tests
            clear(value);
            return quote(value);
        });
    }, `(quasiquote list)

        Similar macro to \`quote\` but inside it you can use special expressions (unquote
        x) abbreviated to ,x that will evaluate x and insert its value verbatim or
        (unquote-splicing x) abbreviated to ,@x that will evaluate x and splice the value
        into the result. Best used with macros but it can be used outside.`),
    // ------------------------------------------------------------------
    clone: doc('clone', function clone(list) {
        typecheck('clone', list, 'pair');
        return list.clone();
    }, `(clone list)

        Function that returns a clone of the list, that does not share any pairs with the
        original, so the clone can be safely mutated without affecting the original.`),
    // ------------------------------------------------------------------
    append: doc('append', function append(...items) {
        items = items.map(item => {
            if (is_pair(item)) {
                return item.clone();
            }
            return item;
        });
        return global_env.get('append!').call(this, ...items);
    }, `(append item ...)

        Function that creates a new list with each argument appended end-to-end.
        It will always return a new list and not modify its arguments.`),
    // ------------------------------------------------------------------
    'append!': doc('append!', function(...items) {
        var is_list = global_env.get('list?');
        return items.reduce((acc, item) => {
            typecheck('append!', acc, ['nil', 'pair']);
            if ((is_pair(item) || is_nil(item)) && !is_list(item)) {
                throw new Error('append!: Invalid argument, value is not a list');
            }
            if (is_nil(acc)) {
                if (is_nil(item)) {
                    return nil;
                }
                return item;
            }
            if (is_null(item)) {
                return acc;
            }
            return acc.append(item);
        }, nil);
    }, `(append! arg1 ...)

        Destructive version of append, it can modify the lists in place. It returns
        a new list where each argument is appended to the end. It may modify
        lists added as arguments.`),
    // ------------------------------------------------------------------
    reverse: doc('reverse', function reverse(arg) {
        typecheck('reverse', arg, ['array', 'pair', 'nil']);
        if (is_nil(arg)) {
            return nil;
        }
        if (is_pair(arg)) {
            var arr = global_env.get('list->array')(arg).reverse();
            return global_env.get('array->list')(arr);
        } else if (Array.isArray(arg)) {
            return arg.reverse();
        } else {
            throw new Error(typeErrorMessage('reverse', type(arg), 'array or pair'));
        }
    }, `(reverse list)

        Function that reverses the list or array. If value is not a list
        or array it will error.`),
    // ------------------------------------------------------------------
    nth: doc('nth', function nth(index, obj) {
        typecheck('nth', index, 'number');
        typecheck('nth', obj, ['array', 'pair']);
        if (is_pair(obj)) {
            var node = obj;
            var count = 0;
            while (count < index) {
                if (!node.cdr || is_nil(node.cdr) || node.have_cycles('cdr')) {
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

        Function that returns the nth element of the list or array.
        If used with a non-indexable value it will error.`),
    // ------------------------------------------------------------------
    list: doc('list', function list(...args) {
        return args.reverse().reduce((list, item) => new Pair(item, list), nil);
    }, `(list . args)

        Function that creates a new list out of its arguments.`),
    // ------------------------------------------------------------------
    substring: doc('substring', function substring(string, start, end) {
        typecheck('substring', string, 'string');
        typecheck('substring', start, 'number');
        typecheck('substring', end, ['number', 'void']);
        return string.substring(start.valueOf(), end && end.valueOf());
    }, `(substring string start end)

        Function that returns the slice of the string starting at start and ending
        with end.`),
    // ------------------------------------------------------------------
    concat: doc('concat', function concat(...args) {
        args.forEach((arg, i) => typecheck('concat', arg, 'string', i + 1));
        return args.join('');
    }, `(concat . strings)

        Function that creates a new string by joining its arguments.`),
    // ------------------------------------------------------------------
    join: doc('join', function join(separator, list) {
        typecheck('join', separator, 'string');
        typecheck('join', list, ['pair', 'nil']);
        return global_env.get('list->array')(list).join(separator);
    }, `(join separator list)

        Function that returns a string by joining elements of the list using separator.`),
    // ------------------------------------------------------------------
    split: doc('split', function split(separator, string) {
        typecheck('split', separator, ['regex', 'string']);
        typecheck('split', string, 'string');
        return global_env.get('array->list')(string.split(separator));
    }, `(split separator string)

        Function that creates a list by splitting string by separator which can
        be a string or regular expression.`),
    // ------------------------------------------------------------------
    replace: doc('replace', function replace(pattern, replacement, string) {
        typecheck('replace', pattern, ['regex', 'string']);
        typecheck('replace', replacement, ['string', 'function']);
        typecheck('replace', string, 'string');
        if (is_function(replacement)) {
            // ref: https://stackoverflow.com/a/48032528/387194
            const replacements = [];
            string.replace(pattern, function(...args) {
                replacements.push(replacement(...args));
            });
            return unpromise(replacements, replacements => {
                return string.replace(pattern, () => replacements.shift());
            });
        }
        return string.replace(pattern, replacement);
    }, `(replace pattern replacement string)

        Function that changes pattern to replacement inside string. Pattern can be a
        string or regex and replacement can be function or string. See Javascript
        String.replace().`),
    // ------------------------------------------------------------------
    match: doc('match', function match(pattern, string) {
        typecheck('match', pattern, ['regex', 'string']);
        typecheck('match', string, 'string');
        var m = string.match(pattern);
        return m ? global_env.get('array->list')(m) : false;
    }, `(match pattern string)

        Function that returns a match object from JavaScript as a list or #f if
        no match.`),
    // ------------------------------------------------------------------
    search: doc('search', function search(pattern, string) {
        typecheck('search', pattern, ['regex', 'string']);
        typecheck('search', string, 'string');
        return string.search(pattern);
    }, `(search pattern string)

        Function that returns the first found index of the pattern inside a string.`),
    // ------------------------------------------------------------------
    repr: doc('repr', function repr(obj, quote) {
        return toString(obj, quote);
    }, `(repr obj)

        Function that returns a LIPS code representation of the object as a string.`),
    // ------------------------------------------------------------------
    'escape-regex': doc('escape-regex', function(string) {
        typecheck('escape-regex', string, 'string');
        return escape_regex(string.valueOf());
    }, `(escape-regex string)

        Function that returns a new string where all special operators used in regex,
        are escaped with backslashes so they can be used in the RegExp constructor
        to match a literal string.`),
    // ------------------------------------------------------------------
    env: doc('env', function env(env) {
        env = env || this.env;
        const names = Object.keys(env.__env__).map(LSymbol);
        let result;
        if (names.length) {
            result = Pair.fromArray(names);
        } else {
            result = nil;
        }
        if (env.__parent__ instanceof Environment) {
            return global_env.get('env').call(this, env.__parent__).append(result);
        }
        return result;
    }, `(env)
        (env obj)

        Function that returns a list of names (functions, macros and variables)
        that are bound in the current environment or one of its parents.`),
    // ------------------------------------------------------------------
    'new': doc('new', function(obj, ...args) {
        var instance = new (unbind(obj))(...args.map(x => unbox(x)));
        return instance;
    }, `(new obj . args)

        Function that creates new JavaScript instance of an object.`),
    // ------------------------------------------------------------------
    'typecheck': doc(
        typecheck,
        `(typecheck label value type [position])

         Checks the type of value and errors if the type is not one allowed.  Type can be
         string or list of strings. The position optional argument is used to create a
         proper error message for the nth argument of function calls.`),
    // ------------------------------------------------------------------
    'typecheck-number': doc(
        typecheck_number,
        `(typecheck-number label value type [position])

         Function similar to typecheck but checks if the argument is a number
         and specific type of number e.g. complex.`),
    // ------------------------------------------------------------------
    'unset-special!': doc('unset-special!', function(symbol) {
        typecheck('remove-special!', symbol, 'string');
        delete specials.remove(symbol.valueOf());
    }, `(unset-special! name)

        Function that removes a special symbol from parser added by \`set-special!\`,
        name must be a string.`),
    // ------------------------------------------------------------------
    'set-special!': doc('set-special!', function(seq, name, type = specials.LITERAL) {
        typecheck('set-special!', seq, 'string', 1);
        typecheck('set-special!', name, 'symbol', 2);
        specials.append(seq.valueOf(), name, type);
    }, `(set-special! symbol name [type])

        Add a special symbol to the list of transforming operators by the parser.
        e.g.: \`(add-special! "#" 'x)\` will allow to use \`#(1 2 3)\` and it will be
        transformed into (x (1 2 3)) so you can write x macro that will process
        the list. 3rd argument is optional, and it can be one of two values:
        lips.specials.LITERAL, which is the default behavior, or
        lips.specials.SPLICE which causes the value to be unpacked into the expression.
        This can be used for e.g. to make \`#(1 2 3)\` into (x 1 2 3) that is needed
        by # that defines vectors.`),
    // ------------------------------------------------------------------
    'get': get,
    '.': get,
    // ------------------------------------------------------------------
    'unbind': doc(
        unbind,
        `(unbind fn)

         Function that removes the weak 'this' binding from a function so you
         can get properties from the actual function object.`),
    // ------------------------------------------------------------------
    type: doc(
        type,
        `(type object)

         Function that returns the type of an object as string.`),
    // ------------------------------------------------------------------
    'debugger': doc('debugger', function() {
        /* eslint-disable */
        debugger;
        /* eslint-enable */
    }, `(debugger)

        Function that triggers the JavaScript debugger (e.g. the browser devtools)
        using the "debugger;" statement. If a debugger is not running this
        function does nothing.`),
    // ------------------------------------------------------------------
    'in': doc('in', function(a, b) {
        if (a instanceof LSymbol ||
            a instanceof LString ||
            a instanceof LNumber) {
            a = a.valueOf();
        }
        return a in unbox(b);
    }, `(in key value)

        Function that uses the Javascript "in" operator to check if key is
        a valid property in the value.`),
    // ------------------------------------------------------------------
    'instance?': doc('instance?', function(obj) {
        return is_instance(obj);
    }, `(instance? obj)

        Checks if object is an instance, created with a new operator`),
    // ------------------------------------------------------------------
    'instanceof': doc('instanceof', function(type, obj) {
        return obj instanceof unbind(type);
    }, `(instanceof type obj)

        Predicate that tests if the obj is an instance of type.`),
    // ------------------------------------------------------------------
    'prototype?': doc(
        'prototype?',
        is_prototype,
        `(prototype? obj)

         Predicate that tests if value is a valid JavaScript prototype,
         i.e. calling (new) with it will not throw '<x> is not a constructor'.`),
    // ------------------------------------------------------------------
    'macro?': doc('macro?', function(obj) {
        return obj instanceof Macro;
    }, `(macro? expression)

        Predicate that tests if value is a macro.`),
    // ------------------------------------------------------------------
    'continuation?': doc(
        'continuation?',
        is_continuation,
        `(continuation? expression)

         Predicate that tests if value is a callable continuation.`),
    // ------------------------------------------------------------------
    'function?': doc(
        'function?',
        is_function,
        `(function? expression)

         Predicate that tests if value is a callable function.`),
    // ------------------------------------------------------------------
    'real?': doc('real?', function(value) {
        if (type(value) !== 'number') {
            return false;
        }
        if (value instanceof LNumber) {
            return value.isFloat();
        }
        return LNumber.isFloat(value);
    }, `(real? number)

        Predicate that tests if value is a real number (not complex).`),
    // ------------------------------------------------------------------
    'number?': doc('number?', function(x) {
        return Number.isNaN(x) || LNumber.isNumber(x);
    }, `(number? expression)

        Predicate that tests if value is a number or NaN value.`),
    // ------------------------------------------------------------------
    'string?': doc('string?', function(obj) {
        return LString.isString(obj);
    }, `(string? expression)

        Predicate that tests if value is a string.`),
    // ------------------------------------------------------------------
    'pair?': doc(
        'pair?',
        is_pair,
        `(pair? expression)

         Predicate that tests if value is a pair or list structure.`),
    // ------------------------------------------------------------------
    'regex?': doc('regex?', function(obj) {
        return obj instanceof RegExp;
    }, `(regex? expression)

        Predicate that tests if value is a regular expression.`),
    // ------------------------------------------------------------------
    'null?': doc('null?', function(obj) {
        return is_null(obj);
    }, `(null? expression)

        Predicate that tests if value is null-ish (i.e. undefined, nil, or
        Javascript null).`),
    // ------------------------------------------------------------------
    'boolean?': doc('boolean?', function(obj) {
        return typeof obj === 'boolean';
    }, `(boolean? expression)

        Predicate that tests if value is a boolean (#t or #f).`),
    // ------------------------------------------------------------------
    'symbol?': doc('symbol?', function(obj) {
        return obj instanceof LSymbol;
    }, `(symbol? expression)

        Predicate that tests if value is a LIPS symbol.`),
    // ------------------------------------------------------------------
    'array?': doc('array?', function(obj) {
        return obj instanceof Array;
    }, `(array? expression)

        Predicate that tests if value is an array.`),
    // ------------------------------------------------------------------
    'object?': doc('object?', function(obj) {
        return !is_nil(obj) && obj !== null &&
            !(obj instanceof LCharacter) &&
            !(obj instanceof RegExp) &&
            !(obj instanceof LString) &&
            !is_pair(obj) &&
            !(obj instanceof LNumber) &&
            typeof obj === 'object' &&
            !(obj instanceof Array);
    }, `(object? expression)

        Predicate that tests if value is an plain object (not another LIPS type).`),
    // ------------------------------------------------------------------
    flatten: doc('flatten', function flatten(list) {
        typecheck('flatten', list, 'pair');
        return list.flatten();
    }, `(flatten list)

        Returns a shallow list from tree structure (pairs).`),
    // ------------------------------------------------------------------
    'vector-append': doc('vector-append', function(...args) {
        if (!args.length) {
            return [];
        }
        typecheck_args('vector-append', args, 'array');
        const [first, ...rest] = args;
        return first.concat(...rest);
    }, `(vector-append v1 v2 ...)

         Returns new vector by combining it's arguments that should be vectors.`),
    // ------------------------------------------------------------------
    'array->list': doc('array->list', function(array) {
        typecheck('array->list', array, 'array');
        return Pair.fromArray(array);
    }, `(array->list array)

        Function that converts a JavaScript array to a LIPS cons list.`),
    // ------------------------------------------------------------------
    'tree->array': doc(
        'tree->array',
        to_array('tree->array', true),
        `(tree->array list)

         Function that converts a LIPS cons tree structure into a JavaScript array.`),
    // ------------------------------------------------------------------
    'list->array': doc(
        'list->array',
        to_array('list->array'),
        `(list->array list)

         Function that converts a LIPS list into a JavaScript array.`),
    // ------------------------------------------------------------------
    apply: doc('apply', function apply(fn, ...args) {
        typecheck('apply', fn, 'function', 1);
        var last = args.pop();
        typecheck('apply', last, ['pair', 'nil'], args.length + 2);
        args = args.concat(global_env.get('list->array').call(this, last));
        return fn.apply(this, prepare_fn_args(fn, args));
    }, `(apply fn list)

        Function that calls fn with the list of arguments.`),
    // ------------------------------------------------------------------
    length: doc('length', function length(obj) {
        if (!obj || is_nil(obj)) {
            return 0;
        }
        if (is_pair(obj)) {
            return obj.length();
        }
        if ("length" in obj) {
            return obj.length;
        }
    }, `(length expression)

        Function that returns the length of the object. The object can be a LIPS
        list or any object that has a "length" property. Returns undefined if the
        length could not be found.`),
    // ------------------------------------------------------------------
    'string->number': doc('string->number', function(arg, radix = 10) {
        typecheck('string->number', arg, 'string', 1);
        typecheck('string->number', radix, 'number', 2);
        arg = arg.valueOf();
        radix = radix.valueOf();
        if (arg.match(rational_bare_re) || arg.match(rational_re)) {
            return parse_rational(arg, radix);
        } else if (arg.match(complex_bare_re) || arg.match(complex_re)) {
            return parse_complex(arg, radix);
        } else {
            const valid_bare = (radix === 10 && !arg.match(/e/i)) || radix === 16;
            if (arg.match(int_bare_re) && valid_bare || arg.match(int_re)) {
                return parse_integer(arg, radix);
            }
            if (arg.match(float_re)) {
                return parse_float(arg);
            }
        }
        return false;
    }, `(string->number number [radix])

        Function that parses a string into a number.`),
    // ------------------------------------------------------------------
    'try': doc(new Macro('try', function(code, { use_dynamic, error }) {
        return new Promise((resolve, reject) => {
            let catch_clause, finally_clause, body_error;
            if (LSymbol.is(code.cdr.car.car, 'catch')) {
                catch_clause = code.cdr.car;
                if (is_pair(code.cdr.cdr) &&
                    LSymbol.is(code.cdr.cdr.car.car, 'finally')) {
                    finally_clause = code.cdr.cdr.car;
                }
            } else if (LSymbol.is(code.cdr.car.car, 'finally')) {
                finally_clause = code.cdr.car;
            }
            if (!(finally_clause || catch_clause)) {
                throw new Error('try: invalid syntax');
            }
            function finalize(result) {
                resolve(result);
                throw new IgnoreException('[CATCH]');
            }
            let next = (result, next) => {
                next(result);
            }
            if (finally_clause) {
                next = function(result, cont) {
                    // prevent infinite loop when finally throw exception
                    next = reject;
                    args.error = (e) => {
                        throw e;
                    };
                    unpromise(evaluate(new Pair(
                        new LSymbol('begin'),
                        finally_clause.cdr
                    ), args), function() {
                        cont(result);
                    });
                };
            }
            const args = {
                env: this,
                use_dynamic,
                dynamic_env: this,
                error: (e) => {
                    if (e instanceof IgnoreException) {
                        throw e;
                    }
                    body_error = true;
                    if (catch_clause) {
                        var env = this.inherit('try');
                        const name = catch_clause.cdr.car.car;
                        if (!(name instanceof LSymbol)) {
                            throw new Error('try: invalid syntax: catch require variable name');
                        }
                        env.set(name, e);
                        let catch_error;
                        var catch_args = {
                            env,
                            use_dynamic,
                            dynamic_env: this,
                            error: (e) => {
                                catch_error = true;
                                reject(e);
                                throw new IgnoreException('[CATCH]');
                            }
                        };
                        const value = evaluate(new Pair(
                            new LSymbol('begin'),
                            catch_clause.cdr.cdr
                        ), catch_args);
                        unpromise(value, function handler(result) {
                            if (!catch_error) {
                                next(result, finalize);
                            }
                        });
                    } else {
                        next(undefined, () => {
                            reject(e);
                        });
                    }
                }
            };
            const value = evaluate(code.car, args);
            unpromise(value, function(result) {
                next(result, resolve);
            }, args.error);
        });
    }), `(try expr (catch (e) code))
         (try expr (catch (e) code) (finally code))
         (try expr (finally code))

         Macro that executes expr and catches any exceptions thrown. If catch is provided
         it's executed when an error is thrown. If finally is provided it's always
         executed at the end.`),
    // ------------------------------------------------------------------
    'raise': doc('raise', function(obj) {
        throw obj;
    }, `(raise obj)

        Throws the object verbatim (no wrapping an a new Error).`),
    'throw': doc('throw', function(message) {
        throw new Error(message);
    }, `(throw string)

        Throws a new exception.`),
    // ------------------------------------------------------------------
    find: doc('find', function find(arg, list) {
        typecheck('find', arg, ['regex', 'function']);
        typecheck('find', list, ['pair', 'nil']);
        if (is_null(list)) {
            return nil;
        }
        var fn = matcher('find', arg);
        return unpromise(fn(list.car), function(value) {
            if (value && !is_nil(value)) {
                return list.car;
            }
            return find(arg, list.cdr);
        });
    }, `(find fn list)
        (find regex list)

        Higher-order function that finds the first value for which fn return true.
        If called with a regex it will create a matcher function.`),
    // ------------------------------------------------------------------
    'for-each': doc('for-each', function(fn, ...lists) {
        typecheck('for-each', fn, 'function');
        lists.forEach((arg, i) => {
            typecheck('for-each', arg, ['pair', 'nil'], i + 1);
        });
        // we need to use call(this because babel transpile this code into:
        // var ret = map.apply(void 0, [fn].concat(lists));
        // it don't work with weakBind
        var ret = global_env.get('map').call(this, fn, ...lists);
        if (is_promise(ret)) {
            return ret.then(() => {});
        }
    }, `(for-each fn . lists)

        Higher-order function that calls function \`fn\` on each
        value of the argument. If you provide more than one list
        it will take each value from each list and call \`fn\` function
        with that many arguments as number of list arguments.`),
    // ------------------------------------------------------------------
    map: doc('map', function map(fn, ...lists) {
        typecheck('map', fn, 'function');
        var is_list = global_env.get('list?');
        lists.forEach((arg, i) => {
            typecheck('map', arg, ['pair', 'nil'], i + 1);
            // detect cycles
            if (is_pair(arg) && !is_list.call(this, arg)) {
                throw new Error(`map: argument ${i + 1} is not a list`);
            }
        });
        if (lists.length === 0) {
            return nil;
        }
        if (lists.some(is_nil)) {
            return nil;
        }
        var args = lists.map(l => l.car);
        const { env, dynamic_env, use_dynamic } = this;
        const result = call_function(fn, args, { env, dynamic_env, use_dynamic });
        return unpromise(result, (head) => {
            return unpromise(map.call(this, fn, ...lists.map(l => l.cdr)), (rest) => {
                return new Pair(head, rest);
            });
        });
    }, `(map fn . lists)

        Higher-order function that calls function \`fn\` with each
        value of the list. If you provide more then one list as argument
        it will take each value from each list and call \`fn\` function
        with that many argument as number of list arguments. The return
        values of the fn calls are accumulated in a result list and
        returned by map.`),
    // ------------------------------------------------------------------
    'list?': doc('list?', function(obj) {
        var node = obj;
        while (true) {
            if (is_nil(node)) {
                return true;
            }
            if (!is_pair(node)) {
                return false;
            }
            if (node.have_cycles('cdr')) {
                return false;
            }
            node = node.cdr;
        }
    }, `(list? obj)

        Predicate that tests if value is a proper linked list structure.
        The car of each pair can be any value. It returns false on cyclic lists."`),
    // ------------------------------------------------------------------
    some: doc('some', function some(fn, list) {
        typecheck('some', fn, 'function');
        typecheck('some', list, ['pair', 'nil']);
        if (is_null(list)) {
            return false;
        } else {
            return unpromise(fn(list.car), (value) => {
                return value || some(fn, list.cdr);
            });
        }
    }, `(some fn list)

        Higher-order function that calls fn on each element of the list.
        It stops and returns true when fn returns true for a value.
        If none of the values give true, some will return false.
        Analogous to Python any(map(fn, list)).`),
    // ------------------------------------------------------------------
    fold: doc('fold', fold('fold', function(fold, fn, init, ...lists) {
        typecheck('fold', fn, 'function');
        lists.forEach((arg, i) => {
            typecheck('fold', arg, ['pair', 'nil'], i + 1);
        });
        if (lists.some(is_nil)) {
            return init;
        }
        const value = fold.call(this, fn, init, ...lists.map(l => l.cdr));
        return unpromise(value, value => {
            return fn(...lists.map(l => l.car), value);
        });
    }), `(fold fn init . lists)

         Function fold is left-to-right reversal of reduce. It call \`fn\`
         on each pair of elements of the list and returns a single value.
         e.g. it computes (fn 'a 'x (fn 'b 'y (fn 'c 'z 'foo)))
         for: (fold fn 'foo '(a b c) '(x y z))`),
    // ------------------------------------------------------------------
    pluck: doc('pluck', function pluck(...keys) {
        return function(obj) {
            keys = keys.map(x => x instanceof LSymbol ? x.__name__ : x);
            if (keys.length === 0) {
                return nil;
            } else if (keys.length === 1) {
                const [key] = keys;
                return obj[key];
            }
            var result = {};
            keys.forEach((key) => {
                result[key] = obj[key];
            });
            return result;
        };
    }, `(pluck . strings)

        If called with a single string it will return a function that when
        called with an object will return that key from the object.
        If called with more then one string the returned function will
        create a new object by copying all properties from the given object.`),
    // ------------------------------------------------------------------
    reduce: doc('reduce', fold('reduce', function(reduce, fn, init, ...lists) {
        typecheck('reduce', fn, 'function');
        lists.forEach((arg, i) => {
            typecheck('reduce', arg, ['pair', 'nil'], i + 1);
        });
        if (lists.some(is_nil)) {
            return init;
        }
        return unpromise(fn(...lists.map(l => l.car), init), (value) => {
            return reduce.call(this, fn, value, ...lists.map(l => l.cdr));
        });
    }), `(reduce fn init list . lists)

         Higher-order function that takes each element of the list and calls
         the fn with result of previous call or init and the next element
         of the list until each element is processed, and returns a single value
         as result of last call to \`fn\` function.
         e.g. it computes (fn 'c 'z (fn 'b 'y (fn 'a 'x 'foo)))
         for: (reduce fn 'foo '(a b c) '(x y z))`),
    // ------------------------------------------------------------------
    filter: doc('filter', function filter(arg, list) {
        typecheck('filter', arg, ['regex', 'function']);
        typecheck('filter', list, ['pair', 'nil']);
        var array = global_env.get('list->array')(list);
        var result = [];
        var fn = matcher('filter', arg);
        return (function loop(i) {
            function next(value) {
                if (value && !is_nil(value)) {
                    result.push(item);
                }
                return loop(++i);
            }
            if (i === array.length) {
                return Pair.fromArray(result);
            }
            var item = array[i];
            return unpromise(fn(item), next);
        })(0);
    }, `(filter fn list)
        (filter regex list)

        Higher-order function that calls \`fn\` for each element of the list
        and return a new list for only those elements for which fn returns
        a truthy value. If called with a regex it will create a matcher function.`),
    // ------------------------------------------------------------------
    compose: doc(
        compose,
        `(compose . fns)

         Higher-order function that creates a new function that applies all functions
         from right to left and returns the last value. Reverse of pipe.
         e.g.:
         ((compose (curry + 2) (curry * 3)) 10) ==> (+ 2 (* 3 10)) ==> 32`),
    pipe: doc(
        pipe,
        `(pipe . fns)

         Higher-order function that creates a new function that applies all functions
         from left to right and returns the last value. Reverse of compose.
         e.g.:
         ((pipe (curry + 2) (curry * 3)) 10) ==> (* 3 (+ 2 10)) ==> 36`),
    curry: doc(
        curry,
        `(curry fn . args)

         Higher-order function that creates a curried version of the function.
         The result function will have partially applied arguments and it
         will keep returning one-argument functions until all arguments are provided,
         then it calls the original function with the accumulated arguments.

         e.g.:
         (define (add a b c d) (+ a b c d))
         (define add1 (curry add 1))
         (define add12 (add 2))
         (display (add12 3 4))`),
    // ------------------------------------------------------------------
    // Numbers
    // ------------------------------------------------------------------
    gcd: doc('gcd', function gcd(...args) {
        typecheck_args('lcm', args, 'number');
        return args.reduce(function(result, item) {
            return result.gcd(item);
        });
    }, `(gcd n1 n2 ...)

        Function that returns the greatest common divisor of the arguments.`),
    // ------------------------------------------------------------------
    lcm: doc('lcm', function lcm(...args) {
        typecheck_args('lcm', args, 'number');
        // ref: https://rosettacode.org/wiki/Least_common_multiple#JavaScript
        var n = args.length, a = abs(args[0]);
        for (var i = 1; i < n; i++) {
            var b = abs(args[i]), c = a;
            while (a && b) {
                a > b ? a %= b : b %= a;
            }
            a = abs(c * args[i]) / (a + b);
        }
        return LNumber(a);
    }, `(lcm n1 n2 ...)

        Function that returns the least common multiple of the arguments.`),
    // ------------------------------------------------------------------
    'odd?': doc('odd?', single_math_op(function(num) {
        return LNumber(num).isOdd();
    }), `(odd? number)

         Checks if number is odd.`),
    // ------------------------------------------------------------------
    'even?': doc('even?', single_math_op(function(num) {
        return LNumber(num).isEven();
    }), `(even? number)

         Checks if number is even.`),
    // ------------------------------------------------------------------
    // math functions
    '*': doc('*', reduce_math_op(function(a, b) {
        return LNumber(a).mul(b);
    }, LNumber(1)), `(* . numbers)

        Multiplies all numbers passed as arguments. If single value is passed
        it will return that value.`),
    // ------------------------------------------------------------------
    '+': doc('+', reduce_math_op(function(a, b) {
        return LNumber(a).add(b);
    }, LNumber(0)), `(+ . numbers)

        Sums all numbers passed as arguments. If single value is passed it will
        return that value.`),
    // ------------------------------------------------------------------
    '-': doc('-', function(...args) {
        if (args.length === 0) {
            throw new Error('-: procedure require at least one argument');
        }
        typecheck_args('-', args, 'number');
        if (args.length === 1) {
            return LNumber(args[0]).sub();
        }
        if (args.length) {
            return args.reduce(binary_math_op(function(a, b) {
                return LNumber(a).sub(b);
            }));
        }
    }, `(- n1 n2 ...)
        (- n)

        Subtracts n2 and subsequent numbers from n1. If only one argument is passed
        it will negate the value.`),
    // ------------------------------------------------------------------
    '/': doc('/', function(...args) {
        if (args.length === 0) {
            throw new Error('/: procedure require at least one argument');
        }
        typecheck_args('/', args, 'number');
        if (args.length === 1) {
            return LNumber(1).div(args[0]);
        }
        return args.reduce(binary_math_op(function(a, b) {
            return LNumber(a).div(b);
        }));
    }, `(/ n1 n2 ...)
        (/ n)

        Divides n1 by n2 and subsequent arguments one by one. If single argument
        is passed it will calculate (/ 1 n).`),
    // ------------------------------------------------------------------
    abs: doc('abs', single_math_op(function(n) {
        return LNumber(n).abs();
    }), `(abs number)

         Function that returns the absolute value (magnitude) of number.`),
    // ------------------------------------------------------------------
    truncate: doc('truncate', function(n) {
        typecheck('truncate', n, 'number');
        if (LNumber.isFloat(n)) {
            if (n instanceof LNumber) {
                n = n.valueOf();
            }
            return LFloat(truncate(n));
        }
        return n;
    }, `(truncate n)

        Function that returns the integer part (floor) of a real number.`),
    // ------------------------------------------------------------------
    sqrt: doc('sqrt', single_math_op(function(n) {
        return LNumber(n).sqrt();
    }), `(sqrt number)

         Function that returns the square root of the number.`),
    // ------------------------------------------------------------------
    '**': doc('**', binary_math_op(function(a, b) {
        a = LNumber(a);
        b = LNumber(b);
        if (b.cmp(0) === -1 && LNumber.isInteger(b)) {
            return LRational({ num: 1, denom: a.pow(b.sub()) });
        }
        [a, b] = a.coerce(b);
        return a.pow(b);
    }), `(** a b)

         Function that calculates number a to to the power of b.`),
    // ------------------------------------------------------------------
    '1+': doc('1+', single_math_op(function(number) {
        return LNumber(number).add(1);
    }), `(1+ number)

         Function that adds 1 to the number and return result.`),
    // ------------------------------------------------------------------
    '1-': doc(single_math_op(function(number) {
        return LNumber(number).sub(1);
    }), `(1- number)

         Function that subtracts 1 from the number and return result.`),
    // ------------------------------------------------------------------
    '%': doc('%', function(a, b) {
        typecheck_args('%', [a, b], 'number');
        return LNumber(a).rem(b);
    }, `(% n1 n2)

        Function returns the remainder of n1/n2 (modulo).`),
    // ------------------------------------------------------------------
    // Booleans
    '==': doc('==', function(...args) {
        typecheck_args('==', args, 'number');
        return seq_compare((a, b) => LNumber(a).cmp(b) === 0, args);
    }, `(== x1 x2 ...)

        Function that compares its numerical arguments and checks if they are
        all equal.`),
    // ------------------------------------------------------------------
    '>': doc('>', function(...args) {
        typecheck_numbers('>', args, ['bigint', 'float', 'rational']);
        return seq_compare((a, b) => LNumber(a).cmp(b) === 1, args);
    }, `(> x1 x2 x3 ...)

        Function that compares its numerical arguments and checks if they are
        monotonically decreasing, i.e. x1 > x2 and x2 > x3 and so on.`),
    // ------------------------------------------------------------------
    '<': doc('<', function(...args) {
        typecheck_numbers('<', args, ['bigint', 'float', 'rational']);
        return seq_compare((a, b) => LNumber(a).cmp(b) === -1, args);
    }, `(< x1 x2 ...)

        Function that compares its numerical arguments and checks if they are
        monotonically increasing, i.e. x1 < x2 and x2 < x3 and so on.`),
    // ------------------------------------------------------------------
    '<=': doc('<=', function(...args) {
        typecheck_numbers('<=', args, ['bigint', 'float', 'rational']);
        return seq_compare((a, b) => [0, -1].includes(LNumber(a).cmp(b)), args);
    }, `(<= x1 x2 ...)

        Function that compares its numerical arguments and checks if they are
        monotonically nondecreasing, i.e. x1 <= x2 and x2 <= x3 and so on.`),
    // ------------------------------------------------------------------
    '>=': doc('>=', function(...args) {
        typecheck_numbers('>=', args, ['bigint', 'float', 'rational']);
        return seq_compare((a, b) => [0, 1].includes(LNumber(a).cmp(b)), args);
    }, `(>= x1 x2 ...)

        Function that compares its numerical arguments and checks if they are
        monotonically nonincreasing, i.e. x1 >= x2 and x2 >= x3 and so on.`),
    // ------------------------------------------------------------------
    'eq?': doc(
        'eq?',
        equal,
        `(eq? a b)

         Function that compares two values if they are identical.`),
    // ------------------------------------------------------------------
    or: doc(new Macro('or', function(code, { use_dynamic, error }) {
        var args = global_env.get('list->array')(code);
        var self = this;
        const dynamic_env = self;
        if (!args.length) {
            return false;
        }
        var result;
        return (function loop() {
            function next(value) {
                result = value;
                if (!is_false(result)) {
                    return result;
                } else {
                    return loop();
                }
            }
            if (!args.length) {
                if (!is_false(result)) {
                    return result;
                } else {
                    return false;
                }
            } else {
                var arg = args.shift();
                var value = evaluate(arg, { env: self, dynamic_env, use_dynamic, error });
                return unpromise(value, next);
            }
        })();
    }), `(or . expressions)

         Macro that executes the values one by one and returns the first that is
         a truthy value. If there are no expressions that evaluate to true it
         returns false.`),
    // ------------------------------------------------------------------
    and: doc(new Macro('and', function(code, { use_dynamic, error } = {}) {
        const args = global_env.get('list->array')(code);
        const self = this;
        const dynamic_env = self;
        if (!args.length) {
            return true;
        }
        let result;
        const eval_args = { env: self, dynamic_env, use_dynamic, error };
        return (function loop() {
            function next(value) {
                result = value;
                if (is_false(result)) {
                    return result;
                } else {
                    return loop();
                }
            }
            if (!args.length) {
                if (!is_false(result)) {
                    return result;
                } else {
                    return false;
                }
            } else {
                const arg = args.shift();
                return unpromise(evaluate(arg, eval_args), next);
            }
        })();
    }), `(and . expressions)

         Macro that evaluates each expression in sequence and if any value returns false
         it will stop and return false. If each value returns true it will return the
         last value. If it's called without arguments it will return true.`),
    // bit operations
    '|': doc('|', function(a, b) {
        return LNumber(a).or(b);
    }, `(| a b)

        Function that calculates the bitwise or operation.`),
    '&': doc('&', function(a, b) {
        return LNumber(a).and(b);
    }, `(& a b)

        Function that calculates the bitwise and operation.`),
    '~': doc('~', function(a) {
        return LNumber(a).neg();
    }, `(~ number)

        Function that calculates the bitwise inverse (flip all the bits).`),
    '>>': doc('>>', function(a, b) {
        return LNumber(a).shr(b);
    }, `(>> a b)

        Function that right shifts the value a by value b bits.`),
    '<<': doc('<<', function(a, b) {
        return LNumber(a).shl(b);
    }, `(<< a b)

        Function that left shifts the value a by value b bits.`),
    not: doc('not', function not(value) {
        return !value;
    }, `(not object)

        Function that returns the Boolean negation of its argument.`)
}, undefined, 'global');
var user_env = global_env.inherit('user-env');
// -------------------------------------------------------------------------
function set_interaction_env(interaction, internal) {
    interaction.constant('**internal-env**', internal);
    interaction.doc(
        '**internal-env**',
        `**internal-env**

         Constant used to hide stdin, stdout and stderr so they don't interfere
         with variables with the same name. Constants are an internal type
         of variable that can't be redefined, defining a variable with the same name
         will throw an error.`
    );
    global_env.set('**interaction-environment**', interaction);
}
// -------------------------------------------------------------------------
set_interaction_env(user_env, internal_env);
global_env.doc(
    '**interaction-environment**',
    `**interaction-environment**

    Internal dynamic, global variable used to find interpreter environment.
    It's used so the read and write functions can locate **internal-env**
    that contains the references to stdin, stdout and stderr.`
);
function set_fs(fs) {
    user_env.get('**internal-env**').set('fs', fs);
}

// -------------------------------------------------------------------------
(function() {
    var map = { ceil: 'ceiling' };
    ['floor', 'round', 'ceil'].forEach(fn => {
        var name = map[fn] ? map[fn] : fn;
        global_env.set(name, doc(name, function(value) {
            typecheck(name, value, 'number');
            if (value instanceof LNumber) {
                return value[fn]();
            }
        }, `(${name} number)

            Function that calculates the ${name} of a number.`));
    });
})();
// -------------------------------------------------------------------------
// ref: https://stackoverflow.com/a/4331218/387194
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

// -------------------------------------------------------------------------
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

// -------------------------------------------------------------------------
// cadr caddr cadadr etc.
combinations(['d', 'a'], 2, 5).forEach(spec => {
    const s = spec.split('');
    const chars = s.slice().reverse();
    const code = s.map(c => `(c${c}r`).join(' ') + ' arg' + ')'.repeat(s.length);
    const name = 'c' + spec + 'r';
    global_env.set(name, doc(name, function(arg) {
        return chars.reduce(function(list, type) {
            typecheck(name, list, 'pair');
            if (type === 'a') {
                return list.car;
            } else {
                return list.cdr;
            }
        }, arg);
    }, `(${name} arg)

        Function that calculates ${code}`));
});
// -----------------------------------------------------------------------------
function reversseFind(dir, fn) {
    var parts = dir.split(path.sep).filter(Boolean);
    for (var i = parts.length; i--;) {
        var p = path.join('/', ...parts.slice(0, i + 1));
        if (fn(p)) {
            return p;
        }
    }
}

// -----------------------------------------------------------------------------
function nodeModuleFind(dir) {
    return reversseFind(dir, function(dir) {
        return fs.existsSync(path.join(dir, 'node_modules'));
    });
}

// -------------------------------------------------------------------------
function is_node() {
    return typeof global !== 'undefined' && global.global === global;
}
// -------------------------------------------------------------------------
const noop = () => {};
// -------------------------------------------------------------------------
async function node_specific() {
    const { createRequire } = await import('mod' + 'ule');
    nodeRequire = createRequire(import.meta.url);
    fs = await import('fs');
    path = await import('path');
    global_env.set('global', global);
    global_env.set('self', global);
    global_env.set('window', undefined);
    const moduleURL = new URL(import.meta.url);
    // using name __direname and __filename breaks after transpilation
    const __dirname__ = path.dirname(moduleURL.pathname);
    const __filename__ = path.basename(moduleURL.pathname);
    global_env.set('__dirname', __dirname__);
    global_env.set('__filename', __filename__);
    // ---------------------------------------------------------------------
    global_env.set('require.resolve', doc('require.resolve', function(path) {
        typecheck('require.resolve', path, 'string');
        var name = path.valueOf();
        return nodeRequire.resolve(name);
    }, `(require.resolve path)

        Returns the path relative to the current module.

        Only available when LIPS is running under Node.js.`));
    // ---------------------------------------------------------------------
    global_env.set('require', doc('require', function(module) {
        typecheck('require', module, 'string');
        module = module.valueOf();
        var root = process.cwd();
        var value;
        try {
            if (module.match(/^\s*\./)) {
                value = nodeRequire(path.join(root, module));
            } else {
                var dir = nodeModuleFind(root);
                if (dir) {
                    value = nodeRequire(path.join(dir, 'node_modules', module));
                } else {
                    value = nodeRequire(module);
                }
            }
        } catch (e) {
            value = nodeRequire(module);
        }
        return patch_value(value, global);
    }, `(require module)

        Function used inside Node.js to import a module.`));

    // ignore exceptions that are caught elsewhere. This is needed to fix AVA
    // reporting unhandled rejections for try..catch
    // see: https://github.com/avajs/ava/discussions/3289
    process.on('unhandledRejection', (reason, promise) => {
        if (reason instanceof IgnoreException) {
            promise.catch(noop);
        }
    });
}
// -------------------------------------------------------------------------
/* c8 ignore next 15 */
let node_ready; // Scheme load function need to wait for node_specific
if (is_node()) {
    node_ready = node_specific();
} else {
    node_ready = Promise.resolve();
    if (typeof window !== 'undefined' && window === root) {
        global_env.set('window', window);
        global_env.set('global', undefined);
        global_env.set('self', window);
    } else if (typeof self !== 'undefined' && typeof WorkerGlobalScope !== 'undefined') {
        global_env.set('self', self);
        global_env.set('window', undefined);
        global_env.set('global', undefined);
    }
}
// -------------------------------------------------------------------------
function typeErrorMessage(fn, got, expected, position = null) {
    let postfix = fn ? ` in expression \`${fn}\`` : '';
    if (position !== null) {
        postfix += ` (argument ${position})`;
    }
    if (is_function(expected)) {
        return `Invalid type: got ${got}${postfix}`;
    }
    if (expected instanceof Array) {
        if (expected.length === 1) {
            const first = expected[0].toLowerCase();
            expected = 'a' + ('aeiou'.includes(first) ? 'n ' : ' ') + expected[0];
        } else {
            expected = new Intl.ListFormat('en', {
                style: 'long', type: 'disjunction'
            }).format(expected);
        }
    }
    return `Expecting ${expected} got ${got}${postfix}`;
}

// -------------------------------------------------------------------------
function typecheck_number(fn, arg, expected, position = null) {
    typecheck(fn, arg, 'number', position);
    const arg_type = arg.__type__;
    let match;
    if (is_pair(expected)) {
        expected = expected.to_array();
    }
    if (expected instanceof Array) {
        expected = expected.map(x => x.valueOf());
    }
    if (expected instanceof Array) {
        expected = expected.map(x => x.valueOf().toLowerCase());
        if (expected.includes(arg_type)) {
            match = true;
        }
    } else {
        expected = expected.valueOf().toLowerCase();
    }
    if (!match && arg_type !== expected) {
        throw new Error(typeErrorMessage(fn, arg_type, expected, position));
    }
}

// -------------------------------------------------------------------------
function typecheck_numbers(fn, args, expected) {
    args.forEach((arg, i) => {
        typecheck_number(fn, arg, expected, i + 1);
    });
}

// -------------------------------------------------------------------------
function typecheck_args(fn, args, expected) {
    args.forEach((arg, i) => {
        typecheck(fn, arg, expected, i + 1);
    });
}
// -------------------------------------------------------------------------
function typecheck_text_port(fn, arg, type) {
    typecheck(fn, arg, type);
    if (arg.__type__ === binary_port) {
        throw new Error(typeErrorMessage(
            fn,
            'binary-port',
            'textual-port'
        ));
    }
}
// -------------------------------------------------------------------------
function typecheck(fn, arg, expected, position = null) {
    fn = fn.valueOf();
    const arg_type = type(arg).toLowerCase();
    if (is_function(expected)) {
        if (!expected(arg)) {
            throw new Error(typeErrorMessage(fn, arg_type, expected, position));
        }
        return;
    }
    var match = false;
    if (is_pair(expected)) {
        expected = expected.to_array();
    }
    if (expected instanceof Array) {
        expected = expected.map(x => x.valueOf());
    }
    if (expected instanceof Array) {
        expected = expected.map(x => x.valueOf().toLowerCase());
        if (expected.includes(arg_type)) {
            match = true;
        }
    } else {
        expected = expected.valueOf().toLowerCase();
    }
    if (!match && arg_type !== expected) {
        throw new Error(typeErrorMessage(fn, arg_type, expected, position));
    }
}

// -------------------------------------------------------------------------
function memoize(fn) {
    var memo = new WeakMap();
    return function(arg) {
        var result = memo.get(arg);
        if (!result) {
            result = fn(arg);
        }
        return result;
    };
}
// -------------------------------------------------------------------------
/* eslint-disable no-func-assign */
type = memoize(type);
/* eslint-enable no-func-assign */
// -------------------------------------------------------------------------
function type(obj) {
    let t = type_constants.get(obj);
    if (t) {
        return t;
    }
    if (typeof obj === 'object') {
        for (let [key, value] of Object.entries(type_mapping)) {
            if (obj instanceof value) {
                return key;
            }
        }
        if (is_instance(obj)) {
            if (is_function(obj.typeOf)) {
                return obj.typeOf();
            }
            return 'instance';
        }
        if (obj.constructor) {
            if (obj.constructor.__class__) {
                return obj.constructor.__class__;
            }
            if (obj.constructor === Object) {
                if (is_iterator(obj, Symbol.iterator)) {
                    return 'iterator';
                }
                if (is_iterator(obj, Symbol.asyncIterator)) {
                    return 'async-iterator';
                }
            }
            if (obj.constructor.name === '') {
                return 'object';
            }
            return obj.constructor.name.toLowerCase();
        }
    }
    if (obj === undefined) {
        return 'void';
    }
    return typeof obj;
}
// -------------------------------------------------------------------------
// :; wrap tree of Promises with single Promise or return argument as is
// :: if tree have no Promises
// -------------------------------------------------------------------------
function resolve_promises(arg) {
    var promises = [];
    traverse(arg);
    if (promises.length) {
        return resolve(arg);
    }
    return arg;
    function traverse(node) {
        if (is_promise(node)) {
            promises.push(node);
        } else if (is_pair(node)) {
            if (!node.have_cycles('car')) {
                traverse(node.car);
            }
            if (!node.have_cycles('cdr')) {
                traverse(node.cdr);
            }
        } else if (node instanceof Array) {
            node.forEach(traverse);
        }
    }
    async function promise(node) {
        var pair = new Pair(
            node.have_cycles('car') ? node.car : await resolve(node.car),
            node.have_cycles('cdr') ? node.cdr : await resolve(node.cdr)
        );
        if (node[__data__]) {
            pair[__data__] = true;
        }
        return pair;
    }
    function resolve(node) {
        if (node instanceof Array) {
            return promise_all(node.map(resolve));
        }
        if (is_pair(node) && promises.length) {
            return promise(node);
        }
        return node;
    }
}
// -------------------------------------------------------------------------
function evaluate_args(rest, { use_dynamic, ...options }) {
    var args = [];
    var node = rest;
    function next() {
        return args;
    }
    return (function loop() {
        if (is_pair(node)) {
            let arg = evaluate(node.car, { use_dynamic, ...options });
            if (use_dynamic) {
                // NOTE: why native function need bind to env?
                arg = unpromise(arg, arg => {
                    if (is_native_function(arg)) {
                        return arg.bind(dynamic_env);
                    }
                    return arg;
                });
            }
            return unpromise(resolve_promises(arg), function(arg) {
                args.push(arg);
                if (node.have_cycles('cdr')) {
                    throw new Error(`Invalid expression: Can't evaluate cycle`);
                }
                node = node.cdr;
                return loop();
            });
        } else if (is_nil(node)) {
            return next();
        } else {
            throw new Error('Syntax Error: improper list found in apply');
        }
    })();
}
// -------------------------------------------------------------------------
function evaluate_syntax(macro, code, eval_args) {
    var value = macro.invoke(code, eval_args);
    return unpromise(resolve_promises(value), function(value) {
        if (is_pair(value)) {
            value.mark_cycles();
        }
        return quote(value);
    });
}
// -------------------------------------------------------------------------
function evaluate_macro(macro, code, eval_args) {
    function finalize(result) {
        if (is_pair(result)) {
            result.mark_cycles();
            return result;
        }
        return quote(result);
    }
    const value = macro.invoke(code, eval_args);
    return unpromise(resolve_promises(value), function ret(value) {
        if (!value || value && value[__data__] || self_evaluated(value)) {
            return value;
        } else {
            return unpromise(evaluate(value, eval_args), finalize);
        }
    }, error => {
        throw error;
    });
}

// -------------------------------------------------------------------------
function prepare_fn_args(fn, args) {
    if (is_bound(fn) && !is_object_bound(fn) &&
        (!lips_context(fn) || is_port_method(fn))) {
        args = args.map(unbox);
    }
    if (!is_raw_lambda(fn) &&
        args.some(is_lips_function) &&
        !is_lips_function(fn) &&
        !is_array_method(fn)) {
        // we unbox values from callback functions #76
        // calling map on array should not unbox the value
        var result = [], i = args.length;
        while (i--) {
            let arg = args[i];
            if (is_lips_function(arg)) {
                var wrapper = function(...args) {
                    return unpromise(arg.apply(this, args), unbox);
                };
                // make wrapper work like output of bind
                hidden_prop(wrapper, '__bound__', true);
                hidden_prop(wrapper, '__fn__', arg);
                // copy prototype from function to wrapper
                // so this work when calling new from JavaScript
                // case of Preact that pass LIPS class as argument
                // to h function
                wrapper.prototype = arg.prototype;
                result[i] = wrapper;
            } else {
                result[i] = arg;
            }
        }
        args = result;
    }
    return args;
}

// -------------------------------------------------------------------------
function call_function(fn, args, { env, dynamic_env, use_dynamic } = {}) {
    const scope = env?.new_frame(fn, args);
    const dynamic_scope = dynamic_env?.new_frame(fn, args);
    const context = new LambdaContext({
        env: scope,
        use_dynamic,
        dynamic_env: dynamic_scope
    });
    return resolve_promises(fn.apply(context, args));
}

// -------------------------------------------------------------------------
function apply(fn, args, { env, dynamic_env, use_dynamic, error = () => {} } = {}) {
    args = evaluate_args(args, { env, dynamic_env, error, use_dynamic });
    return unpromise(args, function(args) {
        if (is_raw_lambda(fn)) {
            // lambda need environment as context
            // normal functions are bound to their contexts
            fn = unbind(fn);
        }
        args = prepare_fn_args(fn, args);
        const _args = args.slice();
        const result = call_function(fn, _args, { env, dynamic_env, use_dynamic });
        return unpromise(result, (result) => {
            if (is_pair(result)) {
                result.mark_cycles();
                return quote(result);
            }
            return box(result);
        }, error);
    });
}
// -------------------------------------------------------------------------
// :: Parameters for make-parameter and parametrize
// -------------------------------------------------------------------------
class Parameter {
    __value__;
    __fn__;
    #__p_name__;
    constructor(init, fn = null, name = null) {
        this.__value__ = init;
        if (fn) {
            if (!is_function(fn)) {
                throw new Error('Section argument to Parameter need to be function ' +
                                `${type(fn)} given`);
            }
            this.__fn__ = fn;
        }
        if (name) {
            this.#__p_name__ = name;
        }
    }
    get __name__() {
        return this.#__p_name__;
    }
    set __name__(name) {
        this.#__p_name__ = name;
        if (this.__fn__) {
            this.__fn__.__name__ = `fn-${name}`;
        }
    }
    invoke() {
        if (is_function(this.__fn__)) {
            return this.__fn__(this.__value__);
        }
        return this.__value__;
    }
    inherit(value) {
        return new Parameter(value, this.__fn__, this.__name__);
    }
}
// -------------------------------------------------------------------------
class LambdaContext {
    env;
    dynamic_env;
    use_dynamic;
    constructor(payload) {
        Object.assign(this, payload);
    }
    get __name__() {
        return this.env.__name__;
    }
    get __parent__() {
        return this.env.__parent__;
    }
    get(...args) {
        return this.env.get(...args);
    }
}
// -------------------------------------------------------------------------
function search_param(env, param) {
    let candidate = env.get(param.__name__, { throwError: false });
    if (is_parameter(candidate) && candidate !== param) {
        return candidate;
    }
    let is_first_env = true;
    const top_env = user_env.get('**interaction-environment**');
    while (true) {
        const parent = env.get('parent.frame', { throwError: false });
        env = parent(0);
        if (env === top_env) {
            break;
        }
        is_first_env = false;
        candidate = env.get(param.__name__, { throwError: false });
        if (is_parameter(candidate) && candidate !== param) {
            return candidate;
        }
    }
    return param;
}

// -------------------------------------------------------------------------
// :: Continuations object from call/cc
// -------------------------------------------------------------------------
class Continuation {
    __value__;
    constructor(k) {
        this.__value__ = k;
    }
    invoke() {
        if (this.__value__ === null) {
            throw new Error('Continuations are not implemented yet');
        }
    }
}

// -------------------------------------------------------------------------
function evaluate(code, { env, dynamic_env, use_dynamic, error = noop, ...rest } = {}) {
    try {
        if (!is_env(dynamic_env)) {
            dynamic_env = env === true ? user_env : (env || user_env);
        }
        if (use_dynamic) {
            env = dynamic_env;
        } else if (env === true) {
            env = user_env;
        } else {
            env = env || global_env;
        }
        var eval_args = { env, dynamic_env, use_dynamic, error };
        var value;
        if (is_null(code)) {
            return code;
        }
        if (code instanceof LSymbol) {
            return env.get(code);
        }
        if (!is_pair(code)) {
            return code;
        }
        var first = code.car;
        var rest = code.cdr;
        if (is_pair(first)) {
            value = resolve_promises(evaluate(first, eval_args));
            if (is_promise(value)) {
                return value.then((value) => {
                    if (!is_callable(value)) {
                        throw new Error(
                            type(value) + ' ' + env.get('repr')(value) +
                                ' is not callable while evaluating ' + code.toString()
                        );
                    }
                    return evaluate(new Pair(value, code.cdr), eval_args);
                });
                // else is later in code
            } else if (!is_callable(value)) {
                throw new Error(
                    type(value) + ' ' + env.get('repr')(value) +
                        ' is not callable while evaluating ' + code.toString()
                );
            }
        }
        if (first instanceof LSymbol) {
            value = env.get(first);
        } else if (is_function(first)) {
            value = first;
        }
        let result;
        if (value instanceof Syntax) {
            result = evaluate_syntax(value, code, eval_args);
        } else if (value instanceof Macro) {
            result = evaluate_macro(value, rest, eval_args);
        } else if (is_function(value)) {
            result = apply(value, rest, eval_args);
        } else if (value instanceof SyntaxParameter) {
            result = evaluate_syntax(value._syntax, code, eval_args);
        } else if (is_parameter(value)) {
            const param = search_param(dynamic_env, value);
            if (is_null(code.cdr)) {
                result = param.invoke();
            } else {
                return unpromise(evaluate(code.cdr.car, eval_args), function(value) {
                    param.__value__ = value;
                });
            }
        } else if (is_continuation(value)) {
            result = value.invoke();
        } else if (is_pair(code)) {
            value = first && first.toString();
            throw new Error(`${type(first)} ${value} is not a function`);
        } else {
            return code;
        }
        // escape promise feature #54
        var __promise__ = env.get(Symbol.for('__promise__'), { throwError: false });
        if (__promise__ === true && is_promise(result)) {
            // fix #139 evaluate the code inside the promise that is not data.
            // When promise is not quoted it happen automatically, when returning
            // promise from evaluate.
            result = result.then(result => {
                if (is_pair(result) && !value[__data__]) {
                    return evaluate(result, eval_args);
                }
                return result;
            });
            return new QuotedPromise(result);
        }
        return result;
    } catch (e) {
        error && error.call(env, e, code);
    }
}
// -------------------------------------------------------------------------
const compile = exec_collect(function(code) {
    return code;
});
// -------------------------------------------------------------------------
const exec = exec_collect(function(code, value) {
    return value;
});
// -------------------------------------------------------------------------
function exec_with_stacktrace(code, { env, dynamic_env, use_dynamic } = {}) {
    return evaluate(code, {
        env,
        dynamic_env,
        use_dynamic,
        error: (e, code) => {
            if (e && e.message) {
                if (e.message.match(/^Error:/)) {
                    var re = /^(Error:)\s*([^:]+:\s*)/;
                    // clean duplicated Error: added by JS
                    e.message = e.message.replace(re, '$1 $2');
                }
                if (code) {
                    // LIPS stack trace
                    if (!(e.__code__ instanceof Array)) {
                        e.__code__ = [];
                    }
                    e.__code__.push(code.toString(true));
                }
            }
            if (!(e instanceof IgnoreException)) {
                throw e;
            }
        }
    });
}
// -------------------------------------------------------------------------
function exec_collect(collect_callback) {
    return async function exec_lambda(arg, { env, dynamic_env, use_dynamic } = {}) {
        if (!is_env(dynamic_env)) {
            dynamic_env = env === true ? user_env : env || user_env;
        }
        if (env === true) {
            env = user_env;
        } else {
            env = env || user_env;
        }
        const results = [];
        if (is_pair(arg)) {
            return [await exec_with_stacktrace(arg, { env, dynamic_env, use_dynamic })];
        }
        const input = Array.isArray(arg) ? arg : _parse(arg);
        for await (let code of input) {
            const value = await exec_with_stacktrace(code, { env, dynamic_env, use_dynamic });
            results.push(collect_callback(code, await value));
        }
        return results;
    };
}
// -------------------------------------------------------------------------
function balanced(code) {
    var maching_pairs = {
        '[': ']',
        '(': ')'
    };
    var tokens;
    if (typeof code === 'string') {
        try {
            tokens = tokenize(code);
        } catch(e) {
            if (e instanceof Unterminated) {
                return false;
            }
            throw e;
        }
    } else {
        tokens = code.map(x => x && x.token ? x.token : x);
    }

    var open_tokens = Object.keys(maching_pairs);
    var brackets = Object.values(maching_pairs).concat(open_tokens);
    tokens = tokens.filter(token => brackets.includes(token));

    const stack = new Stack();
    for (const token of tokens) {
        if (open_tokens.includes(token)) {
            stack.push(token);
        } else if (!stack.is_empty()) { // closing token
            var last = stack.top();
            // last on stack need to match
            const closing_token = maching_pairs[last];
            if (token === closing_token) {
                stack.pop();
            } else {
                throw new Error(`Syntax error: missing closing ${closing_token}`);
            }
        } else {
            // closing bracket without opening
            throw new Error(`Syntax error: not matched closing ${token}`);
        }
    }
    return stack.is_empty();
}

// -------------------------------------------------------------------------
function fworker(fn) {
    // ref: https://stackoverflow.com/a/10372280/387194
    var str = '(' + fn.toString() + ')()';
    var URL = window.URL || window.webkitURL;
    var blob;
    try {
        blob = new Blob([str], { type: 'application/javascript' });
    } catch (e) { // Backwards-compatibility
        const BlobBuilder = window.BlobBuilder ||
              window.WebKitBlobBuilder ||
              window.MozBlobBuilder;
        blob = new BlobBuilder();
        blob.append(str);
        blob = blob.getBlob();
    }
    return new root.Worker(URL.createObjectURL(blob));
}

// -------------------------------------------------------------------------
function is_dev() {
    return lips.version.match(/^(\{\{VER\}\}|DEV)$/);
}

// -------------------------------------------------------------------------
function get_current_script() {
    if (is_node()) {
        return;
    }
    let script;
    if (document.currentScript) {
        script = document.currentScript;
    } else {
        const scripts = document.querySelectorAll('script');
        if (!scripts.length) {
            return;
        }
        script = scripts[scripts.length - 1];
    }
    const url = script.getAttribute('src');
    return url;
}

// -------------------------------------------------------------------------
const current_script = get_current_script();

// -------------------------------------------------------------------------
function bootstrap(url = '') {
    const std = 'dist/std.xcb';
    if (url === '') {
        if (current_script) {
            url = current_script.replace(/[^/]*$/, 'std.xcb');
        } else if (is_dev()) {
            url = `https://cdn.jsdelivr.net/gh/jcubic/lips@devel/${std}`;
        } else {
            url = `https://cdn.jsdelivr.net/npm/@jcubic/lips@${lips.version}/${std}`;
        }
    }
    global_env.set('__dirname', url.replace(/[^/]+$/, ''));
    var load = global_env.get('load');
    return load.call(user_env, url, global_env);
}
// -------------------------------------------------------------------------
function Worker(url) {
    this.url = url;
    const worker = this.worker = fworker(function() {
        var interpreter;
        var init;
        // string, numbers, booleans
        self.addEventListener('message', function(response) {
            var data = response.data;
            var id = data.id;
            if (data.type !== 'RPC' || id === null) {
                return;
            }
            function send_result(result) {
                self.postMessage({ id: id, type: 'RPC', result: result });
            }
            function send_error(message) {
                self.postMessage({ id: id, type: 'RPC', error: message });
            }
            if (data.method === 'eval') {
                if (!init) {
                    send_error('Worker RPC: LIPS not initialized, call init first');
                    return;
                }
                init.then(function() {
                    // we can use ES6 inside function that's converted to blob
                    var code = data.params[0];
                    var use_dynamic = data.params[1];
                    interpreter.exec(code, { use_dynamic }).then(function(result) {
                        result = result.map(function(value) {
                            return value && value.valueOf();
                        });
                        send_result(result);
                    }).catch(error => {
                        send_error(error);
                    });
                });
            } else if (data.method === 'init') {
                var url = data.params[0];
                if (typeof url !== 'string') {
                    send_error('Worker RPC: url is not a string');
                } else {
                    importScripts(`${url}/dist/lips.min.js`);
                    interpreter = new lips.Interpreter('worker');
                    init = bootstrap(url);
                    init.then(() => {
                        send_result(true);
                    });
                }
            }
        });
    });
    this.rpc = (function() {
        var id = 0;
        return function rpc(method, params) {
            var _id = ++id;
            return new Promise(function(resolve, reject) {
                worker.addEventListener('message', function handler(response) {
                    var data = response.data;
                    if (data && data.type === 'RPC' && data.id === _id) {
                        if (data.error) {
                            reject(data.error);
                        } else {
                            resolve(data.result);
                        }
                        worker.removeEventListener('message', handler);
                    }
                });
                worker.postMessage({
                    type: 'RPC',
                    method: method,
                    id: _id,
                    params: params
                });
            });
        };
    })();
    this.rpc('init', [url]).catch((error) => {
        console.error(error);
    });
    this.exec = function(code, { use_dynamic = false }) {
        return this.rpc('eval', [code, use_dynamic]);
    };
}

// -------------------------------------------------------------------------
// :: Serialization
// -------------------------------------------------------------------------
var serialization_map = {
    'pair': ([car, cdr]) => Pair(car, cdr),
    'number': function(value) {
        if (LString.isString(value)) {
            return LNumber([value, 10]);
        }
        return LNumber(value);
    },
    'regex': function([pattern, flag]) {
        return new RegExp(pattern, flag);
    },
    'nil': function() {
        return nil;
    },
    'symbol': function(value) {
        if (LString.isString(value)) {
            return LSymbol(value);
        } else if (Array.isArray(value)) {
            return LSymbol(Symbol.for(value[0]));
        }
    },
    'string': LString,
    'character': LCharacter
};
// -------------------------------------------------------------------------
// class mapping to create smaller JSON
const available_class = Object.keys(serialization_map);
const class_map = {};
for (let [i, cls] of Object.entries(available_class)) {
    class_map[cls] = +i;
}
function mangle_name(name) {
    return class_map[name];
}
function resolve_name(i) {
    return available_class[i];
}
// -------------------------------------------------------------------------
function serialize(data) {
    return JSON.stringify(data, function(key, value) {
        const v0 = this[key];
        if (v0) {
            if (v0 instanceof RegExp) {
                return {
                    '@': mangle_name('regex'),
                    '#': [v0.source, v0.flags]
                };
            }
            var cls = mangle_name(v0.constructor.__class__);
            if (!is_undef(cls)) {
                return {
                    '@': cls,
                    '#': v0.serialize()
                };
            }
        }
        return value;
    });
}
// -------------------------------------------------------------------------
function unserialize(string) {
    return JSON.parse(string, (_, object) => {
        if (object && typeof object === 'object') {
            if (!is_undef(object['@'])) {
                var cls = resolve_name(object['@']);
                if (serialization_map[cls]) {
                    return serialization_map[cls](object['#']);
                }
            }
        }
        return object;
    });
}

// -------------------------------------------------------------------------
// binary serialization using CBOR binary data format
// -------------------------------------------------------------------------
const cbor = (function() {

    var types = {
        'pair': Pair,
        'symbol': LSymbol,
        'number': LNumber,
        'string': LString,
        'character': LCharacter,
        'nil': nil.constructor,
        'regex': RegExp
    };

    function serializer(Class, fn) {
        return {
            deserialize: fn,
            Class
        };
    }

    var encoder = new Encoder();

    const cbor_serialization_map = {};
    for (const [ name, fn ] of Object.entries(serialization_map)) {
        const Class = types[name];
        cbor_serialization_map[name] = serializer(Class, fn);
    }
    // add CBOR data mapping
    let tag = 43311;
    Object.keys(cbor_serialization_map).forEach(type => {
        const data = cbor_serialization_map[type];
        if (typeof data === 'function') {
            const Class = data;
            addExtension({
                Class,
                tag,
                encode(instance, encode) {
                    encode(instance.serialize());
                },
                decode(data) {
                    return new Class(data);
                }
            });
        } else {
            const { deserialize, Class } = data;
            addExtension({
                Class,
                tag,
                encode(instance, encode) {
                    if (instance instanceof RegExp) {
                        return encode([instance.source, instance.flags]);
                    }
                    encode(instance.serialize());
                },
                decode(data) {
                    return deserialize(data);
                }
            });
        }
        tag++;
    });
    return encoder;
})();

// -------------------------------------------------------------------------
function merge_uint8_array(...args) {
    if (args.length > 1) {
        const len = args.reduce((acc, arr) => acc + arr.length, 0);
        const result = new Uint8Array(len);
        let offset = 0;
        args.forEach(item => {
            result.set(item, offset);
            offset += item.length;
        });
        return result;
    } else if (args.length) {
        return args[0];
    }
}

// -------------------------------------------------------------------------
function encode_magic() {
    const VERSION = 1;
    const encoder = new TextEncoder('utf-8');
    return encoder.encode(`LIPS${VERSION.toString().padStart(3, ' ')}`);
}

// -------------------------------------------------------------------------
const MAGIC_LENGTH = 7;

// -------------------------------------------------------------------------
function decode_magic(obj) {
    const decoder = new TextDecoder('utf-8');
    const prefix = decoder.decode(obj.slice(0, MAGIC_LENGTH));
    const name = prefix.substring(0, 4);
    if (name === 'LIPS') {
        const m = prefix.match(/^(....).*([0-9]+)$/);
        if (m) {
            return {
                type: m[1],
                version: Number(m[2])
            };
        }
    }
    return {
        type: 'unknown'
    };
}

// -------------------------------------------------------------------------
function serialize_bin(obj) {
    const magic = encode_magic();
    const payload = cbor.encode(obj);
    return merge_uint8_array(magic, pack(payload, { magic: false }));
}

// -------------------------------------------------------------------------
function unserialize_bin(data) {
    const { type, version } = decode_magic(data);
    if (type === 'LIPS' && version === 1) {
        const arr = unpack(data.slice(MAGIC_LENGTH), { magic: false });
        return cbor.decode(arr);
    } else {
        throw new Error(`Invalid file format ${type}`);
    }
}

// -------------------------------------------------------------------------
function execError(e) {
    console.error(e.message || e);
    if (Array.isArray(e.code)) {
        console.error(e.code.map((line, i) => `[${i + 1}]: ${line}`));
    }
}

// -------------------------------------------------------------------------
function init() {
    var lips_mimes = ['text/x-lips', 'text/x-scheme'];
    var bootstrapped;
    function boostrap_attr(script) {
        return script.getAttribute('data-bootstrap') ?? script.getAttribute('bootstrap');
    }
    function load(script) {
        return new Promise(function(resolve) {
            var src = script.getAttribute('src');
            if (src) {
                return fetch(src).then(res => res.text())
                    .then(exec).then(resolve).catch((e) => {
                        execError(e);
                        resolve();
                    });
            } else {
                return exec(script.innerHTML).then(resolve).catch((e) => {
                    execError(e);
                    resolve();
                });
            }
        });
    }

    function loop() {
        return new Promise(function(resolve) {
            var scripts = Array.from(document.querySelectorAll('script'));
            return (function loop() {
                var script = scripts.shift();
                if (!script) {
                    resolve();
                } else {
                    var type = script.getAttribute('type');
                    if (lips_mimes.includes(type)) {
                        const attr = boostrap_attr(script);
                        if (!bootstrapped && typeof attr === 'string') {
                            return bootstrap(attr).then(function() {
                                return load(script, function(e) {
                                    console.error(e);
                                });
                            }).then(loop);
                        } else {
                            return load(script, function(e) {
                                console.error(e);
                            }).then(loop);
                        }
                    } else if (type && type.match(/lips|lisp/)) {
                        console.warn('Expecting ' + lips_mimes.join(' or ') +
                                     ' found ' + type);
                    }
                    return loop();
                }
            })();
        });
    }
    if (!window.document) {
        return Promise.resolve();
    } else if (currentScript) {
        var script = currentScript;
        const attr = boostrap_attr(script);
        if (typeof attr === 'string') {
            return bootstrap(attr).then(function() {
                bootstrapped = true;
                return loop();
            });
        }
    }
    return loop();
}
// this can't be in init function, because it need to be in script context
const currentScript = typeof window !== 'undefined' &&
      window.document && document.currentScript;
// -------------------------------------------------------------------------
if (typeof window !== 'undefined') {
    contentLoaded(window, init);
}
// -------------------------------------------------------------------------
var banner = (function() {
    // Rollup tree-shaking is removing the variable if it's normal string because
    // obviously '{{DATE}}' == '{{' + 'DATE}}'; can be removed
    // but disabling Tree-shaking is adding lot of not used code so we use this
    // hack instead
    var date = LString('{{DATE}}').valueOf();
    var _date = date === '{{' + 'DATE}}' ? new Date() : new Date(date);
    var _format = x => x.toString().padStart(2, '0');
    var _year = _date.getFullYear();
    var _build = [
        _year,
        _format(_date.getMonth() + 1),
        _format(_date.getDate())
    ].join('-');
    var banner = `
  __ __                          __
 / / \\ \\       _    _  ___  ___  \\ \\
| |   \\ \\     | |  | || . \\/ __>  | |
| |    > \\    | |_ | ||  _/\\__ \\  | |
| |   / ^ \\   |___||_||_|  <___/  | |
 \\_\\ /_/ \\_\\                     /_/

LIPS Interpreter {{VER}} (${_build}) <https://lips.js.org>
Copyright (c) 2018-${_year} Jakub T. Jankiewicz

Type (env) to see environment with functions macros and variables. You can also
use (help name) to display help for specific function or macro, (apropos name)
to display list of matched names in environment and (dir object) to list
properties of an object.
`.replace(/^.*\n/, '');
    return banner;
})();
// -------------------------------------------------------------------------
// to be used with string function when code is minified
// -------------------------------------------------------------------------
read_only(Ahead, '__class__', 'ahead');
read_only(Pair, '__class__', 'pair');
read_only(Nil, '__class__', 'nil');
read_only(Pattern, '__class__', 'pattern');
read_only(Formatter, '__class__', 'formatter');
read_only(Macro, '__class__', 'macro');
read_only(Syntax, '__class__', 'syntax');
read_only(Syntax.Parameter, '__class__', 'syntax-parameter');
read_only(Environment, '__class__', 'environment');
read_only(InputPort, '__class__', 'input-port');
read_only(OutputPort, '__class__', 'output-port');
read_only(BufferedOutputPort, '__class__', 'output-port');
read_only(OutputStringPort, '__class__', 'output-string-port');
read_only(InputStringPort, '__class__', 'input-string-port');
read_only(InputFilePort, '__class__', 'input-file-port');
read_only(OutputFilePort, '__class__', 'output-file-port');
read_only(LipsError, '__class__', 'lips-error');
[LNumber, LComplex, LRational, LFloat, LBigInteger].forEach(cls => {
    read_only(cls, '__class__', 'number');
});
read_only(LCharacter, '__class__', 'character');
read_only(LSymbol, '__class__', 'symbol');
read_only(LString, '__class__', 'string');
read_only(QuotedPromise, '__class__', 'promise');
read_only(Parameter, '__class__', 'parameter');
// -------------------------------------------------------------------------
const version = '{{VER}}';
const date = '{{DATE}}';

// unwrap async generator into Promise<Array>
const parse = compose(uniterate_async, _parse);

export {
    version,
    banner,
    date,
    exec,
    parse,
    tokenize,
    evaluate,
    compile,

    serialize,
    unserialize,

    serialize_bin,
    unserialize_bin,

    bootstrap,

    Environment,
    user_env as env,

    Worker,

    Interpreter,
    balanced as balanced_parenthesis,
    balanced as balancedParenthesis,
    balanced,

    Macro,
    Syntax,
    Pair,
    Values,
    QuotedPromise,
    LipsError as Error,
    is_directive as _is_directive,

    quote,

    InputPort,
    OutputPort,
    BufferedOutputPort,
    InputFilePort,
    OutputFilePort,
    InputStringPort,
    OutputStringPort,
    InputByteVectorPort,
    OutputByteVectorPort,
    InputBinaryFilePort,
    OutputBinaryFilePort,
    set_fs,

    Formatter,
    Parser,
    Lexer,
    specials,
    repr,
    nil,
    eof,

    LSymbol,
    LNumber,
    LFloat,
    LComplex,
    LRational,
    LBigInteger,
    LCharacter,
    LString,
    Parameter,
    rationalize
};
const lips = {
    version,
    banner,
    date,
    exec,
    parse,
    tokenize,
    evaluate,
    compile,

    serialize,
    unserialize,

    serialize_bin,
    unserialize_bin,

    bootstrap,

    Environment,
    env: user_env,

    Worker,

    Interpreter,
    balanced_parenthesis: balanced,
    balancedParenthesis: balanced,
    balanced,

    Macro,
    Syntax,
    Pair,
    Values,
    QuotedPromise,
    Error: LipsError,

    quote,

    InputPort,
    OutputPort,
    BufferedOutputPort,
    InputFilePort,
    OutputFilePort,
    InputStringPort,
    OutputStringPort,
    InputByteVectorPort,
    OutputByteVectorPort,
    InputBinaryFilePort,
    OutputBinaryFilePort,
    set_fs,

    Formatter,
    Parser,
    Lexer,
    specials,
    repr,
    nil,
    eof,

    LSymbol,
    LNumber,
    LFloat,
    LComplex,
    LRational,
    LBigInteger,
    LCharacter,
    LString,
    Parameter,
    rationalize
};
global_env.set('lips', lips);
