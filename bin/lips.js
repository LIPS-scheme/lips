#!/usr/bin/env node

import lily from '@jcubic/lily';

const boolean = [
    'd', 'dynamic', 'q', 'quiet', 'V', 'version', 'trace', 't', 'c', 'compile'
];
const options = lily(process.argv.slice(2), { boolean });

const quiet = options.q || options.quiet;

import {
    exec,
    compile,
    parse,
    Parser,
    Formatter,
    serialize,
    unserialize,
    serialize_bin,
    unserialize_bin,
    balanced_parenthesis,
    tokenize,
    _is_directive,
    Interpreter,
    LSymbol,
    Macro,
    LString,
    evaluate,
    nil,
    Pair,
    env,
    banner,
    InputPort,
    OutputPort
} from '../src/lips.js';
import { version, date } from '../dist/lips.esm.js';

import fs from 'fs';
import os from 'os';
import path from 'path';
import { format } from 'util';
import readline from 'readline';
import highlight from 'prism-cli';
import Prism from 'prismjs';
import 'prismjs/components/prism-scheme.min.js';
import { satisfies } from 'compare-versions';
import '../lib/js/prism.js';

import { createRequire } from 'module';

const require = createRequire(import.meta.url);

const kDebounceHistoryMS = 15;

const supports_paste_brackets = satisfies(process.version, '>=v20.6.0');

// -----------------------------------------------------------------------------
process.on('uncaughtException', function (err) {
    log_error(err.message);
    log_error(err.stack);
});

// -----------------------------------------------------------------------------
function log_error(message) {
    const date = (new Date()).toISOString();
    message = message.split('\n').map(line => {
        return `${date}: ${line}`;
    }).join('\n');
    fs.appendFileSync(path.join(os.homedir(), 'lips.error.log'), message + '\n');
}
// -----------------------------------------------------------------------------
function debug(message) {
    console.log(message);
}
// -----------------------------------------------------------------------------
async function run(code, interpreter, use_dynamic = false, env = null, stack = false, log_unterminated = true) {
    try {
        return await interpreter.exec(code, { use_dynamic, env });
    } catch(e) {
        if (e instanceof Parser.Unterminated && !log_unterminated) {
            return;
        }
        print_error(e, stack);
    }
}

// -----------------------------------------------------------------------------
function print_error(e, stack) {
    if (!e) {
        console.log('Error is null');
        return;
    }
    log_error(e.message);
    if (e.__code__) {
        strace = e.__code__.map((line, i) => {
            const prefix = `[${i+1}]: `;
            const formatter = new Formatter(line);
            const output = formatter.break().format({
                offset: prefix.length
            });
            return prefix + output;
        }).join('\n');
    }
    if (stack) {
        console.error(e.stack);
        console.error(strace);
        process.exit(1);
    } else {
        console.error(e.message);
        console.error('Call (stack-trace) to see the stack');
        console.error('Thrown exception is in global exception variable, use ' +
                      '(display exception.stack) to display JS stack trace');
    }
    global.exception = e;
}

// -----------------------------------------------------------------------------
function print(result) {
    if (result && result.length) {
        const last = result.pop();
        if (last !== undefined) {
            try {
                const ret = env.get('repr')(last, true);
                console.log('\x1b[K' + ret.toString());
            } catch(e) {
                print_error(e, options.t || options.trace);
            }
        }
    }
}

// -----------------------------------------------------------------------------
function bootstrap(interpreter) {
    const bootstrap = options.b || options.bootstrap;
    if (bootstrap === 'none') {
        return Promise.resolve();
    }
    const file = bootstrap ? bootstrap : './dist/std.xcb';
    function read(name) {
        var path;
        try {
            path = require.resolve(`./${name}`);
        } catch (e) {
            try {
                path = require.resolve(`../${name}`);
            } catch (e) {
                path = require.resolve(`@jcubic/lips/../${name}`);
            }
        }
        return readCode(path);
    }
    const code = read(file);
    return run(code, interpreter, false, env.__parent__, true);
}

// -----------------------------------------------------------------------------
function readCode(filename) {
    if (compiled_binary(filename)) {
        return unserialize_bin(readBinary(filename));
    } else {
        const code = readFile(filename);
        if (compiled(filename)) {
            return unserialize(code);
        }
        return code;
    }
}

// -----------------------------------------------------------------------------
function compiled(name) {
    return name.match(/\.xcm$/);
}

// -----------------------------------------------------------------------------
function compiled_binary(name) {
    return name.match(/\.xcb$/);
}

// -----------------------------------------------------------------------------
function indent(code, indent, offset) {
    var formatter = new Formatter(code);
    return formatter.indent({
        indent,
        offset
    });
}

// -----------------------------------------------------------------------------
function doc(fn, doc) {
    fn.__doc__ = doc.split('\n').map(function(line) {
        return line.trim();
    }).join('\n');
    return fn;
}

// -----------------------------------------------------------------------------
function scheme(str) {
    return highlight(str, 'scheme', {
        grammar: Prism.languages.scheme,
        newlines: true
    });
}

// -----------------------------------------------------------------------------
function log(message) {
    if (typeof message !== 'string') {
        message = message.toString();
    }
    fs.appendFile('out.log', message + '\n', (err) => { });
}

// -----------------------------------------------------------------------------
var strace;
var rl;
var newline;
const moduleURL = new URL(import.meta.url);
const __dirname = path.dirname(moduleURL.pathname);
const __filename = path.basename(moduleURL.pathname);
const command_line = [];
let last_line = '';
const interp = Interpreter('repl', {
    stdin: InputPort(function() {
        return new Promise(function(resolve) {
            rl = readline.createInterface({
                input: process.stdin,
                output: process.stdout
            });
            rl.question(last_line, function(data) {
                resolve(data);
                rl.close();
            });
        });
    }),
    // -------------------------------------------------------------------------
    stdout: OutputPort(function(x) {
        if (typeof x !== 'string') {
            x = this.get('repr')(x);
        }
        newline = !x.match(/\n$/);
        last_line = x.split('\n').pop();
        process.stdout.write(x);
    }),
    // -------------------------------------------------------------------------
    __dirname: __dirname,
    __filename: __filename,
    command_line,
    // -------------------------------------------------------------------------
    'stack-trace': doc(function() {
        if (strace) {
            console.log(strace);
        }
    }, `(stack-trace)

        Function display stack trace of last error`),
    // -------------------------------------------------------------------------
    exit: doc(function(code) {
        process.exit(code);
    }, `(exit)
        (exit error-code)

        Function exits LIPS script or the REPL.`),
    // -------------------------------------------------------------------------
    pprint: doc(function(arg) {
        if (arg instanceof Pair) {
            arg = new Formatter(arg.toString(true)).break().format();
            this.get('display').call(this, scheme(arg));
        } else {
            this.get('write').call(this, scheme(arg));
        }
        this.get('newline').call(this);
    }, env.get('pprint').__doc__),
    // -------------------------------------------------------------------------
    help: doc(new Macro('help', function(code, { error }) {
        var new_code = new Pair(new LSymbol('__help'), code);
        var doc = evaluate(new_code, { env: this, error });
        if (doc) {
            console.log(doc.toString());
        }
    }), env.get('help').__doc__),
    // -------------------------------------------------------------------------
    '__help': env.get('help')
});

function readBinary(filename) {
    return fs.readFileSync(filename);
}

// -----------------------------------------------------------------------------
function readFile(filename) {
    const buff = readBinary(filename);
    return buff.toString().replace(/^(#!.*)([\s\n])/, function(_, shebang, delim) {
        if (_is_directive(shebang)) {
            return shebang + delim;
        }
        return '';
    });
}

// -----------------------------------------------------------------------------
function get_command_line_args() {
    const filename = options._[0];
    const index = process.argv.findIndex((item) => item === filename);
    return process.argv.slice(index);
}

// -----------------------------------------------------------------------------
if (options.version || options.V) {
    // SRFI 176
    global.output = Pair.fromArray([
        ["command", "lips"],
        ["website", "https://lips.js.org"],
        ['languages', 'scheme', 'r5rs', 'r7rs'].map(LSymbol),
        ['encodings', 'utf-8'].map(LSymbol),
        ["scheme.srfi", 0, 1, 2, 4, 6, 8, 10, 22, 23, 26, 28, 46, 69, 98, 111, 139, 156, 176, 193, 210, 236],
        ["release", version],
        ["os.uname", os.platform(), os.release()],
        ["os.env.LANG", process.env.LANG],
        ["os.env.TERM", process.env.TERM],
        ["build.date", date.match(/^\{\{|\}\}$/) ? date : new Date(date).toISOString()]
    ].map(([key, ...values]) => {
        return [LSymbol(key), ...values];
    }));
    bootstrap(interp).then(function() {
        return run('(for-each (lambda (x) (write x) (newline)) output)', interp, options.d || options.dynamic);
    });
} else if (options.e || options.eval) {
    // from 1.0 documentation should use -e but it's not breaking change
    bootstrap(interp).then(function() {
        const code = options.e || options.eval;
        const dynamic = options.d || options.dynamic;
        return run(code, interp, dynamic, null, true).then(print);
    });
} else if ((options.c || options.compile) && options._.length === 1) {
    try {
        const filename = options._[0];
        if (!quiet) {
            console.log('Experimental compiler');
            console.log(`Compiling ${filename} ...`);
        }
        const ext = '.xcb';
        const compiled_name = filename.replace(/\.[^.]+$/, '') + ext;
        var code = readFile(filename);
        const cwd = process.cwd();
        bootstrap(interp).then(function() {
            return compile(code, interp.__env__).then(code => {
                if (!quiet) {
                    console.log(`Writing ${compiled_name} ...`);
                }
                try {
                    const encoded = serialize_bin(code);
                    fs.writeFile(compiled_name, encoded, function(err) {
                        if (err) {
                            console.error(err);
                        } else if (!quiet) {
                            console.log('DONE');
                        }
                    });
                } catch (e) {
                    console.error(e);
                }
            })
        }).catch(e => {
            print_error(e, true);
        });
    } catch(e) {
        console.log(e);
    }
} else if (options._.length >= 1) {
    // hack for node-gtk
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    rl.on('exit', () => {
        if (rl._flushing) {
            rl.pause();
            return rl.once('flushHistory', () => {
                process.exit();
            });
        }
        process.exit();
    });
    const filename = options._[0];
    command_line.push(...get_command_line_args());
    try {
        const code = readCode(filename);
        bootstrap(interp).then(() => {
            const dynamic = options.d || options.dynamic;
            return run(code, interp, dynamic, null, options.t || options.trace);
        });
    } catch (err) {
        log_error(err.message || err);
        console.error(err);
    } finally {
        rl.close();
    }
} else if (options.h || options.help) {
    var name = process.argv[1];
    var intro = banner.replace(/(Jankiewicz\n)[\s\S]+$/, '$1').replace('{{VER}}', version);
    console.log(format('%s\nusage:\n  %s -q | -c | -h | -t | -b <file> | -d | -e <code> | <filename>\n' +
                       '\n  [-h --help]\t\tthis help message\n  [-e --eval]\t\texecute code\n  [-V --v' +
                       'ersion]\tdisplay version information according to srfi-176\n  [-c --compile]\t' +
                       'parse and compile the file into binary file format\n  [-b --boostrap]\tpoint t' +
                       'o a file that should be used for boostraping standard library,\n\t\t\tdefault ' +
                       'is ./dist/std.xcb. use none to disable boostraping\n  [-q --quiet]\t\tdon\'t d' +
                       'isplay banner in REPL\n  [-d --dynamic]\trun interpreter with dynamic scope\n ' +
                       ' [-t --trace]\t\tprint JavaScript and scheme stack traces when extensions is th' +
                       'rown\n\nif called without arguments it will run the REPL and if called with on' +
                       'e argument\nit will treat it as filename and execute it.',
                       intro, path.basename(name)));
} else {
    const dynamic = options.d || options.dynamic;
    const entry = '   ' + (dynamic ? 'dynamic' : 'lexical') + ' scope $1';
    if (process.stdin.isTTY && !quiet) {
        console.log(banner.replace(/(\n\nLIPS.+)/m, entry)); // '
    }
    var prompt = 'lips> ';
    var continue_prompt = '... ';
    var terminal = !!process.stdin.isTTY && !(process.env.EMACS || process.env.INSIDE_EMACS);
    rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: prompt,
        terminal
    });
    const historySize = Number(env.LIPS_REPL_HISTORY_SIZE);
    if (!Number.isNaN(historySize) && historySize > 0) {
        rl.historySize = historySize;
    } else {
        rl.historySize = 1000;
    }
    setupHistory(rl, terminal ? env.LIPS_REPL_HISTORY : '', run_repl);
}

function is_open(token) {
    return ['(', '['].includes(token);
}

function is_close(token) {
    return [')', ']'].includes(token);
}

// find matching open parentheses. The last token is always
// a closing parenthesis
function matched_token(code) {
    let count = 0;
    // true to tokenize return tokens with meta data
    return tokenize(code, true).reverse().find(token => {
        if (is_open(token.token)) {
            count--;
        } else if (is_close(token.token)) {
            count++;
        }
        return is_open(token.token) && count === 0;
    });
}

// hightlight the open parentheses based on token metadata
function mark_paren(code, token) {
    const re = /(\x1b\[[0-9;]*m)/;
    let str_len = 0, found;
    return code.split(re).reduce((acc, str) => {
        let result
        if (str.match(re)) {
            result = str;
        } else if (found) {
            result = str;
        } else if (str_len + str.length <= token.offset) {
            result = str;
            str_len += str.length;
        } else {
            const pos = token.offset - str_len;
            const before = str.substring(0, pos);
            const after = str.substring(pos + 1);
            result = `${before}\x1b[7m(\x1b[m${after}`;
            found = true;
        }
        return acc + result;
    }, '');
}

// function accept ANSI (syntax highlighted code) and
// return ANSI escapes to overwrite the code
// this is needed to make sure that syntax hightlighting
// is always correct
function ansi_rewrite_above(ansi_code) {
    const lines = ansi_code.split('\n');
    const stdout = lines.map((line, i) => {
        const prefix = i === 0 ? prompt : continue_prompt;
        return '\x1b[K' + prefix + line;
    }).join('\n');
    const len = lines.length;
    // overwrite all lines to get rid of any artifacts left my stdin
    // mostly because of parenthesis matching
    return `\x1b[${len}F${stdout}\n`;
}

function run_repl(err, rl) {
    const dynamic = options.d || options.dynamic;
    let cmd = '';
    let multiline = false;
    let resolve;
    const brackets_re = /\x1b\[(200|201)~/g;
    // we use promise loop to fix issue when copy paste list of S-Expression
    let prev_eval = Promise.resolve();
    if (process.stdin.isTTY) {
        rl.prompt();
    }
    let prev_line;
    const is_emacs = process.env.INSIDE_EMACS;
    function is_brackets_mode() {
        return cmd.match(brackets_re);
    }
    function continue_multiline(code) {
        multiline = true;
        // we don't do indentation for paste bracket mode
        // indentation will also not work for old Node.js
        // becase it's too problematice to make it right
        if (is_brackets_mode() || !supports_paste_brackets) {
            rl.prompt();
            if (is_emacs) {
                rl.setPrompt('');
            } else {
                rl.setPrompt(continue_prompt);
            }
            if (terminal) {
                rl.write(' '.repeat(prompt.length - continue_prompt.length));
            }
        } else {
            let ind;
            try {
                ind = indent(code, 2, prompt.length - continue_prompt.length);
            } catch (e) {
                ind = 0;
            }
            const spaces = ' '.repeat(ind);
            if (is_emacs) {
                rl.setPrompt('');
                rl.prompt();
            } else {
                rl.setPrompt(continue_prompt);
                rl.prompt();
                rl.write(spaces);
            }
        }
    }
    function char_before_cursor() {
        return rl.line[rl.cursor - 1];
    }
    rl._writeToOutput = function _writeToOutput(string) {
        try {
            const prefix = multiline ? continue_prompt : prompt;
            const current_line = prefix + rl.line;
            let code = scheme(string);
            if (!is_brackets_mode()) {
                // we remove traling newline from cmd
                let code_above = cmd && scheme(cmd.substring(0, cmd.length - 1));
                if (char_before_cursor() === ')') {
                    const substring = rl.line.substring(0, rl.cursor);
                    const input = prefix + substring;
                    const token = matched_token(input);
                    if (token) {
                        code = mark_paren(code, token);
                    } else if (cmd) {
                        const input = cmd + rl.line;
                        // we match paren above the current line
                        // but we need whole code with rl.line
                        // so we need to ignore rl.line
                        const token = matched_token(input);
                        if (token) {
                            // code_above don't include the current line that
                            // is added after user press enter
                            code_above = mark_paren(code_above, token);
                        }
                    }
                }
                // ignore the process when user press enter
                if (code_above && !string.match(/^[\n\r]+$/)) {
                    // overwrite lines above the cursor this is side effect
                    process.stdout.write(ansi_rewrite_above(code_above));
                }
            }
            // this always need to be executed inside rl._writeToOutput
            // even if nothing changes, this make sure that the input
            // stay intact while editing the command line
            rl.output.write(code);
        } catch(e) {
            console.error(e);
        }
    };
    process.stdin.on('keypress', (c, k) => {
        setTimeout(function() {
            // we force triggering rl._writeToOutput function
            // so we have the change to syntax highlight the command line
            // this nees to happen on next tick so the string have time
            // to updated with a given key
            rl._refreshLine();
        }, 0);
    });
    bootstrap(interp).then(function() {
        if (supports_paste_brackets) {
            // this make sure that the paste brackets ANSI escape
            // is added to cmd so they can be processed in 'line' event
            process.stdin.on('keypress', (c, k) => {
                if (k?.name?.match(/^paste-/)) {
                    cmd += k.sequence;
                }
            });
            // enable paste bracket mode by the terminal
            process.stdout.write('\x1b[?2004h');
        }
        rl.on('line', function(line) {
            try {
                cmd += line;
                const code = cmd.replace(brackets_re, '');
                if (cmd.match(/\x1b\[201~$/)) {
                    cmd = code;
                }
                if (terminal) {
                    const output = ansi_rewrite_above(scheme(code));
                    process.stdout.write(output);
                }
                cmd += '\n';
                if (balanced_parenthesis(code)) {
                    // we need to clear the prompt because resume
                    // is adding the prompt that was present when pause was called
                    // https://github.com/nodejs/node/issues/11699
                    rl.setPrompt('');
                    rl.pause();
                    prev_eval = prev_eval.then(function() {
                        const result = run(code, interp, dynamic, null, options.t || options.trace, false);
                        cmd = '';
                        return result;
                    }).then(function(result) {
                        if (process.stdin.isTTY) {
                            print(result);
                            if (newline || is_emacs) {
                                // readline doesn't work with not ended lines
                                // it ignore those, so we end them ourselves
                                process.stdout.write("\n");
                                newline = false;
                            }
                            if (multiline) {
                                multiline = false;
                            }
                        }
                    }).catch(function() {
                        if (process.stdin.isTTY) {
                            if (multiline) {
                                multiline = false;
                            }
                        }
                    }).finally(function() {
                        rl.setPrompt(prompt);
                        rl.prompt();
                        rl.resume();
                    });
                } else {
                    continue_multiline(code);
                }
            } catch (e) {
                console.error(e.message);
                console.error(e.stack);
                cmd = '';
                rl.setPrompt(prompt);
                rl.prompt();
            }
        });
    }).catch(function(e) {
        log_error('Internal Error: bootstrap filed');
        log_error(e.message || e);
        console.error('Internal Error: bootstrap filed');
    });
}

// source: Node.js https://github.com/nodejs/node/blob/master/lib/internal/repl/history.js
function _writeToOutput(repl, message) {
  repl._writeToOutput(message);
  repl._refreshLine();
}

function setupHistory(repl, historyPath, ready) {
  // Empty string disables persistent history
  if (typeof historyPath === 'string')
    historyPath = historyPath.trim();

  if (historyPath === '') {
    repl._historyPrev = _replHistoryMessage;
    return ready(null, repl);
  }

  if (!historyPath) {
    try {
      historyPath = path.join(os.homedir(), '.lips_repl_history');
    } catch (err) {
      _writeToOutput(repl, '\nError: Could not get the home directory.\n' +
        'REPL session history will not be persisted.\n');

      debug(err.stack);
      repl._historyPrev = _replHistoryMessage;
      return ready(null, repl);
    }
  }

  let timer = null;
  let writing = false;
  let pending = false;
  repl.pause();
  // History files are conventionally not readable by others:
  // https://github.com/nodejs/node/issues/3392
  // https://github.com/nodejs/node/pull/3394
  fs.open(historyPath, 'a+', 0o0600, oninit);

  function oninit(err, hnd) {
    if (err) {
      // Cannot open history file.
      // Don't crash, just don't persist history.
      _writeToOutput(repl, '\nError: Could not open history file.\n' +
        'REPL session history will not be persisted.\n');
      debug(err.stack);

      repl._historyPrev = _replHistoryMessage;
      repl.resume();
      return ready(null, repl);
    }
    fs.close(hnd, onclose);
  }

  function onclose(err) {
    if (err) {
      return ready(err);
    }
    fs.readFile(historyPath, 'utf8', onread);
  }

  function onread(err, data) {
    if (err) {
      return ready(err);
    }

    if (data) {
      repl.history = data.split(/[\n\r]+/, repl.historySize);
    } else {
      repl.history = [];
    }

    fs.open(historyPath, 'r+', onhandle);
  }

  function onhandle(err, hnd) {
    if (err) {
      return ready(err);
    }
    fs.ftruncate(hnd, 0, (err) => {
      repl._historyHandle = hnd;
      repl.on('line', online);
      repl.once('exit', onexit);

      // Reading the file data out erases it
      repl.once('flushHistory', function() {
        repl.resume();
        ready(null, repl);
      });
      flushHistory();
    });
  }

  // ------ history listeners ------
  function online(line) {
    repl._flushing = true;

    if (timer) {
      clearTimeout(timer);
    }

    timer = setTimeout(flushHistory, kDebounceHistoryMS);
  }

  function flushHistory() {
    timer = null;
    if (writing) {
      pending = true;
      return;
    }
    writing = true;
    const historyData = repl.history.join(os.EOL);
    fs.write(repl._historyHandle, historyData, 0, 'utf8', onwritten);
  }

  function onwritten(err, data) {
    writing = false;
    if (pending) {
      pending = false;
      online();
    } else {
      repl._flushing = Boolean(timer);
      if (!repl._flushing) {
        repl.emit('flushHistory');
      }
    }
  }

  function onexit() {
    if (repl._flushing) {
      repl.once('flushHistory', onexit);
      return;
    }
    repl.off('line', online);
    fs.close(repl._historyHandle, () => {});
  }
}

function _replHistoryMessage() {
  if (this.history.length === 0) {
    _writeToOutput(
      this,
      '\nPersistent history support disabled. Use user-writable path to enable.\n'
    );
  }
  this._historyPrev = readline.Interface.prototype._historyPrev;
  return this._historyPrev();
}
