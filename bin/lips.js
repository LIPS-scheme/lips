#!/usr/bin/env node

const lily = require('@jcubic/lily');
const util = require('util');

const boolean = ['d', 'dynamic', 'q', 'quiet', 'V', 'version', 'trace', 't', 'debug', 'c', 'compile', 'b'];
const options = lily(process.argv.slice(2), { boolean });

const {
    exec,
    parse,
    Formatter,
    serialize,
    unserialize,
    balanced_parenthesis,
    tokenize,
    Interpreter,
    LSymbol,
    Macro,
    LString,
    evaluate,
    nil,
    version,
    date,
    Pair,
    env,
    banner,
    InputPort,
    OutputPort } = require('../src/lips');
const fs = require('fs');
const os = require('os');
const path = require('path');
const { format } = require('util');
const readline = require('readline');
var highlight = require('prism-cli');
var Prism = require('prismjs');
require('prismjs/components/prism-scheme.min.js');
require('../lib/js/prism.js');

const kDebounceHistoryMS = 15;

// -----------------------------------------------------------------------------
process.on('uncaughtException', function (err) {
  log_error(err.message);
});

// -----------------------------------------------------------------------------
function log_error(message) {
    fs.appendFileSync('error.log', message + '\n');
}
// -----------------------------------------------------------------------------
function debug(message) {
    console.log(message);
}
// -----------------------------------------------------------------------------
async function run(code, interpreter, dynamic = false, env = null, stack = false) {
    try {
        return await interpreter.exec(code, dynamic, env)
    } catch (e) {
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
            var prefix = `[${i+1}]: `;
            var formatter = new Formatter(line);
            var output = formatter.break().format({
                offset: prefix.length
            });
            return prefix + output;
        }).join('\n');
    }
    if (stack) {
        console.error(e.stack);
        console.error(strace);
    } else {
        console.error(e.message);
        console.error('Call (stack-trace) to see the stack');
        console.error('Thrown exception is in global exception variable, use ' +
                      '(display exception.stack) to display JS stack trace');
    }
    global.exception = e;}


// -----------------------------------------------------------------------------
function print(result) {
    if (result && result.length) {
        var last = result.pop();
        if (last !== undefined) {
            try {
                var ret = env.get('repr')(last, true);
                console.log('\x1b[K' + ret.toString());
            } catch(e) {
                print_error(e, options.t || options.trace);
            }
        }
    }
}

// -----------------------------------------------------------------------------
function bootstrap(interpreter) {
    var list = ['./dist/std.scm'];
    function open(name) {
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
        return fs.readFileSync(path).toString();
    }
    return (function next() {
        var name = list.shift();
        if (name) {
            var data = open(name);
            if (compiled(name)) {
                data = unserialize(data);
            }
            return run(data, interpreter, false, env.__parent__, true).then(next);
        } else {
            return Promise.resolve();
        }
    })();
}

// -----------------------------------------------------------------------------
function compiled(name) {
    return name.match(/\.xcm$/);
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
var interp = Interpreter('repl', {
    stdin: InputPort(function() {
        return new Promise(function(resolve) {
            rl = readline.createInterface({
                input: process.stdin,
                output: process.stdout
            });
            rl.question('', function(data) {
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
        process.stdout.write(x);
    }),
    // -------------------------------------------------------------------------
    __dirname: __dirname,
    __filename: __filename,
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

function readFile(filename) {
    const buff = fs.readFileSync(filename);
    return buff.toString().replace(/^#!.*\n/, '');
}
// -----------------------------------------------------------------------------

if (options.version || options.V) {
    // SRFI 176
    global.output = Pair.fromArray([
        ["command", "lips"],
        ["website", "https://lips.js.org"],
        ['languages', 'scheme', 'r5rs', 'r7rs'].map(LSymbol),
        ['encodings', 'utf-8'].map(LSymbol),
        ["scheme.srfi", 0, 4, 6, 10, 22, 23, 46, 176],
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
} else if (options.b && options._.length === 1) {
    const filename = options._[0];
    const code = readFile(filename);
    if (compiled(filename)) {
        for (expr of unserialize(code)) {
            console.log(expr.toString(true));
        }
    }
} else if ((options.c || options.compile) && options._.length === 1) {
    try {
        const filename = options._[0];
        console.log('Experimental compiler');
        console.log(`Compiling ${filename} ...`);
        const compiled_name = filename.replace(/\.[^.]+$/, '') + '.xcm';
        var code = readFile(filename);
        bootstrap(interp).then(function() {
            parse(code, interp.__env__).then(code => {
                fs.writeFileSync(compiled_name, serialize(code));
            });
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
    try {
        let code = readFile(filename);
        if (compiled(filename)) {
            code = unserialize(code);
        }
        return bootstrap(interp).then(() => {
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
    var intro = banner.replace(/(me>\n)[\s\S]+$/, '$1');
    console.log(format('%s\nusage:\n  %s -q | -h | -t | -e <code> | <filename> | -d\n\n  [-h --help]\t' +
                       '\tthis help message\n  [-e --eval]\t\texecute code\n  [-V --version]\tdisplay ' +
                       'version information according to srfi-176\n  [-q --quiet]\t\tdon\'t display ba' +
                       'nner in REPL\n  [-d --dynamic]\trun interpreter with dynamic scope\n  [-t --tr' +
                       'ace]\t\tprint JavaScript stack trace when extensions is thrown\n\nif called wi' +
                       'thout arguments it will run REPL and if called with one argument\nit will trea' +
                       't it as filename and execute it.', intro, path.basename(name)));
} else {
    const dynamic = options.d || options.dynamic;
    const entry = '   ' + (dynamic ? 'dynamic' : 'lexical') + ' scope $1';
    if (process.stdin.isTTY && !options.q && !options.quiet) {
        console.log(banner.replace(/(\n\nLIPS.+)/m, entry)); // '
    }
    var prompt = 'lips> ';
    var continuePrompt = '... ';
    var terminal = !!process.stdin.isTTY && !(process.env.EMACS || process.env.INSIDE_EMACS);
    rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: prompt,
        terminal
    });
    rl._writeToOutput = function _writeToOutput(string) {
        rl.output.write(scheme(string));
    };
    process.stdin.on('keypress', (c, k) => {
        setTimeout(function() {
            rl._refreshLine(); // force refresh colors
        }, 0);
    });
    const historySize = Number(env.LIPS_REPL_HISTORY_SIZE);
    if (!Number.isNaN(historySize) && historySize > 0) {
        rl.historySize = historySize;
    } else {
        rl.historySize = 1000;
    }
    setupHistory(rl, terminal ? env.LIPS_REPL_HISTORY : '', run_repl);
}

function unify_prompt(a, b) {
    var result = a;
    if (a.length < b.length) {
        result += new Array((b.length - a.length) + 1).join(' ');
    }
    return result;
}

function run_repl(err, rl) {
    const dynamic = options.d || options.dynamic;
    var code = '';
    var multiline = false;
    var resolve;
    // we use promise loop to fix issue when copy paste list of S-Expression
    var prev_eval = Promise.resolve();
    if (process.stdin.isTTY) {
        rl.prompt();
    }
    var prev_line;
    bootstrap(interp).then(function() {
        rl.on('line', function(line) {
            code += line;
            const lines = code.split('\n');
            const cols = process.stdout.columns;
            // fix formatting for previous lines that was echo
            // ReadLine will not handle those
            if (terminal && lines.length > 2) {
                var count = 0;
                // correction when line wrapps in original line
                // that will be overwritten
                lines.map(line => {
                    if (line.length > cols) {
                        count += Math.ceil(line.length / cols) - 1;
                    }
                });
                var f = new Formatter(code);
                code = f.format();
                const stdout = scheme(code).split('\n').map((line, i) => {
                    var prefix;
                    if (i === 0) {
                        prefix = unify_prompt(prompt, continuePrompt);
                    } else {
                        prefix = unify_prompt(continuePrompt, prompt);
                    }
                    return '\x1b[K' + prefix + line;
                }).join('\n');
                let num = lines.length + count;
                const format = `\x1b[${num}F${stdout}\n`;
                process.stdout.write(format);
            }
            code += '\n';
            try {
                if (balanced_parenthesis(code)) {
                    // we need to clear the prompt because resume
                    // is adding the prompt that was present when pause was called
                    // https://github.com/nodejs/node/issues/11699
                    rl.setPrompt('');
                    rl.pause();
                    prev_eval = prev_eval.then(function() {
                        var result = run(code, interp, dynamic, null, options.t || options.trace);
                        code = '';
                        return result;
                    }).then(function(result) {
                        if (process.stdin.isTTY) {
                            print(result);
                            if (newline) {
                                // readline doesn't work with not ended lines
                                // it ignore those, so we end them ourselves
                                process.stdout.write("\n");
                                newline = false;
                            }
                            if (multiline) {
                                multiline = false;
                            }
                        }
                        rl.setPrompt(prompt);
                        rl.prompt();
                        rl.resume();
                    }).catch(function() {
                        if (process.stdin.isTTY) {
                            if (multiline) {
                                multiline = false;
                            }
                        }
                        rl.setPrompt(prompt);
                        rl.prompt();
                        rl.resume();
                    });
                } else {
                    multiline = true;
                    var ind = indent(code, 2, prompt.length - continuePrompt.length);
                    rl.setPrompt(continuePrompt);
                    rl.prompt();
                    spaces = new Array(ind + 1).join(' ');
                    if (terminal) {
                        rl.write(spaces);
                    }
                }
            } catch (e) {
                console.error(e.message);
                console.error(e.stack);
                code = '';
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
