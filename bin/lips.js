#!/usr/bin/env -S node --no-deprecation

const {
    exec,
    Formatter,
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
const path = require('path');
const { format } = require('util');
const readline = require('readline');
var highlight = require('prism-cli');
var Prism = require('prismjs');
require('prismjs/components/prism-scheme.min.js');
require('../examples/prism.js');

// -----------------------------------------------------------------------------
process.on('uncaughtException', function (err) {
  log_error(err.message);
});

// -----------------------------------------------------------------------------
function log_error(message) {
    fs.appendFileSync('error.log', message + '\n');
}
// -----------------------------------------------------------------------------
// code taken from jQuery Terminal
function parse_options(arg, options) {
    var settings = Object.assign({}, {
        boolean: []
    }, options);
    var result = {
        _: []
    };
    function token(value) {
        this.value = value;
    }
    var rest = arg.reduce(function(acc, arg) {
        if (typeof arg !== 'string') {
            arg = String(arg);
        }
        if (arg.match(/^-/) && acc instanceof token) {
            result[acc.value] = true;
        }
        if (arg.match(/^--/)) {
            var name = arg.replace(/^--/, '');
            if (settings.boolean.indexOf(name) === -1) {
                return new token(name);
            } else {
                result[name] = true;
            }
        } else if (arg.match(/^-/)) {
            var single = arg.replace(/^-/, '').split('');
            if (settings.boolean.indexOf(single.slice(-1)[0]) === -1) {
                var last = single.pop();
            }
            single.forEach(function(single) {
                result[single] = true;
            });
            if (last) {
                return new token(last);
            }
        } else if (acc instanceof token) {
            result[acc.value] = arg;
        } else if (arg) {
            result._.push(arg);
        }
        return null;
    }, null);
    if (rest instanceof token) {
        result[rest.value] = true;
    }
    return result;
}

// -----------------------------------------------------------------------------
function run(code, interpreter, dynamic = false) {
    if (typeof code !== 'string') {
        code = code.toString();
    }
    return interpreter.exec(code, dynamic).catch(function(e) {
        console.error(e.message);
        console.error('Call (stack-trace) to see the stack');
        console.error('Thrown exception is in global exception variable, use ' +
                      '(display exception.stack) to display JS stack trace');
        log_error(e.message);
        if (e.code) {
            strace = e.code.map((line, i) => {
                var prefix = `[${i+1}]: `;
                var formatter = new Formatter(line);
                var output = formatter.break().format({
                    offset: prefix.length
                });
                return prefix + output;
            }).join('\n');
        }
        global.exception = e;
    });
}

// -----------------------------------------------------------------------------
function print(result) {
    if (result.length) {
        var last = result.pop();
        if (last !== undefined) {
            var ret = env.get('repr')(last, true);
            console.log(ret.toString());
        }
    }
}
// -----------------------------------------------------------------------------

function boostrap(interpreter) {
    var list = ['./lib/bootstrap.scm', './lib/R5RS.scm', './lib/R7RS.scm'];
    return (function next() {
        var name = list.shift();
        if (name) {
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
            var data = fs.readFileSync(path);
            return run(data, interpreter).then(next);
        }
    })();
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
    return highlight(str, 'scheme', { grammar: Prism.languages.scheme });
}

// -----------------------------------------------------------------------------
var strace;
var rl;
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
    stdout: OutputPort(function(x) {
        var repr = this.get('repr')(x);
        newline = !repr.match(/\n$/);
        process.stdout.write(repr);
    }),
    'stack-trace': doc(function() {
        if (strace) {
            console.log(strace);
        }
    }, `(stack-trace)

        Function display stack trace of last error`),
    exit: doc(function() {
        process.exit();
    }, `(exit)

        Function exits LIPS script or the REPL.`),
    help: doc(new Macro('help', function(code, { error }) {
        var new_code = new Pair(new LSymbol('__help'), code);
        var doc = evaluate(new_code, { env: this, error });
        console.log(doc);
    }), env.get('help').__doc__),
    '__help': env.get('help')
});

// -----------------------------------------------------------------------------
const options = parse_options(process.argv.slice(2), {boolean: ['d', 'dynamic']});
if (options.version || options.V) {
    // SRFI 176
    var os = require('os');
    global.output = Pair.fromArray([
        ["command", "lips"],
        ["website", "https://jcubic.github.io/lips/"],
        ['languages', 'scheme', 'r5rs', 'r7rs'].map(LSymbol),
        ['encodings', 'utf-8'].map(LSymbol),
        ["scheme.srfi", 6, 22, 23, 46, 176],
        ["release", version],
        ["os.uname", os.platform(), os.release()],
        ["os.env.LANG", process.env.LANG],
        ["os.env.TERM", process.env.TERM],
        ["build.date", new Date(date).toISOString()]
    ].map(([key, ...values]) => {
        return [LSymbol(key), ...values];
    }));
    boostrap(interp).then(function() {
        run('(for-each (lambda (x) (write x) (newline)) output)', interp, options.d || options.dynamic);
    });
} else if (options.e || options.eval || options.c || options.code) {
    // from 1.0 documentation should use -e but it's not breaking change
    boostrap(interp).then(function() {
        const code = options.e || options.eval || options.c || options.code;
        const dynamic = options.d || options.dynamic;
        return run(code, interp, dynamic).then(print);
    });
} else if (options._.length === 1) {
    // hack for node-gtk
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    fs.promises.readFile(options._[0]).then(function(data) {
        return boostrap(interp).then(() => {
            const code = data.toString().replace(/^#!.*\n/, '');
            const dynamic = options.d || options.dynamic;
            return run(code, interp, dynamic);
        });
    }).catch(err => {
        log_error(err.message || err);
        console.error(err);
    }).finally(function() {
        rl.close();
    });
} else if (options.h || options.help) {
    var name = process.argv[1];
    var intro = banner.replace(/(me>\n)[\s\S]+$/, '$1');
    console.log(format('%s\nusage:\n  %s -q | -h | -c <code> | <filename> | -d\n\n  [-h --help]\t\tthis' +
                       ' help message\n  [-e --eval]\t\texecute code\n  [-V --version]\tdisplay version' +
                       'information according to srfi-176\n  [-q --quiet]\t\tdon\'t display banner in R' +
                       'EPL\n  [-d --dynamic]\trun interpreter with dynamic scope\n\nif called without ' +
                       'arguments it will run REPL and if called with one argument\nit will treat it as' +
                       ' filename and execute it.', intro, path.basename(name)));
} else {
    const dynamic = options.d || options.dynamic;
    const entry = '   ' + (dynamic ? 'dynamic' : 'lexical') + ' scope $1';
    if (process.stdin.isTTY && !options.q && !options.quiet) {
        console.log(banner.replace(/(\n\nLIPS.+)/m, entry));
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
    if (process.stdin.isTTY) {
        rl.prompt();
    }
    var code = '';
    var multiline = false;
    var resolve;
    var newline;
    // we use promise loop to fix issue when copy paste list of S-Expression
    var prev_eval = Promise.resolve();
    boostrap(interp).then(function() {
        rl.on('line', function(line) {
            code += line + '\n';
            var format, spaces, stdout;
            var lines = code.split('\n');
            // fix previous line
            if (terminal && lines.length > 1) {
                var prev_line = lines[lines.length - 2].replace(/^\s+/, '');
                if (lines.length > 2) {
                    var prev = lines.slice(0, -2).join('\n');
                    var i = indent(prev, 2, prompt.length - continuePrompt.length);
                    spaces = new Array(i + 1).join(' ');
                    lines[lines.length - 2] = spaces + prev_line;
                    code = lines.join('\n');
                    stdout = continuePrompt + spaces;
                } else {
                    stdout = prompt;
                }
                stdout += scheme(prev_line);
                format = '\x1b[1F\x1b[K' + stdout + '\n';
                process.stdout.write(format);
            }
            try {
                if (balanced_parenthesis(code)) {
                    rl.pause();
                    prev_eval = prev_eval.then(function() {
                        var result = run(code, interp, dynamic);
                        code = '';
                        return result;
                    }).then(function(result) {
                        if (process.stdin.isTTY) {
                            print(result);
                            if (newline) {
                                // readline don't work with not endend lines
                                // it ignore those so we end then ourselfs
                                process.stdout.write("\n");
                                newline = false;
                            }
                            if (multiline) {
                                rl.setPrompt(prompt);
                                multiline = false;
                            }
                            rl.prompt();
                        }
                        rl.resume();
                    }).catch(function() {
                        if (process.stdin.isTTY) {
                            if (multiline) {
                                rl.setPrompt(prompt);
                                multiline = false;
                            }
                            rl.prompt();
                        }
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
                code = '';
                rl.prompt();
            }
        });
    }).catch(function(e) {
        log_error('Internal Error: boostrap filed');
        log_error(e.message || e);
        console.error('Internal Error: boostrap filed');
    });
}
