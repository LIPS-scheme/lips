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
    OutputPort } = require('../dist/lips');
const fs = require('fs');
const { format } = require('util');
const readline = require('readline');

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
function run(code, interpreter) {
    if (typeof code !== 'string') {
        code = code.toString();
    }
    return interpreter.exec(code).catch(function(e) {
        console.error(e.message);
        if (e.code) {
            console.error(e.code.map((line, i) => `[${i + 1}]: ${line}`).join('\n'));
        }
        console.error(e.stack);
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
    var list = ['./lib/bootstrap.scm', './examples/helpers.scm', './lib/R5RS.scm'];
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
    fn.__doc__ = doc;
    return fn;
}

// -----------------------------------------------------------------------------

var interp = Interpreter('repl', {
    stdin: InputPort(function() {
        return new Promise(function(resolve) {
            rl.question('', resolve);
        });
    }),
    stdout: OutputPort(function(x) {
        var repr = this.get('repr')(x);
        newline = !repr.match(/\n$/);
        process.stdout.write(repr);
    }),
    help: doc(new Macro('help', function(code, { error }) {
        var new_code = new Pair(new LSymbol('__help'), code);
        var doc = evaluate(new_code, { env: this, error });
        console.log(doc);
    }), env.get('help').__doc__),
    '__help': env.get('help')
});

// -----------------------------------------------------------------------------
const options = parse_options(process.argv.slice(2));
if (options.version || options.V) {
    // SRFI 176
    var os = require('os');
    global.output = Pair.fromArray([
        ["command", process.argv[1]],
        ["website", "https://jcubic.github.io/lips/"],
        ['languages', 'scheme', 'r5rs'].map(LSymbol),
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
    exec('(display (concat "(" (join "\n" (map repr output)) ")"))', interp);
} else if (options.c || options.code) {
    boostrap(interp).then(function() {
        run(options.c || options.code, interp).then(print);
    });
} else if (options._.length === 1) {
    // hack for node-gtk
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    fs.promises.readFile(options._[0]).then(function(data) {
        return boostrap(interp).then(function() {
            return run(data.toString().replace(/^#!.*\n/, ''), interp);
        });
    }).catch(err => {
        console.error(err);
    }).finally(function() {
        rl.close();
    });
} else if (options.h || options.help) {
    var name = process.argv[1];
    console.log(format('%s\nusage:\n%s [-h]|[-c <code>]|<filename>\n\n\t[-h --help] this help message' +
                       '\n\t[-c --code] execute code\n\t[--version -V] Display version information ' +
                       'according to srfi-176\n\nif called without arguments it will run REPL and if' +
                       'called with one argument\nit will treat it as filename and execute it.', banner, name));
} else {
    if (process.stdin.isTTY) {
        console.log(banner);
    }
    var prompt = 'lips> ';
    var continuePrompt = '... ';
    var terminal = !!process.stdin.isTTY && !(process.env.EMACS || process.env.INSIDE_EMACS)
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: prompt,
        terminal
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
            code += line.replace(/^\s+/, '') + '\n';
            try {
                if (balanced_parenthesis(code)) {
                    rl.pause();
                    prev_eval = prev_eval.then(function() {
                        var result = run(code, interp);
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
                    var spaces = new Array(ind + 1).join(' ');
                    if (terminal) {
                        rl.write(spaces);
                    } else {
                        process.stdout.write(spaces);
                        code += spaces;
                    }
                }
            } catch (e) {
                console.error(e.message);
                code = '';
                rl.prompt();
            }
        });
    });
}
