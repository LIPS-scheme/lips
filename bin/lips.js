#!/usr/bin/env node

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
function run(code, env) {
    if (typeof code !== 'string') {
        code = code.toString();
    }
    return exec(code, env).catch(function(e) {
        console.error(e.message);
        if (e.code) {
            console.error(e.code.map((line, i) => `[${i + 1}]: ${line}`).join('\n'));
        }
    });
}

// -----------------------------------------------------------------------------
function print(result) {
    if (result.length) {
        var last = result.pop();
        if (last !== undefined) {
            console.log(env.get('repr')(last, true));
        }
    }
}
// -----------------------------------------------------------------------------

function boostrap(env) {
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
            return run(data, env).then(next);
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
const options = parse_options(process.argv.slice(2));
if (options.version || options.V) {
    // SRFI 176
    var os = require('os');
    global.output = Pair.fromArray([
        ["command", process.argv[1]],
        ["website", "https://jcubic.github.io/lips/"],
        ['languages', 'scheme', 'r5rs'].map(LSymbol),
        ['encodings', 'utf-8'].map(LSymbol),
        ["scheme.srfi", 6, 22, 23, 176],
        ["release", version],
        ["os.uname", os.platform(), os.release()],
        ["os.env.LANG", process.env.LANG],
        ["os.env.TERM", process.env.TERM],
        ["build.date", new Date(date).toISOString()]
    ].map(([key, ...values]) => {
        return [LSymbol(key), ...values];
    }));
    exec('(display (concat "(" (join "\n" (map repr output)) ")"))');
} else if (options.c || options.code) {
    boostrap().then(function() {
        run(options.c || options.code, env).then(print);
    });
} else if (options._.length === 1) {
    var e = env.inherit('name');
    // hack for node-gtk
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });
    fs.promises.readFile(options._[0]).then(function(data) {
        return boostrap(e).then(function() {
            return run(data.toString().replace(/^#!.*\n/, ''), e);
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
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: prompt,
        terminal: !!process.stdin.isTTY
    });
    if (process.stdin.isTTY) {
        rl.prompt();
    }
    var code = '';
    var multiline = false;
    var resolve;
    var interp = Interpreter('name', {
        stdin: InputPort(function() {
            return new Promise(function(resolve) {
                rl.question('', resolve);
            });
        }),
        stdout: OutputPort(function(x) {
            //rl.write(this.get('repr')(x));
            console.log(this.get('repr')(x));
        }),
        help: doc(new Macro('help', function(code, { error }) {
            var new_code = new Pair(new LSymbol('__help'), code);
            var doc = evaluate(new_code, { env: this, error });
            console.log(doc);
        }), env.get('help').__doc__),
        '__help': env.get('help')
    });
    function doc(fn, doc) {
        fn.__doc__ = doc;
        return fn;
    }
    boostrap(interp.env).then(function() {
        rl.on('line', function(line) {
            code += line + '\n';
            if (balanced_parenthesis(code)) {
                rl.pause();
                run(code, interp.env).then(function(result) {
                    if (process.stdin.isTTY) {
                        print(result);
                        if (multiline) {
                            rl.setPrompt(prompt);
                        }
                        rl.prompt();
                    }
                    rl.resume();
                }).catch(function() {
                    if (process.stdin.isTTY) {
                        if (multiline) {
                            rl.setPrompt(prompt);
                        }
                        rl.prompt();
                    }
                });
                code = '';
            } else {
                multiline = true;
                var ind = indent(code, 2, prompt.length - continuePrompt.length);
                rl.setPrompt(continuePrompt);
                rl.prompt();
                rl.write(new Array(ind + 1).join(' '));
            }
        });
    });
}
