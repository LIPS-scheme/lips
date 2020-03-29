#!/usr/bin/env node

const {exec, Formatter, balanced_parenthesis, tokenize, env, banner} = require('../dist/lips');
const fs = require('fs');
const {format} = require('util');
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
            console.log(env.get('string')(last));
        }
    }
}
// -----------------------------------------------------------------------------
function boostrap() {
    var path;
    try {
        path = require.resolve('./examples/helpers.lips');
    } catch (e) {
        try {
            path = require.resolve('../examples/helpers.lips');
        } catch (e) {
            path = require.resolve('@jcubic/lips/examples/helpers.lips');
        }
    }
    var data = fs.readFileSync(path);
    return run(data, env);
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
if (options.c) {
    boostrap().then(function() {
        run(options.c, env).then(print);
    });
} else if (options._.length === 1) {
   fs.readFile(options._[0], function(err, data) {
        if (err) {
            console.error(err);
        } else {
            boostrap().then(function() {
                return run(data.toString().replace(/^#!.*\n/, ''), env);
            });
        }
    });
} else if (options.h) {
    var name = process.argv[1];
    console.log(format('%s\nusage:\n%s [-h]|[-c <code>]|<filename>\n\n\t-h this help message\n\t-c execute' +
                       ' code\n\nif called without arguments it will run REPL and if called with one argument' +
                       '\nit will treat it as filename and execute it.', intro, name));
} else {
    console.log(banner);
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
    var e = env.inherit('name', {
        stdin: {
            read: function() {
                return new Promise(function(resolve) {
                    rl.question('', resolve);
                });
            }
        }
    });
    boostrap().then(function() {
        rl.on('line', function(line) {
            code += line + '\n';
            if (balanced_parenthesis(code)) {
                rl.pause();
                run(code, e).then(function(result) {
                    if (process.stdin.isTTY) {
                        print(result);
                        if (multiline) {
                            rl.setPrompt(prompt);
                        }
                        code = '';
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
                    code = '';
                });
            } else {
                multiline = true;
                var i = indent(code, 2, prompt.length - continuePrompt.length);
                rl.setPrompt(continuePrompt);
                rl.prompt();
                rl.write(new Array(i + 1).join(' '));
            }
        });
    });
}
