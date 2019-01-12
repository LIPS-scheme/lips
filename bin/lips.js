#!/usr/bin/env node

const {exec} = require('../src/lips');
const fs = require('fs');
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
function run(code) {
    if (typeof code !== 'string') {
        code = code.toString();
    }
    return exec(code).catch(function(e) {
        console.error(e.message);
        if (e.code) {
            console.error('error in line ' + e.code);
        }
        console.error(e.stack);
    });
}

// -----------------------------------------------------------------------------
function print(result) {
    if (result.length) {
        var last = result.pop();
        if (last) {
            console.log(last.toString());
        }
    }
}
// -----------------------------------------------------------------------------

const options = parse_options(process.argv);

if (options.c) {
    run(options.c).then(print);
} else if (options.f) {
   fs.readFile(options.f, function(err, data) {
        if (err) {
            console.error(err);
        } else {
            run(data)
        }
    });
} else {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: 'lips> ',
        terminal: !!process.stdin.isTTY
    });
    if (process.stdin.isTTY) {
        rl.prompt();
    }
    rl.on('line', function(line) {
        rl.pause();
        run(line).then(function(result) {
            if (process.stdin.isTTY) {
                print(result);
                rl.prompt();
            }
            rl.resume();
        });
    });
}
