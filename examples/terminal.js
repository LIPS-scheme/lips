/**
 * This is example of usuage of LIPS interprter with jQuery Terminal
 *
 * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl>
 * Released under MIT license
 */
/* global jQuery, clearTimeout, setTimeout */

function terminal({selector, lips, dynamic = false, name = 'terminal'}, undefined) {
    var position;
    var timer;
    var help = lips.env.get('help');
    function doc(fn, doc) {
        fn.__doc__ = doc;
        return fn;
    }
    // this is way to make term.echo not enter newline it's easier then echo without newline
    // and it works similar in Node.js REPL
    var out_buffer = [];
    function flush() {
        if (out_buffer.length) {
            term.echo(out_buffer.join(''), {formatters: false});
            out_buffer = [];
        }
    }
    // -------------------------------------------------------------------------
    var interpreter = lips.Interpreter('demo', {
        stdout: lips.OutputPort(function() {
            var args = Array.from(arguments);
            if (args.length) {
                args.forEach(function(arg) {
                    out_buffer.push(arg);
                });
                var last_buff = out_buffer[out_buffer.length - 1];
                if (last_buff.match(/\n$/)) {
                    out_buffer[out_buffer.length - 1] = last_buff.replace(/\n$/, '');
                    flush();
                }
            }
        }),
        // ---------------------------------------------------------------------
        stdin: lips.InputPort(function() {
            return term.read('');
        }),
        // ---------------------------------------------------------------------
        // :: help that don't format the output - right now not needed because
        // :: stdout don't use formatters
        // ---------------------------------------------------------------------
        help: doc(new lips.Macro('help', function(code, { error }) {
            const { evaluate, Pair, LSymbol, nil } = lips;
            var new_code = new Pair(new LSymbol('__help'), code);
            var doc = evaluate(new_code, { env: this, error });
            if (doc !== undefined) {
                term.echo(doc, { formatters: false });
            }
        }), lips.env.get('help').__doc__),
        // ---------------------------------------------------------------------
        'stack-trace': doc(function() {
            if (strace) {
                term.echo(strace);
            }
        }, `(stack-trace)

            Function display stack trace of last error`),
        // ---------------------------------------------------------------------
        error: doc(function(message) {
            term.error(message);
        }, lips.env.get('error').__doc__),
        // ---------------------------------------------------------------------
        // hack so (let ((x lambda)) (help x))
        '__help': lips.env.get('help')
    });
    // -------------------------------------------------------------------------
    // display+repr(true) is same as write but that defined in external file
    // -------------------------------------------------------------------------
    var display = interpreter.get('display');
    var repr = interpreter.get('repr');
    var strace;
    var term = jQuery(selector).terminal(function(code, term) {
        // format before executing mainly for strings in function docs
        code = new lips.Formatter(code).format();
        return interpreter.exec(code, dynamic).then(function(ret) {
            flush();
            ret.forEach(function(ret) {
                if (ret !== undefined) {
                    display(repr(ret, true));
                    flush();
                }
            });
        }).catch(function(e) {
            var message = e.message || e;
            term.error(message);
            term.echo('[[;red;]Call ][[;#fff;](stack-trace)][[;red;] to see the stack]');
            term.echo('[[;red;]Thrown exception is in global exception variable,\nuse ' +
                      '][[;#fff;](display exception.stack)][[;red;] to display JS stack trace]');
            if (e.code) {
                strace = e.code.map((line, i) => {
                    var prefix = `[${i+1}]: `;
                    var formatter = new lips.Formatter(line);
                    var output = formatter.break().format({
                        offset: prefix.length
                    });
                    return prefix + output;
                }).join('\n');
            }
            window.exception = e;
        });
    }, {
        name,
        prompt: 'lips> ',
        enabled: false,
        greetings: false,
        keymap: {
            ENTER: function(e, original) {
                var command = this.get_command();
                try {
                    if (lips.balanced_parenthesis(command)) {
                        original();
                    } else {
                        var code = term.before_cursor();
                        var prompt = this.get_prompt();
                        var formatter = new lips.Formatter(code);
                        var i = formatter.indent({
                            indent: 2,
                            offset: prompt.length
                        });
                        this.insert('\n' + (new Array(i + 1).join(' ')));
                    }
                } catch (e) {
                    this.echo(this.get_prompt() + command);
                    this.set_command('');
                    term.error(e.message);
                }
            }
        },
        onPaste: function(e) {
            if (e.text) {
                var code = e.text;
                var prompt = this.get_prompt();
                try {
                    var formatter = new lips.Formatter(code);
                    if (!code.match(/\n/)) {
                        formatter.break();
                    }
                    var output = formatter.format({
                        offset: prompt.length
                    });
                } catch(e) {
                    console.log(e);
                    // boken LIPS code
                    output = code;
                }
                return Promise.resolve(output);
            }
        },
        keydown: function() {
            if (position) {
                term.set_position(position);
                position = false;
            }
        },
        mobileIngoreAutoSpace: [',', '.', ')'],
        completionEscape: false,
        wordAutocomplete: false,
        doubleTab: function(string, maches, echo_command) {
            echo_command();
            this.echo(maches.map(command => {
                return lips.tokenize(command).pop();
            }));
        },
        completion: function(string) {
            let tokens = lips.tokenize(this.before_cursor(), true);
            tokens = tokens.map(token => token.token);
            // Array.from is need to for jQuery terminal version <2.5.0
            // when terminal is outside iframe and lips is inside
            // jQuery Terminal was using instanceof that don't work between iframes
            var env = Array.from(interpreter.get('env')().toArray());
            if (!tokens.length) {
                return env;
            }
            const last = tokens.pop();
            if (last.trim().length) {
                const globals = Object.getOwnPropertyNames(window);
                const prefix = tokens.join('');
                const re = new RegExp('^' + jQuery.terminal.escape_regex(last));
                var commands = env.concat(globals).filter(name => {
                    return re.test(name);
                }).map(name => prefix + name);
                return Array.from(commands);
            }
        },
        keypress: function(e) {
            var term = this;
            function is_open(token) {
                return ['(', '['].indexOf(token) !== -1;
            }
            function is_close(token) {
                return [')', ']'].indexOf(token) !== -1;
            }
            if (is_close(e.key)) {
                setTimeout(function() {
                    position = term.get_position();
                    var command = term.get_command().substring(0, position);
                    var len = command.split(/\n/)[0].length;
                    var tokens = lips.tokenize(command, true);
                    var count = 1;
                    var token;
                    var i = tokens.length - 1;
                    while (count > 0) {
                        token = tokens[--i];
                        if (!token) {
                            return;
                        }
                        if (is_open(token.token)) {
                            count--;
                        } else if (is_close(token.token)) {
                            count++;
                        }
                    }
                    if (is_open(token.token) && count === 0) {
                        clearTimeout(timer);
                        setTimeout(function() {
                            var offset = token.offset;
                            term.set_position(offset);
                            timer = setTimeout(function() {
                                term.set_position(position);
                                position = false;
                            }, 200);
                        }, 0);
                    }
                }, 0);
            } else {
                position = false;
            }
        }
    });
    term.interpreter = interpreter;
    interpreter.set('term', term);
    return term;
}
