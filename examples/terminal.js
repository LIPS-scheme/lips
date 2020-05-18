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
    // -------------------------------------------------------------------------
    var interpreter = lips.Interpreter('demo', {
        stdout: lips.OutputPort(function() {
            var args = Array.from(arguments);
            args.forEach(function(arg) {
                term.echo(arg, {formatters: false});
            });
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
    var term = jQuery(selector).terminal(function(code, term) {
        // format before executing mainly for strings in function docs
        code = new lips.Formatter(code).format();
        return interpreter.exec(code, dynamic).then(function(ret) {
            ret.forEach(function(ret) {
                if (ret !== undefined) {
                    display(repr(ret, true));
                }
            });
        }).catch(function(e) {
            term.error(e.message || e);
            if (e.code) {
                term.error(e.code.map((line, i) => `[${i+1}]: ${line}`).join('\n'));
            }
        });
    }, {
        name,
        prompt: 'lips> ',
        enabled: false,
        greetings: false,
        keymap: {
            ENTER: function(e, original) {
                try {
                    if (lips.balanced_parenthesis(this.get_command())) {
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
                    original();
                }
            }
        },
        onPaste: function(e) {
            if (e.text) {
                var code = e.text;
                var prompt = this.get_prompt();
                try {
                    var formatter = new lips.Formatter(code);
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
