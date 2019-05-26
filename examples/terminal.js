/**
 * This is example of usuage of LIPS interprter with jQuery Terminal
 *
 * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl>
 * Released under MIT license
 */
/* global jQuery, clearTimeout, setTimeout */

function terminal({selector, lips, dynamic = false, name = 'terminal'}) {
    var position;
    var timer;
    var help = lips.env.get('help');
    function doc(fn, doc) {
        fn.__doc__ = doc;
        return fn;
    }
    // -------------------------------------------------------------------------
    var env = lips.env.inherit(name, {
        stdout: {
            write: function() {
                var args = Array.from(arguments);
                args.forEach(function(arg) {
                    term.echo(arg, {formatters: false});
                });
            }
        },
        // ---------------------------------------------------------------------
        stdin: {
            read: function() {
                return term.read('');
            }
        },
        // ---------------------------------------------------------------------
        help: doc(new lips.Macro('help', function(code, { error }) {
            const { evaluate, Pair, Symbol, nil } = lips;
            var new_code = new Pair(new Symbol('__help'), code);
            var doc = evaluate(new_code, { env: this, error });
            term.echo(doc, { formatters: false });
        }), lips.env.env.help.__doc__),
        // ---------------------------------------------------------------------
        error: doc(function(message) {
            term.error(message);
        }, lips.env.env.error.__doc__)
    });
    // hack so (let ((x lambda)) (help x))
    env.env.__help = lips.env.env.help;
    // -------------------------------------------------------------------------
    var term = jQuery(selector).terminal(function(code, term) {
        // format before executing mainly for strings in function docs
        code = new lips.Formatter(code).format();
        lips.exec(code, env, dynamic).then(function(ret) {
            ret.forEach(function(ret) {
                if (ret !== undefined) {
                    if (ret instanceof lips.Symbol) {
                        ret = ret.name;
                    }
                    env.get('print').call(env, ret);
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
            const last = tokens.pop();
            if (last.trim().length) {
                const globals = Object.getOwnPropertyNames(window);
                const prefix = tokens.join('');
                const re = new RegExp('^' + jQuery.terminal.escape_regex(last));
                var commands = env.get('env')(env).toArray().concat(globals).filter(name => {
                    return re.test(name);
                }).map(name => prefix + name);
                // it don't return instanceof Array that is check by jQuery Terminal when using
                // iframes before version 2.4.2
                return Array.from(commands);
            }
        },
        keypress: function(e) {
            var term = this;
            if (e.key == ')') {
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
                        if (token.token === '(') {
                            count--;
                        } else if (token.token == ')') {
                            count++;
                        }
                    }
                    if (token.token == '(' && count === 0) {
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
    term.env = env;
    env.set('term', term);
    return term;
}
