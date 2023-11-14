/**
 * This is example of usage of LIPS interprter with jQuery Terminal
 *
 * Features: tab completion
 *           parenthesis matching
 *           auto formatting (when copy paste)
 *
 * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under MIT license
 */
/* global jQuery, $, clearTimeout, setTimeout */
var terminal = (function($) {
    return function({ selector, lips, dynamic = false, name = 'terminal' }, undefined) {
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
                    term.echo(String(arg), { newline: false });
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
            }), help.__doc__),
            // ---------------------------------------------------------------------
            pprint: doc(function(arg) {
                if (arg instanceof lips.Pair) {
                    arg = new lips.Formatter(arg.toString(true)).break().format();
                    if ($.terminal.prism) {
                        arg = $.terminal.prism('scheme', arg, { echo: true });
                    }
                    this.get('display').call(this, arg);
                } else {
                    this.get('write').call(this, arg);
                }
                this.get('newline').call(this);
            }, lips.env.get('pprint').__doc__),
            // ---------------------------------------------------------------------
            'stack-trace': doc(function() {
                if (strace) {
                    term.echo(strace);
                }
            }, `(stack-trace)

            Displays the stack trace of the last error.`),
            // ---------------------------------------------------------------------
            'display-error': doc(function(message) {
                term.error(message);
            }, lips.env.get('display-error').__doc__),
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
        var term = $(selector).terminal(function(code, term) {
            // format before executing mainly for strings in function docs
            //code = new lips.Formatter(code).format();
            return interpreter.exec(code, dynamic).then(function(ret) {
                ret.forEach(function(ret) {
                    if (ret !== undefined) {
                        display(repr(ret, true));
                        display("\n");
                    }
                });
            }).catch(function(e) {
                if (!e) {
                    console.warn('Exception is not defined');
                    return;
                }
                var message = e.message || e;
                term.error(message);
                term.echo('[[;red;]Call ][[;#fff;](stack-trace)][[;red;] to see the stack]');
                term.echo('[[;red;]Thrown exception is in global exception variable,\nuse ' +
                          '][[;#fff;](display exception.stack)][[;red;] to display JS stack trace]');
                if (e.__code__) {
                    strace = e.__code__.map((line, i) => {
                        var prefix = `[${i + 1}]: `;
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
                    var command = this.get_command();
                    if (command) {
                        // we handle only copy paste into empty cmd
                        return e.text;
                    }
                    var prompt = this.get_prompt();
                    var output;
                    try {
                        var formatter = new lips.Formatter(code);
                        if (!code.match(/\n/)) {
                            formatter.break();
                        }
                        output = formatter.format({
                            offset: prompt.length
                        });
                    } catch (e) {
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
            doubleTab: function(string, matches, echo_command) {
                echo_command();
                this.echo(matches.map(command => {
                    return lips.tokenize(command).pop();
                }), {
                    formatters: false
                });
            },
            completion: function(string) {
                let tokens = lips.tokenize(this.before_cursor(), true);
                tokens = tokens.map(token => token.token);
                // Array.from is need to for jQuery terminal version <2.5.0
                // when terminal is outside iframe and lips is inside
                // jQuery Terminal was using instanceof that don't work between iframes
                var env = Array.from(interpreter.get('env')().to_array());
                if (!tokens.length) {
                    return env;
                }
                const last = tokens.pop();
                if (last.trim().length) {
                    const globals = Object.getOwnPropertyNames(window);
                    const prefix = tokens.join('');
                    const re = new RegExp('^' + $.terminal.escape_regex(last));
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
        var tooltip = $('<div class="terminal terminal-external terminal-tooltip"/>')
            .appendTo('body');
        term.on('mouseover', '.token.function, .token.keyword, .token.operator, .token.builtin, .token.name', function() {
            var self = $(this);
            var lips = term.interpreter;
            var name = self.data('text');
            var ref = lips.__env__.ref(name);
            var doc = ref && (ref.get(name).__doc__ || ref.doc(name));
            var node = self;
            var top;
            if (doc) {
                var right, left;
                var cls = self.attr('class').split(' ').find(function(cls) {
                    return cls !== 'token';
                });
                var div = self.closest('div');
                var last = node, first = node;
                if (self.closest('.cmd').length) {
                    if (self.next().is('.' + cls)) {
                        last = self.nextUntil(':not(.' + cls + ')').last();
                    }
                    if (self.prev().is('.' + cls)) {
                        first = self.prevUntil(':not(.' + cls + ')').last();
                    }
                }
                top = div.offset().top;
                left = first.offset().left;
                right = last.offset().left + last.width();

                tooltip.text(doc);

                var help_width = tooltip.width();
                var help_height = tooltip.height();

                var term_height = term.height();
                var term_width = term.width();

                var term_offset = term.offset();
                var self_offset = self.offset();
                cls = ['__doc__'];
                if (top > help_height) {
                    cls.push('top');
                }
                if (left > window.innerWidth - help_width) {
                    cls.push('right');
                }
                tooltip.css({
                    '--left': left,
                    '--top': top,
                    '--color': self.css('--color'),
                    '--right': right
                }).addClass(cls.join(' '));
            }
        }).on('mouseout', '.token', function() {
            hide_tooltip();
        }).on('mousemove', '.terminal-scroller', function(e) {
            if ($(e.target).is('.terminal-scroller')) {
                hide_tooltip();
            }
        });
        function hide_tooltip() {
            tooltip.removeClass('__doc__ top right').text('');
        }
        term.interpreter = interpreter;
        interpreter.set('term', term);
        return term;
    };
})(jQuery);
