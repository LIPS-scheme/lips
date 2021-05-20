/**
 * This is example of usuage of LIPS interprter with jQuery Terminal
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
    return function({selector, lips, dynamic = false, name = 'terminal'}, undefined) {
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
            }), help.__doc__),
            // ---------------------------------------------------------------------
            pprint: doc(function(arg) {
                if (arg instanceof lips.Pair) {
                    arg = new lips.Formatter(arg.toString(true)).break().format();
                    if ($.terminal.prism) {
                        arg = $.terminal.prism('scheme', arg, {echo: true});
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

            Function display stack trace of last error`),
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
                flush();
                ret.forEach(function(ret) {
                    if (ret !== undefined) {
                        display(repr(ret, true));
                        flush();
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
            onResize: function() {
                var self = $(this);
                var $prompt = self.find('.cmd-prompt');
                var prompt = $prompt.html();
                $prompt.html('<span>&nbsp;</span>');
                self.data('geometry', {
                    terminal: self[0].getBoundingClientRect(),
                    char: $prompt.find('span')[0].getBoundingClientRect()
                });
                $prompt.html(prompt);
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
                    top = div.position().top;
                } else {
                    top = div.position().top + div.height();
                }
                left = first.position().left;
                right = last.position().left + last.width();
                var line_height = self.height();
                var help_lines = doc.split('\n');
                var help_height = line_height * (help_lines.length + 1);
                var max_chars = Math.max.apply(null, help_lines.map(function(line) { return line.length; }));

                var gemoetry = term.data('geometry');

                var help_width = (max_chars * gemoetry.char.width) + 10;

                var term_height = term.height();
                var term_width = term.width();

                var term_offset = term.offset();
                var self_offset = self.offset();
                cls = ['__doc__'];
                if (gemoetry.terminal.height < self_offset.top - term_offset.top + help_height) {
                    cls.push('top');
                }
                if (gemoetry.terminal.width < self_offset.left - term_offset.left + help_width) {
                    cls.push('right');
                }
                self.addClass(cls.join(' '));
                self.attr('data-doc', doc);
                self.css({
                    '--left': left,
                    '--top': top,
                    '--right': right
                });
            }
        }).on('mouseout', '.token.__doc__', function() {
            var self = $(this).removeClass('__doc__ top right');
            self.removeAttr('data-doc');
        });
        term.interpreter = interpreter;
        interpreter.set('term', term);
        return term;
    };
})(jQuery);
