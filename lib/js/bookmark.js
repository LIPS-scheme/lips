javascript:(function(next) {
    /**
     * This is bookmarklet that will create terminal with LIPS REPL
     *
     * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl/me>
     * Released under MIT license
     */
    /* global jQuery, terminal, lips, Prism */
    var orig_jQuery;
    var dolar;
    if (window.jQuery) {
        if (window.$ === window.jQuery) {
            dolar = true;
        }
        orig_jQuery = window.jQuery.noConflict(true);
    }
    var script = (function() {
        var head = document.getElementsByTagName('head')[0];
        return function(src) {
            var script = document.createElement('script');
            script.setAttribute('src', src);
            script.setAttribute('type', 'text/javascript');
            head.appendChild(script);
            return script;
        };
    })();
    var timer;
    window.addEventListener('securitypolicyviolation', function handler(event) {
        if (event.blockedURI === 'https://cdn.jsdelivr.net/npm/jquery') {
            clearTimeout(timer);
            alert('[WARN]: This bookmark will not run on this page because of ' +
                  'Content Security Policy');
        }
        window.removeEventListener('securitypolicyviolation', handler);
    });
    script('https://cdn.jsdelivr.net/npm/jquery');
    (function delay(time) {
        if (typeof jQuery === 'undefined') {
            timer = setTimeout(function() {
                delay(time);
            }, time);
        } else {
            next(jQuery, function() {
                if (orig_jQuery) {
                    window.jQuery = orig_jQuery;
                    if (dolar) {
                        window.$ = window.jQuery;
                    }
                }
            });
        }
    })(500);
})(async function($, next) {
    async function hash(branch) {
        try {
            var url = `https://api.github.com/repos/jcubic/lips/commits?sha=${branch}`;
            var res = await fetch(url);
            var data = await res.json();
            return data[0].sha;
        } catch (e) {
            return branch;
        }
    }
    const REF = await hash('master');
    function init() {
        $('.terminal.lips').each(function() {
            $(this).terminal().destroy().remove();
        });
        $.terminal.defaults.linksNoReferrer = true;
        $.terminal.defaults.formatters = $.terminal.defaults.formatters.filter((x) => {
            return x.name !== 'syntax_scheme';
        });
        $.terminal.syntax("scheme");
        $('.shell-wrapper').remove();
        var wrapper = $('<div>').addClass('shell-wrapper').appendTo('body');
        var container = $('<div>').addClass('shell-container').appendTo(wrapper);
        $('<div class="shell-mask"/>').appendTo(wrapper);
        var nav = $('<nav/>').appendTo(container);
        var location;
        wrapper.on('mousedown', 'nav', function(e) {
            location = {
                x: e.clientX,
                y: e.clientY,
                rect: container[0].getBoundingClientRect()
            };
            wrapper.addClass('drag');
            return false;
        }).on('mouseup', 'nav', function() {
            wrapper.removeClass('drag');
            setTimeout(() => term.focus(), 0);
        });
        $(document).off('.terminal').on('mousemove.terminal', function(e) {
            if (location) {
                var rect = location.rect;
                if (wrapper.is('.docking')) {
                    var x = e.clientX - location.x;
                    var y = e.clientY - location.y;
                    geometry.top = rect.top + y;
                    geometry.left = rect.left + x;
                    update_geometry();
                } else {
                    container.height(rect.height + (location.y - e.clientY));
                }
            }
        }).on('mouseup.terminal', function() {
            location = null;
            wrapper.removeClass('drag');
        });
        function init_geometry() {
            var width = window.innerWidth;
            var height = window.innerHeight;
            var result = {
                width: Math.min(Math.min(width - 30, width / 2), 600),
                height: Math.min(height / 2, 400)
            };
            result.left = (width - result.width) / 2;
            result.top = (height - result.height) / 2;
            return result;
        }
        function update_geometry() {
            wrapper.css({
                '--width': geometry.width,
                '--height': geometry.height,
                '--top': geometry.top,
                '--left': geometry.left
            });
        }
        var geometry = init_geometry();
        update_geometry();
        $('<span class="shell-dock"></span>').click(function() {
            wrapper.toggleClass('docking');
            return false;
        }).appendTo(nav);
        $('<span class="shell-destroy">[x]</span>').click(function() {
            term.destroy();
            wrapper.remove();
            return false;
        }).appendTo(nav);
        var term = terminal({
            selector: $('<div class="lips">').appendTo(container),
            name: 'lips',
            lips
        });
        if (typeof lips.env.get('write', { throwError: false }) === 'undefined') {
            var path = `https://cdn.jsdelivr.net/gh/jcubic/lips@${REF}/`;
            term.exec([
                '(let ((e lips.env.__parent__))',
                '  (load "' + path + 'dist/std.xcb" e))'
            ].join('\n'), true);
        }
        function format_baner(banner) {
            return banner.replace(/^[\s\S]+(LIPS.*\nCopy.*\n)[\s\S]*/, '$1')
                .replace(/(Jakub T. Jankiewicz)/, '[[!;;;;https://jcubic.pl/me]$1]');
        }
        term.echo(format_baner(lips.banner), { formatters: false });
        next();
    }
    var LIPS = `gh/jcubic/lips@${REF}`;
    var cdn = 'https://cdn.jsdelivr.net';
    [
        `${cdn}/gh/jcubic/jquery.terminal@devel/css/jquery.terminal.min.css`,
        `${cdn}/${LIPS}/lib/css/terminal.css`,
        `${cdn}/npm/terminal-prism/css/prism-coy.css`
    ].forEach(function(url) {
        if (!$('link[href="' + url + '"]').length) {
            var link = $('<link href="' + url + '" rel="stylesheet"/>');
            var head = $('head');
            if (head.length) {
                link.appendTo(head);
            } else {
                link.appendTo('body');
            }
        }
    });
    (window.Prism = window.Prism || {}).manual = true;
    if (typeof $.terminal !== 'undefined') {
        init();
    } else {
        var scripts = [
            `${cdn}/npm/prismjs/prism.js`,
            [
                `${cdn}/combine/npm/jquery.terminal/js/jquery.terminal.min.js`,
                'npm/jquery.terminal/js/prism.js',
                'npm/prismjs/components/prism-scheme.min.js',
                `${LIPS}/lib/js/terminal.js`,
                `${LIPS}/lib/js/prism.js`,
                'npm/js-polyfills/keyboard.js'
            ].join(','),
            `${cdn}/npm/browserfs@1.x.x/dist/browserfs.min.js`
        ];
        (function recur() {
            var script = scripts.shift();
            if (!script) {
                if (window.lips) {
                    init();
                } else {
                    var url = `${cdn}/${LIPS}/dist/lips.min.js`;
                    $.getScript(url, init);
                }
            } else if (script.match(/prism.js$/) &&
                       typeof Prism.languages !== 'undefined') {
                recur();
            } else {
                $.getScript(script, recur);
            }
        })();
    }
});
