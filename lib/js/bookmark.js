javascript:(function(next) {
    /**
     * This is bookmarklet that will create terminal with LIPS REPL
     *
     * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl/me>
     * Released under MIT license
     */
    var orig_jQuery;
    var dolar;
    if (window.jQuery) {
        if (window.$ === window.jQuery) {
            dolar = true;
        }
        orig_jQuery = window.jQuery.noConflict(true);
    }
    function attr(elem, key, value) {
        elem.setAttribute(document.createAttribute(key, value));
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
    script('https://cdn.jsdelivr.net/npm/jquery');
    (function delay(time) {
        if (typeof jQuery == 'undefined') {
            setTimeout(delay, time);
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
        } catch(e) {
            return branch;
        }
    }
    const REF = await hash('master');
    function track() {
        /*
         * this is traking code using Matomo instance,
         * it don't save any personal information,
         * it's only used for statistics of usage.
         */
        try {
            var searchParams = new URLSearchParams({
                idsite: 7,
                rec: 1,
                action_name: 'bookmark',
                url: location.href
            });
            var url = new URL('https://piwik.jcubic.pl/matomo.php');
            url.search = searchParams.toString();
            var img = new Image();
            img.src = url.href;
        } catch (e) {}
    }
    function init() {
        var t = $('.terminal.lips');
        if (t.length) {
            t.each(function() {
                $(this).terminal().destroy().remove();
            });
        }
        $.terminal.defaults.linksNoReferrer = true;
        $.terminal.defaults.formatters = $.terminal.defaults.formatters.filter((x) => {
            return x.name !== 'syntax_scheme';
        });
        $.terminal.syntax("scheme");
        $('.shell-wrapper').remove();
        var wrapper = $('<div>').addClass('shell-wrapper').appendTo('body');
        var container = $('<div>').addClass('shell-container').appendTo(wrapper);
        var mask = $('<div class="shell-mask"/>').appendTo(wrapper);
        var nav = $('<nav/>').appendTo(container);
        var pos; $(document).off('mousemove');
        var height;
        $('nav').off('mousedown').mousedown(function(e) {
            height = container.height();
            pos = e.clientY;
            wrapper.addClass('drag');
            return false;
        });
        $(document).off('.terminal').on('mousemove.terminal', function(e) {
            if (pos) {
                container.height(height + (pos - e.clientY));
            }
        }).on('mouseup.terminal', function() {
            pos = null;
            wrapper.removeClass('drag');
        });
        $('<span class="shell-destroy">[x]</span>').click(function() {
            term.destroy();
            wrapper.remove();
        }).appendTo(nav);
        var term = terminal({ selector: $('<div class="lips">').appendTo(container), name: 'lips', lips });
        if (typeof lips.env.get('write', { throwError: false }) === 'undefined') {
            var path = `https://cdn.jsdelivr.net/gh/jcubic/lips@${REF}/`;
            term.exec([
                '(let ((e lips.env.__parent__))',
                  '(load "' + path + 'dist/std.min.scm" e))'
            ].join('\n'), true);
        }
        function format_baner(banner) {
            return banner.replace(/^[\s\S]+(LIPS.*\nCopy.*\n)[\s\S]*/, '$1')
                .replace(/(Jakub T. Jankiewicz)/, '[[!;;;;https://jcubic.pl/me]$1]');
        }
        term.echo(format_baner(lips.banner), {formatters: false});
        track();
        next();
    }
    [
        'https://cdn.jsdelivr.net/gh/jcubic/jquery.terminal/css/jquery.terminal.min.css',
        'https://cdn.jsdelivr.net/gh/jcubic/lips@devel/lib/css/terminal.css',
        'https://cdn.jsdelivr.net/gh/jcubic/terminal-prism/css/prism-coy.css'
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
    if (typeof $.terminal !== 'undefined') {
        init();
    } else {
        var scripts = [
            'https://cdn.jsdelivr.net/npm/prismjs/prism.js',
            [
                'https://cdn.jsdelivr.net/combine/npm/jquery.terminal',
                'npm/jquery.terminal/js/prism.js',
                'npm/prismjs/components/prism-scheme.min.js',
                `gh/jcubic/lips@${REF}/lib/js/terminal.js`,
                `gh/jcubic/lips@${REF}/lib/js/prism.js`,
                'npm/js-polyfills/keyboard.js'
            ].join(','),
            'https://cdn.jsdelivr.net/npm/browserfs@1.x.x/dist/browserfs.min.js'
        ];
        (function recur() {
            var script = scripts.shift();
            if (!script) {
                if (window.lips) {
                    init();
                } else {
                    $.getScript(`https://cdn.jsdelivr.net/gh/jcubic/lips@${REF}/dist/lips.min.js`, init);
                }
            } else if (script.match(/prism.js$/) && typeof Prism !== 'undefined') {
                recur();
            } else {
                $.getScript(script, recur);
            }
        })();
    }
});
