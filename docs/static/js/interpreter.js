$(() => {
$.terminal.syntax('scheme');
$('.term').empty();
var term = terminal({
    selector: '.term',
    dynamic: false,
    name: 'demo',
    lips: lips
});
term.tooltips($('.example'), $node => $node.text(), function($node) {
    const offset = $node.offset();
    const top = offset.top;
    const left = offset.left;
    const right = offset.left + $node.width();
    return { top, right, left };
});

const intro = `LIPS version ${lips.version}
Copyright (c) 2018-${new Date().getFullYear()} [[!;;;;https://jcubic.pl/me]Jakub T. Jankiewicz]

Type (env) to see environment with functions macros and variables
You can also use (help obj)to display help for specic function or macro.
Use (dir name) to get list of JavaScript properties and (apropos name)
to display list of matched names in environment.`

term.echo(intro.replace(/(\((?:env|dir|help|apropos)[^)]*\))/g, function(code) {
    return $.terminal.prism('scheme', code, { echo: true });
}), {
    formatters: false
});
function pretty(code, options = {}) {
    const formatter = new lips.Formatter(code.toString(true));
    code = formatter.break().format(options);
    return $.terminal.prism('scheme', code, { echo: true });
}

$('.examples .run').on('click', function() {
    const code = $('.example:visible').text();
    term.echo(term.get_prompt(), { formatters: false });
    term.exec(code, true);
    setTimeout(() => term.focus(), 0);
});

function make_active($list, index) {
    $list.eq(index).addClass('active').siblings().removeClass('active');
}

const $examples = $('.examples .list li');
const $pagination = $('.examples .pagination');
const $pages = $pagination.find('li');
$examples.eq(0).addClass('active');
$pages.eq(0).addClass('active');
$pagination.on('click', 'li:not(.active)', function(e) {
    const self = $(this);
    const index = self.index();
    make_active($examples, index);
    make_active($pages, index);
    return false;
});

function highlight(code) {
    return Prism.highlight(code, Prism.languages.scheme, 'scheme');
}

const change = (() => {
    let size = 1;
    return function change(inc) {
        size += inc;
        term.css('--size', size);
    }
})();

$('.actions .zoom-in').click(function() {
    change(0.1);
});

$('.actions .zoom-out').click(function() {
    change(-0.1);
});

$('.actions > li .full-screen').click(function() {
    $(document.body).addClass('full-screen');
});

$('.actions .exit-full-screen').click(function() {
    $(document.body).removeClass('full-screen');
});
});
