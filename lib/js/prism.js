/**
 * This is file that extend scheme PrismJS syntax to work better with LIPS
 * This file is part of LIPS interpreter
 * Copyright (c) Jakub T. Jankiewicz <https://jcubic.pl/me>
 *
 * based on 1.22.0
 *
 */
/* global Prism */

// extend scheme syntax in PrismJS to include regular expressions
// (it's modification of JavaScript regex, only look behind was modified)
Prism.languages.insertBefore('scheme', 'string', {
   'regex': {
       pattern: /(^|[()[\]\s])#\/((?:\\\/|[^/]|\[[^\]]*\/[^\]]*\])+)\/([gimyus]*)(?=$|[()[\]\s])/g,
       lookbehind: true,
       greedy: true
   }
});
// symbols
Prism.languages.insertBefore('scheme', 'string', {
   symbol: {
       pattern: /'(?:[^\s()\[\]]+|\|[^|]+\|)/g,
       greedy: true
   }
});
// modification of PrismJS JavaScript string interpolation
Prism.languages.insertBefore('scheme', 'string', {
    'template-string': {
        pattern: /#"(?:\\[\s\S]|\$\{(?:[^{}]|\{(?:[^{}]|\{[^}]*\})*\})+\}|(?!\$\{)[^\\"])*"/,
        greedy: true,
        inside: {
            'template-punctuation': {
                pattern: /^#"|"$/,
                alias: 'string'
            },
            'interpolation': {
                pattern: /((?:^|[^\\])(?:\\{2})*)\$\{(?:[^{}]|\{(?:[^{}]|\{[^}]*\})*\})+\}/,
                lookbehind: true,
                inside: {
                    'interpolation-punctuation': {
                        pattern: /^\$\{|\}$/,
                        alias: 'punctuation'
                    },
                    rest: Prism.languages.scheme
                }
            },
            'string': /[\s\S]+/
        }
    }
});
// properties of object literals
Prism.languages.insertBefore('scheme', 'string', {
    variable: {
        pattern: /:[^\s]+(?=\s)/g,
        greedy: true
    }
});
// allow to have non closed strings
Prism.languages.scheme.string.pattern = /"(?:\\[\S\s]|[^"])*"?/g;

// add LIPS specific keywords
var keyword = Prism.languages.scheme.keyword.pattern;
Prism.languages.scheme.keyword.pattern = new RegExp(keyword.source.replace(/\|when/, '|when|set-obj!|let-env|try|catch|throw|raise'));
// names should be marked so we can show help message on higher order function that have function as argument
Prism.languages.scheme.name = {
    pattern: /(^|[()[\]\s])[^#()[\]\s][^()\s]+/g,
    lookbehind: true
};
