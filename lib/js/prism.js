/**
 * This is file that extend scheme PrismJS sytnax to work better with LIPS
 * This file is part of LIPS interpreter
 * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl>
 *
 * based on 1.22.0
 *
 */
/* global Prism */

// extend scheme syntax in PrismJS to inlcude regular expressions
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
