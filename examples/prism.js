/**
 * This is file that extend scheme PrismJS sytnax to work better with LIPS
 * This file is part of LIPS interpreter
 * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl>
 *
 */
/* global Prism */

// extend scheme syntax in PrismJS to inlcude regular expressions
Prism.languages.insertBefore('scheme', 'character', {
   regex: {
       pattern: /\/(?! )[^\n\/\\]*(?:\\[\S\s][^\n\/\\]*)*\/[gimy]*(?=\s|\(|\)|\]|\[|$)/g,
       greedy: true
   }
});
// symbols
Prism.languages.insertBefore('scheme', 'string', {
   symbol: {
       pattern: /'[^\s()\[\]]+/g,
       greedy: true
   }
});
// and define-macro as keyword (update original regex) don't need most of it
//Prism.languages.scheme.keyword.pattern = new RegExp(Prism.languages.scheme.keyword.pattern.source.replace(/\)\(\?\=\[\(\)\\\s\]\)/g, 'let-env|define-macro|try|catch)(?=[()\s])'));


Prism.languages.scheme.keyword.pattern = /(\()(?:define(?:-syntax|-library|-values)?|(?:case-)?lambda|let(?:(?:\*|rec)?(?:-values)?|-syntax|rec-syntax)|else|if|cond|begin|delay(?:-force)?|parameterize|guard|set!|(?:quasi-)?quote|syntax-(?:case|rules)|let-env|try|catch|throw|define-macro)(?=[()\s]|$)/;
