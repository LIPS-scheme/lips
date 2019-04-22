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
       pattern: /\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)/g,
       greedy: true
   }
});
// symbols
Prism.languages.insertBefore('scheme', 'string', {
   symbol: {
       pattern: /'[^\s()]+/g,
       greedy: true
   }
});
// and define-macro as keyword (update original regex) don't need most of it
Prism.languages.scheme.keyword.pattern = /(\()(?:define(?:-syntax|-library|-values)?|(?:case-)?lambda|let(?:\*|rec)?(?:-values)?|else|if|cond|begin|delay(?:-force)?|parameterize|define-macro|guard|set!|(?:quasi-)?quote|syntax-rules)(?=[()\s])/;