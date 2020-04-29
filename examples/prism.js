/**
 * This is file that extend scheme PrismJS sytnax to work better with LIPS
 * This file is part of LIPS interpreter
 * Copyright (C) Jakub T. Jankiewicz <https://jcubic.pl>
 *
 */
/* global Prism */

// extend scheme syntax in PrismJS to inlcude regular expressions
// (it's modification of JavaScript regex, only look behind was modified)
Prism.languages.insertBefore('scheme', 'string', {
   'regex': {
		  pattern: /(^|\s)\/(?:\[(?:[^\]\\\r\n]|\\.)*]|\\.|[^/\\\[\r\n])+\/[gimyus]{0,6}(?=(?:\s|\/\*(?:[^*]|\*(?!\/))*\*\/)*(?:$|[\r\n,.;:})\]]|\/\/))/,
		  lookbehind: true,
		  greedy: true
	  }
});

Prism.languages.scheme.function.pattern = /(\()[^()'\s]+(?=[()\s]|$)/;

// symbols
Prism.languages.insertBefore('scheme', 'string', {
   symbol: {
       pattern: /'(?:[^\s()\[\]]+|\|[^|]+\|)/g,
       greedy: true
   }
});
// and define-macro as keyword (update original regex) don't need most of it
//Prism.languages.scheme.keyword.pattern = new RegExp(Prism.languages.scheme.keyword.pattern.source.replace(/\)\(\?\=\[\(\)\\\s\]\)/g, 'let-env|define-macro|try|catch|throw)(?=[()\s])'));
Prism.languages.scheme.keyword.pattern = /(\()(?:define(?:-syntax|-library|-values)?|(?:case-)?lambda|let(?:(?:\*|rec)?(?:-values)?|-syntax|rec-syntax)|else|if|cond|begin|delay(?:-force)?|parameterize|guard|set!|(?:quasi-)?quote|syntax-(?:case|rules)|let-env|try|catch|throw|define-macro)(?=[()\s]|$)/;
