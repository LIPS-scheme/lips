#!/usr/bin/env node

var fs = require('fs');

const readFile = fs.promises.readFile;
const writeFile = fs.promises.writeFile;

Promise.all([readFile('head.html'), readFile('footer.html')]).then(([head, footer]) => {
  head = head.toString();
  footer = footer.toString();
  ['index', 'docs'].forEach(file => {
    readFile(`${file}.tmpl`).then(content => {
        content = content.toString()
            .replace(/\{\{HEAD\}\}/, head)
            .replace(/\{\{FOOTER\}\}/, footer);
        writeFile(`${file}.html`, content);
    });
  });
});
