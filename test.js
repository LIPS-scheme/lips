const ava = require('ava');

const {promisify} = require('util');
const fs = require('fs');
const readFile = promisify(fs.readFile);
const readDir = promisify(fs.readdir);

const lips = require('./src/lips');

readDir('./tests/').then(function(filenames) {
  
  return Promise.all(filenames.filter(function(file) {
      return file.match(/.scm$/) && !file.match(/^\.#/);
  }).map(function(file) {
    return readFile(`tests/${file}`).then(d => d.toString());
  })).then(function (files) {
      return lips.exec([`
      (define test (require "ava"))

      (load "./lib/bootstrap.scm")
      (load "./lib/R5RS.scm")
      (load "./examples/helpers.scm")
      (load "./tests/helpers/helpers.scm")
      `
      ].concat(files).join('\n\n')).catch((e) => {
          console.log(e);
      });
  });
});

