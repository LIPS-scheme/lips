/*
 * Boostrap tests written in Scheme using AVA testing framework
 *
 * This file is part of the LIPS - Scheme based Powerful lips in JavaScript
 *
 * Copyright (c) 2018-2020 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 */

// without this tests stop before running LIPS files
const ava = require('ava');

const {promisify} = require('util');
const fs = require('fs');
const readFile = promisify(fs.readFile);
const readDir = promisify(fs.readdir);

const lips = require('./src/lips');

readDir('./tests/').then(function(filenames) {
  return Promise.all(filenames.filter(function(file) {
      return file.match(/.scm$/) && !file.match(/^\.#|^_/);
  }).map(function(file) {
    return readFile(`tests/${file}`).then(d => d.toString());
  })).then(async function (files) {
      await lips.exec(`
        (let-env lips.env.parent
          (load "./lib/bootstrap.scm")
          (load "./lib/R5RS.scm")
          (load "./lib/R7RS.scm")
          (load "./tests/helpers/helpers.scm"))
      `);
      return lips.exec([`
      (define test (require "ava"))
      `].concat(files).join('\n\n')).catch((e) => {
          console.log(e);
      });
  });
});

