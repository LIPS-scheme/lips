/*
 * Bootstrap tests written in Scheme using AVA testing framework
 *
 * This file is part of the LIPS - Scheme based Powerful lips in JavaScript
 *
 * Copyright (c) 2018-2020 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 */

// without this tests stop before running LIPS files
import ava  from 'ava';
import lily from '@jcubic/lily';
import { promisify } from 'util';
import fs from 'fs';
import util from 'util';

import lips from '../src/lips.js';

const readDir = promisify(fs.readdir);
const readFile = promisify(fs.readFile);


async function get_files() {
    const options = lily(process.argv.slice(2));
    if (options.f) {
        return [options.f];
    }
    var files = await readDir('./tests/');
    return files.filter(function(file) {
        return file.match(/.scm$/) && !file.match(/^\.#|^_/);
    });
}

get_files().then(filenames => {
    return Promise.all(filenames.map(function(file) {
        return readFile(`tests/${file}`).then(d => d.toString());
    })).then(async function (files) {
      await lips.exec(`
        (let-env lips.env.__parent__
          (load "./dist/std.xcb")
          (load "./tests/helpers/helpers.scm"))
      `);
      return lips.exec([`
      (define test (require "ava"))
      `].concat(files).join('\n\n')).catch((e) => {
          console.log(e);
      });
  }).catch(e => {
      console.error(e.message);
      console.error(e.stack);
  });
});

