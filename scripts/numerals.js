#!/usr/bin/env node

const fs = require('fs');

function get_data() {
    return new Promise((resolve, reject) => {
        fs.readFile(`${__dirname}/../assets/UnicodeData.txt`, function(err, data) {
            if (err) {
                reject(err);
            } else {
                resolve(data.toString());
            }
        });
    });
}
var numbers;
get_data().then((body) => {
    const zeros = body.split('\n').filter(line => line.match(/ZERO;Nd;/)).map(line => {
      const parts = line.split(';');
      return parseInt(parts[0], 16).toString();
    });
    console.log(`(define *zero-number-chars* #(${zeros.join(' ')}))`);
});