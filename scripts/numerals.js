#!/usr/bin/env node

const http = require('https');
const cheerio = require('cheerio');
const fs = require('fs');
// when debuggin the script you can download the html so it's faster
// and don't potentially block your IP, by defense mechanism against
// lot of request when developing the code
const DEBUG = false;

function fetch(url) {
    url = new URL(url);
    return new Promise((resolve, reject) => {
        if (DEBUG) {
            fs.readFile('tmp.html', function(err, data) {
                if (err) {
                    reject(err);
                } else {
                    resolve(data.toString());
                }
            });
            return;
        }
        const req = http.get(url, function(res) {
            const bodyChunks = [];
            res.on('data', function(chunk) {
                bodyChunks.push(chunk);
            }).on('end', function() {
                const body = Buffer.concat(bodyChunks);
                resolve(body.toString());
            });
        });
        req.on('error', reject);
    });
}

fetch('https://www.fileformat.info/info/unicode/category/Nd/list.htm').then((body) => {
    const $ = cheerio.load(body);
    var zeros = $('.table-list tbody tr').map(function() {
        var tr = $(this);
        return {
            chr: tr.find('td').eq(0).text(),
            name: tr.find('td').eq(1).text()
        };
    }).get().filter(({name}) => {
        return name.match(/ZERO/);
    }).map(({chr}) => {
        return parseInt(chr.replace(/U+/, ''), 16).toString();
    });
    console.log(`(define *zero-number-chars* #(${zeros.join(' ')}))`);
});