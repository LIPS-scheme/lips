#!/usr/bin/env node

var {exec} = require('../src/lips');
var fs = require('fs');
global.$ = function() {
    return {
        css: function(...args) {
            console.log('css');
            console.log(args);
            return this;
        },
        on: function(...args) {
            console.log('on');
            console.log(args);
            return this;
        }
    };
};

if (process.argv.length > 2) {
    process.argv.slice(2).forEach(function(filename) {
        fs.readFile(filename, function(err, data) {
            if (err) {
                console.error(err);
            } else {
                exec(data.toString()).catch(function(e) {
                    console.error(e.message);
                    console.error(e.stack);
                });
            }
        });
    });
}
