/**@license
 *   ___ ___ _____  __      __   _      _____              _           _
 *  / __|_ _|_   _| \ \    / /__| |__  |_   _|__ _ _ _ __ (_)_ _  __ _| |
 * | (_ || |  | |    \ \/\/ / -_) '_ \   | |/ -_) '_| '  \| | ' \/ _` | |
 *  \___|___| |_|     \_/\_/\___|_.__/   |_|\___|_| |_|_|_|_|_||_\__,_|_|
 *
 * this is service worker and it's part of GIT Web terminal
 *
 * Copyright (c) 2018 Jakub Jankiewicz <http://jcubic.pl/me>
 * Released under the MIT license
 *
 */
/* global BrowserFS, Response, setTimeout, fetch, Blob, Headers */
//self.importScripts('https://cdn.jsdelivr.net/npm/browserfs');
self.importScripts('./lightning-fs.min.js');
//https://unpkg.com/@isomorphic-git/lightning-fs@3.2.0/dist/lightning-fs.min.js');

self.addEventListener('install', self.skipWaiting);

self.addEventListener('activate', self.skipWaiting);

self.addEventListener('fetch', function (event) {
    let fs = Promise.resolve(new LightningFS('fs'));
    event.respondWith(fs.then(function(fs) {
        return new Promise(function(resolve, reject) {
            function sendFile(path) {
                fs.readFile(path, function(err, buffer) {
                    if (err) {
                        err.fn = 'readFile(' + path + ')';
                        return reject(err);
                    }
                    var ext = path.replace(/.*\./, '');
                    var mime = {
                        'html': 'text/html',
                        'json': 'application/json',
                        'js': 'application/javascript',
                        'css': 'text/css'
                    };
                    var headers = new Headers({
                        'Content-Type': mime[ext]
                    });
                    resolve(new Response(buffer, {headers}));
                });
            }
            var url = event.request.url;
            var m = url.match(/__browserfs__(.*)/);
            function redirect_dir() {
                return resolve(Response.redirect(url + '/', 301));
            }
            function serve() {
                fs.stat(path, function(err, stat) {
                    if (err) {
                        return resolve(textResponse(error404Page(path)));
                    }
                    if (stat.isFile()) {
                        sendFile(path);
                    } else if (stat.isDirectory()) {
                        if (path.substr(-1, 1) !== '/') {
                            //return redirect_dir();
                        }
                        fs.readdir(path, function(err, list) {
                            if (err) {
                                err.fn = 'readdir(' + path + ')';
                                return reject(err);
                            }
                            var len = list.length;
                            if (list.includes('index.html')) {
                                sendFile(path + '/index.html');
                            } else {
                                listDirectory({fs, path, list}).then(function(list) {
                                    resolve(textResponse(fileListingPage(path, list)));
                                }).catch(reject);
                            }
                        });
                    }
                });
            }
            if (m) {
                var path = m[1];
                if (path === '') {
                    return redirect_dir();
                }
                console.log('serving ' + path + ' from browserfs');
                serve();
            } else {
                if (event.request.cache === 'only-if-cached' && event.request.mode !== 'same-origin') {
                    return;
                }
                //request = credentials: 'include'
                fetch(event.request).then(resolve).catch(reject);
            }
        });
    }));
});
// -----------------------------------------------------------------------------
function listDirectory({fs, path, list}) {
    return new Promise(function(resolve, reject) {
        var items = [];
        (function loop() {
            var item = list.shift();
            if (!item) {
                return resolve(items);
            }
            fs.stat(path + '/' + item, function(err, stat) {
                if (err) {
                    err.fn = 'stat(' + path + '/' + item + ')';
                    return reject(err);
                }
                items.push(stat.isDirectory() ? item + '/' : item);
                loop();
            });
        })();
    });
}

// -----------------------------------------------------------------------------
function textResponse(string, filename) {
    var blob = new Blob([string], {
        type: 'text/html'
    });
    return new Response(blob);
}

// -----------------------------------------------------------------------------
function fileListingPage(path, list) {
    var output = [
        '<!DOCTYPE html>',
        '<html>',
        '<body>',
        `<h1>BrowserFS ${path}</h1>`,
        '<ul>'
    ];
    if (path.match(/^\/(.*\/)/)) {
        output.push('<li><a href="..">..</a></li>');
    }
    list.forEach(function(name) {
        output.push('<li><a href="./' + name + '">' + name + '</a></li>');
    });
    output = output.concat(['</ul>', '</body>', '</html>']);
    return output.join('\n');
}

// -----------------------------------------------------------------------------
function error404Page(path) {
    var output = [
        '<!DOCTYPE html>',
        '<html>',
        '<body>',
        '<h1>404 File Not Found</h1>',
        `<p>File ${path} not found in browserfs`,
        '</body>',
        '</html>'
    ];
    return output.join('\n');
}
