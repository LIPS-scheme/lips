(function() {
    const url = 'https://cdn.jsdelivr.net/gh/jcubic/lips@devel/lib/js/bookmark.js';
    fetch(url).then(function(res) {
        return res.text();
    }).then(function(text) {
        const link = document.querySelector('#bookmark_link');
        link.setAttribute('href', text);
    });
})();
