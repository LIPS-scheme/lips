(() => {
const box = document.querySelector('.box');
const code = document.querySelector('.box pre.hidden').innerText;
const container = document.querySelector('.cm-body');

CodeMirror.commands.autocomplete = function(cm) {
    cm.showHint({ hint: CodeMirror.hint.scheme });
};

const myCodeMirror = CodeMirror(container, {
    value: code,
    mode:  "lips",
    lineWrapping: true,
    lineNumbers: true,
    matchBrackets: true,
    theme: 'twilight',
    inputStyle: "contenteditable",
    extraKeys: { "Ctrl-Space": "autocomplete" }
});

function download(url) {
    const a = document.createElement('a');
    a.download = 'screenshot.png';
    a.href = url;
    a.style.display = 'none';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
}

const button = document.querySelector('.box button');
button.addEventListener('click', () => {
    const node = document.querySelector(".box .wrapper");
    htmlToImage.toPng(node)
        .then(download)
        .catch(function(error) {
            console.error('oops, something went wrong!', error);
        });
});
let height;
const body = document.body;

myCodeMirror.on('update', adjust_height);

function adjust_height() {
    var rect = container.getBoundingClientRect();
    if (height !== rect.height) {
        height = rect.height;
        body.style.setProperty('--height', rect.height + 300);
    }
}

})();
