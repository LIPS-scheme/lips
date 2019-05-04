require('expose-loader?CodeMirror!codemirror/lib/codemirror');
//require('expose-loader?LightningFS!../../lightning-fs');
require('expose-loader?LightningFS!../lightning-fs.min.js');
require("codemirror/lib/codemirror.css");
require('codemirror/addon/scroll/simplescrollbars');
require('codemirror/addon/scroll/simplescrollbars.css');
require("codemirror/addon/edit/matchbrackets");
['css', 'xml', 'javascript', 'htmlmixed', 'scheme'].forEach(function(mode) {
    require(`codemirror/mode/${mode}/${mode}`);
});
require("codemirror/theme/twilight.css");

