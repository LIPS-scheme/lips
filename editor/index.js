require('expose-loader?CodeMirror!codemirror/lib/codemirror');
require('expose-loader?jQuery!jquery');
require('jquery.terminal');
require('jquery.splitter');
require('jquery.splitter/css/jquery.splitter.css');
require('expose-loader?Prism!prismjs');
require('prismjs/components/prism-scheme.min');
require('prismjs/themes/prism-coy.css');
require('jquery.terminal/js/prism');
require('expose-loader?git!isomorphic-git');
//require('expose-loader?LightningFS!../../lightning-fs');
require('expose-loader?fs!./lightning-fs.min.js');
require('jquery.terminal/css/jquery.terminal.min.css');
require("codemirror/lib/codemirror.css");
require("codemirror/addon/edit/matchbrackets");
['css', 'xml', 'javascript', 'htmlmixed', 'scheme'].forEach(function(mode) {
    require(`codemirror/mode/${mode}/${mode}`);
});
require("codemirror/theme/twilight.css");

