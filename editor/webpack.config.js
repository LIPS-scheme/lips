const path = require('path');
const webpack = require('webpack');

module.exports = {
    mode: "production",
    entry: {
        'a': "./src/a.js",
        'b': "./src/b.js"
    },
    output: {
        filename: 'bundle-[name].js',
        path: path.resolve(__dirname)
    },
    module: {
        rules: [
            {
                test: () => true,
                sideEffects: true
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            }
        ]
    }
};
