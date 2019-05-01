const path = require('path');
module.exports = {
    mode: "production",
    entry: "./index.js",
    output: {
        filename: "bundle.js",
        path: path.resolve(__dirname)
        //libraryTarget: "umd"
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
