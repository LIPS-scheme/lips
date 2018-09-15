import commonjs from "rollup-plugin-commonjs";
import nodeResolve from "rollup-plugin-node-resolve";
import babel from "rollup-plugin-babel";
import replace from "rollup-plugin-replace";

export default {
    input: "src/lips.js",
    output: {
        name: "lib",
        file: "dist/lips.js",
        format: "iife"
    },
    plugins: [
        commonjs({
            include: "node_modules/**"
        }),
        nodeResolve({
          jsnext: true,
          main: false
        }),
        babel({
            "babelrc": false,
            "runtimeHelpers": true,
            "plugins": [
                "syntax-async-functions",
                "@babel/plugin-external-helpers",
                "@babel/plugin-transform-async-to-generator",
                ["@babel/plugin-transform-runtime", {
                    //"helpers": true,
                    "regenerator": true
                }]
            ],
            "presets": [
                "@babel/preset-env"
            ]
        }),
        // fix for a bug in Babel or rollup babel plugin rollup/rollup-plugin-babel#252
        replace({
            delimiters: ["", ""],
            "_typeof2(Symbol.iterator)": "typeof Symbol.iterator",
            "_typeof2(obj);": "typeof obj;"
        })
    ]
};
