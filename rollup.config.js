import commonjs from "rollup-plugin-commonjs";
import nodeResolve from "rollup-plugin-node-resolve";
import babel from "rollup-plugin-babel";

export default {
    input: "src/lips.js",
    output: {
        name: "lib",
        file: "dist/lips.js",
        format: "iife"
    },
    plugins: [
        babel({
            "babelrc": false,
            "runtimeHelpers": true,
            "plugins": [
                "@babel/plugin-transform-async-to-generator",
                "@babel/plugin-transform-regenerator"
            ],
            "presets": [
                "@babel/preset-env"
            ],
            "exclude": "node_modules/**"
        }),
        nodeResolve({
            mainFields: ["jsnext:main"]
        }),
        commonjs({
            include: "node_modules/**"
        })
    ]
};
