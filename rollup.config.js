import commonjs from "rollup-plugin-commonjs";
import nodeResolve from "rollup-plugin-node-resolve";
import babel from "rollup-plugin-babel";

export default {
    input: "src/lips.js",
    output: {
        name: "lib",
        file: "dist/lips.js",
        format: "iife",
        globals: {
            "@babel/runtime/regenerator": "regeneratorRuntime"
        }
    },
    onwarn: (warning, next) => {
        const str = warning.toString();
        if (/Use of eval is strongly discouraged/.test(str)) {
            return;
        }
        next(warning);
    },
    plugins: [
        babel({
            "babelrc": false,
            "runtimeHelpers": true,
            "plugins": [
                "@babel/plugin-transform-async-to-generator",
                "@babel/plugin-transform-regenerator",
                ["@babel/plugin-transform-runtime", {
                    "helpers": true,
                    "regenerator": true
                }]
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
