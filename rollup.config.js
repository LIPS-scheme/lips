import commonjs from "@rollup/plugin-commonjs";
import nodeResolve from "@rollup/plugin-node-resolve";
import babel from "@rollup/plugin-babel";
import fs from 'node:fs';

const banner = fs.readFileSync('./dist/banner.js', 'utf8');

const base = {
  input: "dist/base.js",
  onwarn: (warning, next) => {
    const str = warning.toString();
    if (/Use of eval is strongly discouraged/.test(str)) {
      return;
    }
    next(warning);
  },
  plugins: [
    babel({
      babelrc: false,
      babelHelpers: 'runtime',
      "plugins": [
        "@babel/plugin-transform-async-to-generator",
        ["@babel/plugin-transform-runtime", {
          "helpers": true
        }],
      ],
      "presets": [
        "@babel/preset-env"
      ],
      "exclude": "node_modules/**"
    }),
    commonjs({
      include: "node_modules/**"
    }),
    nodeResolve({
      mainFields: ["jsnext:main"]
    })
  ]
};

export default [
    {
        output: {
            name: "lips",
            file: "dist/lips.js",
            format: "umd",
            banner,
            manualChunks: () => 'everything.js'
        },
        ...base
    },
    {
        output: {
            name: "lips",
            file: "dist/lips.cjs",
            format: "cjs",
            banner,
            manualChunks: () => 'everything.js'
        },
        ...base
    },
    {
        output: {
            name: "lips",
            file: "dist/lips.esm.js",
            format: "esm",
            banner,
            manualChunks: () => 'everything.js'
        },
        ...base
    }
];
