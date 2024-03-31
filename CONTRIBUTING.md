## Contributing to LIPS

First off, thank you to consider contributing to LIPS!
Few things to consider when contributing:

* JavaScript code that can be modified is located in lib/js and src directory
  use ES6 ESNext code you can use ES6 classes (initial code was not using ES6 classes
  because I don't like them, but since then I started to use them more).
  You can use any new feature of the language.
* Writing Scheme code should not use named lets and recursion on lists (because LIPS
  as of now, don't have TCO).
* New Scheme functions that are not part of R5RS and R7RS specs should be first
  discussed in issue.
* If you want to know what function is missing execute `./scripts/todo.scm`.
* Spelling and grammar fixes in function and macros doc strings are welcome, the same
  for any place were there is written text.
* I will not accept any external dependencies (as for now only bn.js is used in browser)
  except for Node.js REPL where there can be additional packages used.
* Any breaking change should be first discussed in issue, but it probably will not be
  be merged unless the current behavior seems like bug.
* Any PRs should be added into devel branch.

## How to contribute
* first fork the repo, then you need to clone it and change into devel branch:

```bash
git clone git@github.com:<YOUR NAME>/lips.git
git checkout devel
```

Then you install dependencies:

```bash
cd lips
npm install
```

Building project is simple, just execute:

```bash
make
```

Make assume Unix like system with standard Unix tools like `sed`, `cp`, `rm`, `cat`.
Make use [Rollup](https://rollupjs.org/) and [Babel](https://babeljs.io/) for
creating [UMD](https://github.com/umdjs/umd) file in `./dist` directory.

## Unit tests

Unit Tests are written in
[AVA JavaScript framework](https://github.com/avajs/ava), but whole code is in
Scheme (`./tests/*.scm` files), except small bootstrap script in tests/test.js
file. Ava support async tests that's why it was chosen. Old
[Jest](https://jestjs.io/) tests (in `./spec/lips.spec.js`) are obsolete and are
only left as reference for creating same tests in Scheme.

To run tests execute:

```bash
make test
```

when tests are failing and single test is the problem you can speed it up,
but running single test file after fixing the issue:

```
make test-file FILE=ports.scm
```

This will run single Scheme test file.

If your code don't pass tests it will be not merged. You can run tests locally,
but they will also run in CI (by [Travis](https://travis-ci.org/)).

## ESLint

LIPS use [ESLint](https://eslint.org/) to check the code syntax. If your code
fail linter it will also not be merged. Linter is also executed by Travis.  To
run ESLint execute:

```bash
make lint
```

## Codepell

LIPS use [Codespell](https://github.com/codespell-project/codespell) to catch
grammar mistakes. It runs as GitHub Action. But you can test locally so you can
fix the type before it will give error from Action.

You can install Codespell using PIP (you need to have Python installed):

```bash
pip install codespell
```

The run:

```bash
make codespell
```
