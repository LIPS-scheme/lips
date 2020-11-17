var lips = require('../dist/lips');

var {
    parse,
    tokenize,
    evaluate,
    Pair,
    LSymbol,
    nil,
    Environment,
    env: global_environment,
    LNumber,
    LString,
    LFloat,
    quote
} = lips;

var deps = lips.exec('(load "./lib/bootstrap.scm") (load "./examples/helpers.scm")');

describe('tokenizer', function() {
    it('should create tokens for simple list', function() {
        expect(tokenize('(foo bar baz)')).toEqual(['(', 'foo', 'bar', 'baz', ')']);
    });
    it('should create tokens for numbers string and regexes', function() {
        expect(tokenize('(foo /( \\/)/g "bar baz" 10 1.1 10e2)')).toEqual([
            '(', 'foo', '/( \\/)/g', '"bar baz"', '10', '1.1', '10e2', ')'
        ]);
    });
    it('should create tokens for alists', function() {
        expect(tokenize('((foo . 10) (bar . 20) (baz . 30))')).toEqual([
            '(', '(', 'foo', '.', '10', ')', '(', 'bar', '.', '20', ')', '(',
            'baz', '.', '30', ')', ')'
        ]);
    });
    it('should ignore comments', function() {
        expect(tokenize('(foo bar baz); (baz quux)')).toEqual([
            '(', 'foo', 'bar', 'baz', ')'
        ]);
    });
    it('should handle semicolon in regexes and strings', function() {
        expect(tokenize('(";()" /;;;/g baz); (baz quux)')).toEqual([
            '(', '";()"', '/;;;/g', 'baz', ')'
        ]);
    });
    it('should tokenize with data', function() {
        var code = `(define-macro (defstruct name . fields)
  "First Line." "word" 10
  "Second Line."    "word" /regex/
  "Third Line."     "word"
  (let ((names (map (lambda (symbol) (gensym)) fields))
        (struct (gensym))
        (field-arg (gensym)))
`;
        var output = [
            { col: 0, line: 0, token: '(', offset: 0 },
            { col: 1, line: 0, token: 'define-macro', offset: 1 },
            { col: 13, line: 0, token: ' ', offset: 13 },
            { col: 14, line: 0, token: '(', offset: 14 },
            { col: 15, line: 0, token: 'defstruct', offset: 15 },
            { col: 24, line: 0, token: ' ', offset: 24 },
            { col: 25, line: 0, token: 'name', offset: 25 },
            { col: 29, line: 0, token: ' ', offset: 29 },
            { col: 30, line: 0, token: '.', offset: 30 },
            { col: 31, line: 0, token: ' ', offset: 31 },
            { col: 32, line: 0, token: 'fields', offset: 32 },
            { col: 38, line: 0, token: ')', offset: 38 },
            { col: 39, line: 0, token: '\n', offset: 39 },
            { col: 0, line: 1, token: '  ', offset: 40 },
            { col: 2, line: 1, token: '"First Line."', offset: 42 },
            { col: 15, line: 1, token: ' ', offset: 55 },
            { col: 16, line: 1, token: '"word"', offset: 56 },
            { col: 22, line: 1, token: ' ', offset: 62 },
            { col: 23, line: 1, token: '10', offset: 63 },
            { col: 25, line: 1, token: '\n', offset: 65 },
            { col: 0, line: 2, token: '  ', offset: 66 },
            { col: 2, line: 2, token: '"Second Line."', offset: 68 },
            { col: 16, line: 2, token: '    ', offset: 82 },
            { col: 20, line: 2, token: '"word"', offset: 86 },
            { col: 26, line: 2, token: ' ', offset: 92 },
            { col: 27, line: 2, token: '/regex/', offset: 93 },
            { col: 34, line: 2, token: '\n', offset: 100 },
            { col: 0, line: 3, token: '  ', offset: 101 },
            { col: 2, line: 3, token: '"Third Line."', offset: 103 },
            { col: 15, line: 3, token: '     ', offset: 116 },
            { col: 20, line: 3, token: '"word"', offset: 121 },
            { col: 26, line: 3, token: '\n', offset: 127 },
            { col: 0, line: 4, token: '  ', offset: 128 },
            { col: 2, line: 4, token: '(', offset: 130 },
            { col: 3, line: 4, token: 'let', offset: 131 },
            { col: 6, line: 4, token: ' ', offset: 134 },
            { col: 7, line: 4, token: '(', offset: 135 },
            { col: 8, line: 4, token: '(', offset: 136 },
            { col: 9, line: 4, token: 'names', offset: 137 },
            { col: 14, line: 4, token: ' ', offset: 142 },
            { col: 15, line: 4, token: '(', offset: 143 },
            { col: 16, line: 4, token: 'map', offset: 144 },
            { col: 19, line: 4, token: ' ', offset: 147 },
            { col: 20, line: 4, token: '(', offset: 148 },
            { col: 21, line: 4, token: 'lambda', offset: 149 },
            { col: 27, line: 4, token: ' ', offset: 155 },
            { col: 28, line: 4, token: '(', offset: 156 },
            { col: 29, line: 4, token: 'symbol', offset: 157 },
            { col: 35, line: 4, token: ')', offset: 163 },
            { col: 36, line: 4, token: ' ', offset: 164 },
            { col: 37, line: 4, token: '(', offset: 165 },
            { col: 38, line: 4, token: 'gensym', offset: 166 },
            { col: 44, line: 4, token: ')', offset: 172 },
            { col: 45, line: 4, token: ')', offset: 173 },
            { col: 46, line: 4, token: ' ', offset: 174 },
            { col: 47, line: 4, token: 'fields', offset: 175 },
            { col: 53, line: 4, token: ')', offset: 181 },
            { col: 54, line: 4, token: ')', offset: 182 },
            { col: 55, line: 4, token: '\n', offset: 183 },
            { col: 0, line: 5, token: '        ', offset: 184 },
            { col: 8, line: 5, token: '(', offset: 192 },
            { col: 9, line: 5, token: 'struct', offset: 193 },
            { col: 15, line: 5, token: ' ', offset: 199 },
            { col: 16, line: 5, token: '(', offset: 200 },
            { col: 17, line: 5, token: 'gensym', offset: 201 },
            { col: 23, line: 5, token: ')', offset: 207 },
            { col: 24, line: 5, token: ')', offset: 208 },
            { col: 25, line: 5, token: '\n', offset: 209 },
            { col: 0, line: 6, token: '        ', offset: 210 },
            { col: 8, line: 6, token: '(', offset: 218 },
            { col: 9, line: 6, token: 'field-arg', offset: 219 },
            { col: 18, line: 6, token: ' ', offset: 228 },
            { col: 19, line: 6, token: '(', offset: 229 },
            { col: 20, line: 6, token: 'gensym', offset: 230 },
            { col: 26, line: 6, token: ')', offset: 236 },
            { col: 27, line: 6, token: ')', offset: 237 },
            { col: 28, line: 6, token: ')', offset: 238 },
            { col: 29, line: 6, token: '\n', offset: 239 }
        ];
        expect(tokenize(code, true).slice(0, output.length)).toEqual(output);
    });
});

function exec(string, env, dynamic_scope) {
    return evaluate(parse(tokenize(string))[0], { env, dynamic_scope });
}

function deepQuote(pair) {
    if (pair instanceof Pair) {
        quote(pair);
        deepQuote(pair.cdr);
        deepQuote(pair.car);
    }
    return pair;
}

describe('evaluate', function() {
    const rand = Math.random();
    const env = new Environment({
        value: rand,
        fun: function(a, b) {
            if (LNumber.isNumber(a)) {
                return LNumber(a).add(b);
            } else if (LString.isString(a)) {
                return a + b;
            }
        },
        f2: (a, b) => new Pair(a, new Pair(b, nil))
    }, global_environment, 'test');
    
    describe('parallel invocation', function() {
        it('should run async code sequentially', function() {
            var output = LNumber(30);
            function test(code, range) {
                var start = Date.now();
                return lips.exec(code).then(result => {
                    var time = Date.now() - start;
                    expect(result[0]).toEqual(output);
                    expect(time).not.toBeLessThan(range[0]);
                    expect(time).not.toBeGreaterThan(range[1]);
                });
            }
            return deps.then(function() {
                return Promise.all([
                    test('(begin (wait 300 10) (wait 300 20) (wait 300 30))', [
                        800, 1000
                    ]),
                    test('(begin* (wait 300 10) (wait 300 20) (wait 300 30))', [
                        300, 350
                    ])
                ]);
            });
        });
    });
});
describe('environment', function() {
    const env = global_environment;
    var functions = {
        scope_name: function() {
            if (this.name === '__frame__') {
                return this.parent.name;
            }
            return this.name;
        }
    };
    function scope(e) {
        return lips.exec('(scope_name)', e).then(result => result[0].valueOf());
    }
    it('should return name of the enviroment', function() {
        var e = env.inherit('foo', functions);
        return scope(e).then(result => {
            return expect(result).toEqual('foo');
        });
    });
    it('should create default scope name', function() {
        var e = env.inherit(functions);
        return scope(e).then(result => {
            return expect(result).toEqual('child of user-env');
        });
    });
    it('should create default scope name for child scope', function() {
        var e = env.inherit('foo', functions);
        var child = e.inherit();
        return scope(child).then(result => {
            return expect(result).toEqual('child of foo');
        });
    });
});
describe('scope', function() {
    const ge = global_environment;
    function exec(code, dynamic_scope) {
        var env = ge.inherit();
        return lips.exec(code, env, dynamic_scope ? env : undefined);
    }
    describe('lexical', function() {
        it('should evaluate let', function() {
            return exec(`(define x 10) (let ((x 10)) x)`).then((result) => {
                expect(result).toEqual([undefined, LNumber(10)]);
            });
        });
        it('should evaluate let over let', function() {
            var code = `(define x 10)
                        (let ((x 20)) (let ((x 30)) x))`;
            return exec(code).then(result => {
                expect(result).toEqual([undefined, LNumber(30)]);
            });
        });
        it('should evalute lambda', function() {
            var code = `(define x 10)
                        ((let ((x 20)) (lambda () x)))`;
            return exec(code).then((result) => {
                expect(result).toEqual([undefined, LNumber(20)]);
            });
        });
        it('sould create closure', function() {
            var code = `(define fn (let ((x 10))
                                      (let ((y 20)) (lambda () (+ x y)))))
                        (fn)`;
            return exec(code).then(result => {
                expect(result).toEqual([undefined, LNumber(30)]);
            });
        });
    });
    describe('dynamic', function() {
        it('should get value from let', function() {
            var code = `(define fn (lambda (x) (* x y)))
                        (let ((y 10)) (fn 20))`;
            return exec(code, true).then(result => {
                expect(result).toEqual([undefined, LNumber(200)]);
            });
        });
        it('should evalute simple lambda', function() {
            var code = `(define y 20)
                        (define (foo x) (* x y))
                        ((lambda (y) (foo 10)) 2)`;
            return exec(code, true).then(result => {
                expect(result).toEqual([undefined, undefined, LNumber(20)]);
            });
        });
        it('should evalute let over lambda', function() {
            var code = `(define y 10)
                        ((let ((y 2)) (lambda () y)))`;
            return exec(code, true).then(result => {
                expect(result).toEqual([undefined, LNumber(10)]);
            });
        });
    });
});
describe('docs', function() {
    it('all functions should have docs', function() {
        Object.keys(lips.env.env).forEach(key => {
            var value = lips.env.env[key];
            if (typeof value === 'function') {
                var __doc__ = value.__doc__;
                expect([key, typeof __doc__]).toEqual([key, 'string']);
                expect(__doc__.length).toBeGreaterThan(0);
            }
        });
    });
});
const str2list = code => lips.parse(lips.tokenize(code))[0];
describe('lists', function() {
    describe('append', function() {
        it('should append pair', function() {
            [
                ['(1 2 3)', '(1 2 3 10)'],
                ['((1 2 3))', '((1 2 3) 10)'],
                ['(1 2 (3) 4)', '(1 2 (3) 4 10)'],
                ['(1 2 3 (4))', '(1 2 3 (4) 10)']
            ].forEach(([code, expected]) => {
                var input = str2list(code);
                input.append(new Pair(LNumber(10), nil));
                expect(input).toEqual(str2list(expected));
            });
        });
        it('should append value', function() {
            [
                ['(1 2 3)', '(1 2 3 . 10)'],
                ['((1 2 3))', '((1 2 3) . 10)'],
                ['(1 2 (3) 4)', '(1 2 (3) 4 . 10)'],
                ['(1 2 3 (4))', '(1 2 3 (4) . 10)']
            ].forEach(([code, expected]) => {
                var input = str2list(code);
                input.append(LNumber(10));
                expect(input).toEqual(str2list(expected));
            });
        });
        it('should not append nil', function() {
            [
                '(1 2 3)',
                '((1 2 3))',
                '(1 2 (3) 4)',
                '(1 2 3 (4))',
                '(1 . 2)',
                '((1 . 2))'
            ].forEach((code) => {
                var input = str2list(code);
                input.append(lips.nil);
                expect(input).toEqual(str2list(code));
            });
        });
    });
});
