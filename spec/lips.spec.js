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
