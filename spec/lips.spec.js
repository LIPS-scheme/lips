/* global require, describe, it, expect, jest */

var lips = require('../src/lips');


var {
    parse,
    tokenize,
    evaluate,
    Pair,
    Symbol,
    nil,
    Environment,
    global_environment
} = lips;

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
});
describe('parser', function() {
    it('should create Pair for simple list', function() {
        var tokens = tokenize('(foo bar baz)');
        var array = parse(tokens);
        expect(array.length).toBe(1);
        expect(array[0]).toEqual(
            new Pair(
                new Symbol('foo'),
                new Pair(
                    new Symbol('bar'),
                    new Pair(
                        new Symbol('baz'),
                        nil
                    )
                )
            )
        );
    });
    it('should create regular expressions numbers and strings', function() {
        var tokens = tokenize('(foo /( \\/)/g "bar baz" 10 1.1 10e2)');
        var array = parse(tokens);
        expect(array[0]).toEqual(
            new Pair(
                new Symbol('foo'),
                new Pair(
                    /( \/)/g,
                    new Pair(
                        'bar baz',
                        new Pair(
                            10,
                            new Pair(
                                1.1,
                                new Pair(
                                    10e2,
                                    nil
                                )
                            )
                        )
                    )
                )
            )
        );
    });
    it('should create AList', function() {
        var tokens = tokenize('((foo . 10) (bar . 20) (baz . 30))');
        var array = parse(tokens);
        expect(array[0]).toEqual(
            new Pair(
                new Pair(
                    new Symbol('foo'),
                    10
                ),
                new Pair(
                    new Pair(
                        new Symbol('bar'),
                        20
                    ),
                    new Pair(
                        new Pair(
                            new Symbol('baz'),
                            30
                        ),
                        nil
                    )
                )
            )
        );
    });
});

describe('Pair', function() {
    const pairs = new Pair(
        1,
        new Pair(
            2,
            new Pair(
                new Pair(
                    3,
                    new Pair(
                        4,
                        nil)
                ),
                new Pair(
                    new Pair(
                        5,
                        new Pair(
                            6,
                            nil
                        )
                    ),
                    nil
                )
            )
        )
    );
    const array = [1, 2, [3, 4], [5, 6]];
    it('should create Pair structure from array', function() {
        expect(Pair.fromArray(array)).toEqual(pairs);
    });
    it('should create Array from list structure', function() {
        expect(pairs.toArray()).toEqual(array);
    });
    it('should return same array', function() {
        var array = [[1], 2, [3, 4], [5, [1, [2, 3]], [1, 2]]];
        expect(Pair.fromArray(array).toArray()).toEqual(array);
    });
});

function exec(string, env) {
    return evaluate(parse(tokenize(string))[0], env);
}

describe('evaluate', function() {
    const rand = Math.random();
    const env = new Environment({
        value: rand,
        fun: (a, b) => a + b,
        f2: (a, b) => new Pair(a, new Pair(b, nil))
    }, global_environment);
    it('should return value', function() {
        expect(exec('value', env)).toEqual(rand);
    });
    it('should call function', function() {
        expect(exec('(fun 1 2)', env)).toEqual(3);
        expect(exec('(fun "foo" "bar")', env)).toEqual("foobar");
    });
    it('should set environment', function() {
        exec('(define x "foobar")', env);
        expect(exec('x', env)).toEqual("foobar");
        expect(exec('x')).toEqual(undefined);
    });
    it('should create list', function() {
        expect(exec('(cons 1 (cons 2 (cons 3 nil)))'))
            .toEqual(Pair.fromArray([1, 2, 3]));
    });
    describe('quote', function() {
        it('should return literal list', function() {
            expect(exec(`'(1 2 3 (4 5))`)).toEqual(
                Pair.fromArray([1, 2, 3, [4, 5]])
            );
        });
        it('should return alist', function() {
            expect(exec(`'((foo . 1)
                           (bar . 2.1)
                           (baz . "string")
                           (quux . /foo./g))`)).toEqual(
                new Pair(
                    new Pair(
                        new Symbol('foo'),
                        1
                    ),
                    new Pair(
                        new Pair(
                            new Symbol('bar'),
                            2.1
                        ),
                        new Pair(
                            new Pair(
                                new Symbol('baz'),
                                "string"
                            ),
                            new Pair(
                                new Pair(
                                    new Symbol('quux'),
                                    /foo./g
                                ),
                                nil
                            )
                        )
                    )
                )
            );
        });
    });
    describe('quasiquote', function() {
        it('should create list with function call', function() {
            expect(exec('`(1 2 3 ,(fun 2 2) 5)', env)).toEqual(
                Pair.fromArray([1, 2, 3, 4, 5])
            );
        });
        it('should create list with value', function() {
            expect(exec('`(1 2 3 ,value 4)', env)).toEqual(
                Pair.fromArray([1, 2, 3, rand, 4])
            );
        });
        it('should create single list using uquote-splice', function() {
            expect(exec('`(1 2 3 ,@(f2 4 5) 6)', env)).toEqual(
                Pair.fromArray([1, 2, 3, 4, 5, 6])
            );
        });
        it('should create single pair', function() {
            [
                '`(1 . 2)',
                '`(,(car (list 1 2 3)) . 2)',
                '`(1 . ,(cadr (list 1 2 3)))',
                '`(,(car (list 1 2 3)) . ,(cadr (list 1 2 3)))'
            ].forEach((code) => {
                expect(exec(code)).toEqual(new Pair(1, 2));
            });
        });
        it('should create list from pair syntax', function() {
            expect(exec('`(,(car (list 1 2 3)) . (1 2 3))')).toEqual(
                Pair.fromArray([1, 1, 2, 3])
            );
        });
        it('should create alist with values', function() {
            expect(exec(`\`((1 . ,(car (list 1 2)))
                            (2 . ,(cadr (list 1 "foo"))))`))
                .toEqual(
                    new Pair(
                        new Pair(1, 1),
                        new Pair(new Pair(2, "foo"), nil))
                );
            expect(exec(`\`((,(car (list "foo")) . ,(car (list 1 2)))
                            (2 . ,(cadr (list 1 "foo"))))`))
                .toEqual(new Pair(
                    new Pair("foo", 1),
                    new Pair(
                        new Pair(2, "foo"),
                        nil
                    )));
        });
        it('should process nested backquote', function() {
            expect(exec('`(1 2 3 ,(cadr `(1 ,(+ "foo" "bar") 3)) 4)')).toEqual(
                Pair.fromArray([1, 2, 3, "foobar", 4])
            );
        });
        it('should process multiple backquote/unquote', function() {
            expect(exec('``(a ,,(+ 1 2) ,(+ 3 4))')).toEqual(
                Pair.fromArray([
                    new Symbol('quasiquote'),
                    [
                        new Symbol('a'),
                        [
                            new Symbol('unquote'),
                            3
                        ],
                        [
                            new Symbol('unquote'),
                            [
                                new Symbol('+'),
                                3,
                                4
                            ]
                        ]
                    ]
                ]));
        });
    });
});


/*

var code = parse(tokenize(`
    (print (cons 1 (cons 2 (cons 3 nil))))
    (print (list 1 2 3 4))
    (print (car (list 1 2 3)))
    (print (concat "hello" " " "world"))
    (print (append (list 1 2 3) (list 10)))
    (print nil)
    (define x 10)
    (print (* x x))
    (print (/ 1 2))
    (define l1 (list 1 2 3 4))
    (define l2 (append l1 (list 5 6)))
    (print l1)
    (print l2)
    (defmacro (foo code) \`(print ,(string (car code))))
    (foo (name baz))
    (print \`(,(car (list "a" "b" "c")) 2 3))
    (print \`(,@(list 1 2 3)))
    (print \`(1 2 3 ,@(list 4 5) 6))
    (defmacro (xxx code) \`(list 1 ,(car (cdr code)) 2))
    (print (xxx ("10" "20")))
    (if (== 10 20) (print "1 true") (print "1 false"))
    (if (== 10 10) (print "2 true") (print "2 false"))
    (print (concat "if " (if (== x 10) "10" "20")))
`));

(function() {
  var env = new Environment({}, global_environment);
  var c = parse(tokenize([
    "(define x '(1 2 3))",
    "(print x)",
    "(print `(1 2 ,@x 4 5))",
    "(print `(foo bar ,@x 4 5))"
  ].join(' ')));
  c.forEach(function(code) {
    console.log(code.toString());
    try {
      evaluate(code, env);
    } catch (e) {
      console.error(e.message);
    }
  })
})();
*/
