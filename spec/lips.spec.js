/* global require, describe, it, expect */

var lips = require('../src/lips');

/* TODO
 *     test quasiquote
 *  (define (test)
 *       (let ((struct 'foo)
 *             (name 'bar))
 *         (print `(define (,(make-predicate name)  ,struct)))))
 */

var {
    parse,
    tokenize,
    evaluate,
    Pair,
    Symbol,
    nil,
    Environment,
    global_environment,
    LNumber,
    quote
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
                            LNumber(10),
                            new Pair(
                                LNumber(1.1),
                                new Pair(
                                    LNumber(10e2),
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
                    LNumber(10)
                ),
                new Pair(
                    new Pair(
                        new Symbol('bar'),
                        LNumber(20)
                    ),
                    new Pair(
                        new Pair(
                            new Symbol('baz'),
                            LNumber(30)
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
            } else if (typeof a === 'string') {
                return a + b;
            }
        },
        f2: (a, b) => new Pair(a, new Pair(b, nil))
    }, global_environment, 'test');
    it('should return value', function() {
        expect(exec('value', env)).toEqual(LNumber(rand));
    });
    it('should call function', function() {
        expect(exec('(fun 1 2)', env)).toEqual(LNumber(3));
        expect(exec('(fun "foo" "bar")', env)).toEqual("foobar");
    });
    it('should set environment', function() {
        exec('(define x "foobar")', env);
        expect(exec('x', env)).toEqual("foobar");
        expect(exec('x')).toEqual(undefined);
    });
    it('should create list', function() {
        expect(exec('(cons 1 (cons 2 (cons 3 nil)))'))
            .toEqual(deepQuote(Pair.fromArray([LNumber(1), LNumber(2), LNumber(3)])));
    });
    describe('quote', function() {
        it('should return literal list', function() {
            expect(exec(`'(1 2 3 (4 5))`)).toEqual(
                quote(Pair.fromArray([
                    LNumber(1),
                    LNumber(2),
                    LNumber(3),
                    [LNumber(4), LNumber(5)]
                ]))
            );
        });
        it('should return alist', function() {
            expect(exec(`'((foo . 1)
                           (bar . 2.1)
                           (baz . "string")
                           (quux . /foo./g))`)).toEqual(
                quote(new Pair(
                    new Pair(
                        new Symbol('foo'),
                        LNumber(1)
                    ),
                    new Pair(
                        new Pair(
                            new Symbol('bar'),
                            LNumber(2.1)
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
                ))
            );
        });
    });
    describe('quasiquote', function() {
        it('should create list with function call', async function() {
            expect(await exec('`(1 2 3 ,(fun 2 2) 5)', env)).toEqual(
                quote(Pair.fromArray([1, 2, 3, 4, 5].map(LNumber)))
            );
        });
        it('should create list with value', async function() {
            expect(await exec('`(1 2 3 ,value 4)', env)).toEqual(
                quote(Pair.fromArray([1, 2, 3, rand, 4].map(LNumber)))
            );
        });
        it('should create single list using uquote-splice', async function() {
            expect(await exec('`(1 2 3 ,@(f2 4 5) 6)', env)).toEqual(
                quote(Pair.fromArray([1, 2, 3, 4, 5, 6].map(LNumber)))
            );
        });
        it('should create single pair', async function() {
            var specs = [
                '`(1 . 2)',
                '`(,(car (list 1 2 3)) . 2)',
                '`(1 . ,(cadr (list 1 2 3)))',
                '`(,(car (list 1 2 3)) . ,(cadr (list 1 2 3)))'
            ];
            for (let code of specs) {
                expect(await exec(code)).toEqual(quote(new Pair(LNumber(1), LNumber(2))));
            }
        });
        it('should create list from pair syntax', async function() {
            expect(await exec('`(,(car (list 1 2 3)) . (1 2 3))')).toEqual(
                quote(Pair.fromArray([
                    LNumber(1),
                    LNumber(1),
                    LNumber(2),
                    LNumber(3)
                ]))
            );
        });
        it('should create alist with values', async function() {
            expect(await exec(`\`((1 . ,(car (list 1 2)))
                            (2 . ,(cadr (list 1 "foo"))))`))
                .toEqual(
                    quote(new Pair(
                        new Pair(LNumber(1), LNumber(1)),
                        new Pair(new Pair(LNumber(2), "foo"), nil)))
                );
            expect(await exec(`\`((,(car (list "foo")) . ,(car (list 1 2)))
                            (2 . ,(cadr (list 1 "foo"))))`))
                .toEqual(quote(new Pair(
                    new Pair("foo", LNumber(1)),
                    new Pair(
                        new Pair(LNumber(2), "foo"),
                        nil
                    ))));
        });
        it('should process nested backquote', async function() {
            expect(await exec('`(1 2 3 ,(cadr `(1 ,(concat "foo" "bar") 3)) 4)')).toEqual(
                quote(Pair.fromArray([
                    LNumber(1), LNumber(2), LNumber(3), "foobar", LNumber(4)
                ]))
            );
        });
        it('should process multiple backquote/unquote', async function() {
            expect(await exec('``(a ,,(+ 1 2) ,(+ 3 4))')).toEqual(
                quote(Pair.fromArray([
                    new Symbol('quasiquote'),
                    [
                        new Symbol('a'),
                        [
                            new Symbol('unquote'),
                            LNumber(3)
                        ],
                        [
                            new Symbol('unquote'),
                            [
                                new Symbol('+'),
                                LNumber(3),
                                LNumber(4)
                            ]
                        ]
                    ]
                ])));
        });
    });
    describe('trampoline', function() {
        var code = `(define Y
                       (lambda (h)
                          ((lambda (x) (x x))
                           (lambda (g)
                             (h (lambda args (apply (g g) args)))))))

                    (define (trampoline f)
                         (lambda args
                            (let ((result (apply f args)))
                                (while (eq? (type result) "function")
                                   (set! result (result)))
                                result)))

                     (define (! n)
                        ((trampoline (Y (lambda (f)
                                         (lambda (n acc)
                                           (if (== n 0)
                                               acc
                                             (lambda ()
                                                 (f (- n 1) (* n acc)))))))) n 1))

                       (string (! 1000))`;
        var factorial_1000 = [
            "402387260077093773543702433923003985719374864210714632543799910",
            "429938512398629020592044208486969404800479988610197196058631666",
            "872994808558901323829669944590997424504087073759918823627727188",
            "732519779505950995276120874975462497043601418278094646496291056",
            "393887437886487337119181045825783647849977012476632889835955735",
            "432513185323958463075557409114262417474349347553428646576611667",
            "797396668820291207379143853719588249808126867838374559731746136",
            "085379534524221586593201928090878297308431392844403281231558611",
            "036976801357304216168747609675871348312025478589320767169132448",
            "426236131412508780208000261683151027341827977704784635868170164",
            "365024153691398281264810213092761244896359928705114964975419909",
            "342221566832572080821333186116811553615836546984046708975602900",
            "950537616475847728421889679646244945160765353408198901385442487",
            "984959953319101723355556602139450399736280750137837615307127761",
            "926849034352625200015888535147331611702103968175921510907788019",
            "393178114194545257223865541461062892187960223838971476088506276",
            "862967146674697562911234082439208160153780889893964518263243671",
            "616762179168909779911903754031274622289988005195444414282012187",
            "361745992642956581746628302955570299024324153181617210465832036",
            "786906117260158783520751516284225540265170483304226143974286933",
            "061690897968482590125458327168226458066526769958652682272807075",
            "781391858178889652208164348344825993266043367660176999612831860",
            "788386150279465955131156552036093988180612138558600301435694527",
            "224206344631797460594682573103790084024432438465657245014402821",
            "885252470935190620929023136493273497565513958720559654228749774",
            "011413346962715422845862377387538230483865688976461927383814900",
            "140767310446640259899490222221765904339901886018566526485061799",
            "702356193897017860040811889729918311021171229845901641921068884",
            "387121855646124960798722908519296819372388642614839657382291123",
            "125024186649353143970137428531926649875337218940694281434118520",
            "158014123344828015051399694290153483077644569099073152433278288",
            "269864602789864321139083506217095002597389863554277196742822248",
            "757586765752344220207573630569498825087968928162753848863396909",
            "959826280956121450994871701244516461260379029309120889086942028",
            "510640182154399457156805941872748998094254742173582401063677404",
            "595741785160829230135358081840096996372524230560855903700624271",
            "243416909004153690105933983835777939410970027753472000000000000",
            "000000000000000000000000000000000000000000000000000000000000000",
            "000000000000000000000000000000000000000000000000000000000000000",
            "000000000000000000000000000000000000000000000000000000000000000",
            "000000000000000000000000000000000000000000000000"].join('');
        var env = global_environment.inherit('trampoline');
        it('should calculate factorial using Y (TOC)', function() {
            var result = lips.exec(code, env);
            return result.then(function(result) {
                expect(result).toEqual([
                    undefined,
                    undefined,
                    undefined,
                    factorial_1000
                ]);
            });
        });
        it('should throw exception', function() {
            return lips.exec(code, env, env).catch(e => {
                expect(e.code).toEqual('(string (! 1000))');
                expect(e).toEqual(new Error("Unbound variable `f'"));
            });
        });
    });
});
describe('environment', function() {
    const env = global_environment;
    var functions = {
        scope_name: function() {
            return this.name;
        }
    };
    it('should return name of the enviroment', function() {
        var e = env.inherit('foo', functions);
        return lips.exec('(scope_name)', e).then(result => {
            return expect(result).toEqual(['foo']);
        });
    });
    it('should create default scope name', function() {
        var e = env.inherit(functions);
        return lips.exec('(scope_name)', e).then(result => {
            return expect(result).toEqual(['child of global']);
        });
    });
    it('should create default scope name for child scope', function() {
        var e = env.inherit('foo', functions);
        var child = e.inherit();
        return lips.exec('(scope_name)', child).then(result => {
            return expect(result).toEqual(['child of foo']);
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
        it('should append value', function() {
            [
                ['(1 2 3)', '(1 2 3 10)'],
                ['((1 2 3))', '((1 2 3) 10)'],
                ['(1 2 (3) 4)', '(1 2 (3) 4 10)'],
                ['(1 2 3 (4))', '(1 2 3 (4) 10)']
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
                '(1 2 3 (4))'
            ].forEach((code) => {
                var input = str2list(code);
                input.append(lips.nil);
                expect(input).toEqual(str2list(code));
            });
        });
    });
});
describe('env', function() {
    // data on lists are leftovers after macro processing
    // it prevent to evalute retults of macros we need to remove
    // it to have same value is parsing clean data
    // using str2list helper
    function clean(node) {
        if (node instanceof Pair) {
            delete node.data;
            clean(node.car);
            clean(node.cdr);
        }
        return node;
    }
    function testList(code, expected) {
        return lips.exec(code).then(([list]) => {
            expect(clean(list)).toEqual(str2list(expected));
        });
    }
    function testValue([code, expected]) {
        return lips.exec(code).then(([result]) => {
            expect(result).toEqual(expected);
        });
    }
    describe('reduce', function() {
        it('should create and reduce async list', function() {
            const code = `(reduce +
                                  0
                                  (list (timer 1000 10) (timer 2000 30)))`;
            return testValue([code, LNumber(40)]);
        });
        it('should reverse list', function() {
            const code = `(reduce cons '() '(1 2 3))`;
            return testList(code, '(3 2 1)');
        });
        it('should create wrong list', function() {
            return testList(`(reduce list '() '(1 2 3))`, '(3 (2 (1 ())))');
        });
    });
    describe('dot function', function() {
        it('sould call pair method on list structure', function() {
            var code = `((. '((a . "10") (b . 20)) "toObject"))`;
            return lips.exec(code).then(([result]) => {
                expect(result).toEqual({ a: "10", b: 20 });
            });
        });
    });
    describe('gensym', function() {
        it('should create uniqe symbols', function() {
            return Promise.all([
                [`(eq? (gensym "foo") (gensym "foo"))`, false],
                [`(eq? 'foo 'foo)`, true],
                [`(eq? (string (gensym "foo")) (string (gensym "foo")))`, true]
            ].map(testValue));
        });
    });
    describe('math', function() {
        it('should calculate math operations', function() {
            return Promise.all([
                ['(* 1 2 3 4 5)', LNumber(120)],
                ['(+ 1 2 3 4 5)', LNumber(15)],
                ['(- 10 2 3)', LNumber(5)]
            ].map(testValue));
        });
    });
});
