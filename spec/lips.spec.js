/* global require, describe, it, expect */

var lips = require('../index');


var {
    parse,
    tokenize,
    Environment,
    evaluate,
    global_environment,
    Pair,
    Symbol,
    nil
} = lips;

describe('tokenizer', function() {
    it('should create tokens for simple list', function() {
        expect(tokenize('(foo bar baz)')).toEqual(['(', 'foo', 'bar', 'baz', ')']);
    });
    it('should create tokens for numbers string and regexes', function() {
        expect(tokenize('(foo /( \\/)/g "bar baz" 10 10e2)')).toEqual([
            '(', 'foo', '/( \\/)/g', '"bar baz"', '10', '10e2', ')'
        ]);
    });
    it('should create tokens for alists', function() {
        expect(tokenize('((foo . 10) (bar . 20) (baz . 30))')).toEqual([
            '(', '(', 'foo', '.', '10', ')', '(', 'bar', '.', '20', ')', '(', 'baz', '.', '30', ')', ')'
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
        var tokens = tokenize('(foo /( \\/)/g "bar baz" 10 10e2)');
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
                                10e2,
                                nil
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
