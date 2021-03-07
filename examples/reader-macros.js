/**
 * This is example how you can extend LIPS parser using reader macros
 *
 * This is part of the LIPS JavaScript interpter
 * Copyriht (C) Jakub T. Jankiewicz <https://jcubic.pl>
 * Released under MIT license
 */
/* global lips */
(function() {
    const { env, specials, Symbol, Pair, Macro, evaluate, LNumber } = lips;
    // new vector type #(1 2 3) will create new array
    // you can use any expression inside that will evalute to list that in turn will be
    // converter to array by the macro. Parser will convert #(1 2 3) into (vector (1 2 3))
    // so vector need to be LIPS macro
    specials['#'] = Symbol('vector');
    env.set('vector', new Macro('vector', function(code, { dynamic_scope, error }) {
        const args = { env: this, error };
        if (dynamic_scope) {
            args.dynamic_scope = this;
        }
        var wrap = new Pair(new Symbol('list'), code.car);
        return this.get('list->array')(evaluate(wrap, args));
    }));

    // new Alist ot object syntax you use is like this:
    // {}((foo . 10)) it will also evalute all pairs expressions like:
    // {}(((concat "foo" "-" "bar") . (+ 2 1))) it will create obect {"foo-bar": 3}
    specials['{}'] = Symbol('make-object');
    env.set('make-object', new Macro('make-object', function(code, { dynamic_scope, error }) {
        const args = { env: this, error };
        if (dynamic_scope) {
            args.dynamic_scope = this;
        }
        return code.car.map((pair) => {
            const [car, cdr] = [pair.car, pair.cdr].map(obj => {
                if (obj instanceof Pair) {
                    return evaluate(obj, args);
                }
                return obj;
            }).map(obj => {
                // unwrap LNumbers
                if (obj instanceof LNumber) {
                    return obj.valueOf();
                }
                return obj;
            });
            return new Pair(car, cdr);
        }).to_object();
    }));
    env.set('object->string', function(obj) {
        return JSON.stringify(obj);
    });
})();
