/**@license
 * LIPS is Pretty Simple - version DEV
 *
 * Copyright (c) 2018 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 *
 * includes unfetch by Jason Miller (@developit) MIT License
 *
 * build: Sun, 06 Jan 2019 20:42:06 +0000
 */
(function () {
'use strict';

function _arrayWithHoles(arr) {
  if (Array.isArray(arr)) return arr;
}

function _iterableToArrayLimit(arr, i) {
  var _arr = [];
  var _n = true;
  var _d = false;
  var _e = undefined;

  try {
    for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) {
      _arr.push(_s.value);

      if (i && _arr.length === i) break;
    }
  } catch (err) {
    _d = true;
    _e = err;
  } finally {
    try {
      if (!_n && _i["return"] != null) _i["return"]();
    } finally {
      if (_d) throw _e;
    }
  }

  return _arr;
}

function _nonIterableRest() {
  throw new TypeError("Invalid attempt to destructure non-iterable instance");
}

function _slicedToArray(arr, i) {
  return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _nonIterableRest();
}

function _setPrototypeOf(o, p) {
  _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) {
    o.__proto__ = p;
    return o;
  };

  return _setPrototypeOf(o, p);
}

function isNativeReflectConstruct() {
  if (typeof Reflect === "undefined" || !Reflect.construct) return false;
  if (Reflect.construct.sham) return false;
  if (typeof Proxy === "function") return true;

  try {
    Date.prototype.toString.call(Reflect.construct(Date, [], function () {}));
    return true;
  } catch (e) {
    return false;
  }
}

function _construct(Parent, args, Class) {
  if (isNativeReflectConstruct()) {
    _construct = Reflect.construct;
  } else {
    _construct = function _construct(Parent, args, Class) {
      var a = [null];
      a.push.apply(a, args);
      var Constructor = Function.bind.apply(Parent, a);
      var instance = new Constructor();
      if (Class) setPrototypeOf(instance, Class.prototype);
      return instance;
    };
  }

  return _construct.apply(null, arguments);
}

function createCommonjsModule(fn, module) {
	return module = { exports: {} }, fn(module, module.exports), module.exports;
}

var runtime = createCommonjsModule(function (module) {
/**
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

!(function(global) {

  var Op = Object.prototype;
  var hasOwn = Op.hasOwnProperty;
  var undefined; // More compressible than void 0.
  var $Symbol = typeof Symbol === "function" ? Symbol : {};
  var iteratorSymbol = $Symbol.iterator || "@@iterator";
  var asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator";
  var toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";

  var inModule = 'object' === "object";
  var runtime = global.regeneratorRuntime;
  if (runtime) {
    if (inModule) {
      // If regeneratorRuntime is defined globally and we're in a module,
      // make the exports object identical to regeneratorRuntime.
      module.exports = runtime;
    }
    // Don't bother evaluating the rest of this file if the runtime was
    // already defined globally.
    return;
  }

  // Define the runtime globally (as expected by generated code) as either
  // module.exports (if we're in a module) or a new, empty object.
  runtime = global.regeneratorRuntime = inModule ? module.exports : {};

  function wrap(innerFn, outerFn, self, tryLocsList) {
    // If outerFn provided and outerFn.prototype is a Generator, then outerFn.prototype instanceof Generator.
    var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator;
    var generator = Object.create(protoGenerator.prototype);
    var context = new Context(tryLocsList || []);

    // The ._invoke method unifies the implementations of the .next,
    // .throw, and .return methods.
    generator._invoke = makeInvokeMethod(innerFn, self, context);

    return generator;
  }
  runtime.wrap = wrap;

  // Try/catch helper to minimize deoptimizations. Returns a completion
  // record like context.tryEntries[i].completion. This interface could
  // have been (and was previously) designed to take a closure to be
  // invoked without arguments, but in all the cases we care about we
  // already have an existing method we want to call, so there's no need
  // to create a new function object. We can even get away with assuming
  // the method takes exactly one argument, since that happens to be true
  // in every case, so we don't have to touch the arguments object. The
  // only additional allocation required is the completion record, which
  // has a stable shape and so hopefully should be cheap to allocate.
  function tryCatch(fn, obj, arg) {
    try {
      return { type: "normal", arg: fn.call(obj, arg) };
    } catch (err) {
      return { type: "throw", arg: err };
    }
  }

  var GenStateSuspendedStart = "suspendedStart";
  var GenStateSuspendedYield = "suspendedYield";
  var GenStateExecuting = "executing";
  var GenStateCompleted = "completed";

  // Returning this object from the innerFn has the same effect as
  // breaking out of the dispatch switch statement.
  var ContinueSentinel = {};

  // Dummy constructor functions that we use as the .constructor and
  // .constructor.prototype properties for functions that return Generator
  // objects. For full spec compliance, you may wish to configure your
  // minifier not to mangle the names of these two functions.
  function Generator() {}
  function GeneratorFunction() {}
  function GeneratorFunctionPrototype() {}

  // This is a polyfill for %IteratorPrototype% for environments that
  // don't natively support it.
  var IteratorPrototype = {};
  IteratorPrototype[iteratorSymbol] = function () {
    return this;
  };

  var getProto = Object.getPrototypeOf;
  var NativeIteratorPrototype = getProto && getProto(getProto(values([])));
  if (NativeIteratorPrototype &&
      NativeIteratorPrototype !== Op &&
      hasOwn.call(NativeIteratorPrototype, iteratorSymbol)) {
    // This environment has a native %IteratorPrototype%; use it instead
    // of the polyfill.
    IteratorPrototype = NativeIteratorPrototype;
  }

  var Gp = GeneratorFunctionPrototype.prototype =
    Generator.prototype = Object.create(IteratorPrototype);
  GeneratorFunction.prototype = Gp.constructor = GeneratorFunctionPrototype;
  GeneratorFunctionPrototype.constructor = GeneratorFunction;
  GeneratorFunctionPrototype[toStringTagSymbol] =
    GeneratorFunction.displayName = "GeneratorFunction";

  // Helper for defining the .next, .throw, and .return methods of the
  // Iterator interface in terms of a single ._invoke method.
  function defineIteratorMethods(prototype) {
    ["next", "throw", "return"].forEach(function(method) {
      prototype[method] = function(arg) {
        return this._invoke(method, arg);
      };
    });
  }

  runtime.isGeneratorFunction = function(genFun) {
    var ctor = typeof genFun === "function" && genFun.constructor;
    return ctor
      ? ctor === GeneratorFunction ||
        // For the native GeneratorFunction constructor, the best we can
        // do is to check its .name property.
        (ctor.displayName || ctor.name) === "GeneratorFunction"
      : false;
  };

  runtime.mark = function(genFun) {
    if (Object.setPrototypeOf) {
      Object.setPrototypeOf(genFun, GeneratorFunctionPrototype);
    } else {
      genFun.__proto__ = GeneratorFunctionPrototype;
      if (!(toStringTagSymbol in genFun)) {
        genFun[toStringTagSymbol] = "GeneratorFunction";
      }
    }
    genFun.prototype = Object.create(Gp);
    return genFun;
  };

  // Within the body of any async function, `await x` is transformed to
  // `yield regeneratorRuntime.awrap(x)`, so that the runtime can test
  // `hasOwn.call(value, "__await")` to determine if the yielded value is
  // meant to be awaited.
  runtime.awrap = function(arg) {
    return { __await: arg };
  };

  function AsyncIterator(generator) {
    function invoke(method, arg, resolve, reject) {
      var record = tryCatch(generator[method], generator, arg);
      if (record.type === "throw") {
        reject(record.arg);
      } else {
        var result = record.arg;
        var value = result.value;
        if (value &&
            typeof value === "object" &&
            hasOwn.call(value, "__await")) {
          return Promise.resolve(value.__await).then(function(value) {
            invoke("next", value, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          });
        }

        return Promise.resolve(value).then(function(unwrapped) {
          // When a yielded Promise is resolved, its final value becomes
          // the .value of the Promise<{value,done}> result for the
          // current iteration.
          result.value = unwrapped;
          resolve(result);
        }, function(error) {
          // If a rejected Promise was yielded, throw the rejection back
          // into the async generator function so it can be handled there.
          return invoke("throw", error, resolve, reject);
        });
      }
    }

    var previousPromise;

    function enqueue(method, arg) {
      function callInvokeWithMethodAndArg() {
        return new Promise(function(resolve, reject) {
          invoke(method, arg, resolve, reject);
        });
      }

      return previousPromise =
        // If enqueue has been called before, then we want to wait until
        // all previous Promises have been resolved before calling invoke,
        // so that results are always delivered in the correct order. If
        // enqueue has not been called before, then it is important to
        // call invoke immediately, without waiting on a callback to fire,
        // so that the async generator function has the opportunity to do
        // any necessary setup in a predictable way. This predictability
        // is why the Promise constructor synchronously invokes its
        // executor callback, and why async functions synchronously
        // execute code before the first await. Since we implement simple
        // async functions in terms of async generators, it is especially
        // important to get this right, even though it requires care.
        previousPromise ? previousPromise.then(
          callInvokeWithMethodAndArg,
          // Avoid propagating failures to Promises returned by later
          // invocations of the iterator.
          callInvokeWithMethodAndArg
        ) : callInvokeWithMethodAndArg();
    }

    // Define the unified helper method that is used to implement .next,
    // .throw, and .return (see defineIteratorMethods).
    this._invoke = enqueue;
  }

  defineIteratorMethods(AsyncIterator.prototype);
  AsyncIterator.prototype[asyncIteratorSymbol] = function () {
    return this;
  };
  runtime.AsyncIterator = AsyncIterator;

  // Note that simple async functions are implemented on top of
  // AsyncIterator objects; they just return a Promise for the value of
  // the final result produced by the iterator.
  runtime.async = function(innerFn, outerFn, self, tryLocsList) {
    var iter = new AsyncIterator(
      wrap(innerFn, outerFn, self, tryLocsList)
    );

    return runtime.isGeneratorFunction(outerFn)
      ? iter // If outerFn is a generator, return the full iterator.
      : iter.next().then(function(result) {
          return result.done ? result.value : iter.next();
        });
  };

  function makeInvokeMethod(innerFn, self, context) {
    var state = GenStateSuspendedStart;

    return function invoke(method, arg) {
      if (state === GenStateExecuting) {
        throw new Error("Generator is already running");
      }

      if (state === GenStateCompleted) {
        if (method === "throw") {
          throw arg;
        }

        // Be forgiving, per 25.3.3.3.3 of the spec:
        // https://people.mozilla.org/~jorendorff/es6-draft.html#sec-generatorresume
        return doneResult();
      }

      context.method = method;
      context.arg = arg;

      while (true) {
        var delegate = context.delegate;
        if (delegate) {
          var delegateResult = maybeInvokeDelegate(delegate, context);
          if (delegateResult) {
            if (delegateResult === ContinueSentinel) continue;
            return delegateResult;
          }
        }

        if (context.method === "next") {
          // Setting context._sent for legacy support of Babel's
          // function.sent implementation.
          context.sent = context._sent = context.arg;

        } else if (context.method === "throw") {
          if (state === GenStateSuspendedStart) {
            state = GenStateCompleted;
            throw context.arg;
          }

          context.dispatchException(context.arg);

        } else if (context.method === "return") {
          context.abrupt("return", context.arg);
        }

        state = GenStateExecuting;

        var record = tryCatch(innerFn, self, context);
        if (record.type === "normal") {
          // If an exception is thrown from innerFn, we leave state ===
          // GenStateExecuting and loop back for another invocation.
          state = context.done
            ? GenStateCompleted
            : GenStateSuspendedYield;

          if (record.arg === ContinueSentinel) {
            continue;
          }

          return {
            value: record.arg,
            done: context.done
          };

        } else if (record.type === "throw") {
          state = GenStateCompleted;
          // Dispatch the exception by looping back around to the
          // context.dispatchException(context.arg) call above.
          context.method = "throw";
          context.arg = record.arg;
        }
      }
    };
  }

  // Call delegate.iterator[context.method](context.arg) and handle the
  // result, either by returning a { value, done } result from the
  // delegate iterator, or by modifying context.method and context.arg,
  // setting context.delegate to null, and returning the ContinueSentinel.
  function maybeInvokeDelegate(delegate, context) {
    var method = delegate.iterator[context.method];
    if (method === undefined) {
      // A .throw or .return when the delegate iterator has no .throw
      // method always terminates the yield* loop.
      context.delegate = null;

      if (context.method === "throw") {
        if (delegate.iterator.return) {
          // If the delegate iterator has a return method, give it a
          // chance to clean up.
          context.method = "return";
          context.arg = undefined;
          maybeInvokeDelegate(delegate, context);

          if (context.method === "throw") {
            // If maybeInvokeDelegate(context) changed context.method from
            // "return" to "throw", let that override the TypeError below.
            return ContinueSentinel;
          }
        }

        context.method = "throw";
        context.arg = new TypeError(
          "The iterator does not provide a 'throw' method");
      }

      return ContinueSentinel;
    }

    var record = tryCatch(method, delegate.iterator, context.arg);

    if (record.type === "throw") {
      context.method = "throw";
      context.arg = record.arg;
      context.delegate = null;
      return ContinueSentinel;
    }

    var info = record.arg;

    if (! info) {
      context.method = "throw";
      context.arg = new TypeError("iterator result is not an object");
      context.delegate = null;
      return ContinueSentinel;
    }

    if (info.done) {
      // Assign the result of the finished delegate to the temporary
      // variable specified by delegate.resultName (see delegateYield).
      context[delegate.resultName] = info.value;

      // Resume execution at the desired location (see delegateYield).
      context.next = delegate.nextLoc;

      // If context.method was "throw" but the delegate handled the
      // exception, let the outer generator proceed normally. If
      // context.method was "next", forget context.arg since it has been
      // "consumed" by the delegate iterator. If context.method was
      // "return", allow the original .return call to continue in the
      // outer generator.
      if (context.method !== "return") {
        context.method = "next";
        context.arg = undefined;
      }

    } else {
      // Re-yield the result returned by the delegate method.
      return info;
    }

    // The delegate iterator is finished, so forget it and continue with
    // the outer generator.
    context.delegate = null;
    return ContinueSentinel;
  }

  // Define Generator.prototype.{next,throw,return} in terms of the
  // unified ._invoke helper method.
  defineIteratorMethods(Gp);

  Gp[toStringTagSymbol] = "Generator";

  // A Generator should always return itself as the iterator object when the
  // @@iterator function is called on it. Some browsers' implementations of the
  // iterator prototype chain incorrectly implement this, causing the Generator
  // object to not be returned from this call. This ensures that doesn't happen.
  // See https://github.com/facebook/regenerator/issues/274 for more details.
  Gp[iteratorSymbol] = function() {
    return this;
  };

  Gp.toString = function() {
    return "[object Generator]";
  };

  function pushTryEntry(locs) {
    var entry = { tryLoc: locs[0] };

    if (1 in locs) {
      entry.catchLoc = locs[1];
    }

    if (2 in locs) {
      entry.finallyLoc = locs[2];
      entry.afterLoc = locs[3];
    }

    this.tryEntries.push(entry);
  }

  function resetTryEntry(entry) {
    var record = entry.completion || {};
    record.type = "normal";
    delete record.arg;
    entry.completion = record;
  }

  function Context(tryLocsList) {
    // The root entry object (effectively a try statement without a catch
    // or a finally block) gives us a place to store values thrown from
    // locations where there is no enclosing try statement.
    this.tryEntries = [{ tryLoc: "root" }];
    tryLocsList.forEach(pushTryEntry, this);
    this.reset(true);
  }

  runtime.keys = function(object) {
    var keys = [];
    for (var key in object) {
      keys.push(key);
    }
    keys.reverse();

    // Rather than returning an object with a next method, we keep
    // things simple and return the next function itself.
    return function next() {
      while (keys.length) {
        var key = keys.pop();
        if (key in object) {
          next.value = key;
          next.done = false;
          return next;
        }
      }

      // To avoid creating an additional object, we just hang the .value
      // and .done properties off the next function object itself. This
      // also ensures that the minifier will not anonymize the function.
      next.done = true;
      return next;
    };
  };

  function values(iterable) {
    if (iterable) {
      var iteratorMethod = iterable[iteratorSymbol];
      if (iteratorMethod) {
        return iteratorMethod.call(iterable);
      }

      if (typeof iterable.next === "function") {
        return iterable;
      }

      if (!isNaN(iterable.length)) {
        var i = -1, next = function next() {
          while (++i < iterable.length) {
            if (hasOwn.call(iterable, i)) {
              next.value = iterable[i];
              next.done = false;
              return next;
            }
          }

          next.value = undefined;
          next.done = true;

          return next;
        };

        return next.next = next;
      }
    }

    // Return an iterator with no values.
    return { next: doneResult };
  }
  runtime.values = values;

  function doneResult() {
    return { value: undefined, done: true };
  }

  Context.prototype = {
    constructor: Context,

    reset: function(skipTempReset) {
      this.prev = 0;
      this.next = 0;
      // Resetting context._sent for legacy support of Babel's
      // function.sent implementation.
      this.sent = this._sent = undefined;
      this.done = false;
      this.delegate = null;

      this.method = "next";
      this.arg = undefined;

      this.tryEntries.forEach(resetTryEntry);

      if (!skipTempReset) {
        for (var name in this) {
          // Not sure about the optimal order of these conditions:
          if (name.charAt(0) === "t" &&
              hasOwn.call(this, name) &&
              !isNaN(+name.slice(1))) {
            this[name] = undefined;
          }
        }
      }
    },

    stop: function() {
      this.done = true;

      var rootEntry = this.tryEntries[0];
      var rootRecord = rootEntry.completion;
      if (rootRecord.type === "throw") {
        throw rootRecord.arg;
      }

      return this.rval;
    },

    dispatchException: function(exception) {
      if (this.done) {
        throw exception;
      }

      var context = this;
      function handle(loc, caught) {
        record.type = "throw";
        record.arg = exception;
        context.next = loc;

        if (caught) {
          // If the dispatched exception was caught by a catch block,
          // then let that catch block handle the exception normally.
          context.method = "next";
          context.arg = undefined;
        }

        return !! caught;
      }

      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        var record = entry.completion;

        if (entry.tryLoc === "root") {
          // Exception thrown outside of any try block that could handle
          // it, so set the completion value of the entire function to
          // throw the exception.
          return handle("end");
        }

        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn.call(entry, "catchLoc");
          var hasFinally = hasOwn.call(entry, "finallyLoc");

          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc) {
              return handle(entry.catchLoc, true);
            } else if (this.prev < entry.finallyLoc) {
              return handle(entry.finallyLoc);
            }

          } else if (hasCatch) {
            if (this.prev < entry.catchLoc) {
              return handle(entry.catchLoc, true);
            }

          } else if (hasFinally) {
            if (this.prev < entry.finallyLoc) {
              return handle(entry.finallyLoc);
            }

          } else {
            throw new Error("try statement without catch or finally");
          }
        }
      }
    },

    abrupt: function(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev &&
            hasOwn.call(entry, "finallyLoc") &&
            this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }

      if (finallyEntry &&
          (type === "break" ||
           type === "continue") &&
          finallyEntry.tryLoc <= arg &&
          arg <= finallyEntry.finallyLoc) {
        // Ignore the finally entry if control is not jumping to a
        // location outside the try/catch block.
        finallyEntry = null;
      }

      var record = finallyEntry ? finallyEntry.completion : {};
      record.type = type;
      record.arg = arg;

      if (finallyEntry) {
        this.method = "next";
        this.next = finallyEntry.finallyLoc;
        return ContinueSentinel;
      }

      return this.complete(record);
    },

    complete: function(record, afterLoc) {
      if (record.type === "throw") {
        throw record.arg;
      }

      if (record.type === "break" ||
          record.type === "continue") {
        this.next = record.arg;
      } else if (record.type === "return") {
        this.rval = this.arg = record.arg;
        this.method = "return";
        this.next = "end";
      } else if (record.type === "normal" && afterLoc) {
        this.next = afterLoc;
      }

      return ContinueSentinel;
    },

    finish: function(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc) {
          this.complete(entry.completion, entry.afterLoc);
          resetTryEntry(entry);
          return ContinueSentinel;
        }
      }
    },

    "catch": function(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if (record.type === "throw") {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }

      // The context.catch method must only be called with a location
      // argument that corresponds to a known catch block.
      throw new Error("illegal catch attempt");
    },

    delegateYield: function(iterable, resultName, nextLoc) {
      this.delegate = {
        iterator: values(iterable),
        resultName: resultName,
        nextLoc: nextLoc
      };

      if (this.method === "next") {
        // Deliberately forget the last sent value so that we don't
        // accidentally pass it on to the delegate.
        this.arg = undefined;
      }

      return ContinueSentinel;
    }
  };
})(
  // In sloppy mode, unbound `this` refers to the global object, fallback to
  // Function constructor if we're in global strict mode. That is sadly a form
  // of indirect eval which violates Content Security Policy.
  (function() {
    return this || (typeof self === "object" && self);
  })() || Function("return this")()
);
});

/**
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This method of obtaining a reference to the global object needs to be
// kept identical to the way it is obtained in runtime.js
var g = (function() {
  return this || (typeof self === "object" && self);
})() || Function("return this")();

// Use `getOwnPropertyNames` because not all browsers support calling
// `hasOwnProperty` on the global `self` object in a worker. See #183.
var hadRuntime = g.regeneratorRuntime &&
  Object.getOwnPropertyNames(g).indexOf("regeneratorRuntime") >= 0;

// Save the old regeneratorRuntime in case it needs to be restored later.
var oldRuntime = hadRuntime && g.regeneratorRuntime;

// Force reevalutation of runtime.js.
g.regeneratorRuntime = undefined;

var runtimeModule = runtime;

if (hadRuntime) {
  // Restore the original runtime.
  g.regeneratorRuntime = oldRuntime;
} else {
  // Remove the global property added by runtime.js.
  try {
    delete g.regeneratorRuntime;
  } catch(e) {
    g.regeneratorRuntime = undefined;
  }
}

var regenerator = runtimeModule;

function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) {
  try {
    var info = gen[key](arg);
    var value = info.value;
  } catch (error) {
    reject(error);
    return;
  }

  if (info.done) {
    resolve(value);
  } else {
    Promise.resolve(value).then(_next, _throw);
  }
}

function _asyncToGenerator(fn) {
  return function () {
    var self = this,
        args = arguments;
    return new Promise(function (resolve, reject) {
      var gen = fn.apply(self, args);

      function _next(value) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value);
      }

      function _throw(err) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err);
      }

      _next(undefined);
    });
  };
}

function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) {
    for (var i = 0, arr2 = new Array(arr.length); i < arr.length; i++) {
      arr2[i] = arr[i];
    }

    return arr2;
  }
}

function _iterableToArray(iter) {
  if (Symbol.iterator in Object(iter) || Object.prototype.toString.call(iter) === "[object Arguments]") return Array.from(iter);
}

function _nonIterableSpread() {
  throw new TypeError("Invalid attempt to spread non-iterable instance");
}

function _toConsumableArray(arr) {
  return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _nonIterableSpread();
}

function _typeof2(obj) { if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof2 = function _typeof2(obj) { return typeof obj; }; } else { _typeof2 = function _typeof2(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof2(obj); }

function _typeof(obj) {
  if (typeof Symbol === "function" && _typeof2(Symbol.iterator) === "symbol") {
    _typeof = function _typeof(obj) {
      return _typeof2(obj);
    };
  } else {
    _typeof = function _typeof(obj) {
      return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : _typeof2(obj);
    };
  }

  return _typeof(obj);
}

/**@license
 * LIPS is Pretty Simple
 *
 * Copyright (c) 2018 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 *
 */

(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    // AMD. Register as an anonymous module.
    define(['bn.js'], function (BN) {
      return root.lips = factory(root, BN);
    });
  } else if ((typeof module === "undefined" ? "undefined" : _typeof(module)) === 'object' && module.exports) {
    // Node/CommonJS
    module.exports = factory(root, require('bn.js'));
  } else {
    root.lips = factory(root, root.BN);
  }
})(typeof window !== 'undefined' ? window : global, function (root, BN, undefined) {

  if (!root.fetch) {
    root.fetch = function (url, options) {
      options = options || {};
      return new Promise(function (resolve, reject) {
        var request = new XMLHttpRequest();
        request.open(options.method || 'get', url, true);

        for (var i in options.headers) {
          request.setRequestHeader(i, options.headers[i]);
        }

        request.withCredentials = options.credentials == 'include';

        request.onload = function () {
          resolve(response());
        };

        request.onerror = reject;
        request.send(options.body || null);

        function response() {
          var _keys = [],
              all = [],
              headers = {},
              header;
          request.getAllResponseHeaders().replace(/^(.*?):[^\S\n]*([\s\S]*?)$/gm, function (m, key, value) {
            _keys.push(key = key.toLowerCase());

            all.push([key, value]);
            header = headers[key];
            headers[key] = header ? "".concat(header, ",").concat(value) : value;
          });
          return {
            ok: (request.status / 100 | 0) == 2,
            // 200-299
            status: request.status,
            statusText: request.statusText,
            url: request.responseURL,
            clone: response,
            text: function text() {
              return Promise.resolve(request.responseText);
            },
            json: function json() {
              return Promise.resolve(request.responseText).then(JSON.parse);
            },
            blob: function blob() {
              return Promise.resolve(new Blob([request.response]));
            },
            headers: {
              keys: function keys() {
                return _keys;
              },
              entries: function entries() {
                return all;
              },
              get: function get(n) {
                return headers[n.toLowerCase()];
              },
              has: function has(n) {
                return n.toLowerCase() in headers;
              }
            }
          };
        }
      });
    };
  }
  /* eslint-enable */
  // parse_argument based on function from jQuery Terminal


  var re_re = /^\/((?:\\\/|[^/]|\[[^\]]*\/[^\]]*\])+)\/([gimy]*)$/;
  var int_re = /^[-+]?[0-9]+([eE][-+]?[0-9]+)?$/;
  var float_re = /^([-+]?((\.[0-9]+|[0-9]+\.[0-9]+)([eE][-+]?[0-9]+)?)|[0-9]+\.)$/; // ----------------------------------------------------------------------

  function parse_argument(arg) {
    function parse_string(string) {
      // remove quotes if before are even number of slashes
      // we don't remove slases becuase they are handled by JSON.parse
      //string = string.replace(/([^\\])['"]$/, '$1');
      if (string.match(/^['"]/)) {
        if (string === '""' || string === "''") {
          return '';
        }

        var quote = string[0];
        var re = new RegExp("((^|[^\\\\])(?:\\\\\\\\)*)" + quote, "g");
        string = string.replace(re, "$1");
      } // use build in function to parse rest of escaped characters


      return JSON.parse('"' + string.replace(/\n/g, '\\n') + '"');
    }

    var regex = arg.match(re_re);

    if (regex) {
      return new RegExp(regex[1], regex[2]);
    } else if (arg.match(/['"]/)) {
      return parse_string(arg);
    } else if (arg.match(int_re)) {
      return LNumber(parseFloat(arg));
    } else if (arg.match(float_re)) {
      return LNumber(parseFloat(arg), true);
    } else if (arg === 'nil') {
      return nil;
    } else if (arg === 'true') {
      return true;
    } else if (arg === 'false') {
      return false;
    } else {
      return new _Symbol(arg);
    }
  } // ----------------------------------------------------------------------

  /* eslint-disable */


  var pre_parse_re = /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|;.*)/g;
  var tokens_re = /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|\(|\)|'|"(?:\\[\S\s]|[^"])+|(?:\\[\S\s]|[^"])*"|;.*|(?:[-+]?(?:(?:\.[0-9]+|[0-9]+\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\.)[0-9]|\.|,@|,|`|[^(\s)]+)/gim;
  /* eslint-enable */
  // ----------------------------------------------------------------------

  function tokens(str) {
    var count = 0;
    var offset = 0;
    var tokens = [];
    str.split(pre_parse_re).filter(Boolean).forEach(function (string) {
      if (string.match(pre_parse_re)) {
        if (!string.match(/^;/)) {
          var col = (string.split(/\n/), [""]).pop().length;
          tokens.push({
            token: string,
            col: col,
            offset: count,
            line: offset
          });
          count += string.length;
        }

        offset += (string.match("\n") || []).length;
        return;
      }

      string.split('\n').filter(Boolean).forEach(function (line, i) {
        var col = 0;
        line.split(tokens_re).filter(Boolean).forEach(function (token) {
          var result = {
            col: col,
            line: i + offset,
            token: token,
            offset: count
          };
          col += token.length;
          count += token.length;
          tokens.push(result);
        });
      });
    });
    return tokens;
  } // ----------------------------------------------------------------------


  function tokenize(str, extra) {
    if (extra) {
      return tokens(str);
    } else {
      return tokens(str).map(function (token) {
        return token.token.trim();
      }).filter(function (token) {
        return token && !token.match(/^;/);
      });
    }
  } // ----------------------------------------------------------------------


  var specials = {
    "'": new _Symbol('quote'),
    '`': new _Symbol('quasiquote'),
    ',': new _Symbol('unquote'),
    ',@': new _Symbol('unquote-splicing')
  }; // ----------------------------------------------------------------------
  // :: tokens are the array of strings from tokenizer
  // :: the return value is lisp code created out of Pair class
  // ----------------------------------------------------------------------

  function parse(tokens) {
    var stack = [];
    var result = [];
    var special = null;
    var special_tokens = Object.keys(specials);
    var special_forms = special_tokens.map(function (s) {
      return specials[s].name;
    });
    var parents = 0;
    var first_value = false;
    var specials_stack = [];
    var single_list_specials = [];
    var special_count = 0;

    function pop_join() {
      var top = stack[stack.length - 1];

      if (top instanceof Array && top[0] instanceof _Symbol && special_forms.includes(top[0].name) && stack.length > 1) {
        stack.pop();

        if (stack[stack.length - 1].length === 1 && stack[stack.length - 1][0] instanceof _Symbol) {
          stack[stack.length - 1].push(top);
        } else if (stack[stack.length - 1].length === 0) {
          stack[stack.length - 1] = top;
        } else if (stack[stack.length - 1] instanceof Pair) {
          if (stack[stack.length - 1].cdr instanceof Pair) {
            stack[stack.length - 1] = new Pair(stack[stack.length - 1], Pair.fromArray(top));
          } else {
            stack[stack.length - 1].cdr = Pair.fromArray(top);
          }
        } else {
          stack[stack.length - 1].push(top);
        }
      }
    }

    tokens.forEach(function (token) {
      var top = stack[stack.length - 1];

      if (special_tokens.indexOf(token) !== -1) {
        special_count++;
        special = token;
        stack.push([specials[special]]);

        if (!special) {
          single_list_specials = [];
        }

        single_list_specials.push(special);
      } else if (token === '(') {
        first_value = true;
        parents++;

        if (special) {
          specials_stack.push(single_list_specials);
          single_list_specials = [];
        }

        stack.push([]);
        special = null;
        special_count = 0;
      } else if (token === '.' && !first_value) {
        stack[stack.length - 1] = Pair.fromArray(top);
      } else if (token === ')') {
        parents--;

        if (!stack.length) {
          throw new Error('Unbalanced parenthesis 1');
        }

        if (stack.length === 1) {
          result.push(stack.pop());
        } else if (stack.length > 1) {
          var list = stack.pop();
          top = stack[stack.length - 1];

          if (top instanceof Array) {
            top.push(list);
          } else if (top instanceof Pair) {
            top.append(Pair.fromArray(list));
          }

          if (specials_stack.length) {
            single_list_specials = specials_stack.pop();

            while (single_list_specials.length) {
              pop_join();
              single_list_specials.pop();
            }
          } else {
            pop_join();
          }
        }

        if (parents === 0 && stack.length) {
          result.push(stack.pop());
        }
      } else {
        first_value = false;
        var value = parse_argument(token);

        if (special) {
          // special without list like ,foo
          while (special_count--) {
            stack[stack.length - 1][1] = value;
            value = stack.pop();
          }

          special_count = 0;
          special = false;
        }

        top = stack[stack.length - 1];

        if (top instanceof Pair) {
          var node = top;

          while (true) {
            if (node.cdr === nil) {
              if (value instanceof Array) {
                node.cdr = Pair.fromArray(value);
              } else {
                node.cdr = value;
              }

              break;
            } else {
              node = node.cdr;
            }
          }
        } else if (!stack.length) {
          result.push(value);
        } else {
          top.push(value);
        }
      }
    });

    if (stack.length) {
      console.log({
        end: stack.slice()
      });
      throw new Error('Unbalanced parenthesis 2');
    }

    return result.map(function (arg) {
      if (arg instanceof Array) {
        return Pair.fromArray(arg);
      }

      return arg;
    });
  } // ----------------------------------------------------------------------
  // :: flatten nested arrays
  // :: source: https://stackoverflow.com/a/27282907/387194
  // ----------------------------------------------------------------------


  function flatten(array, mutable) {
    var toString = Object.prototype.toString;
    var arrayTypeStr = '[object Array]';
    var result = [];
    var nodes = mutable && array || array.slice();
    var node;

    if (!array.length) {
      return result;
    }

    node = nodes.pop();

    do {
      if (toString.call(node) === arrayTypeStr) {
        nodes.push.apply(nodes, node);
      } else {
        result.push(node);
      }
    } while (nodes.length && (node = nodes.pop()) !== undefined);

    result.reverse(); // we reverse result to restore the original order

    return result;
  } // ----------------------------------------------------------------------
  // detect if object is ES6 Symbol that work with polyfills
  // ----------------------------------------------------------------------


  function isSymbol(x) {
    return _typeof(x) === 'symbol' || _typeof(x) === 'object' && Object.prototype.toString.call(x) === '[object Symbol]';
  } // ----------------------------------------------------------------------
  // :: Symbol constructor
  // ----------------------------------------------------------------------


  function _Symbol(name) {
    if (typeof this !== 'undefined' && this.constructor !== _Symbol || typeof this === 'undefined') {
      return new _Symbol(name);
    }

    this.name = name;
  }

  _Symbol.is = function (symbol, name) {
    return symbol instanceof _Symbol && typeof name === 'string' && symbol.name === name;
  };

  _Symbol.prototype.toJSON = _Symbol.prototype.toString = function () {
    //return '<#symbol \'' + this.name + '\'>';
    if (isSymbol(this.name)) {
      return this.name.toString();
    }

    return this.name;
  }; // ----------------------------------------------------------------------
  // :: Nil constructor with only once instance
  // ----------------------------------------------------------------------


  function Nil() {}

  Nil.prototype.toString = function () {
    return 'nil';
  };

  var nil = new Nil(); // ----------------------------------------------------------------------
  // :: Pair constructor
  // ----------------------------------------------------------------------

  function Pair(car, cdr) {
    if (typeof this !== 'undefined' && this.constructor !== Pair || typeof this === 'undefined') {
      return new Pair(car, cdr);
    }

    this.car = car;
    this.cdr = cdr;
  } // ----------------------------------------------------------------------


  function emptyList() {
    return new Pair(undefined, nil);
  } // ----------------------------------------------------------------------


  Pair.prototype.flatten = function () {
    return Pair.fromArray(flatten(this.toArray()));
  }; // ----------------------------------------------------------------------


  Pair.prototype.length = function () {
    if (isEmptyList(this)) {
      return 0;
    }

    var len = 0;
    var node = this;

    while (true) {
      if (node === nil) {
        break;
      }

      len++;
      node = node.cdr;
    }

    return len;
  }; // ----------------------------------------------------------------------


  Pair.prototype.clone = function () {
    var cdr = this.cdr;
    var car = this.car;

    if (car instanceof Pair) {
      car = car.clone();
    }

    if (cdr instanceof Pair) {
      cdr = this.cdr.clone();
    }

    return new Pair(car, cdr);
  }; // ----------------------------------------------------------------------


  Pair.prototype.toArray = function () {
    if (this.isEmptyList()) {
      return [];
    }

    var result = [];

    if (this.car instanceof Pair) {
      result.push(this.car.toArray());
    } else {
      result.push(this.car);
    }

    if (this.cdr instanceof Pair) {
      result = result.concat(this.cdr.toArray());
    }

    return result;
  }; // ----------------------------------------------------------------------


  Pair.prototype.isEmptyList = function () {
    return typeof this.car === 'undefined' && this.cdr === nil;
  }; // ----------------------------------------------------------------------


  Pair.fromArray = function (array) {
    if (array instanceof Pair) {
      return array;
    }

    if (array.length && !(array instanceof Array)) {
      array = _toConsumableArray(array);
    }

    if (array.length === 0) {
      return emptyList();
    } else {
      var car;

      if (array[0] instanceof Array) {
        car = Pair.fromArray(array[0]);
      } else {
        car = array[0];
      }

      if (array.length === 1) {
        return new Pair(car, nil);
      } else {
        return new Pair(car, Pair.fromArray(array.slice(1)));
      }
    }
  }; // ----------------------------------------------------------------------


  Pair.prototype.toObject = function () {
    var node = this;
    var result = {};

    while (true) {
      if (node instanceof Pair && node.car instanceof Pair) {
        var pair = node.car;
        var name = pair.car;

        if (name instanceof _Symbol) {
          name = name.name;
        }

        var cdr = pair.cdr;

        if (cdr instanceof Pair) {
          cdr = cdr.toObject();
        }

        result[name] = cdr;
        node = node.cdr;
      } else {
        break;
      }
    }

    return result;
  }; // ----------------------------------------------------------------------


  Pair.fromPairs = function (array) {
    return array.reduce(function (list, pair) {
      return new Pair(new Pair(new _Symbol(pair[0]), pair[1]), list);
    }, nil);
  }; // ----------------------------------------------------------------------


  Pair.fromObject = function (obj) {
    var array = Object.keys(obj).map(function (key) {
      return [key, obj[key]];
    });
    return Pair.fromPairs(array);
  }; // ----------------------------------------------------------------------


  Pair.prototype.reduce = function (fn) {
    var node = this;
    var result = nil;

    while (true) {
      if (node !== nil) {
        result = fn(result, node.car);
        node = node.cdr;
      } else {
        break;
      }
    }

    return result;
  }; // ----------------------------------------------------------------------


  Pair.prototype.reverse = function () {
    var node = this;
    var prev = nil;

    while (node !== nil) {
      var next = node.cdr;
      node.cdr = prev;
      prev = node;
      node = next;
    }

    return prev;
  }; // ----------------------------------------------------------------------


  Pair.prototype.transform = function (fn) {

    function recur(pair) {
      if (pair instanceof Pair) {
        if (pair.replace) {
          delete pair.replace;
          return pair;
        }

        var car = fn(pair.car);

        if (car instanceof Pair) {
          car = recur(car);
        }

        var cdr = fn(pair.cdr);

        if (cdr instanceof Pair) {
          cdr = recur(cdr);
        }

        return new Pair(car, cdr);
      }

      return pair;
    }

    return recur(this);
  }; // ----------------------------------------------------------------------


  Pair.prototype.map = function (fn) {
    if (typeof this.car !== 'undefined') {
      return new Pair(fn(this.car), isEmptyList(this.cdr) ? nil : this.cdr.map(fn));
    } else {
      return nil;
    }
  }; // ----------------------------------------------------------------------


  Pair.prototype.toString = function () {
    var arr = ['('];

    if (this.car !== undefined) {
      if (typeof this.car === 'function') {
        arr.push('<#function ' + (this.car.name || 'anonymous') + '>');
      } else if (typeof this.car === 'string') {
        arr.push(JSON.stringify(this.car));
      } else if (this.car instanceof _Symbol) {
        arr.push(this.car.toString());
      } else if (typeof this.car !== 'undefined') {
        arr.push(this.car);
      }

      if (this.cdr instanceof Pair) {
        arr.push(' ');
        arr.push(this.cdr.toString().replace(/^\(|\)$/g, ''));
      } else if (typeof this.cdr !== 'undefined' && this.cdr !== nil) {
        if (typeof this.cdr === 'string') {
          arr = arr.concat([' . ', JSON.stringify(this.cdr)]);
        } else {
          arr = arr.concat([' . ', this.cdr]);
        }
      }
    }

    arr.push(')');
    return arr.join('');
  }; // ----------------------------------------------------------------------


  Pair.prototype.append = function (pair) {
    if (pair instanceof Array) {
      return this.append(Pair.fromArray(pair));
    }

    var p = this;

    if (p.car === undefined) {
      if (pair instanceof Pair) {
        this.car = pair.car;
        this.cdr = pair.cdr;
      } else {
        return pair;
      }
    } else {
      while (true) {
        if (p instanceof Pair && p.cdr !== nil) {
          p = p.cdr;
        } else {
          break;
        }
      }

      p.cdr = pair;
    }

    return this;
  }; // ----------------------------------------------------------------------


  function equal(x, y) {
    if (x instanceof LNumber && y instanceof LNumber) {
      return x.cmp(y) === 0;
    } else if (typeof x === 'number' || typeof y === 'number') {
      return LNumber(x).cmp(LNumber(y));
    } else if (x instanceof _Symbol && y instanceof _Symbol) {
      return x.name === y.name;
    } else {
      return x === y;
    }
  } // ----------------------------------------------------------------------


  function isEmptyList(x) {
    return x instanceof Pair && x.isEmptyList() || x === nil;
  } // ----------------------------------------------------------------------
  // :: Macro constructor
  // ----------------------------------------------------------------------


  function Macro(name, fn) {
    if (typeof this !== 'undefined' && this.constructor !== Macro || typeof this === 'undefined') {
      return new Macro(name, fn);
    }

    this.name = name;
    this.fn = fn;
  }

  Macro.defmacro = function (name, fn) {
    var macro = new Macro(name, fn);
    macro.defmacro = true;
    return macro;
  };

  Macro.prototype.invoke = function (code, _ref, macro_expand) {
    var env = _ref.env,
        dynamic_scope = _ref.dynamic_scope,
        error = _ref.error;
    var args = {
      dynamic_scope: dynamic_scope,
      error: error,
      macro_expand: macro_expand
    };
    var result = this.fn.call(env, code, args, this.name);
    return macro_expand ? quote(result) : result;
  };

  Macro.prototype.toString = function () {
    return '#<Macro ' + this.name + '>';
  };

  var macro = 'define-macro';

  function macro_expand(single) {
    return (
      /*#__PURE__*/
      function () {
        var _ref2 = _asyncToGenerator(
        /*#__PURE__*/
        regenerator.mark(function _callee2(code, args) {
          var env, traverse, _traverse, new_code;

          return regenerator.wrap(function _callee2$(_context2) {
            while (1) {
              switch (_context2.prev = _context2.next) {
                case 0:
                  _traverse = function _ref4() {
                    _traverse = _asyncToGenerator(
                    /*#__PURE__*/
                    regenerator.mark(function _callee(node) {
                      var value, result, car, cdr, pair;
                      return regenerator.wrap(function _callee$(_context) {
                        while (1) {
                          switch (_context.prev = _context.next) {
                            case 0:
                              if (!(node instanceof Pair && node.car instanceof _Symbol)) {
                                _context.next = 13;
                                break;
                              }

                              _context.prev = 1;
                              value = env.get(node.car);

                              if (!(value instanceof Macro && value.defmacro)) {
                                _context.next = 9;
                                break;
                              }

                              _context.next = 6;
                              return value.invoke(node.cdr, args, true);

                            case 6:
                              result = _context.sent;

                              if (!(result instanceof Pair)) {
                                _context.next = 9;
                                break;
                              }

                              return _context.abrupt("return", result);

                            case 9:
                              _context.next = 13;
                              break;

                            case 11:
                              _context.prev = 11;
                              _context.t0 = _context["catch"](1);

                            case 13:
                              car = node.car;

                              if (!(car instanceof Pair)) {
                                _context.next = 18;
                                break;
                              }

                              _context.next = 17;
                              return traverse(car);

                            case 17:
                              car = _context.sent;

                            case 18:
                              cdr = node.cdr;

                              if (!(cdr instanceof Pair)) {
                                _context.next = 23;
                                break;
                              }

                              _context.next = 22;
                              return traverse(cdr);

                            case 22:
                              cdr = _context.sent;

                            case 23:
                              pair = new Pair(car, cdr);
                              return _context.abrupt("return", pair);

                            case 25:
                            case "end":
                              return _context.stop();
                          }
                        }
                      }, _callee, this, [[1, 11]]);
                    }));
                    return _traverse.apply(this, arguments);
                  };

                  traverse = function _ref3(_x3) {
                    return _traverse.apply(this, arguments);
                  };

                  env = args['env'] = this;
                  new_code = code;

                  if (!single) {
                    _context2.next = 12;
                    break;
                  }

                  _context2.t0 = quote;
                  _context2.next = 8;
                  return traverse(code);

                case 8:
                  _context2.t1 = _context2.sent.car;
                  return _context2.abrupt("return", (0, _context2.t0)(_context2.t1));

                case 12:

                  _context2.next = 15;
                  return traverse(code);

                case 15:
                  new_code = _context2.sent;

                  if (!(code.toString() === new_code.toString())) {
                    _context2.next = 18;
                    break;
                  }

                  return _context2.abrupt("break", 21);

                case 18:
                  code = new_code;
                  _context2.next = 12;
                  break;

                case 21:
                  return _context2.abrupt("return", quote(new_code.car));

                case 22:
                case "end":
                  return _context2.stop();
              }
            }
          }, _callee2, this);
        }));

        return function (_x, _x2) {
          return _ref2.apply(this, arguments);
        };
      }()
    );
  } // ----------------------------------------------------------------------
  // :: check for nullish values


  function is_null(value) {
    return typeof value === 'undefined' || value === nil || value === null;
  } // ----------------------------------------------------------------------
  // :: function that return macro for let and let*
  // ----------------------------------------------------------------------


  function let_macro(asterisk) {
    var name = 'let' + (asterisk ? '*' : '');
    return Macro.defmacro(name, function (code, _ref5) {
      var dynamic_scope = _ref5.dynamic_scope,
          error = _ref5.error,
          macro_expand = _ref5.macro_expand;
      var args; // named let:
      // (let iter ((x 10)) (iter (- x 1))) -> (let* ((iter (lambda (x) ...

      if (code.car instanceof _Symbol) {
        if (!(code.cdr.car instanceof Pair)) {
          throw new Error('let require list of pairs');
        }

        var params = code.cdr.car.map(function (pair) {
          return pair.car;
        });
        args = code.cdr.car.map(function (pair) {
          return pair.cdr.car;
        });
        return Pair.fromArray([_Symbol('let*'), [[code.car, Pair(_Symbol('lambda'), Pair(params, code.cdr.cdr))]], Pair(code.car, args)]);
      } else if (macro_expand) {
        // Macro.defmacro are special macros that should return lisp code
        // here we use evaluate, so we need to check special flag set by
        // macroexpand to prevent evaluation of code in normal let
        return;
      }

      var self = this;
      args = this.get('list->array')(code.car);
      var env = self.inherit('let');
      return new Promise(function (resolve) {
        var promises = [];
        var i = 0;

        (function loop() {
          var pair = args[i++];

          function set(value) {
            if (value instanceof Promise) {
              promises.push(value);
              return value.then(set);
            } else if (typeof value === 'undefined') {
              env.set(pair.car, nil);
            } else {
              env.set(pair.car, value);
            }
          }

          if (dynamic_scope) {
            dynamic_scope = asterisk ? env : self;
          }

          if (!pair) {
            var output = new Pair(new _Symbol('begin'), code.cdr);
            evaluate(output, {
              env: env,
              dynamic_scope: dynamic_scope,
              error: error
            }).then(function (result) {
              resolve(result);
            });
          } else {
            var value = evaluate(pair.cdr.car, {
              env: asterisk ? env : self,
              dynamic_scope: dynamic_scope,
              error: error
            });
            var promise = set(value);

            if (promise instanceof Promise) {
              promise.then(function () {
                Promise.all(promises).then(loop);
              });
            } else {
              loop();
            }
          }
        })();
      });
    });
  } // ----------------------------------------------------------------------
  // :: Number wrapper that handle BigNumbers
  // ----------------------------------------------------------------------


  function LNumber(n, float) {
    if (n instanceof LNumber) {
      return n;
    }

    if (typeof this !== 'undefined' && this.constructor !== LNumber || typeof this === 'undefined') {
      return new LNumber(n, float === true ? true : undefined);
    }

    if (!LNumber.isNumber(n)) {
      throw new Error("You can't create LNumber from " + _typeof(n));
    } // prevent infite loop https://github.com/indutny/bn.js/issues/186


    if (n === null) {
      n = 0;
    }

    if (LNumber.isFloat(n)) {
      this.value = n;
    } else if (float) {
      this.value = n;
      this.float = true;
    } else if (typeof BigInt !== 'undefined') {
      if (typeof n !== 'bigint') {
        this.value = BigInt(n);
      } else {
        this.value = n;
      }
    } else if (typeof BN !== 'undefined' && !(n instanceof BN)) {
      this.value = new BN(n);
    } else {
      this.value = n;
    }
  } // ----------------------------------------------------------------------


  LNumber.isFloat = function isFloat(n) {
    return Number(n) === n && n % 1 !== 0;
  }; // ----------------------------------------------------------------------


  LNumber.isNumber = function (n) {
    return n instanceof LNumber || LNumber.isNative(n) || LNumber.isBN(n);
  }; // ----------------------------------------------------------------------


  LNumber.isNative = function (n) {
    return typeof n === 'bigint' || typeof n === 'number';
  }; // ----------------------------------------------------------------------


  LNumber.isBN = function (n) {
    return typeof BN !== 'undefined' && n instanceof BN;
  }; // ----------------------------------------------------------------------


  LNumber.prototype.toString = LNumber.prototype.toJSON = function () {
    return this.value.toString();
  }; // ----------------------------------------------------------------------


  ['floor', 'ceil', 'round'].forEach(function (fn) {
    LNumber.prototype[fn] = function () {
      if (this.float || LNumber.isFloat(this.value)) {
        return LNumber(Math[fn](this.value));
      } else {
        return LNumber(this.value);
      }
    };
  }); // ----------------------------------------------------------------------

  LNumber.prototype.valueOf = function () {
    if (LNumber.isNative(this.value)) {
      return Number(this.value);
    } else if (LNumber.isBN(this.value)) {
      return this.value.toNumber();
    }
  }; // ----------------------------------------------------------------------


  LNumber.prototype.coerce = function (n) {
    if (n === null) {
      n = 0;
    }

    var value;

    if (n instanceof LNumber) {
      value = n.value;
    } else {
      value = n;
    }

    if (LNumber.isFloat(value)) {// skip
    } else if (typeof this.value === 'bigint' && typeof value !== 'bigint') {
      value = BigInt(value);
    } else if (typeof BN !== 'undefined' && this.value instanceof BN && !value instanceof BN) {
      value = new BN(value);
    }

    return LNumber(value);
  }; // ----------------------------------------------------------------------


  LNumber.prototype.op = function (op, n) {
    var ops = {
      '*': function _(a, b) {
        return a * b;
      },
      '+': function _(a, b) {
        return a + b;
      },
      '-': function _(a, b) {
        return a - b;
      },
      '/': function _(a, b) {
        return a / b;
      },
      '%': function _(a, b) {
        return a % b;
      },
      '|': function _(a, b) {
        return a | b;
      },
      '&': function _(a, b) {
        return a & b;
      },
      '~': function _(a) {
        return ~a;
      },
      '>>': function _(a, b) {
        return a >> b;
      },
      '<<': function _(a, b) {
        return a << b;
      }
    };

    if (LNumber.isFloat(n) || n instanceof LNumber && (LNumber.isFloat(n.value) || n.float) || LNumber.isFloat(this.value) || this.float) {
      var value = n instanceof LNumber ? n.valueOf() : n;
      return LNumber(ops[op](this.valueOf(), value));
    }

    n = this.coerce(n);

    if (LNumber.isNative(n.value) && LNumber.isNative(this.value)) {
      return LNumber(ops[op](this.value, n.value));
    }

    if (LNumber.isBN(this.value) && LNumber.isBN(n.value)) {
      var bn_op = {
        '+': 'iadd',
        '-': 'isub',
        '*': 'imul',
        '/': 'idiv',
        '%': 'imod',
        '|': 'ior',
        '&': 'iand',
        '~': 'inot',
        '<<': 'ishrn',
        '>>': 'ishln'
      };
      op = bn_op[op];
      return this.value.clone()[op](n, value);
    }
  }; // ----------------------------------------------------------------------


  var ops = {
    '+': 'add',
    '-': 'sub',
    '*': 'mul',
    '/': 'div',
    '%': 'mod',
    '|': 'or',
    '&': 'and',
    '~': 'neg',
    '<<': 'shl',
    '>>': 'shr'
  };
  Object.keys(ops).forEach(function (op) {
    LNumber.prototype[ops[op]] = function (n) {
      return this.op(op, n);
    };
  }); // ----------------------------------------------------------------------

  LNumber.prototype.sqrt = function () {
    var value;

    if (LNumber.isNative(this.value)) {
      value = Math.sqrt(this.value);
    } else if (LNumber.isBN(this.value)) {
      value = this.value.sqrt();
    }

    return new LNumber(value);
  }; // ----------------------------------------------------------------------


  LNumber.prototype.pow = function (n) {
    n = this.coerce(n);

    if (LNumber.isNative(this.value)) {
      try {
        var pow = new Function('a,b', 'return a**b;');
        n.value = pow(this.value, n.value);
      } catch (e) {
        throw new Error("Power operator not supported");
      }
    } else if (LNumber.isBN(this.value)) {
      n.value = this.value.pow(n.value);
    } else {
      n.value = Math.pow(this.value, n.value);
    }

    return n;
  }; // ----------------------------------------------------------------------


  LNumber.prototype.neg = function () {
    var value = this.value;

    if (LNumber.isNative(value)) {
      value = -value;
    } else if (LNumber.isBN(value)) {
      value = value.neg();
    }

    return new LNumber(value);
  }; // ----------------------------------------------------------------------


  LNumber.prototype.abs = function () {
    var value = this.value;

    if (LNumber.isNative(this.value)) {
      if (value < 0) {
        value = -value;
      }
    } else if (LNumber.isBN(value)) {
      value.iabs();
    }

    return new LNumber(value);
  }; // ----------------------------------------------------------------------


  LNumber.prototype.isOdd = function () {
    if (LNumber.isNative(this.value)) {
      return this.value % 2 === 1;
    } else if (LNumber.isBN(this.value)) {
      return this.value.isOdd();
    }
  }; // ----------------------------------------------------------------------


  LNumber.prototype.isEven = function () {
    return !this.isOdd();
  }; // ----------------------------------------------------------------------


  LNumber.prototype.cmp = function (n) {
    n = this.coerce(n);

    if (LNumber.isNative(this.value)) {
      if (this.value < n.value) {
        return -1;
      } else if (this.value === n.value) {
        return 0;
      } else {
        return 1;
      }
    } else if (LNumber.isBN(this.value)) {
      return this.value.cmp(n.value);
    }
  }; // ----------------------------------------------------------------------
  // :: Environment constructor (parent and name arguments are optional)
  // ----------------------------------------------------------------------


  function Environment(obj, parent, name) {
    this.env = obj;
    this.parent = parent;
    this.name = name;
  } // ----------------------------------------------------------------------


  Environment.prototype.inherit = function (name) {
    var obj = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

    if (_typeof(name) === "object") {
      obj = name;
    }

    if (!name || _typeof(name) === "object") {
      name = 'child of ' + (this.name || 'unknown');
    }

    return new Environment(obj || {}, this, name);
  }; // ----------------------------------------------------------------------


  Environment.prototype.get = function (symbol) {
    var value;
    var defined = false;

    if (symbol instanceof _Symbol) {
      if (symbol.name in this.env) {
        value = this.env[symbol.name];
        defined = true;
      }
    } else if (typeof symbol === 'string') {
      if (typeof this.env[symbol] !== 'undefined') {
        value = this.env[symbol];
        defined = true;
      }
    }

    if (defined) {
      if (LNumber.isNumber(value)) {
        return LNumber(value);
      }

      return value;
    }

    if (this.parent instanceof Environment) {
      return this.parent.get(symbol);
    } else {
      var name;

      if (symbol instanceof _Symbol) {
        name = symbol.name;
      } else if (typeof symbol === 'string') {
        name = symbol;
      }

      if (name) {
        var type = _typeof(root[name]);

        if (type === 'function') {
          return root[name].bind(root);
        } else if (type !== 'undefined') {
          return root[name];
        }
      }
    }

    name = (name.name || name).toString();
    throw new Error("Unbound variable `" + name + "'");
  }; // ----------------------------------------------------------------------


  Environment.prototype.set = function (name, value) {
    if (LNumber.isNumber(value)) {
      value = LNumber(value);
    }

    if (name instanceof _Symbol) {
      name = name.name;
    }

    this.env[name] = value;
  }; // ----------------------------------------------------------------------


  Environment.prototype.has = function (name) {
    return typeof this.env[name] !== 'undefined';
  }; // ----------------------------------------------------------------------


  Environment.prototype.ref = function (name) {
    var env = this;

    while (true) {
      if (!env) {
        break;
      }

      if (env.has(name)) {
        return env;
      }

      env = env.parent;
    }
  }; // ----------------------------------------------------------------------
  // :: Quote funtion used to pause evaluation from Macro
  // ----------------------------------------------------------------------


  function quote(value) {
    if (value instanceof Promise) {
      return value.then(quote);
    }

    if (value instanceof Pair || value instanceof _Symbol) {
      value.data = true;
    }

    return value;
  } // ----------------------------------------------------------------------
  // :: Unquote is used for multiple backticks and unquote
  // ----------------------------------------------------------------------


  function Unquote(value, count) {
    this.value = value;
    this.count = count;
  }

  Unquote.prototype.toString = function () {
    return '<#unquote[' + this.count + '] ' + this.value + '>';
  }; // ----------------------------------------------------------------------


  var gensym = function () {
    var count = 0;
    return function () {
      var name = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;

      // use ES6 symbol as name for lips symbol to they are uniq
      if (name !== null) {
        return new _Symbol(root.Symbol('#' + name));
      }

      count++;
      return new _Symbol(root.Symbol('#' + count));
    };
  }();

  var global_env = new Environment({
    nil: nil,
    'true': true,
    'false': false,
    // ------------------------------------------------------------------
    stdout: {
      write: function write() {
        var _console;

        (_console = console).log.apply(_console, arguments);
      }
    },
    // ------------------------------------------------------------------
    stdin: {
      read: function read() {
        return new Promise(function (resolve) {
          resolve(prompt(''));
        });
      }
    },
    test: function test() {
      return Promise.resolve(undefined);
    },
    // ------------------------------------------------------------------
    cons: function cons(car, cdr) {
      if (isEmptyList(cdr)) {
        cdr = nil;
      }

      return new Pair(car, cdr);
    },
    // ------------------------------------------------------------------
    car: function car(list) {
      if (list instanceof Pair) {
        return list.car;
      } else {
        throw new Error('argument to car need to be a list');
      }
    },
    // ------------------------------------------------------------------
    cdr: function cdr(list) {
      if (list instanceof Pair) {
        return list.cdr;
      } else {
        throw new Error('argument to cdr need to be a list');
      }
    },
    // ------------------------------------------------------------------
    'set!': new Macro('set!', function (code) {
      var _ref6 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref6.dynamic_scope,
          error = _ref6.error;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      var value = evaluate(code.cdr.car, {
        env: this,
        dynamic_scope: dynamic_scope,
        error: error
      });
      value = maybe_promise(value);
      var ref;

      if (code.car instanceof Pair && _Symbol.is(code.car.car, '.')) {
        var second = code.car.cdr.car;
        var thrid = code.car.cdr.cdr.car;
        var object = evaluate(second, {
          env: this,
          dynamic_scope: dynamic_scope,
          error: error
        });
        var key = evaluate(thrid, {
          env: this,
          dynamic_scope: dynamic_scope,
          error: error
        });

        if (value instanceof Promise) {
          return value.then(function (value) {
            object[key] = value;
          });
        } else {
          object[key] = value;
          return value;
        }
      }

      if (!(code.car instanceof _Symbol)) {
        throw new Error('set! first argument need to be a symbol');
      }

      ref = this.ref(code.car.name);

      if (!ref) {
        ref = this;
      }

      if (value instanceof Promise) {
        return value.then(function (value) {
          return ref.set(code.car, value);
        });
      } else {
        ref.set(code.car, value);
      }
    }),
    // ------------------------------------------------------------------
    'set-car!': function setCar(slot, value) {
      slot.car = value;
    },
    // ------------------------------------------------------------------
    'set-cdr!': function setCdr(slot, value) {
      slot.cdr = value;
    },
    'empty?': function empty(x) {
      return typeof x === 'undefined' || isEmptyList(x);
    },
    // ------------------------------------------------------------------
    assoc: function assoc(key, list) {
      if (key instanceof Pair && !(list instanceof Pair)) {
        throw new Error('First argument to assoc new to a key');
      }

      var node = list;

      while (true) {
        if (!(node instanceof Pair) || this.get('empty?')(node)) {
          break;
        }

        var car = node.car.car;

        if (equal(car, key)) {
          return node.car;
        } else {
          node = node.cdr;
        }
      }

      return nil;
    },
    // ------------------------------------------------------------------
    gensym: gensym,
    // ------------------------------------------------------------------
    load: function load(file) {
      var _this = this;

      root.fetch(file).then(function (res) {
        return res.text();
      }).then(function (code) {
        _this.get('eval')(_this.get('read')(code));
      });
    },
    // ------------------------------------------------------------------
    'while': new Macro('while',
    /*#__PURE__*/
    function () {
      var _ref7 = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee3(code, _ref8) {
        var dynamic_scope, error, self, begin, result, cond;
        return regenerator.wrap(function _callee3$(_context3) {
          while (1) {
            switch (_context3.prev = _context3.next) {
              case 0:
                dynamic_scope = _ref8.dynamic_scope, error = _ref8.error;
                self = this;
                begin = new Pair(new _Symbol('begin'), code.cdr);

                if (dynamic_scope) {
                  dynamic_scope = self;
                }

              case 4:

                _context3.next = 7;
                return evaluate(code.car, {
                  env: self,
                  dynamic_scope: dynamic_scope,
                  error: error
                });

              case 7:
                cond = _context3.sent;

                if (!(cond && !isEmptyList(cond))) {
                  _context3.next = 14;
                  break;
                }

                _context3.next = 11;
                return evaluate(begin, {
                  env: self,
                  dynamic_scope: dynamic_scope,
                  error: error
                });

              case 11:
                result = _context3.sent;
                _context3.next = 15;
                break;

              case 14:
                return _context3.abrupt("return", result);

              case 15:
                _context3.next = 4;
                break;

              case 17:
              case "end":
                return _context3.stop();
            }
          }
        }, _callee3, this);
      }));

      return function (_x4, _x5) {
        return _ref7.apply(this, arguments);
      };
    }()),
    // ------------------------------------------------------------------
    'if': new Macro('if', function (code, _ref9) {
      var dynamic_scope = _ref9.dynamic_scope,
          error = _ref9.error;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      var env = this;

      var resolve = function resolve(cond) {
        if (cond && !isEmptyList(cond)) {
          var true_value = evaluate(code.cdr.car, {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          });

          if (typeof true_value === 'undefined') {
            return;
          }

          return true_value;
        } else {
          var false_value = evaluate(code.cdr.cdr.car, {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          });

          if (typeof false_value === 'undefined') {
            return false;
          }

          return false_value;
        }
      };

      var cond = evaluate(code.car, {
        env: env,
        dynamic_scope: dynamic_scope,
        error: error
      });

      if (cond instanceof Promise) {
        return cond.then(resolve);
      } else {
        return resolve(cond);
      }
    }),
    // ------------------------------------------------------------------
    'let*': let_macro(true),
    // ------------------------------------------------------------------
    'let': let_macro(false),
    // ------------------------------------------------------------------
    'begin': new Macro('begin',
    /*#__PURE__*/
    function () {
      var _ref10 = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee4(code, _ref11) {
        var dynamic_scope, error, arr, result;
        return regenerator.wrap(function _callee4$(_context4) {
          while (1) {
            switch (_context4.prev = _context4.next) {
              case 0:
                dynamic_scope = _ref11.dynamic_scope, error = _ref11.error;
                arr = this.get('list->array')(code);

                if (dynamic_scope) {
                  dynamic_scope = this;
                }

              case 3:

                if (!arr.length) {
                  _context4.next = 11;
                  break;
                }

                code = arr.shift();
                _context4.next = 8;
                return evaluate(code, {
                  env: this,
                  dynamic_scope: dynamic_scope,
                  error: error
                });

              case 8:
                result = _context4.sent;
                _context4.next = 12;
                break;

              case 11:
                return _context4.abrupt("return", result);

              case 12:
                _context4.next = 3;
                break;

              case 14:
              case "end":
                return _context4.stop();
            }
          }
        }, _callee4, this);
      }));

      return function (_x6, _x7) {
        return _ref10.apply(this, arguments);
      };
    }()),
    // ------------------------------------------------------------------
    timer: new Macro('timer', function (code) {
      var _ref12 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref12.dynamic_scope,
          error = _ref12.error;

      var env = this;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      return new Promise(function (resolve) {
        setTimeout(function () {
          resolve(evaluate(code.cdr, {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          }));
        }, code.car);
      });
    }),
    // ------------------------------------------------------------------
    define: Macro.defmacro('define', function (code, eval_args) {
      var env = this;

      if (code.car instanceof Pair && code.car.car instanceof _Symbol) {
        var new_code = new Pair(new _Symbol("define"), new Pair(code.car.car, new Pair(new Pair(new _Symbol("lambda"), new Pair(code.car.cdr, code.cdr)))));
        return new_code;
      } else if (eval_args.macro_expand) {
        // prevent evaluation in macroexpand
        return;
      }

      if (eval_args.dynamic_scope) {
        eval_args.dynamic_scope = this;
      }

      eval_args.env = env;
      var value = code.cdr.car;

      if (value instanceof Pair) {
        value = evaluate(value, eval_args);
      }

      if (code.car instanceof _Symbol) {
        if (value instanceof Promise) {
          return value.then(function (value) {
            env.set(code.car, value);
          });
        } else {
          env.set(code.car, value);
        }
      }
    }),
    // ------------------------------------------------------------------
    'set-obj': function setObj(obj, key, value) {
      obj[key] = value;
    },
    // ------------------------------------------------------------------
    'eval': function _eval(code) {
      var _this2 = this;

      if (code instanceof Pair) {
        return evaluate(code, {
          env: this,
          dynamic_scope: this,
          error: function error(e) {
            return _this2.get('print')(e.message);
          }
        });
      }

      if (code instanceof Array) {
        var result;
        code.forEach(function (code) {
          result = evaluate(code, {
            env: _this2,
            dynamic_scope: _this2,
            error: function error(e) {
              return _this2.get('print')(e.message);
            }
          });
        });
        return result;
      }
    },
    // ------------------------------------------------------------------
    lambda: new Macro('lambda', function (code) {
      var _ref13 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref13.dynamic_scope,
          error = _ref13.error;

      var self = this;

      function lambda() {
        var env = (dynamic_scope ? this : self).inherit('lambda');
        var name = code.car;
        var i = 0;
        var value;

        if (name instanceof _Symbol || !isEmptyList(name)) {
          for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
            args[_key] = arguments[_key];
          }

          while (true) {
            if (name.car !== nil) {
              if (name instanceof _Symbol) {
                // rest argument,  can also be first argument
                value = Pair.fromArray(args.slice(i));
                env.env[name.name] = value;
                break;
              } else {
                if (typeof args[i] === 'undefined') {
                  value = nil;
                } else {
                  value = args[i];
                }

                env.env[name.car.name] = value;
              }
            }

            if (name.cdr === nil) {
              break;
            }

            i++;
            name = name.cdr;
          }
        }

        if (dynamic_scope) {
          dynamic_scope = env;
        }

        var output = new Pair(new _Symbol('begin'), code.cdr);
        return evaluate(output, {
          env: env,
          dynamic_scope: dynamic_scope,
          error: error
        });
      }

      var length = code.car instanceof Pair ? code.car.length() : null;

      if (!(code.car instanceof Pair)) {
        return lambda; // variable arguments
      } // list of arguments (name don't matter


      var args = new Array(length).fill(0).map(function (_, i) {
        return 'a' + i;
      }).join(','); // hack that create function with specific length

      var wrapper = new Function("f", "return function(".concat(args, ") {\n                return f.call(this, ").concat(args, ");\n            };"));
      return wrapper(lambda);
    }),
    'macroexpand': new Macro('macro-expand', macro_expand()),
    'macroexpand-1': new Macro('macro-expand', macro_expand(true)),
    // ------------------------------------------------------------------
    'define-macro': new Macro(macro, function (macro, _ref14) {
      var dynamic_scope = _ref14.dynamic_scope,
          error = _ref14.error;

      function clear(node) {
        if (node instanceof Pair) {
          delete node.data;
        }

        return node;
      }

      if (macro.car instanceof Pair && macro.car.car instanceof _Symbol) {
        var name = macro.car.car.name;
        this.env[name] = Macro.defmacro(name, function (code, _ref15) {
          var macro_expand = _ref15.macro_expand;
          var env = new Environment({}, this, 'defmacro');
          var name = macro.car.cdr;
          var arg = code;

          while (true) {
            if (name instanceof _Symbol) {
              env.env[name.name] = arg;
              break;
            } else if (name.car !== nil && arg.car !== nil) {
              env.env[name.car.name] = arg.car;
            }

            if (name.cdr === nil) {
              break;
            }

            arg = arg.cdr;
            name = name.cdr;
          }

          if (dynamic_scope) {
            dynamic_scope = env;
          } // evaluate macro


          if (macro.cdr instanceof Pair) {
            var pair = macro.cdr.reduce(function (result, node) {
              return evaluate(node, {
                env: env,
                dynamic_scope: dynamic_scope,
                error: error
              });
            });

            if (macro_expand) {
              return pair;
            }

            pair = evaluate(pair, {
              env: env,
              dynamic_scope: dynamic_scope,
              error: error
            });

            if (pair instanceof Promise) {
              return pair.then(clear);
            }

            return clear(pair);
          }
        });
      }
    }),
    // ------------------------------------------------------------------
    quote: new Macro('quote', function (arg) {
      return quote(arg.car);
    }),
    // ------------------------------------------------------------------
    quasiquote: new Macro('quasiquote', function (arg, _ref16) {
      var dynamic_scope = _ref16.dynamic_scope,
          error = _ref16.error;
      var self = this;
      var max_unquote = 0;

      if (dynamic_scope) {
        dynamic_scope = self;
      }

      function recur(pair) {
        if (pair instanceof Pair) {
          var eval_pair;

          if (_Symbol.is(pair.car.car, 'unquote-splicing')) {
            eval_pair = evaluate(pair.car.cdr.car, {
              env: self,
              dynamic_scope: dynamic_scope,
              error: error
            });

            if (!eval_pair instanceof Pair) {
              throw new Error('Value of unquote-splicing need' + ' to be pair');
            }

            if (pair.cdr instanceof Pair) {
              if (eval_pair instanceof Pair) {
                eval_pair.cdr.append(recur(pair.cdr));
              } else {
                eval_pair = new Pair(eval_pair, recur(pair.cdr));
              }
            }

            return eval_pair;
          }

          if (_Symbol.is(pair.car, 'unquote')) {
            var head = pair.cdr;
            var node = head;
            var parent = node;
            var unquote_count = 1;

            while (_Symbol.is(node.car.car, 'unquote')) {
              parent = node;
              unquote_count++;
              node = node.car.cdr.car;
            }

            if (unquote_count > max_unquote) {
              max_unquote = unquote_count;
            } // we use Unquote to proccess inner most unquote first
            // in unquote function afer processing whole s-expression


            if (parent === node) {
              if (pair.cdr.cdr !== nil) {
                return new Pair(new Unquote(pair.cdr.car, unquote_count), pair.cdr.cdr);
              } else {
                return new Unquote(pair.cdr.car, unquote_count);
              }
            } else if (parent.cdr.cdr !== nil) {
              parent.car.cdr = new Pair(new Unquote(node, unquote_count), parent.cdr === nil ? nil : parent.cdr.cdr);
            } else {
              parent.car.cdr = new Unquote(node, unquote_count);
            }

            return head.car;
          }

          var car = pair.car;

          if (car instanceof Pair) {
            car = recur(car);
          }

          var cdr = pair.cdr;

          if (cdr instanceof Pair) {
            cdr = recur(cdr);
          }

          return new Pair(car, cdr);
        }

        return pair;
      }

      function unquoting(pair) {
        if (pair instanceof Unquote) {
          if (max_unquote === pair.count) {
            return evaluate(pair.value, {
              env: self,
              dynamic_scope: dynamic_scope,
              error: error
            });
          } else {
            return new Pair(new _Symbol('unquote'), new Pair(unquoting(pair.value), nil));
          }
        }

        if (pair instanceof Pair) {
          var car = pair.car;

          if (car instanceof Pair || car instanceof Unquote) {
            car = unquoting(car);
          }

          var cdr = pair.cdr;

          if (cdr instanceof Pair || cdr instanceof Unquote) {
            cdr = unquoting(cdr);
          }

          return new Pair(car, cdr);
        }

        return pair;
      }

      return quote(unquoting(recur(arg.car)));
    }),
    // ------------------------------------------------------------------
    clone: function clone(list) {
      return list.clone();
    },
    // ------------------------------------------------------------------
    append: function append(list, item) {
      return list.clone().append([item]);
    },
    // ------------------------------------------------------------------
    'append!': new Macro('append!', function (code, _ref17) {
      var dynamic_scope = _ref17.dynamic_scope,
          error = _ref17.error;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      var value = evaluate(code.cdr.car, {
        env: this,
        dynamic_scope: dynamic_scope,
        error: error
      });
      return this.get(code.car).append([value]);
    }),
    // ------------------------------------------------------------------
    list: function list() {
      return Pair.fromArray([].slice.call(arguments));
    },
    concat: function concat() {
      return [].join.call(arguments, '');
    },
    // ------------------------------------------------------------------
    join: function join(separator, list) {
      return this.get('list->array')(list).join(separator);
    },
    // ------------------------------------------------------------------
    split: function split(string, separator) {
      return this.get('array->list')(string.split(separator));
    },
    // ------------------------------------------------------------------
    replace: function replace(string, pattern, replacement) {
      return string.replace(pattern, replacement);
    },
    // ------------------------------------------------------------------
    match: function match(string, pattern) {
      return this.get('array->list')(string.match(pattern));
    },
    // ------------------------------------------------------------------
    search: function search(string, pattern) {
      return string.search(pattern);
    },
    // ------------------------------------------------------------------
    string: function string(obj, quote) {
      if (typeof jQuery !== 'undefined' && obj instanceof jQuery.fn.init) {
        return '<#jQuery(' + obj.length + ')>';
      }

      if (obj instanceof LNumber) {
        return obj.value.toString();
      }

      if (typeof obj === 'undefined') {
        return '<#undefined>';
      }

      if (typeof obj === 'function') {
        if (obj.toString().match(/\{\s*\[native code\]\s*\}/)) {
          return '<#function(native)>';
        }

        return '<#function>';
      }

      if (obj === nil) {
        return 'nil';
      }

      if (obj instanceof Array) {
        return '[' + obj.map(function (x) {
          return string(x, true);
        }).join(', ') + ']';
      }

      if (obj === null || typeof obj === 'string' && quote) {
        return JSON.stringify(obj);
      }

      if (obj instanceof Pair) {
        return obj.toString();
      }

      if (_typeof(obj) === 'object') {
        var name = obj.constructor.name;

        if (name !== '') {
          return '<#' + name + '>';
        }

        return '<#Object>';
      }

      if (typeof obj !== 'string') {
        return obj.toString();
      }

      return obj;
    },
    // ------------------------------------------------------------------
    env: function env(_env) {
      _env = _env || this;
      var names = Object.keys(_env.env);
      var result;

      if (names.length) {
        result = Pair.fromArray(names);
      } else {
        result = nil;
      }

      if (_env.parent !== undefined) {
        return this.get('env').call(this, _env.parent).append(result);
      }

      return result;
    },
    'new': function _new(obj) {
      for (var _len2 = arguments.length, args = new Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
        args[_key2 - 1] = arguments[_key2];
      }

      return _construct(obj, args);
    },
    // ------------------------------------------------------------------
    '.': function _(obj, arg) {
      var name = arg instanceof _Symbol ? arg.name : arg;
      var value = obj[name];

      if (typeof value === 'function') {
        return value.bind(obj);
      }

      return value;
    },
    type: function type(obj) {
      return _typeof(obj);
    },
    'instanceof': function _instanceof(obj, type) {
      return obj instanceof type;
    },
    // ------------------------------------------------------------------
    read: function read(arg) {
      var _this3 = this;

      if (typeof arg === 'string') {
        arg = parse(tokenize(arg));

        if (arg.length) {
          return arg[arg.length - 1];
        }

        return emptyList();
      }

      return this.get('stdin').read().then(function (text) {
        return read.call(_this3, text);
      });
    },
    // ------------------------------------------------------------------
    print: function print() {
      var _this$get,
          _this4 = this;

      for (var _len3 = arguments.length, args = new Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
        args[_key3] = arguments[_key3];
      }

      (_this$get = this.get('stdout')).write.apply(_this$get, _toConsumableArray(args.map(function (arg) {
        return _this4.get('string')(arg);
      })));
    },
    // ------------------------------------------------------------------
    flatten: function flatten(list) {
      return list.flatten();
    },
    // ------------------------------------------------------------------
    'array->list': function arrayList(array) {
      return Pair.fromArray(array);
    },
    // ------------------------------------------------------------------
    'list->array': function listArray(list) {
      if (list instanceof Pair && list.isEmptyList()) {
        return [];
      }

      var result = [];
      var node = list;

      while (true) {
        if (node instanceof Pair) {
          result.push(node.car);
          node = node.cdr;
        } else {
          break;
        }
      }

      return result;
    },
    // ------------------------------------------------------------------
    apply: new Macro('apply', function (code) {
      var _this5 = this;

      var _ref18 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref18.dynamic_scope,
          error = _ref18.error;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      function type_check(fn) {
        if (typeof fn !== 'function') {
          var message;

          if (code.car instanceof _Symbol) {
            message = "Variable `" + code.car.name + "' is not a function";
          } else {
            message = "Expression `" + code.car.toString() + "' is not a function";
          }

          throw new Error(message);
        }
      }

      var invoke = function invoke(fn) {
        type_check(fn);
        var args = evaluate(code.cdr.car, {
          env: _this5,
          dynamic_scope: dynamic_scope,
          error: error
        });
        args = _this5.get('list->array')(args);

        if (args.filter(function (a) {
          return a instanceof Promise;
        }).length) {
          return Promise.all(args).then(function (args) {
            return fn.apply(_this5, args);
          });
        } else {
          return fn.apply(_this5, args);
        }
      };

      var fn = evaluate(code.car, {
        env: this,
        dynamic_scope: dynamic_scope,
        error: error
      });

      if (fn instanceof Promise) {
        return fn.then(invoke);
      } else {
        return invoke(fn);
      }
    }),
    // ------------------------------------------------------------------
    'length': function length(obj) {
      if (!obj) {
        return LNumber(0);
      }

      if (obj instanceof Pair) {
        return LNumber(obj.length());
      }

      if ("length" in obj) {
        return LNumber(obj.length);
      }
    },
    // ------------------------------------------------------------------
    find: function () {
      var _find = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee5(fn, list) {
        var array, i;
        return regenerator.wrap(function _callee5$(_context5) {
          while (1) {
            switch (_context5.prev = _context5.next) {
              case 0:
                array = this.get('list->array')(list);
                i = 0;

              case 2:
                if (!(i < array.length)) {
                  _context5.next = 10;
                  break;
                }

                _context5.next = 5;
                return fn(array[i], i);

              case 5:
                if (!_context5.sent) {
                  _context5.next = 7;
                  break;
                }

                return _context5.abrupt("return", array[i]);

              case 7:
                ++i;
                _context5.next = 2;
                break;

              case 10:
              case "end":
                return _context5.stop();
            }
          }
        }, _callee5, this);
      }));

      return function find(_x8, _x9) {
        return _find.apply(this, arguments);
      };
    }(),
    // ------------------------------------------------------------------
    'for-each': function () {
      var _forEach = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee6(fn, list) {
        var array, i, item;
        return regenerator.wrap(function _callee6$(_context6) {
          while (1) {
            switch (_context6.prev = _context6.next) {
              case 0:
                array = this.get('list->array')(list);
                i = 0;

              case 2:
                if (!(i < array.length)) {
                  _context6.next = 8;
                  break;
                }

                item = array[i++];
                _context6.next = 6;
                return fn(item, i);

              case 6:
                _context6.next = 2;
                break;

              case 8:
              case "end":
                return _context6.stop();
            }
          }
        }, _callee6, this);
      }));

      return function forEach(_x10, _x11) {
        return _forEach.apply(this, arguments);
      };
    }(),
    // ------------------------------------------------------------------
    map: function () {
      var _map = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee7(fn, list) {
        var array, result, i, item;
        return regenerator.wrap(function _callee7$(_context7) {
          while (1) {
            switch (_context7.prev = _context7.next) {
              case 0:
                array = this.get('list->array')(list);
                result = [];
                i = 0;

              case 3:
                if (!(i < array.length)) {
                  _context7.next = 12;
                  break;
                }

                item = array[i++];
                _context7.t0 = result;
                _context7.next = 8;
                return fn(item, i);

              case 8:
                _context7.t1 = _context7.sent;

                _context7.t0.push.call(_context7.t0, _context7.t1);

                _context7.next = 3;
                break;

              case 12:
                return _context7.abrupt("return", Pair.fromArray(result));

              case 13:
              case "end":
                return _context7.stop();
            }
          }
        }, _callee7, this);
      }));

      return function map(_x12, _x13) {
        return _map.apply(this, arguments);
      };
    }(),
    // ------------------------------------------------------------------
    reduce: function () {
      var _reduce = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee8(fn, list) {
        var init,
            array,
            result,
            i,
            item,
            _args8 = arguments;
        return regenerator.wrap(function _callee8$(_context8) {
          while (1) {
            switch (_context8.prev = _context8.next) {
              case 0:
                init = _args8.length > 2 && _args8[2] !== undefined ? _args8[2] : null;
                array = this.get('list->array')(list);

                if (!(list.length === 0)) {
                  _context8.next = 4;
                  break;
                }

                return _context8.abrupt("return", nil);

              case 4:
                result = init;

                if (init === null) {
                  result = array.unshift();
                }

                i = 0;

              case 7:
                if (!(i < array.length)) {
                  _context8.next = 14;
                  break;
                }

                item = array[i++];
                _context8.next = 11;
                return fn(result, item);

              case 11:
                result = _context8.sent;
                _context8.next = 7;
                break;

              case 14:
                if (!(typeof result === 'number')) {
                  _context8.next = 16;
                  break;
                }

                return _context8.abrupt("return", LNumber(result));

              case 16:
                return _context8.abrupt("return", result);

              case 17:
              case "end":
                return _context8.stop();
            }
          }
        }, _callee8, this);
      }));

      return function reduce(_x14, _x15) {
        return _reduce.apply(this, arguments);
      };
    }(),
    // ------------------------------------------------------------------
    filter: function () {
      var _filter = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee9(fn, list) {
        var array, result, i, item, cond;
        return regenerator.wrap(function _callee9$(_context9) {
          while (1) {
            switch (_context9.prev = _context9.next) {
              case 0:
                array = this.get('list->array')(list);
                result = [];
                i = 0;

              case 3:
                if (!(i < array.length)) {
                  _context9.next = 11;
                  break;
                }

                item = array[i++];
                _context9.next = 7;
                return fn(item, i);

              case 7:
                cond = _context9.sent;

                if (cond) {
                  result.push(item);
                }

                _context9.next = 3;
                break;

              case 11:
                return _context9.abrupt("return", Pair.fromArray(result, true));

              case 12:
              case "end":
                return _context9.stop();
            }
          }
        }, _callee9, this);
      }));

      return function filter(_x16, _x17) {
        return _filter.apply(this, arguments);
      };
    }(),
    // ------------------------------------------------------------------
    range: function range(n) {
      if (n instanceof LNumber) {
        n = n.valueOf();
      }

      return Pair.fromArray(new Array(n).fill(0).map(function (_, i) {
        return LNumber(i);
      }));
    },
    // ------------------------------------------------------------------
    curry: function curry(fn) {
      for (var _len4 = arguments.length, init_args = new Array(_len4 > 1 ? _len4 - 1 : 0), _key4 = 1; _key4 < _len4; _key4++) {
        init_args[_key4 - 1] = arguments[_key4];
      }

      var len = fn.length;
      return function () {
        var args = init_args.slice();

        function call() {
          for (var _len5 = arguments.length, more_args = new Array(_len5), _key5 = 0; _key5 < _len5; _key5++) {
            more_args[_key5] = arguments[_key5];
          }

          args = args.concat(more_args);

          if (args.length >= len) {
            return fn.apply(this, args);
          } else {
            return call;
          }
        }

        return call.apply(this, arguments);
      };
    },
    // ------------------------------------------------------------------
    odd: function odd(num) {
      return LNumber(num).isOdd();
    },
    // ------------------------------------------------------------------
    even: function even(num) {
      return LNumber(num).isEvent();
    },
    // ------------------------------------------------------------------
    // math functions
    '*': function _() {
      for (var _len6 = arguments.length, args = new Array(_len6), _key6 = 0; _key6 < _len6; _key6++) {
        args[_key6] = arguments[_key6];
      }

      if (args.length) {
        return args.reduce(function (a, b) {
          return LNumber(a).mul(b);
        });
      }
    },
    // ------------------------------------------------------------------
    '+': function _() {
      for (var _len7 = arguments.length, args = new Array(_len7), _key7 = 0; _key7 < _len7; _key7++) {
        args[_key7] = arguments[_key7];
      }

      if (args.length) {
        return args.reduce(function (a, b) {
          if (LNumber.isNumber(a) && LNumber.isNumber(b)) {
            return LNumber(a).add(b);
          } else if (typeof a === 'string') {
            throw new Error("To concatenate strings use `concat`");
          }

          return a + b;
        });
      }
    },
    // ------------------------------------------------------------------
    '-': function _() {
      for (var _len8 = arguments.length, args = new Array(_len8), _key8 = 0; _key8 < _len8; _key8++) {
        args[_key8] = arguments[_key8];
      }

      if (args.length === 1) {
        return LNumber(args[0]).neg();
      }

      if (args.length) {
        return args.reduce(function (a, b) {
          return LNumber(a).sub(b);
        });
      }
    },
    // ------------------------------------------------------------------
    '/': function _() {
      for (var _len9 = arguments.length, args = new Array(_len9), _key9 = 0; _key9 < _len9; _key9++) {
        args[_key9] = arguments[_key9];
      }

      if (args.length) {
        return args.reduce(function (a, b) {
          return LNumber(a).div(b);
        });
      }
    },
    // ------------------------------------------------------------------
    'abs': function abs(n) {
      return LNumber(n).abs();
    },
    // ------------------------------------------------------------------
    'sqrt': function sqrt(n) {
      if (n instanceof LNumber) {
        return Math.sqrt(n.valueOf());
      }

      return Math.sqrt(n);
    },
    // ------------------------------------------------------------------
    '**': function _(a, b) {
      return LNumber(a).pow(b);
    },
    // ------------------------------------------------------------------
    '1+': function _(number) {
      return LNumber(number).add(1);
    },
    // ------------------------------------------------------------------
    '1-': function _(number) {
      return LNumber(number).sub(1);
    },
    // ------------------------------------------------------------------
    '++': new Macro('++', function (code) {
      var car = this.get(code.car);
      var value = LNumber(car).add(1);
      this.set(code.car, value);
      return value;
    }),
    // ------------------------------------------------------------------
    '--': new Macro('--', function (code) {
      var car = this.get(code.car);
      var value = LNumber(car).sub(1);
      this.set(code.car, value);
      return value;
    }),
    // ------------------------------------------------------------------
    '%': function _(a, b) {
      return LNumber(a).mod(b);
    },
    // ------------------------------------------------------------------
    // Booleans
    "==": function _(a, b) {
      return LNumber(a).cmp(b) === 0;
    },
    // ------------------------------------------------------------------
    '>': function _(a, b) {
      return LNumber(a).cmp(b) === 1;
    },
    // ------------------------------------------------------------------
    '<': function _(a, b) {
      return LNumber(a).cmp(b) === -1;
    },
    // ------------------------------------------------------------------
    '<=': function _(a, b) {
      return [0, -1].includes(LNumber(a).cmp(b));
    },
    // ------------------------------------------------------------------
    '>=': function _(a, b) {
      return [0, 1].includes(LNumber(a).cmp(b));
    },
    // ------------------------------------------------------------------
    'eq?': equal,
    // ------------------------------------------------------------------
    or: new Macro('or', function (code, _ref19) {
      var dynamic_scope = _ref19.dynamic_scope,
          error = _ref19.error;
      var args = this.get('list->array')(code);
      var self = this;

      if (dynamic_scope) {
        dynamic_scope = self;
      }

      return new Promise(function (resolve) {
        var result;

        (function loop() {
          function next(value) {
            result = value;

            if (result) {
              resolve(value);
            }

            loop();
          }

          var arg = args.shift();

          if (typeof arg === 'undefined') {
            if (result) {
              resolve(result);
            } else {
              resolve(false);
            }
          } else {
            var value = evaluate(arg, {
              env: self,
              dynamic_scope: dynamic_scope,
              error: error
            });

            if (value instanceof Promise) {
              value.then(next);
            } else {
              next(value);
            }
          }
        })();
      });
    }),
    // ------------------------------------------------------------------
    and: new Macro('and', function (code) {
      var _ref20 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref20.dynamic_scope,
          error = _ref20.error;

      var args = this.get('list->array')(code);
      var self = this;

      if (dynamic_scope) {
        dynamic_scope = self;
      }

      return new Promise(function (resolve) {
        var result;

        (function loop() {
          function next(value) {
            result = value;

            if (!result) {
              resolve(false);
            }

            loop();
          }

          var arg = args.shift();

          if (typeof arg === 'undefined') {
            if (result) {
              resolve(result);
            } else {
              resolve(false);
            }
          } else {
            var value = evaluate(arg, {
              env: self,
              dynamic_scope: dynamic_scope,
              error: error
            });

            if (value instanceof Promise) {
              value.then(next);
            } else {
              next(value);
            }
          }
        })();
      });
    }),
    // bit operations
    '|': function _(a, b) {
      return LNumber(a).or(b);
    },
    '&': function _(a, b) {
      return LNumber(a).and(b);
    },
    '~': function _(a) {
      return LNumber(a).neg();
    },
    '>>': function _(a, b) {
      return LNumber(a).shr(b);
    },
    '<<': function _(a, b) {
      return LNumber(a).shl(b);
    },
    not: function not(value) {
      if (isEmptyList(value)) {
        return true;
      }

      return !value;
    },
    '->': function _(obj, name) {
      for (var _len10 = arguments.length, args = new Array(_len10 > 2 ? _len10 - 2 : 0), _key10 = 2; _key10 < _len10; _key10++) {
        args[_key10 - 2] = arguments[_key10];
      }

      return obj[name].apply(obj, args);
    }
  }, undefined, 'global'); // ----------------------------------------------------------------------

  ['floor', 'round', 'ceil'].forEach(function (fn) {
    global_env.set(fn, function (value) {
      if (value instanceof LNumber) {
        return value[fn]();
      }

      throw new Error("".concat(_typeof(value), " ").concat(value.toString(), " is not a number"));
    });
  }); // ----------------------------------------------------------------------
  // source: https://stackoverflow.com/a/4331218/387194

  function allPossibleCases(arr) {
    if (arr.length === 1) {
      return arr[0];
    } else {
      var result = []; // recur with the rest of array

      var allCasesOfRest = allPossibleCases(arr.slice(1));

      for (var i = 0; i < allCasesOfRest.length; i++) {
        for (var j = 0; j < arr[0].length; j++) {
          result.push(arr[0][j] + allCasesOfRest[i]);
        }
      }

      return result;
    }
  } // ----------------------------------------------------------------------


  function combinations(input, start, end) {
    var result = [];

    for (var i = start; i <= end; ++i) {
      var input_arr = [];

      for (var j = 0; j < i; ++j) {
        input_arr.push(input);
      }

      result = result.concat(allPossibleCases(input_arr));
    }

    return result;
  } // ----------------------------------------------------------------------
  // cadr caddr cadadr etc.


  combinations(['d', 'a'], 2, 5).forEach(function (spec) {
    var chars = spec.split('').reverse();
    global_env.set('c' + spec + 'r', function (arg) {
      return chars.reduce(function (list, type) {
        if (type === 'a') {
          return list.car;
        } else {
          return list.cdr;
        }
      }, arg);
    });
  }); // ----------------------------------------------------------------------

  if (typeof global !== 'undefined') {
    global_env.set('global', global);
  } else if (typeof window !== 'undefined') {
    global_env.set('window', window);
  }

  function type(value) {
    if (typeof value === "string") {
      return "string";
    } else if (value instanceof LNumber) {
      return "number";
    } else if (value instanceof RegExp) {
      return "regex";
    } else if (typeof value === 'boolean') {
      return 'boolean';
    } else {
      return 'unknown type';
    }
  } // ----------------------------------------------------------------------
  // :; wrap tree of Promises with single Promise or return argument as is
  // :: if tree have no Promises
  // ----------------------------------------------------------------------


  function maybe_promise(arg) {
    var promises = [];
    traverse(arg);

    if (promises.length) {
      return resolve(arg);
    }

    return arg;

    function traverse(node) {
      if (node instanceof Promise) {
        promises.push(node);
      } else if (node instanceof Pair) {
        traverse(node.car);
        traverse(node.cdr);
      } else if (node instanceof Array) {
        node.forEach(traverse);
      }
    }

    function resolve(node) {
      if (node instanceof Array) {
        return Promise.all(node.map(resolve));
      }

      if (node instanceof Pair) {
        var car = resolve(node.car);
        var cdr = resolve(node.cdr);
        return Promise.all([car, cdr]).then(function (_ref21) {
          var _ref22 = _slicedToArray(_ref21, 2),
              car = _ref22[0],
              cdr = _ref22[1];

          var pair = new Pair(car, cdr);

          if (node.data) {
            quote(pair);
          }

          return pair;
        });
      } else {
        return node;
      }
    }
  } // ----------------------------------------------------------------------


  function get_function_args(rest, _ref23) {
    var env = _ref23.env,
        dynamic_scope = _ref23.dynamic_scope,
        error = _ref23.error;
    var args = [];
    var node = rest;

    while (true) {
      if (node instanceof Pair) {
        var arg = evaluate(node.car, {
          env: env,
          dynamic_scope: dynamic_scope,
          error: error
        });

        if (dynamic_scope) {
          if (arg instanceof Promise) {
            arg = arg.then(function (arg) {
              if (typeof arg === 'function') {
                return arg.bind(dynamic_scope);
              }

              return arg;
            });
          } else if (typeof arg === 'function') {
            arg = arg.bind(dynamic_scope);
          }
        }

        args.push(arg);
        node = node.cdr;
      } else {
        break;
      }
    }

    return maybe_promise(args);
  } // ----------------------------------------------------------------------


  function evaluate_macro(macro, code, eval_args) {
    if (code instanceof Pair) {
      code = code.clone();
    }

    var value = macro.invoke(code, eval_args);
    value = maybe_promise(value, true);

    if (value && value.data) {
      return value;
    } else if (value instanceof Promise) {
      return value.then(function (value) {
        if (value && value.data) {
          return value;
        }

        return evaluate(value, eval_args);
      });
    }

    return evaluate(value, eval_args);
  } // ----------------------------------------------------------------------


  function evaluate(code) {
    var _ref24 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        env = _ref24.env,
        dynamic_scope = _ref24.dynamic_scope,
        _ref24$error = _ref24.error,
        error = _ref24$error === void 0 ? function () {} : _ref24$error;

    try {
      if (dynamic_scope === true) {
        env = dynamic_scope = env || global_env;
      } else if (env === true) {
        env = dynamic_scope = global_env;
      } else {
        env = env || global_env;
      }

      var eval_args = {
        env: env,
        dynamic_scope: dynamic_scope,
        error: error
      };
      var value;

      if (is_null(code)) {
        return code;
      }

      if (isEmptyList(code)) {
        return emptyList();
      }

      var first = code.car;
      var rest = code.cdr;

      if (first instanceof Pair) {
        value = maybe_promise(evaluate(first, eval_args));

        if (value instanceof Promise) {
          return value.then(function (value) {
            return evaluate(new Pair(value, code.cdr), eval_args);
          });
        } else if (typeof value !== 'function') {
          throw new Error(env.get('string')(value) + ' is not a function');
        }
      }

      if (first instanceof _Symbol) {
        value = env.get(first);

        if (value instanceof Macro) {
          return evaluate_macro(value, rest, eval_args);
        } else if (typeof value !== 'function') {
          if (value) {
            var msg = "".concat(type(value), " `").concat(value, "' is not a function");
            throw new Error(msg);
          }

          throw new Error("Unknown function `".concat(first.name, "'"));
        }
      } else if (typeof first === 'function') {
        value = first;
      }

      if (typeof value === 'function') {
        var args = get_function_args(rest, eval_args);

        if (args instanceof Promise) {
          return args.then(function (args) {
            var scope = dynamic_scope || env;
            return quote(maybe_promise(value.apply(scope, args)));
          });
        }

        return quote(maybe_promise(value.apply(dynamic_scope || env, args)));
      } else if (code instanceof _Symbol) {
        value = env.get(code);

        if (value === 'undefined') {
          throw new Error('Unbound variable `' + code.name + '\'');
        }

        return value;
      } else if (code instanceof Pair) {
        value = first && first.toString();
        throw new Error("".concat(type(first), " ").concat(value, " is not a function"));
      } else {
        return code;
      }
    } catch (e) {
      error && error(e);
    }
  } // ----------------------------------------------------------------------


  function exec(_x18, _x19, _x20) {
    return _exec.apply(this, arguments);
  } // ----------------------------------------------------------------------


  function _exec() {
    _exec = _asyncToGenerator(
    /*#__PURE__*/
    regenerator.mark(function _callee10(string, env, dynamic_scope) {
      var list, results, code, result;
      return regenerator.wrap(function _callee10$(_context10) {
        while (1) {
          switch (_context10.prev = _context10.next) {
            case 0:
              if (dynamic_scope === true) {
                env = dynamic_scope = env || global_env;
              } else if (env === true) {
                env = dynamic_scope = global_env;
              } else {
                env = env || global_env;
              }

              list = parse(tokenize(string));
              results = [];

            case 3:

              code = list.shift();

              if (code) {
                _context10.next = 9;
                break;
              }

              return _context10.abrupt("return", results);

            case 9:
              _context10.next = 11;
              return evaluate(code, {
                env: env,
                dynamic_scope: dynamic_scope,
                error: function error(e) {
                  throw e;
                }
              });

            case 11:
              result = _context10.sent;
              results.push(result);

            case 13:
              _context10.next = 3;
              break;

            case 15:
            case "end":
              return _context10.stop();
          }
        }
      }, _callee10, this);
    }));
    return _exec.apply(this, arguments);
  }

  function balanced(code) {
    var re = /[()]/;
    var parenthesis = tokenize(code).filter(function (token) {
      return token.match(re);
    });
    var open = parenthesis.filter(function (p) {
      return p === ')';
    });
    var close = parenthesis.filter(function (p) {
      return p === '(';
    });
    return open.length === close.length;
  } // ----------------------------------------------------------------------


  Pair.unDry = function (value) {
    return new Pair(value.car, value.cdr);
  };

  Pair.prototype.toDry = function () {
    return {
      value: {
        car: this.car,
        cdr: this.cdr
      }
    };
  };

  Nil.prototype.toDry = function () {
    return {
      value: null
    };
  };

  Nil.unDry = function () {
    return nil;
  };

  _Symbol.prototype.toDry = function () {
    return {
      value: {
        name: this.name
      }
    };
  };

  _Symbol.unDry = function (value) {
    return new _Symbol(value.name);
  }; // ----------------------------------------------------------------------


  function init() {
    var lips_mime = 'text/x-lips';

    if (window.document) {
      Array.from(document.querySelectorAll('script')).forEach(function (script) {
        var type = script.getAttribute('type');

        if (type === lips_mime) {
          var src = script.getAttribute('src');

          if (src) {
            root.fetch(src).then(function (res) {
              return res.text();
            }).then(exec);
          } else {
            exec(script.innerHTML);
          }
        } else if (type && type.match(/lips|lisp/)) {
          console.warn('Expecting ' + lips_mime + ' found ' + type);
        }
      });
    }
  } // ----------------------------------------------------------------------


  function load(callback) {
    if (typeof window !== 'undefined') {
      if (window.addEventListener) {
        window.addEventListener("load", callback, false);
      } else if (window.attachEvent) {
        window.attachEvent("onload", callback);
      } else if (typeof window.onload === 'function') {
        (function (old) {
          window.onload = function () {
            callback();
            old();
          };
        })(window.onload);
      } else {
        window.onload = callback;
      }
    }
  } // ----------------------------------------------------------------------


  load(function () {
    setTimeout(init, 0);
  }); // --------------------------------------

  return {
    version: 'DEV',
    exec: exec,
    parse: parse,
    tokenize: tokenize,
    evaluate: evaluate,
    Environment: Environment,
    global_environment: global_env,
    env: global_env,
    balanced_parenthesis: balanced,
    Macro: Macro,
    quote: quote,
    Pair: Pair,
    nil: nil,
    maybe_promise: maybe_promise,
    Symbol: _Symbol,
    LNumber: LNumber
  };
});

}());
