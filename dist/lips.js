/**@license
 * LIPS is Pretty Simple - simple scheme like lisp in JavaScript - v. DEV
 *
 * Copyright (c) 2018-2019 Jakub T. Jankiewicz <https://jcubic.pl/me>
 * Released under the MIT license
 *
 * includes:
 *
 * unfetch by Jason Miller (@developit) MIT License
 *
 * includes:
 * contentloaded.js
 *
 * Author: Diego Perini (diego.perini at gmail.com)
 * Summary: cross-browser wrapper for DOMContentLoaded
 * Updated: 20101020
 * License: MIT
 * Version: 1.2
 *
 * URL:
 * http://javascript.nwbox.com/ContentLoaded/
 * http://javascript.nwbox.com/ContentLoaded/MIT-LICENSE
 *
 * build: Sat, 18 May 2019 09:29:23 +0000
 */
(function () {
'use strict';

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

function _readOnlyError(name) {
  throw new Error("\"" + name + "\" is read-only");
}

function createCommonjsModule(fn, module) {
	return module = { exports: {} }, fn(module, module.exports), module.exports;
}

var runtime_1 = createCommonjsModule(function (module) {

var runtime = (function (exports) {

  var Op = Object.prototype;
  var hasOwn = Op.hasOwnProperty;
  var undefined; // More compressible than void 0.
  var $Symbol = typeof Symbol === "function" ? Symbol : {};
  var iteratorSymbol = $Symbol.iterator || "@@iterator";
  var asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator";
  var toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";

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
  exports.wrap = wrap;

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

  exports.isGeneratorFunction = function(genFun) {
    var ctor = typeof genFun === "function" && genFun.constructor;
    return ctor
      ? ctor === GeneratorFunction ||
        // For the native GeneratorFunction constructor, the best we can
        // do is to check its .name property.
        (ctor.displayName || ctor.name) === "GeneratorFunction"
      : false;
  };

  exports.mark = function(genFun) {
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
  exports.awrap = function(arg) {
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
  exports.AsyncIterator = AsyncIterator;

  // Note that simple async functions are implemented on top of
  // AsyncIterator objects; they just return a Promise for the value of
  // the final result produced by the iterator.
  exports.async = function(innerFn, outerFn, self, tryLocsList) {
    var iter = new AsyncIterator(
      wrap(innerFn, outerFn, self, tryLocsList)
    );

    return exports.isGeneratorFunction(outerFn)
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
        // Note: ["return"] must be used for ES3 parsing compatibility.
        if (delegate.iterator["return"]) {
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

  exports.keys = function(object) {
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
  exports.values = values;

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

  // Regardless of whether this script is executing as a CommonJS module
  // or not, return the runtime object so that we can declare the variable
  // regeneratorRuntime in the outer scope, which allows this module to be
  // injected easily by `bin/regenerator --include-runtime script.js`.
  return exports;

}(
  // If this script is executing as a CommonJS module, use module.exports
  // as the regeneratorRuntime namespace. Otherwise create a new empty
  // object. Either way, the resulting object will be used to initialize
  // the regeneratorRuntime variable at the top of this file.
  module.exports
));

try {
  regeneratorRuntime = runtime;
} catch (accidentalStrictMode) {
  // This module should not be running in strict mode, so the above
  // assignment should always work unless something is misconfigured. Just
  // in case runtime.js accidentally runs in strict mode, we can escape
  // strict mode using a global Function call. This could conceivably fail
  // if a Content Security Policy forbids using Function, but in that case
  // the proper solution is to fix the accidental strict mode problem. If
  // you've misconfigured your bundler to force strict mode and applied a
  // CSP to forbid Function, and you're not willing to fix either of those
  // problems, please detail your unique predicament in a GitHub issue.
  Function("r", "regeneratorRuntime = r")(runtime);
}
});

var regenerator = runtime_1;

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
  /* eslint-disable */

  /* istanbul ignore next */
  function contentLoaded(win, fn) {
    var done = false,
        top = true,
        doc = win.document,
        root = doc.documentElement,
        modern = doc.addEventListener,
        add = modern ? 'addEventListener' : 'attachEvent',
        rem = modern ? 'removeEventListener' : 'detachEvent',
        pre = modern ? '' : 'on',
        init = function init(e) {
      if (e.type == 'readystatechange' && doc.readyState != 'complete') return;
      (e.type == 'load' ? win : doc)[rem](pre + e.type, init, false);
      if (!done && (done = true)) fn.call(win, e.type || e);
    },
        poll = function poll() {
      try {
        root.doScroll('left');
      } catch (e) {
        setTimeout(poll, 50);
        return;
      }

      init('poll');
    };

    if (doc.readyState == 'complete') fn.call(win, 'lazy');else {
      if (!modern && root.doScroll) {
        try {
          top = !win.frameElement;
        } catch (e) {}

        if (top) poll();
      }

      doc[add](pre + 'DOMContentLoaded', init, false);
      doc[add](pre + 'readystatechange', init, false);
      win[add](pre + 'load', init, false);
    }
  }

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
  var string_re = /"(?:\\[\S\s]|[^"])*"/g; //var tokens_re = /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|\(|\)|'|"(?:\\[\S\s]|[^"])+|\n|(?:\\[\S\s]|[^"])*"|;.*|(?:[-+]?(?:(?:\.[0-9]+|[0-9]+\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\.)[0-9]|\.{2,}|\.|,@|,|#|`|[^(\s)]+)/gim;
  // ----------------------------------------------------------------------

  function makeTokenRe() {
    var tokens = Object.keys(specials).map(escapeRegex).join('|');
    return new RegExp("(\"(?:\\\\[\\S\\s]|[^\"])*\"|\\/(?! )[^\\/\\\\]*(?:\\\\[\\S\\s][^\\/\\\\]*)*\\/[gimy]*(?=\\s|\\(|\\)|$)|\\(|\\)|'|\"(?:\\\\[\\S\\s]|[^\"])+|\\n|(?:\\\\[\\S\\s]|[^\"])*\"|;.*|(?:[-+]?(?:(?:\\.[0-9]+|[0-9]+\\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\\.)[0-9]|\\.{2,}|".concat(tokens, "|[^(\\s)]+)"), 'gim');
  }
  /* eslint-enable */
  // ----------------------------------------------------------------------


  function lastItem(array) {
    var n = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 1;
    return array[array.length - n];
  } // ----------------------------------------------------------------------


  function escapeRegex(str) {
    if (typeof str === 'string') {
      var special = /([-\\^$[\]()+{}?*.|])/g;
      return str.replace(special, '\\$1');
    }
  } // ----------------------------------------------------------------------


  function tokens(str) {
    var tokens_re = makeTokenRe();
    str = str.replace(/\n\r|\r/g, '\n');
    var count = 0;
    var line = 0;
    var tokens = [];
    var current_line = [];
    var col = 0;
    str.split(pre_parse_re).filter(Boolean).forEach(function (string) {
      if (string.match(pre_parse_re)) {
        col = 0;

        if (current_line.length) {
          var lastToken = lastItem(current_line);

          if (lastToken.token.match(/\n/)) {
            var last_line = lastToken.token.split('\n').pop();
            col += last_line.length;
          } else {
            col += lastToken.token.length;
          }

          col += lastToken.col;
        }

        var token = {
          col: col,
          line: line,
          token: string,
          offset: count
        };
        tokens.push(token);
        current_line.push(token);
        count += string.length;
        col += string.length;
        line += (string.match("\n") || []).length;
        return;
      }

      string.split(tokens_re).filter(Boolean).forEach(function (string) {
        var token = {
          col: col,
          line: line,
          token: string,
          offset: count
        };
        col += string.length;
        count += string.length;
        tokens.push(token);
        current_line.push(token);

        if (string === '\n') {
          ++line;
          current_line = [];
          col = 0;
        }
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
    ',@': new _Symbol('unquote-splicing'),
    ',': new _Symbol('unquote')
  }; // ----------------------------------------------------------------------
  // :: tokens are the array of strings from tokenizer
  // :: the return value is lisp code created out of Pair class
  // ----------------------------------------------------------------------

  function parse(tokens) {
    if (typeof tokens === 'string') {
      tokens = tokenize(tokens);
    }

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

      if (top instanceof Array && top[0] instanceof _Symbol && special_forms.includes(top[0].name) && stack.length > 1 && !top[0].literal) {
        stack.pop();

        if (stack[stack.length - 1].length === 1 && stack[stack.length - 1][0] instanceof _Symbol) {
          stack[stack.length - 1].push(top);
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
      } else {
        if (special) {
          specials_stack.push(single_list_specials);
          single_list_specials = [];
        }

        if (token === '(') {
          first_value = true;
          parents++;
          stack.push([]);
          special = null;
          special_count = 0;
        } else if (token === '.' && !first_value) {
          stack[stack.length - 1] = Pair.fromArray(top);
        } else if (token === ')') {
          parents--;

          if (!stack.length) {
            throw new Error('Unbalanced parenthesis');
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
              stack[stack.length - 1].push(value);
              value = stack.pop();
            }

            specials_stack.pop();
            special_count = 0;
            special = false;
          } else if (value instanceof _Symbol && special_forms.includes(value.name)) {
            // handle parsing os special forms as literal symbols
            // (values they expand into)
            value.literal = true;
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
      }
    });

    if (!tokens.filter(function (t) {
      return t.match(/^[()]$/);
    }).length && stack.length) {
      // list of parser macros
      result = result.concat(stack);
      stack = [];
    }

    if (stack.length) {
      throw new Error('Unbalanced parenthesis 2');
    }

    return result.map(function (arg) {
      if (arg instanceof Array) {
        return Pair.fromArray(arg);
      }

      return arg;
    });
  } // ----------------------------------------------------------------------


  function unpromise(value) {
    var fn = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : function (x) {
      return x;
    };
    var error = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;

    if (isPromise(value)) {
      var ret = value.then(fn);

      if (error === null) {
        return ret;
      } else {
        return ret.catch(error);
      }
    }

    return fn(value);
  } // ----------------------------------------------------------------------


  function matcher(name, arg) {
    if (arg instanceof RegExp) {
      return function (x) {
        return String(x).match(arg);
      };
    } else if (typeof arg !== 'function') {
      throw new Error("".concat(name, " argument need to be a function or RegExp"));
    } else {
      return arg;
    }
  } // ----------------------------------------------------------------------
  // :: documentaton decorator to LIPS functions if lines starts with :
  // :: they are ignored (not trim) otherwise it trim so
  // :: so you can have indent in source code
  // ----------------------------------------------------------------------


  function doc(fn, doc, dump) {
    if (doc) {
      if (dump) {
        fn.__doc__ = doc;
      } else {
        fn.__doc__ = trimLines(doc);
      }
    }

    return fn;
  } // ----------------------------------------------------------------------


  function trimLines(string) {
    return string.split('\n').map(function (line) {
      return line.trim();
    }).join('\n');
  } // ----------------------------------------------------------------------
  // return last S-Expression
  // ----------------------------------------------------------------------


  function previousSexp(tokens) {
    var sexp = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 1;
    var i = tokens.length;

    if (sexp <= 0) {
      throw Error("previousSexp: Invlaid argument sexp = ".concat(sexp));
    }

    outer: while (sexp-- && i >= 0) {
      var count = 1;

      while (count > 0) {
        var token = tokens[--i];

        if (!token) {
          break outer;
        }

        if (token === '(' || token.token === '(') {
          count--;
        } else if (token === ')' || token.token === ')') {
          count++;
        }
      }

      i--;
    }

    return tokens.slice(i + 1);
  } // ----------------------------------------------------------------------
  // :: find number of spaces in line
  // ----------------------------------------------------------------------


  function lineIndent(tokens) {
    if (!tokens || !tokens.length) {
      return 0;
    }

    var i = tokens.length;

    if (tokens[i - 1].token === '\n') {
      return 0;
    }

    while (--i) {
      if (tokens[i].token === '\n') {
        var token = (tokens[i + 1] || {}).token;

        if (token) {
          return token.length;
        }
      }
    }

    return 0;
  } // ----------------------------------------------------------------------
  // :: token based pattern matching
  // ----------------------------------------------------------------------


  function match(pattern, input) {
    return inner_match(pattern, input) === input.length;

    function inner_match(pattern, input) {
      var p = 0;
      var glob = {};

      for (var i = 0; i < input.length; ++i) {
        if (typeof pattern[p] === 'undefined') {
          return i;
        }

        if (!input[i].trim()) {
          continue;
        }

        if (pattern[p] instanceof Pattern) {
          if (pattern[p].flag === '+') {
            var m;

            while (i < input.length) {
              m = inner_match(pattern[p].pattern, input.slice(i));

              if (m === -1) {
                break;
              }

              i += m;
            }

            if (m === -1) {
              return false;
            }

            p++;
            i -= 1;
            continue;
          }
        }

        if (pattern[p] instanceof RegExp) {
          if (!input[i].match(pattern[p])) {
            return -1;
          }
        } else if (typeof pattern[p] === 'string') {
          if (pattern[p] !== input[i]) {
            return -1;
          }
        } else if (_typeof(pattern[p]) === 'symbol') {
          if (pattern[p] === root.Symbol.for('*')) {
            // ignore S-expressions inside for case when next pattern is ')'
            glob[p] = glob[p] || 0;

            if (input[i] === '(') {
              glob[p]++;
            } else if (input[i] === ')') {
              glob[p]--;
            }

            if (typeof pattern[p + 1] !== 'undefined' && glob[p] === 0 && pattern[p + 1] !== input[i + 1] || glob[p] > 0) {
              continue;
            }
          }
        } else if (pattern[p] instanceof Array) {
          var inc = inner_match(pattern[p], input.slice(i));

          if (inc === -1 || inc + i > input.length) {
            // if no more input it's not match
            return -1;
          }

          i += inc - 1;
          p++;
          continue;
        } else {
          return -1;
        }

        p++;
      }

      if (pattern.length !== p) {
        // if there are still patterns it's not match
        return -1;
      }

      return input.length;
    }
  } // ----------------------------------------------------------------------
  // :: Code formatter class
  // :: based on http://community.schemewiki.org/?scheme-style
  // :: and GNU Emacs scheme mode
  // :: it rely on meta data from tokenizer function
  // ----------------------------------------------------------------------


  function Formatter(code) {
    this._code = code.replace(/\r/g, '');
  } // ----------------------------------------------------------------------


  Formatter.defaults = {
    offset: 0,
    indent: 2,
    specials: [/^define/, 'lambda', 'let', 'let*']
  };
  Formatter.match = match; // ----------------------------------------------------------------------
  // :: return indent for next line
  // ----------------------------------------------------------------------

  Formatter.prototype._options = function _options(options) {
    var defaults = Formatter.defaults;

    if (typeof options === 'undefined') {
      return Object.assign({}, defaults);
    }

    var specials = options && options.specials || [];
    return Object.assign({}, defaults, options, {
      specials: defaults.specials.concat(specials)
    });
  }; // ----------------------------------------------------------------------


  Formatter.prototype.indent = function indent(options) {
    var tokens = tokenize(this._code, true);
    return this._indent(tokens, options);
  }; // ----------------------------------------------------------------------


  Formatter.matchSpecial = function (token, settings) {
    var specials = settings.specials;

    if (specials.indexOf(token) !== -1) {
      return true;
    } else {
      var regexes = specials.filter(function (s) {
        return s instanceof RegExp;
      });
      var _iteratorNormalCompletion = true;
      var _didIteratorError = false;
      var _iteratorError = undefined;

      try {
        for (var _iterator = regexes[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
          var re = _step.value;

          if (token.match(re)) {
            return true;
          }
        }
      } catch (err) {
        _didIteratorError = true;
        _iteratorError = err;
      } finally {
        try {
          if (!_iteratorNormalCompletion && _iterator.return != null) {
            _iterator.return();
          }
        } finally {
          if (_didIteratorError) {
            throw _iteratorError;
          }
        }
      }
    }

    return false;
  }; // ----------------------------------------------------------------------


  Formatter.prototype._indent = function _indent(tokens, options) {
    var settings = this._options(options);

    var spaces = lineIndent(tokens);
    var sexp = previousSexp(tokens);

    if (sexp) {
      if (sexp[0].line > 0) {
        settings.offset = 0;
      }

      if (sexp.length === 1) {
        return settings.offset + sexp[0].col + 1;
      } else if (Formatter.matchSpecial(sexp[1].token, settings)) {
        return settings.offset + sexp[0].col + settings.indent;
      } else if (sexp[0].line < sexp[1].line) {
        return settings.offset + sexp[0].col + 1;
      } else if (sexp.length > 3 && sexp[1].line === sexp[3].line) {
        if (sexp[1].token === '(') {
          return settings.offset + sexp[1].col;
        }

        return settings.offset + sexp[3].col;
      } else if (sexp[0].line === sexp[1].line) {
        return settings.offset + settings.indent + sexp[0].col;
      } else {
        var next_tokens = sexp.slice(2);

        for (var i = 0; i < next_tokens.length; ++i) {
          var token = next_tokens[i];

          if (token.token.trim()) {
            return token.col;
          }
        }
      }
    } else {
      return 0;
    }

    return spaces + settings.indent;
  }; // ----------------------------------------------------------------------


  function Ahead(pattern) {
    this.pattern = pattern;
  } // ----------------------------------------------------------------------


  Ahead.prototype.match = function (string) {
    return string.match(this.pattern);
  }; // ----------------------------------------------------------------------


  function Pattern(pattern, flag) {
    this.pattern = pattern;
    this.flag = flag;
  } // ----------------------------------------------------------------------


  Formatter.Pattern = Pattern;
  Formatter.Ahead = Ahead;
  var notParen = new Ahead(/[^)]/);
  var glob = root.Symbol.for('*');
  var sexp = new Pattern(['(', glob, ')'], '+'); // rules for breaking S-Expressions into lines

  Formatter.rules = [[['(', 'begin'], 1], [['(', 'begin', sexp], 1, notParen], [['(', /^let\*?$/, '(', glob, ')'], 1], [['(', /^let\*?$/, '(', sexp], 2, notParen], [['(', /^let\*?$/, ['(', glob, ')'], sexp], 1, notParen], [['(', 'if', /[^()]/], 1], [['(', 'if', ['(', glob, ')']], 1], [['(', 'if', ['(', glob, ')'], ['(', glob, ')']], 1, notParen], [['(', glob, ')'], 1], [['(', /^(define|lambda)/, ['(', glob, ')'], string_re], 1], [['(', /^(define|lambda)/, '(', glob, ')'], 1], [['(', /^(define|lambda)/, ['(', glob, ')'], string_re, sexp], 1, notParen], [['(', /^(define|lambda)/, ['(', glob, ')'], sexp], 1, notParen]]; // ----------------------------------------------------------------------

  Formatter.prototype.break = function () {
    var code = this._code.replace(/\n[ \t]*/g, '\n ');

    var token = function token(t) {
      if (t.token.match(string_re)) {
        return t.token;
      } else {
        return t.token.replace(/\s+/, ' ');
      }
    };

    var tokens = tokenize(code, true).map(token).filter(function (t) {
      return t !== '\n';
    });
    var rules = Formatter.rules;

    for (var i = 0; i < tokens.length; ++i) {
      if (!tokens[i].trim()) {
        continue;
      }

      var sub = tokens.slice(0, i);
      var sexp = {};
      rules.map(function (b) {
        return b[1];
      }).forEach(function (count) {
        if (!sexp[count]) {
          sexp[count] = previousSexp(sub, count);
        }
      });
      var _iteratorNormalCompletion2 = true;
      var _didIteratorError2 = false;
      var _iteratorError2 = undefined;

      try {
        for (var _iterator2 = rules[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
          var _step2$value = _slicedToArray(_step2.value, 3),
              pattern = _step2$value[0],
              count = _step2$value[1],
              ext = _step2$value[2];

          var m = match(pattern, sexp[count].filter(function (t) {
            return t.trim();
          }));
          var next = tokens.slice(i).find(function (t) {
            return t.trim();
          });

          if (m && (ext instanceof Ahead && ext.match(next) || !ext)) {
            tokens.splice(i, 0, '\n');
            i++;
            continue;
          }
        }
      } catch (err) {
        _didIteratorError2 = true;
        _iteratorError2 = err;
      } finally {
        try {
          if (!_iteratorNormalCompletion2 && _iterator2.return != null) {
            _iterator2.return();
          }
        } finally {
          if (_didIteratorError2) {
            throw _iteratorError2;
          }
        }
      }
    }

    this._code = tokens.join('');
    return this;
  }; // ----------------------------------------------------------------------


  Formatter.prototype._spaces = function (i) {
    return new Array(i + 1).join(' ');
  }; // ----------------------------------------------------------------------
  // :: auto formatting of code, it require to have newlines
  // ----------------------------------------------------------------------


  Formatter.prototype.format = function format(options) {
    // prepare code with single space after newline
    // so we have space token to align
    var code = this._code.replace(/[ \t]*\n[ \t]*/g, '\n ');

    var tokens = tokenize(code, true);

    var settings = this._options(options);

    var indent = 0;
    var offset = 0;

    for (var i = 0; i < tokens.length; ++i) {
      var token = tokens[i];

      if (token.token === '\n') {
        indent = this._indent(tokens.slice(0, i), settings);
        offset += indent;

        if (tokens[i + 1]) {
          tokens[i + 1].token = this._spaces(indent); // because we have single space as initial indent

          indent--;
          offset--;

          for (var j = i + 2; j < tokens.length; ++j) {
            tokens[j].offset += offset;
            tokens[j].col += indent;

            if (tokens[j].token === '\n') {
              // ++i is called after the loop
              i = j - 1;
              break;
            }
          }
        }
      }
    }

    return tokens.map(function (token) {
      if (token.token.match(string_re)) {
        if (token.token.match(/\n/)) {
          var spaces = new Array(token.col + 1).join(' ');
          var lines = token.token.split('\n');
          token.token = [lines[0]].concat(lines.slice(1).map(function (line) {
            return spaces + line;
          })).join('\n');
        }
      }

      return token.token;
    }).join('');
  }; // ----------------------------------------------------------------------
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
  } // ----------------------------------------------------------------------


  _Symbol.is = function (symbol, name) {
    return symbol instanceof _Symbol && (typeof name === 'string' && symbol.name === name || name instanceof RegExp && name.test(symbol.name));
  }; // ----------------------------------------------------------------------


  _Symbol.prototype.toJSON = _Symbol.prototype.toString = function () {
    //return '<#symbol \'' + this.name + '\'>';
    if (isSymbol(this.name)) {
      return this.name.toString().replace(/^Symbol\(([^)]+)\)/, '$1');
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


  function toArray(name, deep) {
    return function recur(list) {
      typecheck(name, list, ['pair', 'nil']);

      if (list instanceof Pair && list.isEmptyList()) {
        return [];
      }

      var result = [];
      var node = list;

      while (true) {
        if (node instanceof Pair) {
          if (node.haveCycles('cdr')) {
            break;
          }

          var car = node.car;

          if (deep && car instanceof Pair) {
            car = this.get(name).call(this, car);
          }

          result.push(car);
          node = node.cdr;
        } else {
          break;
        }
      }

      return result;
    };
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
      if (!node || node === nil || !(node instanceof Pair) || node.haveCycles('cdr')) {
        break;
      }

      len++;
      node = node.cdr;
    }

    return len;
  }; // ----------------------------------------------------------------------


  Pair.prototype.clone = function () {
    var visited = new Map();

    function clone(node) {
      if (node instanceof Pair) {
        if (visited.has(node)) {
          return visited.get(node);
        }

        var pair = new Pair();
        visited.set(node, pair);
        pair.car = clone(node.car);
        pair.cdr = clone(node.cdr);
        return pair;
      }

      return node;
    }

    return clone(this);
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

        if (cdr instanceof LNumber) {
          cdr = cdr.valueOf();
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
    if (this.haveCycles()) {
      throw new Error("You can't reverse list that have cycles");
    }

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


  function toString(value) {
    if (typeof value === 'function') {
      return '<#function ' + (value.name || 'anonymous') + '>';
    } else if (typeof value === 'string') {
      return JSON.stringify(value).replace(/\\n/g, '\n');
    } else if (isPromise(value)) {
      return '<#Promise>';
    } else if (value instanceof _Symbol || value instanceof LNumber || value instanceof RegExp || value instanceof Pair || value === nil) {
      return value.toString();
    } else if (value instanceof Array) {
      return value.map(toString);
    } else if (_typeof(value) === 'object') {
      if (value === null) {
        return 'null';
      }

      var name = value.constructor.name;

      if (name === 'Object') {
        return JSON.stringify(value);
      }

      return '<#object(' + value.constructor.name + ')>';
    } else if (typeof value !== 'undefined') {
      return value;
    }
  } // ----------------------------------------------------------------------------


  Pair.prototype.markCycles = function () {
    markCycles(this);
    return this;
  }; // ----------------------------------------------------------------------------


  Pair.prototype.haveCycles = function () {
    var name = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;

    if (!name) {
      return this.haveCycles('car') || this.haveCycles('cdr');
    }

    return !!(this.cycles && this.cycles[name]);
  }; // ----------------------------------------------------------------------------


  function markCycles(pair) {
    var seenPairs = [];
    var cycles = [];

    function cycleName(pair) {
      if (pair instanceof Pair) {
        if (seenPairs.includes(pair)) {
          if (!cycles.includes(pair)) {
            cycles.push(pair);
          }

          return "#".concat(cycles.length - 1, "#");
        }
      }
    }

    function detect(pair) {
      if (pair instanceof Pair) {
        seenPairs.push(pair);
        var cycles = {};
        var carCycle = cycleName(pair.car);
        var cdrCycle = cycleName(pair.cdr);

        if (carCycle) {
          cycles['car'] = carCycle;
        } else {
          detect(pair.car);
        }

        if (cdrCycle) {
          cycles['cdr'] = cdrCycle;
        } else {
          detect(pair.cdr);
        }

        if (carCycle || cdrCycle) {
          pair.cycles = cycles;
        } else if (pair.cycles) {
          delete pair.cycles;
        }
      }
    }

    detect(pair);
  } // ----------------------------------------------------------------------


  Pair.prototype.toString = function () {
    var arr = ['('];

    if (this.car !== undefined) {
      var value;

      if (this.cycles && this.cycles.car) {
        value = this.cycles.car;
      } else {
        value = toString(this.car);
      }

      if (value) {
        arr.push(value);
      }

      if (this.cdr instanceof Pair) {
        if (this.cycles && this.cycles.cdr) {
          arr.push(' . ');
          arr.push(this.cycles.cdr);
        } else {
          var name;

          if (this.cycles && this.cycles.cdr) {
            name = this.cycles.cdr;
          }

          var cdr = this.cdr.toString(name).replace(/^\(|\)$/g, '');
          arr.push(' ');
          arr.push(cdr);
        }
      } else if (typeof this.cdr !== 'undefined' && this.cdr !== nil) {
        arr = arr.concat([' . ', toString(this.cdr)]);
      }
    }

    arr.push(')');
    return arr.join('');
  }; // ----------------------------------------------------------------------


  Pair.prototype.set = function (prop, value) {
    this[prop] = value;

    if (value instanceof Pair) {
      this.markCycles();
    }
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
        this.car = pair;
      }
    } else {
      while (true) {
        if (p instanceof Pair && p.cdr !== nil) {
          p = p.cdr;
        } else {
          break;
        }
      }

      if (pair instanceof Pair) {
        p.cdr = pair;
      } else if (pair !== nil) {
        p.cdr = new Pair(pair, nil);
      }
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


  function Macro(name, fn, doc) {
    if (typeof this !== 'undefined' && this.constructor !== Macro || typeof this === 'undefined') {
      return new Macro(name, fn);
    }

    typecheck('Macro', name, 'string', 1);
    typecheck('Macro', fn, 'function', 2);

    if (doc) {
      this.__doc__ = trimLines(doc);
    }

    this.name = name;
    this.fn = fn;
  } // ----------------------------------------------------------------------


  Macro.defmacro = function (name, fn, doc) {
    var macro = new Macro(name, fn, doc);
    macro.defmacro = true;
    return macro;
  }; // ----------------------------------------------------------------------


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
  }; // ----------------------------------------------------------------------


  Macro.prototype.toString = function () {
    return '#<Macro ' + this.name + '>';
  }; // ----------------------------------------------------------------------


  var macro = 'define-macro'; // ----------------------------------------------------------------------

  function macroExpand(single) {
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
                              // CYCLE DETECT
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
                      }, _callee, null, [[1, 11]]);
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


  function isNull(value) {
    return typeof value === 'undefined' || value === nil || value === null;
  } // ----------------------------------------------------------------------


  function isPromise(o) {
    return o instanceof Promise || o && typeof o !== 'undefined' && typeof o.then === 'function';
  } // ----------------------------------------------------------------------
  // :: Function utilities
  // ----------------------------------------------------------------------
  // :: weak version fn.bind as function - it can be rebinded and
  // :: and applied with different context after bind
  // ----------------------------------------------------------------------


  function weakBind(fn, context) {
    var binded = function binded() {
      for (var _len2 = arguments.length, moreArgs = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
        moreArgs[_key2] = arguments[_key2];
      }

      var args = [].concat(_toConsumableArray(binded.__bind.args), moreArgs);
      return binded.__bind.fn.apply(context, args);
    };

    if (fn.__doc__) {
      binded.__doc__ = fn.__doc__;
    }

    if (fn.__code__) {
      binded.__code__ = fn.__code__;
    }

    for (var _len = arguments.length, args = new Array(_len > 2 ? _len - 2 : 0), _key = 2; _key < _len; _key++) {
      args[_key - 2] = arguments[_key];
    }

    binded.__bind = {
      args: fn.__bind ? fn.__bind.args.concat(args) : args,
      fn: fn.__bind ? fn.__bind.fn : fn
    };

    binded.toString = function () {
      return binded.__bind.fn.toString();
    };

    binded.apply = function (context, args) {
      return binded.__bind.fn.apply(context, args);
    };

    binded.call = function (context) {
      var _binded$__bind$fn;

      for (var _len3 = arguments.length, args = new Array(_len3 > 1 ? _len3 - 1 : 0), _key3 = 1; _key3 < _len3; _key3++) {
        args[_key3 - 1] = arguments[_key3];
      }

      return (_binded$__bind$fn = binded.__bind.fn).call.apply(_binded$__bind$fn, [context].concat(args));
    };

    binded.bind = function (context) {
      for (var _len4 = arguments.length, moreArgs = new Array(_len4 > 1 ? _len4 - 1 : 0), _key4 = 1; _key4 < _len4; _key4++) {
        moreArgs[_key4 - 1] = arguments[_key4];
      }

      return weakBind.apply(void 0, [binded, context].concat(moreArgs));
    };

    return setFnLength(binded, binded.__bind.fn.length);
  } // ----------------------------------------------------------------------


  function unbind(obj) {
    if (typeof obj === 'function' && obj.__bind) {
      return obj.__bind.fn;
    }

    return obj;
  } // ----------------------------------------------------------------------
  // :: function bind fn with context but it also move all props
  // :: mostly used for Object function
  // ----------------------------------------------------------------------


  var exludedNames = ['name', 'length', 'caller', 'callee', 'arguments', 'prototype'];

  function filterFnNames(name) {
    return !exludedNames.includes(name);
  } // ----------------------------------------------------------------------


  function bindWithProps(fn, context) {
    var bound = fn.bind(context);
    var props = Object.getOwnPropertyNames(fn).filter(filterFnNames);
    props.forEach(function (prop) {
      bound[prop] = fn[prop];
    });

    if (isNativeFunction(fn)) {
      Object.defineProperty(bound, root.Symbol.for('__native__'), {
        value: true,
        writable: false,
        configurable: false,
        enumerable: false
      });
    }

    return bound;
  } // ----------------------------------------------------------------------


  function setFnLength(fn, length) {
    try {
      Object.defineProperty(fn, 'length', {
        get: function get() {
          return length;
        }
      });
      return fn;
    } catch (e) {
      // hack that create function with specific length should work for browsers
      // that don't support Object.defineProperty like old IE
      var args = new Array(length).fill(0).map(function (_, i) {
        return 'a' + i;
      }).join(',');
      var wrapper = new Function("f", "return function(".concat(args, ") {\n                return f.apply(this, arguments);\n            };"));
      return wrapper(fn);
    }
  } // ----------------------------------------------------------------------


  function isNativeFunction(fn) {
    var native = root.Symbol.for('__native__');
    return typeof fn === 'function' && fn.toString().match(/\{\s*\[native code\]\s*\}/) && (fn.name.match(/^bound /) && fn[native] === true || !fn.name.match(/^bound /) && !fn[native]);
  } // ----------------------------------------------------------------------
  // :: function that return macro for let and let*
  // ----------------------------------------------------------------------


  function let_macro(asterisk) {
    var name = 'let' + (asterisk ? '*' : '');
    return Macro.defmacro(name, function (code, options) {
      var dynamic_scope = options.dynamic_scope,
          error = options.error,
          macro_expand = options.macro_expand;
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
      var i = 0;
      return function loop() {
        var pair = args[i++];

        function set(value) {
          if (isPromise(value)) {
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
          return evaluate(output, {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          });
        } else {
          var value = evaluate(pair.cdr.car, {
            env: asterisk ? env : self,
            dynamic_scope: dynamic_scope,
            error: error
          });
          return unpromise(set(value), loop);
        }
      }();
    });
  } // -------------------------------------------------------------------------


  function pararel(name, fn) {
    return new Macro(name, function (code) {
      var _ref5 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref5.dynamic_scope,
          error = _ref5.error;

      var env = this;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      var node = code;
      var results = [];

      while (node instanceof Pair && !isEmptyList(node)) {
        results.push(evaluate(node.car, {
          env: env,
          dynamic_scope: dynamic_scope,
          error: error
        }));
        node = node.cdr;
      }

      var havePromises = results.filter(isPromise).length;

      if (havePromises) {
        return Promise.all(results).then(fn.bind(this));
      } else {
        return fn.call(this, results);
      }
    });
  } // -------------------------------------------------------------------------


  function guardMathCall(fn) {
    for (var _len5 = arguments.length, args = new Array(_len5 > 1 ? _len5 - 1 : 0), _key5 = 1; _key5 < _len5; _key5++) {
      args[_key5 - 1] = arguments[_key5];
    }

    args.forEach(function (arg) {
      typecheck('', arg, 'number');
    });
    return fn.apply(void 0, args);
  } // ----------------------------------------------------------------------


  function pipe() {
    for (var _len6 = arguments.length, fns = new Array(_len6), _key6 = 0; _key6 < _len6; _key6++) {
      fns[_key6] = arguments[_key6];
    }

    fns.forEach(function (fn, i) {
      typecheck('pipe', fn, 'function', i + 1);
    });
    return function () {
      for (var _len7 = arguments.length, args = new Array(_len7), _key7 = 0; _key7 < _len7; _key7++) {
        args[_key7] = arguments[_key7];
      }

      return fns.reduce(function (args, f) {
        return [f.apply(void 0, _toConsumableArray(args))];
      }, args)[0];
    };
  } // -------------------------------------------------------------------------


  function compose() {
    for (var _len8 = arguments.length, fns = new Array(_len8), _key8 = 0; _key8 < _len8; _key8++) {
      fns[_key8] = arguments[_key8];
    }

    fns.forEach(function (fn, i) {
      typecheck('compose', fn, 'function', i + 1);
    });
    return pipe.apply(void 0, _toConsumableArray(fns.reverse()));
  } // -------------------------------------------------------------------------
  // :: fold functions generator
  // -------------------------------------------------------------------------


  function fold(name, fold) {
    var self = this;
    return function recur(fn, init) {
      typecheck(name, fn, 'function');

      for (var _len9 = arguments.length, lists = new Array(_len9 > 2 ? _len9 - 2 : 0), _key9 = 2; _key9 < _len9; _key9++) {
        lists[_key9 - 2] = arguments[_key9];
      }

      if (lists.some(function (l) {
        return isEmptyList(l) || isNull(l);
      })) {
        if (typeof init === 'number') {
          return LNumber(init);
        }

        return init;
      } else {
        return fold.call.apply(fold, [self, recur, fn, init].concat(lists));
      }
    };
  } // -------------------------------------------------------------------------


  function limitMathOp(n, fn) {
    // + 1 so it inlcude function in guardMathCall
    return limit(n + 1, curry(guardMathCall, fn));
  } // -------------------------------------------------------------------------


  var singleMathOp = curry(limitMathOp, 1);
  var binaryMathOp = curry(limitMathOp, 2); // -------------------------------------------------------------------------

  function reduceMathOp(fn) {
    return function () {
      for (var _len10 = arguments.length, args = new Array(_len10), _key10 = 0; _key10 < _len10; _key10++) {
        args[_key10] = arguments[_key10];
      }

      if (args.length) {
        return args.reduce(binaryMathOp(fn));
      }
    };
  } // -------------------------------------------------------------------------


  function curry(fn) {
    for (var _len11 = arguments.length, init_args = new Array(_len11 > 1 ? _len11 - 1 : 0), _key11 = 1; _key11 < _len11; _key11++) {
      init_args[_key11 - 1] = arguments[_key11];
    }

    typecheck('curry', fn, 'function');
    var len = fn.length;
    return function () {
      var args = init_args.slice();

      function call() {
        for (var _len12 = arguments.length, more_args = new Array(_len12), _key12 = 0; _key12 < _len12; _key12++) {
          more_args[_key12] = arguments[_key12];
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
  } // -------------------------------------------------------------------------
  // return function with limited number of arguments


  function limit(n, fn) {
    typecheck('limit', fn, 'function', 2);
    return function () {
      for (var _len13 = arguments.length, args = new Array(_len13), _key13 = 0; _key13 < _len13; _key13++) {
        args[_key13] = arguments[_key13];
      }

      return fn.apply(void 0, _toConsumableArray(args.slice(0, n)));
    };
  } // -------------------------------------------------------------------------------


  var get = doc(function get(obj) {
    if (typeof obj === 'function' && obj.__bind) {
      obj = obj.__bind.fn;
    }

    for (var _len14 = arguments.length, args = new Array(_len14 > 1 ? _len14 - 1 : 0), _key14 = 1; _key14 < _len14; _key14++) {
      args[_key14 - 1] = arguments[_key14];
    }

    var _arr = args;

    for (var _i = 0; _i < _arr.length; _i++) {
      var arg = _arr[_i];
      var name = arg instanceof _Symbol ? arg.name : arg;
      var value = obj[name];

      if (typeof value === 'function') {
        value = bindWithProps(value, obj);
      }

      obj = value;
    }

    return value;
  }, "(. obj . args)\n        (get obj . args)\n\n        Function use object as based and keep using arguments to get the\n        property of JavaScript object. Arguments need to be a strings.\n        e.g. `(. console \"log\")` if you use any function inside LIPS is\n        will be weakly bind (can be rebind), so you can call this log function\n        without problem unlike in JavaScript when you use\n       `var log = console.log`.\n       `get` is an alias because . don't work in every place, you can't\n        pass it as argument"); // -------------------------------------------------------------------------
  // :: Number wrapper that handle BigNumbers
  // -------------------------------------------------------------------------

  function LNumber(n, float) {
    if (typeof this !== 'undefined' && this.constructor !== LNumber || typeof this === 'undefined') {
      return new LNumber(n, float === true ? true : undefined);
    }

    if (n instanceof LNumber) {
      return LNumber(n.value, n.float);
    }

    if (!LNumber.isNumber(n)) {
      throw new Error("You can't create LNumber from ".concat(type(n)));
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
  } // -------------------------------------------------------------------------


  LNumber.isFloat = function isFloat(n) {
    return Number(n) === n && n % 1 !== 0;
  }; // -------------------------------------------------------------------------


  LNumber.isNumber = function (n) {
    return n instanceof LNumber || !Number.isNaN(n) && LNumber.isNative(n) || LNumber.isBN(n);
  }; // -------------------------------------------------------------------------


  LNumber.isNative = function (n) {
    return typeof n === 'bigint' || typeof n === 'number';
  }; // -------------------------------------------------------------------------


  LNumber.isBN = function (n) {
    return typeof BN !== 'undefined' && n instanceof BN;
  }; // -------------------------------------------------------------------------


  LNumber.prototype.toString = LNumber.prototype.toJSON = function () {
    return this.value.toString();
  }; // -------------------------------------------------------------------------


  LNumber.prototype.isBigNumber = function () {
    return typeof this.value === 'bigint' || typeof BN !== 'undefined' && !(this.value instanceof BN);
  }; // -------------------------------------------------------------------------


  ['floor', 'ceil', 'round'].forEach(function (fn) {
    LNumber.prototype[fn] = function () {
      if (this.float || LNumber.isFloat(this.value)) {
        return LNumber(Math[fn](this.value));
      } else {
        return LNumber(this.value);
      }
    };
  }); // -------------------------------------------------------------------------

  LNumber.prototype.valueOf = function () {
    if (LNumber.isNative(this.value)) {
      return Number(this.value);
    } else if (LNumber.isBN(this.value)) {
      return this.value.toNumber();
    }
  }; // -------------------------------------------------------------------------


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
  }; // -------------------------------------------------------------------------


  LNumber.prototype.isFloat = function () {
    return !!(LNumber.isFloat(this.value) || this.float);
  }; // -------------------------------------------------------------------------


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

    if (LNumber.isFloat(n) || n instanceof LNumber && n.isFloat() || this.isFloat()) {
      var value = n instanceof LNumber ? n.valueOf() : n;
      return LNumber(ops[op](this.valueOf(), value), true);
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
      return LNumber(this.value.clone()[op](n, value));
    }
  }; // -------------------------------------------------------------------------


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
  }); // -------------------------------------------------------------------------

  LNumber.prototype.sqrt = function () {
    var value;

    if (LNumber.isNative(this.value)) {
      value = Math.sqrt(this.value);
    } else if (LNumber.isBN(this.value)) {
      value = this.value.sqrt();
    }

    return new LNumber(value);
  }; // -------------------------------------------------------------------------


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
  }; // -------------------------------------------------------------------------


  LNumber.prototype.neg = function () {
    var value = this.value;

    if (LNumber.isNative(value)) {
      value = -value;
    } else if (LNumber.isBN(value)) {
      value = value.neg();
    }

    return new LNumber(value);
  }; // -------------------------------------------------------------------------


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
  }; // -------------------------------------------------------------------------


  LNumber.prototype.isOdd = function () {
    if (LNumber.isNative(this.value)) {
      if (this.isBigNumber()) {
        return this.value % BigInt(2) === BigInt(1);
      }

      return this.value % 2 === 1;
    } else if (LNumber.isBN(this.value)) {
      return this.value.isOdd();
    }
  }; // -------------------------------------------------------------------------


  LNumber.prototype.isEven = function () {
    return !this.isOdd();
  }; // -------------------------------------------------------------------------


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
  }; // -------------------------------------------------------------------------
  // :: Environment constructor (parent and name arguments are optional)
  // -------------------------------------------------------------------------


  function Environment(obj, parent, name) {
    if (arguments.length === 1) {
      if (_typeof(arguments[0]) === 'object') {
        obj = arguments[0];
        this.parent = null;
      } else if (typeof arguments[0] === 'string') {
        obj = {};
        parent = {};
        name = arguments[0];
      }
    }

    this.env = obj;
    this.parent = parent;
    this.name = name || 'anonymous';
  } // -------------------------------------------------------------------------


  Environment.prototype.inherit = function (name) {
    var obj = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

    if (_typeof(name) === "object") {
      obj = name;
    }

    if (!name || _typeof(name) === "object") {
      name = 'child of ' + (this.name || 'unknown');
    }

    return new Environment(obj || {}, this, name);
  }; // -------------------------------------------------------------------------


  Environment.prototype.get = function (symbol, weak, context) {
    // we keep original environment as context for bind
    // so print will get user stdout
    context = context || this;
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

      if (value instanceof Pair) {
        return value.markCycles();
      }

      if (typeof value === 'function') {
        // bind only functions that are not binded for case:
        // (let ((x Object)) (. x 'keys))
        // second x access is already bound when accessing Object
        if (!value.name.match(/^bound /)) {
          if (weak) {
            return weakBind(value, context);
          }

          return bindWithProps(value, context);
        }
      }

      return value;
    }

    if (this.parent instanceof Environment) {
      return this.parent.get(symbol, weak, context);
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
          if (isNativeFunction(root[name])) {
            // hard bind of native functions with props for Object
            // hard because of console.log
            return bindWithProps(root[name], root);
          }

          return root[name];
        } else if (type !== 'undefined') {
          return root[name];
        }
      }
    }

    name = (name.name || name).toString();
    throw new Error("Unbound variable `" + name + "'");
  }; // -------------------------------------------------------------------------


  Environment.prototype.set = function (name, value) {
    if (LNumber.isNumber(value)) {
      value = LNumber(value);
    }

    if (name instanceof _Symbol) {
      name = name.name;
    }

    this.env[name] = value;
  }; // -------------------------------------------------------------------------


  Environment.prototype.has = function (name) {
    return typeof this.env[name] !== 'undefined';
  }; // -------------------------------------------------------------------------


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
  };

  Environment.prototype.parents = function () {
    var env = this;
    var result = [];

    while (env) {
      result.unshift(env);
      env = env.parent;
    }

    return result;
  }; // -------------------------------------------------------------------------
  // :: Quote funtion used to pause evaluation from Macro
  // -------------------------------------------------------------------------


  function quote(value) {
    if (isPromise(value)) {
      return value.then(quote);
    }

    if (value instanceof Pair || value instanceof _Symbol) {
      value.data = true;
    }

    return value;
  } // -------------------------------------------------------------------------
  // :: Unquote is used for multiple backticks and unquote
  // -------------------------------------------------------------------------


  function Unquote(value, count, max) {
    this.value = value;
    this.count = count;
    this.max = max;
  }

  Unquote.prototype.toString = function () {
    return '<#unquote[' + this.count + '] ' + this.value + '>';
  }; // -------------------------------------------------------------------------


  var gensym = function () {
    var count = 0;
    return function () {
      var name = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;

      // use ES6 symbol as name for lips symbol (they are unique)
      if (name !== null) {
        return new _Symbol(root.Symbol("#".concat(name)));
      }

      count++;
      return new _Symbol(root.Symbol("#gensym_".concat(count, "#")));
    };
  }(); // -------------------------------------------------------------------------


  var global_env = new Environment({
    nil: nil,
    'undefined': undefined,
    'true': true,
    'NaN': NaN,
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
    // ------------------------------------------------------------------
    help: doc(new Macro('help', function (code, _ref6) {
      var dynamic_scope = _ref6.dynamic_scope,
          error = _ref6.error;
      var symbol;

      if (code.car instanceof _Symbol) {
        symbol = code.car;
      } else if (code.car instanceof Pair && code.car.car instanceof _Symbol) {
        symbol = code.car.car;
      } else {
        var env = this;

        if (dynamic_scope) {
          dynamic_scope = this;
        }

        var ret = evaluate(code.car, {
          env: env,
          error: error,
          dynamic_scope: dynamic_scope
        });

        if (ret && ret.__doc__) {
          return ret.__doc__;
        }
      }

      return this.get(symbol).__doc__;
    }), "(help object)\n\n             Macro returns documentation for function or macros including parser\n             macros but only if called with parser macro symbol like (help `).\n             For normal functions and macros you can save the function in variable."),
    // ------------------------------------------------------------------
    cons: doc(function (car, cdr) {
      if (isEmptyList(cdr)) {
        cdr = nil;
      }

      return new Pair(car, cdr);
    }, "(cons left right)\n\n            Function return new Pair out of two arguments."),
    // ------------------------------------------------------------------
    car: doc(function (list) {
      if (list === nil) {
        return nil;
      }

      typecheck('car', list, 'pair');

      if (isEmptyList(list)) {
        return nil;
      }

      return list.car;
    }, "(car pair)\n\n            Function returns car (head) of the list/pair."),
    // ------------------------------------------------------------------
    cdr: doc(function (list) {
      if (list === nil) {
        return nil;
      }

      typecheck('cdr', list, 'pair');

      if (isEmptyList(list)) {
        return nil;
      }

      return list.cdr;
    }, "(cdr pair)\n\n            Function returns cdr (tail) of the list/pair."),
    // ------------------------------------------------------------------
    'set!': doc(new Macro('set!', function (code) {
      var _ref7 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref7.dynamic_scope,
          error = _ref7.error;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      var value = evaluate(code.cdr.car, {
        env: this,
        dynamic_scope: dynamic_scope,
        error: error
      });
      value = resolvePromises(value);
      var ref;

      function set(key, value) {
        if (isPromise(key)) {
          return key.then(function (key) {
            return set(key, value);
          });
        }

        if (isPromise(value)) {
          return value.then(function (value) {
            return set(key, value);
          });
        }

        object[key] = value;
        return value;
      }

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
        return set(key, value);
      }

      if (!(code.car instanceof _Symbol)) {
        throw new Error('set! first argument need to be a symbol or ' + 'dot accessor that evaluate to object.');
      }

      ref = this.ref(code.car.name);

      if (!ref) {
        ref = this;
      } // we don't return value because we only care about sync of set value
      // when value is a promise


      return unpromise(value, function (value) {
        ref.set(code.car, value);
      });
    }), "(set! name value)\n\n            Macro that can be used to set the value of the variable (mutate)\n            it search the scope chain until it finds first non emtpy slot and set it."),
    // ------------------------------------------------------------------
    'set-car!': doc(function (slot, value) {
      typecheck('set-car!', slot, 'pair');
      slot.car = value;
    }, "(set-car! obj value)\n\n            Function that set car (head) of the list/pair to specified value.\n            It can destroy the list. Old value is lost."),
    // ------------------------------------------------------------------
    'set-cdr!': doc(function (slot, value) {
      typecheck('set-cdr!', slot, 'pair');
      slot.cdr = value;
    }, "(set-cdr! obj value)\n\n            Function that set cdr (tail) of the list/pair to specified value.\n            It can destroy the list. Old value is lost."),
    // ------------------------------------------------------------------
    'empty?': doc(function (x) {
      return typeof x === 'undefined' || isEmptyList(x);
    }, "(empty? object)\n\n            Function return true if value is undfined empty list."),
    // ------------------------------------------------------------------
    assoc: doc(function (key, list) {
      if (key instanceof Pair && !(list instanceof Pair)) {
        throw new Error('First argument to assoc ned to be a key');
      }

      typecheck('assoc', list, 'pair');
      var node = list;

      while (true) {
        if (!(node instanceof Pair) || this.get('empty?')(node)) {
          break;
        }

        var car = node.car.car;

        if (equal(car, key)) {
          return node.car;
        } else if (!node.haveCycles('cdr')) {
          node = node.cdr;
        }
      }

      return nil;
    }, "(assoc key alist)\n\n            Function search Alist (list of pairs) until it find the one that\n            have head set equal to key, and return found pair."),
    // ------------------------------------------------------------------
    gensym: doc(gensym, "(gensym)\n\n             Function generate unique symbol, to use with macros as meta name."),
    // ------------------------------------------------------------------
    'require': doc(function (module) {
      return require(module);
    }, "(require module)\n\n            Function to be used inside Node.js to import the module."),
    // ------------------------------------------------------------------
    load: doc(function (file) {
      typecheck('load', file, 'string');
      var env = this;

      if (typeof this.env.global !== 'undefined') {
        return new Promise(function (resolve, reject) {
          require('fs').readFile(file, function (err, data) {
            if (err) {
              reject(err);
            } else {
              exec(data.toString(), env).then(function () {
                resolve();
              });
            }
          });
        });
      }

      return root.fetch(file).then(function (res) {
        return res.text();
      }).then(function (code) {
        return exec(code, env);
      }).then(function () {});
    }, "(load filename)\n\n            Function fetch the file and evaluate its content as LIPS code."),
    // ------------------------------------------------------------------
    'debugger': doc(function () {
      return new Promise(function (resolve) {
        debugger;
        resolve();
      });
    }, "(debugger)\n\n            Function stops executing of LIPS code in dev tools debugger."),
    // ------------------------------------------------------------------
    'while': doc(new Macro('while', function (code, _ref8) {
      var dynamic_scope = _ref8.dynamic_scope,
          error = _ref8.error;
      var self = this;
      var begin = new Pair(new _Symbol('begin'), code.cdr);
      var result;

      if (dynamic_scope) {
        dynamic_scope = self;
      }

      return function loop() {
        var cond = evaluate(code.car, {
          env: self,
          dynamic_scope: dynamic_scope,
          error: error
        });

        function next(cond) {
          if (cond && !isNull(cond) && !isEmptyList(cond)) {
            result = evaluate(begin, {
              env: self,
              dynamic_scope: dynamic_scope,
              error: error
            });

            if (isPromise(result)) {
              return result.then(function (ret) {
                result = ret;
                return loop();
              });
            } else {
              return loop();
            }
          } else {
            return result;
          }
        }

        return unpromise(cond, next);
      }();
    }), "(while cond . body)\n\n            Macro that create a loop, it exectue body untill cond expression is false"),
    // ------------------------------------------------------------------
    'if': doc(new Macro('if', function (code, _ref9) {
      var dynamic_scope = _ref9.dynamic_scope,
          error = _ref9.error;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      var env = this;

      var resolve = function resolve(cond) {
        typecheck('if', cond, 'boolean');

        if (cond) {
          return evaluate(code.cdr.car, {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          });
        } else {
          return evaluate(code.cdr.cdr.car, {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          });
        }
      };

      var cond = evaluate(code.car, {
        env: env,
        dynamic_scope: dynamic_scope,
        error: error
      });
      return unpromise(cond, resolve);
    }), "(if cond true-expr false-expr)\n\n            Macro evaluate condition expression and if the value is true, it\n            evaluate and return true expression if not it evaluate and return\n            false expression"),
    // ------------------------------------------------------------------
    'let*': doc(let_macro(true), "(let* ((a value-a) (b value-b)) body)\n\n             Macro that creates new environment, then evaluate and assign values to\n             names and then evaluate the body in context of that environment.\n             Values are evaluated sequentialy and next value can access to\n             previous values/names."),
    // ------------------------------------------------------------------
    'let': doc(let_macro(false), "(let ((a value-a) (b value-b)) body)\n\n             Macro that creates new environment, then evaluate and assign values to\n             names and then evaluate the body in context of that environment.\n             Values are evaluated sequentialy but you can't access\n             previous values/names when next are evaluated. You can only get them\n             from body of let expression."),
    // ------------------------------------------------------------------
    'begin*': doc(pararel('begin*', function (values) {
      return values.pop();
    }), "(begin* . expr)\n\n             This macro is parallel version of begin. It evaluate each expression and\n             if it's a promise it will evaluate it in parallel and return value\n             of last expression."),
    // ------------------------------------------------------------------
    'begin': doc(new Macro('begin', function (code, options) {
      var args = Object.assign({}, options);
      var arr = this.get('list->array')(code);

      if (args.dynamic_scope) {
        args.dynamic_scope = this;
      }

      args.env = this;
      var result;
      return function loop() {
        if (arr.length) {
          var code = arr.shift();
          var ret = evaluate(code, args);
          return unpromise(ret, function (value) {
            result = value;
            return loop();
          });
        } else {
          return result;
        }
      }();
    }), "(begin . args)\n\n             Macro runs list of expression and return valuate of the list one.\n             It can be used in place where you can only have single exression,\n             like if expression."),
    // ------------------------------------------------------------------
    'ignore': new Macro('ignore', function (code, _ref10) {
      var dynamic_scope = _ref10.dynamic_scope,
          error = _ref10.error;
      var args = {
        env: this,
        error: error
      };

      if (dynamic_scope) {
        args.dynamic_scope = this;
      }

      evaluate(code, args);
    }, "(ignore expression)\n\n            Macro that will evaluate expression and swallow any promises that may\n            be created. It wil run and ignore any value that may be returned by\n            expression. The code should have side effects and/or when it's promise\n            it should resolve to undefined."),
    // ------------------------------------------------------------------
    timer: doc(new Macro('timer', function (code) {
      var _ref11 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref11.dynamic_scope,
          error = _ref11.error;

      typecheck('timer', code.car, 'number');
      var env = this;

      if (dynamic_scope) {
        dynamic_scope = this;
      }

      return new Promise(function (resolve) {
        setTimeout(function () {
          resolve(evaluate(code.cdr.car, {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          }));
        }, code.car);
      }).then(quote);
    }), "(timer time expression)\n\n             Function return a promise, and it will be automatically evaluated\n             after specific time passes. The return value of the function\n             will be value of the timer exprssion. If you want to do side effect\n             only expression you can wrap your expression in nol call."),
    // ------------------------------------------------------------------
    define: doc(Macro.defmacro('define', function (code, eval_args) {
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
      } else if (value instanceof _Symbol) {
        value = env.get(value);
      }

      if (code.car instanceof _Symbol) {
        unpromise(value, function (value) {
          env.set(code.car, value);
        });
      }
    }), "(define name expression)\n             (define (function-name . args) body)\n\n             Macro for defining values. It can be used to define variables,\n             or function. If first argument is list it will create function\n             with name beeing first element of the list. The macro evalute\n             code `(define function (lambda args body))`"),
    // ------------------------------------------------------------------
    'set-obj!': doc(function (obj, key, value) {
      if (_typeof(obj) !== 'object' || isNull(obj)) {
        throw new Error(typeErrorMessage('set-obj!', type(obj), 'object'));
      }

      obj[key] = value;
    }, "(set-obj! obj key value)\n\n            Function set property of JavaScript object"),
    'current-environment': doc(function () {
      return this;
    }, "(current-environment)\n\n            Function return current environement."),
    // ------------------------------------------------------------------
    'eval': doc(function (code, env) {
      var _this = this;

      typecheck('eval', code, ['symbol', 'pair', 'array']);
      env = env || this;

      if (code instanceof _Symbol) {
        return env.get(code);
      }

      if (code instanceof Pair) {
        return evaluate(code, {
          env: env,
          //dynamic_scope: this,
          error: function error(e) {
            _this.get('error')(e.message);

            if (e.code) {
              var stack = e.code.map(function (line, i) {
                return "[".concat(i + 1, "]: ").concat(line);
              }).join('\n');

              _this.get('error')(stack);
            }
          }
        });
      }

      if (code instanceof Array) {
        var _eval = this.get('eval');

        return code.reduce(function (_, code) {
          return _eval(code, env);
        });
      }
    }, "(eval list)\n\n            Function evalute LIPS code as list structure."),
    // ------------------------------------------------------------------
    lambda: new Macro('lambda', function (code) {
      var _ref12 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref12.dynamic_scope,
          error = _ref12.error;

      var self = this;

      var __doc__;

      if (code.cdr instanceof Pair && typeof code.cdr.car === 'string' && code.cdr.cdr !== nil) {
        __doc__ = code.cdr.car;
      }

      function lambda() {
        var env = (dynamic_scope ? this : self).inherit('lambda');
        var name = code.car;
        var i = 0;
        var value;

        if (name instanceof _Symbol || !isEmptyList(name)) {
          for (var _len15 = arguments.length, args = new Array(_len15), _key15 = 0; _key15 < _len15; _key15++) {
            args[_key15] = arguments[_key15];
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

        var rest = __doc__ ? code.cdr.cdr : code.cdr;
        var output = new Pair(new _Symbol('begin'), rest);
        return evaluate(output, {
          env: env,
          dynamic_scope: dynamic_scope,
          error: error
        });
      }

      var length = code.car instanceof Pair ? code.car.length() : null;
      lambda.__code__ = new Pair(new _Symbol('lambda'), code);

      if (!(code.car instanceof Pair)) {
        return doc(lambda, __doc__, true); // variable arguments
      } // wrap and decorate with __doc__


      return doc(setFnLength(lambda, length), __doc__, true);
    }, "(lambda (a b) body)\n            (lambda args body)\n            (lambda (a b . rest) body)\n\n            Macro lambda create new anonymous function, if first element of the body\n            is string and there is more elements it will be documentation, that can\n            be read using (help fn)"),
    'macroexpand': new Macro('macroexpand', macroExpand()),
    'macroexpand-1': new Macro('macroexpand-1', macroExpand(true)),
    // ------------------------------------------------------------------
    'define-macro': doc(new Macro(macro, function (macro, _ref13) {
      var dynamic_scope = _ref13.dynamic_scope,
          error = _ref13.error;

      if (macro.car instanceof Pair && macro.car.car instanceof _Symbol) {
        var name = macro.car.car.name;

        var __doc__;

        if (typeof macro.cdr.car === 'string' && macro.cdr.cdr !== nil) {
          __doc__ = macro.cdr.car;
        }

        this.env[name] = Macro.defmacro(name, function (code) {
          var env = new Environment({}, this, 'defmacro');
          var name = macro.car.cdr;
          var arg = code;

          while (true) {
            if (name === nil) {
              break;
            }

            if (name instanceof _Symbol) {
              env.env[name.name] = arg;
              break;
            } else if (name.car !== nil) {
              if (arg === nil) {
                env.env[name.car.name] = nil;
              } else {
                env.env[name.car.name] = arg.car;
              }
            }

            if (name.cdr === nil) {
              break;
            }

            if (arg !== nil) {
              arg = arg.cdr;
            }

            name = name.cdr;
          }

          if (dynamic_scope) {
            dynamic_scope = env;
          }

          var eval_args = {
            env: env,
            dynamic_scope: dynamic_scope,
            error: error
          }; // evaluate macro

          if (macro.cdr instanceof Pair) {
            // this eval will return lips code
            var rest = __doc__ ? macro.cdr.cdr : macro.cdr;
            var result = rest.reduce(function (result, node) {
              return evaluate(node, eval_args);
            });
            return unpromise(result, function (result) {
              if (_typeof(result) === 'object') {
                delete result.data;
              }

              return result;
            });
          }
        }, __doc__);
        this.env[name].__code__ = new Pair(new _Symbol('define-macro'), macro);
      }
    }), "(define-macro (name . args) body)\n\n             Meta macro, macro that create new macros, if return value is list structure\n             it will be evaluated when macro is invoked. You can use quasiquote ` and\n             unquote , and unquote-splicing ,@ inside to create expression that will be\n             evaluated on runtime. Macros works like this: if you pass any expression to\n             macro the arguments will not be evaluated unless macro itself evaluate it.\n             Because of this macro can manipulate expression (arguments) as lists."),
    // ------------------------------------------------------------------
    quote: doc(new Macro('quote', function (arg) {
      return quote(arg.car);
    }), "(quote expression)\n\n             Macro that return single lips expression as data (it don't evaluate its\n             argument). It will return list of pairs if put in front of lips code.\n             And if put in fron of symbol it will return that symbol not value\n             associated with that name."),
    'unquote-splicing': doc(function () {
      throw new Error("You can't call `unquote-splicing` outside of quasiquote");
    }, "(unquote-splicing code)\n\n            Special form to be used in quasiquote macro, parser is processing special\n            characters ,@ and create call to this pseudo function. It can be used\n            to evalute expression inside and return the value without parenthesis.\n            the value will be joined to the output list structure."),
    'unquote': doc(function () {
      throw new Error("You can't call `unquote` outside of quasiquote");
    }, "(unquote code)\n\n            Special form to be used in quasiquote macro, parser is processing special\n            characters , and create call to this pseudo function. It can be used\n            to evalute expression inside and return the value, the output is inserted\n            into list structure created by queasiquote."),
    // ------------------------------------------------------------------
    quasiquote: Macro.defmacro('quasiquote', function (arg, env) {
      var dynamic_scope = env.dynamic_scope,
          error = env.error;
      var self = this; //var max_unquote = 1;

      if (dynamic_scope) {
        dynamic_scope = (_readOnlyError("dynamic_scope"), self);
      }

      function isPair(value) {
        return value instanceof Pair;
      }

      function resolve_pair(pair, fn) {
        var test = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : isPair;

        if (pair instanceof Pair && !isEmptyList(pair)) {
          var car = pair.car;
          var cdr = pair.cdr;

          if (test(car)) {
            car = fn(car);
          }

          if (test(cdr)) {
            cdr = fn(cdr);
          }

          if (isPromise(car) || isPromise(cdr)) {
            return Promise.all([car, cdr]).then(function (_ref14) {
              var _ref15 = _slicedToArray(_ref14, 2),
                  car = _ref15[0],
                  cdr = _ref15[1];

              return new Pair(car, cdr);
            });
          } else {
            return new Pair(car, cdr);
          }
        }

        return pair;
      }

      function join(eval_pair, value) {
        if (eval_pair instanceof Pair) {
          if (isEmptyList(eval_pair) && value === nil) {
            return nil;
          }

          eval_pair.append(value);
        } else {
          eval_pair = new Pair(eval_pair, value);
        }

        return eval_pair;
      }

      function recur(pair, unquote_cnt, max_unq) {
        if (pair instanceof Pair && !isEmptyList(pair)) {
          var eval_pair;

          if (_Symbol.is(pair.car.car, 'unquote-splicing')) {
            eval_pair = evaluate(pair.car.cdr.car, {
              env: self,
              dynamic_scope: dynamic_scope,
              error: error
            });
            return unpromise(eval_pair, function (eval_pair) {
              if (!(eval_pair instanceof Pair)) {
                return eval_pair;
              }

              var value = recur(pair.cdr, 0, 1);

              if (value === nil && eval_pair === nil) {
                return undefined;
              }

              return unpromise(value, function (value) {
                return join(eval_pair, value);
              });
            });
          }

          if (_Symbol.is(pair.car, 'quasiquote')) {
            var cdr = recur(pair.cdr, unquote_cnt, max_unq + 1);
            return new Pair(pair.car, cdr);
          }

          if (_Symbol.is(pair.car, 'unquote')) {
            var head = pair.cdr;
            var node = head;
            var parent = node;
            unquote_cnt++;

            while (_Symbol.is(node.car.car, 'unquote')) {
              parent = node;
              unquote_cnt++;
              node = node.car.cdr.car;
            } // we use Unquote to proccess inner most unquote first
            // in unquote function afer processing whole s-expression


            if (parent === node) {
              if (pair.cdr.cdr !== nil) {
                return unpromise(recur(pair.cdr.cdr), function (value) {
                  var unquoted = new Unquote(pair.cdr.car, unquote_cnt, max_unq);
                  return new Pair(unquoted, value);
                });
              } else {
                return new Unquote(pair.cdr.car, unquote_cnt, max_unq);
              }
            } else if (parent.cdr.cdr !== nil) {
              var value = recur(parent.cdr.cdr, unquote_cnt, max_unq);
              return unpromise(value, function (value) {
                parent.car.cdr = new Pair(new Unquote(node, unquote_cnt, max_unq), parent.cdr === nil ? nil : value);
                return head.car;
              });
            } else {
              parent.car.cdr = new Unquote(node, unquote_cnt, max_unq);
            }

            return head.car;
          }

          return resolve_pair(pair, function (pair) {
            return recur(pair, unquote_cnt, max_unq);
          });
        }

        return pair;
      }

      var unquoteTest = function unquoteTest(v) {
        return isPair(v) || v instanceof Unquote;
      };

      function unquoting(node) {
        if (node instanceof Unquote) {
          if (node.max === node.count) {
            var ret = evaluate(node.value, {
              env: self,
              dynamic_scope: dynamic_scope,
              error: error
            });
            return ret;
          } else {
            return unpromise(unquoting(node.value), function (value) {
              return new Pair(new _Symbol('unquote'), new Pair(value, nil));
            });
          }
        }

        return resolve_pair(node, unquoting, unquoteTest);
      }

      var x = recur(arg.car, 0, 1);
      return unpromise(x, function (value) {
        value = unquoting(value);
        return unpromise(value, quote);
      });
    }, "(quasiquote list ,value ,@value)\n\n            Similar macro to `quote` but inside it you can use special\n            expressions unquote abbreviated to , that will evaluate expresion inside\n            and return its value or unquote-splicing abbreviated to ,@ that will\n            evaluate expression but return value without parenthesis (it will join)\n            the list with its value. Best used with macros but it can be used outside"),
    // ------------------------------------------------------------------
    clone: doc(function (list) {
      typecheck('clone', list, 'pair');
      return list.clone();
    }, "(clone list)\n\n            Function return clone of the list."),
    // ------------------------------------------------------------------
    append: doc(function (list, item) {
      typecheck('append', list, 'pair');
      return this.get('append!')(list.clone(), item);
    }, "(append list item)\n\n            Function will create new list with value appended to the end. It return\n            New list."),
    'append!': doc(function (list, item) {
      typecheck('append!', list, 'pair');

      if (isNull(item) || isEmptyList(item)) {
        return list;
      }

      return list.append(item);
    }, "(append! name expression)\n\n             Destructive version of append, it modify the list in place. It return\n             original list."),
    // ------------------------------------------------------------------
    reverse: doc(function (arg) {
      typecheck('reverse', arg, ['array', 'pair']);

      if (arg instanceof Pair) {
        var arr = this.get('list->array')(arg).reverse();
        return this.get('array->list')(arr);
      } else if (!(arg instanceof Array)) {
        throw new Error(typeErrorMessage('reverse', type(arg), 'array or pair'));
      } else {
        return arg.reverse();
      }
    }, "(reverse list)\n\n            Function will reverse the list or array. If value is not a list\n            or array it will throw exception."),
    // ------------------------------------------------------------------
    nth: doc(function (index, obj) {
      typecheck('nth', index, 'number');
      typecheck('nth', obj, ['array', 'pair']);

      if (obj instanceof Pair) {
        var node = obj;
        var count = 0;

        while (count < index) {
          if (!node.cdr || node.cdr === nil || node.haveCycles('cdr')) {
            return nil;
          }

          node = node.cdr;
          count++;
        }

        return node.car;
      } else if (obj instanceof Array) {
        return obj[index];
      } else {
        throw new Error(typeErrorMessage('nth', type(obj), 'array or pair', 2));
      }
    }, "(nth index obj)\n\n            Function return nth element of the list or array. If used with different\n            value it will throw exception"),
    // ------------------------------------------------------------------
    list: doc(function () {
      for (var _len16 = arguments.length, args = new Array(_len16), _key16 = 0; _key16 < _len16; _key16++) {
        args[_key16] = arguments[_key16];
      }

      return Pair.fromArray(args);
    }, "(list . args)\n\n            Function create new list out of its arguments."),
    // ------------------------------------------------------------------
    substring: doc(function (string, start, end) {
      typecheck('substring', string, 'string');
      typecheck('substring', start, 'number');
      typecheck('substring', end, ['number', 'undefined']);
      return string.substring(start.valueOf(), end && end.valueOf());
    }, "(substring string start end)\n\n            Function return part of the string starting at start ending with end."),
    // ------------------------------------------------------------------
    concat: doc(function () {
      for (var _len17 = arguments.length, args = new Array(_len17), _key17 = 0; _key17 < _len17; _key17++) {
        args[_key17] = arguments[_key17];
      }

      args.forEach(function (arg, i) {
        return typecheck('concat', arg, 'string', i + 1);
      });
      return args.join('');
    }, "(concat . strings)\n\n            Function create new string by joining its arguments"),
    // ------------------------------------------------------------------
    join: doc(function (separator, list) {
      typecheck('join', separator, 'string');
      typecheck('join', list, 'pair');
      return this.get('list->array')(list).join(separator);
    }, "(join separator list)\n\n            Function return string by joining elements of the list"),
    // ------------------------------------------------------------------
    split: doc(function (separator, string) {
      typecheck('split', separator, ['regex', 'string']);
      typecheck('split', string, 'string');
      return this.get('array->list')(string.split(separator));
    }, "(split separator string)\n\n            Function create list by splitting string by separatar that can\n            be a string or regular expression."),
    // ------------------------------------------------------------------
    replace: doc(function (pattern, replacement, string) {
      typecheck('replace', pattern, ['regex', 'string']);
      typecheck('replace', replacement, ['string', 'function']);
      typecheck('replace', string, 'string');
      return string.replace(pattern, replacement);
    }, "(replace pattern replacement string)\n\n            Function change pattern to replacement inside string. Pattern can be string\n            or regex and replacement can be function or string."),
    // ------------------------------------------------------------------
    match: doc(function (pattern, string) {
      typecheck('match', pattern, ['regex', 'string']);
      typecheck('match', string, 'string');
      var m = string.match(pattern);
      return m ? this.get('array->list')(m) : nil;
    }, "(match pattern string)\n\n            function return match object from JavaScript as list."),
    // ------------------------------------------------------------------
    search: doc(function (pattern, string) {
      typecheck('search', pattern, ['regex', 'string']);
      typecheck('search', string, 'string');
      return string.search(pattern);
    }, "(search pattern string)\n\n            Function return first found index of the pattern inside a string"),
    // ------------------------------------------------------------------
    string: doc(function string(obj, quote) {
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
        if (isNativeFunction(obj)) {
          return '<#function(native)>';
        }

        return '<#function>';
      }

      if (obj instanceof RegExp) {
        return obj.toString();
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
        return JSON.stringify(obj).replace(/\\n/g, '\n');
      }

      if (obj instanceof Pair) {
        return new lips.Formatter(obj.toString()).break().format();
      }

      if (obj instanceof _Symbol) {
        return obj.toString();
      }

      if (root.HTMLElement && obj instanceof root.HTMLElement) {
        return "<#HTMLElement(".concat(obj.tagName.toLowerCase(), ")>");
      }

      if (_typeof(obj) === 'object') {
        var constructor = obj.constructor;
        var name;

        if (typeof constructor.__className === 'string') {
          name = constructor.__className;
        } else {
          name = constructor.name;
        }

        if (name !== '') {
          return '<#' + name + '>';
        }

        return '<#Object>';
      }

      if (typeof obj !== 'string') {
        return obj.toString();
      }

      return obj;
    }, "(string obj)\n\n            Function return string LIPS representation of an object as string."),
    // ------------------------------------------------------------------
    env: doc(function (env) {
      env = env || this;
      var names = Object.keys(env.env);
      var result;

      if (names.length) {
        result = Pair.fromArray(names);
      } else {
        result = nil;
      }

      if (env.parent !== undefined) {
        return this.get('env').call(this, env.parent).append(result);
      }

      return result;
    }, "(env obj)\n\n            Function return list values (functions and variables) inside environment."),
    'new': doc(function (obj) {
      for (var _len18 = arguments.length, args = new Array(_len18 > 1 ? _len18 - 1 : 0), _key18 = 1; _key18 < _len18; _key18++) {
        args[_key18 - 1] = arguments[_key18];
      }

      return _construct(obj, args);
    }, "(new obj . args)\n\n            Function create new JavaScript instance of an object."),
    // ------------------------------------------------------------------
    'unset!': doc(function (symbol) {
      typecheck('unset!', symbol, 'symbol');
      delete this.env[symbol.name];
    }, "(unset! name)\n\n            Function delete specified name from environment."),
    // ------------------------------------------------------------------
    'remove-special!': doc(function (symbol) {
      typecheck('remove-special!', symbol, 'string');
      delete specials[symbol];
    }, "(remove-special! symbol)\n\n            Function remove special symbol from parser. Added by `add-special!`"),
    // ------------------------------------------------------------------
    'add-special!': doc(function (symbol, name) {
      typecheck('remove-special!', symbol, 'string', 1);
      typecheck('remove-special!', name, 'symbol', 2);
      lips.specials[symbol] = name;
    }, "(add-special! symbol name)\n\n            Add special symbol to the list of transforming operators by the parser.\n            e.g.: `(add-special! '#)` will allow to use `#(1 2 3)` and it will be\n            transformed into (# (1 2 3)) so you can write # macro that will process\n            the list. It's main purpose to to allow to use `define-symbol-macro`"),
    // ------------------------------------------------------------------
    'get': get,
    '.': get,
    // ------------------------------------------------------------------
    'unbind': doc(unbind, "(unbind fn)\n\n             Function remove bidning from function so you can get props from it."),
    // ------------------------------------------------------------------
    type: doc(type, "(type object)\n\n             Function return type of an object as string."),
    // ------------------------------------------------------------------
    'instanceof': doc(function (type, obj) {
      return obj instanceof type;
    }, "(instanceof type obj)\n\n            Function check of object is instance of object."),
    // ------------------------------------------------------------------
    'macro?': doc(function (obj) {
      return obj instanceof Macro;
    }, "(macro? expression)\n\n            Function check if value is a macro."),
    // ------------------------------------------------------------------
    'function?': doc(function (obj) {
      return typeof obj === 'function';
    }, "(function? expression)\n\n            Function check if value is a function."),
    // ------------------------------------------------------------------
    'real?': doc(function (value) {
      if (type(value) !== 'number') {
        return false;
      }

      if (value instanceof LNumber) {
        return value.isFloat();
      }

      return LNumber.isFloat(value);
    }, "(real? number)\n\n            Function check if value is real number."),
    // ------------------------------------------------------------------
    'number?': doc(LNumber.isNumber, "(number? expression)\n\n             Function check if value is a number"),
    // ------------------------------------------------------------------
    'string?': doc(function (obj) {
      return typeof obj === 'string';
    }, "(string? expression)\n\n            Function check if value is a string."),
    // ------------------------------------------------------------------
    'pair?': doc(function (obj) {
      return obj instanceof Pair;
    }, "(pair? expression)\n\n            Function check if value is a pair or list structure."),
    // ------------------------------------------------------------------
    'regex?': doc(function (obj) {
      return obj instanceof RegExp;
    }, "(regex? expression)\n\n            Function check if value is regular expression."),
    // ------------------------------------------------------------------
    'null?': doc(function (obj) {
      return isNull(obj) || obj instanceof Pair && obj.isEmptyList();
    }, "(null? expression)\n\n            Function check if value is nulish."),
    // ------------------------------------------------------------------
    'boolean?': doc(function (obj) {
      return typeof obj === 'boolean';
    }, "(boolean? expression)\n\n            Function check if value is boolean."),
    // ------------------------------------------------------------------
    'symbol?': doc(function (obj) {
      return obj instanceof _Symbol;
    }, "(symbol? expression)\n\n            Function check if value is LIPS symbol"),
    // ------------------------------------------------------------------
    'array?': doc(function (obj) {
      return obj instanceof Array;
    }, "(array? expression)\n\n            Function check if value is an arrray."),
    // ------------------------------------------------------------------
    'object?': doc(function (obj) {
      return obj !== nil && obj !== null && !(obj instanceof LNumber) && _typeof(obj) === 'object' && !(obj instanceof Array);
    }, "(object? expression)\n\n            Function check if value is an object."),
    // ------------------------------------------------------------------
    read: doc(function read(arg) {
      var _this2 = this;

      if (typeof arg === 'string') {
        return parse(tokenize(arg));
      }

      return this.get('stdin').read().then(function (text) {
        return read.call(_this2, text);
      });
    }, "(read [string])\n\n            Function if used with string will parse the string and return\n            list structure of LIPS code. If called without an argument it\n            will read string from standard input (using browser prompt or\n            user defined way) and call itself with that string (parse is)\n            function can be used together with eval to evaluate code from\n            string"),
    // ------------------------------------------------------------------
    print: doc(function () {
      var _this$get,
          _this3 = this;

      for (var _len19 = arguments.length, args = new Array(_len19), _key19 = 0; _key19 < _len19; _key19++) {
        args[_key19] = arguments[_key19];
      }

      (_this$get = this.get('stdout')).write.apply(_this$get, _toConsumableArray(args.map(function (arg) {
        return _this3.get('string').call(_this3, arg);
      })));
    }, "(print . args)\n\n            Function convert each argument to string and print the result to\n            standard output (by default it's console but it can be defined\n            it user code)"),
    // ------------------------------------------------------------------
    error: doc(function () {
      this.get('print').apply(void 0, arguments);
    }, "(error . args)\n\n            Display error message."),
    // ------------------------------------------------------------------
    flatten: doc(function (list) {
      typecheck('flatten', list, 'pair');
      return list.flatten();
    }, "(flatten list)\n\n            Return shallow list from tree structure (pairs)."),
    // ------------------------------------------------------------------
    'array->list': doc(function (array) {
      typecheck('array->list', array, 'array');
      return Pair.fromArray(array);
    }, "(array->list array)\n\n            Function convert JavaScript array to LIPS list."),
    // ------------------------------------------------------------------
    'tree->array': doc(toArray('tree->array', true), "(tree->array list)\n\n             Function convert LIPS list structure into JavaScript array."),
    // ------------------------------------------------------------------
    'list->array': doc(toArray('list->array'), "(list->array list)\n\n             Function convert LIPS list into JavaScript array."),
    // ------------------------------------------------------------------
    apply: doc(function (fn, list) {
      typecheck('call', fn, 'function', 1);
      typecheck('call', list, 'pair', 2);
      return fn.apply(void 0, _toConsumableArray(this.get('list->array')(list)));
    }, "(apply fn list)\n\n            Function that call function with list of arguments."),
    // ------------------------------------------------------------------
    'length': doc(function (obj) {
      if (!obj) {
        return LNumber(0);
      }

      if (obj instanceof Pair) {
        return LNumber(obj.length());
      }

      if ("length" in obj) {
        return LNumber(obj.length);
      }
    }, "(length expression)\n\n            Function return length of the object, the object can be list\n            or any object that have length property."),
    'try': doc(new Macro('try', function (code, _ref16) {
      var _this4 = this;

      var dynamic_scope = _ref16.dynamic_scope,
          _error = _ref16.error;
      return new Promise(function (resolve) {
        var args = {
          env: _this4,
          error: function error(e) {
            var env = _this4.inherit('try');

            env.set(code.cdr.car.cdr.car.car, e);
            var args = {
              env: env,
              error: _error
            };

            if (dynamic_scope) {
              args.dynamic_scope = _this4;
            }

            unpromise(evaluate(new Pair(new _Symbol('begin'), code.cdr.car.cdr.cdr), args), function (result) {
              resolve(result);
            });
          }
        };

        if (dynamic_scope) {
          args.dynamic_scope = _this4;
        }

        var ret = evaluate(code.car, args);

        if (isPromise(ret)) {
          ret.catch(args.error).then(resolve);
        } else {
          resolve(ret);
        }
      });
    }), "(try expr (catch (e) code)"),
    // ------------------------------------------------------------------
    find: doc(function find(arg, list) {
      typecheck('find', arg, ['regex', 'function']);
      typecheck('find', list, 'pair');

      if (isNull(list)) {
        return nil;
      }

      var fn = matcher('find', arg);
      return unpromise(fn(list.car), function (value) {
        if (value && value !== nil) {
          return list.car;
        }

        return find(arg, list.cdr);
      });
    }, "(find fn list)\n            (find regex list)\n\n            Higher order Function find first value for which function return true.\n            If called with regex it will create matcher function."),
    // ------------------------------------------------------------------
    'for-each': doc(function (fn) {
      typecheck('for-each', fn, 'function');

      for (var _len20 = arguments.length, lists = new Array(_len20 > 1 ? _len20 - 1 : 0), _key20 = 1; _key20 < _len20; _key20++) {
        lists[_key20 - 1] = arguments[_key20];
      }

      lists.forEach(function (arg, i) {
        typecheck('for-each', arg, ['pair', 'nil'], i + 1);
      }); // we need to use call(this because babel transpile this code into:
      // var ret = map.apply(void 0, [fn].concat(lists));
      // it don't work with weakBind

      var ret = this.get('map').apply(void 0, [fn].concat(lists));

      if (isPromise(ret)) {
        return ret.then(function () {});
      }
    }, "(for-each fn . lists)\n\n            Higher order function that call function `fn` by for each\n            value of the argument. If you provide more then one list as argument\n            it will take each value from each list and call `fn` function\n            with that many argument as number of list arguments."),
    // ------------------------------------------------------------------
    map: doc(function map(fn) {
      var _this5 = this;

      for (var _len21 = arguments.length, lists = new Array(_len21 > 1 ? _len21 - 1 : 0), _key21 = 1; _key21 < _len21; _key21++) {
        lists[_key21 - 1] = arguments[_key21];
      }

      typecheck('map', fn, 'function');
      lists.forEach(function (arg, i) {
        typecheck('map', arg, ['pair', 'nil'], i + 1);
      });

      if (lists.some(function (x) {
        return isEmptyList(x);
      })) {
        return nil;
      }

      return unpromise(fn.call.apply(fn, [this].concat(_toConsumableArray(lists.map(function (l) {
        return l.car;
      })))), function (head) {
        return unpromise(map.call.apply(map, [_this5, fn].concat(_toConsumableArray(lists.map(function (l) {
          return l.cdr;
        })))), function (rest) {
          return new Pair(head, rest);
        });
      });
    }, "(map fn . lists)\n\n            Higher order function that call function `fn` by for each\n            value of the argument. If you provide more then one list as argument\n            it will take each value from each list and call `fn` function\n            with that many argument as number of list arguments. The return\n            values of the function call is acumulated in result list and\n            returned by the call to map."),
    // ------------------------------------------------------------------
    some: doc(function some(fn, list) {
      typecheck('some', fn, 'function');
      typecheck('some', list, ['pair', 'nil']);

      if (isNull(list)) {
        return false;
      } else {
        return unpromise(fn(list.car), function (value) {
          return value || some(fn, list.cdr);
        });
      }
    }, "(some fn list)\n\n            Higher order function that call argument on each element of the list.\n            It stops when function fn return true for a value if so it will\n            return true. If it don't find the value it will return false"),
    // ------------------------------------------------------------------
    fold: doc(fold('fold', function (fold, fn, init) {
      for (var _len22 = arguments.length, lists = new Array(_len22 > 3 ? _len22 - 3 : 0), _key22 = 3; _key22 < _len22; _key22++) {
        lists[_key22 - 3] = arguments[_key22];
      }

      typecheck('fold', fn, 'function');
      lists.forEach(function (arg, i) {
        typecheck('fold', arg, ['pair', 'nil'], i + 1);
      });

      if (lists.some(isEmptyList)) {
        return init;
      }

      var value = fold.call.apply(fold, [this, fn, init].concat(_toConsumableArray(lists.map(function (l) {
        return l.cdr;
      }))));
      return unpromise(value, function (value) {
        return fn.apply(void 0, _toConsumableArray(lists.map(function (l) {
          return l.car;
        })).concat([value]));
      });
    }), "(fold fn init . lists)\n\n             Function fold is reverse of the reduce. it call function `fn`\n             on each elements on the list and return single value.\n             e.g. it call (fn a1 b1 (fn a2 b2 (fn a3 b3 '())))\n             for: (fold fn '() alist blist"),
    // ------------------------------------------------------------------
    pluck: doc(function () {
      for (var _len23 = arguments.length, keys = new Array(_len23), _key23 = 0; _key23 < _len23; _key23++) {
        keys[_key23] = arguments[_key23];
      }

      return function (obj) {
        keys = keys.map(function (x) {
          return x instanceof _Symbol ? x.name : x;
        });

        if (keys.length === 0) {
          return nil;
        } else if (keys.length === 1) {
          var _keys2 = keys,
              _keys3 = _slicedToArray(_keys2, 1),
              key = _keys3[0];

          return obj[key];
        }

        var result = {};
        keys.forEach(function (key) {
          result[key] = obj[key];
        });
        return result;
      };
    }, "(pluck . string)\n\n            If called with single string it will return function that will return\n            key from object. If called with more then one argument function will\n            return new object by taking all properties from given object."),
    // ------------------------------------------------------------------
    reduce: doc(fold('reduce', function (reduce, fn, init) {
      var _this6 = this;

      for (var _len24 = arguments.length, lists = new Array(_len24 > 3 ? _len24 - 3 : 0), _key24 = 3; _key24 < _len24; _key24++) {
        lists[_key24 - 3] = arguments[_key24];
      }

      typecheck('reduce', fn, 'function');
      lists.forEach(function (arg, i) {
        typecheck('reduce', arg, ['pair', 'nil'], i + 1);
      });

      if (lists.some(isEmptyList)) {
        return init;
      }

      return unpromise(fn.apply(void 0, _toConsumableArray(lists.map(function (l) {
        return l.car;
      })).concat([init])), function (value) {
        return reduce.call.apply(reduce, [_this6, fn, value].concat(_toConsumableArray(lists.map(function (l) {
          return l.cdr;
        }))));
      });
    }), "(reduce fn init list . lists)\n\n             Higher order function take each element of the list and call\n             the function with result of previous call or init and next element\n             on the list until each element is processed and return single value\n             as result of last call to `fn` function.\n             e.g. it call (fn a3 b3 (fn a2 b2 (fn a1 b1 init)))\n             for (reduce fn init alist blist"),
    // ------------------------------------------------------------------
    filter: doc(function (arg, list) {
      typecheck('filter', arg, ['regex', 'function']);
      typecheck('filter', list, ['pair', 'nil']);
      var array = this.get('list->array')(list);
      var result = [];
      var fn = matcher('filter', arg);
      return function loop(i) {
        function next(value) {
          if (value && value !== nil) {
            result.push(item);
          }

          return loop(++i);
        }

        if (i === array.length) {
          return Pair.fromArray(result);
        }

        var item = array[i];
        return unpromise(fn(item, i), next);
      }(0);
    }, "(filter fn list)\n            (filter regex list)\n\n            Higher order function that call `fn` for each element of the list\n            and return list for only those elements for which funtion return\n            true value. If called with regex it will create matcher function."),
    // ------------------------------------------------------------------
    range: doc(function (n) {
      typecheck('range', n, 'number');

      if (n instanceof LNumber) {
        n = n.valueOf();
      }

      return Pair.fromArray(new Array(n).fill(0).map(function (_, i) {
        return LNumber(i);
      }));
    }, "(range n)\n\n            Function return list of n numbers from 0 to n - 1"),
    // ------------------------------------------------------------------
    compose: doc(compose, "(compose . fns)\n\n             Higher order function and create new function that apply all functions\n             From right to left and return it's value. Reverse of compose.\n             e.g.:\n             ((compose (curry + 2) (curry * 3)) 3)\n             11\n            "),
    pipe: doc(pipe, "(pipe . fns)\n\n             Higher order function and create new function that apply all functions\n             From left to right and return it's value. Reverse of compose.\n             e.g.:\n             ((pipe (curry + 2) (curry * 3)) 3)\n             15"),
    curry: doc(curry, "(curry fn . args)\n\n             Higher order function that create curried version of the function.\n             The result function will have parially applied arguments and it\n             will keep returning functions until all arguments are added\n\n             e.g.:\n             (define (add a b c d) (+ a b c d))\n             (define add1 (curry add 1))\n             (define add12 (add 2))\n             (print (add12 3 4))"),
    // ------------------------------------------------------------------
    odd: doc(singleMathOp(function (num) {
      return LNumber(num).isOdd();
    }), "(odd number)\n             Function check if number os odd."),
    // ------------------------------------------------------------------
    even: doc(singleMathOp(function (num) {
      return LNumber(num).isEvent();
    }), "(even number)\n\n             Function check if number is even."),
    // ------------------------------------------------------------------
    // math functions
    '*': doc(reduceMathOp(function (a, b) {
      return LNumber(a).mul(b);
    }), "(* . numbers)\n\n             Multiplicate all numbers passed as arguments. If single value is passed\n              it will return that value."),
    // ------------------------------------------------------------------
    '+': doc(reduceMathOp(function (a, b) {
      return LNumber(a).add(b);
    }), "(+ . numbers)\n\n             Sum all numbers passed as arguments. If single value is passed it will\n             return that value."),
    // ------------------------------------------------------------------
    '-': doc(function () {
      for (var _len25 = arguments.length, args = new Array(_len25), _key25 = 0; _key25 < _len25; _key25++) {
        args[_key25] = arguments[_key25];
      }

      if (args.length === 1) {
        return LNumber(args[0]).neg();
      }

      if (args.length) {
        return args.reduce(binaryMathOp(function (a, b) {
          return LNumber(a).sub(b);
        }));
      }
    }, "(- . numbers)\n            (- number)\n\n            Substract number passed as argument. If only one argument is passed\n            it will negate the value."),
    // ------------------------------------------------------------------
    '/': doc(reduceMathOp(function (a, b) {
      return LNumber(a).div(b);
    }), "(/ . numbers)\n\n             Divide number passed as arguments one by one. If single argument\n             is passed it will return that value."),
    // ------------------------------------------------------------------
    'abs': doc(singleMathOp(function (n) {
      return LNumber(n).abs();
    }), "(abs number)\n\n             Function create absolute value from number."),
    // ------------------------------------------------------------------
    'sqrt': doc(singleMathOp(function (n) {
      return Math.sqrt(n);
    }), "(sqrt number)\n\n             Function return square root of the number."),
    // ------------------------------------------------------------------
    '**': doc(binaryMathOp(function (a, b) {
      return LNumber(a).pow(b);
    }), "(** a b)\n\n            Function calculate number a to to the power of b. It can throw\n            exception when ** native operator is not supported."),
    // ------------------------------------------------------------------
    '1+': doc(singleMathOp(function (number) {
      return LNumber(number).add(1);
    }), "(1+ number)\n\n             Function add 1 to the number and return result."),
    // ------------------------------------------------------------------
    '1-': doc(singleMathOp(function (number) {
      return LNumber(number).sub(1);
    }), "(1- number)\n\n             Function substract 1 from the number and return result."),
    // ------------------------------------------------------------------
    '++': doc(new Macro('++', function (code) {
      typecheck('++', code.car, 'symbol');
      var car = this.get(code.car);
      var value = LNumber(car).add(1);
      this.set(code.car, value);
      return value;
    }), "(++ variable)\n\n             Macro that work only on variables and increment the value by one."),
    // ------------------------------------------------------------------
    '--': doc(new Macro('--', function (code) {
      typecheck('--', code.car, 'symbol');
      var car = this.get(code.car);
      var value = LNumber(car).sub(1);
      this.set(code.car, value);
      return value;
    }), "(-- variable)\n\n             Macro that decrement the value it work only on symbols"),
    // ------------------------------------------------------------------
    '%': doc(reduceMathOp(function (a, b) {
      return LNumber(a).mod(b);
    }), "(% . numbers)\n\n             Function use modulo operation on each of the numbers. It return\n             single number."),
    // ------------------------------------------------------------------
    // Booleans
    '==': doc(function (a, b) {
      return LNumber(a).cmp(b) === 0;
    }, "(== a b)\n\n            Function compare two numbers and check if they are equal."),
    // ------------------------------------------------------------------
    '>': doc(function (a, b) {
      return LNumber(a).cmp(b) === 1;
    }, "(> a b)\n\n            Function compare two numbers and check if first argument is greater\n            than the second one"),
    // ------------------------------------------------------------------
    '<': doc(function (a, b) {
      return LNumber(a).cmp(b) === -1;
    }, "(< a b)\n\n            Function compare two numbers and check if first argument is less\n            than the second one"),
    // ------------------------------------------------------------------
    '<=': doc(function (a, b) {
      return [0, -1].includes(LNumber(a).cmp(b));
    }, "(<= a b)\n\n            Function compare two numbers and check if first argument is less or\n            equal to the second one"),
    // ------------------------------------------------------------------
    '>=': doc(function (a, b) {
      return [0, 1].includes(LNumber(a).cmp(b));
    }, "(>= a b)\n\n            Function compare two numbers and check if first argument is less or\n            equal to the second one"),
    // ------------------------------------------------------------------
    'eq?': doc(equal, "(eq? a b)\n\n             Function compare two values if they are identical."),
    // ------------------------------------------------------------------
    or: doc(new Macro('or', function (code, _ref17) {
      var dynamic_scope = _ref17.dynamic_scope,
          error = _ref17.error;
      var args = this.get('list->array')(code);
      var self = this;

      if (dynamic_scope) {
        dynamic_scope = self;
      }

      var result;
      return function loop() {
        function next(value) {
          result = value;

          if (result) {
            return result;
          } else {
            return loop();
          }
        }

        var arg = args.shift();

        if (typeof arg === 'undefined') {
          if (result) {
            return result;
          } else {
            return false;
          }
        } else {
          var value = evaluate(arg, {
            env: self,
            dynamic_scope: dynamic_scope,
            error: error
          });
          return unpromise(value, next);
        }
      }();
    }), "(or . expressions)\n\n             Macro execute the values one by one and return the one that is truthy value.\n             If there are no expression that evaluate to true it return false."),
    // ------------------------------------------------------------------
    and: doc(new Macro('and', function (code) {
      var _ref18 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          dynamic_scope = _ref18.dynamic_scope,
          error = _ref18.error;

      var args = this.get('list->array')(code);
      var self = this;

      if (dynamic_scope) {
        dynamic_scope = self;
      }

      if (!args.length) {
        return true;
      }

      var result;
      return function loop() {
        function next(value) {
          result = value;

          if (!result) {
            return false;
          } else {
            return loop();
          }
        }

        var arg = args.shift();

        if (typeof arg === 'undefined') {
          if (result) {
            return result;
          } else {
            return false;
          }
        } else {
          var value = evaluate(arg, {
            env: self,
            dynamic_scope: dynamic_scope,
            error: error
          });
          return unpromise(value, next);
        }
      }();
    }), "(and . expressions)\n\n             Macro evalute each expression in sequence if any value return false it will\n             return false. If each value return true it will return the last value.\n             If it's called without arguments it will return true."),
    // bit operations
    '|': doc(function (a, b) {
      return LNumber(a).or(b);
    }, "(& a b)\n\n            Function calculate or bit operation."),
    '&': doc(function (a, b) {
      return LNumber(a).and(b);
    }, "(& a b)\n\n            Function calculate and bit operation."),
    '~': doc(function (a) {
      return LNumber(a).neg();
    }, "(~ number)\n\n            Function negate the value."),
    '>>': doc(function (a, b) {
      return LNumber(a).shr(b);
    }, "(>> a b)\n\n            Function right shit the value a by value b."),
    '<<': doc(function (a, b) {
      return LNumber(a).shl(b);
    }, "(<< a b)\n\n            Function left shit the value a by value b."),
    not: doc(function (value) {
      if (isEmptyList(value)) {
        return true;
      }

      return !value;
    }, "(not object)\n\n            Function return negation of the argument."),
    '->': doc(function (obj, name) {
      for (var _len26 = arguments.length, args = new Array(_len26 > 2 ? _len26 - 2 : 0), _key26 = 2; _key26 < _len26; _key26++) {
        args[_key26 - 2] = arguments[_key26];
      }

      return obj[name].apply(obj, args);
    }, "(-> obj name . args)\n\n            Function get function from object and call it with arguments.")
  }, undefined, 'global'); // -------------------------------------------------------------------------

  ['floor', 'round', 'ceil'].forEach(function (fn) {
    global_env.set(fn, doc(function (value) {
      typecheck(fn, value, 'number');

      if (value instanceof LNumber) {
        return value[fn]();
      }
    }, "(".concat(fn, " number)\n\n            Function calculate ").concat(fn, " of a number.")));
  }); // -------------------------------------------------------------------------
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
  } // -------------------------------------------------------------------------


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
  } // -------------------------------------------------------------------------
  // cadr caddr cadadr etc.


  combinations(['d', 'a'], 2, 5).forEach(function (spec) {
    var s = spec.split('');
    var chars = s.slice().reverse();
    var code = s.map(function (c) {
      return "(c".concat(c, "r");
    }).join(' ') + ' arg' + ')'.repeat(s.length);
    var name = 'c' + spec + 'r';
    global_env.set(name, doc(function (arg) {
      return chars.reduce(function (list, type) {
        if (list === nil) {
          return nil;
        }

        typecheck(name, list, 'pair');

        if (type === 'a') {
          return list.car;
        } else {
          return list.cdr;
        }
      }, arg);
    }, "(".concat(name, " arg)\n\n            Function calculate ").concat(code)));
  }); // -------------------------------------------------------------------------

  if (typeof global !== 'undefined') {
    global_env.set('global', global);
  } else if (typeof window !== 'undefined') {
    global_env.set('window', window);
  } // -------------------------------------------------------------------------


  function typeErrorMessage(fn, got, expected) {
    var position = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
    var postfix = fn ? " in function `".concat(fn, "`") : '';

    if (position !== null) {
      postfix += " argument ".concat(position);
    }

    if (expected instanceof Array) {
      var last = expected[expected.length - 1];
      expected = expected.slice(0, -1).join(', ') + ' or ' + last;
    }

    return "Expecting ".concat(expected, " got ").concat(got).concat(postfix);
  } // -------------------------------------------------------------------------


  function typecheck(fn, arg, expected) {
    var position = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
    var arg_type = type(arg);
    var match = false;

    if (expected instanceof Array && expected.includes(arg_type)) {
      match = true;
    }

    if (!match && arg_type !== expected) {
      throw new Error(typeErrorMessage(fn, arg_type, expected, position));
    }
  } // -------------------------------------------------------------------------


  function selfEvaluated(obj) {
    var type = _typeof(obj);

    return ['string', 'function'].includes(type) || obj instanceof LNumber || obj instanceof RegExp;
  } // -------------------------------------------------------------------------


  function type(obj) {
    var mapping = {
      'pair': Pair,
      'symbol': _Symbol,
      'macro': Macro,
      'array': Array,
      'native_symbol': root.Symbol
    };

    if (obj === nil) {
      return 'nil';
    }

    if (obj === null) {
      return 'null';
    }

    var _arr2 = Object.entries(mapping);

    for (var _i2 = 0; _i2 < _arr2.length; _i2++) {
      var _arr2$_i = _slicedToArray(_arr2[_i2], 2),
          key = _arr2$_i[0],
          value = _arr2$_i[1];

      if (obj instanceof value) {
        return key;
      }
    }

    if (obj instanceof LNumber) {
      return 'number';
    }

    if (obj instanceof RegExp) {
      return "regex";
    }

    if (_typeof(obj) === 'object') {
      return obj.constructor.name;
    }

    return _typeof(obj);
  } // -------------------------------------------------------------------------
  // :; wrap tree of Promises with single Promise or return argument as is
  // :: if tree have no Promises
  // -------------------------------------------------------------------------


  function resolvePromises(arg) {
    var promises = [];
    traverse(arg);

    if (promises.length) {
      return resolve(arg);
    }

    return arg;

    function traverse(node) {
      if (isPromise(node)) {
        promises.push(node);
      } else if (node instanceof Pair) {
        if (!node.haveCycles('car')) {
          traverse(node.car);
        }

        if (!node.haveCycles('cdr')) {
          traverse(node.cdr);
        }
      } else if (node instanceof Array) {
        node.forEach(traverse);
      }
    }

    function promise(_x4) {
      return _promise.apply(this, arguments);
    }

    function _promise() {
      _promise = _asyncToGenerator(
      /*#__PURE__*/
      regenerator.mark(function _callee3(node) {
        var pair;
        return regenerator.wrap(function _callee3$(_context3) {
          while (1) {
            switch (_context3.prev = _context3.next) {
              case 0:
                _context3.t0 = Pair;

                if (!node.haveCycles('car')) {
                  _context3.next = 5;
                  break;
                }

                _context3.t1 = node.car;
                _context3.next = 8;
                break;

              case 5:
                _context3.next = 7;
                return resolve(node.car);

              case 7:
                _context3.t1 = _context3.sent;

              case 8:
                _context3.t2 = _context3.t1;

                if (!node.haveCycles('cdr')) {
                  _context3.next = 13;
                  break;
                }

                _context3.t3 = node.cdr;
                _context3.next = 16;
                break;

              case 13:
                _context3.next = 15;
                return resolve(node.cdr);

              case 15:
                _context3.t3 = _context3.sent;

              case 16:
                _context3.t4 = _context3.t3;
                pair = new _context3.t0(_context3.t2, _context3.t4);

                if (node.data) {
                  pair.data = true;
                }

                return _context3.abrupt("return", pair);

              case 20:
              case "end":
                return _context3.stop();
            }
          }
        }, _callee3);
      }));
      return _promise.apply(this, arguments);
    }

    function resolve(node) {
      if (node instanceof Array) {
        return Promise.all(node.map(resolve));
      }

      if (node instanceof Pair && promises.length) {
        return promise(node);
      }

      return node;
    }
  }

  function getFunctionArgs(rest, _ref19) {
    var env = _ref19.env,
        dynamic_scope = _ref19.dynamic_scope,
        error = _ref19.error;
    var args = [];
    var node = rest;
    markCycles(node);

    while (true) {
      if (node instanceof Pair && !isEmptyList(node)) {
        var arg = evaluate(node.car, {
          env: env,
          dynamic_scope: dynamic_scope,
          error: error
        });

        if (dynamic_scope) {
          arg = unpromise(arg, function (arg) {
            if (typeof arg === 'function' && isNativeFunction(arg)) {
              return arg.bind(dynamic_scope);
            }

            return arg;
          });
        }

        args.push(arg);

        if (node.haveCycles('cdr')) {
          break;
        }

        node = node.cdr;
      } else {
        break;
      }
    }

    return resolvePromises(args);
  } // -------------------------------------------------------------------------


  function evaluateMacro(macro, code, eval_args) {

    var value = macro.invoke(code, eval_args);
    return unpromise(resolvePromises(value), function ret(value) {
      if (value && value.data || !value || selfEvaluated(value)) {
        return value;
      } else {
        return quote(evaluate(value, eval_args));
      }
    });
  } // -------------------------------------------------------------------------


  function evaluate(code) {
    var _ref20 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        env = _ref20.env,
        dynamic_scope = _ref20.dynamic_scope,
        _ref20$error = _ref20.error,
        error = _ref20$error === void 0 ? function () {} : _ref20$error;

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

      if (isNull(code)) {
        return code;
      }

      if (isEmptyList(code)) {
        return emptyList();
      }

      if (code instanceof _Symbol) {
        return env.get(code, true);
      }

      var first = code.car;
      var rest = code.cdr;

      if (first instanceof Pair) {
        value = resolvePromises(evaluate(first, eval_args));

        if (isPromise(value)) {
          return value.then(function (value) {
            return evaluate(new Pair(value, code.cdr), eval_args);
          }); // else is later in code
        } else if (typeof value !== 'function') {
          throw new Error(type(value) + ' ' + env.get('string')(value) + ' is not a function while evaluating ' + code.toString());
        }
      }

      if (first instanceof _Symbol) {
        value = env.get(first, true);

        if (value instanceof Macro) {
          var ret = evaluateMacro(value, rest, eval_args);
          return unpromise(ret, function (result) {
            if (result instanceof Pair) {
              return result.markCycles();
            }

            return result;
          });
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
        var args = getFunctionArgs(rest, eval_args);
        return unpromise(args, function (args) {
          var scope = dynamic_scope || env;
          var result = resolvePromises(value.apply(scope, args));
          return unpromise(result, function (result) {
            if (result instanceof Pair) {
              return quote(result.markCycles());
            }

            return result;
          }, error);
        });
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
      error && error(e, code);
    }
  } // -------------------------------------------------------------------------


  function exec(_x5, _x6, _x7) {
    return _exec.apply(this, arguments);
  } // -------------------------------------------------------------------------
  // create token matcher that work with string and object token
  // -------------------------------------------------------------------------


  function _exec() {
    _exec = _asyncToGenerator(
    /*#__PURE__*/
    regenerator.mark(function _callee4(string, env, dynamic_scope) {
      var list, results, code, result;
      return regenerator.wrap(function _callee4$(_context4) {
        while (1) {
          switch (_context4.prev = _context4.next) {
            case 0:
              if (dynamic_scope === true) {
                env = dynamic_scope = env || global_env;
              } else if (env === true) {
                env = dynamic_scope = global_env;
              } else {
                env = env || global_env;
              }

              list = parse(string);
              results = [];

            case 3:

              code = list.shift();

              if (code) {
                _context4.next = 9;
                break;
              }

              return _context4.abrupt("return", results);

            case 9:
              _context4.next = 11;
              return evaluate(code, {
                env: env,
                dynamic_scope: dynamic_scope,
                error: function error(e, code) {
                  if (code) {
                    // LIPS stack trace
                    e.code = e.code || [];
                    e.code.push(code.toString());
                  }

                  throw e;
                }
              });

            case 11:
              result = _context4.sent;
              results.push(result);

            case 13:
              _context4.next = 3;
              break;

            case 15:
            case "end":
              return _context4.stop();
          }
        }
      }, _callee4);
    }));
    return _exec.apply(this, arguments);
  }

  function matchToken(re) {
    return function (token) {
      if (!token) {
        return false;
      }

      return (token.token || token).match(re);
    };
  }

  var isParen = matchToken(/[()]/); // -------------------------------------------------------------------------

  function balanced(code) {
    var tokens = typeof code === 'string' ? tokenize(code) : code;
    var parenthesis = tokens.filter(isParen);
    var open = parenthesis.filter(function (p) {
      return (p.token || p) === ')';
    });
    var close = parenthesis.filter(function (p) {
      return (p.token || p) === '(';
    });
    return open.length === close.length;
  } // -------------------------------------------------------------------------


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
  }; // -------------------------------------------------------------------------


  function execError(e) {
    console.error(e.message || e);

    if (e.code) {
      console.error(e.code.map(function (line, i) {
        return "[".concat(i + 1, "]: ").concat(line);
      }));
    }
  } // -------------------------------------------------------------------------


  function init() {
    var lips_mime = 'text/x-lips';

    if (window.document) {
      var scripts = Array.from(document.querySelectorAll('script'));
      return function loop() {
        var script = scripts.shift();

        if (script) {
          var type = script.getAttribute('type');

          if (type === lips_mime) {
            var src = script.getAttribute('src');

            if (src) {
              return root.fetch(src).then(function (res) {
                return res.text();
              }).then(exec).then(loop).catch(function (e) {
                execError(e);
                loop();
              });
            } else {
              return exec(script.innerHTML).then(loop).catch(function (e) {
                execError(e);
                loop();
              });
            }
          } else if (type && type.match(/lips|lisp/)) {
            console.warn('Expecting ' + lips_mime + ' found ' + type);
          }

          return loop();
        }
      }();
    }
  } // -------------------------------------------------------------------------


  if (typeof window !== 'undefined') {
    contentLoaded(window, init);
  } // -------------------------------------------------------------------------
  // to be used with string function when code is minified
  // -------------------------------------------------------------------------


  Ahead.__className = 'Ahead';
  Pattern.__className = 'Pattern';
  Formatter.__className = 'Formatter';
  Macro.__className = 'Macro';
  Environment.__className = 'Environment'; // -------------------------------------------------------------------------

  var lips = {
    version: 'DEV',
    exec: exec,
    parse: parse,
    tokenize: tokenize,
    evaluate: evaluate,
    Environment: Environment,
    global_environment: global_env,
    globalEnvironment: global_env,
    env: global_env,
    balanced_parenthesis: balanced,
    balancedParenthesis: balanced,
    Macro: Macro,
    quote: quote,
    Pair: Pair,
    Formatter: Formatter,
    specials: specials,
    nil: nil,
    resolvePromises: resolvePromises,
    Symbol: _Symbol,
    LNumber: LNumber
  }; // so it work when used with webpack where it will be not global

  global_env.set('lips', lips);
  return lips;
});

}());
