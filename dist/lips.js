(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
  typeof define === 'function' && define.amd ? define(factory) :
  (global = typeof globalThis !== 'undefined' ? globalThis : global || self, global.lips = factory());
})(this, (function () { 'use strict';

  function _classApplyDescriptorGet(receiver, descriptor) {
    if (descriptor.get) {
      return descriptor.get.call(receiver);
    }

    return descriptor.value;
  }

  function _classExtractFieldDescriptor(receiver, privateMap, action) {
    if (!privateMap.has(receiver)) {
      throw new TypeError("attempted to " + action + " private field on non-instance");
    }

    return privateMap.get(receiver);
  }

  function _classPrivateFieldGet(receiver, privateMap) {
    var descriptor = _classExtractFieldDescriptor(receiver, privateMap, "get");
    return _classApplyDescriptorGet(receiver, descriptor);
  }

  function _classApplyDescriptorSet(receiver, descriptor, value) {
    if (descriptor.set) {
      descriptor.set.call(receiver, value);
    } else {
      if (!descriptor.writable) {
        throw new TypeError("attempted to set read only private field");
      }

      descriptor.value = value;
    }
  }

  function _classPrivateFieldSet(receiver, privateMap, value) {
    var descriptor = _classExtractFieldDescriptor(receiver, privateMap, "set");
    _classApplyDescriptorSet(receiver, descriptor, value);
    return value;
  }

  function _assertThisInitialized(self) {
    if (self === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }

    return self;
  }

  function _setPrototypeOf(o, p) {
    _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf(o, p) {
      o.__proto__ = p;
      return o;
    };

    return _setPrototypeOf(o, p);
  }

  function _isNativeReflectConstruct$1() {
    if (typeof Reflect === "undefined" || !Reflect.construct) return false;
    if (Reflect.construct.sham) return false;
    if (typeof Proxy === "function") return true;

    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {}));
      return true;
    } catch (e) {
      return false;
    }
  }

  function _construct(Parent, args, Class) {
    if (_isNativeReflectConstruct$1()) {
      _construct = Reflect.construct;
    } else {
      _construct = function _construct(Parent, args, Class) {
        var a = [null];
        a.push.apply(a, args);
        var Constructor = Function.bind.apply(Parent, a);
        var instance = new Constructor();
        if (Class) _setPrototypeOf(instance, Class.prototype);
        return instance;
      };
    }

    return _construct.apply(null, arguments);
  }

  function _inherits(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }

    subClass.prototype = Object.create(superClass && superClass.prototype, {
      constructor: {
        value: subClass,
        writable: true,
        configurable: true
      }
    });
    Object.defineProperty(subClass, "prototype", {
      writable: false
    });
    if (superClass) _setPrototypeOf(subClass, superClass);
  }

  function _typeof(obj) {
    "@babel/helpers - typeof";

    return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (obj) {
      return typeof obj;
    } : function (obj) {
      return obj && "function" == typeof Symbol && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj;
    }, _typeof(obj);
  }

  function _possibleConstructorReturn(self, call) {
    if (call && (_typeof(call) === "object" || typeof call === "function")) {
      return call;
    } else if (call !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }

    return _assertThisInitialized(self);
  }

  function _getPrototypeOf(o) {
    _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf(o) {
      return o.__proto__ || Object.getPrototypeOf(o);
    };
    return _getPrototypeOf(o);
  }

  function _arrayWithHoles(arr) {
    if (Array.isArray(arr)) return arr;
  }

  function _iterableToArray(iter) {
    if (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null || iter["@@iterator"] != null) return Array.from(iter);
  }

  function _arrayLikeToArray$1(arr, len) {
    if (len == null || len > arr.length) len = arr.length;

    for (var i = 0, arr2 = new Array(len); i < len; i++) {
      arr2[i] = arr[i];
    }

    return arr2;
  }

  function _unsupportedIterableToArray$1(o, minLen) {
    if (!o) return;
    if (typeof o === "string") return _arrayLikeToArray$1(o, minLen);
    var n = Object.prototype.toString.call(o).slice(8, -1);
    if (n === "Object" && o.constructor) n = o.constructor.name;
    if (n === "Map" || n === "Set") return Array.from(o);
    if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray$1(o, minLen);
  }

  function _nonIterableRest() {
    throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
  }

  function _toArray(arr) {
    return _arrayWithHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray$1(arr) || _nonIterableRest();
  }

  function _arrayWithoutHoles(arr) {
    if (Array.isArray(arr)) return _arrayLikeToArray$1(arr);
  }

  function _nonIterableSpread() {
    throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
  }

  function _toConsumableArray(arr) {
    return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray$1(arr) || _nonIterableSpread();
  }

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

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  function _defineProperties(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor) descriptor.writable = true;
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }

  function _createClass(Constructor, protoProps, staticProps) {
    if (protoProps) _defineProperties(Constructor.prototype, protoProps);
    if (staticProps) _defineProperties(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", {
      writable: false
    });
    return Constructor;
  }

  function _defineProperty(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, {
        value: value,
        enumerable: true,
        configurable: true,
        writable: true
      });
    } else {
      obj[key] = value;
    }

    return obj;
  }

  function _objectWithoutPropertiesLoose(source, excluded) {
    if (source == null) return {};
    var target = {};
    var sourceKeys = Object.keys(source);
    var key, i;

    for (i = 0; i < sourceKeys.length; i++) {
      key = sourceKeys[i];
      if (excluded.indexOf(key) >= 0) continue;
      target[key] = source[key];
    }

    return target;
  }

  function _objectWithoutProperties(source, excluded) {
    if (source == null) return {};
    var target = _objectWithoutPropertiesLoose(source, excluded);
    var key, i;

    if (Object.getOwnPropertySymbols) {
      var sourceSymbolKeys = Object.getOwnPropertySymbols(source);

      for (i = 0; i < sourceSymbolKeys.length; i++) {
        key = sourceSymbolKeys[i];
        if (excluded.indexOf(key) >= 0) continue;
        if (!Object.prototype.propertyIsEnumerable.call(source, key)) continue;
        target[key] = source[key];
      }
    }

    return target;
  }

  function _iterableToArrayLimit(arr, i) {
    var _i = arr == null ? null : typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"];

    if (_i == null) return;
    var _arr = [];
    var _n = true;
    var _d = false;

    var _s, _e;

    try {
      for (_i = _i.call(arr); !(_n = (_s = _i.next()).done); _n = true) {
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

  function _slicedToArray(arr, i) {
    return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray$1(arr, i) || _nonIterableRest();
  }

  function _AwaitValue(value) {
    this.wrapped = value;
  }

  function _awaitAsyncGenerator(value) {
    return new _AwaitValue(value);
  }

  function AsyncGenerator(gen) {
    var front, back;

    function send(key, arg) {
      return new Promise(function (resolve, reject) {
        var request = {
          key: key,
          arg: arg,
          resolve: resolve,
          reject: reject,
          next: null
        };

        if (back) {
          back = back.next = request;
        } else {
          front = back = request;
          resume(key, arg);
        }
      });
    }

    function resume(key, arg) {
      try {
        var result = gen[key](arg);
        var value = result.value;
        var wrappedAwait = value instanceof _AwaitValue;
        Promise.resolve(wrappedAwait ? value.wrapped : value).then(function (arg) {
          if (wrappedAwait) {
            resume(key === "return" ? "return" : "next", arg);
            return;
          }

          settle(result.done ? "return" : "normal", arg);
        }, function (err) {
          resume("throw", err);
        });
      } catch (err) {
        settle("throw", err);
      }
    }

    function settle(type, value) {
      switch (type) {
        case "return":
          front.resolve({
            value: value,
            done: true
          });
          break;

        case "throw":
          front.reject(value);
          break;

        default:
          front.resolve({
            value: value,
            done: false
          });
          break;
      }

      front = front.next;

      if (front) {
        resume(front.key, front.arg);
      } else {
        back = null;
      }
    }

    this._invoke = send;

    if (typeof gen["return"] !== "function") {
      this["return"] = undefined;
    }
  }

  AsyncGenerator.prototype[typeof Symbol === "function" && Symbol.asyncIterator || "@@asyncIterator"] = function () {
    return this;
  };

  AsyncGenerator.prototype.next = function (arg) {
    return this._invoke("next", arg);
  };

  AsyncGenerator.prototype["throw"] = function (arg) {
    return this._invoke("throw", arg);
  };

  AsyncGenerator.prototype["return"] = function (arg) {
    return this._invoke("return", arg);
  };

  function _wrapAsyncGenerator(fn) {
    return function () {
      return new AsyncGenerator(fn.apply(this, arguments));
    };
  }

  function getDefaultExportFromCjs (x) {
  	return x && x.__esModule && Object.prototype.hasOwnProperty.call(x, 'default') ? x['default'] : x;
  }

  var regenerator = {exports: {}};

  var runtime = {exports: {}};

  /**
   * Copyright (c) 2014-present, Facebook, Inc.
   *
   * This source code is licensed under the MIT license found in the
   * LICENSE file in the root directory of this source tree.
   */

  (function (module) {
  	var runtime = (function (exports) {

  	  var Op = Object.prototype;
  	  var hasOwn = Op.hasOwnProperty;
  	  var undefined$1; // More compressible than void 0.
  	  var $Symbol = typeof Symbol === "function" ? Symbol : {};
  	  var iteratorSymbol = $Symbol.iterator || "@@iterator";
  	  var asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator";
  	  var toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";

  	  function define(obj, key, value) {
  	    Object.defineProperty(obj, key, {
  	      value: value,
  	      enumerable: true,
  	      configurable: true,
  	      writable: true
  	    });
  	    return obj[key];
  	  }
  	  try {
  	    // IE 8 has a broken Object.defineProperty that only works on DOM objects.
  	    define({}, "");
  	  } catch (err) {
  	    define = function(obj, key, value) {
  	      return obj[key] = value;
  	    };
  	  }

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
  	  define(IteratorPrototype, iteratorSymbol, function () {
  	    return this;
  	  });

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
  	  GeneratorFunction.prototype = GeneratorFunctionPrototype;
  	  define(Gp, "constructor", GeneratorFunctionPrototype);
  	  define(GeneratorFunctionPrototype, "constructor", GeneratorFunction);
  	  GeneratorFunction.displayName = define(
  	    GeneratorFunctionPrototype,
  	    toStringTagSymbol,
  	    "GeneratorFunction"
  	  );

  	  // Helper for defining the .next, .throw, and .return methods of the
  	  // Iterator interface in terms of a single ._invoke method.
  	  function defineIteratorMethods(prototype) {
  	    ["next", "throw", "return"].forEach(function(method) {
  	      define(prototype, method, function(arg) {
  	        return this._invoke(method, arg);
  	      });
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
  	      define(genFun, toStringTagSymbol, "GeneratorFunction");
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

  	  function AsyncIterator(generator, PromiseImpl) {
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
  	          return PromiseImpl.resolve(value.__await).then(function(value) {
  	            invoke("next", value, resolve, reject);
  	          }, function(err) {
  	            invoke("throw", err, resolve, reject);
  	          });
  	        }

  	        return PromiseImpl.resolve(value).then(function(unwrapped) {
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
  	        return new PromiseImpl(function(resolve, reject) {
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
  	  define(AsyncIterator.prototype, asyncIteratorSymbol, function () {
  	    return this;
  	  });
  	  exports.AsyncIterator = AsyncIterator;

  	  // Note that simple async functions are implemented on top of
  	  // AsyncIterator objects; they just return a Promise for the value of
  	  // the final result produced by the iterator.
  	  exports.async = function(innerFn, outerFn, self, tryLocsList, PromiseImpl) {
  	    if (PromiseImpl === void 0) PromiseImpl = Promise;

  	    var iter = new AsyncIterator(
  	      wrap(innerFn, outerFn, self, tryLocsList),
  	      PromiseImpl
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
  	    if (method === undefined$1) {
  	      // A .throw or .return when the delegate iterator has no .throw
  	      // method always terminates the yield* loop.
  	      context.delegate = null;

  	      if (context.method === "throw") {
  	        // Note: ["return"] must be used for ES3 parsing compatibility.
  	        if (delegate.iterator["return"]) {
  	          // If the delegate iterator has a return method, give it a
  	          // chance to clean up.
  	          context.method = "return";
  	          context.arg = undefined$1;
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
  	        context.arg = undefined$1;
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

  	  define(Gp, toStringTagSymbol, "Generator");

  	  // A Generator should always return itself as the iterator object when the
  	  // @@iterator function is called on it. Some browsers' implementations of the
  	  // iterator prototype chain incorrectly implement this, causing the Generator
  	  // object to not be returned from this call. This ensures that doesn't happen.
  	  // See https://github.com/facebook/regenerator/issues/274 for more details.
  	  define(Gp, iteratorSymbol, function() {
  	    return this;
  	  });

  	  define(Gp, "toString", function() {
  	    return "[object Generator]";
  	  });

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

  	          next.value = undefined$1;
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
  	    return { value: undefined$1, done: true };
  	  }

  	  Context.prototype = {
  	    constructor: Context,

  	    reset: function(skipTempReset) {
  	      this.prev = 0;
  	      this.next = 0;
  	      // Resetting context._sent for legacy support of Babel's
  	      // function.sent implementation.
  	      this.sent = this._sent = undefined$1;
  	      this.done = false;
  	      this.delegate = null;

  	      this.method = "next";
  	      this.arg = undefined$1;

  	      this.tryEntries.forEach(resetTryEntry);

  	      if (!skipTempReset) {
  	        for (var name in this) {
  	          // Not sure about the optimal order of these conditions:
  	          if (name.charAt(0) === "t" &&
  	              hasOwn.call(this, name) &&
  	              !isNaN(+name.slice(1))) {
  	            this[name] = undefined$1;
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
  	          context.arg = undefined$1;
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
  	        this.arg = undefined$1;
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
  	  // in case runtime.js accidentally runs in strict mode, in modern engines
  	  // we can explicitly access globalThis. In older engines we can escape
  	  // strict mode using a global Function call. This could conceivably fail
  	  // if a Content Security Policy forbids using Function, but in that case
  	  // the proper solution is to fix the accidental strict mode problem. If
  	  // you've misconfigured your bundler to force strict mode and applied a
  	  // CSP to forbid Function, and you're not willing to fix either of those
  	  // problems, please detail your unique predicament in a GitHub issue.
  	  if (typeof globalThis === "object") {
  	    globalThis.regeneratorRuntime = runtime;
  	  } else {
  	    Function("r", "regeneratorRuntime = r")(runtime);
  	  }
  	}
  } (runtime));

  (function (module) {
  	module.exports = runtime.exports;
  } (regenerator));

  var _regeneratorRuntime = /*@__PURE__*/getDefaultExportFromCjs(regenerator.exports);

  var global$1 = (typeof global !== "undefined" ? global :
    typeof self !== "undefined" ? self :
    typeof window !== "undefined" ? window : {});

  var lookup = [];
  var revLookup = [];
  var Arr = typeof Uint8Array !== 'undefined' ? Uint8Array : Array;
  var inited = false;
  function init$1 () {
    inited = true;
    var code = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    for (var i = 0, len = code.length; i < len; ++i) {
      lookup[i] = code[i];
      revLookup[code.charCodeAt(i)] = i;
    }

    revLookup['-'.charCodeAt(0)] = 62;
    revLookup['_'.charCodeAt(0)] = 63;
  }

  function toByteArray (b64) {
    if (!inited) {
      init$1();
    }
    var i, j, l, tmp, placeHolders, arr;
    var len = b64.length;

    if (len % 4 > 0) {
      throw new Error('Invalid string. Length must be a multiple of 4')
    }

    // the number of equal signs (place holders)
    // if there are two placeholders, than the two characters before it
    // represent one byte
    // if there is only one, then the three characters before it represent 2 bytes
    // this is just a cheap hack to not do indexOf twice
    placeHolders = b64[len - 2] === '=' ? 2 : b64[len - 1] === '=' ? 1 : 0;

    // base64 is 4/3 + up to two characters of the original data
    arr = new Arr(len * 3 / 4 - placeHolders);

    // if there are placeholders, only get up to the last complete 4 chars
    l = placeHolders > 0 ? len - 4 : len;

    var L = 0;

    for (i = 0, j = 0; i < l; i += 4, j += 3) {
      tmp = (revLookup[b64.charCodeAt(i)] << 18) | (revLookup[b64.charCodeAt(i + 1)] << 12) | (revLookup[b64.charCodeAt(i + 2)] << 6) | revLookup[b64.charCodeAt(i + 3)];
      arr[L++] = (tmp >> 16) & 0xFF;
      arr[L++] = (tmp >> 8) & 0xFF;
      arr[L++] = tmp & 0xFF;
    }

    if (placeHolders === 2) {
      tmp = (revLookup[b64.charCodeAt(i)] << 2) | (revLookup[b64.charCodeAt(i + 1)] >> 4);
      arr[L++] = tmp & 0xFF;
    } else if (placeHolders === 1) {
      tmp = (revLookup[b64.charCodeAt(i)] << 10) | (revLookup[b64.charCodeAt(i + 1)] << 4) | (revLookup[b64.charCodeAt(i + 2)] >> 2);
      arr[L++] = (tmp >> 8) & 0xFF;
      arr[L++] = tmp & 0xFF;
    }

    return arr
  }

  function tripletToBase64 (num) {
    return lookup[num >> 18 & 0x3F] + lookup[num >> 12 & 0x3F] + lookup[num >> 6 & 0x3F] + lookup[num & 0x3F]
  }

  function encodeChunk (uint8, start, end) {
    var tmp;
    var output = [];
    for (var i = start; i < end; i += 3) {
      tmp = (uint8[i] << 16) + (uint8[i + 1] << 8) + (uint8[i + 2]);
      output.push(tripletToBase64(tmp));
    }
    return output.join('')
  }

  function fromByteArray (uint8) {
    if (!inited) {
      init$1();
    }
    var tmp;
    var len = uint8.length;
    var extraBytes = len % 3; // if we have 1 byte left, pad 2 bytes
    var output = '';
    var parts = [];
    var maxChunkLength = 16383; // must be multiple of 3

    // go through the array every three bytes, we'll deal with trailing stuff later
    for (var i = 0, len2 = len - extraBytes; i < len2; i += maxChunkLength) {
      parts.push(encodeChunk(uint8, i, (i + maxChunkLength) > len2 ? len2 : (i + maxChunkLength)));
    }

    // pad the end with zeros, but make sure to not forget the extra bytes
    if (extraBytes === 1) {
      tmp = uint8[len - 1];
      output += lookup[tmp >> 2];
      output += lookup[(tmp << 4) & 0x3F];
      output += '==';
    } else if (extraBytes === 2) {
      tmp = (uint8[len - 2] << 8) + (uint8[len - 1]);
      output += lookup[tmp >> 10];
      output += lookup[(tmp >> 4) & 0x3F];
      output += lookup[(tmp << 2) & 0x3F];
      output += '=';
    }

    parts.push(output);

    return parts.join('')
  }

  function read$1 (buffer, offset, isLE, mLen, nBytes) {
    var e, m;
    var eLen = nBytes * 8 - mLen - 1;
    var eMax = (1 << eLen) - 1;
    var eBias = eMax >> 1;
    var nBits = -7;
    var i = isLE ? (nBytes - 1) : 0;
    var d = isLE ? -1 : 1;
    var s = buffer[offset + i];

    i += d;

    e = s & ((1 << (-nBits)) - 1);
    s >>= (-nBits);
    nBits += eLen;
    for (; nBits > 0; e = e * 256 + buffer[offset + i], i += d, nBits -= 8) {}

    m = e & ((1 << (-nBits)) - 1);
    e >>= (-nBits);
    nBits += mLen;
    for (; nBits > 0; m = m * 256 + buffer[offset + i], i += d, nBits -= 8) {}

    if (e === 0) {
      e = 1 - eBias;
    } else if (e === eMax) {
      return m ? NaN : ((s ? -1 : 1) * Infinity)
    } else {
      m = m + Math.pow(2, mLen);
      e = e - eBias;
    }
    return (s ? -1 : 1) * m * Math.pow(2, e - mLen)
  }

  function write (buffer, value, offset, isLE, mLen, nBytes) {
    var e, m, c;
    var eLen = nBytes * 8 - mLen - 1;
    var eMax = (1 << eLen) - 1;
    var eBias = eMax >> 1;
    var rt = (mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0);
    var i = isLE ? 0 : (nBytes - 1);
    var d = isLE ? 1 : -1;
    var s = value < 0 || (value === 0 && 1 / value < 0) ? 1 : 0;

    value = Math.abs(value);

    if (isNaN(value) || value === Infinity) {
      m = isNaN(value) ? 1 : 0;
      e = eMax;
    } else {
      e = Math.floor(Math.log(value) / Math.LN2);
      if (value * (c = Math.pow(2, -e)) < 1) {
        e--;
        c *= 2;
      }
      if (e + eBias >= 1) {
        value += rt / c;
      } else {
        value += rt * Math.pow(2, 1 - eBias);
      }
      if (value * c >= 2) {
        e++;
        c /= 2;
      }

      if (e + eBias >= eMax) {
        m = 0;
        e = eMax;
      } else if (e + eBias >= 1) {
        m = (value * c - 1) * Math.pow(2, mLen);
        e = e + eBias;
      } else {
        m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen);
        e = 0;
      }
    }

    for (; mLen >= 8; buffer[offset + i] = m & 0xff, i += d, m /= 256, mLen -= 8) {}

    e = (e << mLen) | m;
    eLen += mLen;
    for (; eLen > 0; buffer[offset + i] = e & 0xff, i += d, e /= 256, eLen -= 8) {}

    buffer[offset + i - d] |= s * 128;
  }

  var toString$1 = {}.toString;

  var isArray = Array.isArray || function (arr) {
    return toString$1.call(arr) == '[object Array]';
  };

  /*!
   * The buffer module from node.js, for the browser.
   *
   * @author   Feross Aboukhadijeh <feross@feross.org> <http://feross.org>
   * @license  MIT
   */

  var INSPECT_MAX_BYTES = 50;

  /**
   * If `Buffer.TYPED_ARRAY_SUPPORT`:
   *   === true    Use Uint8Array implementation (fastest)
   *   === false   Use Object implementation (most compatible, even IE6)
   *
   * Browsers that support typed arrays are IE 10+, Firefox 4+, Chrome 7+, Safari 5.1+,
   * Opera 11.6+, iOS 4.2+.
   *
   * Due to various browser bugs, sometimes the Object implementation will be used even
   * when the browser supports typed arrays.
   *
   * Note:
   *
   *   - Firefox 4-29 lacks support for adding new properties to `Uint8Array` instances,
   *     See: https://bugzilla.mozilla.org/show_bug.cgi?id=695438.
   *
   *   - Chrome 9-10 is missing the `TypedArray.prototype.subarray` function.
   *
   *   - IE10 has a broken `TypedArray.prototype.subarray` function which returns arrays of
   *     incorrect length in some situations.

   * We detect these buggy browsers and set `Buffer.TYPED_ARRAY_SUPPORT` to `false` so they
   * get the Object implementation, which is slower but behaves correctly.
   */
  Buffer$1.TYPED_ARRAY_SUPPORT = global$1.TYPED_ARRAY_SUPPORT !== undefined
    ? global$1.TYPED_ARRAY_SUPPORT
    : true;

  /*
   * Export kMaxLength after typed array support is determined.
   */
  kMaxLength();

  function kMaxLength () {
    return Buffer$1.TYPED_ARRAY_SUPPORT
      ? 0x7fffffff
      : 0x3fffffff
  }

  function createBuffer (that, length) {
    if (kMaxLength() < length) {
      throw new RangeError('Invalid typed array length')
    }
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      // Return an augmented `Uint8Array` instance, for best performance
      that = new Uint8Array(length);
      that.__proto__ = Buffer$1.prototype;
    } else {
      // Fallback: Return an object instance of the Buffer class
      if (that === null) {
        that = new Buffer$1(length);
      }
      that.length = length;
    }

    return that
  }

  /**
   * The Buffer constructor returns instances of `Uint8Array` that have their
   * prototype changed to `Buffer.prototype`. Furthermore, `Buffer` is a subclass of
   * `Uint8Array`, so the returned instances will have all the node `Buffer` methods
   * and the `Uint8Array` methods. Square bracket notation works as expected -- it
   * returns a single octet.
   *
   * The `Uint8Array` prototype remains unmodified.
   */

  function Buffer$1 (arg, encodingOrOffset, length) {
    if (!Buffer$1.TYPED_ARRAY_SUPPORT && !(this instanceof Buffer$1)) {
      return new Buffer$1(arg, encodingOrOffset, length)
    }

    // Common case.
    if (typeof arg === 'number') {
      if (typeof encodingOrOffset === 'string') {
        throw new Error(
          'If encoding is specified then the first argument must be a string'
        )
      }
      return allocUnsafe(this, arg)
    }
    return from(this, arg, encodingOrOffset, length)
  }

  Buffer$1.poolSize = 8192; // not used by this implementation

  // TODO: Legacy, not needed anymore. Remove in next major version.
  Buffer$1._augment = function (arr) {
    arr.__proto__ = Buffer$1.prototype;
    return arr
  };

  function from (that, value, encodingOrOffset, length) {
    if (typeof value === 'number') {
      throw new TypeError('"value" argument must not be a number')
    }

    if (typeof ArrayBuffer !== 'undefined' && value instanceof ArrayBuffer) {
      return fromArrayBuffer(that, value, encodingOrOffset, length)
    }

    if (typeof value === 'string') {
      return fromString(that, value, encodingOrOffset)
    }

    return fromObject(that, value)
  }

  /**
   * Functionally equivalent to Buffer(arg, encoding) but throws a TypeError
   * if value is a number.
   * Buffer.from(str[, encoding])
   * Buffer.from(array)
   * Buffer.from(buffer)
   * Buffer.from(arrayBuffer[, byteOffset[, length]])
   **/
  Buffer$1.from = function (value, encodingOrOffset, length) {
    return from(null, value, encodingOrOffset, length)
  };

  if (Buffer$1.TYPED_ARRAY_SUPPORT) {
    Buffer$1.prototype.__proto__ = Uint8Array.prototype;
    Buffer$1.__proto__ = Uint8Array;
  }

  function assertSize (size) {
    if (typeof size !== 'number') {
      throw new TypeError('"size" argument must be a number')
    } else if (size < 0) {
      throw new RangeError('"size" argument must not be negative')
    }
  }

  function alloc (that, size, fill, encoding) {
    assertSize(size);
    if (size <= 0) {
      return createBuffer(that, size)
    }
    if (fill !== undefined) {
      // Only pay attention to encoding if it's a string. This
      // prevents accidentally sending in a number that would
      // be interpretted as a start offset.
      return typeof encoding === 'string'
        ? createBuffer(that, size).fill(fill, encoding)
        : createBuffer(that, size).fill(fill)
    }
    return createBuffer(that, size)
  }

  /**
   * Creates a new filled Buffer instance.
   * alloc(size[, fill[, encoding]])
   **/
  Buffer$1.alloc = function (size, fill, encoding) {
    return alloc(null, size, fill, encoding)
  };

  function allocUnsafe (that, size) {
    assertSize(size);
    that = createBuffer(that, size < 0 ? 0 : checked(size) | 0);
    if (!Buffer$1.TYPED_ARRAY_SUPPORT) {
      for (var i = 0; i < size; ++i) {
        that[i] = 0;
      }
    }
    return that
  }

  /**
   * Equivalent to Buffer(num), by default creates a non-zero-filled Buffer instance.
   * */
  Buffer$1.allocUnsafe = function (size) {
    return allocUnsafe(null, size)
  };
  /**
   * Equivalent to SlowBuffer(num), by default creates a non-zero-filled Buffer instance.
   */
  Buffer$1.allocUnsafeSlow = function (size) {
    return allocUnsafe(null, size)
  };

  function fromString (that, string, encoding) {
    if (typeof encoding !== 'string' || encoding === '') {
      encoding = 'utf8';
    }

    if (!Buffer$1.isEncoding(encoding)) {
      throw new TypeError('"encoding" must be a valid string encoding')
    }

    var length = byteLength(string, encoding) | 0;
    that = createBuffer(that, length);

    var actual = that.write(string, encoding);

    if (actual !== length) {
      // Writing a hex string, for example, that contains invalid characters will
      // cause everything after the first invalid character to be ignored. (e.g.
      // 'abxxcd' will be treated as 'ab')
      that = that.slice(0, actual);
    }

    return that
  }

  function fromArrayLike (that, array) {
    var length = array.length < 0 ? 0 : checked(array.length) | 0;
    that = createBuffer(that, length);
    for (var i = 0; i < length; i += 1) {
      that[i] = array[i] & 255;
    }
    return that
  }

  function fromArrayBuffer (that, array, byteOffset, length) {
    array.byteLength; // this throws if `array` is not a valid ArrayBuffer

    if (byteOffset < 0 || array.byteLength < byteOffset) {
      throw new RangeError('\'offset\' is out of bounds')
    }

    if (array.byteLength < byteOffset + (length || 0)) {
      throw new RangeError('\'length\' is out of bounds')
    }

    if (byteOffset === undefined && length === undefined) {
      array = new Uint8Array(array);
    } else if (length === undefined) {
      array = new Uint8Array(array, byteOffset);
    } else {
      array = new Uint8Array(array, byteOffset, length);
    }

    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      // Return an augmented `Uint8Array` instance, for best performance
      that = array;
      that.__proto__ = Buffer$1.prototype;
    } else {
      // Fallback: Return an object instance of the Buffer class
      that = fromArrayLike(that, array);
    }
    return that
  }

  function fromObject (that, obj) {
    if (internalIsBuffer(obj)) {
      var len = checked(obj.length) | 0;
      that = createBuffer(that, len);

      if (that.length === 0) {
        return that
      }

      obj.copy(that, 0, 0, len);
      return that
    }

    if (obj) {
      if ((typeof ArrayBuffer !== 'undefined' &&
          obj.buffer instanceof ArrayBuffer) || 'length' in obj) {
        if (typeof obj.length !== 'number' || isnan(obj.length)) {
          return createBuffer(that, 0)
        }
        return fromArrayLike(that, obj)
      }

      if (obj.type === 'Buffer' && isArray(obj.data)) {
        return fromArrayLike(that, obj.data)
      }
    }

    throw new TypeError('First argument must be a string, Buffer, ArrayBuffer, Array, or array-like object.')
  }

  function checked (length) {
    // Note: cannot use `length < kMaxLength()` here because that fails when
    // length is NaN (which is otherwise coerced to zero.)
    if (length >= kMaxLength()) {
      throw new RangeError('Attempt to allocate Buffer larger than maximum ' +
                           'size: 0x' + kMaxLength().toString(16) + ' bytes')
    }
    return length | 0
  }
  Buffer$1.isBuffer = isBuffer;
  function internalIsBuffer (b) {
    return !!(b != null && b._isBuffer)
  }

  Buffer$1.compare = function compare (a, b) {
    if (!internalIsBuffer(a) || !internalIsBuffer(b)) {
      throw new TypeError('Arguments must be Buffers')
    }

    if (a === b) return 0

    var x = a.length;
    var y = b.length;

    for (var i = 0, len = Math.min(x, y); i < len; ++i) {
      if (a[i] !== b[i]) {
        x = a[i];
        y = b[i];
        break
      }
    }

    if (x < y) return -1
    if (y < x) return 1
    return 0
  };

  Buffer$1.isEncoding = function isEncoding (encoding) {
    switch (String(encoding).toLowerCase()) {
      case 'hex':
      case 'utf8':
      case 'utf-8':
      case 'ascii':
      case 'latin1':
      case 'binary':
      case 'base64':
      case 'ucs2':
      case 'ucs-2':
      case 'utf16le':
      case 'utf-16le':
        return true
      default:
        return false
    }
  };

  Buffer$1.concat = function concat (list, length) {
    if (!isArray(list)) {
      throw new TypeError('"list" argument must be an Array of Buffers')
    }

    if (list.length === 0) {
      return Buffer$1.alloc(0)
    }

    var i;
    if (length === undefined) {
      length = 0;
      for (i = 0; i < list.length; ++i) {
        length += list[i].length;
      }
    }

    var buffer = Buffer$1.allocUnsafe(length);
    var pos = 0;
    for (i = 0; i < list.length; ++i) {
      var buf = list[i];
      if (!internalIsBuffer(buf)) {
        throw new TypeError('"list" argument must be an Array of Buffers')
      }
      buf.copy(buffer, pos);
      pos += buf.length;
    }
    return buffer
  };

  function byteLength (string, encoding) {
    if (internalIsBuffer(string)) {
      return string.length
    }
    if (typeof ArrayBuffer !== 'undefined' && typeof ArrayBuffer.isView === 'function' &&
        (ArrayBuffer.isView(string) || string instanceof ArrayBuffer)) {
      return string.byteLength
    }
    if (typeof string !== 'string') {
      string = '' + string;
    }

    var len = string.length;
    if (len === 0) return 0

    // Use a for loop to avoid recursion
    var loweredCase = false;
    for (;;) {
      switch (encoding) {
        case 'ascii':
        case 'latin1':
        case 'binary':
          return len
        case 'utf8':
        case 'utf-8':
        case undefined:
          return utf8ToBytes(string).length
        case 'ucs2':
        case 'ucs-2':
        case 'utf16le':
        case 'utf-16le':
          return len * 2
        case 'hex':
          return len >>> 1
        case 'base64':
          return base64ToBytes(string).length
        default:
          if (loweredCase) return utf8ToBytes(string).length // assume utf8
          encoding = ('' + encoding).toLowerCase();
          loweredCase = true;
      }
    }
  }
  Buffer$1.byteLength = byteLength;

  function slowToString (encoding, start, end) {
    var loweredCase = false;

    // No need to verify that "this.length <= MAX_UINT32" since it's a read-only
    // property of a typed array.

    // This behaves neither like String nor Uint8Array in that we set start/end
    // to their upper/lower bounds if the value passed is out of range.
    // undefined is handled specially as per ECMA-262 6th Edition,
    // Section 13.3.3.7 Runtime Semantics: KeyedBindingInitialization.
    if (start === undefined || start < 0) {
      start = 0;
    }
    // Return early if start > this.length. Done here to prevent potential uint32
    // coercion fail below.
    if (start > this.length) {
      return ''
    }

    if (end === undefined || end > this.length) {
      end = this.length;
    }

    if (end <= 0) {
      return ''
    }

    // Force coersion to uint32. This will also coerce falsey/NaN values to 0.
    end >>>= 0;
    start >>>= 0;

    if (end <= start) {
      return ''
    }

    if (!encoding) encoding = 'utf8';

    while (true) {
      switch (encoding) {
        case 'hex':
          return hexSlice(this, start, end)

        case 'utf8':
        case 'utf-8':
          return utf8Slice(this, start, end)

        case 'ascii':
          return asciiSlice(this, start, end)

        case 'latin1':
        case 'binary':
          return latin1Slice(this, start, end)

        case 'base64':
          return base64Slice(this, start, end)

        case 'ucs2':
        case 'ucs-2':
        case 'utf16le':
        case 'utf-16le':
          return utf16leSlice(this, start, end)

        default:
          if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
          encoding = (encoding + '').toLowerCase();
          loweredCase = true;
      }
    }
  }

  // The property is used by `Buffer.isBuffer` and `is-buffer` (in Safari 5-7) to detect
  // Buffer instances.
  Buffer$1.prototype._isBuffer = true;

  function swap (b, n, m) {
    var i = b[n];
    b[n] = b[m];
    b[m] = i;
  }

  Buffer$1.prototype.swap16 = function swap16 () {
    var len = this.length;
    if (len % 2 !== 0) {
      throw new RangeError('Buffer size must be a multiple of 16-bits')
    }
    for (var i = 0; i < len; i += 2) {
      swap(this, i, i + 1);
    }
    return this
  };

  Buffer$1.prototype.swap32 = function swap32 () {
    var len = this.length;
    if (len % 4 !== 0) {
      throw new RangeError('Buffer size must be a multiple of 32-bits')
    }
    for (var i = 0; i < len; i += 4) {
      swap(this, i, i + 3);
      swap(this, i + 1, i + 2);
    }
    return this
  };

  Buffer$1.prototype.swap64 = function swap64 () {
    var len = this.length;
    if (len % 8 !== 0) {
      throw new RangeError('Buffer size must be a multiple of 64-bits')
    }
    for (var i = 0; i < len; i += 8) {
      swap(this, i, i + 7);
      swap(this, i + 1, i + 6);
      swap(this, i + 2, i + 5);
      swap(this, i + 3, i + 4);
    }
    return this
  };

  Buffer$1.prototype.toString = function toString () {
    var length = this.length | 0;
    if (length === 0) return ''
    if (arguments.length === 0) return utf8Slice(this, 0, length)
    return slowToString.apply(this, arguments)
  };

  Buffer$1.prototype.equals = function equals (b) {
    if (!internalIsBuffer(b)) throw new TypeError('Argument must be a Buffer')
    if (this === b) return true
    return Buffer$1.compare(this, b) === 0
  };

  Buffer$1.prototype.inspect = function inspect () {
    var str = '';
    var max = INSPECT_MAX_BYTES;
    if (this.length > 0) {
      str = this.toString('hex', 0, max).match(/.{2}/g).join(' ');
      if (this.length > max) str += ' ... ';
    }
    return '<Buffer ' + str + '>'
  };

  Buffer$1.prototype.compare = function compare (target, start, end, thisStart, thisEnd) {
    if (!internalIsBuffer(target)) {
      throw new TypeError('Argument must be a Buffer')
    }

    if (start === undefined) {
      start = 0;
    }
    if (end === undefined) {
      end = target ? target.length : 0;
    }
    if (thisStart === undefined) {
      thisStart = 0;
    }
    if (thisEnd === undefined) {
      thisEnd = this.length;
    }

    if (start < 0 || end > target.length || thisStart < 0 || thisEnd > this.length) {
      throw new RangeError('out of range index')
    }

    if (thisStart >= thisEnd && start >= end) {
      return 0
    }
    if (thisStart >= thisEnd) {
      return -1
    }
    if (start >= end) {
      return 1
    }

    start >>>= 0;
    end >>>= 0;
    thisStart >>>= 0;
    thisEnd >>>= 0;

    if (this === target) return 0

    var x = thisEnd - thisStart;
    var y = end - start;
    var len = Math.min(x, y);

    var thisCopy = this.slice(thisStart, thisEnd);
    var targetCopy = target.slice(start, end);

    for (var i = 0; i < len; ++i) {
      if (thisCopy[i] !== targetCopy[i]) {
        x = thisCopy[i];
        y = targetCopy[i];
        break
      }
    }

    if (x < y) return -1
    if (y < x) return 1
    return 0
  };

  // Finds either the first index of `val` in `buffer` at offset >= `byteOffset`,
  // OR the last index of `val` in `buffer` at offset <= `byteOffset`.
  //
  // Arguments:
  // - buffer - a Buffer to search
  // - val - a string, Buffer, or number
  // - byteOffset - an index into `buffer`; will be clamped to an int32
  // - encoding - an optional encoding, relevant is val is a string
  // - dir - true for indexOf, false for lastIndexOf
  function bidirectionalIndexOf (buffer, val, byteOffset, encoding, dir) {
    // Empty buffer means no match
    if (buffer.length === 0) return -1

    // Normalize byteOffset
    if (typeof byteOffset === 'string') {
      encoding = byteOffset;
      byteOffset = 0;
    } else if (byteOffset > 0x7fffffff) {
      byteOffset = 0x7fffffff;
    } else if (byteOffset < -0x80000000) {
      byteOffset = -0x80000000;
    }
    byteOffset = +byteOffset;  // Coerce to Number.
    if (isNaN(byteOffset)) {
      // byteOffset: it it's undefined, null, NaN, "foo", etc, search whole buffer
      byteOffset = dir ? 0 : (buffer.length - 1);
    }

    // Normalize byteOffset: negative offsets start from the end of the buffer
    if (byteOffset < 0) byteOffset = buffer.length + byteOffset;
    if (byteOffset >= buffer.length) {
      if (dir) return -1
      else byteOffset = buffer.length - 1;
    } else if (byteOffset < 0) {
      if (dir) byteOffset = 0;
      else return -1
    }

    // Normalize val
    if (typeof val === 'string') {
      val = Buffer$1.from(val, encoding);
    }

    // Finally, search either indexOf (if dir is true) or lastIndexOf
    if (internalIsBuffer(val)) {
      // Special case: looking for empty string/buffer always fails
      if (val.length === 0) {
        return -1
      }
      return arrayIndexOf(buffer, val, byteOffset, encoding, dir)
    } else if (typeof val === 'number') {
      val = val & 0xFF; // Search for a byte value [0-255]
      if (Buffer$1.TYPED_ARRAY_SUPPORT &&
          typeof Uint8Array.prototype.indexOf === 'function') {
        if (dir) {
          return Uint8Array.prototype.indexOf.call(buffer, val, byteOffset)
        } else {
          return Uint8Array.prototype.lastIndexOf.call(buffer, val, byteOffset)
        }
      }
      return arrayIndexOf(buffer, [ val ], byteOffset, encoding, dir)
    }

    throw new TypeError('val must be string, number or Buffer')
  }

  function arrayIndexOf (arr, val, byteOffset, encoding, dir) {
    var indexSize = 1;
    var arrLength = arr.length;
    var valLength = val.length;

    if (encoding !== undefined) {
      encoding = String(encoding).toLowerCase();
      if (encoding === 'ucs2' || encoding === 'ucs-2' ||
          encoding === 'utf16le' || encoding === 'utf-16le') {
        if (arr.length < 2 || val.length < 2) {
          return -1
        }
        indexSize = 2;
        arrLength /= 2;
        valLength /= 2;
        byteOffset /= 2;
      }
    }

    function read (buf, i) {
      if (indexSize === 1) {
        return buf[i]
      } else {
        return buf.readUInt16BE(i * indexSize)
      }
    }

    var i;
    if (dir) {
      var foundIndex = -1;
      for (i = byteOffset; i < arrLength; i++) {
        if (read(arr, i) === read(val, foundIndex === -1 ? 0 : i - foundIndex)) {
          if (foundIndex === -1) foundIndex = i;
          if (i - foundIndex + 1 === valLength) return foundIndex * indexSize
        } else {
          if (foundIndex !== -1) i -= i - foundIndex;
          foundIndex = -1;
        }
      }
    } else {
      if (byteOffset + valLength > arrLength) byteOffset = arrLength - valLength;
      for (i = byteOffset; i >= 0; i--) {
        var found = true;
        for (var j = 0; j < valLength; j++) {
          if (read(arr, i + j) !== read(val, j)) {
            found = false;
            break
          }
        }
        if (found) return i
      }
    }

    return -1
  }

  Buffer$1.prototype.includes = function includes (val, byteOffset, encoding) {
    return this.indexOf(val, byteOffset, encoding) !== -1
  };

  Buffer$1.prototype.indexOf = function indexOf (val, byteOffset, encoding) {
    return bidirectionalIndexOf(this, val, byteOffset, encoding, true)
  };

  Buffer$1.prototype.lastIndexOf = function lastIndexOf (val, byteOffset, encoding) {
    return bidirectionalIndexOf(this, val, byteOffset, encoding, false)
  };

  function hexWrite (buf, string, offset, length) {
    offset = Number(offset) || 0;
    var remaining = buf.length - offset;
    if (!length) {
      length = remaining;
    } else {
      length = Number(length);
      if (length > remaining) {
        length = remaining;
      }
    }

    // must be an even number of digits
    var strLen = string.length;
    if (strLen % 2 !== 0) throw new TypeError('Invalid hex string')

    if (length > strLen / 2) {
      length = strLen / 2;
    }
    for (var i = 0; i < length; ++i) {
      var parsed = parseInt(string.substr(i * 2, 2), 16);
      if (isNaN(parsed)) return i
      buf[offset + i] = parsed;
    }
    return i
  }

  function utf8Write (buf, string, offset, length) {
    return blitBuffer(utf8ToBytes(string, buf.length - offset), buf, offset, length)
  }

  function asciiWrite (buf, string, offset, length) {
    return blitBuffer(asciiToBytes(string), buf, offset, length)
  }

  function latin1Write (buf, string, offset, length) {
    return asciiWrite(buf, string, offset, length)
  }

  function base64Write (buf, string, offset, length) {
    return blitBuffer(base64ToBytes(string), buf, offset, length)
  }

  function ucs2Write (buf, string, offset, length) {
    return blitBuffer(utf16leToBytes(string, buf.length - offset), buf, offset, length)
  }

  Buffer$1.prototype.write = function write (string, offset, length, encoding) {
    // Buffer#write(string)
    if (offset === undefined) {
      encoding = 'utf8';
      length = this.length;
      offset = 0;
    // Buffer#write(string, encoding)
    } else if (length === undefined && typeof offset === 'string') {
      encoding = offset;
      length = this.length;
      offset = 0;
    // Buffer#write(string, offset[, length][, encoding])
    } else if (isFinite(offset)) {
      offset = offset | 0;
      if (isFinite(length)) {
        length = length | 0;
        if (encoding === undefined) encoding = 'utf8';
      } else {
        encoding = length;
        length = undefined;
      }
    // legacy write(string, encoding, offset, length) - remove in v0.13
    } else {
      throw new Error(
        'Buffer.write(string, encoding, offset[, length]) is no longer supported'
      )
    }

    var remaining = this.length - offset;
    if (length === undefined || length > remaining) length = remaining;

    if ((string.length > 0 && (length < 0 || offset < 0)) || offset > this.length) {
      throw new RangeError('Attempt to write outside buffer bounds')
    }

    if (!encoding) encoding = 'utf8';

    var loweredCase = false;
    for (;;) {
      switch (encoding) {
        case 'hex':
          return hexWrite(this, string, offset, length)

        case 'utf8':
        case 'utf-8':
          return utf8Write(this, string, offset, length)

        case 'ascii':
          return asciiWrite(this, string, offset, length)

        case 'latin1':
        case 'binary':
          return latin1Write(this, string, offset, length)

        case 'base64':
          // Warning: maxLength not taken into account in base64Write
          return base64Write(this, string, offset, length)

        case 'ucs2':
        case 'ucs-2':
        case 'utf16le':
        case 'utf-16le':
          return ucs2Write(this, string, offset, length)

        default:
          if (loweredCase) throw new TypeError('Unknown encoding: ' + encoding)
          encoding = ('' + encoding).toLowerCase();
          loweredCase = true;
      }
    }
  };

  Buffer$1.prototype.toJSON = function toJSON () {
    return {
      type: 'Buffer',
      data: Array.prototype.slice.call(this._arr || this, 0)
    }
  };

  function base64Slice (buf, start, end) {
    if (start === 0 && end === buf.length) {
      return fromByteArray(buf)
    } else {
      return fromByteArray(buf.slice(start, end))
    }
  }

  function utf8Slice (buf, start, end) {
    end = Math.min(buf.length, end);
    var res = [];

    var i = start;
    while (i < end) {
      var firstByte = buf[i];
      var codePoint = null;
      var bytesPerSequence = (firstByte > 0xEF) ? 4
        : (firstByte > 0xDF) ? 3
        : (firstByte > 0xBF) ? 2
        : 1;

      if (i + bytesPerSequence <= end) {
        var secondByte, thirdByte, fourthByte, tempCodePoint;

        switch (bytesPerSequence) {
          case 1:
            if (firstByte < 0x80) {
              codePoint = firstByte;
            }
            break
          case 2:
            secondByte = buf[i + 1];
            if ((secondByte & 0xC0) === 0x80) {
              tempCodePoint = (firstByte & 0x1F) << 0x6 | (secondByte & 0x3F);
              if (tempCodePoint > 0x7F) {
                codePoint = tempCodePoint;
              }
            }
            break
          case 3:
            secondByte = buf[i + 1];
            thirdByte = buf[i + 2];
            if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80) {
              tempCodePoint = (firstByte & 0xF) << 0xC | (secondByte & 0x3F) << 0x6 | (thirdByte & 0x3F);
              if (tempCodePoint > 0x7FF && (tempCodePoint < 0xD800 || tempCodePoint > 0xDFFF)) {
                codePoint = tempCodePoint;
              }
            }
            break
          case 4:
            secondByte = buf[i + 1];
            thirdByte = buf[i + 2];
            fourthByte = buf[i + 3];
            if ((secondByte & 0xC0) === 0x80 && (thirdByte & 0xC0) === 0x80 && (fourthByte & 0xC0) === 0x80) {
              tempCodePoint = (firstByte & 0xF) << 0x12 | (secondByte & 0x3F) << 0xC | (thirdByte & 0x3F) << 0x6 | (fourthByte & 0x3F);
              if (tempCodePoint > 0xFFFF && tempCodePoint < 0x110000) {
                codePoint = tempCodePoint;
              }
            }
        }
      }

      if (codePoint === null) {
        // we did not generate a valid codePoint so insert a
        // replacement char (U+FFFD) and advance only 1 byte
        codePoint = 0xFFFD;
        bytesPerSequence = 1;
      } else if (codePoint > 0xFFFF) {
        // encode to utf16 (surrogate pair dance)
        codePoint -= 0x10000;
        res.push(codePoint >>> 10 & 0x3FF | 0xD800);
        codePoint = 0xDC00 | codePoint & 0x3FF;
      }

      res.push(codePoint);
      i += bytesPerSequence;
    }

    return decodeCodePointsArray(res)
  }

  // Based on http://stackoverflow.com/a/22747272/680742, the browser with
  // the lowest limit is Chrome, with 0x10000 args.
  // We go 1 magnitude less, for safety
  var MAX_ARGUMENTS_LENGTH = 0x1000;

  function decodeCodePointsArray (codePoints) {
    var len = codePoints.length;
    if (len <= MAX_ARGUMENTS_LENGTH) {
      return String.fromCharCode.apply(String, codePoints) // avoid extra slice()
    }

    // Decode in chunks to avoid "call stack size exceeded".
    var res = '';
    var i = 0;
    while (i < len) {
      res += String.fromCharCode.apply(
        String,
        codePoints.slice(i, i += MAX_ARGUMENTS_LENGTH)
      );
    }
    return res
  }

  function asciiSlice (buf, start, end) {
    var ret = '';
    end = Math.min(buf.length, end);

    for (var i = start; i < end; ++i) {
      ret += String.fromCharCode(buf[i] & 0x7F);
    }
    return ret
  }

  function latin1Slice (buf, start, end) {
    var ret = '';
    end = Math.min(buf.length, end);

    for (var i = start; i < end; ++i) {
      ret += String.fromCharCode(buf[i]);
    }
    return ret
  }

  function hexSlice (buf, start, end) {
    var len = buf.length;

    if (!start || start < 0) start = 0;
    if (!end || end < 0 || end > len) end = len;

    var out = '';
    for (var i = start; i < end; ++i) {
      out += toHex(buf[i]);
    }
    return out
  }

  function utf16leSlice (buf, start, end) {
    var bytes = buf.slice(start, end);
    var res = '';
    for (var i = 0; i < bytes.length; i += 2) {
      res += String.fromCharCode(bytes[i] + bytes[i + 1] * 256);
    }
    return res
  }

  Buffer$1.prototype.slice = function slice (start, end) {
    var len = this.length;
    start = ~~start;
    end = end === undefined ? len : ~~end;

    if (start < 0) {
      start += len;
      if (start < 0) start = 0;
    } else if (start > len) {
      start = len;
    }

    if (end < 0) {
      end += len;
      if (end < 0) end = 0;
    } else if (end > len) {
      end = len;
    }

    if (end < start) end = start;

    var newBuf;
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      newBuf = this.subarray(start, end);
      newBuf.__proto__ = Buffer$1.prototype;
    } else {
      var sliceLen = end - start;
      newBuf = new Buffer$1(sliceLen, undefined);
      for (var i = 0; i < sliceLen; ++i) {
        newBuf[i] = this[i + start];
      }
    }

    return newBuf
  };

  /*
   * Need to make sure that buffer isn't trying to write out of bounds.
   */
  function checkOffset (offset, ext, length) {
    if ((offset % 1) !== 0 || offset < 0) throw new RangeError('offset is not uint')
    if (offset + ext > length) throw new RangeError('Trying to access beyond buffer length')
  }

  Buffer$1.prototype.readUIntLE = function readUIntLE (offset, byteLength, noAssert) {
    offset = offset | 0;
    byteLength = byteLength | 0;
    if (!noAssert) checkOffset(offset, byteLength, this.length);

    var val = this[offset];
    var mul = 1;
    var i = 0;
    while (++i < byteLength && (mul *= 0x100)) {
      val += this[offset + i] * mul;
    }

    return val
  };

  Buffer$1.prototype.readUIntBE = function readUIntBE (offset, byteLength, noAssert) {
    offset = offset | 0;
    byteLength = byteLength | 0;
    if (!noAssert) {
      checkOffset(offset, byteLength, this.length);
    }

    var val = this[offset + --byteLength];
    var mul = 1;
    while (byteLength > 0 && (mul *= 0x100)) {
      val += this[offset + --byteLength] * mul;
    }

    return val
  };

  Buffer$1.prototype.readUInt8 = function readUInt8 (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 1, this.length);
    return this[offset]
  };

  Buffer$1.prototype.readUInt16LE = function readUInt16LE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 2, this.length);
    return this[offset] | (this[offset + 1] << 8)
  };

  Buffer$1.prototype.readUInt16BE = function readUInt16BE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 2, this.length);
    return (this[offset] << 8) | this[offset + 1]
  };

  Buffer$1.prototype.readUInt32LE = function readUInt32LE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 4, this.length);

    return ((this[offset]) |
        (this[offset + 1] << 8) |
        (this[offset + 2] << 16)) +
        (this[offset + 3] * 0x1000000)
  };

  Buffer$1.prototype.readUInt32BE = function readUInt32BE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 4, this.length);

    return (this[offset] * 0x1000000) +
      ((this[offset + 1] << 16) |
      (this[offset + 2] << 8) |
      this[offset + 3])
  };

  Buffer$1.prototype.readIntLE = function readIntLE (offset, byteLength, noAssert) {
    offset = offset | 0;
    byteLength = byteLength | 0;
    if (!noAssert) checkOffset(offset, byteLength, this.length);

    var val = this[offset];
    var mul = 1;
    var i = 0;
    while (++i < byteLength && (mul *= 0x100)) {
      val += this[offset + i] * mul;
    }
    mul *= 0x80;

    if (val >= mul) val -= Math.pow(2, 8 * byteLength);

    return val
  };

  Buffer$1.prototype.readIntBE = function readIntBE (offset, byteLength, noAssert) {
    offset = offset | 0;
    byteLength = byteLength | 0;
    if (!noAssert) checkOffset(offset, byteLength, this.length);

    var i = byteLength;
    var mul = 1;
    var val = this[offset + --i];
    while (i > 0 && (mul *= 0x100)) {
      val += this[offset + --i] * mul;
    }
    mul *= 0x80;

    if (val >= mul) val -= Math.pow(2, 8 * byteLength);

    return val
  };

  Buffer$1.prototype.readInt8 = function readInt8 (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 1, this.length);
    if (!(this[offset] & 0x80)) return (this[offset])
    return ((0xff - this[offset] + 1) * -1)
  };

  Buffer$1.prototype.readInt16LE = function readInt16LE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 2, this.length);
    var val = this[offset] | (this[offset + 1] << 8);
    return (val & 0x8000) ? val | 0xFFFF0000 : val
  };

  Buffer$1.prototype.readInt16BE = function readInt16BE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 2, this.length);
    var val = this[offset + 1] | (this[offset] << 8);
    return (val & 0x8000) ? val | 0xFFFF0000 : val
  };

  Buffer$1.prototype.readInt32LE = function readInt32LE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 4, this.length);

    return (this[offset]) |
      (this[offset + 1] << 8) |
      (this[offset + 2] << 16) |
      (this[offset + 3] << 24)
  };

  Buffer$1.prototype.readInt32BE = function readInt32BE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 4, this.length);

    return (this[offset] << 24) |
      (this[offset + 1] << 16) |
      (this[offset + 2] << 8) |
      (this[offset + 3])
  };

  Buffer$1.prototype.readFloatLE = function readFloatLE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 4, this.length);
    return read$1(this, offset, true, 23, 4)
  };

  Buffer$1.prototype.readFloatBE = function readFloatBE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 4, this.length);
    return read$1(this, offset, false, 23, 4)
  };

  Buffer$1.prototype.readDoubleLE = function readDoubleLE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 8, this.length);
    return read$1(this, offset, true, 52, 8)
  };

  Buffer$1.prototype.readDoubleBE = function readDoubleBE (offset, noAssert) {
    if (!noAssert) checkOffset(offset, 8, this.length);
    return read$1(this, offset, false, 52, 8)
  };

  function checkInt (buf, value, offset, ext, max, min) {
    if (!internalIsBuffer(buf)) throw new TypeError('"buffer" argument must be a Buffer instance')
    if (value > max || value < min) throw new RangeError('"value" argument is out of bounds')
    if (offset + ext > buf.length) throw new RangeError('Index out of range')
  }

  Buffer$1.prototype.writeUIntLE = function writeUIntLE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset | 0;
    byteLength = byteLength | 0;
    if (!noAssert) {
      var maxBytes = Math.pow(2, 8 * byteLength) - 1;
      checkInt(this, value, offset, byteLength, maxBytes, 0);
    }

    var mul = 1;
    var i = 0;
    this[offset] = value & 0xFF;
    while (++i < byteLength && (mul *= 0x100)) {
      this[offset + i] = (value / mul) & 0xFF;
    }

    return offset + byteLength
  };

  Buffer$1.prototype.writeUIntBE = function writeUIntBE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset | 0;
    byteLength = byteLength | 0;
    if (!noAssert) {
      var maxBytes = Math.pow(2, 8 * byteLength) - 1;
      checkInt(this, value, offset, byteLength, maxBytes, 0);
    }

    var i = byteLength - 1;
    var mul = 1;
    this[offset + i] = value & 0xFF;
    while (--i >= 0 && (mul *= 0x100)) {
      this[offset + i] = (value / mul) & 0xFF;
    }

    return offset + byteLength
  };

  Buffer$1.prototype.writeUInt8 = function writeUInt8 (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 1, 0xff, 0);
    if (!Buffer$1.TYPED_ARRAY_SUPPORT) value = Math.floor(value);
    this[offset] = (value & 0xff);
    return offset + 1
  };

  function objectWriteUInt16 (buf, value, offset, littleEndian) {
    if (value < 0) value = 0xffff + value + 1;
    for (var i = 0, j = Math.min(buf.length - offset, 2); i < j; ++i) {
      buf[offset + i] = (value & (0xff << (8 * (littleEndian ? i : 1 - i)))) >>>
        (littleEndian ? i : 1 - i) * 8;
    }
  }

  Buffer$1.prototype.writeUInt16LE = function writeUInt16LE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0);
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset] = (value & 0xff);
      this[offset + 1] = (value >>> 8);
    } else {
      objectWriteUInt16(this, value, offset, true);
    }
    return offset + 2
  };

  Buffer$1.prototype.writeUInt16BE = function writeUInt16BE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0xffff, 0);
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset] = (value >>> 8);
      this[offset + 1] = (value & 0xff);
    } else {
      objectWriteUInt16(this, value, offset, false);
    }
    return offset + 2
  };

  function objectWriteUInt32 (buf, value, offset, littleEndian) {
    if (value < 0) value = 0xffffffff + value + 1;
    for (var i = 0, j = Math.min(buf.length - offset, 4); i < j; ++i) {
      buf[offset + i] = (value >>> (littleEndian ? i : 3 - i) * 8) & 0xff;
    }
  }

  Buffer$1.prototype.writeUInt32LE = function writeUInt32LE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0);
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset + 3] = (value >>> 24);
      this[offset + 2] = (value >>> 16);
      this[offset + 1] = (value >>> 8);
      this[offset] = (value & 0xff);
    } else {
      objectWriteUInt32(this, value, offset, true);
    }
    return offset + 4
  };

  Buffer$1.prototype.writeUInt32BE = function writeUInt32BE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0xffffffff, 0);
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset] = (value >>> 24);
      this[offset + 1] = (value >>> 16);
      this[offset + 2] = (value >>> 8);
      this[offset + 3] = (value & 0xff);
    } else {
      objectWriteUInt32(this, value, offset, false);
    }
    return offset + 4
  };

  Buffer$1.prototype.writeIntLE = function writeIntLE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) {
      var limit = Math.pow(2, 8 * byteLength - 1);

      checkInt(this, value, offset, byteLength, limit - 1, -limit);
    }

    var i = 0;
    var mul = 1;
    var sub = 0;
    this[offset] = value & 0xFF;
    while (++i < byteLength && (mul *= 0x100)) {
      if (value < 0 && sub === 0 && this[offset + i - 1] !== 0) {
        sub = 1;
      }
      this[offset + i] = ((value / mul) >> 0) - sub & 0xFF;
    }

    return offset + byteLength
  };

  Buffer$1.prototype.writeIntBE = function writeIntBE (value, offset, byteLength, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) {
      var limit = Math.pow(2, 8 * byteLength - 1);

      checkInt(this, value, offset, byteLength, limit - 1, -limit);
    }

    var i = byteLength - 1;
    var mul = 1;
    var sub = 0;
    this[offset + i] = value & 0xFF;
    while (--i >= 0 && (mul *= 0x100)) {
      if (value < 0 && sub === 0 && this[offset + i + 1] !== 0) {
        sub = 1;
      }
      this[offset + i] = ((value / mul) >> 0) - sub & 0xFF;
    }

    return offset + byteLength
  };

  Buffer$1.prototype.writeInt8 = function writeInt8 (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 1, 0x7f, -0x80);
    if (!Buffer$1.TYPED_ARRAY_SUPPORT) value = Math.floor(value);
    if (value < 0) value = 0xff + value + 1;
    this[offset] = (value & 0xff);
    return offset + 1
  };

  Buffer$1.prototype.writeInt16LE = function writeInt16LE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000);
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset] = (value & 0xff);
      this[offset + 1] = (value >>> 8);
    } else {
      objectWriteUInt16(this, value, offset, true);
    }
    return offset + 2
  };

  Buffer$1.prototype.writeInt16BE = function writeInt16BE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 2, 0x7fff, -0x8000);
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset] = (value >>> 8);
      this[offset + 1] = (value & 0xff);
    } else {
      objectWriteUInt16(this, value, offset, false);
    }
    return offset + 2
  };

  Buffer$1.prototype.writeInt32LE = function writeInt32LE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000);
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset] = (value & 0xff);
      this[offset + 1] = (value >>> 8);
      this[offset + 2] = (value >>> 16);
      this[offset + 3] = (value >>> 24);
    } else {
      objectWriteUInt32(this, value, offset, true);
    }
    return offset + 4
  };

  Buffer$1.prototype.writeInt32BE = function writeInt32BE (value, offset, noAssert) {
    value = +value;
    offset = offset | 0;
    if (!noAssert) checkInt(this, value, offset, 4, 0x7fffffff, -0x80000000);
    if (value < 0) value = 0xffffffff + value + 1;
    if (Buffer$1.TYPED_ARRAY_SUPPORT) {
      this[offset] = (value >>> 24);
      this[offset + 1] = (value >>> 16);
      this[offset + 2] = (value >>> 8);
      this[offset + 3] = (value & 0xff);
    } else {
      objectWriteUInt32(this, value, offset, false);
    }
    return offset + 4
  };

  function checkIEEE754 (buf, value, offset, ext, max, min) {
    if (offset + ext > buf.length) throw new RangeError('Index out of range')
    if (offset < 0) throw new RangeError('Index out of range')
  }

  function writeFloat (buf, value, offset, littleEndian, noAssert) {
    if (!noAssert) {
      checkIEEE754(buf, value, offset, 4);
    }
    write(buf, value, offset, littleEndian, 23, 4);
    return offset + 4
  }

  Buffer$1.prototype.writeFloatLE = function writeFloatLE (value, offset, noAssert) {
    return writeFloat(this, value, offset, true, noAssert)
  };

  Buffer$1.prototype.writeFloatBE = function writeFloatBE (value, offset, noAssert) {
    return writeFloat(this, value, offset, false, noAssert)
  };

  function writeDouble (buf, value, offset, littleEndian, noAssert) {
    if (!noAssert) {
      checkIEEE754(buf, value, offset, 8);
    }
    write(buf, value, offset, littleEndian, 52, 8);
    return offset + 8
  }

  Buffer$1.prototype.writeDoubleLE = function writeDoubleLE (value, offset, noAssert) {
    return writeDouble(this, value, offset, true, noAssert)
  };

  Buffer$1.prototype.writeDoubleBE = function writeDoubleBE (value, offset, noAssert) {
    return writeDouble(this, value, offset, false, noAssert)
  };

  // copy(targetBuffer, targetStart=0, sourceStart=0, sourceEnd=buffer.length)
  Buffer$1.prototype.copy = function copy (target, targetStart, start, end) {
    if (!start) start = 0;
    if (!end && end !== 0) end = this.length;
    if (targetStart >= target.length) targetStart = target.length;
    if (!targetStart) targetStart = 0;
    if (end > 0 && end < start) end = start;

    // Copy 0 bytes; we're done
    if (end === start) return 0
    if (target.length === 0 || this.length === 0) return 0

    // Fatal error conditions
    if (targetStart < 0) {
      throw new RangeError('targetStart out of bounds')
    }
    if (start < 0 || start >= this.length) throw new RangeError('sourceStart out of bounds')
    if (end < 0) throw new RangeError('sourceEnd out of bounds')

    // Are we oob?
    if (end > this.length) end = this.length;
    if (target.length - targetStart < end - start) {
      end = target.length - targetStart + start;
    }

    var len = end - start;
    var i;

    if (this === target && start < targetStart && targetStart < end) {
      // descending copy from end
      for (i = len - 1; i >= 0; --i) {
        target[i + targetStart] = this[i + start];
      }
    } else if (len < 1000 || !Buffer$1.TYPED_ARRAY_SUPPORT) {
      // ascending copy from start
      for (i = 0; i < len; ++i) {
        target[i + targetStart] = this[i + start];
      }
    } else {
      Uint8Array.prototype.set.call(
        target,
        this.subarray(start, start + len),
        targetStart
      );
    }

    return len
  };

  // Usage:
  //    buffer.fill(number[, offset[, end]])
  //    buffer.fill(buffer[, offset[, end]])
  //    buffer.fill(string[, offset[, end]][, encoding])
  Buffer$1.prototype.fill = function fill (val, start, end, encoding) {
    // Handle string cases:
    if (typeof val === 'string') {
      if (typeof start === 'string') {
        encoding = start;
        start = 0;
        end = this.length;
      } else if (typeof end === 'string') {
        encoding = end;
        end = this.length;
      }
      if (val.length === 1) {
        var code = val.charCodeAt(0);
        if (code < 256) {
          val = code;
        }
      }
      if (encoding !== undefined && typeof encoding !== 'string') {
        throw new TypeError('encoding must be a string')
      }
      if (typeof encoding === 'string' && !Buffer$1.isEncoding(encoding)) {
        throw new TypeError('Unknown encoding: ' + encoding)
      }
    } else if (typeof val === 'number') {
      val = val & 255;
    }

    // Invalid ranges are not set to a default, so can range check early.
    if (start < 0 || this.length < start || this.length < end) {
      throw new RangeError('Out of range index')
    }

    if (end <= start) {
      return this
    }

    start = start >>> 0;
    end = end === undefined ? this.length : end >>> 0;

    if (!val) val = 0;

    var i;
    if (typeof val === 'number') {
      for (i = start; i < end; ++i) {
        this[i] = val;
      }
    } else {
      var bytes = internalIsBuffer(val)
        ? val
        : utf8ToBytes(new Buffer$1(val, encoding).toString());
      var len = bytes.length;
      for (i = 0; i < end - start; ++i) {
        this[i + start] = bytes[i % len];
      }
    }

    return this
  };

  // HELPER FUNCTIONS
  // ================

  var INVALID_BASE64_RE = /[^+\/0-9A-Za-z-_]/g;

  function base64clean (str) {
    // Node strips out invalid characters like \n and \t from the string, base64-js does not
    str = stringtrim(str).replace(INVALID_BASE64_RE, '');
    // Node converts strings with length < 2 to ''
    if (str.length < 2) return ''
    // Node allows for non-padded base64 strings (missing trailing ===), base64-js does not
    while (str.length % 4 !== 0) {
      str = str + '=';
    }
    return str
  }

  function stringtrim (str) {
    if (str.trim) return str.trim()
    return str.replace(/^\s+|\s+$/g, '')
  }

  function toHex (n) {
    if (n < 16) return '0' + n.toString(16)
    return n.toString(16)
  }

  function utf8ToBytes (string, units) {
    units = units || Infinity;
    var codePoint;
    var length = string.length;
    var leadSurrogate = null;
    var bytes = [];

    for (var i = 0; i < length; ++i) {
      codePoint = string.charCodeAt(i);

      // is surrogate component
      if (codePoint > 0xD7FF && codePoint < 0xE000) {
        // last char was a lead
        if (!leadSurrogate) {
          // no lead yet
          if (codePoint > 0xDBFF) {
            // unexpected trail
            if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
            continue
          } else if (i + 1 === length) {
            // unpaired lead
            if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
            continue
          }

          // valid lead
          leadSurrogate = codePoint;

          continue
        }

        // 2 leads in a row
        if (codePoint < 0xDC00) {
          if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
          leadSurrogate = codePoint;
          continue
        }

        // valid surrogate pair
        codePoint = (leadSurrogate - 0xD800 << 10 | codePoint - 0xDC00) + 0x10000;
      } else if (leadSurrogate) {
        // valid bmp char, but last char was a lead
        if ((units -= 3) > -1) bytes.push(0xEF, 0xBF, 0xBD);
      }

      leadSurrogate = null;

      // encode utf8
      if (codePoint < 0x80) {
        if ((units -= 1) < 0) break
        bytes.push(codePoint);
      } else if (codePoint < 0x800) {
        if ((units -= 2) < 0) break
        bytes.push(
          codePoint >> 0x6 | 0xC0,
          codePoint & 0x3F | 0x80
        );
      } else if (codePoint < 0x10000) {
        if ((units -= 3) < 0) break
        bytes.push(
          codePoint >> 0xC | 0xE0,
          codePoint >> 0x6 & 0x3F | 0x80,
          codePoint & 0x3F | 0x80
        );
      } else if (codePoint < 0x110000) {
        if ((units -= 4) < 0) break
        bytes.push(
          codePoint >> 0x12 | 0xF0,
          codePoint >> 0xC & 0x3F | 0x80,
          codePoint >> 0x6 & 0x3F | 0x80,
          codePoint & 0x3F | 0x80
        );
      } else {
        throw new Error('Invalid code point')
      }
    }

    return bytes
  }

  function asciiToBytes (str) {
    var byteArray = [];
    for (var i = 0; i < str.length; ++i) {
      // Node's code seems to be doing this and not & 0x7F..
      byteArray.push(str.charCodeAt(i) & 0xFF);
    }
    return byteArray
  }

  function utf16leToBytes (str, units) {
    var c, hi, lo;
    var byteArray = [];
    for (var i = 0; i < str.length; ++i) {
      if ((units -= 2) < 0) break

      c = str.charCodeAt(i);
      hi = c >> 8;
      lo = c % 256;
      byteArray.push(lo);
      byteArray.push(hi);
    }

    return byteArray
  }


  function base64ToBytes (str) {
    return toByteArray(base64clean(str))
  }

  function blitBuffer (src, dst, offset, length) {
    for (var i = 0; i < length; ++i) {
      if ((i + offset >= dst.length) || (i >= src.length)) break
      dst[i + offset] = src[i];
    }
    return i
  }

  function isnan (val) {
    return val !== val // eslint-disable-line no-self-compare
  }


  // the following is from is-buffer, also by Feross Aboukhadijeh and with same lisence
  // The _isBuffer check is for Safari 5-7 support, because it's missing
  // Object.prototype.constructor. Remove this eventually
  function isBuffer(obj) {
    return obj != null && (!!obj._isBuffer || isFastBuffer(obj) || isSlowBuffer(obj))
  }

  function isFastBuffer (obj) {
    return !!obj.constructor && typeof obj.constructor.isBuffer === 'function' && obj.constructor.isBuffer(obj)
  }

  // For Node v0.10 support. Remove this eventually.
  function isSlowBuffer (obj) {
    return typeof obj.readFloatLE === 'function' && typeof obj.slice === 'function' && isFastBuffer(obj.slice(0, 0))
  }

  let decoder;
  try {
  	decoder = new TextDecoder();
  } catch(error) {}
  let src;
  let srcEnd;
  let position$1 = 0;
  const LEGACY_RECORD_INLINE_ID = 105;
  const RECORD_DEFINITIONS_ID = 0xdffe;
  const RECORD_INLINE_ID = 0xdfff; // temporary first-come first-serve tag // proposed tag: 0x7265 // 're'
  const BUNDLED_STRINGS_ID = 0xdff9;
  const PACKED_REFERENCE_TAG_ID = 6;
  const STOP_CODE = {};
  let currentDecoder = {};
  let currentStructures;
  let srcString;
  let srcStringStart = 0;
  let srcStringEnd = 0;
  let bundledStrings$1;
  let referenceMap;
  let currentExtensions = [];
  let currentExtensionRanges = [];
  let packedValues;
  let dataView;
  let restoreMapsAsObject;
  let defaultOptions = {
  	useRecords: false,
  	mapsAsObjects: true
  };
  let sequentialMode = false;
  let inlineObjectReadThreshold = 2;
  // no-eval build
  try {
  	new Function('');
  } catch(error) {
  	// if eval variants are not supported, do not create inline object readers ever
  	inlineObjectReadThreshold = Infinity;
  }



  class Decoder {
  	constructor(options) {
  		if (options) {
  			if ((options.keyMap || options._keyMap) && !options.useRecords) {
  				options.useRecords = false;
  				options.mapsAsObjects = true;
  			}
  			if (options.useRecords === false && options.mapsAsObjects === undefined)
  				options.mapsAsObjects = true;
  			if (options.getStructures)
  				options.getShared = options.getStructures;
  			if (options.getShared && !options.structures)
  				(options.structures = []).uninitialized = true; // this is what we use to denote an uninitialized structures
  			if (options.keyMap) {
  				this.mapKey = new Map();
  				for (let [k,v] of Object.entries(options.keyMap)) this.mapKey.set(v,k);
  			}
  		}
  		Object.assign(this, options);
  	}
  	/*
  	decodeKey(key) {
  		return this.keyMap
  			? Object.keys(this.keyMap)[Object.values(this.keyMap).indexOf(key)] || key
  			: key
  	}
  	*/
  	decodeKey(key) {
  		return this.keyMap ? this.mapKey.get(key) || key : key
  	}
  	
  	encodeKey(key) {
  		return this.keyMap && this.keyMap.hasOwnProperty(key) ? this.keyMap[key] : key
  	}

  	encodeKeys(rec) {
  		if (!this._keyMap) return rec
  		let map = new Map();
  		for (let [k,v] of Object.entries(rec)) map.set((this._keyMap.hasOwnProperty(k) ? this._keyMap[k] : k), v);
  		return map
  	}

  	decodeKeys(map) {
  		if (!this._keyMap || map.constructor.name != 'Map') return map
  		if (!this._mapKey) {
  			this._mapKey = new Map();
  			for (let [k,v] of Object.entries(this._keyMap)) this._mapKey.set(v,k);
  		}
  		let res = {};
  		//map.forEach((v,k) => res[Object.keys(this._keyMap)[Object.values(this._keyMap).indexOf(k)] || k] = v)
  		map.forEach((v,k) => res[safeKey(this._mapKey.has(k) ? this._mapKey.get(k) : k)] =  v);
  		return res
  	}
  	
  	mapDecode(source, end) {
  	
  		let res = this.decode(source);
  		if (this._keyMap) { 
  			//Experiemntal support for Optimised KeyMap  decoding 
  			switch (res.constructor.name) {
  				case 'Array': return res.map(r => this.decodeKeys(r))
  				//case 'Map': return this.decodeKeys(res)
  			}
  		}
  		return res
  	}

  	decode(source, end) {
  		if (src) {
  			// re-entrant execution, save the state and restore it after we do this decode
  			return saveState(() => {
  				clearSource();
  				return this ? this.decode(source, end) : Decoder.prototype.decode.call(defaultOptions, source, end)
  			})
  		}
  		srcEnd = end > -1 ? end : source.length;
  		position$1 = 0;
  		srcStringEnd = 0;
  		srcString = null;
  		bundledStrings$1 = null;
  		src = source;
  		// this provides cached access to the data view for a buffer if it is getting reused, which is a recommend
  		// technique for getting data from a database where it can be copied into an existing buffer instead of creating
  		// new ones
  		try {
  			dataView = source.dataView || (source.dataView = new DataView(source.buffer, source.byteOffset, source.byteLength));
  		} catch(error) {
  			// if it doesn't have a buffer, maybe it is the wrong type of object
  			src = null;
  			if (source instanceof Uint8Array)
  				throw error
  			throw new Error('Source must be a Uint8Array or Buffer but was a ' + ((source && typeof source == 'object') ? source.constructor.name : typeof source))
  		}
  		if (this instanceof Decoder) {
  			currentDecoder = this;
  			packedValues = this.sharedValues &&
  				(this.pack ? new Array(this.maxPrivatePackedValues || 16).concat(this.sharedValues) :
  				this.sharedValues);
  			if (this.structures) {
  				currentStructures = this.structures;
  				return checkedRead()
  			} else if (!currentStructures || currentStructures.length > 0) {
  				currentStructures = [];
  			}
  		} else {
  			currentDecoder = defaultOptions;
  			if (!currentStructures || currentStructures.length > 0)
  				currentStructures = [];
  			packedValues = null;
  		}
  		return checkedRead()
  	}
  	decodeMultiple(source, forEach) {
  		let values, lastPosition = 0;
  		try {
  			let size = source.length;
  			sequentialMode = true;
  			let value = this ? this.decode(source, size) : defaultDecoder.decode(source, size);
  			if (forEach) {
  				if (forEach(value) === false) {
  					return
  				}
  				while(position$1 < size) {
  					lastPosition = position$1;
  					if (forEach(checkedRead()) === false) {
  						return
  					}
  				}
  			}
  			else {
  				values = [ value ];
  				while(position$1 < size) {
  					lastPosition = position$1;
  					values.push(checkedRead());
  				}
  				return values
  			}
  		} catch(error) {
  			error.lastPosition = lastPosition;
  			error.values = values;
  			throw error
  		} finally {
  			sequentialMode = false;
  			clearSource();
  		}
  	}
  }
  function checkedRead() {
  	try {
  		let result = read();
  		if (bundledStrings$1) {
  			if (position$1 >= bundledStrings$1.postBundlePosition) {
  				let error = new Error('Unexpected bundle position');
  				error.incomplete = true;
  				throw error
  			}
  			// bundled strings to skip past
  			position$1 = bundledStrings$1.postBundlePosition;
  			bundledStrings$1 = null;
  		}

  		if (position$1 == srcEnd) {
  			// finished reading this source, cleanup references
  			currentStructures = null;
  			src = null;
  			if (referenceMap)
  				referenceMap = null;
  		} else if (position$1 > srcEnd) {
  			// over read
  			let error = new Error('Unexpected end of CBOR data');
  			error.incomplete = true;
  			throw error
  		} else if (!sequentialMode) {
  			throw new Error('Data read, but end of buffer not reached')
  		}
  		// else more to read, but we are reading sequentially, so don't clear source yet
  		return result
  	} catch(error) {
  		clearSource();
  		if (error instanceof RangeError || error.message.startsWith('Unexpected end of buffer')) {
  			error.incomplete = true;
  		}
  		throw error
  	}
  }

  function read() {
  	let token = src[position$1++];
  	let majorType = token >> 5;
  	token = token & 0x1f;
  	if (token > 0x17) {
  		switch (token) {
  			case 0x18:
  				token = src[position$1++];
  				break
  			case 0x19:
  				if (majorType == 7) {
  					return getFloat16()
  				}
  				token = dataView.getUint16(position$1);
  				position$1 += 2;
  				break
  			case 0x1a:
  				if (majorType == 7) {
  					let value = dataView.getFloat32(position$1);
  					if (currentDecoder.useFloat32 > 2) {
  						// this does rounding of numbers that were encoded in 32-bit float to nearest significant decimal digit that could be preserved
  						let multiplier = mult10[((src[position$1] & 0x7f) << 1) | (src[position$1 + 1] >> 7)];
  						position$1 += 4;
  						return ((multiplier * value + (value > 0 ? 0.5 : -0.5)) >> 0) / multiplier
  					}
  					position$1 += 4;
  					return value
  				}
  				token = dataView.getUint32(position$1);
  				position$1 += 4;
  				break
  			case 0x1b:
  				if (majorType == 7) {
  					let value = dataView.getFloat64(position$1);
  					position$1 += 8;
  					return value
  				}
  				if (majorType > 1) {
  					if (dataView.getUint32(position$1) > 0)
  						throw new Error('JavaScript does not support arrays, maps, or strings with length over 4294967295')
  					token = dataView.getUint32(position$1 + 4);
  				} else if (currentDecoder.int64AsNumber) {
  					token = dataView.getUint32(position$1) * 0x100000000;
  					token += dataView.getUint32(position$1 + 4);
  				} else
  					token = dataView.getBigUint64(position$1);
  				position$1 += 8;
  				break
  			case 0x1f: 
  				// indefinite length
  				switch(majorType) {
  					case 2: // byte string
  					case 3: // text string
  						throw new Error('Indefinite length not supported for byte or text strings')
  					case 4: // array
  						let array = [];
  						let value, i = 0;
  						while ((value = read()) != STOP_CODE) {
  							array[i++] = value;
  						}
  						return majorType == 4 ? array : majorType == 3 ? array.join('') : Buffer$1.concat(array)
  					case 5: // map
  						let key;
  						if (currentDecoder.mapsAsObjects) {
  							let object = {};
  							if (currentDecoder.keyMap) while((key = read()) != STOP_CODE) object[safeKey(currentDecoder.decodeKey(key))] = read();
  							else while ((key = read()) != STOP_CODE) object[safeKey(key)] = read();
  							return object
  						} else {
  							if (restoreMapsAsObject) {
  								currentDecoder.mapsAsObjects = true;
  								restoreMapsAsObject = false;
  							}
  							let map = new Map();
  							if (currentDecoder.keyMap) while((key = read()) != STOP_CODE) map.set(currentDecoder.decodeKey(key), read());
  							else while ((key = read()) != STOP_CODE) map.set(key, read());
  							return map
  						}
  					case 7:
  						return STOP_CODE
  					default:
  						throw new Error('Invalid major type for indefinite length ' + majorType)
  				}
  			default:
  				throw new Error('Unknown token ' + token)
  		}
  	}
  	switch (majorType) {
  		case 0: // positive int
  			return token
  		case 1: // negative int
  			return ~token
  		case 2: // buffer
  			return readBin(token)
  		case 3: // string
  			if (srcStringEnd >= position$1) {
  				return srcString.slice(position$1 - srcStringStart, (position$1 += token) - srcStringStart)
  			}
  			if (srcStringEnd == 0 && srcEnd < 140 && token < 32) {
  				// for small blocks, avoiding the overhead of the extract call is helpful
  				let string = token < 16 ? shortStringInJS(token) : longStringInJS(token);
  				if (string != null)
  					return string
  			}
  			return readFixedString(token)
  		case 4: // array
  			let array = new Array(token);
  		  //if (currentDecoder.keyMap) for (let i = 0; i < token; i++) array[i] = currentDecoder.decodeKey(read())	
  			//else 
  			for (let i = 0; i < token; i++) array[i] = read();
  			return array
  		case 5: // map
  			if (currentDecoder.mapsAsObjects) {
  				let object = {};
  				if (currentDecoder.keyMap) for (let i = 0; i < token; i++) object[safeKey(currentDecoder.decodeKey(read()))] = read();
  				else for (let i = 0; i < token; i++) object[safeKey(read())] = read();
  				return object
  			} else {
  				if (restoreMapsAsObject) {
  					currentDecoder.mapsAsObjects = true;
  					restoreMapsAsObject = false;
  				}
  				let map = new Map();
  				if (currentDecoder.keyMap) for (let i = 0; i < token; i++) map.set(currentDecoder.decodeKey(read()),read());
  				else for (let i = 0; i < token; i++) map.set(read(), read());
  				return map
  			}
  		case 6: // extension
  			if (token >= BUNDLED_STRINGS_ID) {
  				let structure = currentStructures[token & 0x1fff]; // check record structures first
  				// At some point we may provide an option for dynamic tag assignment with a range like token >= 8 && (token < 16 || (token > 0x80 && token < 0xc0) || (token > 0x130 && token < 0x4000))
  				if (structure) {
  					if (!structure.read) structure.read = createStructureReader(structure);
  					return structure.read()
  				}
  				if (token < 0x10000) {
  					if (token == RECORD_INLINE_ID) { // we do a special check for this so that we can keep the
  						// currentExtensions as densely stored array (v8 stores arrays densely under about 3000 elements)
  						let length = readJustLength();
  						let id = read();
  						let structure = read();
  						recordDefinition(id, structure);
  						let object = {};
  						if (currentDecoder.keyMap) for (let i = 2; i < length; i++) {
  							let key = currentDecoder.decodeKey(structure[i - 2]);
  							object[safeKey(key)] = read();
  						}
  						else for (let i = 2; i < length; i++) {
  							let key = structure[i - 2];
  							object[safeKey(key)] = read();
  						}
  						return object
  					}
  					else if (token == RECORD_DEFINITIONS_ID) {
  						let length = readJustLength();
  						let id = read();
  						for (let i = 2; i < length; i++) {
  							recordDefinition(id++, read());
  						}
  						return read()
  					} else if (token == BUNDLED_STRINGS_ID) {
  						return readBundleExt()
  					}
  					if (currentDecoder.getShared) {
  						loadShared();
  						structure = currentStructures[token & 0x1fff];
  						if (structure) {
  							if (!structure.read)
  								structure.read = createStructureReader(structure);
  							return structure.read()
  						}
  					}
  				}
  			}
  			let extension = currentExtensions[token];
  			if (extension) {
  				if (extension.handlesRead)
  					return extension(read)
  				else
  					return extension(read())
  			} else {
  				let input = read();
  				for (let i = 0; i < currentExtensionRanges.length; i++) {
  					let value = currentExtensionRanges[i](token, input);
  					if (value !== undefined)
  						return value
  				}
  				return new Tag(input, token)
  			}
  		case 7: // fixed value
  			switch (token) {
  				case 0x14: return false
  				case 0x15: return true
  				case 0x16: return null
  				case 0x17: return; // undefined
  				case 0x1f:
  				default:
  					let packedValue = (packedValues || getPackedValues())[token];
  					if (packedValue !== undefined)
  						return packedValue
  					throw new Error('Unknown token ' + token)
  			}
  		default: // negative int
  			if (isNaN(token)) {
  				let error = new Error('Unexpected end of CBOR data');
  				error.incomplete = true;
  				throw error
  			}
  			throw new Error('Unknown CBOR token ' + token)
  	}
  }
  const validName = /^[a-zA-Z_$][a-zA-Z\d_$]*$/;
  function createStructureReader(structure) {
  	function readObject() {
  		// get the array size from the header
  		let length = src[position$1++];
  		//let majorType = token >> 5
  		length = length & 0x1f;
  		if (length > 0x17) {
  			switch (length) {
  				case 0x18:
  					length = src[position$1++];
  					break
  				case 0x19:
  					length = dataView.getUint16(position$1);
  					position$1 += 2;
  					break
  				case 0x1a:
  					length = dataView.getUint32(position$1);
  					position$1 += 4;
  					break
  				default:
  					throw new Error('Expected array header, but got ' + src[position$1 - 1])
  			}
  		}
  		// This initial function is quick to instantiate, but runs slower. After several iterations pay the cost to build the faster function
  		let compiledReader = this.compiledReader; // first look to see if we have the fast compiled function
  		while(compiledReader) {
  			// we have a fast compiled object literal reader
  			if (compiledReader.propertyCount === length)
  				return compiledReader(read) // with the right length, so we use it
  			compiledReader = compiledReader.next; // see if there is another reader with the right length
  		}
  		if (this.slowReads++ >= inlineObjectReadThreshold) { // create a fast compiled reader
  			let array = this.length == length ? this : this.slice(0, length);
  			compiledReader = currentDecoder.keyMap 
  			? new Function('r', 'return {' + array.map(k => currentDecoder.decodeKey(k)).map(k => validName.test(k) ? safeKey(k) + ':r()' : ('[' + JSON.stringify(k) + ']:r()')).join(',') + '}')
  			: new Function('r', 'return {' + array.map(key => validName.test(key) ? safeKey(key) + ':r()' : ('[' + JSON.stringify(key) + ']:r()')).join(',') + '}');
  			if (this.compiledReader)
  				compiledReader.next = this.compiledReader; // if there is an existing one, we store multiple readers as a linked list because it is usually pretty rare to have multiple readers (of different length) for the same structure
  			compiledReader.propertyCount = length;
  			this.compiledReader = compiledReader;
  			return compiledReader(read)
  		}
  		let object = {};
  		if (currentDecoder.keyMap) for (let i = 0; i < length; i++) object[safeKey(currentDecoder.decodeKey(this[i]))] = read();
  		else for (let i = 0; i < length; i++) {
  			object[safeKey(this[i])] = read();
  		}
  		return object
  	}
  	structure.slowReads = 0;
  	return readObject
  }

  function safeKey(key) {
  	return key === '__proto__' ? '__proto_' : key
  }

  let readFixedString = readStringJS;
  function readStringJS(length) {
  	let result;
  	if (length < 16) {
  		if (result = shortStringInJS(length))
  			return result
  	}
  	if (length > 64 && decoder)
  		return decoder.decode(src.subarray(position$1, position$1 += length))
  	const end = position$1 + length;
  	const units = [];
  	result = '';
  	while (position$1 < end) {
  		const byte1 = src[position$1++];
  		if ((byte1 & 0x80) === 0) {
  			// 1 byte
  			units.push(byte1);
  		} else if ((byte1 & 0xe0) === 0xc0) {
  			// 2 bytes
  			const byte2 = src[position$1++] & 0x3f;
  			units.push(((byte1 & 0x1f) << 6) | byte2);
  		} else if ((byte1 & 0xf0) === 0xe0) {
  			// 3 bytes
  			const byte2 = src[position$1++] & 0x3f;
  			const byte3 = src[position$1++] & 0x3f;
  			units.push(((byte1 & 0x1f) << 12) | (byte2 << 6) | byte3);
  		} else if ((byte1 & 0xf8) === 0xf0) {
  			// 4 bytes
  			const byte2 = src[position$1++] & 0x3f;
  			const byte3 = src[position$1++] & 0x3f;
  			const byte4 = src[position$1++] & 0x3f;
  			let unit = ((byte1 & 0x07) << 0x12) | (byte2 << 0x0c) | (byte3 << 0x06) | byte4;
  			if (unit > 0xffff) {
  				unit -= 0x10000;
  				units.push(((unit >>> 10) & 0x3ff) | 0xd800);
  				unit = 0xdc00 | (unit & 0x3ff);
  			}
  			units.push(unit);
  		} else {
  			units.push(byte1);
  		}

  		if (units.length >= 0x1000) {
  			result += fromCharCode.apply(String, units);
  			units.length = 0;
  		}
  	}

  	if (units.length > 0) {
  		result += fromCharCode.apply(String, units);
  	}

  	return result
  }
  let fromCharCode = String.fromCharCode;
  function longStringInJS(length) {
  	let start = position$1;
  	let bytes = new Array(length);
  	for (let i = 0; i < length; i++) {
  		const byte = src[position$1++];
  		if ((byte & 0x80) > 0) {
  			position$1 = start;
      			return
      		}
      		bytes[i] = byte;
      	}
      	return fromCharCode.apply(String, bytes)
  }
  function shortStringInJS(length) {
  	if (length < 4) {
  		if (length < 2) {
  			if (length === 0)
  				return ''
  			else {
  				let a = src[position$1++];
  				if ((a & 0x80) > 1) {
  					position$1 -= 1;
  					return
  				}
  				return fromCharCode(a)
  			}
  		} else {
  			let a = src[position$1++];
  			let b = src[position$1++];
  			if ((a & 0x80) > 0 || (b & 0x80) > 0) {
  				position$1 -= 2;
  				return
  			}
  			if (length < 3)
  				return fromCharCode(a, b)
  			let c = src[position$1++];
  			if ((c & 0x80) > 0) {
  				position$1 -= 3;
  				return
  			}
  			return fromCharCode(a, b, c)
  		}
  	} else {
  		let a = src[position$1++];
  		let b = src[position$1++];
  		let c = src[position$1++];
  		let d = src[position$1++];
  		if ((a & 0x80) > 0 || (b & 0x80) > 0 || (c & 0x80) > 0 || (d & 0x80) > 0) {
  			position$1 -= 4;
  			return
  		}
  		if (length < 6) {
  			if (length === 4)
  				return fromCharCode(a, b, c, d)
  			else {
  				let e = src[position$1++];
  				if ((e & 0x80) > 0) {
  					position$1 -= 5;
  					return
  				}
  				return fromCharCode(a, b, c, d, e)
  			}
  		} else if (length < 8) {
  			let e = src[position$1++];
  			let f = src[position$1++];
  			if ((e & 0x80) > 0 || (f & 0x80) > 0) {
  				position$1 -= 6;
  				return
  			}
  			if (length < 7)
  				return fromCharCode(a, b, c, d, e, f)
  			let g = src[position$1++];
  			if ((g & 0x80) > 0) {
  				position$1 -= 7;
  				return
  			}
  			return fromCharCode(a, b, c, d, e, f, g)
  		} else {
  			let e = src[position$1++];
  			let f = src[position$1++];
  			let g = src[position$1++];
  			let h = src[position$1++];
  			if ((e & 0x80) > 0 || (f & 0x80) > 0 || (g & 0x80) > 0 || (h & 0x80) > 0) {
  				position$1 -= 8;
  				return
  			}
  			if (length < 10) {
  				if (length === 8)
  					return fromCharCode(a, b, c, d, e, f, g, h)
  				else {
  					let i = src[position$1++];
  					if ((i & 0x80) > 0) {
  						position$1 -= 9;
  						return
  					}
  					return fromCharCode(a, b, c, d, e, f, g, h, i)
  				}
  			} else if (length < 12) {
  				let i = src[position$1++];
  				let j = src[position$1++];
  				if ((i & 0x80) > 0 || (j & 0x80) > 0) {
  					position$1 -= 10;
  					return
  				}
  				if (length < 11)
  					return fromCharCode(a, b, c, d, e, f, g, h, i, j)
  				let k = src[position$1++];
  				if ((k & 0x80) > 0) {
  					position$1 -= 11;
  					return
  				}
  				return fromCharCode(a, b, c, d, e, f, g, h, i, j, k)
  			} else {
  				let i = src[position$1++];
  				let j = src[position$1++];
  				let k = src[position$1++];
  				let l = src[position$1++];
  				if ((i & 0x80) > 0 || (j & 0x80) > 0 || (k & 0x80) > 0 || (l & 0x80) > 0) {
  					position$1 -= 12;
  					return
  				}
  				if (length < 14) {
  					if (length === 12)
  						return fromCharCode(a, b, c, d, e, f, g, h, i, j, k, l)
  					else {
  						let m = src[position$1++];
  						if ((m & 0x80) > 0) {
  							position$1 -= 13;
  							return
  						}
  						return fromCharCode(a, b, c, d, e, f, g, h, i, j, k, l, m)
  					}
  				} else {
  					let m = src[position$1++];
  					let n = src[position$1++];
  					if ((m & 0x80) > 0 || (n & 0x80) > 0) {
  						position$1 -= 14;
  						return
  					}
  					if (length < 15)
  						return fromCharCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  					let o = src[position$1++];
  					if ((o & 0x80) > 0) {
  						position$1 -= 15;
  						return
  					}
  					return fromCharCode(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  				}
  			}
  		}
  	}
  }

  function readBin(length) {
  	return currentDecoder.copyBuffers ?
  		// specifically use the copying slice (not the node one)
  		Uint8Array.prototype.slice.call(src, position$1, position$1 += length) :
  		src.subarray(position$1, position$1 += length)
  }
  let f32Array = new Float32Array(1);
  let u8Array = new Uint8Array(f32Array.buffer, 0, 4);
  function getFloat16() {
  	let byte0 = src[position$1++];
  	let byte1 = src[position$1++];
  	let exponent = (byte0 & 0x7f) >> 2;
  	if (exponent === 0x1f) { // specials
  		if (byte1 || (byte0 & 3))
  			return NaN;
  		return (byte0 & 0x80) ? -Infinity : Infinity;
  	}
  	if (exponent === 0) { // sub-normals
  		// significand with 10 fractional bits and divided by 2^14
  		let abs = (((byte0 & 3) << 8) | byte1) / (1 << 24);
  		return (byte0 & 0x80) ? -abs : abs
  	}

  	u8Array[3] = (byte0 & 0x80) | // sign bit
  		((exponent >> 1) + 56); // 4 of 5 of the exponent bits, re-offset-ed
  	u8Array[2] = ((byte0 & 7) << 5) | // last exponent bit and first two mantissa bits
  		(byte1 >> 3); // next 5 bits of mantissa
  	u8Array[1] = byte1 << 5; // last three bits of mantissa
  	u8Array[0] = 0;
  	return f32Array[0];
  }

  new Array(4096);

  class Tag {
  	constructor(value, tag) {
  		this.value = value;
  		this.tag = tag;
  	}
  }

  currentExtensions[0] = (dateString) => {
  	// string date extension
  	return new Date(dateString)
  };

  currentExtensions[1] = (epochSec) => {
  	// numeric date extension
  	return new Date(Math.round(epochSec * 1000))
  };

  currentExtensions[2] = (buffer) => {
  	// bigint extension
  	let value = BigInt(0);
  	for (let i = 0, l = buffer.byteLength; i < l; i++) {
  		value = BigInt(buffer[i]) + value << BigInt(8);
  	}
  	return value
  };

  currentExtensions[3] = (buffer) => {
  	// negative bigint extension
  	return BigInt(-1) - currentExtensions[2](buffer)
  };
  currentExtensions[4] = (fraction) => {
  	// best to reparse to maintain accuracy
  	return +(fraction[1] + 'e' + fraction[0])
  };

  currentExtensions[5] = (fraction) => {
  	// probably not sufficiently accurate
  	return fraction[1] * Math.exp(fraction[0] * Math.log(2))
  };

  // the registration of the record definition extension
  const recordDefinition = (id, structure) => {
  	id = id - 0xe000;
  	let existingStructure = currentStructures[id];
  	if (existingStructure && existingStructure.isShared) {
  		(currentStructures.restoreStructures || (currentStructures.restoreStructures = []))[id] = existingStructure;
  	}
  	currentStructures[id] = structure;

  	structure.read = createStructureReader(structure);
  };
  currentExtensions[LEGACY_RECORD_INLINE_ID] = (data) => {
  	let length = data.length;
  	let structure = data[1];
  	recordDefinition(data[0], structure);
  	let object = {};
  	for (let i = 2; i < length; i++) {
  		let key = structure[i - 2];
  		object[safeKey(key)] = data[i];
  	}
  	return object
  };
  currentExtensions[14] = (value) => {
  	if (bundledStrings$1)
  		return bundledStrings$1[0].slice(bundledStrings$1.position0, bundledStrings$1.position0 += value)
  	return new Tag(value, 14)
  };
  currentExtensions[15] = (value) => {
  	if (bundledStrings$1)
  		return bundledStrings$1[1].slice(bundledStrings$1.position1, bundledStrings$1.position1 += value)
  	return new Tag(value, 15)
  };
  let glbl = { Error, RegExp };
  currentExtensions[27] = (data) => { // http://cbor.schmorp.de/generic-object
  	return (glbl[data[0]] || Error)(data[1], data[2])
  };
  const packedTable = (read) => {
  	if (src[position$1++] != 0x84)
  		throw new Error('Packed values structure must be followed by a 4 element array')
  	let newPackedValues = read(); // packed values
  	packedValues = packedValues ? newPackedValues.concat(packedValues.slice(newPackedValues.length)) : newPackedValues;
  	packedValues.prefixes = read();
  	packedValues.suffixes = read();
  	return read() // read the rump
  };
  packedTable.handlesRead = true;
  currentExtensions[51] = packedTable;

  currentExtensions[PACKED_REFERENCE_TAG_ID] = (data) => { // packed reference
  	if (!packedValues) {
  		if (currentDecoder.getShared)
  			loadShared();
  		else
  			return new Tag(data, PACKED_REFERENCE_TAG_ID)
  	}
  	if (typeof data == 'number')
  		return packedValues[16 + (data >= 0 ? 2 * data : (-2 * data - 1))]
  	throw new Error('No support for non-integer packed references yet')
  };

  // The following code is an incomplete implementation of http://cbor.schmorp.de/stringref
  // the real thing would need to implemennt more logic to populate the stringRefs table and
  // maintain a stack of stringRef "namespaces".
  //
  // currentExtensions[25] = (id) => {
  // 	return stringRefs[id]
  // }
  // currentExtensions[256] = (read) => {
  // 	stringRefs = []
  // 	try {
  // 		return read()
  // 	} finally {
  // 		stringRefs = null
  // 	}
  // }
  // currentExtensions[256].handlesRead = true

  currentExtensions[28] = (read) => { 
  	// shareable http://cbor.schmorp.de/value-sharing (for structured clones)
  	if (!referenceMap) {
  		referenceMap = new Map();
  		referenceMap.id = 0;
  	}
  	let id = referenceMap.id++;
  	let token = src[position$1];
  	let target;
  	// TODO: handle Maps, Sets, and other types that can cycle; this is complicated, because you potentially need to read
  	// ahead past references to record structure definitions
  	if ((token >> 5) == 4)
  		target = [];
  	else
  		target = {};

  	let refEntry = { target }; // a placeholder object
  	referenceMap.set(id, refEntry);
  	let targetProperties = read(); // read the next value as the target object to id
  	if (refEntry.used) // there is a cycle, so we have to assign properties to original target
  		return Object.assign(target, targetProperties)
  	refEntry.target = targetProperties; // the placeholder wasn't used, replace with the deserialized one
  	return targetProperties // no cycle, can just use the returned read object
  };
  currentExtensions[28].handlesRead = true;

  currentExtensions[29] = (id) => {
  	// sharedref http://cbor.schmorp.de/value-sharing (for structured clones)
  	let refEntry = referenceMap.get(id);
  	refEntry.used = true;
  	return refEntry.target
  };

  currentExtensions[258] = (array) => new Set(array); // https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md
  (currentExtensions[259] = (read) => {
  	// https://github.com/shanewholloway/js-cbor-codec/blob/master/docs/CBOR-259-spec
  	// for decoding as a standard Map
  	if (currentDecoder.mapsAsObjects) {
  		currentDecoder.mapsAsObjects = false;
  		restoreMapsAsObject = true;
  	}
  	return read()
  }).handlesRead = true;
  function combine(a, b) {
  	if (typeof a === 'string')
  		return a + b
  	if (a instanceof Array)
  		return a.concat(b)
  	return Object.assign({}, a, b)
  }
  function getPackedValues() {
  	if (!packedValues) {
  		if (currentDecoder.getShared)
  			loadShared();
  		else
  			throw new Error('No packed values available')
  	}
  	return packedValues
  }
  const SHARED_DATA_TAG_ID = 0x53687264; // ascii 'Shrd'
  currentExtensionRanges.push((tag, input) => {
  	if (tag >= 225 && tag <= 255)
  		return combine(getPackedValues().prefixes[tag - 224], input)
  	if (tag >= 28704 && tag <= 32767)
  		return combine(getPackedValues().prefixes[tag - 28672], input)
  	if (tag >= 1879052288 && tag <= 2147483647)
  		return combine(getPackedValues().prefixes[tag - 1879048192], input)
  	if (tag >= 216 && tag <= 223)
  		return combine(input, getPackedValues().suffixes[tag - 216])
  	if (tag >= 27647 && tag <= 28671)
  		return combine(input, getPackedValues().suffixes[tag - 27639])
  	if (tag >= 1811940352 && tag <= 1879048191)
  		return combine(input, getPackedValues().suffixes[tag - 1811939328])
  	if (tag == SHARED_DATA_TAG_ID) {// we do a special check for this so that we can keep the currentExtensions as densely stored array (v8 stores arrays densely under about 3000 elements)
  		return {
  			packedValues: packedValues,
  			structures: currentStructures.slice(0),
  			version: input,
  		}
  	}
  	if (tag == 55799) // self-descriptive CBOR tag, just return input value
  		return input
  });

  const isLittleEndianMachine$1 = new Uint8Array(new Uint16Array([1]).buffer)[0] == 1;
  const typedArrays = [Uint8Array, Uint8ClampedArray, Uint16Array, Uint32Array,
  	typeof BigUint64Array == 'undefined' ? { name:'BigUint64Array' } : BigUint64Array, Int8Array, Int16Array, Int32Array,
  	typeof BigInt64Array == 'undefined' ? { name:'BigInt64Array' } : BigInt64Array, Float32Array, Float64Array];
  const typedArrayTags = [64, 68, 69, 70, 71, 72, 77, 78, 79, 85, 86];
  for (let i = 0; i < typedArrays.length; i++) {
  	registerTypedArray(typedArrays[i], typedArrayTags[i]);
  }
  function registerTypedArray(TypedArray, tag) {
  	let dvMethod = 'get' + TypedArray.name.slice(0, -5);
  	let bytesPerElement;
  	if (typeof TypedArray === 'function')
  		bytesPerElement = TypedArray.BYTES_PER_ELEMENT;
  	else
  		TypedArray = null;
  	for (let littleEndian = 0; littleEndian < 2; littleEndian++) {
  		if (!littleEndian && bytesPerElement == 1)
  			continue
  		let sizeShift = bytesPerElement == 2 ? 1 : bytesPerElement == 4 ? 2 : 3;
  		currentExtensions[littleEndian ? tag : (tag - 4)] = (bytesPerElement == 1 || littleEndian == isLittleEndianMachine$1) ? (buffer) => {
  			if (!TypedArray)
  				throw new Error('Could not find typed array for code ' + tag)
  			// we have to always slice/copy here to get a new ArrayBuffer that is word/byte aligned
  			return new TypedArray(Uint8Array.prototype.slice.call(buffer, 0).buffer)
  		} : buffer => {
  			if (!TypedArray)
  				throw new Error('Could not find typed array for code ' + tag)
  			let dv = new DataView(buffer.buffer, buffer.byteOffset, buffer.byteLength);
  			let elements = buffer.length >> sizeShift;
  			let ta = new TypedArray(elements);
  			let method = dv[dvMethod];
  			for (let i = 0; i < elements; i++) {
  				ta[i] = method.call(dv, i << sizeShift, littleEndian);
  			}
  			return ta
  		};
  	}
  }

  function readBundleExt() {
  	let length = readJustLength();
  	let bundlePosition = position$1 + read();
  	for (let i = 2; i < length; i++) {
  		// skip past bundles that were already read
  		let bundleLength = readJustLength(); // this will increment position, so must add to position afterwards
  		position$1 += bundleLength;
  	}
  	let dataPosition = position$1;
  	position$1 = bundlePosition;
  	bundledStrings$1 = [readStringJS(readJustLength()), readStringJS(readJustLength())];
  	bundledStrings$1.position0 = 0;
  	bundledStrings$1.position1 = 0;
  	bundledStrings$1.postBundlePosition = position$1;
  	position$1 = dataPosition;
  	return read()
  }

  function readJustLength() {
  	let token = src[position$1++] & 0x1f;
  	if (token > 0x17) {
  		switch (token) {
  			case 0x18:
  				token = src[position$1++];
  				break
  			case 0x19:
  				token = dataView.getUint16(position$1);
  				position$1 += 2;
  				break
  			case 0x1a:
  				token = dataView.getUint32(position$1);
  				position$1 += 4;
  				break
  		}
  	}
  	return token
  }

  function loadShared() {
  	if (currentDecoder.getShared) {
  		let sharedData = saveState(() => {
  			// save the state in case getShared modifies our buffer
  			src = null;
  			return currentDecoder.getShared()
  		}) || {};
  		let updatedStructures = sharedData.structures || [];
  		currentDecoder.sharedVersion = sharedData.version;
  		packedValues = currentDecoder.sharedValues = sharedData.packedValues;
  		if (currentStructures === true)
  			currentDecoder.structures = currentStructures = updatedStructures;
  		else
  			currentStructures.splice.apply(currentStructures, [0, updatedStructures.length].concat(updatedStructures));
  	}
  }

  function saveState(callback) {
  	let savedSrcEnd = srcEnd;
  	let savedPosition = position$1;
  	let savedSrcStringStart = srcStringStart;
  	let savedSrcStringEnd = srcStringEnd;
  	let savedSrcString = srcString;
  	let savedReferenceMap = referenceMap;
  	let savedBundledStrings = bundledStrings$1;

  	// TODO: We may need to revisit this if we do more external calls to user code (since it could be slow)
  	let savedSrc = new Uint8Array(src.slice(0, srcEnd)); // we copy the data in case it changes while external data is processed
  	let savedStructures = currentStructures;
  	let savedDecoder = currentDecoder;
  	let savedSequentialMode = sequentialMode;
  	let value = callback();
  	srcEnd = savedSrcEnd;
  	position$1 = savedPosition;
  	srcStringStart = savedSrcStringStart;
  	srcStringEnd = savedSrcStringEnd;
  	srcString = savedSrcString;
  	referenceMap = savedReferenceMap;
  	bundledStrings$1 = savedBundledStrings;
  	src = savedSrc;
  	sequentialMode = savedSequentialMode;
  	currentStructures = savedStructures;
  	currentDecoder = savedDecoder;
  	dataView = new DataView(src.buffer, src.byteOffset, src.byteLength);
  	return value
  }
  function clearSource() {
  	src = null;
  	referenceMap = null;
  	currentStructures = null;
  }

  function addExtension$1(extension) {
  	currentExtensions[extension.tag] = extension.decode;
  }

  const mult10 = new Array(147); // this is a table matching binary exponents to the multiplier to determine significant digit rounding
  for (let i = 0; i < 256; i++) {
  	mult10[i] = +('1e' + Math.floor(45.15 - i * 0.30103));
  }
  let defaultDecoder = new Decoder({ useRecords: false });
  defaultDecoder.decode;
  defaultDecoder.decodeMultiple;

  let textEncoder;
  try {
  	textEncoder = new TextEncoder();
  } catch (error) {}
  let extensions, extensionClasses;
  const Buffer = typeof globalThis === 'object' && globalThis.Buffer;
  const hasNodeBuffer = typeof Buffer !== 'undefined';
  const ByteArrayAllocate = hasNodeBuffer ? Buffer.allocUnsafeSlow : Uint8Array;
  const ByteArray = hasNodeBuffer ? Buffer : Uint8Array;
  const MAX_STRUCTURES = 0x100;
  const MAX_BUFFER_SIZE = hasNodeBuffer ? 0x100000000 : 0x7fd00000;
  let throwOnIterable;
  let target;
  let targetView;
  let position = 0;
  let safeEnd;
  let bundledStrings = null;
  const MAX_BUNDLE_SIZE = 0xf000;
  const hasNonLatin = /[\u0080-\uFFFF]/;
  const RECORD_SYMBOL = Symbol('record-id');
  class Encoder extends Decoder {
  	constructor(options) {
  		super(options);
  		this.offset = 0;
  		let start;
  		let sharedStructures;
  		let hasSharedUpdate;
  		let structures;
  		let referenceMap;
  		options = options || {};
  		let encodeUtf8 = ByteArray.prototype.utf8Write ? function(string, position, maxBytes) {
  			return target.utf8Write(string, position, maxBytes)
  		} : (textEncoder && textEncoder.encodeInto) ?
  			function(string, position) {
  				return textEncoder.encodeInto(string, target.subarray(position)).written
  			} : false;

  		let encoder = this;
  		let hasSharedStructures = options.structures || options.saveStructures;
  		let maxSharedStructures = options.maxSharedStructures;
  		if (maxSharedStructures == null)
  			maxSharedStructures = hasSharedStructures ? 128 : 0;
  		if (maxSharedStructures > 8190)
  			throw new Error('Maximum maxSharedStructure is 8190')
  		let isSequential = options.sequential;
  		if (isSequential) {
  			maxSharedStructures = 0;
  		}
  		if (!this.structures)
  			this.structures = [];
  		if (this.saveStructures)
  			this.saveShared = this.saveStructures;
  		let samplingPackedValues, packedObjectMap, sharedValues = options.sharedValues;
  		let sharedPackedObjectMap;
  		if (sharedValues) {
  			sharedPackedObjectMap = Object.create(null);
  			for (let i = 0, l = sharedValues.length; i < l; i++) {
  				sharedPackedObjectMap[sharedValues[i]] = i;
  			}
  		}
  		let recordIdsToRemove = [];
  		let transitionsCount = 0;
  		let serializationsSinceTransitionRebuild = 0;
  		
  		this.mapEncode = function(value, encodeOptions) {
  			// Experimental support for premapping keys using _keyMap instad of keyMap - not optiimised yet)
  			if (this._keyMap && !this._mapped) {
  				//console.log('encoding ', value)
  				switch (value.constructor.name) {
  					case 'Array': 
  						value = value.map(r => this.encodeKeys(r));
  						break
  					//case 'Map': 
  					//	value = this.encodeKeys(value)
  					//	break
  				}
  				//this._mapped = true
  			}
  			return this.encode(value, encodeOptions)
  		};
  		
  		this.encode = function(value, encodeOptions)	{
  			if (!target) {
  				target = new ByteArrayAllocate(8192);
  				targetView = new DataView(target.buffer, 0, 8192);
  				position = 0;
  			}
  			safeEnd = target.length - 10;
  			if (safeEnd - position < 0x800) {
  				// don't start too close to the end, 
  				target = new ByteArrayAllocate(target.length);
  				targetView = new DataView(target.buffer, 0, target.length);
  				safeEnd = target.length - 10;
  				position = 0;
  			} else if (encodeOptions === REUSE_BUFFER_MODE)
  				position = (position + 7) & 0x7ffffff8; // Word align to make any future copying of this buffer faster
  			start = position;
  			if (encoder.useSelfDescribedHeader) {
  				targetView.setUint32(position, 0xd9d9f700); // tag two byte, then self-descriptive tag
  				position += 3;
  			}
  			referenceMap = encoder.structuredClone ? new Map() : null;
  			if (encoder.bundleStrings && typeof value !== 'string') {
  				bundledStrings = [];
  				bundledStrings.size = Infinity; // force a new bundle start on first string
  			} else
  				bundledStrings = null;

  			sharedStructures = encoder.structures;
  			if (sharedStructures) {
  				if (sharedStructures.uninitialized) {
  					let sharedData = encoder.getShared() || {};
  					encoder.structures = sharedStructures = sharedData.structures || [];
  					encoder.sharedVersion = sharedData.version;
  					let sharedValues = encoder.sharedValues = sharedData.packedValues;
  					if (sharedValues) {
  						sharedPackedObjectMap = {};
  						for (let i = 0, l = sharedValues.length; i < l; i++)
  							sharedPackedObjectMap[sharedValues[i]] = i;
  					}
  				}
  				let sharedStructuresLength = sharedStructures.length;
  				if (sharedStructuresLength > maxSharedStructures && !isSequential)
  					sharedStructuresLength = maxSharedStructures;
  				if (!sharedStructures.transitions) {
  					// rebuild our structure transitions
  					sharedStructures.transitions = Object.create(null);
  					for (let i = 0; i < sharedStructuresLength; i++) {
  						let keys = sharedStructures[i];
  						//console.log('shared struct keys:', keys)
  						if (!keys)
  							continue
  						let nextTransition, transition = sharedStructures.transitions;
  						for (let j = 0, l = keys.length; j < l; j++) {
  							if (transition[RECORD_SYMBOL] === undefined)
  								transition[RECORD_SYMBOL] = i;
  							let key = keys[j];
  							nextTransition = transition[key];
  							if (!nextTransition) {
  								nextTransition = transition[key] = Object.create(null);
  							}
  							transition = nextTransition;
  						}
  						transition[RECORD_SYMBOL] = i | 0x100000;
  					}
  				}
  				if (!isSequential)
  					sharedStructures.nextId = sharedStructuresLength;
  			}
  			if (hasSharedUpdate)
  				hasSharedUpdate = false;
  			structures = sharedStructures || [];
  			packedObjectMap = sharedPackedObjectMap;
  			if (options.pack) {
  				let packedValues = new Map();
  				packedValues.values = [];
  				packedValues.encoder = encoder;
  				packedValues.maxValues = options.maxPrivatePackedValues || (sharedPackedObjectMap ? 16 : Infinity);
  				packedValues.objectMap = sharedPackedObjectMap || false;
  				packedValues.samplingPackedValues = samplingPackedValues;
  				findRepetitiveStrings(value, packedValues);
  				if (packedValues.values.length > 0) {
  					target[position++] = 0xd8; // one-byte tag
  					target[position++] = 51; // tag 51 for packed shared structures https://www.potaroo.net/ietf/ids/draft-ietf-cbor-packed-03.txt
  					writeArrayHeader(4);
  					let valuesArray = packedValues.values;
  					encode(valuesArray);
  					writeArrayHeader(0); // prefixes
  					writeArrayHeader(0); // suffixes
  					packedObjectMap = Object.create(sharedPackedObjectMap || null);
  					for (let i = 0, l = valuesArray.length; i < l; i++) {
  						packedObjectMap[valuesArray[i]] = i;
  					}
  				}
  			}
  			throwOnIterable = encodeOptions & THROW_ON_ITERABLE;
  			try {
  				if (throwOnIterable)
  					return;
  				encode(value);
  				if (bundledStrings) {
  					writeBundles(start, encode);
  				}
  				encoder.offset = position; // update the offset so next serialization doesn't write over our buffer, but can continue writing to same buffer sequentially
  				if (referenceMap && referenceMap.idsToInsert) {
  					position += referenceMap.idsToInsert.length * 2;
  					if (position > safeEnd)
  						makeRoom(position);
  					encoder.offset = position;
  					let serialized = insertIds(target.subarray(start, position), referenceMap.idsToInsert);
  					referenceMap = null;
  					return serialized
  				}
  				if (encodeOptions & REUSE_BUFFER_MODE) {
  					target.start = start;
  					target.end = position;
  					return target
  				}
  				return target.subarray(start, position) // position can change if we call encode again in saveShared, so we get the buffer now
  			} finally {
  				if (sharedStructures) {
  					if (serializationsSinceTransitionRebuild < 10)
  						serializationsSinceTransitionRebuild++;
  					if (sharedStructures.length > maxSharedStructures)
  						sharedStructures.length = maxSharedStructures;
  					if (transitionsCount > 10000) {
  						// force a rebuild occasionally after a lot of transitions so it can get cleaned up
  						sharedStructures.transitions = null;
  						serializationsSinceTransitionRebuild = 0;
  						transitionsCount = 0;
  						if (recordIdsToRemove.length > 0)
  							recordIdsToRemove = [];
  					} else if (recordIdsToRemove.length > 0 && !isSequential) {
  						for (let i = 0, l = recordIdsToRemove.length; i < l; i++) {
  							recordIdsToRemove[i][RECORD_SYMBOL] = undefined;
  						}
  						recordIdsToRemove = [];
  						//sharedStructures.nextId = maxSharedStructures
  					}
  				}
  				if (hasSharedUpdate && encoder.saveShared) {
  					if (encoder.structures.length > maxSharedStructures) {
  						encoder.structures = encoder.structures.slice(0, maxSharedStructures);
  					}
  					// we can't rely on start/end with REUSE_BUFFER_MODE since they will (probably) change when we save
  					let returnBuffer = target.subarray(start, position);
  					if (encoder.updateSharedData() === false)
  						return encoder.encode(value) // re-encode if it fails
  					return returnBuffer
  				}
  				if (encodeOptions & RESET_BUFFER_MODE)
  					position = start;
  			}
  		};
  		this.findCommonStringsToPack = () => {
  			samplingPackedValues = new Map();
  			if (!sharedPackedObjectMap)
  				sharedPackedObjectMap = Object.create(null);
  			return (options) => {
  				let threshold = options && options.threshold || 4;
  				let position = this.pack ? options.maxPrivatePackedValues || 16 : 0;
  				if (!sharedValues)
  					sharedValues = this.sharedValues = [];
  				for (let [ key, status ] of samplingPackedValues) {
  					if (status.count > threshold) {
  						sharedPackedObjectMap[key] = position++;
  						sharedValues.push(key);
  						hasSharedUpdate = true;
  					}
  				}
  				while (this.saveShared && this.updateSharedData() === false) {}
  				samplingPackedValues = null;
  			}
  		};
  		const encode = (value) => {
  			if (position > safeEnd)
  				target = makeRoom(position);

  			var type = typeof value;
  			var length;
  			if (type === 'string') {
  				if (packedObjectMap) {
  					let packedPosition = packedObjectMap[value];
  					if (packedPosition >= 0) {
  						if (packedPosition < 16)
  							target[position++] = packedPosition + 0xe0; // simple values, defined in https://www.potaroo.net/ietf/ids/draft-ietf-cbor-packed-03.txt
  						else {
  							target[position++] = 0xc6; // tag 6 defined in https://www.potaroo.net/ietf/ids/draft-ietf-cbor-packed-03.txt
  							if (packedPosition & 1)
  								encode((15 - packedPosition) >> 1);
  							else
  								encode((packedPosition - 16) >> 1);
  						}
  						return
  /*						} else if (packedStatus.serializationId != serializationId) {
  							packedStatus.serializationId = serializationId
  							packedStatus.count = 1
  							if (options.sharedPack) {
  								let sharedCount = packedStatus.sharedCount = (packedStatus.sharedCount || 0) + 1
  								if (shareCount > (options.sharedPack.threshold || 5)) {
  									let sharedPosition = packedStatus.position = packedStatus.nextSharedPosition
  									hasSharedUpdate = true
  									if (sharedPosition < 16)
  										target[position++] = sharedPosition + 0xc0

  								}
  							}
  						} // else any in-doc incrementation?*/
  					} else if (samplingPackedValues && !options.pack) {
  						let status = samplingPackedValues.get(value);
  						if (status)
  							status.count++;
  						else
  							samplingPackedValues.set(value, {
  								count: 1,
  							});
  					}
  				}
  				let strLength = value.length;
  				if (bundledStrings && strLength >= 4 && strLength < 0x400) {
  					if ((bundledStrings.size += strLength) > MAX_BUNDLE_SIZE) {
  						let extStart;
  						let maxBytes = (bundledStrings[0] ? bundledStrings[0].length * 3 + bundledStrings[1].length : 0) + 10;
  						if (position + maxBytes > safeEnd)
  							target = makeRoom(position + maxBytes);
  						target[position++] = 0xd9; // tag 16-bit
  						target[position++] = 0xdf; // tag 0xdff9
  						target[position++] = 0xf9;
  						// TODO: If we only have one bundle with any string data, only write one string bundle
  						target[position++] = bundledStrings.position ? 0x84 : 0x82; // array of 4 or 2 elements depending on if we write bundles
  						target[position++] = 0x1a; // 32-bit unsigned int
  						extStart = position - start;
  						position += 4; // reserve for writing bundle reference
  						if (bundledStrings.position) {
  							writeBundles(start, encode); // write the last bundles
  						}
  						bundledStrings = ['', '']; // create new ones
  						bundledStrings.size = 0;
  						bundledStrings.position = extStart;
  					}
  					let twoByte = hasNonLatin.test(value);
  					bundledStrings[twoByte ? 0 : 1] += value;
  					target[position++] = twoByte ? 0xce : 0xcf;
  					encode(strLength);
  					return
  				}
  				let headerSize;
  				// first we estimate the header size, so we can write to the correct location
  				if (strLength < 0x20) {
  					headerSize = 1;
  				} else if (strLength < 0x100) {
  					headerSize = 2;
  				} else if (strLength < 0x10000) {
  					headerSize = 3;
  				} else {
  					headerSize = 5;
  				}
  				let maxBytes = strLength * 3;
  				if (position + maxBytes > safeEnd)
  					target = makeRoom(position + maxBytes);

  				if (strLength < 0x40 || !encodeUtf8) {
  					let i, c1, c2, strPosition = position + headerSize;
  					for (i = 0; i < strLength; i++) {
  						c1 = value.charCodeAt(i);
  						if (c1 < 0x80) {
  							target[strPosition++] = c1;
  						} else if (c1 < 0x800) {
  							target[strPosition++] = c1 >> 6 | 0xc0;
  							target[strPosition++] = c1 & 0x3f | 0x80;
  						} else if (
  							(c1 & 0xfc00) === 0xd800 &&
  							((c2 = value.charCodeAt(i + 1)) & 0xfc00) === 0xdc00
  						) {
  							c1 = 0x10000 + ((c1 & 0x03ff) << 10) + (c2 & 0x03ff);
  							i++;
  							target[strPosition++] = c1 >> 18 | 0xf0;
  							target[strPosition++] = c1 >> 12 & 0x3f | 0x80;
  							target[strPosition++] = c1 >> 6 & 0x3f | 0x80;
  							target[strPosition++] = c1 & 0x3f | 0x80;
  						} else {
  							target[strPosition++] = c1 >> 12 | 0xe0;
  							target[strPosition++] = c1 >> 6 & 0x3f | 0x80;
  							target[strPosition++] = c1 & 0x3f | 0x80;
  						}
  					}
  					length = strPosition - position - headerSize;
  				} else {
  					length = encodeUtf8(value, position + headerSize, maxBytes);
  				}

  				if (length < 0x18) {
  					target[position++] = 0x60 | length;
  				} else if (length < 0x100) {
  					if (headerSize < 2) {
  						target.copyWithin(position + 2, position + 1, position + 1 + length);
  					}
  					target[position++] = 0x78;
  					target[position++] = length;
  				} else if (length < 0x10000) {
  					if (headerSize < 3) {
  						target.copyWithin(position + 3, position + 2, position + 2 + length);
  					}
  					target[position++] = 0x79;
  					target[position++] = length >> 8;
  					target[position++] = length & 0xff;
  				} else {
  					if (headerSize < 5) {
  						target.copyWithin(position + 5, position + 3, position + 3 + length);
  					}
  					target[position++] = 0x7a;
  					targetView.setUint32(position, length);
  					position += 4;
  				}
  				position += length;
  			} else if (type === 'number') {
  				if (!this.alwaysUseFloat && value >>> 0 === value) {// positive integer, 32-bit or less
  					// positive uint
  					if (value < 0x18) {
  						target[position++] = value;
  					} else if (value < 0x100) {
  						target[position++] = 0x18;
  						target[position++] = value;
  					} else if (value < 0x10000) {
  						target[position++] = 0x19;
  						target[position++] = value >> 8;
  						target[position++] = value & 0xff;
  					} else {
  						target[position++] = 0x1a;
  						targetView.setUint32(position, value);
  						position += 4;
  					}
  				} else if (!this.alwaysUseFloat && value >> 0 === value) { // negative integer
  					if (value >= -0x18) {
  						target[position++] = 0x1f - value;
  					} else if (value >= -0x100) {
  						target[position++] = 0x38;
  						target[position++] = ~value;
  					} else if (value >= -0x10000) {
  						target[position++] = 0x39;
  						targetView.setUint16(position, ~value);
  						position += 2;
  					} else {
  						target[position++] = 0x3a;
  						targetView.setUint32(position, ~value);
  						position += 4;
  					}
  				} else {
  					let useFloat32;
  					if ((useFloat32 = this.useFloat32) > 0 && value < 0x100000000 && value >= -0x80000000) {
  						target[position++] = 0xfa;
  						targetView.setFloat32(position, value);
  						let xShifted;
  						if (useFloat32 < 4 ||
  								// this checks for rounding of numbers that were encoded in 32-bit float to nearest significant decimal digit that could be preserved
  								((xShifted = value * mult10[((target[position] & 0x7f) << 1) | (target[position + 1] >> 7)]) >> 0) === xShifted) {
  							position += 4;
  							return
  						} else
  							position--; // move back into position for writing a double
  					}
  					target[position++] = 0xfb;
  					targetView.setFloat64(position, value);
  					position += 8;
  				}
  			} else if (type === 'object') {
  				if (!value)
  					target[position++] = 0xf6;
  				else {
  					if (referenceMap) {
  						let referee = referenceMap.get(value);
  						if (referee) {
  							target[position++] = 0xd8;
  							target[position++] = 29; // http://cbor.schmorp.de/value-sharing
  							target[position++] = 0x19; // 16-bit uint
  							if (!referee.references) {
  								let idsToInsert = referenceMap.idsToInsert || (referenceMap.idsToInsert = []);
  								referee.references = [];
  								idsToInsert.push(referee);
  							}
  							referee.references.push(position - start);
  							position += 2; // TODO: also support 32-bit
  							return
  						} else 
  							referenceMap.set(value, { offset: position - start });
  					}
  					let constructor = value.constructor;
  					if (constructor === Object) {
  						writeObject(value, true);
  					} else if (constructor === Array) {
  						length = value.length;
  						if (length < 0x18) {
  							target[position++] = 0x80 | length;
  						} else {
  							writeArrayHeader(length);
  						}
  						for (let i = 0; i < length; i++) {
  							encode(value[i]);
  						}
  					} else if (constructor === Map) {
  						if (this.mapsAsObjects ? this.useTag259ForMaps !== false : this.useTag259ForMaps) {
  							// use Tag 259 (https://github.com/shanewholloway/js-cbor-codec/blob/master/docs/CBOR-259-spec--explicit-maps.md) for maps if the user wants it that way
  							target[position++] = 0xd9;
  							target[position++] = 1;
  							target[position++] = 3;
  						}
  						length = value.size;
  						if (length < 0x18) {
  							target[position++] = 0xa0 | length;
  						} else if (length < 0x100) {
  							target[position++] = 0xb8;
  							target[position++] = length;
  						} else if (length < 0x10000) {
  							target[position++] = 0xb9;
  							target[position++] = length >> 8;
  							target[position++] = length & 0xff;
  						} else {
  							target[position++] = 0xba;
  							targetView.setUint32(position, length);
  							position += 4;
  						}
  						if (encoder.keyMap) { 
  							for (let [ key, entryValue ] of value) {
  								encode(encoder.encodeKey(key));
  								encode(entryValue);
  							} 
  						} else { 
  							for (let [ key, entryValue ] of value) {
  								encode(key); 
  								encode(entryValue);
  							} 	
  						}
  					} else {
  						for (let i = 0, l = extensions.length; i < l; i++) {
  							let extensionClass = extensionClasses[i];
  							if (value instanceof extensionClass) {
  								let extension = extensions[i];
  								let tag = extension.tag;
  								if (tag == undefined)
  									tag = extension.getTag && extension.getTag.call(this, value);
  								if (tag < 0x18) {
  									target[position++] = 0xc0 | tag;
  								} else if (tag < 0x100) {
  									target[position++] = 0xd8;
  									target[position++] = tag;
  								} else if (tag < 0x10000) {
  									target[position++] = 0xd9;
  									target[position++] = tag >> 8;
  									target[position++] = tag & 0xff;
  								} else if (tag > -1) {
  									target[position++] = 0xda;
  									targetView.setUint32(position, tag);
  									position += 4;
  								} // else undefined, don't write tag
  								extension.encode.call(this, value, encode, makeRoom);
  								return
  							}
  						}
  						if (value[Symbol.iterator]) {
  							if (throwOnIterable) {
  								let error = new Error('Iterable should be serialized as iterator');
  								error.iteratorNotHandled = true;
  								throw error;
  							}
  							target[position++] = 0x9f; // indefinite length array
  							for (let entry of value) {
  								encode(entry);
  							}
  							target[position++] = 0xff; // stop-code
  							return
  						}
  						if (value[Symbol.asyncIterator] || isBlob(value)) {
  							let error = new Error('Iterable/blob should be serialized as iterator');
  							error.iteratorNotHandled = true;
  							throw error;
  						}
  						if (this.useToJSON && value.toJSON) {
  							const json = value.toJSON();
  							// if for some reason value.toJSON returns itself it'll loop forever
  							if (json !== value)
  								return encode(json)
  						}

  						// no extension found, write as object
  						writeObject(value, !value.hasOwnProperty); // if it doesn't have hasOwnProperty, don't do hasOwnProperty checks
  					}
  				}
  			} else if (type === 'boolean') {
  				target[position++] = value ? 0xf5 : 0xf4;
  			} else if (type === 'bigint') {
  				if (value < (BigInt(1)<<BigInt(64)) && value >= 0) {
  					// use an unsigned int as long as it fits
  					target[position++] = 0x1b;
  					targetView.setBigUint64(position, value);
  				} else if (value > -(BigInt(1)<<BigInt(64)) && value < 0) {
  					// if we can fit an unsigned int, use that
  					target[position++] = 0x3b;
  					targetView.setBigUint64(position, -value - BigInt(1));
  				} else {
  					// overflow
  					if (this.largeBigIntToFloat) {
  						target[position++] = 0xfb;
  						targetView.setFloat64(position, Number(value));
  					} else {
  						throw new RangeError(value + ' was too large to fit in CBOR 64-bit integer format, set largeBigIntToFloat to convert to float-64')
  					}
  				}
  				position += 8;
  			} else if (type === 'undefined') {
  				target[position++] = 0xf7;
  			} else {
  				throw new Error('Unknown type: ' + type)
  			}
  		};

  		const writeObject = this.useRecords === false ? this.variableMapSize ? (object) => {
  			// this method is slightly slower, but generates "preferred serialization" (optimally small for smaller objects)
  			let keys = Object.keys(object);
  			let vals = Object.values(object);
  			let length = keys.length;
  			if (length < 0x18) {
  				target[position++] = 0xa0 | length;
  			} else if (length < 0x100) {
  				target[position++] = 0xb8;
  				target[position++] = length;
  			} else if (length < 0x10000) {
  				target[position++] = 0xb9;
  				target[position++] = length >> 8;
  				target[position++] = length & 0xff;
  			} else {
  				target[position++] = 0xba;
  				targetView.setUint32(position, length);
  				position += 4;
  			}
  			if (encoder.keyMap) { 
  				for (let i = 0; i < length; i++) {
  					encode(encoder.encodeKey(keys[i]));
  					encode(vals[i]);
  				}
  			} else {
  				for (let i = 0; i < length; i++) {
  					encode(keys[i]);
  					encode(vals[i]);
  				}
  			}
  		} :
  		(object, safePrototype) => {
  			target[position++] = 0xb9; // always use map 16, so we can preallocate and set the length afterwards
  			let objectOffset = position - start;
  			position += 2;
  			let size = 0;
  			if (encoder.keyMap) { 
  				for (let key in object) if (safePrototype || object.hasOwnProperty(key)) {
  					encode(encoder.encodeKey(key));
  					encode(object[key]);
  					size++;
  				}
  			} else { 
  				for (let key in object) if (safePrototype || object.hasOwnProperty(key)) {
  						encode(key);
  						encode(object[key]);
  					size++;
  				}
  			}
  			target[objectOffset++ + start] = size >> 8;
  			target[objectOffset + start] = size & 0xff;
  		} :
  		(object, safePrototype) => {
  			let nextTransition, transition = structures.transitions || (structures.transitions = Object.create(null));
  			let newTransitions = 0;
  			let length = 0;
  			let parentRecordId;
  			let keys;
  			if (this.keyMap) {
  				keys = Object.keys(object).map(k => this.encodeKey(k));
  				length = keys.length;
  				for (let i = 0; i < length; i++) {
  					let key = keys[i];
  					nextTransition = transition[key];
  					if (!nextTransition) {
  						nextTransition = transition[key] = Object.create(null);
  						newTransitions++;
  					}
  					transition = nextTransition;
  				}				
  			} else {
  				for (let key in object) if (safePrototype || object.hasOwnProperty(key)) {
  					nextTransition = transition[key];
  					if (!nextTransition) {
  						if (transition[RECORD_SYMBOL] & 0x100000) {// this indicates it is a brancheable/extendable terminal node, so we will use this record id and extend it
  							parentRecordId = transition[RECORD_SYMBOL] & 0xffff;
  						}
  						nextTransition = transition[key] = Object.create(null);
  						newTransitions++;
  					}
  					transition = nextTransition;
  					length++;
  				}
  			}
  			let recordId = transition[RECORD_SYMBOL];
  			if (recordId !== undefined) {
  				recordId &= 0xffff;
  				target[position++] = 0xd9;
  				target[position++] = (recordId >> 8) | 0xe0;
  				target[position++] = recordId & 0xff;
  			} else {
  				if (!keys)
  					keys = transition.__keys__ || (transition.__keys__ = Object.keys(object));
  				if (parentRecordId === undefined) {
  					recordId = structures.nextId++;
  					if (!recordId) {
  						recordId = 0;
  						structures.nextId = 1;
  					}
  					if (recordId >= MAX_STRUCTURES) {// cycle back around
  						structures.nextId = (recordId = maxSharedStructures) + 1;
  					}
  				} else {
  					recordId = parentRecordId;
  				}
  				structures[recordId] = keys;
  				if (recordId < maxSharedStructures) {
  					target[position++] = 0xd9;
  					target[position++] = (recordId >> 8) | 0xe0;
  					target[position++] = recordId & 0xff;
  					transition = structures.transitions;
  					for (let i = 0; i < length; i++) {
  						if (transition[RECORD_SYMBOL] === undefined || (transition[RECORD_SYMBOL] & 0x100000))
  							transition[RECORD_SYMBOL] = recordId;
  						transition = transition[keys[i]];
  					}
  					transition[RECORD_SYMBOL] = recordId | 0x100000; // indicates it is a extendable terminal
  					hasSharedUpdate = true;
  				} else {
  					transition[RECORD_SYMBOL] = recordId;
  					targetView.setUint32(position, 0xd9dfff00); // tag two byte, then record definition id
  					position += 3;
  					if (newTransitions)
  						transitionsCount += serializationsSinceTransitionRebuild * newTransitions;
  					// record the removal of the id, we can maintain our shared structure
  					if (recordIdsToRemove.length >= MAX_STRUCTURES - maxSharedStructures)
  						recordIdsToRemove.shift()[RECORD_SYMBOL] = undefined; // we are cycling back through, and have to remove old ones
  					recordIdsToRemove.push(transition);
  					writeArrayHeader(length + 2);
  					encode(0xe000 + recordId);
  					encode(keys);
  					if (safePrototype === null) return; // special exit for iterator
  					for (let key in object)
  						if (safePrototype || object.hasOwnProperty(key))
  							encode(object[key]);
  					return
  				}
  			}
  			if (length < 0x18) { // write the array header
  				target[position++] = 0x80 | length;
  			} else {
  				writeArrayHeader(length);
  			}
  			if (safePrototype === null) return; // special exit for iterator
  			for (let key in object)
  				if (safePrototype || object.hasOwnProperty(key))
  					encode(object[key]);
  		};
  		const makeRoom = (end) => {
  			let newSize;
  			if (end > 0x1000000) {
  				// special handling for really large buffers
  				if ((end - start) > MAX_BUFFER_SIZE)
  					throw new Error('Encoded buffer would be larger than maximum buffer size')
  				newSize = Math.min(MAX_BUFFER_SIZE,
  					Math.round(Math.max((end - start) * (end > 0x4000000 ? 1.25 : 2), 0x400000) / 0x1000) * 0x1000);
  			} else // faster handling for smaller buffers
  				newSize = ((Math.max((end - start) << 2, target.length - 1) >> 12) + 1) << 12;
  			let newBuffer = new ByteArrayAllocate(newSize);
  			targetView = new DataView(newBuffer.buffer, 0, newSize);
  			if (target.copy)
  				target.copy(newBuffer, 0, start, end);
  			else
  				newBuffer.set(target.slice(start, end));
  			position -= start;
  			start = 0;
  			safeEnd = newBuffer.length - 10;
  			return target = newBuffer
  		};
  		let chunkThreshold = 100;
  		let continuedChunkThreshold = 1000;
  		this.encodeAsIterable = function(value, options) {
  			return startEncoding(value, options, encodeObjectAsIterable);
  		};
  		this.encodeAsAsyncIterable = function(value, options) {
  			return startEncoding(value, options, encodeObjectAsAsyncIterable);
  		};

  		function* encodeObjectAsIterable(object, iterateProperties, finalIterable) {
  			let constructor = object.constructor;
  			if (constructor === Object) {
  				let useRecords = encoder.useRecords !== false;
  				if (useRecords)
  					writeObject(object, null); // write the record identifier
  				else
  					writeEntityLength(Object.keys(object).length, 0xa0);
  				for (let key in object) {
  					let value = object[key];
  					if (!useRecords) encode(key);
  					if (value && typeof value === 'object') {
  						if (iterateProperties[key])
  							yield* encodeObjectAsIterable(value, iterateProperties[key]);
  						else
  							yield* tryEncode(value, iterateProperties, key);
  					} else encode(value);
  				}
  			} else if (constructor === Array) {
  				let length = object.length;
  				writeArrayHeader(length);
  				for (let i = 0; i < length; i++) {
  					let value = object[i];
  					if (value && (typeof value === 'object' || position - start > chunkThreshold)) {
  						if (iterateProperties.element)
  							yield* encodeObjectAsIterable(value, iterateProperties.element);
  						else
  							yield* tryEncode(value, iterateProperties, 'element');
  					} else encode(value);
  				}
  			} else if (object[Symbol.iterator]) {
  				target[position++] = 0x9f; // start indefinite array
  				for (let value of object) {
  					if (value && (typeof value === 'object' || position - start > chunkThreshold)) {
  						if (iterateProperties.element)
  							yield* encodeObjectAsIterable(value, iterateProperties.element);
  						else
  							yield* tryEncode(value, iterateProperties, 'element');
  					} else encode(value);
  				}
  				target[position++] = 0xff; // stop byte
  			} else if (isBlob(object)){
  				writeEntityLength(object.size, 0x40); // encode as binary data
  				yield target.subarray(start, position);
  				yield object; // directly return blobs, they have to be encoded asynchronously
  				restartEncoding();
  			} else if (object[Symbol.asyncIterator]) {
  				target[position++] = 0x9f; // start indefinite array
  				yield target.subarray(start, position);
  				yield object; // directly return async iterators, they have to be encoded asynchronously
  				restartEncoding();
  				target[position++] = 0xff; // stop byte
  			} else {
  				encode(object);
  			}
  			if (finalIterable && position > start) yield target.subarray(start, position);
  			else if (position - start > chunkThreshold) {
  				yield target.subarray(start, position);
  				restartEncoding();
  			}
  		}
  		function* tryEncode(value, iterateProperties, key) {
  			let restart = position - start;
  			try {
  				encode(value);
  				if (position - start > chunkThreshold) {
  					yield target.subarray(start, position);
  					restartEncoding();
  				}
  			} catch (error) {
  				if (error.iteratorNotHandled) {
  					iterateProperties[key] = {};
  					position = start + restart; // restart our position so we don't have partial data from last encode
  					yield* encodeObjectAsIterable.call(this, value, iterateProperties[key]);
  				} else throw error;
  			}
  		}
  		function restartEncoding() {
  			chunkThreshold = continuedChunkThreshold;
  			encoder.encode(null, THROW_ON_ITERABLE); // restart encoding
  		}
  		function startEncoding(value, options, encodeIterable) {
  			if (options && options.chunkThreshold) // explicitly specified chunk sizes
  				chunkThreshold = continuedChunkThreshold = options.chunkThreshold;
  			else // we start with a smaller threshold to get initial bytes sent quickly
  				chunkThreshold = 100;
  			if (value && typeof value === 'object') {
  				encoder.encode(null, THROW_ON_ITERABLE); // start encoding
  				return encodeIterable(value, encoder.iterateProperties || (encoder.iterateProperties = {}), true);
  			}
  			return [encoder.encode(value)];
  		}

  		async function* encodeObjectAsAsyncIterable(value, iterateProperties) {
  			for (let encodedValue of encodeObjectAsIterable(value, iterateProperties, true)) {
  				let constructor = encodedValue.constructor;
  				if (constructor === ByteArray || constructor === Uint8Array)
  					yield encodedValue;
  				else if (isBlob(encodedValue)) {
  					let reader = encodedValue.stream().getReader();
  					let next;
  					while (!(next = await reader.read()).done) {
  						yield next.value;
  					}
  				} else if (encodedValue[Symbol.asyncIterator]) {
  					for await (let asyncValue of encodedValue) {
  						restartEncoding();
  						if (asyncValue)
  							yield* encodeObjectAsAsyncIterable(asyncValue, iterateProperties.async || (iterateProperties.async = {}));
  						else yield encoder.encode(asyncValue);
  					}
  				} else {
  					yield encodedValue;
  				}
  			}
  		}
  	}
  	useBuffer(buffer) {
  		// this means we are finished using our own buffer and we can write over it safely
  		target = buffer;
  		targetView = new DataView(target.buffer, target.byteOffset, target.byteLength);
  		position = 0;
  	}
  	clearSharedData() {
  		if (this.structures)
  			this.structures = [];
  		if (this.sharedValues)
  			this.sharedValues = undefined;
  	}
  	updateSharedData() {
  		let lastVersion = this.sharedVersion || 0;
  		this.sharedVersion = lastVersion + 1;
  		let structuresCopy = this.structures.slice(0);
  		let sharedData = new SharedData(structuresCopy, this.sharedValues, this.sharedVersion);
  		let saveResults = this.saveShared(sharedData,
  				existingShared => (existingShared && existingShared.version || 0) == lastVersion);
  		if (saveResults === false) {
  			// get updated structures and try again if the update failed
  			sharedData = this.getShared() || {};
  			this.structures = sharedData.structures || [];
  			this.sharedValues = sharedData.packedValues;
  			this.sharedVersion = sharedData.version;
  			this.structures.nextId = this.structures.length;
  		} else {
  			// restore structures
  			structuresCopy.forEach((structure, i) => this.structures[i] = structure);
  		}
  		// saveShared may fail to write and reload, or may have reloaded to check compatibility and overwrite saved data, either way load the correct shared data
  		return saveResults
  	}
  }
  function writeEntityLength(length, majorValue) {
  	if (length < 0x18)
  		target[position++] = majorValue | length;
  	else if (length < 0x100) {
  		target[position++] = majorValue | 0x18;
  		target[position++] = length;
  	} else if (length < 0x10000) {
  		target[position++] = majorValue | 0x19;
  		target[position++] = length >> 8;
  		target[position++] = length & 0xff;
  	} else {
  		target[position++] = majorValue | 0x1a;
  		targetView.setUint32(position, length);
  		position += 4;
  	}

  }
  class SharedData {
  	constructor(structures, values, version) {
  		this.structures = structures;
  		this.packedValues = values;
  		this.version = version;
  	}
  }

  function writeArrayHeader(length) {
  	if (length < 0x18)
  		target[position++] = 0x80 | length;
  	else if (length < 0x100) {
  		target[position++] = 0x98;
  		target[position++] = length;
  	} else if (length < 0x10000) {
  		target[position++] = 0x99;
  		target[position++] = length >> 8;
  		target[position++] = length & 0xff;
  	} else {
  		target[position++] = 0x9a;
  		targetView.setUint32(position, length);
  		position += 4;
  	}
  }

  const BlobConstructor = typeof Blob === 'undefined' ? function(){} : Blob;
  function isBlob(object) {
  	if (object instanceof BlobConstructor)
  		return true;
  	let tag = object[Symbol.toStringTag];
  	return tag === 'Blob' || tag === 'File';
  }
  function findRepetitiveStrings(value, packedValues) {
  	switch(typeof value) {
  		case 'string':
  			if (value.length > 3) {
  				if (packedValues.objectMap[value] > -1 || packedValues.values.length >= packedValues.maxValues)
  					return
  				let packedStatus = packedValues.get(value);
  				if (packedStatus) {
  					if (++packedStatus.count == 2) {
  						packedValues.values.push(value);
  					}
  				} else {
  					packedValues.set(value, {
  						count: 1,
  					});
  					if (packedValues.samplingPackedValues) {
  						let status = packedValues.samplingPackedValues.get(value);
  						if (status)
  							status.count++;
  						else
  							packedValues.samplingPackedValues.set(value, {
  								count: 1,
  							});
  					}
  				}
  			}
  			break
  		case 'object':
  			if (value) {
  				if (value instanceof Array) {
  					for (let i = 0, l = value.length; i < l; i++) {
  						findRepetitiveStrings(value[i], packedValues);
  					}

  				} else {
  					let includeKeys = !packedValues.encoder.useRecords;
  					for (var key in value) {
  						if (value.hasOwnProperty(key)) {
  							if (includeKeys)
  								findRepetitiveStrings(key, packedValues);
  							findRepetitiveStrings(value[key], packedValues);
  						}
  					}
  				}
  			}
  			break
  		case 'function': console.log(value);
  	}
  }
  const isLittleEndianMachine = new Uint8Array(new Uint16Array([1]).buffer)[0] == 1;
  extensionClasses = [ Date, Set, Error, RegExp, Tag, ArrayBuffer,
  	Uint8Array, Uint8ClampedArray, Uint16Array, Uint32Array,
  	typeof BigUint64Array == 'undefined' ? function() {} : BigUint64Array, Int8Array, Int16Array, Int32Array,
  	typeof BigInt64Array == 'undefined' ? function() {} : BigInt64Array,
  	Float32Array, Float64Array, SharedData ];

  //Object.getPrototypeOf(Uint8Array.prototype).constructor /*TypedArray*/
  extensions = [{ // Date
  	tag: 1,
  	encode(date, encode) {
  		let seconds = date.getTime() / 1000;
  		if ((this.useTimestamp32 || date.getMilliseconds() === 0) && seconds >= 0 && seconds < 0x100000000) {
  			// Timestamp 32
  			target[position++] = 0x1a;
  			targetView.setUint32(position, seconds);
  			position += 4;
  		} else {
  			// Timestamp float64
  			target[position++] = 0xfb;
  			targetView.setFloat64(position, seconds);
  			position += 8;
  		}
  	}
  }, { // Set
  	tag: 258, // https://github.com/input-output-hk/cbor-sets-spec/blob/master/CBOR_SETS.md
  	encode(set, encode) {
  		let array = Array.from(set);
  		encode(array);
  	}
  }, { // Error
  	tag: 27, // http://cbor.schmorp.de/generic-object
  	encode(error, encode) {
  		encode([ error.name, error.message ]);
  	}
  }, { // RegExp
  	tag: 27, // http://cbor.schmorp.de/generic-object
  	encode(regex, encode) {
  		encode([ 'RegExp', regex.source, regex.flags ]);
  	}
  }, { // Tag
  	getTag(tag) {
  		return tag.tag
  	},
  	encode(tag, encode) {
  		encode(tag.value);
  	}
  }, { // ArrayBuffer
  	encode(arrayBuffer, encode, makeRoom) {
  		writeBuffer(arrayBuffer, makeRoom);
  	}
  }, { // Uint8Array
  	getTag(typedArray) {
  		if (typedArray.constructor === Uint8Array) {
  			if (this.tagUint8Array || hasNodeBuffer && this.tagUint8Array !== false)
  				return 64;
  		} // else no tag
  	},
  	encode(typedArray, encode, makeRoom) {
  		writeBuffer(typedArray, makeRoom);
  	}
  },
  	typedArrayEncoder(68, 1),
  	typedArrayEncoder(69, 2),
  	typedArrayEncoder(70, 4),
  	typedArrayEncoder(71, 8),
  	typedArrayEncoder(72, 1),
  	typedArrayEncoder(77, 2),
  	typedArrayEncoder(78, 4),
  	typedArrayEncoder(79, 8),
  	typedArrayEncoder(85, 4),
  	typedArrayEncoder(86, 8),
  {
  	encode(sharedData, encode) { // write SharedData
  		let packedValues = sharedData.packedValues || [];
  		let sharedStructures = sharedData.structures || [];
  		if (packedValues.values.length > 0) {
  			target[position++] = 0xd8; // one-byte tag
  			target[position++] = 51; // tag 51 for packed shared structures https://www.potaroo.net/ietf/ids/draft-ietf-cbor-packed-03.txt
  			writeArrayHeader(4);
  			let valuesArray = packedValues.values;
  			encode(valuesArray);
  			writeArrayHeader(0); // prefixes
  			writeArrayHeader(0); // suffixes
  			packedObjectMap = Object.create(sharedPackedObjectMap || null);
  			for (let i = 0, l = valuesArray.length; i < l; i++) {
  				packedObjectMap[valuesArray[i]] = i;
  			}
  		}
  		if (sharedStructures) {
  			targetView.setUint32(position, 0xd9dffe00);
  			position += 3;
  			let definitions = sharedStructures.slice(0);
  			definitions.unshift(0xe000);
  			definitions.push(new Tag(sharedData.version, 0x53687264));
  			encode(definitions);
  		} else
  			encode(new Tag(sharedData.version, 0x53687264));
  		}
  	}];
  function typedArrayEncoder(tag, size) {
  	if (!isLittleEndianMachine && size > 1)
  		tag -= 4; // the big endian equivalents are 4 less
  	return {
  		tag: tag,
  		encode: function writeExtBuffer(typedArray, encode) {
  			let length = typedArray.byteLength;
  			let offset = typedArray.byteOffset || 0;
  			let buffer = typedArray.buffer || typedArray;
  			encode(hasNodeBuffer ? Buffer.from(buffer, offset, length) :
  				new Uint8Array(buffer, offset, length));
  		}
  	}
  }
  function writeBuffer(buffer, makeRoom) {
  	let length = buffer.byteLength;
  	if (length < 0x18) {
  		target[position++] = 0x40 + length;
  	} else if (length < 0x100) {
  		target[position++] = 0x58;
  		target[position++] = length;
  	} else if (length < 0x10000) {
  		target[position++] = 0x59;
  		target[position++] = length >> 8;
  		target[position++] = length & 0xff;
  	} else {
  		target[position++] = 0x5a;
  		targetView.setUint32(position, length);
  		position += 4;
  	}
  	if (position + length >= target.length) {
  		makeRoom(position + length);
  	}
  	// if it is already a typed array (has an ArrayBuffer), use that, but if it is an ArrayBuffer itself,
  	// must wrap it to set it.
  	target.set(buffer.buffer ? buffer : new Uint8Array(buffer), position);
  	position += length;
  }

  function insertIds(serialized, idsToInsert) {
  	// insert the ids that need to be referenced for structured clones
  	let nextId;
  	let distanceToMove = idsToInsert.length * 2;
  	let lastEnd = serialized.length - distanceToMove;
  	idsToInsert.sort((a, b) => a.offset > b.offset ? 1 : -1);
  	for (let id = 0; id < idsToInsert.length; id++) {
  		let referee = idsToInsert[id];
  		referee.id = id;
  		for (let position of referee.references) {
  			serialized[position++] = id >> 8;
  			serialized[position] = id & 0xff;
  		}
  	}
  	while (nextId = idsToInsert.pop()) {
  		let offset = nextId.offset;
  		serialized.copyWithin(offset + distanceToMove, offset, lastEnd);
  		distanceToMove -= 2;
  		let position = offset + distanceToMove;
  		serialized[position++] = 0xd8;
  		serialized[position++] = 28; // http://cbor.schmorp.de/value-sharing
  		lastEnd = offset;
  	}
  	return serialized
  }
  function writeBundles(start, encode) {
  	targetView.setUint32(bundledStrings.position + start, position - bundledStrings.position - start + 1); // the offset to bundle
  	let writeStrings = bundledStrings;
  	bundledStrings = null;
  	encode(writeStrings[0]);
  	encode(writeStrings[1]);
  }

  function addExtension(extension) {
  	if (extension.Class) {
  		if (!extension.encode)
  			throw new Error('Extension has no encode function')
  		extensionClasses.unshift(extension.Class);
  		extensions.unshift(extension);
  	}
  	addExtension$1(extension);
  }
  let defaultEncoder = new Encoder({ useRecords: false });
  defaultEncoder.encode;
  defaultEncoder.encodeAsIterable;
  defaultEncoder.encodeAsAsyncIterable;
  const REUSE_BUFFER_MODE = 512;
  const RESET_BUFFER_MODE = 1024;
  const THROW_ON_ITERABLE = 2048;

  var lzjbPack = {};

  /**@license
   *
   * No Dependency fast and small LZJB Compression for Browser and Node
   *
   * Copyright (c) 2021 Jakub T. Jankiewicz https://jcubic.pl/me
   * Released under BSD-3-Clause License
   *
   * build: Wed, 27 Oct 2021 10:43:10 GMT
   */

  Object.defineProperty(lzjbPack, '__esModule', { value: true });

  /*
   * source https://github.com/copy/jslzjb-k
   * Based on jslzjb: https://code.google.com/p/jslzjb/
   * Heavily modified for speed
   */
  // Constants was used for compress/decompress function.
  const
  /** @const */ NBBY = 8,
        /** @const */ MATCH_BITS = 6,
        /** @const */ MATCH_MIN = 3,
        /** @const */ MATCH_MAX = ((1 << MATCH_BITS) + (MATCH_MIN - 1)),
        /** @const */ OFFSET_MASK = ((1 << (16 - MATCH_BITS)) - 1),
        /** @const */ LEMPEL_SIZE = 256;

  /**
    * Because of weak of javascript's natural, many compression algorithm
    * become useless in javascript implementation. The main problem is
    * performance, even the simple Huffman, LZ77/78 algorithm will take many
    * many time to operate. We use LZJB algorithm to do that, it suprisingly
    * fulfills our requirement to compress string fastly and efficiently.
    *
    * Our implementation is based on
    * http://src.opensolaris.org/source/raw/onnv/onnv-gate/
    * usr/src/uts/common/os/compress.c
    * It is licensed under CDDL.
    *
    * Compress byte array using fast and efficient algorithm.
    *
    * @param {Uint8Array} sstart  The buffer to compress
    * @param {Uint8Array} dstart  The buffer to write into
    * @return {number} compressed length (number of bytes written to the
    *                  output buffer). May be bigger than the size of the
    *                  output buffer, in which case some bytes are lost
    */
  function compress(sstart, dstart) {
      var slen = 0,
          src = 0,
          dst = 0,
          cpy = 0,
          copymap = 0,
          copymask = 1 << (NBBY - 1),
          mlen = 0,
          offset = 0,
          hp = 0,
          lempel = new Int32Array(LEMPEL_SIZE),
          i = 0;

      // Initialize lempel array.
      for(i = 0; i < LEMPEL_SIZE; i++) {
          lempel[i] = -858993460;
      }

      slen = sstart.length;

      while (src < slen) {
          if ((copymask <<= 1) == (1 << NBBY)) {
              copymask = 1;
              copymap = dst;
              dstart[dst++] = 0;
          }

          if (src > slen - MATCH_MAX) {
              dstart[dst++] = sstart[src++];
              continue;
          }

          hp = ((sstart[src] + 13) ^
                (sstart[src + 1] - 13) ^
                sstart[src + 2]) &
              (LEMPEL_SIZE - 1);

          offset = (src - lempel[hp]) & OFFSET_MASK;
          lempel[hp] = src;
          cpy = src - offset;

          if (cpy >= 0 && cpy != src &&
              sstart[src] == sstart[cpy] &&
              sstart[src + 1] == sstart[cpy + 1] &&
              sstart[src + 2] == sstart[cpy + 2]) {
              dstart[copymap] |= copymask;
              for (mlen = MATCH_MIN; mlen < MATCH_MAX; mlen++)
                  if (sstart[src + mlen] != sstart[cpy + mlen])
                      break;
              dstart[dst++] = ((mlen - MATCH_MIN) << (NBBY - MATCH_BITS)) |
                  (offset >> NBBY);
              dstart[dst++] = offset;
              src += mlen;
          } else {
              dstart[dst++] = sstart[src++];
          }
      }

      console.assert(sstart.length >= src);

      return dst;
  }

  /**
    * Our implementation is based on
    * http://src.opensolaris.org/source/raw/onnv/onnv-gate/
    * usr/src/uts/common/os/compress.c
    * It is licensed under CDDL.
    *
    * Decompress byte array using fast and efficient algorithm.
    *
    * @param {Uint8Array} sstart  The buffer to decompress
    * @param {number} slen  compressed length
    * @param {Uint8Array} dstart  The buffer to write into
    * @return {number} decompressed length
    */
  function decompress(sstart, slen, dstart) {
      slen = slen | 0;

      var src = 0,
          dst = 0,
          cpy = 0,
          copymap = 0,
          copymask = 1 << (NBBY - 1 | 0),
          mlen = 0,
          offset = 0;

      //var avg_mlen = [];

      while (src < slen) {
          if ((copymask <<= 1) === (1 << NBBY)) {
              copymask = 1;
              copymap = sstart[src];
              src = src + 1 | 0;
          }

          if (copymap & copymask) {
              mlen = (sstart[src] >> (NBBY - MATCH_BITS | 0)) + MATCH_MIN | 0;
              offset = ((sstart[src] << NBBY) | sstart[src + 1 | 0]) & OFFSET_MASK;
              src = src + 2 | 0;

              cpy = dst - offset | 0;
              //if (cpy >= 0)
              {
                  //console.log(mlen);
                  //avg_mlen.push(mlen);

                  //dstart.set(dstart.subarray(cpy, cpy + mlen | 0), dst);
                  //dst = dst + mlen | 0;
                  //cpy = cpy + mlen | 0;

                  //mlen = mlen - 1 | 0;
                  while (mlen > 4) {
                      dstart[dst] = dstart[cpy];
                      dst = dst + 1 | 0;
                      cpy = cpy + 1 | 0;

                      dstart[dst] = dstart[cpy];
                      dst = dst + 1 | 0;
                      cpy = cpy + 1 | 0;

                      dstart[dst] = dstart[cpy];
                      dst = dst + 1 | 0;
                      cpy = cpy + 1 | 0;

                      dstart[dst] = dstart[cpy];
                      dst = dst + 1 | 0;
                      cpy = cpy + 1 | 0;

                      mlen = mlen - 4 | 0;
                  }

                  while (mlen > 0) {
                      dstart[dst] = dstart[cpy];
                      dst = dst + 1 | 0;
                      cpy = cpy + 1 | 0;
                      mlen = mlen - 1 | 0;
                  }
              }
              //else
              //{
              //    /*
              //     * offset before start of destination buffer
              //     * indicates corrupt source data
              //     */
              //    console.warn("possibly corrupt data");
              //    return dstart;
              //}
          } else {
              dstart[dst] = sstart[src];
              dst = dst + 1 | 0;
              src = src + 1 | 0;
          }
      }

      //console.log(avg_mlen.reduce(function(a, x) { return a + x; }, 0) / avg_mlen.length);

      //console.assert(dstart.length >= dst);
      //console.assert(sstart.length >= src);

      return dst;
  }


  function encode_magic$1() {
      const encoder = new TextEncoder('utf-8');
      return encoder.encode(MAGIC_STRING);
  }

  const MAGIC_STRING = '@lzjb';
  const MAGIC = encode_magic$1();

  function merge_uint8_array$1(...args) {
      if (args.length > 1) {
          const len = args.reduce((acc, arr) => acc + arr.length, 0);
          const result = new Uint8Array(len);
          let offset = 0;
          args.forEach(item => {
              result.set(item, offset);
              offset += item.length;
          });
          return result;
      } else if (args.length) {
          return args[0];
      }
  }

  function number_to_bytes(number) {
      const len = Math.ceil(Math.log2(number) / 8);
      const byteArray = new Uint8Array(len);

      for (let index = 0; index < byteArray.length; index ++ ) {
          const byte = number & 0xff;
          byteArray[index] = byte;
          number = (number - byte) / 256;
      }

      return byteArray;
  }

  function bytes_to_number(byteArray) {
      let result = 0;
      for (let i = byteArray.length - 1; i >= 0; i--) {
          result = (result * 256) + byteArray[i];
      }

      return result;
  }

  function pack(input, { magic = true } = {}) {
      const out = new Uint8Array(Math.max(input.length * 1.5 | 0, 16 * 1024));
      const len = compress(input, out);
      const len_array = number_to_bytes(input.length);
      const payload = [
          Uint8Array.of(len_array.length),
          len_array,
          out.slice(0, len)
      ];
      if (magic) {
          payload.unshift(MAGIC);
      }
      return merge_uint8_array$1(...payload);
  }

  function unpack(input, { magic = true } = {}) {
      if (magic) {
          const decoder = new TextDecoder('utf-8');
          const magic_prefix = decoder.decode(input.slice(0, MAGIC.length));
          if (magic_prefix !== MAGIC_STRING) {
              throw new Error('Invalid magic value');
          }
      }
      const magic_length = magic ? MAGIC.length : 0;
      const size = input[magic_length];
      const start = magic_length + 1;
      const end = magic_length + size + 1;
      const len = bytes_to_number(input.slice(start, end));
      input = input.slice(end);
      const out = new Uint8Array(len);
      decompress(input, input.length, out);
      return out;
  }

  var pack_1 = lzjbPack.pack = pack;
  var unpack_1 = lzjbPack.unpack = unpack;

  function unfetch(e,n){return n=n||{},new Promise(function(t,r){var s=new XMLHttpRequest,o=[],u=[],i={},a=function(){return {ok:2==(s.status/100|0),statusText:s.statusText,status:s.status,url:s.responseURL,text:function(){return Promise.resolve(s.responseText)},json:function(){return Promise.resolve(s.responseText).then(JSON.parse)},blob:function(){return Promise.resolve(new Blob([s.response]))},clone:a,headers:{keys:function(){return o},entries:function(){return u},get:function(e){return i[e.toLowerCase()]},has:function(e){return e.toLowerCase()in i}}}};for(var l in s.open(n.method||"get",e,!0),s.onload=function(){s.getAllResponseHeaders().replace(/^(.*?):[^\S\n]*([\s\S]*?)$/gm,function(e,n,t){o.push(n=n.toLowerCase()),u.push([n,t]),i[n]=i[n]?i[n]+","+t:t;}),t(a());},s.onerror=r,s.withCredentials="include"==n.credentials,n.headers)s.setRequestHeader(l,n.headers[l]);s.send(n.body||null);})}

  /**@license
   *   __ __                          __
   *  / / \ \       _    _  ___  ___  \ \
   * | |   \ \     | |  | || . \/ __>  | |
   * | |    > \    | |_ | ||  _/\__ \  | |
   * | |   / ^ \   |___||_||_|  <___/  | |
   *  \_\ /_/ \_\                     /_/ v. DEV
   *
   * LIPS is Pretty Simple - Scheme based Powerful LISP in JavaScript
   *
   * Copyright (c) 2018-2023 Jakub T. Jankiewicz <https://jcubic.pl/me>
   * Released under the MIT license
   *
   * includes:
   *
   * unfetch by Jason Miller (@developit) MIT License
   *
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
   * dist file generated by Babel includes code
   * Copyright (c) 2014-present, Facebook, Inc.
   * released under MIT license
   *
   * build: Mon, 15 Jan 2024 12:39:38 +0000
   */
  var _excluded = ["token"],
    _excluded2 = ["env"],
    _excluded3 = ["stderr", "stdin", "stdout", "command_line"],
    _excluded4 = ["use_dynamic"],
    _excluded5 = ["use_dynamic"],
    _excluded6 = ["env", "dynamic_env", "use_dynamic", "error"];
  function _classPrivateFieldInitSpec(obj, privateMap, value) { _checkPrivateRedeclaration(obj, privateMap); privateMap.set(obj, value); }
  function _checkPrivateRedeclaration(obj, privateCollection) { if (privateCollection.has(obj)) { throw new TypeError("Cannot initialize the same private elements twice on an object"); } }
  function _createSuper(Derived) { var hasNativeReflectConstruct = _isNativeReflectConstruct(); return function _createSuperInternal() { var Super = _getPrototypeOf(Derived), result; if (hasNativeReflectConstruct) { var NewTarget = _getPrototypeOf(this).constructor; result = Reflect.construct(Super, arguments, NewTarget); } else { result = Super.apply(this, arguments); } return _possibleConstructorReturn(this, result); }; }
  function _isNativeReflectConstruct() { if (typeof Reflect === "undefined" || !Reflect.construct) return false; if (Reflect.construct.sham) return false; if (typeof Proxy === "function") return true; try { Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function () {})); return true; } catch (e) { return false; } }
  function _createForOfIteratorHelper(o, allowArrayLike) { var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"]; if (!it) { if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") { if (it) o = it; var i = 0; var F = function F() {}; return { s: F, n: function n() { if (i >= o.length) return { done: true }; return { done: false, value: o[i++] }; }, e: function e(_e) { throw _e; }, f: F }; } throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); } var normalCompletion = true, didErr = false, err; return { s: function s() { it = it.call(o); }, n: function n() { var step = it.next(); normalCompletion = step.done; return step; }, e: function e(_e2) { didErr = true; err = _e2; }, f: function f() { try { if (!normalCompletion && it["return"] != null) it["return"](); } finally { if (didErr) throw err; } } }; }
  function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }
  function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }
  function ownKeys(object, enumerableOnly) { var keys = Object.keys(object); if (Object.getOwnPropertySymbols) { var symbols = Object.getOwnPropertySymbols(object); enumerableOnly && (symbols = symbols.filter(function (sym) { return Object.getOwnPropertyDescriptor(object, sym).enumerable; })), keys.push.apply(keys, symbols); } return keys; }
  function _objectSpread(target) { for (var i = 1; i < arguments.length; i++) { var source = null != arguments[i] ? arguments[i] : {}; i % 2 ? ownKeys(Object(source), !0).forEach(function (key) { _defineProperty(target, key, source[key]); }) : Object.getOwnPropertyDescriptors ? Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)) : ownKeys(Object(source)).forEach(function (key) { Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key)); }); } return target; }
  function _asyncIterator(iterable) { var method, async, sync, retry = 2; for ("undefined" != typeof Symbol && (async = Symbol.asyncIterator, sync = Symbol.iterator); retry--;) { if (async && null != (method = iterable[async])) return method.call(iterable); if (sync && null != (method = iterable[sync])) return new AsyncFromSyncIterator(method.call(iterable)); async = "@@asyncIterator", sync = "@@iterator"; } throw new TypeError("Object is not async iterable"); }
  function AsyncFromSyncIterator(s) { function AsyncFromSyncIteratorContinuation(r) { if (Object(r) !== r) return Promise.reject(new TypeError(r + " is not an object.")); var done = r.done; return Promise.resolve(r.value).then(function (value) { return { value: value, done: done }; }); } return AsyncFromSyncIterator = function AsyncFromSyncIterator(s) { this.s = s, this.n = s.next; }, AsyncFromSyncIterator.prototype = { s: null, n: null, next: function next() { return AsyncFromSyncIteratorContinuation(this.n.apply(this.s, arguments)); }, "return": function _return(value) { var ret = this.s["return"]; return void 0 === ret ? Promise.resolve({ value: value, done: !0 }) : AsyncFromSyncIteratorContinuation(ret.apply(this.s, arguments)); }, "throw": function _throw(value) { var thr = this.s["return"]; return void 0 === thr ? Promise.reject(value) : AsyncFromSyncIteratorContinuation(thr.apply(this.s, arguments)); } }, new AsyncFromSyncIterator(s); }
  var root = typeof global !== 'undefined' ? global : self;

  /* c8 ignore next 3 */
  if (!root.fetch) {
    root.fetch = unfetch;
  }

  // -------------------------------------------------------------------------
  // :: typechecking maps
  // -------------------------------------------------------------------------
  var type_mapping = {
    'pair': Pair,
    'symbol': LSymbol,
    'number': LNumber,
    'array': Array,
    'nil': Nil,
    'character': LCharacter,
    'values': Values,
    'input-port': InputPort,
    'output-port': OutputPort,
    'regex': RegExp,
    'syntax': Syntax,
    'eof': EOF,
    'macro': Macro,
    'string': LString,
    'native-symbol': Symbol
  };
  var type_constants = new Map([[NaN, 'NaN'], [null, 'null']]);
  // -------------------------------------------------------------------------

  var fs, path$2, nodeRequire;
  var BN = root.BN;

  /* eslint-disable */
  /* c8 ignore next */
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
  // -------------------------------------------------------------------------
  /* eslint-disable */
  /* c8 ignore next 21 */
  function log(x) {
    var regex = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;
    var literal = arguments[1] === true;
    function msg(x) {
      if (!is_debug()) {
        return;
      }
      var value = global_env.get('repr')(x);
      if (regex === null || regex instanceof RegExp && regex.test(value)) {
        console.log(global_env.get('type')(x) + ": " + value);
      }
      if (literal) {
        console.log(x);
      }
    }
    if (is_promise(x)) {
      x.then(msg);
    } else {
      msg(x);
    }
    return x;
  }
  // ----------------------------------------------------------------------
  /* c8 ignore next */
  function is_debug() {
    return user_env && user_env.get('DEBUG', {
      throwError: false
    });
  }
  /* eslint-enable */
  /* eslint-disable max-len */
  // functions generate regexes to match number rational, integer, complex, complex+rational
  function num_mnemicic_re(mnemonic) {
    return mnemonic ? "(?:#".concat(mnemonic, "(?:#[ie])?|#[ie]#").concat(mnemonic, ")") : '(?:#[ie])?';
  }
  function gen_rational_re(mnemonic, range) {
    return "".concat(num_mnemicic_re(mnemonic), "[+-]?").concat(range, "+/").concat(range, "+");
  }
  // TODO: float complex
  function gen_complex_re(mnemonic, range) {
    // [+-]i have (?=..) so it don't match +i from +inf.0
    return "".concat(num_mnemicic_re(mnemonic), "(?:[+-]?(?:").concat(range, "+/").concat(range, "+|nan.0|inf.0|").concat(range, "+))?(?:[+-]i|[+-]?(?:").concat(range, "+/").concat(range, "+|").concat(range, "+|nan.0|inf.0)i)(?=[()[\\]\\s]|$)");
  }
  function gen_integer_re(mnemonic, range) {
    return "".concat(num_mnemicic_re(mnemonic), "[+-]?").concat(range, "+");
  }
  var re_re = /^#\/((?:\\\/|[^/]|\[[^\]]*\/[^\]]*\])+)\/([gimyus]*)$/;
  var float_stre = '(?:[-+]?(?:[0-9]+(?:[eE][-+]?[0-9]+)|(?:\\.[0-9]+|[0-9]+\\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\\.)';
  // TODO: extend to ([+-]1/2|float)([+-]1/2|float)
  var complex_float_stre = "(?:#[ie])?(?:[+-]?(?:[0-9]+/[0-9]+|nan.0|inf.0|".concat(float_stre, "|[+-]?[0-9]+))?(?:").concat(float_stre, "|[+-](?:[0-9]+/[0-9]+|[0-9]+|nan.0|inf.0))i");
  var float_re = new RegExp("^(#[ie])?".concat(float_stre, "$"), 'i');
  function make_complex_match_re(mnemonic, range) {
    // complex need special treatment of 10e+1i when it's hex or decimal
    var neg = mnemonic === 'x' ? "(?!\\+|".concat(range, ")") : "(?!\\.|".concat(range, ")");
    var fl = '';
    if (mnemonic === '') {
      fl = '(?:[-+]?(?:[0-9]+(?:[eE][-+]?[0-9]+)|(?:\\.[0-9]+|[0-9]+\\.[0-9]+(?![0-9]))(?:[eE][-+]?[0-9]+)?))';
    }
    return new RegExp("^((?:(?:".concat(fl, "|[-+]?inf.0|[-+]?nan.0|[+-]?").concat(range, "+/").concat(range, "+(?!").concat(range, ")|[+-]?").concat(range, "+)").concat(neg, ")?)(").concat(fl, "|[-+]?inf.0|[-+]?nan.0|[+-]?").concat(range, "+/").concat(range, "+|[+-]?").concat(range, "+|[+-])i$"), 'i');
  }
  var complex_list_re = function () {
    var result = {};
    [[10, '', '[0-9]'], [16, 'x', '[0-9a-fA-F]'], [8, 'o', '[0-7]'], [2, 'b', '[01]']].forEach(function (_ref) {
      var _ref2 = _slicedToArray(_ref, 3),
        radix = _ref2[0],
        mnemonic = _ref2[1],
        range = _ref2[2];
      result[radix] = make_complex_match_re(mnemonic, range);
    });
    return result;
  }();
  var characters = {
    'alarm': '\x07',
    'backspace': '\x08',
    'delete': '\x7F',
    'escape': '\x1B',
    'newline': '\n',
    'null': '\x00',
    'return': '\r',
    'space': ' ',
    'tab': '\t',
    // new symbols from ASCII table in SRFI-175
    'dle': '\x10',
    'soh': '\x01',
    'dc1': '\x11',
    'stx': '\x02',
    'dc2': '\x12',
    'etx': '\x03',
    'dc3': '\x13',
    'eot': '\x04',
    'dc4': '\x14',
    'enq': '\x05',
    'nak': '\x15',
    'ack': '\x06',
    'syn': '\x16',
    'bel': '\x07',
    'etb': '\x17',
    'bs': '\x08',
    'can': '\x18',
    'ht': '\x09',
    'em': '\x19',
    'lf': '\x0a',
    'sub': '\x1a',
    'vt': '\x0b',
    'esc': '\x1b',
    'ff': '\x0c',
    'fs': '\x1c',
    'cr': '\x0d',
    'gs': '\x1d',
    'so': '\x0e',
    'rs': '\x1e',
    'si': '\x0f',
    'us': '\x1f',
    'del': '\x7f'
  };
  // -------------------------------------------------------------------------
  // :: ref: https://github.com/bestiejs/punycode.js/blob/master/punycode.js
  // -------------------------------------------------------------------------
  function ucs2decode(string) {
    var output = [];
    var counter = 0;
    var length = string.length;
    while (counter < length) {
      var value = string.charCodeAt(counter++);
      if (value >= 0xD800 && value <= 0xDBFF && counter < length) {
        // It's a high surrogate, and there is a next character.
        var extra = string.charCodeAt(counter++);
        if ((extra & 0xFC00) === 0xDC00) {
          // Low surrogate.
          output.push(((value & 0x3FF) << 10) + (extra & 0x3FF) + 0x10000);
        } else {
          // It's an unmatched surrogate; only append this code unit, in case the
          // next code unit is the high surrogate of a surrogate pair.
          output.push(value);
          counter--;
        }
      } else {
        output.push(value);
      }
    }
    return output;
  }
  // -------------------------------------------------------------------------
  var character_symbols = Object.keys(characters).join('|');
  var char_sre_re = "#\\\\(?:x[0-9a-f]+|".concat(character_symbols, "|[\\s\\S])");
  var char_re = new RegExp("^".concat(char_sre_re, "$"), 'i');
  // Complex with (int) (float) (rational)
  function make_num_stre(fn) {
    var ranges = [['o', '[0-7]'], ['x', '[0-9a-fA-F]'], ['b', '[01]'], ['d', '[0-9]'], ['', '[0-9]']];
    // float exception that don't accept mnemonics
    var result = ranges.map(function (_ref3) {
      var _ref4 = _slicedToArray(_ref3, 2),
        m = _ref4[0],
        range = _ref4[1];
      return fn(m, range);
    }).join('|');
    if (fn === gen_complex_re) {
      result = complex_float_stre + '|' + result;
    }
    return result;
  }
  function make_type_re(fn) {
    return new RegExp('^(?:' + make_num_stre(fn) + ')$', 'i');
  }
  var complex_re = make_type_re(gen_complex_re);
  var rational_re = make_type_re(gen_rational_re);
  var int_re = make_type_re(gen_integer_re);

  // regexes with full range but without mnemonics for string->number
  var int_bare_re = new RegExp('^(?:' + gen_integer_re('', '[0-9a-f]') + ')$', 'i');
  var rational_bare_re = new RegExp('^(?:' + gen_rational_re('', '[0-9a-f]') + ')$', 'i');
  var complex_bare_re = new RegExp('^(?:' + gen_complex_re('', '[0-9a-f]') + ')$', 'i');
  var complex_bare_match_re = make_complex_match_re('', '[0-9a-fA-F]');
  var pre_num_parse_re = /((?:#[xodbie]){0,2})(.*)/i;
  /* eslint-enable */
  function num_pre_parse(arg) {
    var parts = arg.match(pre_num_parse_re);
    var options = {};
    if (parts[1]) {
      var type = parts[1].replace(/#/g, '').toLowerCase().split('');
      if (type.includes('x')) {
        options.radix = 16;
      } else if (type.includes('o')) {
        options.radix = 8;
      } else if (type.includes('b')) {
        options.radix = 2;
      } else if (type.includes('d')) {
        options.radix = 10;
      }
      if (type.includes('i')) {
        options.inexact = true;
      }
      if (type.includes('e')) {
        options.exact = true;
      }
    }
    options.number = parts[2];
    return options;
  }
  // ----------------------------------------------------------------------
  function parse_rational(arg) {
    var radix = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 10;
    var parse = num_pre_parse(arg);
    var parts = parse.number.split('/');
    var num = LRational({
      num: LNumber([parts[0], parse.radix || radix]),
      denom: LNumber([parts[1], parse.radix || radix])
    });
    if (parse.inexact) {
      return num.valueOf();
    } else {
      return num;
    }
  }
  // ----------------------------------------------------------------------
  function parse_integer(arg) {
    var radix = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 10;
    var parse = num_pre_parse(arg);
    if (parse.inexact) {
      return LFloat(parseInt(parse.number, parse.radix || radix));
    }
    return LNumber([parse.number, parse.radix || radix]);
  }
  // ----------------------------------------------------------------------
  function parse_character(arg) {
    var m = arg.match(/#\\x([0-9a-f]+)$/i);
    var _char;
    if (m) {
      var ord = parseInt(m[1], 16);
      _char = String.fromCodePoint(ord);
    } else {
      m = arg.match(/#\\(.+)$/);
      if (m) {
        _char = m[1];
      }
    }
    if (_char) {
      return LCharacter(_char);
    }
    throw new Error('Parse: invalid character');
  }
  // ----------------------------------------------------------------------
  function parse_complex(arg) {
    var radix = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 10;
    function parse_num(n) {
      var value;
      if (n === '+') {
        value = LNumber(1);
      } else if (n === '-') {
        value = LNumber(-1);
      } else if (n.match(int_bare_re)) {
        value = LNumber([n, radix]);
      } else if (n.match(rational_bare_re)) {
        var parts = n.split('/');
        value = LRational({
          num: LNumber([parts[0], radix]),
          denom: LNumber([parts[1], radix])
        });
      } else if (n.match(float_re)) {
        var _float = parse_float(n);
        if (parse.exact) {
          return _float.toRational();
        }
        return _float;
      } else if (n.match(/nan.0$/)) {
        return LNumber(NaN);
      } else if (n.match(/inf.0$/)) {
        if (n[0] === '-') {
          return LNumber(Number.NEGATIVE_INFINITY);
        }
        return LNumber(Number.POSITIVE_INFINITY);
      } else {
        throw new Error('Internal Parser Error');
      }
      if (parse.inexact) {
        return LFloat(value.valueOf());
      }
      return value;
    }
    var parse = num_pre_parse(arg);
    radix = parse.radix || radix;
    var parts;
    var bare_match = parse.number.match(complex_bare_match_re);
    if (radix !== 10 && bare_match) {
      parts = bare_match;
    } else {
      parts = parse.number.match(complex_list_re[radix]);
    }
    var re, im;
    im = parse_num(parts[2]);
    if (parts[1]) {
      re = parse_num(parts[1]);
    } else {
      re = LNumber(0);
    }
    if (im.cmp(0) === 0 && im.__type__ === 'bigint') {
      return re;
    }
    return LComplex({
      im: im,
      re: re
    });
  }
  // ----------------------------------------------------------------------
  function is_int(value) {
    return parseInt(value.toString(), 10) === value;
  }
  // ----------------------------------------------------------------------
  function parse_big_int(str) {
    var num_match = str.match(/^(([-+]?[0-9]*)(?:\.([0-9]+))?)e([-+]?[0-9]+)/i);
    if (num_match) {
      var exponent = parseInt(num_match[4], 10);
      var mantisa; // = parseFloat(num_match[1]);
      var digits = num_match[1].replace(/[-+]?([0-9]*)\..+$/, '$1').length;
      var decimal_points = num_match[3] && num_match[3].length;
      if (digits < Math.abs(exponent)) {
        mantisa = LNumber([num_match[1].replace(/\./, ''), 10]);
        if (decimal_points) {
          exponent -= decimal_points;
        }
      }
    }
    return {
      exponent: exponent,
      mantisa: mantisa
    };
  }
  // ----------------------------------------------------------------------
  function parse_float(arg) {
    var parse = num_pre_parse(arg);
    var value = parseFloat(parse.number);
    var simple_number = (parse.number.match(/\.0$/) || !parse.number.match(/\./)) && !parse.number.match(/e/i);
    if (!parse.inexact) {
      if (parse.exact && simple_number) {
        return LNumber(value);
      }
      // positive big num that eval to int e.g.: 1.2e+20
      if (is_int(value) && parse.number.match(/e\+?[0-9]/i)) {
        return LNumber(value);
      }
      // calculate big int and big fraction by hand - it don't fit into JS float
      var _parse_big_int = parse_big_int(parse.number),
        mantisa = _parse_big_int.mantisa,
        exponent = _parse_big_int.exponent;
      if (mantisa !== undefined && exponent !== undefined) {
        var factor = LNumber(10).pow(LNumber(Math.abs(exponent)));
        if (parse.exact && exponent < 0) {
          return LRational({
            num: mantisa,
            denom: factor
          });
        } else if (exponent > 0) {
          return LNumber(mantisa).mul(factor);
        }
      }
    }
    value = LFloat(value);
    if (parse.exact) {
      return value.toRational();
    }
    return value;
  }
  // ----------------------------------------------------------------------
  function parse_string(string) {
    // handle non JSON escapes and skip unicode escape \u (even partial)
    string = string.replace(/\\x([0-9a-f]+);/ig, function (_, hex) {
      return "\\u" + hex.padStart(4, '0');
    }).replace(/\n/g, '\\n'); // in LIPS strings can be multiline
    var m = string.match(/(\\*)(\\x[0-9A-F])/i);
    if (m && m[1].length % 2 === 0) {
      throw new Error("Invalid string literal, unclosed ".concat(m[2]));
    }
    try {
      return LString(JSON.parse(string));
    } catch (e) {
      var msg = e.message.replace(/in JSON /, '').replace(/.*Error: /, '');
      throw new Error("Invalid string literal: ".concat(msg));
    }
  }
  // ----------------------------------------------------------------------
  function parse_symbol(arg) {
    if (arg.match(/^\|.*\|$/)) {
      arg = arg.replace(/(^\|)|(\|$)/g, '');
      var chars = {
        t: '\t',
        r: '\r',
        n: '\n'
      };
      arg = arg.replace(/\\(x[^;]+);/g, function (_, chr) {
        return String.fromCharCode(parseInt('0' + chr, 16));
      }).replace(/\\(.)/g, function (_, chr) {
        return chars[chr] || chr;
      });
    }
    return new LSymbol(arg);
  }
  // ----------------------------------------------------------------------
  function parse_argument(arg) {
    if (constants.hasOwnProperty(arg)) {
      return constants[arg];
    }
    if (arg.match(/^"[\s\S]*"$/)) {
      return parse_string(arg);
    } else if (arg[0] === '#') {
      var regex = arg.match(re_re);
      if (regex) {
        return new RegExp(regex[1], regex[2]);
      } else if (arg.match(char_re)) {
        return parse_character(arg);
      }
      // characters with more than one codepoint
      var m = arg.match(/#\\(.+)/);
      if (m && ucs2decode(m[1]).length === 1) {
        return parse_character(arg);
      }
    }
    if (arg.match(/[0-9a-f]|[+-]i/i)) {
      if (arg.match(int_re)) {
        return parse_integer(arg);
      } else if (arg.match(float_re)) {
        return parse_float(arg);
      } else if (arg.match(rational_re)) {
        return parse_rational(arg);
      } else if (arg.match(complex_re)) {
        return parse_complex(arg);
      }
    }
    if (arg.match(/^#[iexobd]/)) {
      throw new Error('Invalid numeric constant: ' + arg);
    }
    return parse_symbol(arg);
  }
  // ----------------------------------------------------------------------
  function is_atom_string(str) {
    return !(['(', ')', '[', ']'].includes(str) || specials.names().includes(str));
  }
  // ----------------------------------------------------------------------
  function is_symbol_string(str) {
    return is_atom_string(str) && !(str.match(re_re) || str.match(/^"[\s\S]*"$/) || str.match(int_re) || str.match(float_re) || str.match(complex_re) || str.match(rational_re) || str.match(char_re) || ['#t', '#f', 'nil', 'true', 'false'].includes(str));
  }
  // ----------------------------------------------------------------------
  var string_re = /"(?:\\[\S\s]|[^"])*"?/g;
  // ----------------------------------------------------------------------
  function escape_regex(str) {
    if (typeof str === 'string') {
      var special = /([-\\^$[\]()+{}?*.|])/g;
      return str.replace(special, '\\$1');
    }
    return str;
  }
  // ----------------------------------------------------------------------
  // Stack used in balanced function
  // TODO: use it in parser
  // ----------------------------------------------------------------------
  function Stack() {
    this.data = [];
  }
  Stack.prototype.push = function (item) {
    this.data.push(item);
  };
  Stack.prototype.top = function () {
    return this.data[this.data.length - 1];
  };
  Stack.prototype.pop = function () {
    return this.data.pop();
  };
  Stack.prototype.is_empty = function () {
    return !this.data.length;
  };
  // ----------------------------------------------------------------------
  function tokens(str) {
    if (str instanceof LString) {
      str = str.valueOf();
    }
    var lexer = new Lexer(str, {
      whitespace: true
    });
    var result = [];
    while (true) {
      var token = lexer.peek(true);
      if (token === eof) {
        break;
      }
      result.push(token);
      lexer.skip();
    }
    return result;
  }
  // ----------------------------------------------------------------------
  function multiline_formatter(meta) {
    var token = meta.token,
      rest = _objectWithoutProperties(meta, _excluded);
    if (token.match(/^"[\s\S]*"$/) && token.match(/\n/)) {
      var re = new RegExp('^ {1,' + (meta.col + 1) + '}', 'mg');
      token = token.replace(re, '');
    }
    return _objectSpread({
      token: token
    }, rest);
  }
  // ----------------------------------------------------------------------
  function Thunk(fn) {
    var cont = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : function () {};
    this.fn = fn;
    this.cont = cont;
  }
  // ----------------------------------------------------------------------
  Thunk.prototype.toString = function () {
    return '#<Thunk>';
  };
  // ----------------------------------------------------------------------
  function trampoline(fn) {
    return function () {
      for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }
      return unwind(fn.apply(this, args));
    };
  }
  // ----------------------------------------------------------------------
  function unwind(result) {
    while (result instanceof Thunk) {
      var thunk = result;
      result = result.fn();
      if (!(result instanceof Thunk)) {
        thunk.cont();
      }
    }
    return result;
  }
  // ----------------------------------------------------------------------
  function tokenize(str) {
    var meta = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
    if (str instanceof LString) {
      str = str.toString();
    }
    if (meta) {
      return tokens(str);
    } else {
      var result = tokens(str).map(function (token) {
        // we don't want literal space character to be trimmed
        if (token.token === '#\\ ') {
          return token.token;
        }
        return token.token.trim();
      }).filter(function (token) {
        return token && !token.match(/^;/) && !token.match(/^#\|[\s\S]*\|#$/);
      });
      return strip_s_comments(result);
    }
  }
  // ----------------------------------------------------------------------
  function strip_s_comments(tokens) {
    var s_count = 0;
    var s_start = null;
    var remove_list = [];
    for (var i = 0; i < tokens.length; ++i) {
      var token = tokens[i];
      if (token === '#;') {
        if (['(', '['].includes(tokens[i + 1])) {
          s_count = 1;
          s_start = i;
        } else {
          remove_list.push([i, i + 2]);
        }
        i += 1;
        continue;
      }
      if (s_start !== null) {
        if ([')', ']'].includes(token)) {
          s_count--;
        } else if (['(', '['].includes(token)) {
          s_count++;
        }
        if (s_count === 0) {
          remove_list.push([s_start, i + 1]);
          s_start = null;
        }
      }
    }
    tokens = tokens.slice();
    remove_list.reverse();
    for (var _i = 0, _remove_list = remove_list; _i < _remove_list.length; _i++) {
      var _remove_list$_i = _slicedToArray(_remove_list[_i], 2),
        begin = _remove_list$_i[0],
        end = _remove_list$_i[1];
      tokens.splice(begin, end - begin);
    }
    return tokens;
  }
  // ----------------------------------------------------------------------
  // Detect if object is ES6 Symbol that work with polyfills
  // ----------------------------------------------------------------------
  function isSymbol(x) {
    return _typeof(x) === 'symbol' || _typeof(x) === 'object' && Object.prototype.toString.call(x) === '[object Symbol]';
  }
  // ----------------------------------------------------------------------
  // :: LSymbol constructor
  // ----------------------------------------------------------------------
  function LSymbol(name) {
    if (typeof this !== 'undefined' && this.constructor !== LSymbol || typeof this === 'undefined') {
      return new LSymbol(name);
    }
    if (name instanceof LString) {
      name = name.valueOf();
    }
    if (LSymbol.list[name] instanceof LSymbol) {
      return LSymbol.list[name];
    }
    this.__name__ = name;
    if (typeof name === 'string') {
      LSymbol.list[name] = this;
    }
  }
  LSymbol.list = {};
  LSymbol.literal = Symbol["for"]('__literal__');
  LSymbol.object = Symbol["for"]('__object__');
  // ----------------------------------------------------------------------
  LSymbol.is = function (symbol, name) {
    return symbol instanceof LSymbol && (name instanceof LSymbol && symbol.__name__ === name.__name__ || typeof name === 'string' && symbol.__name__ === name || name instanceof RegExp && name.test(symbol.__name__));
  };
  // ----------------------------------------------------------------------
  LSymbol.prototype.toString = function (quote) {
    //return '#<symbol \'' + this.name + '\'>';
    if (isSymbol(this.__name__)) {
      return symbol_to_string(this.__name__);
    }
    var str = this.valueOf();
    // those special characters can be normal symbol when printed
    if (quote && str.match(/(^;|[\s()[\]'])/)) {
      return "|".concat(str, "|");
    }
    return str;
  };
  LSymbol.prototype.literal = function () {
    if (this.is_gensym()) {
      return this[LSymbol.literal];
    }
    return this.valueOf();
  };
  LSymbol.prototype.serialize = function () {
    if (LString.isString(this.__name__)) {
      return this.__name__;
    }
    return [symbol_to_string(this.__name__)];
  };
  LSymbol.prototype.valueOf = function () {
    return this.__name__.valueOf();
  };
  // -------------------------------------------------------------------------
  LSymbol.prototype.is_gensym = function () {
    return is_gensym(this.__name__);
  };
  // -------------------------------------------------------------------------
  function symbol_to_string(obj) {
    return obj.toString().replace(/^Symbol\(([^)]+)\)/, '$1');
  }
  // -------------------------------------------------------------------------
  function is_gensym(symbol) {
    if (_typeof(symbol) === 'symbol') {
      return !!symbol.toString().match(/^Symbol\(#:/);
    }
    return false;
  }
  // -------------------------------------------------------------------------
  var gensym = function () {
    var count = 0;
    function with_props(name, sym) {
      var symbol = new LSymbol(sym);
      hidden_prop(symbol, '__literal__', name);
      return symbol;
    }
    return function () {
      var name = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;
      if (name instanceof LSymbol) {
        if (name.is_gensym()) {
          return name;
        }
        name = name.valueOf();
      }
      if (is_gensym(name)) {
        // don't do double gynsyms in nested syntax-rules
        return LSymbol(name);
      }
      // use ES6 symbol as name for lips symbol (they are unique)
      if (name !== null) {
        return with_props(name, Symbol("#:".concat(name)));
      }
      count++;
      return with_props(count, Symbol("#:g".concat(count)));
    };
  }();
  // ----------------------------------------------------------------------
  // Class used to escape promises: feature #54
  // ----------------------------------------------------------------------
  function QuotedPromise(promise) {
    var _this = this;
    var internal = {
      pending: true,
      rejected: false,
      fulfilled: false,
      reason: undefined,
      type: undefined
    };
    // then added to __promise__ is needed otherwise rejection
    // will give UnhandledPromiseRejectionWarning in Node.js
    promise = promise.then(function (v) {
      internal.type = type(v);
      internal.fulfilled = true;
      internal.pending = false;
      return v;
    });
    // promise without catch, used for valueOf - for rejecting
    // that should throw an error when used with await
    read_only(this, '_promise', promise, {
      hidden: true
    });
    if (is_function(promise["catch"])) {
      // prevent exception on unhandled rejecting when using
      // '>(Promise.reject (new Error "zonk")) in REPL
      promise = promise["catch"](function (err) {
        internal.rejected = true;
        internal.pending = false;
        internal.reason = err;
      });
    }
    Object.keys(internal).forEach(function (name) {
      Object.defineProperty(_this, "__".concat(name, "__"), {
        enumerable: true,
        get: function get() {
          return internal[name];
        }
      });
    });
    read_only(this, '__promise__', promise);
    // prevent resolving when returned from real promise #153
    this.then = false;
  }
  // ----------------------------------------------------------------------
  QuotedPromise.prototype.then = function (fn) {
    return new QuotedPromise(this.valueOf().then(fn));
  };
  // ----------------------------------------------------------------------
  QuotedPromise.prototype["catch"] = function (fn) {
    return new QuotedPromise(this.valueOf()["catch"](fn));
  };
  // ----------------------------------------------------------------------
  QuotedPromise.prototype.valueOf = function () {
    if (!this._promise) {
      throw new Error('QuotedPromise: invalid promise created');
    }
    return this._promise;
  };
  // ----------------------------------------------------------------------
  QuotedPromise.prototype.toString = function () {
    if (this.__pending__) {
      return QuotedPromise.pending_str;
    }
    if (this.__rejected__) {
      return QuotedPromise.rejected_str;
    }
    return "#<js-promise resolved (".concat(this.__type__, ")>");
  };
  QuotedPromise.pending_str = '#<js-promise (pending)>';
  QuotedPromise.rejected_str = '#<js-promise (rejected)>';
  // ----------------------------------------------------------------------
  // wrapper over Promise.all that ignore quoted promises
  // ----------------------------------------------------------------------
  function promise_all(arg) {
    if (Array.isArray(arg)) {
      return Promise.all(escape_quoted_promises(arg)).then(unescape_quoted_promises);
    }
    return arg;
  }
  // ----------------------------------------------------------------------
  function escape_quoted_promises(array) {
    // using loops for performance
    var escaped = new Array(array.length),
      i = array.length;
    while (i--) {
      var value = array[i];
      if (value instanceof QuotedPromise) {
        escaped[i] = new Value(value);
      } else {
        escaped[i] = value;
      }
    }
    return escaped;
  }
  // ----------------------------------------------------------------------
  function unescape_quoted_promises(array) {
    var unescaped = new Array(array.length),
      i = array.length;
    while (i--) {
      var value = array[i];
      if (value instanceof Value) {
        unescaped[i] = value.valueOf();
      } else {
        unescaped[i] = value;
      }
    }
    return unescaped;
  }
  // ----------------------------------------------------------------------
  // :: Parser macros transformers
  // ----------------------------------------------------------------------
  var specials = {
    LITERAL: Symbol["for"]('literal'),
    SPLICE: Symbol["for"]('splice'),
    SYMBOL: Symbol["for"]('symbol'),
    names: function names() {
      return Object.keys(this.__list__);
    },
    type: function type(name) {
      try {
        return this.get(name).type;
      } catch (e) {
        console.log({
          name: name
        });
        console.log(e);
        return null;
      }
    },
    get: function get(name) {
      return this.__list__[name];
    },
    // events are used in Lexer dynamic rules
    off: function off(name) {
      var _this2 = this;
      var fn = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;
      if (Array.isArray(name)) {
        name.forEach(function (name) {
          return _this2.off(name, fn);
        });
      } else if (fn === null) {
        delete this.__events__[name];
      } else {
        this.__events__ = this.__events__.filter(function (test) {
          return test !== fn;
        });
      }
    },
    on: function on(name, fn) {
      var _this3 = this;
      if (Array.isArray(name)) {
        name.forEach(function (name) {
          return _this3.on(name, fn);
        });
      } else if (!this.__events__[name]) {
        this.__events__[name] = [fn];
      } else {
        this.__events__[name].push(fn);
      }
    },
    trigger: function trigger(name) {
      for (var _len2 = arguments.length, args = new Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
        args[_key2 - 1] = arguments[_key2];
      }
      if (this.__events__[name]) {
        this.__events__[name].forEach(function (fn) {
          return fn.apply(void 0, args);
        });
      }
    },
    remove: function remove(name) {
      delete this.__list__[name];
      this.trigger('remove');
    },
    append: function append(name, value, type) {
      this.__list__[name] = {
        seq: name,
        symbol: value,
        type: type
      };
      this.trigger('append');
    },
    __events__: {},
    __list__: {}
  };
  function is_special(token) {
    return specials.names().includes(token);
  }
  function is_builtin(token) {
    return specials.__builtins__.includes(token);
  }
  function is_literal(special) {
    return specials.type(special) === specials.LITERAL;
  }
  function is_symbol_extension(special) {
    return specials.type(special) === specials.SYMBOL;
  }
  // ----------------------------------------------------------------------
  var defined_specials = [["'", new LSymbol('quote'), specials.LITERAL], ['`', new LSymbol('quasiquote'), specials.LITERAL], [',@', new LSymbol('unquote-splicing'), specials.LITERAL], [',', new LSymbol('unquote'), specials.LITERAL], ["'>", new LSymbol('quote-promise'), specials.LITERAL]];
  var builtins = defined_specials.map(function (arr) {
    return arr[0];
  });
  Object.freeze(builtins);
  Object.defineProperty(specials, '__builtins__', {
    writable: false,
    value: builtins
  });
  defined_specials.forEach(function (_ref5) {
    var _ref6 = _slicedToArray(_ref5, 3),
      seq = _ref6[0],
      symbol = _ref6[1],
      type = _ref6[2];
    specials.append(seq, symbol, type);
  });
  // ----------------------------------------------------------------------
  // :: Finite State Machine based incremental Lexer
  // ----------------------------------------------------------------------
  /* Lexer debugger
     var DEBUG = false;
     function log(...args) {
     if (DEBUG) {
     console.log(...args);
     }
     }
  */
  var Lexer = /*#__PURE__*/function () {
    function Lexer(input) {
      var _this4 = this;
      var _ref7 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        _ref7$whitespace = _ref7.whitespace,
        whitespace = _ref7$whitespace === void 0 ? false : _ref7$whitespace;
      _classCallCheck(this, Lexer);
      read_only(this, '__input__', input.replace(/\r/g, ''));
      var internals = {};
      // hide internals from introspection
      ['_i', '_whitespace', '_col', '_newline', '_line', '_state', '_next', '_token', '_prev_char'].forEach(function (name) {
        Object.defineProperty(_this4, name, {
          configurable: false,
          enumerable: false,
          get: function get() {
            return internals[name];
          },
          set: function set(value) {
            internals[name] = value;
          }
        });
      });
      this._whitespace = whitespace;
      this._i = this._line = this._col = this._newline = 0;
      this._state = this._next = this._token = null;
      this._prev_char = '';
    }
    _createClass(Lexer, [{
      key: "get",
      value: function get(name) {
        return this.__internal[name];
      }
    }, {
      key: "set",
      value: function set(name, value) {
        this.__internal[name] = value;
      }
    }, {
      key: "token",
      value: function token() {
        var meta = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : false;
        if (meta) {
          var line = this._line;
          if (this._whitespace && this._token === '\n') {
            --line;
          }
          return {
            token: this._token,
            col: this._col,
            offset: this._i,
            line: line
          };
        }
        return this._token;
      }
    }, {
      key: "peek",
      value: function peek() {
        var meta = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : false;
        if (this._i >= this.__input__.length) {
          return eof;
        }
        if (this._token) {
          return this.token(meta);
        }
        var found = this.next_token();
        if (found) {
          this._token = this.__input__.substring(this._i, this._next);
          return this.token(meta);
        }
        return eof;
      }
    }, {
      key: "skip",
      value: function skip() {
        if (this._next !== null) {
          this._token = null;
          this._i = this._next;
        }
      }
    }, {
      key: "read_line",
      value: function read_line() {
        var len = this.__input__.length;
        if (this._i >= len) {
          return eof;
        }
        for (var i = this._i; i < len; ++i) {
          var _char2 = this.__input__[i];
          if (_char2 === '\n') {
            var line = this.__input__.substring(this._i, i);
            this._i = i + 1;
            ++this._line;
            return line;
          }
        }
        return this.read_rest();
      }
    }, {
      key: "read_rest",
      value: function read_rest() {
        var i = this._i;
        this._i = this.__input__.length;
        return this.__input__.substring(i);
      }
    }, {
      key: "read_string",
      value: function read_string(num) {
        var len = this.__input__.length;
        if (this._i >= len) {
          return eof;
        }
        if (num + this._i >= len) {
          return this.read_rest();
        }
        var end = this._i + num;
        var result = this.__input__.substring(this._i, end);
        var found = result.match(/\n/g);
        if (found) {
          this._line += found.length;
        }
        this._i = end;
        return result;
      }
    }, {
      key: "peek_char",
      value: function peek_char() {
        if (this._i >= this.__input__.length) {
          return eof;
        }
        return LCharacter(this.__input__[this._i]);
      }
    }, {
      key: "read_char",
      value: function read_char() {
        var _char3 = this.peek_char();
        this.skip_char();
        return _char3;
      }
    }, {
      key: "skip_char",
      value: function skip_char() {
        if (this._i < this.__input__.length) {
          ++this._i;
          this._token = null;
        }
      }
    }, {
      key: "match_rule",
      value: function match_rule(rule) {
        var _ref8 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          prev_char = _ref8.prev_char,
          _char4 = _ref8["char"],
          next_char = _ref8.next_char;
        var _rule = _slicedToArray(rule, 4),
          re = _rule[0],
          prev_re = _rule[1],
          next_re = _rule[2],
          state = _rule[3];
        if (rule.length !== 5) {
          throw new Error("Lexer: Invalid rule of length ".concat(rule.length));
        }
        if (!_char4.match(re)) {
          return false;
        }
        if (!match_or_null(prev_re, prev_char)) {
          return false;
        }
        if (!match_or_null(next_re, next_char)) {
          return false;
        }
        if (state !== this._state) {
          return false;
        }
        return true;
      }
    }, {
      key: "next_token",
      value: function next_token() {
        if (this._i >= this.__input__.length) {
          return false;
        }
        var start = true;
        loop: for (var i = this._i, len = this.__input__.length; i < len; ++i) {
          var _char5 = this.__input__[i];
          var prev_char = this.__input__[i - 1] || '';
          var next_char = this.__input__[i + 1] || '';
          if (_char5 === '\n') {
            ++this._line;
            var newline = this._newline;
            if (this._state === null) {
              // keep beginning of the newline to calculate col
              // we don't want to check inside the token (e.g. strings)
              this._newline = i + 1;
            }
            if (this._whitespace && this._state === null) {
              this._next = i + 1;
              this._col = this._i - newline;
              return true;
            }
          }
          // skip leading spaces
          if (start && this._state === null && _char5.match(/\s/)) {
            if (this._whitespace) {
              if (!next_char.match(/\s/)) {
                this._next = i + 1;
                this._col = this._i - this._newline;
                return true;
              } else {
                continue;
              }
            } else {
              this._i = i + 1;
              continue;
            }
          }
          start = false;
          var _iterator4 = _createForOfIteratorHelper(Lexer.rules),
            _step4;
          try {
            for (_iterator4.s(); !(_step4 = _iterator4.n()).done;) {
              var rule = _step4.value;
              if (this.match_rule(rule, {
                prev_char: prev_char,
                "char": _char5,
                next_char: next_char
              })) {
                // change state to null if end of the token
                var next_state = rule[rule.length - 1];
                this._state = next_state;
                if (this._state === null) {
                  this._next = i + 1;
                  this._col = this._i - this._newline;
                  return true;
                }
                // token is activated
                continue loop;
              }
            }
          } catch (err) {
            _iterator4.e(err);
          } finally {
            _iterator4.f();
          }
          if (this._state !== null) {
            // collect char in token
            continue loop;
          }
          // no rule for token
          var line = this.__input__.split('\n')[this._line];
          throw new Error("Invalid Syntax at line ".concat(this._line, "\n").concat(line));
        }
      }
    }]);
    return Lexer;
  }(); // ----------------------------------------------------------------------
  // TODO: cache the rules creation or whole list
  // ----------------------------------------------------------------------
  // State rule for literal symbol
  // ----------------------------------------------------------------------
  Lexer.literal_rule = function literal_rule(string, symbol) {
    var p_re = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;
    var n_re = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
    if (string.length === 0) {
      throw new Error('Lexer: invalid literal rule');
    }
    if (string.length === 1) {
      return [[string, p_re, n_re, null, null]];
    }
    var rules = [];
    for (var i = 0, len = string.length; i < len; ++i) {
      var rule = [];
      rule.push(string[i]);
      rule.push(string[i - 1] || p_re);
      rule.push(string[i + 1] || n_re);
      if (i === 0) {
        rule.push(null);
        rule.push(symbol);
      } else if (i === len - 1) {
        rule.push(symbol);
        rule.push(null);
      } else {
        rule.push(symbol);
        rule.push(symbol);
      }
      rules.push(rule);
    }
    return rules;
  };
  // ----------------------------------------------------------------------
  Lexer.string = Symbol["for"]('string');
  Lexer.string_escape = Symbol["for"]('string_escape');
  Lexer.symbol = Symbol["for"]('symbol');
  Lexer.comment = Symbol["for"]('comment');
  Lexer.regex = Symbol["for"]('regex');
  Lexer.regex_init = Symbol["for"]('regex_init');
  Lexer.regex_class = Symbol["for"]('regex_class');
  Lexer.character = Symbol["for"]('character');
  Lexer.bracket = Symbol["for"]('bracket');
  Lexer.b_symbol = Symbol["for"]('b_symbol');
  Lexer.b_comment = Symbol["for"]('b_comment');
  Lexer.i_comment = Symbol["for"]('i_comment');
  Lexer.l_datum = Symbol["for"]('l_datum');
  Lexer.dot = Symbol["for"]('dot');
  // ----------------------------------------------------------------------
  Lexer.boundary = /^$|[\s()[\]']/;
  // ----------------------------------------------------------------------
  Lexer._rules = [
  // char_re prev_re next_re from_state to_state
  // null as to_state mean that is single char token
  // string
  [/"/, null, null, Lexer.string, null], [/"/, null, null, null, Lexer.string], [/"/, null, null, Lexer.string_escape, Lexer.string], [/\\/, null, null, Lexer.string, Lexer.string_escape], [/./, /\\/, null, Lexer.string_escape, Lexer.string],
  // hash special symbols, lexer don't need to distinguish those
  // we only care if it's not pick up by vectors literals
  [/#/, null, /[bdxoeitf]/i, null, Lexer.symbol],
  // characters
  [/#/, null, /\\/, null, Lexer.character], [/\\/, /#/, /\s/, Lexer.character, Lexer.character], [/\\/, /#/, /[()[\]]/, Lexer.character, Lexer.character], [/\s/, /\\/, null, Lexer.character, null], [/\S/, null, Lexer.boundary, Lexer.character, null],
  // regex
  [/#/, Lexer.boundary, /\//, null, Lexer.regex_init], [/./, /\//, null, Lexer.regex_init, Lexer.regex], [/[ \t]/, null, null, Lexer.regex, Lexer.regex], [/\[/, null, null, Lexer.regex, Lexer.regex_class], [/\]/, /[^\\]/, null, Lexer.regex_class, Lexer.regex], [/[()[\]]/, null, null, Lexer.regex, Lexer.regex], [/\//, /\\/, null, Lexer.regex, Lexer.regex], [/\//, /[^#]/, Lexer.boundary, Lexer.regex, null], [/[gimyus]/, /\//, Lexer.boundary, Lexer.regex, null], [/[gimyus]/, /\//, /[gimyus]/, Lexer.regex, Lexer.regex], [/[gimyus]/, /[gimyus]/, Lexer.boundary, Lexer.regex, null],
  // comment
  [/;/, /^$|[^#]/, null, null, Lexer.comment], [/\n/, ';', null, Lexer.comment, null], [/[\s\S]/, null, /\n/, Lexer.comment, null], [/\s/, null, null, Lexer.comment, Lexer.comment],
  // block comment
  [/#/, null, /\|/, null, Lexer.b_comment], [/\s/, null, null, Lexer.b_comment, Lexer.b_comment], [/#/, /\|/, null, Lexer.b_comment, null],
  // inline commentss
  [/#/, null, /;/, null, Lexer.i_comment], [/;/, /#/, null, Lexer.i_comment, null],
  // datum label
  [/#/, null, /[0-9]/, null, Lexer.l_datum], [/=/, /[0-9]/, null, Lexer.l_datum, null], [/#/, /[0-9]/, null, Lexer.l_datum, null],
  // for dot comma `(a .,b)
  [/\./, Lexer.boundary, /,/, null, null],
  // block symbols
  [/\|/, null, null, null, Lexer.b_symbol], [/\s/, null, null, Lexer.b_symbol, Lexer.b_symbol], [/\|/, null, Lexer.boundary, Lexer.b_symbol, null]];
  // ----------------------------------------------------------------------
  Lexer._brackets = [[/[()[\]]/, null, null, null, null]];
  // ----------------------------------------------------------------------
  // :: symbols should be matched last
  // ----------------------------------------------------------------------
  Lexer._symbol_rules = [[/\S/, Lexer.boundary, Lexer.boundary, null, null], [/\S/, Lexer.boundary, null, null, Lexer.symbol], [/\S/, null, Lexer.boundary, null, null], [/\S/, null, null, null, Lexer.symbol], [/\S/, null, Lexer.boundary, Lexer.symbol, null]];
  // ----------------------------------------------------------------------
  // :: Dynamic getter or Lexer state rules, parser uses this
  // :: so user code can modify Lexer using syntax extensions
  // ----------------------------------------------------------------------
  Lexer._cache = {
    valid: false,
    rules: null
  };
  // ----------------------------------------------------------------------
  specials.on(['remove', 'append'], function () {
    Lexer._cache.valid = false;
    Lexer._cache.rules = null;
  });
  // ----------------------------------------------------------------------
  Object.defineProperty(Lexer, 'rules', {
    get: function get() {
      if (Lexer._cache.valid) {
        return Lexer._cache.rules;
      }
      var tokens = specials.names().sort(function (a, b) {
        return b.length - a.length || a.localeCompare(b);
      });
      var special_rules = tokens.reduce(function (acc, token) {
        var _specials$get = specials.get(token);
          _specials$get.type;
          var special_symbol = _specials$get.symbol;
        var rules;
        var symbol;
        // we need distinct symbols_ for syntax extensions
        if (token[0] === '#') {
          if (token.length === 1) {
            symbol = Symbol["for"](token);
          } else {
            symbol = Symbol["for"](token[1]);
          }
        } else {
          symbol = special_symbol;
        }
        rules = Lexer.literal_rule(token, symbol);
        return acc.concat(rules);
      }, []);
      Lexer._cache.rules = Lexer._rules.concat(Lexer._brackets, special_rules, Lexer._symbol_rules);
      Lexer._cache.valid = true;
      return Lexer._cache.rules;
    }
  });
  // ----------------------------------------------------------------------
  function match_or_null(re, _char6) {
    return re === null || _char6.match(re);
  }
  // ----------------------------------------------------------------------
  // :: Parser inspired by BiwaScheme
  // :: ref: https://github.com/biwascheme/biwascheme/blob/master/src/system/parser.js
  // ----------------------------------------------------------------------
  var Parser = /*#__PURE__*/function () {
    function Parser(arg) {
      var _ref9 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        env = _ref9.env,
        _ref9$meta = _ref9.meta,
        meta = _ref9$meta === void 0 ? false : _ref9$meta,
        _ref9$formatter = _ref9.formatter,
        formatter = _ref9$formatter === void 0 ? multiline_formatter : _ref9$formatter;
      _classCallCheck(this, Parser);
      if (arg instanceof LString) {
        arg = arg.toString();
      }
      read_only(this, '_formatter', formatter, {
        hidden: true
      });
      read_only(this, '__lexer__', new Lexer(arg));
      read_only(this, '__env__', env);
      read_only(this, '_meta', meta, {
        hidden: true
      });
      // datum labels
      read_only(this, '_refs', [], {
        hidden: true
      });
      read_only(this, '_state', {
        parentheses: 0
      }, {
        hidden: true
      });
    }
    _createClass(Parser, [{
      key: "resolve",
      value: function resolve(name) {
        return this.__env__ && this.__env__.get(name, {
          throwError: false
        });
      }
    }, {
      key: "peek",
      value: function () {
        var _peek = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee() {
          var token;
          return _regeneratorRuntime.wrap(function _callee$(_context) {
            while (1) {
              switch (_context.prev = _context.next) {
                case 0:
                  token = this.__lexer__.peek(true);
                  if (!(token === eof)) {
                    _context.next = 4;
                    break;
                  }
                  return _context.abrupt("return", eof);
                case 4:
                  if (!this.is_comment(token.token)) {
                    _context.next = 7;
                    break;
                  }
                  this.skip();
                  return _context.abrupt("continue", 0);
                case 7:
                  if (!(token.token === '#;')) {
                    _context.next = 14;
                    break;
                  }
                  this.skip();
                  if (!(this.__lexer__.peek() === eof)) {
                    _context.next = 11;
                    break;
                  }
                  throw new Error('Lexer: syntax error eof found after comment');
                case 11:
                  _context.next = 13;
                  return this._read_object();
                case 13:
                  return _context.abrupt("continue", 0);
                case 14:
                  return _context.abrupt("break", 17);
                case 17:
                  token = this._formatter(token);
                  if (!this._meta) {
                    _context.next = 20;
                    break;
                  }
                  return _context.abrupt("return", token);
                case 20:
                  return _context.abrupt("return", token.token);
                case 21:
                case "end":
                  return _context.stop();
              }
            }
          }, _callee, this);
        }));
        function peek() {
          return _peek.apply(this, arguments);
        }
        return peek;
      }()
    }, {
      key: "reset",
      value: function reset() {
        this._refs.length = 0;
      }
    }, {
      key: "skip",
      value: function skip() {
        this.__lexer__.skip();
      }
    }, {
      key: "read",
      value: function () {
        var _read = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee2() {
          var token;
          return _regeneratorRuntime.wrap(function _callee2$(_context2) {
            while (1) {
              switch (_context2.prev = _context2.next) {
                case 0:
                  _context2.next = 2;
                  return this.peek();
                case 2:
                  token = _context2.sent;
                  this.skip();
                  return _context2.abrupt("return", token);
                case 5:
                case "end":
                  return _context2.stop();
              }
            }
          }, _callee2, this);
        }));
        function read() {
          return _read.apply(this, arguments);
        }
        return read;
      }()
    }, {
      key: "match_datum_label",
      value: function match_datum_label(token) {
        var m = token.match(/^#([0-9]+)=$/);
        return m && m[1];
      }
    }, {
      key: "match_datum_ref",
      value: function match_datum_ref(token) {
        var m = token.match(/^#([0-9]+)#$/);
        return m && m[1];
      }
    }, {
      key: "is_open",
      value: function is_open(token) {
        var result = ['(', '['].includes(token);
        if (result) {
          this._state.parentheses++;
        }
        return result;
      }
    }, {
      key: "is_close",
      value: function is_close(token) {
        var result = [')', ']'].includes(token);
        if (result) {
          this._state.parentheses--;
        }
        return result;
      }
    }, {
      key: "read_list",
      value: function () {
        var _read_list = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee3() {
          var head, prev, token, cur;
          return _regeneratorRuntime.wrap(function _callee3$(_context3) {
            while (1) {
              switch (_context3.prev = _context3.next) {
                case 0:
                  head = _nil, prev = head;
                case 1:
                  _context3.next = 4;
                  return this.peek();
                case 4:
                  token = _context3.sent;
                  if (!(token === eof)) {
                    _context3.next = 7;
                    break;
                  }
                  return _context3.abrupt("break", 27);
                case 7:
                  if (!this.is_close(token)) {
                    _context3.next = 10;
                    break;
                  }
                  this.skip();
                  return _context3.abrupt("break", 27);
                case 10:
                  if (!(token === '.' && head !== _nil)) {
                    _context3.next = 17;
                    break;
                  }
                  this.skip();
                  _context3.next = 14;
                  return this._read_object();
                case 14:
                  prev.cdr = _context3.sent;
                  _context3.next = 25;
                  break;
                case 17:
                  _context3.t0 = Pair;
                  _context3.next = 20;
                  return this._read_object();
                case 20:
                  _context3.t1 = _context3.sent;
                  _context3.t2 = _nil;
                  cur = new _context3.t0(_context3.t1, _context3.t2);
                  if (head === _nil) {
                    head = cur;
                  } else {
                    prev.cdr = cur;
                  }
                  prev = cur;
                case 25:
                  _context3.next = 1;
                  break;
                case 27:
                  return _context3.abrupt("return", head);
                case 28:
                case "end":
                  return _context3.stop();
              }
            }
          }, _callee3, this);
        }));
        function read_list() {
          return _read_list.apply(this, arguments);
        }
        return read_list;
      }()
    }, {
      key: "read_value",
      value: function () {
        var _read_value = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee4() {
          var token;
          return _regeneratorRuntime.wrap(function _callee4$(_context4) {
            while (1) {
              switch (_context4.prev = _context4.next) {
                case 0:
                  _context4.next = 2;
                  return this.read();
                case 2:
                  token = _context4.sent;
                  if (!(token === eof)) {
                    _context4.next = 5;
                    break;
                  }
                  throw new Error('Parser: Expected token eof found');
                case 5:
                  return _context4.abrupt("return", parse_argument(token));
                case 6:
                case "end":
                  return _context4.stop();
              }
            }
          }, _callee4, this);
        }));
        function read_value() {
          return _read_value.apply(this, arguments);
        }
        return read_value;
      }()
    }, {
      key: "is_comment",
      value: function is_comment(token) {
        return token.match(/^;/) || token.match(/^#\|/) && token.match(/\|#$/);
      }
    }, {
      key: "evaluate",
      value: function evaluate(code) {
        return _evaluate(code, {
          env: this.__env__,
          error: function error(e) {
            throw e;
          }
        });
      }
      // public API that handle R7RS datum labels
    }, {
      key: "read_object",
      value: function () {
        var _read_object2 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee5() {
          var object;
          return _regeneratorRuntime.wrap(function _callee5$(_context5) {
            while (1) {
              switch (_context5.prev = _context5.next) {
                case 0:
                  this.reset();
                  _context5.next = 3;
                  return this._read_object();
                case 3:
                  object = _context5.sent;
                  if (object instanceof DatumReference) {
                    object = object.valueOf();
                  }
                  if (!this._refs.length) {
                    _context5.next = 7;
                    break;
                  }
                  return _context5.abrupt("return", this._resolve_object(object));
                case 7:
                  return _context5.abrupt("return", object);
                case 8:
                case "end":
                  return _context5.stop();
              }
            }
          }, _callee5, this);
        }));
        function read_object() {
          return _read_object2.apply(this, arguments);
        }
        return read_object;
      }()
    }, {
      key: "balanced",
      value: function balanced() {
        return this._state.parentheses === 0;
      }
    }, {
      key: "ballancing_error",
      value: function ballancing_error(expr, prev) {
        var count = this._state.parentheses;
        var e;
        if (count < 0) {
          e = new Error('Parser: unexpected parenthesis');
          e.__code__ = [prev.toString() + ')'];
        } else {
          e = new Error('Parser: expected parenthesis but eof found');
          var re = new RegExp("\\){".concat(count, "}$"));
          e.__code__ = [expr.toString().replace(re, '')];
        }
        throw e;
      }
      // Cover This function (array and object branch)
    }, {
      key: "_resolve_object",
      value: function () {
        var _resolve_object2 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee6(object) {
          var _this5 = this;
          var result;
          return _regeneratorRuntime.wrap(function _callee6$(_context6) {
            while (1) {
              switch (_context6.prev = _context6.next) {
                case 0:
                  if (!Array.isArray(object)) {
                    _context6.next = 2;
                    break;
                  }
                  return _context6.abrupt("return", object.map(function (item) {
                    return _this5._resolve_object(item);
                  }));
                case 2:
                  if (!is_plain_object(object)) {
                    _context6.next = 6;
                    break;
                  }
                  result = {};
                  Object.keys(object).forEach(function (key) {
                    result[key] = _this5._resolve_object(object[key]);
                  });
                  return _context6.abrupt("return", result);
                case 6:
                  if (!(object instanceof Pair)) {
                    _context6.next = 8;
                    break;
                  }
                  return _context6.abrupt("return", this._resolve_pair(object));
                case 8:
                  return _context6.abrupt("return", object);
                case 9:
                case "end":
                  return _context6.stop();
              }
            }
          }, _callee6, this);
        }));
        function _resolve_object(_x3) {
          return _resolve_object2.apply(this, arguments);
        }
        return _resolve_object;
      }()
    }, {
      key: "_resolve_pair",
      value: function () {
        var _resolve_pair2 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee7(pair) {
          return _regeneratorRuntime.wrap(function _callee7$(_context7) {
            while (1) {
              switch (_context7.prev = _context7.next) {
                case 0:
                  if (!(pair instanceof Pair)) {
                    _context7.next = 15;
                    break;
                  }
                  if (!(pair.car instanceof DatumReference)) {
                    _context7.next = 7;
                    break;
                  }
                  _context7.next = 4;
                  return pair.car.valueOf();
                case 4:
                  pair.car = _context7.sent;
                  _context7.next = 8;
                  break;
                case 7:
                  this._resolve_pair(pair.car);
                case 8:
                  if (!(pair.cdr instanceof DatumReference)) {
                    _context7.next = 14;
                    break;
                  }
                  _context7.next = 11;
                  return pair.cdr.valueOf();
                case 11:
                  pair.cdr = _context7.sent;
                  _context7.next = 15;
                  break;
                case 14:
                  this._resolve_pair(pair.cdr);
                case 15:
                  return _context7.abrupt("return", pair);
                case 16:
                case "end":
                  return _context7.stop();
              }
            }
          }, _callee7, this);
        }));
        function _resolve_pair(_x4) {
          return _resolve_pair2.apply(this, arguments);
        }
        return _resolve_pair;
      }()
    }, {
      key: "_read_object",
      value: function () {
        var _read_object3 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee8() {
          var token, special, builtin, expr, is_symbol, object, extension, args, result, ref, ref_label;
          return _regeneratorRuntime.wrap(function _callee8$(_context8) {
            while (1) {
              switch (_context8.prev = _context8.next) {
                case 0:
                  _context8.next = 2;
                  return this.peek();
                case 2:
                  token = _context8.sent;
                  if (!(token === eof)) {
                    _context8.next = 5;
                    break;
                  }
                  return _context8.abrupt("return", token);
                case 5:
                  if (!is_special(token)) {
                    _context8.next = 38;
                    break;
                  }
                  // Built-in parser extensions are mapping short symbols to longer symbols
                  // that can be function or macro. Parser doesn't care
                  // if it's not built-in and the extension can be macro or function.
                  // FUNCTION: when it's used, it gets arguments like FEXPR and the
                  // result is returned by parser as is the macro.
                  // MACRO: if macro is used, then it is evaluated in place and the
                  // result is returned by parser and it is quoted.
                  special = specials.get(token);
                  builtin = is_builtin(token);
                  this.skip();
                  is_symbol = is_symbol_extension(token);
                  if (!is_symbol) {
                    _context8.next = 14;
                    break;
                  }
                  _context8.t0 = undefined;
                  _context8.next = 17;
                  break;
                case 14:
                  _context8.next = 16;
                  return this._read_object();
                case 16:
                  _context8.t0 = _context8.sent;
                case 17:
                  object = _context8.t0;
                  if (builtin) {
                    _context8.next = 25;
                    break;
                  }
                  extension = this.__env__.get(special.symbol);
                  if (!(typeof extension === 'function')) {
                    _context8.next = 25;
                    break;
                  }
                  if (is_literal(token)) {
                    args = [object];
                  } else if (object === _nil) {
                    args = [];
                  } else if (object instanceof Pair) {
                    args = object.to_array(false);
                  }
                  if (!(args || is_symbol)) {
                    _context8.next = 24;
                    break;
                  }
                  return _context8.abrupt("return", call_function(extension, is_symbol ? [] : args, {
                    env: this.__env__,
                    dynamic_env: this.__env__,
                    use_dynamic: false
                  }));
                case 24:
                  throw new Error('Parse Error: Invalid parser extension ' + "invocation ".concat(special.symbol));
                case 25:
                  if (is_literal(token)) {
                    expr = new Pair(special.symbol, new Pair(object, _nil));
                  } else {
                    expr = new Pair(special.symbol, object);
                  }
                  // Built-in parser extensions just expand into lists like 'x ==> (quote x)
                  if (!builtin) {
                    _context8.next = 28;
                    break;
                  }
                  return _context8.abrupt("return", expr);
                case 28:
                  if (!(extension instanceof Macro)) {
                    _context8.next = 37;
                    break;
                  }
                  _context8.next = 31;
                  return this.evaluate(expr);
                case 31:
                  result = _context8.sent;
                  if (!(result instanceof Pair || result instanceof LSymbol)) {
                    _context8.next = 34;
                    break;
                  }
                  return _context8.abrupt("return", Pair.fromArray([LSymbol('quote'), result]));
                case 34:
                  return _context8.abrupt("return", result);
                case 37:
                  throw new Error('Parse Error: invalid parser extension: ' + special.symbol);
                case 38:
                  ref = this.match_datum_ref(token);
                  if (!(ref !== null)) {
                    _context8.next = 44;
                    break;
                  }
                  this.skip();
                  if (!this._refs[ref]) {
                    _context8.next = 43;
                    break;
                  }
                  return _context8.abrupt("return", new DatumReference(ref, this._refs[ref]));
                case 43:
                  throw new Error("Parse Error: invalid datum label #".concat(ref, "#"));
                case 44:
                  ref_label = this.match_datum_label(token);
                  if (!(ref_label !== null)) {
                    _context8.next = 51;
                    break;
                  }
                  this.skip();
                  this._refs[ref_label] = this._read_object();
                  return _context8.abrupt("return", this._refs[ref_label]);
                case 51:
                  if (!this.is_close(token)) {
                    _context8.next = 55;
                    break;
                  }
                  this.skip();
                  // invalid state, we don't need to return anything
                  _context8.next = 61;
                  break;
                case 55:
                  if (!this.is_open(token)) {
                    _context8.next = 60;
                    break;
                  }
                  this.skip();
                  return _context8.abrupt("return", this.read_list());
                case 60:
                  return _context8.abrupt("return", this.read_value());
                case 61:
                case "end":
                  return _context8.stop();
              }
            }
          }, _callee8, this);
        }));
        function _read_object() {
          return _read_object3.apply(this, arguments);
        }
        return _read_object;
      }()
    }]);
    return Parser;
  }(); // ----------------------------------------------------------------------
  // :: Parser helper that handles circular list structures
  // :: using datum labels
  // ----------------------------------------------------------------------
  var DatumReference = /*#__PURE__*/function () {
    function DatumReference(name, data) {
      _classCallCheck(this, DatumReference);
      this.name = name;
      this.data = data;
    }
    _createClass(DatumReference, [{
      key: "valueOf",
      value: function valueOf() {
        return this.data;
      }
    }]);
    return DatumReference;
  }(); // ----------------------------------------------------------------------
  // :: Tokens are the array of strings from tokenizer
  // :: the return value is an array of lips code created out of Pair class.
  // :: env is needed for parser extensions that will invoke the function
  // :: or macro assigned to symbol, this function is async because
  // :: it evaluates the code, from parser extensions, that may return a promise.
  // ----------------------------------------------------------------------
  function parse(_x, _x2) {
    return _parse.apply(this, arguments);
  } // ----------------------------------------------------------------------
  function _parse() {
    _parse = _wrapAsyncGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee9(arg, env) {
      var parser, prev, expr;
      return _regeneratorRuntime.wrap(function _callee9$(_context9) {
        while (1) {
          switch (_context9.prev = _context9.next) {
            case 0:
              if (!env) {
                if (global_env) {
                  env = global_env.get('**interaction-environment**', {
                    throwError: false
                  });
                } else {
                  env = user_env;
                }
              }
              parser = new Parser(arg, {
                env: env
              });
            case 3:
              _context9.next = 6;
              return _awaitAsyncGenerator(parser.read_object());
            case 6:
              expr = _context9.sent;
              if (!parser.balanced()) {
                parser.ballancing_error(expr, prev);
              }
              if (!(expr === eof)) {
                _context9.next = 10;
                break;
              }
              return _context9.abrupt("break", 15);
            case 10:
              prev = expr;
              _context9.next = 13;
              return expr;
            case 13:
              _context9.next = 3;
              break;
            case 15:
            case "end":
              return _context9.stop();
          }
        }
      }, _callee9);
    }));
    return _parse.apply(this, arguments);
  }
  function unpromise(value) {
    var fn = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : function (x) {
      return x;
    };
    var error = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;
    if (is_promise(value)) {
      var ret = value.then(fn);
      if (error === null) {
        return ret;
      } else {
        return ret["catch"](error);
      }
    }
    if (value instanceof Array) {
      return unpromise_array(value, fn, error);
    }
    if (is_plain_object(value)) {
      return unpromise_object(value, fn, error);
    }
    return fn(value);
  }
  // ----------------------------------------------------------------------
  function unpromise_array(array, fn, error) {
    if (array.find(is_promise)) {
      return unpromise(promise_all(array), function (arr) {
        if (Object.isFrozen(array)) {
          Object.freeze(arr);
        }
        return fn(arr);
      }, error);
    }
    return fn(array);
  }
  // ----------------------------------------------------------------------
  function unpromise_object(object, fn, error) {
    var keys = Object.keys(object);
    var values = [],
      anyPromise = [];
    var i = keys.length;
    while (i--) {
      var key = keys[i];
      var value = object[key];
      values[i] = value;
      if (is_promise(value)) {
        anyPromise.push(value);
      }
    }
    if (anyPromise.length) {
      return unpromise(promise_all(values), function (values) {
        var result = {};
        values.forEach(function (value, i) {
          var key = keys[i];
          result[key] = value;
        });
        if (Object.isFrozen(object)) {
          Object.freeze(result);
        }
        return result;
      }, error);
    }
    return fn(object);
  }
  // ----------------------------------------------------------------------
  function read_only(object, property, value) {
    var _ref10 = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : {},
      _ref10$hidden = _ref10.hidden,
      hidden = _ref10$hidden === void 0 ? false : _ref10$hidden;
    Object.defineProperty(object, property, {
      value: value,
      configurable: true,
      enumerable: !hidden
    });
  }
  // ----------------------------------------------------------------------
  // :: Function similar to Array.from that work on async iterators
  // ----------------------------------------------------------------------
  function uniterate_async(_x5) {
    return _uniterate_async.apply(this, arguments);
  } // ----------------------------------------------------------------------
  // :: Function that return matcher function that match string against string
  // ----------------------------------------------------------------------
  function _uniterate_async() {
    _uniterate_async = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee22(object) {
      var result, _iteratorAbruptCompletion, _didIteratorError, _iteratorError, _iterator, _step, item;
      return _regeneratorRuntime.wrap(function _callee22$(_context22) {
        while (1) {
          switch (_context22.prev = _context22.next) {
            case 0:
              result = [];
              _iteratorAbruptCompletion = false;
              _didIteratorError = false;
              _context22.prev = 3;
              _iterator = _asyncIterator(object);
            case 5:
              _context22.next = 7;
              return _iterator.next();
            case 7:
              if (!(_iteratorAbruptCompletion = !(_step = _context22.sent).done)) {
                _context22.next = 13;
                break;
              }
              item = _step.value;
              result.push(item);
            case 10:
              _iteratorAbruptCompletion = false;
              _context22.next = 5;
              break;
            case 13:
              _context22.next = 19;
              break;
            case 15:
              _context22.prev = 15;
              _context22.t0 = _context22["catch"](3);
              _didIteratorError = true;
              _iteratorError = _context22.t0;
            case 19:
              _context22.prev = 19;
              _context22.prev = 20;
              if (!(_iteratorAbruptCompletion && _iterator["return"] != null)) {
                _context22.next = 24;
                break;
              }
              _context22.next = 24;
              return _iterator["return"]();
            case 24:
              _context22.prev = 24;
              if (!_didIteratorError) {
                _context22.next = 27;
                break;
              }
              throw _iteratorError;
            case 27:
              return _context22.finish(24);
            case 28:
              return _context22.finish(19);
            case 29:
              return _context22.abrupt("return", result);
            case 30:
            case "end":
              return _context22.stop();
          }
        }
      }, _callee22, null, [[3, 15, 19, 29], [20,, 24, 28]]);
    }));
    return _uniterate_async.apply(this, arguments);
  }
  function matcher(name, arg) {
    if (arg instanceof RegExp) {
      return function (x) {
        return String(x).match(arg);
      };
    } else if (is_function(arg)) {
      // it will always be function
      return arg;
    }
    throw new Error('Invalid matcher');
  }
  // ----------------------------------------------------------------------
  // :: Documentation decorator to LIPS functions if lines starts with :
  // :: they are ignored (not trimmed) otherwise it trims so
  // :: so you can have indent in source code
  // ----------------------------------------------------------------------
  function doc(name, fn, doc, dump) {
    if (typeof name !== 'string') {
      fn = arguments[0];
      doc = arguments[1];
      dump = arguments[2];
      name = null;
    }
    if (doc) {
      if (dump) {
        fn.__doc__ = doc;
      } else {
        fn.__doc__ = trim_lines(doc);
      }
    }
    if (name) {
      fn.__name__ = name;
    } else if (fn.name && !is_lambda(fn)) {
      fn.__name__ = fn.name;
    }
    return fn;
  }
  // ----------------------------------------------------------------------
  function trim_lines(string) {
    return string.split('\n').map(function (line) {
      return line.trim();
    }).join('\n');
  }
  // ----------------------------------------------------------------------
  // return last S-Expression
  // @param tokens - array of tokens (objects from tokenizer or strings)
  // @param sexp - number of expression to look behind
  // ----------------------------------------------------------------------
  function previousSexp(tokens) {
    var sexp = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 1;
    var i = tokens.length;
    if (sexp <= 0) {
      throw Error("previousSexp: Invalid argument sexp = ".concat(sexp));
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
  }
  // ----------------------------------------------------------------------
  // :: Find the number of spaces in line
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
  }
  // ----------------------------------------------------------------------
  // :: Token based pattern matching (used by formatter)
  // ----------------------------------------------------------------------
  /*
    Function nested_pattern(pattern) {
    return pattern instanceof Array ||
    pattern instanceof Pattern;
    }
  */
  // ----------------------------------------------------------------------
  function match(pattern, input) {
    return inner_match(pattern, input) === input.length;
    function inner_match(pattern, input) {
      /*
        function empty_match() {
        if (p <= 0 && i <= 0) {
        return false;
        }
        var prev_pattern = pattern[p - 1];
        if (!nested_pattern(prev_pattern)) {
        prev_pattern = [prev_pattern];
        }
        var next_pattern = pattern[p + 1];
        if (next_pattern && !nested_pattern(next_pattern)) {
        next_pattern = [next_pattern];
        }
        return match(prev_pattern, [input[i - 1]]) &&
        (!next_pattern || match(next_pattern, [input[i]]));
        }
      */
      function get_first_match(patterns, input) {
        var _iterator5 = _createForOfIteratorHelper(patterns),
          _step5;
        try {
          for (_iterator5.s(); !(_step5 = _iterator5.n()).done;) {
            var _p = _step5.value;
            var _m = inner_match(_p, input);
            if (_m !== -1) {
              return _m;
            }
          }
        } catch (err) {
          _iterator5.e(err);
        } finally {
          _iterator5.f();
        }
        return -1;
      }
      function not_symbol_match() {
        return pattern[p] === Symbol["for"]('symbol') && !is_symbol_string(input[i]);
      }
      function match_next() {
        var next_pattern = pattern[p + 1];
        var next_input = input[i + 1];
        if (next_pattern !== undefined && next_input !== undefined) {
          return inner_match([next_pattern], [next_input]);
        }
      }
      var p = 0;
      var glob = {};
      for (var i = 0; i < input.length; ++i) {
        if (typeof pattern[p] === 'undefined') {
          return i;
        }
        if (pattern[p] instanceof Pattern) {
          var m;
          if (['+', '*'].includes(pattern[p].flag)) {
            while (i < input.length) {
              m = get_first_match(pattern[p].patterns, input.slice(i));
              if (m === -1) {
                break;
              }
              i += m;
            }
            i -= 1;
            p++;
            continue;
          } else if (pattern[p].flag === '?') {
            m = get_first_match(pattern[p].patterns, input.slice(i));
            if (m === -1) {
              i -= 2; // if not found use same test on same input again
            } else {
              p++;
            }
            continue;
          }
        } else if (pattern[p] instanceof RegExp) {
          if (!input[i].match(pattern[p])) {
            return -1;
          }
        } else if (lips.LString.isString(pattern[p])) {
          if (pattern[p].valueOf() !== input[i]) {
            return -1;
          }
        } else if (_typeof(pattern[p]) === 'symbol') {
          if (pattern[p] === Symbol["for"]('*')) {
            // ignore S-expressions inside for case when next pattern is )
            glob[p] = glob[p] || 0;
            //var zero_match = empty_match();
            if (['(', '['].includes(input[i])) {
              glob[p]++;
            } else if ([')', ']'].includes(input[i])) {
              glob[p]--;
            }
            if (typeof pattern[p + 1] !== 'undefined' && glob[p] === 0 && match_next() === -1 || glob[p] > 0) {
              continue;
            }
          } else if (not_symbol_match()) {
            return -1;
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
  }
  // ----------------------------------------------------------------------
  // :: Code formatter class
  // :: based on http://community.schemewiki.org/?scheme-style
  // :: and GNU Emacs scheme mode
  // :: it rely on meta data from tokenizer function
  // ----------------------------------------------------------------------
  function Formatter(code) {
    this.__code__ = code.replace(/\r/g, '');
  }
  // ----------------------------------------------------------------------
  Formatter.defaults = {
    offset: 0,
    indent: 2,
    exceptions: {
      specials: [/* eslint-disable max-len */
      /^(?:#:)?(?:define(?:-values|-syntax|-macro|-class|-record-type)?|(?:call-with-(?:input-file|output-file|port))|lambda|let-env|try|catch|when|unless|while|syntax-rules|(let|letrec)(-syntax|\*)?)$/
      /* eslint-enable */],
      shift: {
        1: ['&', '#']
      }
    }
  };
  Formatter.match = match;
  // ----------------------------------------------------------------------
  // :: Return indent for next line
  // ----------------------------------------------------------------------
  Formatter.prototype._options = function _options(options) {
    var defaults = Formatter.defaults;
    if (typeof options === 'undefined') {
      return Object.assign({}, defaults);
    }
    var exceptions = options && options.exceptions || {};
    var specials = exceptions.specials || [];
    var shift = exceptions.shift || {
      1: []
    };
    return _objectSpread(_objectSpread(_objectSpread({}, defaults), options), {}, {
      exceptions: {
        specials: [].concat(_toConsumableArray(defaults.exceptions.specials), _toConsumableArray(specials)),
        shift: _objectSpread(_objectSpread({}, shift), {}, {
          1: [].concat(_toConsumableArray(defaults.exceptions.shift[1]), _toConsumableArray(shift[1]))
        })
      }
    });
  };
  // ----------------------------------------------------------------------
  Formatter.prototype.indent = function indent(options) {
    var tokens = tokenize(this.__code__, true);
    return this._indent(tokens, options);
  };
  // ----------------------------------------------------------------------
  Formatter.exception_shift = function (token, settings) {
    function match(list) {
      if (!list.length) {
        return false;
      }
      if (list.indexOf(token) !== -1) {
        return true;
      } else {
        var regexes = list.filter(function (s) {
          return s instanceof RegExp;
        });
        if (!regexes.length) {
          return false;
        }
        var _iterator6 = _createForOfIteratorHelper(regexes),
          _step6;
        try {
          for (_iterator6.s(); !(_step6 = _iterator6.n()).done;) {
            var re = _step6.value;
            if (token.match(re)) {
              return true;
            }
          }
        } catch (err) {
          _iterator6.e(err);
        } finally {
          _iterator6.f();
        }
      }
      return false;
    }
    if (match(settings.exceptions.specials)) {
      return settings.indent;
    }
    var shift = settings.exceptions.shift;
    for (var _i2 = 0, _Object$entries = Object.entries(shift); _i2 < _Object$entries.length; _i2++) {
      var _Object$entries$_i = _slicedToArray(_Object$entries[_i2], 2),
        indent = _Object$entries$_i[0],
        tokens = _Object$entries$_i[1];
      if (match(tokens)) {
        return +indent;
      }
    }
    return -1;
  };
  // ----------------------------------------------------------------------
  Formatter.prototype._indent = function _indent(tokens, options) {
    var settings = this._options(options);
    var spaces = lineIndent(tokens);
    var sexp = previousSexp(tokens);
    // one character before S-Expression
    var before_sexpr = tokens[tokens.length - sexp.length - 1];
    var last = tokens[tokens.length - 1];
    if (last.token.match(/^"[\S\s]+[^"]$/)) {
      return spaces + settings.indent;
    }
    if (sexp && sexp.length) {
      if (sexp[0].line > 0) {
        settings.offset = 0;
      }
      if (sexp.toString() === tokens.toString() && balanced(sexp)) {
        return settings.offset + sexp[0].col;
      } else if (sexp.length === 1) {
        return settings.offset + sexp[0].col + 1;
      } else {
        // search for token before S-Expression for case like #(10 or &(:x
        var exception = -1;
        if (before_sexpr) {
          var shift = Formatter.exception_shift(before_sexpr.token, settings);
          if (shift !== -1) {
            exception = shift;
          }
        }
        if (exception === -1) {
          exception = Formatter.exception_shift(sexp[1].token, settings);
        }
        if (exception !== -1) {
          return settings.offset + sexp[0].col + exception;
        } else if (sexp[0].line < sexp[1].line) {
          return settings.offset + sexp[0].col + 1;
        } else if (sexp.length > 3 && sexp[1].line === sexp[3].line) {
          if (sexp[1].token === '(' || sexp[1].token === '[') {
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
      }
    } else {
      return 0;
    }
    return spaces + settings.indent;
  };
  // ----------------------------------------------------------------------
  function Ahead(pattern) {
    this.pattern = pattern;
  }
  // TODO: make it print
  Ahead.prototype.toString = function () {
    return "#<pattern(".concat(this.pattern, ")>");
  };
  // ----------------------------------------------------------------------
  Ahead.prototype.match = function (string) {
    return string.match(this.pattern);
  };
  // ----------------------------------------------------------------------
  // Pattern has any number of patterns that it matches using OR operator
  // Pattern is in form of array with regular expressions
  // ----------------------------------------------------------------------
  function Pattern() {
    for (var _len3 = arguments.length, args = new Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
      args[_key3] = arguments[_key3];
    }
    var flag = args.pop();
    this.patterns = args;
    this.flag = flag;
  }
  Pattern.prototype.toString = function () {
    var patterns = this.patterns.map(function (x) {
      return toString(x);
    }).join('|');
    return "#<pattern(".concat(patterns, " ").concat(this.flag, ")>");
  };
  // ----------------------------------------------------------------------
  Formatter.Pattern = Pattern;
  Formatter.Ahead = Ahead;
  var p_o = /^[[(]$/;
  var p_e = /^[\])]$/;
  var not_p = /[^()[\]]/;
  var not_close = new Ahead(/[^)\]]/);
  //const open = new Ahead(/[([]/);
  var glob = Symbol["for"]('*');
  var sexp_or_atom = new Pattern([p_o, glob, p_e], [not_p], '+');
  var sexp = new Pattern([p_o, glob, p_e], '+');
  var symbol = new Pattern([Symbol["for"]('symbol')], '?');
  var symbols = new Pattern([Symbol["for"]('symbol')], '*');
  var identifiers = [p_o, symbols, p_e];
  var let_value = new Pattern([p_o, Symbol["for"]('symbol'), glob, p_e], '+');
  // rules for breaking S-Expressions into lines
  var def_lambda_re = keywords_re('define', 'lambda', 'define-macro', 'syntax-rules');
  /* eslint-disable max-len */
  var non_def = /^(?!.*\b(?:[()[\]]|define(?:-macro)?|let(?:\*|rec|-env|-syntax|)?|lambda|syntax-rules)\b).*$/;
  /* eslint-enable */
  var let_re = /^(?:#:)?(let(?:\*|rec|-env|-syntax)?)$/;
  // match keyword if it's normal token or gensym (prefixed with #:)
  function keywords_re() {
    for (var _len4 = arguments.length, args = new Array(_len4), _key4 = 0; _key4 < _len4; _key4++) {
      args[_key4] = arguments[_key4];
    }
    return new RegExp("^(?:#:)?(?:".concat(args.join('|'), ")$"));
  }
  // line breaking rules
  Formatter.rules = [[[sexp], 0, not_close], [[p_o, keywords_re('begin', 'cond-expand')], 1], [[p_o, let_re, symbol, p_o, let_value, p_e], 1], [[p_o, let_re, symbol, sexp_or_atom], 1, not_close], [[p_o, let_re, p_o, let_value], 1, not_close],
  //--[[p_o, keywords_re('define-syntax'), /.+/], 1],
  [[p_o, non_def, new Pattern([/[^()[\]]/], '+'), sexp], 1, not_close], [[p_o, sexp], 1, not_close], [[p_o, not_p, sexp], 1, not_close], [[p_o, keywords_re('lambda', 'if'), not_p], 1, not_close], [[p_o, keywords_re('while'), not_p, sexp], 1, not_close], [[p_o, keywords_re('if'), not_p, glob], 1], [[p_o, def_lambda_re, identifiers], 0, not_close], [[p_o, def_lambda_re, identifiers, string_re], 0, not_close], [[p_o, def_lambda_re, identifiers, string_re, sexp], 0, not_close], [[p_o, def_lambda_re, identifiers, sexp], 0, not_close]];
  // ----------------------------------------------------------------------
  Formatter.prototype["break"] = function () {
    var code = this.__code__.replace(/\n[ \t]*/g, '\n ').replace(/^\s+/, '');
    // function that work when calling tokenize with meta data or not
    var token = function token(t) {
      if (t.token.match(string_re) || t.token.match(re_re)) {
        return t.token;
      } else {
        return t.token.replace(/\s+/, ' ');
      }
    };
    var first_token_index = function first_token_index(tokens) {
      for (var i = tokens.length; i--;) {
        var _token = tokens[i];
        if (_token.trim() && !is_special(_token)) {
          return tokens.length - i - 1;
        }
      }
    };
    // Tokenize is part of the parser/lexer that split code into tokens and includes
    // meta data like number of column or line
    var tokens = tokenize(code, true).map(token).filter(function (t) {
      return t !== '\n';
    });
    var rules = Formatter.rules;
    outer: for (var i = 1; i < tokens.length; ++i) {
      if (!tokens[i].trim()) {
        continue;
      }
      var sub = tokens.slice(0, i);
      var sexp = {};
      rules.map(function (b) {
        return b[1];
      }).forEach(function (count) {
        count = count.valueOf();
        // some patterns require to check what was before like
        // if inside let binding
        if (count > 0 && !sexp[count]) {
          sexp[count] = previousSexp(sub, count);
        }
      });
      var _iterator7 = _createForOfIteratorHelper(rules),
        _step7;
      try {
        for (_iterator7.s(); !(_step7 = _iterator7.n()).done;) {
          var _step7$value = _slicedToArray(_step7.value, 3),
            pattern = _step7$value[0],
            count = _step7$value[1],
            ext = _step7$value[2];
          count = count.valueOf();
          // 0 count mean ignore the previous S-Expression
          var test_sexp = count > 0 ? sexp[count] : sub;
          var input = test_sexp.filter(function (t) {
            return t.trim() && !is_special(t);
          });
          var inc = first_token_index(test_sexp);
          var m = match(pattern, input);
          var next = tokens.slice(i).find(function (t) {
            return t.trim() && !is_special(t);
          });
          if (m && (ext instanceof Ahead && ext.match(next) || !ext)) {
            var index = i - inc;
            if (tokens[index] !== '\n') {
              if (!tokens[index].trim()) {
                tokens[index] = '\n';
              } else {
                tokens.splice(index, 0, '\n');
                i++;
              }
            }
            i += inc;
            continue outer;
          }
        }
      } catch (err) {
        _iterator7.e(err);
      } finally {
        _iterator7.f();
      }
    }
    this.__code__ = tokens.join('');
    return this;
  };
  // ----------------------------------------------------------------------
  Formatter.prototype._spaces = function (i) {
    return new Array(i + 1).join(' ');
  };
  // ----------------------------------------------------------------------
  // :: Auto formatting of code, it requires to have newlines
  // ----------------------------------------------------------------------
  Formatter.prototype.format = function format(options) {
    // prepare code with single space after newline
    // so we have space token to align
    var code = this.__code__.replace(/[ \t]*\n[ \t]*/g, '\n ');
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
          tokens[i + 1].token = this._spaces(indent);
          // because we have single space as initial indent
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
  };
  // ----------------------------------------------------------------------
  // :: Flatten nested arrays
  // :: ref: https://stackoverflow.com/a/27282907/387194
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
  }
  // ----------------------------------------------------------------------
  // :: Fisher-Yates (aka Knuth) Shuffle
  // :: ref: https://stackoverflow.com/a/2450976/387194
  // ----------------------------------------------------------------------
  function shuffle(array, random) {
    var currentIndex = array.length,
      randomIndex;

    // While there remain elements to shuffle.
    while (currentIndex > 0) {
      // Pick a remaining element.
      randomIndex = Math.floor(random() * currentIndex);
      currentIndex--;

      // And swap it with the current element.
      var _ref11 = [array[randomIndex], array[currentIndex]];
      array[currentIndex] = _ref11[0];
      array[randomIndex] = _ref11[1];
    }
    return array;
  }
  // ----------------------------------------------------------------------
  // :: Nil constructor with only once instance
  // ----------------------------------------------------------------------
  function Nil() {}
  Nil.prototype.toString = function () {
    return '()';
  };
  Nil.prototype.valueOf = function () {
    return undefined;
  };
  Nil.prototype.serialize = function () {
    return 0;
  };
  Nil.prototype.to_object = function () {
    return {};
  };
  Nil.prototype.append = function (x) {
    return new Pair(x, _nil);
  };
  Nil.prototype.to_array = function () {
    return [];
  };
  var _nil = new Nil();
  // ----------------------------------------------------------------------
  // :: Pair constructor
  // ----------------------------------------------------------------------
  function Pair(car, cdr) {
    if (typeof this !== 'undefined' && this.constructor !== Pair || typeof this === 'undefined') {
      return new Pair(car, cdr);
    }
    this.car = car;
    this.cdr = cdr;
  }
  // ----------------------------------------------------------------------
  function to_array(name, deep) {
    return function recur(list) {
      typecheck(name, list, ['pair', 'nil']);
      if (list === _nil) {
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
        } else if (node === _nil) {
          break;
        } else {
          throw new Error("".concat(name, ": can't convert improper list"));
        }
      }
      return result;
    };
  }
  // ----------------------------------------------------------------------
  Pair.prototype.flatten = function () {
    return Pair.fromArray(flatten(this.to_array()));
  };
  // ----------------------------------------------------------------------
  Pair.prototype.length = function () {
    var len = 0;
    var node = this;
    while (true) {
      if (!node || node === _nil || !(node instanceof Pair) || node.haveCycles('cdr')) {
        break;
      }
      len++;
      node = node.cdr;
    }
    return len;
  };
  // ----------------------------------------------------------------------
  Pair.match = function (obj, item) {
    if (obj instanceof LSymbol) {
      return LSymbol.is(obj, item);
    } else if (obj instanceof Pair) {
      return Pair.match(obj.car, item) || Pair.match(obj.cdr, item);
    } else if (Array.isArray(obj)) {
      return obj.some(function (x) {
        return Pair.match(x, item);
      });
    } else if (is_plain_object(obj)) {
      return Object.values(obj).some(function (x) {
        return Pair.match(x, item);
      });
    }
    return false;
  };
  // ----------------------------------------------------------------------
  Pair.prototype.find = function (item) {
    return Pair.match(this, item);
  };

  // ----------------------------------------------------------------------
  Pair.prototype.clone = function () {
    var deep = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : true;
    var visited = new Map();
    function clone(node) {
      if (node instanceof Pair) {
        if (visited.has(node)) {
          return visited.get(node);
        }
        var pair = new Pair();
        visited.set(node, pair);
        if (deep) {
          pair.car = clone(node.car);
        } else {
          pair.car = node.car;
        }
        pair.cdr = clone(node.cdr);
        pair[__cycles__] = node[__cycles__];
        return pair;
      }
      return node;
    }
    return clone(this);
  };

  // ----------------------------------------------------------------------
  Pair.prototype.last_pair = function () {
    var node = this;
    while (true) {
      if (node.cdr === _nil) {
        return node;
      }
      node = node.cdr;
    }
  };

  // ----------------------------------------------------------------------
  Pair.prototype.to_array = function () {
    var deep = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : true;
    var result = [];
    if (this.car instanceof Pair) {
      if (deep) {
        result.push(this.car.to_array());
      } else {
        result.push(this.car);
      }
    } else {
      result.push(this.car.valueOf());
    }
    if (this.cdr instanceof Pair) {
      result = result.concat(this.cdr.to_array(deep));
    }
    return result;
  };

  // ----------------------------------------------------------------------
  Pair.fromArray = function (array) {
    var deep = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : true;
    var quote = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;
    if (array instanceof Pair || quote && array instanceof Array && array[__data__]) {
      return array;
    }
    if (deep === false) {
      var list = _nil;
      for (var _i3 = array.length; _i3--;) {
        list = new Pair(array[_i3], list);
      }
      return list;
    }
    if (array.length && !(array instanceof Array)) {
      array = _toConsumableArray(array);
    }
    var result = _nil;
    var i = array.length;
    while (i--) {
      var car = array[i];
      if (car instanceof Array) {
        car = Pair.fromArray(car, deep, quote);
      } else if (typeof car === 'string') {
        car = LString(car);
      } else if (typeof car === 'number' && !Number.isNaN(car)) {
        car = LNumber(car);
      }
      result = new Pair(car, result);
    }
    return result;
  };

  // ----------------------------------------------------------------------
  // By default to_object was created to create JavaScript objects,
  // so it uses valueOf to get native values.
  // Literal parameter was a hack to allow creating LComplex from LIPS code
  // ----------------------------------------------------------------------
  Pair.prototype.to_object = function () {
    var literal = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : false;
    var node = this;
    var result = {};
    while (true) {
      if (node instanceof Pair && node.car instanceof Pair) {
        var pair = node.car;
        var name = pair.car;
        if (name instanceof LSymbol) {
          name = name.__name__;
        }
        if (name instanceof LString) {
          name = name.valueOf();
        }
        var cdr = pair.cdr;
        if (cdr instanceof Pair) {
          cdr = cdr.to_object(literal);
        }
        if (is_native(cdr)) {
          if (!literal) {
            cdr = cdr.valueOf();
          }
        }
        result[name] = cdr;
        node = node.cdr;
      } else {
        break;
      }
    }
    return result;
  };

  // ----------------------------------------------------------------------
  Pair.fromPairs = function (array) {
    return array.reduce(function (list, pair) {
      return new Pair(new Pair(new LSymbol(pair[0]), pair[1]), list);
    }, _nil);
  };

  // ----------------------------------------------------------------------
  Pair.fromObject = function (obj) {
    var array = Object.keys(obj).map(function (key) {
      return [key, obj[key]];
    });
    return Pair.fromPairs(array);
  };

  // ----------------------------------------------------------------------
  Pair.prototype.reduce = function (fn) {
    var node = this;
    var result = _nil;
    while (true) {
      if (node !== _nil) {
        result = fn(result, node.car);
        node = node.cdr;
      } else {
        break;
      }
    }
    return result;
  };

  // ----------------------------------------------------------------------
  Pair.prototype.reverse = function () {
    if (this.haveCycles()) {
      throw new Error("You can't reverse list that have cycles");
    }
    var node = this;
    var prev = _nil;
    while (node !== _nil) {
      var next = node.cdr;
      node.cdr = prev;
      prev = node;
      node = next;
    }
    return prev;
  };

  // ----------------------------------------------------------------------
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
  };

  // ----------------------------------------------------------------------
  Pair.prototype.map = function (fn) {
    if (typeof this.car !== 'undefined') {
      return new Pair(fn(this.car), this.cdr === _nil ? _nil : this.cdr.map(fn));
    } else {
      return _nil;
    }
  };
  var repr = new Map();
  // ----------------------------------------------------------------------
  function is_plain_object(object) {
    return object && _typeof(object) === 'object' && object.constructor === Object;
  }
  // ----------------------------------------------------------------------
  var props = Object.getOwnPropertyNames(Array.prototype);
  var array_methods = [];
  props.forEach(function (x) {
    array_methods.push(Array[x], Array.prototype[x]);
  });
  // ----------------------------------------------------------------------
  function is_array_method(x) {
    x = unbind(x);
    return array_methods.includes(x);
  }
  // ----------------------------------------------------------------------
  function is_lips_function(x) {
    return is_function(x) && (is_lambda(x) || x.__doc__);
  }
  // ----------------------------------------------------------------------
  function user_repr(obj) {
    var constructor = obj.constructor || Object;
    var plain_object = is_plain_object(obj);
    var iterator = is_function(obj[Symbol.asyncIterator]) || is_function(obj[Symbol.iterator]);
    var fn;
    if (repr.has(constructor)) {
      fn = repr.get(constructor);
    } else {
      repr.forEach(function (value, key) {
        key = unbind(key);
        // if key is Object it should only work for plain_object
        // because otherwise it will match every object
        // we don't use instanceof so it don't work for subclasses
        if (obj.constructor === key && (key === Object && plain_object && !iterator || key !== Object)) {
          fn = value;
        }
      });
    }
    return fn;
  }
  // ----------------------------------------------------------------------
  var str_mapping = new Map();
  [[true, '#t'], [false, '#f'], [null, 'null'], [undefined, '#<undefined>']].forEach(function (_ref12) {
    var _ref13 = _slicedToArray(_ref12, 2),
      key = _ref13[0],
      value = _ref13[1];
    str_mapping.set(key, value);
  });
  // ----------------------------------------------------------------------
  // :: Debug function that can be used with JSON.stringify
  // :: that will show symbols
  // ----------------------------------------------------------------------
  /* c8 ignore next 22 */
  function symbolize(obj) {
    if (obj && _typeof(obj) === 'object') {
      var result = {};
      var _symbols = Object.getOwnPropertySymbols(obj);
      _symbols.forEach(function (key) {
        var name = key.toString().replace(/Symbol\(([^)]+)\)/, '$1');
        result[name] = toString(obj[key]);
      });
      var _props = Object.getOwnPropertyNames(obj);
      _props.forEach(function (key) {
        var o = obj[key];
        if (o && _typeof(o) === 'object' && o.constructor === Object) {
          result[key] = symbolize(o);
        } else {
          result[key] = toString(o);
        }
      });
      return result;
    }
    return obj;
  }
  // ----------------------------------------------------------------------
  function get_props(obj) {
    return Object.keys(obj).concat(Object.getOwnPropertySymbols(obj));
  }
  // ----------------------------------------------------------------------
  function has_own_function(obj, name) {
    return obj.hasOwnProperty(name) && is_function(obj.toString);
  }
  // ----------------------------------------------------------------------
  function function_to_string(fn) {
    if (is_native_function(fn)) {
      return '#<procedure(native)>';
    }
    var constructor = fn.prototype && fn.prototype.constructor;
    if (is_function(constructor) && is_lambda(constructor)) {
      if (fn[__class__] && constructor.hasOwnProperty('__name__')) {
        var name = constructor.__name__;
        if (LString.isString(name)) {
          name = name.toString();
          return "#<class:".concat(name, ">");
        }
        return '#<class>';
      }
    }
    if (fn.hasOwnProperty('__name__')) {
      var _name = fn.__name__;
      if (_typeof(_name) === 'symbol') {
        _name = symbol_to_string(_name);
      }
      if (typeof _name === 'string') {
        return "#<procedure:".concat(_name, ">");
      }
    }
    if (has_own_function(fn, 'toString')) {
      return fn.toString();
    } else if (fn.name && !is_lambda(fn)) {
      return "#<procedure:".concat(fn.name.trim(), ">");
    } else {
      return '#<procedure>';
    }
  }
  // ----------------------------------------------------------------------
  // Instances extracted to make cyclomatic complexity of toString smaller
  var instances = new Map();
  // ----------------------------------------------------------------------
  [[Error, function (e) {
    return e.message;
  }], [Pair, function (pair, _ref14) {
    var quote = _ref14.quote,
      skip_cycles = _ref14.skip_cycles,
      pair_args = _ref14.pair_args;
    // make sure that repr directly after update set the cycle ref
    if (!skip_cycles) {
      pair.markCycles();
    }
    return pair.toString.apply(pair, [quote].concat(_toConsumableArray(pair_args)));
  }], [LCharacter, function (chr, _ref15) {
    var quote = _ref15.quote;
    if (quote) {
      return chr.toString();
    }
    return chr.valueOf();
  }], [LString, function (str, _ref16) {
    var quote = _ref16.quote;
    str = str.toString();
    if (quote) {
      return JSON.stringify(str).replace(/\\n/g, '\n');
    }
    return str;
  }], [RegExp, function (re) {
    return '#' + re.toString();
  }]].forEach(function (_ref17) {
    var _ref18 = _slicedToArray(_ref17, 2),
      cls = _ref18[0],
      fn = _ref18[1];
    instances.set(cls, fn);
  });
  // ----------------------------------------------------------------------
  var native_types = [LSymbol, LNumber, Macro, Values, InputPort, OutputPort, Environment, QuotedPromise];
  // ----------------------------------------------------------------------
  function toString(obj, quote, skip_cycles) {
    if (typeof jQuery !== 'undefined' && obj instanceof jQuery.fn.init) {
      return '#<jQuery(' + obj.length + ')>';
    }
    if (str_mapping.has(obj)) {
      return str_mapping.get(obj);
    }
    if (is_prototype(obj)) {
      return '#<prototype>';
    }
    if (obj) {
      var cls = obj.constructor;
      if (instances.has(cls)) {
        for (var _len5 = arguments.length, pair_args = new Array(_len5 > 3 ? _len5 - 3 : 0), _key5 = 3; _key5 < _len5; _key5++) {
          pair_args[_key5 - 3] = arguments[_key5];
        }
        return instances.get(cls)(obj, {
          quote: quote,
          skip_cycles: skip_cycles,
          pair_args: pair_args
        });
      }
    }
    // standard objects that have toString
    var _iterator8 = _createForOfIteratorHelper(native_types),
      _step8;
    try {
      for (_iterator8.s(); !(_step8 = _iterator8.n()).done;) {
        var _type2 = _step8.value;
        if (obj instanceof _type2) {
          return obj.toString(quote);
        }
      }
      // constants
    } catch (err) {
      _iterator8.e(err);
    } finally {
      _iterator8.f();
    }
    if ([_nil, eof].includes(obj)) {
      return obj.toString();
    }
    if (is_function(obj)) {
      return function_to_string(obj);
    }
    if (obj === root) {
      return '#<js:global>';
    }
    if (obj === null) {
      return 'null';
    }
    if (_typeof(obj) === 'object') {
      var constructor = obj.constructor;
      if (!constructor) {
        // This is case of fs.constants in Node.js that is null constructor object.
        // This object can be handled like normal objects that have properties
        constructor = Object;
      }
      var name;
      if (typeof constructor.__class__ === 'string') {
        name = constructor.__class__;
      } else {
        var fn = user_repr(obj);
        if (fn) {
          if (is_function(fn)) {
            return fn(obj, quote);
          } else {
            throw new Error('toString: Invalid repr value');
          }
        }
        name = constructor.name;
      }
      // user defined representation
      if (is_function(obj.toString) && is_lambda(obj.toString)) {
        return obj.toString().valueOf();
      }
      if (type(obj) === 'instance') {
        if (is_lambda(constructor) && constructor.__name__) {
          name = constructor.__name__.valueOf();
        } else if (!is_native_function(constructor)) {
          name = 'instance';
        }
      }
      if (is_iterator(obj, Symbol.iterator)) {
        if (name) {
          return "#<iterator(".concat(name, ")>");
        }
        return '#<iterator>';
      }
      if (is_iterator(obj, Symbol.asyncIterator)) {
        if (name) {
          return "#<asyncIterator(".concat(name, ")>");
        }
        return '#<asyncIterator>';
      }
      if (name !== '') {
        return '#<' + name + '>';
      }
      return '#<Object>';
    }
    if (typeof obj !== 'string') {
      return obj.toString();
    }
    return obj;
  }
  // ----------------------------------------------------------------------------
  function is_prototype(obj) {
    return obj && _typeof(obj) === 'object' && obj.hasOwnProperty && obj.hasOwnProperty("constructor") && typeof obj.constructor === "function" && obj.constructor.prototype === obj;
  }
  // ----------------------------------------------------------------------------
  Pair.prototype.markCycles = function () {
    markCycles(this);
    return this;
  };

  // ----------------------------------------------------------------------------
  Pair.prototype.haveCycles = function () {
    var name = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;
    if (!name) {
      return this.haveCycles('car') || this.haveCycles('cdr');
    }
    return !!(this[__cycles__] && this[__cycles__][name]);
  };

  // ----------------------------------------------------------------------------
  function markCycles(pair) {
    var seen_pairs = [];
    var cycles = [];
    var refs = [];
    function visit(pair) {
      if (!seen_pairs.includes(pair)) {
        seen_pairs.push(pair);
      }
    }
    function set(node, type, child, parents) {
      if (child instanceof Pair) {
        if (parents.includes(child)) {
          if (!refs.includes(child)) {
            refs.push(child);
          }
          if (!node[__cycles__]) {
            node[__cycles__] = {};
          }
          node[__cycles__][type] = child;
          if (!cycles.includes(node)) {
            cycles.push(node);
          }
          return true;
        }
      }
    }
    var detect = trampoline(function detect_thunk(pair, parents) {
      if (pair instanceof Pair) {
        delete pair.ref;
        delete pair[__cycles__];
        visit(pair);
        parents.push(pair);
        var car = set(pair, 'car', pair.car, parents);
        var cdr = set(pair, 'cdr', pair.cdr, parents);
        if (!car) {
          detect(pair.car, parents.slice());
        }
        if (!cdr) {
          return new Thunk(function () {
            return detect_thunk(pair.cdr, parents.slice());
          });
        }
      }
    });
    function mark_node(node, type) {
      if (node[__cycles__][type] instanceof Pair) {
        var count = ref_nodes.indexOf(node[__cycles__][type]);
        node[__cycles__][type] = "#".concat(count, "#");
      }
    }
    detect(pair, []);
    var ref_nodes = seen_pairs.filter(function (node) {
      return refs.includes(node);
    });
    ref_nodes.forEach(function (node, i) {
      node[__ref__] = "#".concat(i, "=");
    });
    cycles.forEach(function (node) {
      mark_node(node, 'car');
      mark_node(node, 'cdr');
    });
  }

  // ----------------------------------------------------------------------
  Pair.prototype.toString = function (quote) {
    var _ref19 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
      _ref19$nested = _ref19.nested,
      nested = _ref19$nested === void 0 ? false : _ref19$nested;
    var arr = [];
    if (this[__ref__]) {
      arr.push(this[__ref__] + '(');
    } else if (!nested) {
      arr.push('(');
    }
    var value;
    if (this[__cycles__] && this[__cycles__].car) {
      value = this[__cycles__].car;
    } else {
      value = toString(this.car, quote, true);
    }
    if (value !== undefined) {
      arr.push(value);
    }
    if (this.cdr instanceof Pair) {
      if (this[__cycles__] && this[__cycles__].cdr) {
        arr.push(' . ');
        arr.push(this[__cycles__].cdr);
      } else {
        if (this.cdr[__ref__]) {
          arr.push(' . ');
        } else {
          arr.push(' ');
        }
        var cdr = this.cdr.toString(quote, {
          nested: true
        });
        arr.push(cdr);
      }
    } else if (this.cdr !== _nil) {
      arr = arr.concat([' . ', toString(this.cdr, quote, true)]);
    }
    if (!nested || this[__ref__]) {
      arr.push(')');
    }
    return arr.join('');
  };

  // ----------------------------------------------------------------------
  Pair.prototype.set = function (prop, value) {
    this[prop] = value;
    if (value instanceof Pair) {
      this.markCycles();
    }
  };

  // ----------------------------------------------------------------------
  Pair.prototype.append = function (arg) {
    if (arg instanceof Array) {
      return this.append(Pair.fromArray(arg));
    }
    var p = this;
    if (p.car === undefined) {
      if (arg instanceof Pair) {
        this.car = arg.car;
        this.cdr = arg.cdr;
      } else {
        this.car = arg;
      }
    } else if (arg !== _nil) {
      while (true) {
        if (p instanceof Pair && p.cdr !== _nil) {
          p = p.cdr;
        } else {
          break;
        }
      }
      p.cdr = arg;
    }
    return this;
  };
  // ----------------------------------------------------------------------
  Pair.prototype.serialize = function () {
    return [this.car, this.cdr];
  };
  // ----------------------------------------------------------------------
  // :: List iterator (for do-iterator macro)
  // ----------------------------------------------------------------------
  Pair.prototype[Symbol.iterator] = function () {
    var node = this;
    return {
      next: function next() {
        var cur = node;
        node = cur.cdr;
        if (cur === _nil) {
          return {
            value: undefined,
            done: true
          };
        } else {
          return {
            value: cur.car,
            done: false
          };
        }
      }
    };
  };
  // ----------------------------------------------------------------------
  // :: abs that work on BigInt
  // ----------------------------------------------------------------------
  function abs(x) {
    return x < 0 ? -x : x;
  }
  // ----------------------------------------------------------------------
  function seq_compare(fn, args) {
    var _args11 = _toArray(args),
      a = _args11[0],
      rest = _args11.slice(1);
    while (rest.length > 0) {
      var _rest = rest,
        _rest2 = _slicedToArray(_rest, 1),
        b = _rest2[0];
      if (!fn(a, b)) {
        return false;
      }
      var _rest3 = rest;
      var _rest4 = _toArray(_rest3);
      a = _rest4[0];
      rest = _rest4.slice(1);
    }
    return true;
  }

  // ----------------------------------------------------------------------
  function equal(x, y) {
    if (is_function(x)) {
      return is_function(y) && unbind(x) === unbind(y);
    } else if (x instanceof LNumber) {
      if (!(y instanceof LNumber)) {
        return false;
      }
      var _type3;
      if (x.__type__ === y.__type__) {
        if (x.__type__ === 'complex') {
          _type3 = x.__im__.__type__ === y.__im__.__type__ && x.__re__.__type__ === y.__re__.__type__;
        } else {
          _type3 = true;
        }
        if (_type3 && x.cmp(y) === 0) {
          if (x.valueOf() === 0) {
            return Object.is(x.valueOf(), y.valueOf());
          }
          return true;
        }
      }
      return false;
    } else if (typeof x === 'number') {
      if (typeof y !== 'number') {
        return false;
      }
      if (Number.isNaN(x)) {
        return Number.isNaN(y);
      }
      if (x === Number.NEGATIVE_INFINITY) {
        return y === Number.NEGATIVE_INFINITY;
      }
      if (x === Number.POSITIVE_INFINITY) {
        return y === Number.POSITIVE_INFINITY;
      }
      return equal(LNumber(x), LNumber(y));
    } else if (x instanceof LCharacter) {
      if (!(y instanceof LCharacter)) {
        return false;
      }
      return x.__char__ === y.__char__;
    } else {
      return x === y;
    }
  }
  // ----------------------------------------------------------------------
  function same_atom(a, b) {
    if (type(a) !== type(b)) {
      return false;
    }
    if (!is_atom(a)) {
      return false;
    }
    if (a instanceof RegExp) {
      return a.source === b.source;
    }
    if (a instanceof LString) {
      return a.valueOf() === b.valueOf();
    }
    return equal(a, b);
  }
  // ----------------------------------------------------------------------
  function is_atom(obj) {
    return obj instanceof LSymbol || LString.isString(obj) || obj === _nil || obj === null || obj instanceof LCharacter || obj instanceof LNumber || obj === true || obj === false;
  }
  // ----------------------------------------------------------------------
  var truncate = function () {
    if (Math.trunc) {
      return Math.trunc;
    } else {
      return function (x) {
        if (x === 0) {
          return 0;
        } else if (x < 0) {
          return Math.ceil(x);
        } else {
          return Math.floor(x);
        }
      };
    }
  }();
  // ----------------------------------------------------------------------
  // :: Macro constructor
  // ----------------------------------------------------------------------
  function Macro(name, fn, doc, dump) {
    if (typeof this !== 'undefined' && this.constructor !== Macro || typeof this === 'undefined') {
      return new Macro(name, fn);
    }
    typecheck('Macro', name, 'string', 1);
    typecheck('Macro', fn, 'function', 2);
    if (doc) {
      if (dump) {
        this.__doc__ = doc;
      } else {
        this.__doc__ = trim_lines(doc);
      }
    }
    this.__name__ = name;
    this.__fn__ = fn;
  }
  // ----------------------------------------------------------------------
  Macro.defmacro = function (name, fn, doc, dump) {
    var macro = new Macro(name, fn, doc, dump);
    macro.__defmacro__ = true;
    return macro;
  };
  // ----------------------------------------------------------------------
  Macro.prototype.invoke = function (code, _ref20, macro_expand) {
    var env = _ref20.env,
      rest = _objectWithoutProperties(_ref20, _excluded2);
    var args = _objectSpread(_objectSpread({}, rest), {}, {
      macro_expand: macro_expand
    });
    var result = this.__fn__.call(env, code, args, this.__name__);
    return result;
    //return macro_expand ? quote(result) : result;
  };
  // ----------------------------------------------------------------------
  Macro.prototype.toString = function () {
    return "#<macro:".concat(this.__name__, ">");
  };
  // ----------------------------------------------------------------------
  var macro = 'define-macro';
  // ----------------------------------------------------------------------
  var recur_guard = -10000;
  function macro_expand(single) {
    return /*#__PURE__*/function () {
      var _ref21 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee12(code, args) {
        var env, bindings, let_macros, lambda, define, is_let_macro, is_procedure, is_lambda, proc_bindings, let_binding, is_macro, expand_let_binding, _expand_let_binding, traverse, _traverse;
        return _regeneratorRuntime.wrap(function _callee12$(_context12) {
          while (1) {
            switch (_context12.prev = _context12.next) {
              case 0:
                _traverse = function _traverse3() {
                  _traverse = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee11(node, n, env) {
                    var name, value, is_let, is_binding, second, code, result, _result, expr, scope, car, cdr, pair;
                    return _regeneratorRuntime.wrap(function _callee11$(_context11) {
                      while (1) {
                        switch (_context11.prev = _context11.next) {
                          case 0:
                            if (!(node instanceof Pair && node.car instanceof LSymbol)) {
                              _context11.next = 50;
                              break;
                            }
                            if (!node[__data__]) {
                              _context11.next = 3;
                              break;
                            }
                            return _context11.abrupt("return", node);
                          case 3:
                            name = node.car.valueOf();
                            value = env.get(node.car, {
                              throwError: false
                            });
                            is_let = is_let_macro(node.car);
                            is_binding = is_let || is_procedure(value, node) || is_lambda(value);
                            if (!(is_binding && node.cdr.car instanceof Pair)) {
                              _context11.next = 28;
                              break;
                            }
                            if (!is_let) {
                              _context11.next = 15;
                              break;
                            }
                            bindings = let_binding(node.cdr.car);
                            _context11.next = 12;
                            return expand_let_binding(node.cdr.car, n);
                          case 12:
                            second = _context11.sent;
                            _context11.next = 17;
                            break;
                          case 15:
                            bindings = proc_bindings(node.cdr.car);
                            second = node.cdr.car;
                          case 17:
                            _context11.t0 = Pair;
                            _context11.t1 = node.car;
                            _context11.t2 = Pair;
                            _context11.t3 = second;
                            _context11.next = 23;
                            return traverse(node.cdr.cdr, n, env);
                          case 23:
                            _context11.t4 = _context11.sent;
                            _context11.t5 = new _context11.t2(_context11.t3, _context11.t4);
                            return _context11.abrupt("return", new _context11.t0(_context11.t1, _context11.t5));
                          case 28:
                            if (!is_macro(name, value)) {
                              _context11.next = 50;
                              break;
                            }
                            code = value instanceof Syntax ? node : node.cdr;
                            _context11.next = 32;
                            return value.invoke(code, _objectSpread(_objectSpread({}, args), {}, {
                              env: env
                            }), true);
                          case 32:
                            result = _context11.sent;
                            if (!(value instanceof Syntax)) {
                              _context11.next = 41;
                              break;
                            }
                            _result = result, expr = _result.expr, scope = _result.scope;
                            if (!(expr instanceof Pair)) {
                              _context11.next = 40;
                              break;
                            }
                            if (!(n !== -1 && n <= 1 || n < recur_guard)) {
                              _context11.next = 38;
                              break;
                            }
                            return _context11.abrupt("return", expr);
                          case 38:
                            if (n !== -1) {
                              n = n - 1;
                            }
                            return _context11.abrupt("return", traverse(expr, n, scope));
                          case 40:
                            result = expr;
                          case 41:
                            if (!(result instanceof LSymbol)) {
                              _context11.next = 43;
                              break;
                            }
                            return _context11.abrupt("return", quote(result));
                          case 43:
                            if (!(result instanceof Pair)) {
                              _context11.next = 48;
                              break;
                            }
                            if (!(n !== -1 && n <= 1 || n < recur_guard)) {
                              _context11.next = 46;
                              break;
                            }
                            return _context11.abrupt("return", result);
                          case 46:
                            if (n !== -1) {
                              n = n - 1;
                            }
                            return _context11.abrupt("return", traverse(result, n, env));
                          case 48:
                            if (!is_atom(result)) {
                              _context11.next = 50;
                              break;
                            }
                            return _context11.abrupt("return", result);
                          case 50:
                            // TODO: CYCLE DETECT
                            car = node.car;
                            if (!(car instanceof Pair)) {
                              _context11.next = 55;
                              break;
                            }
                            _context11.next = 54;
                            return traverse(car, n, env);
                          case 54:
                            car = _context11.sent;
                          case 55:
                            cdr = node.cdr;
                            if (!(cdr instanceof Pair)) {
                              _context11.next = 60;
                              break;
                            }
                            _context11.next = 59;
                            return traverse(cdr, n, env);
                          case 59:
                            cdr = _context11.sent;
                          case 60:
                            pair = new Pair(car, cdr);
                            return _context11.abrupt("return", pair);
                          case 62:
                          case "end":
                            return _context11.stop();
                        }
                      }
                    }, _callee11);
                  }));
                  return _traverse.apply(this, arguments);
                };
                traverse = function _traverse2(_x10, _x11, _x12) {
                  return _traverse.apply(this, arguments);
                };
                _expand_let_binding = function _expand_let_binding3() {
                  _expand_let_binding = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee10(node, n) {
                    var pair;
                    return _regeneratorRuntime.wrap(function _callee10$(_context10) {
                      while (1) {
                        switch (_context10.prev = _context10.next) {
                          case 0:
                            if (!(node === _nil)) {
                              _context10.next = 2;
                              break;
                            }
                            return _context10.abrupt("return", _nil);
                          case 2:
                            pair = node.car;
                            _context10.t0 = Pair;
                            _context10.t1 = Pair;
                            _context10.t2 = pair.car;
                            _context10.next = 8;
                            return traverse(pair.cdr, n, env);
                          case 8:
                            _context10.t3 = _context10.sent;
                            _context10.t4 = new _context10.t1(_context10.t2, _context10.t3);
                            _context10.next = 12;
                            return expand_let_binding(node.cdr);
                          case 12:
                            _context10.t5 = _context10.sent;
                            return _context10.abrupt("return", new _context10.t0(_context10.t4, _context10.t5));
                          case 14:
                          case "end":
                            return _context10.stop();
                        }
                      }
                    }, _callee10);
                  }));
                  return _expand_let_binding.apply(this, arguments);
                };
                expand_let_binding = function _expand_let_binding2(_x8, _x9) {
                  return _expand_let_binding.apply(this, arguments);
                };
                is_macro = function _is_macro(name, value) {
                  return value instanceof Macro && value.__defmacro__ && !bindings.includes(name);
                };
                let_binding = function _let_binding(node) {
                  return [].concat(_toConsumableArray(bindings), _toConsumableArray(node.to_array(false).map(function (node) {
                    if (node instanceof Pair) {
                      return node.car.valueOf();
                    }
                    throw new Error('macroexpand: Invalid let binding');
                  })));
                };
                proc_bindings = function _proc_bindings(node) {
                  var names = [];
                  while (true) {
                    if (node !== _nil) {
                      if (node instanceof LSymbol) {
                        names.push(node.valueOf());
                        break;
                      }
                      names.push(node.car.valueOf());
                      node = node.cdr;
                    } else {
                      break;
                    }
                  }
                  return [].concat(_toConsumableArray(bindings), names);
                };
                is_lambda = function _is_lambda(value) {
                  return value === lambda;
                };
                is_procedure = function _is_procedure(value, node) {
                  return value === define && node.cdr.car instanceof Pair;
                };
                is_let_macro = function _is_let_macro(symbol) {
                  var name = symbol.valueOf();
                  return let_macros.includes(name);
                };
                env = args['env'] = this;
                bindings = [];
                let_macros = ['let', 'let*', 'letrec'];
                lambda = global_env.get('lambda');
                define = global_env.get('define'); //var this.__code__ = code;
                if (!(code.cdr instanceof Pair && LNumber.isNumber(code.cdr.car))) {
                  _context12.next = 21;
                  break;
                }
                _context12.t0 = quote;
                _context12.next = 19;
                return traverse(code, code.cdr.car.valueOf(), env);
              case 19:
                _context12.t1 = _context12.sent.car;
                return _context12.abrupt("return", (0, _context12.t0)(_context12.t1));
              case 21:
                if (!single) {
                  _context12.next = 27;
                  break;
                }
                _context12.t2 = quote;
                _context12.next = 25;
                return traverse(code, 1, env);
              case 25:
                _context12.t3 = _context12.sent.car;
                return _context12.abrupt("return", (0, _context12.t2)(_context12.t3));
              case 27:
                _context12.t4 = quote;
                _context12.next = 30;
                return traverse(code, -1, env);
              case 30:
                _context12.t5 = _context12.sent.car;
                return _context12.abrupt("return", (0, _context12.t4)(_context12.t5));
              case 32:
              case "end":
                return _context12.stop();
            }
          }
        }, _callee12, this);
      }));
      return function (_x6, _x7) {
        return _ref21.apply(this, arguments);
      };
    }();
  }
  // ----------------------------------------------------------------------
  // TODO: Don't put Syntax as Macro they are not runtime
  // ----------------------------------------------------------------------
  function Syntax(fn, env) {
    this.__env__ = env;
    this.__fn__ = fn;
    // allow macroexpand
    this.__defmacro__ = true;
  }
  Syntax.__merge_env__ = Symbol["for"]('merge');
  // ----------------------------------------------------------------------
  Syntax.prototype = Object.create(Macro.prototype);
  Syntax.prototype.invoke = function (code, _ref22, macro_expand) {
    var error = _ref22.error,
      env = _ref22.env,
      use_dynamic = _ref22.use_dynamic;
    var args = {
      error: error,
      env: env,
      use_dynamic: use_dynamic,
      dynamic_env: this.__env__,
      macro_expand: macro_expand
    };
    return this.__fn__.call(env, code, args, this.__name__ || 'syntax');
  };
  Syntax.prototype.constructor = Syntax;
  Syntax.prototype.toString = function () {
    if (this.__name__) {
      return "#<syntax:".concat(this.__name__, ">");
    }
    return '#<syntax>';
  };
  Syntax.className = 'syntax';
  // ----------------------------------------------------------------------
  // :: TODO: SRFI-139
  // ----------------------------------------------------------------------
  var SyntaxParameter = /*#__PURE__*/function (_Syntax) {
    _inherits(SyntaxParameter, _Syntax);
    var _super = _createSuper(SyntaxParameter);
    function SyntaxParameter() {
      _classCallCheck(this, SyntaxParameter);
      return _super.apply(this, arguments);
    }
    return _createClass(SyntaxParameter);
  }(Syntax);
  Syntax.Parameter = SyntaxParameter;
  // ----------------------------------------------------------------------
  // :: for usage in syntax-rule when pattern match it will return
  // :: list of bindings from code that match the pattern
  // :: TODO detect cycles
  // ----------------------------------------------------------------------
  function extract_patterns(pattern, code, symbols, ellipsis_symbol) {
    var scope = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : {};
    var bindings = {
      '...': {
        symbols: {},
        // symbols ellipsis (x ...)
        lists: []
      },
      symbols: {}
    };
    var expansion = scope.expansion,
      define = scope.define;
    // pattern_names parameter is used to distinguish
    // multiple matches of ((x ...) ...) against ((1 2 3) (1 2 3))
    // in loop we add x to the list so we know that this is not
    // duplicated ellipsis symbol
    function log(x) {
      /* c8 ignore next 3 */
      if (is_debug()) {
        console.log(x);
      }
    }
    log(symbols);
    /* eslint-disable complexity */
    function traverse(pattern, code) {
      var pattern_names = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : [];
      var ellipsis = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : false;
      log({
        code: code && toString(code, true),
        pattern: pattern && toString(pattern, true)
      });
      if (is_atom(pattern) && !(pattern instanceof LSymbol)) {
        return same_atom(pattern, code);
      }
      if (pattern instanceof LSymbol && symbols.includes(pattern.literal())) {
        // TODO: literal() may be SLOW
        var ref = expansion.ref(code);
        // shadowing the identifier works only with lambda and let
        if (LSymbol.is(code, pattern)) {
          if (typeof ref === 'undefined') {
            return true;
          }
          return ref === define || ref === global_env;
        }
        return false;
      }
      // pattern (a b (x ...)) and (x ...) match nil
      if (pattern instanceof Pair && pattern.car instanceof Pair && pattern.car.cdr instanceof Pair && LSymbol.is(pattern.car.cdr.car, ellipsis_symbol)) {
        log('>> 0');
        if (code === _nil) {
          log({
            pattern: pattern.toString()
          });
          if (pattern.car.car instanceof LSymbol) {
            if (pattern.car.cdr instanceof Pair && LSymbol.is(pattern.car.cdr.car, ellipsis_symbol)) {
              var _name2 = pattern.car.car.valueOf();
              var last = pattern.last_pair();
              if (LSymbol.is(last.car, ellipsis_symbol)) {
                bindings['...'].symbols[_name2] = null;
                return true;
              } else {
                return false;
              }
            }
            var name = pattern.car.car.valueOf();
            if (bindings['...'].symbols[name]) {
              throw new Error('syntax: named ellipsis can only ' + 'appear onces');
            }
            bindings['...'].symbols[name] = code;
          }
        }
      }
      if (pattern instanceof Pair && pattern.cdr instanceof Pair && LSymbol.is(pattern.cdr.car, ellipsis_symbol)) {
        // pattern (... ???) - SRFI-46
        if (pattern.cdr.cdr !== _nil) {
          if (pattern.cdr.cdr instanceof Pair) {
            // if we have (x ... a b) we need to remove two from the end
            var list_len = pattern.cdr.cdr.length();
            var code_len = code.length();
            var list = code;
            while (code_len - 1 > list_len) {
              list = list.cdr;
              code_len--;
            }
            var rest = list.cdr;
            list.cdr = _nil;
            if (!traverse(pattern.cdr.cdr, rest, pattern_names, ellipsis)) {
              return false;
            }
          }
        }
        if (pattern.car instanceof LSymbol) {
          var _name3 = pattern.car.__name__;
          if (bindings['...'].symbols[_name3] && !pattern_names.includes(_name3) && !ellipsis) {
            throw new Error('syntax: named ellipsis can only appear onces');
          }
          log('>> 1');
          if (code === _nil) {
            log('>> 2');
            if (ellipsis) {
              log('NIL');
              bindings['...'].symbols[_name3] = _nil;
            } else {
              log('NULL');
              bindings['...'].symbols[_name3] = null;
            }
          } else if (code instanceof Pair && (code.car instanceof Pair || code.car === _nil)) {
            log('>> 3 ' + ellipsis);
            if (ellipsis) {
              if (bindings['...'].symbols[_name3]) {
                var node = bindings['...'].symbols[_name3];
                if (node === _nil) {
                  node = new Pair(_nil, new Pair(code, _nil));
                } else {
                  node = node.append(new Pair(code, _nil));
                }
                bindings['...'].symbols[_name3] = node;
              } else {
                bindings['...'].symbols[_name3] = new Pair(code, _nil);
              }
            } else {
              log('>> 4');
              bindings['...'].symbols[_name3] = new Pair(code, _nil);
            }
          } else {
            log('>> 6');
            if (code instanceof Pair) {
              log('>> 7 ' + ellipsis);
              pattern_names.push(_name3);
              if (!bindings['...'].symbols[_name3]) {
                bindings['...'].symbols[_name3] = new Pair(code, _nil);
              } else {
                var _node = bindings['...'].symbols[_name3];
                bindings['...'].symbols[_name3] = _node.append(new Pair(code, _nil));
              }
              log({
                IIIIII: bindings['...'].symbols[_name3].toString()
              });
            } else {
              log('>> 8');
              return false;
              //bindings['...'].symbols[name] = code;
            }
          }
          return true;
        } else if (pattern.car instanceof Pair) {
          var names = _toConsumableArray(pattern_names);
          if (code === _nil) {
            log('>> 9');
            bindings['...'].lists.push(_nil);
            return true;
          }
          log('>> 10');
          var _node2 = code;
          while (_node2 instanceof Pair) {
            if (!traverse(pattern.car, _node2.car, names, true)) {
              return false;
            }
            _node2 = _node2.cdr;
          }
          return true;
        }
        return false;
      }
      if (pattern instanceof LSymbol) {
        if (LSymbol.is(pattern, ellipsis_symbol)) {
          throw new Error('syntax: invalid usage of ellipsis');
        }
        log('>> 11');
        var _name4 = pattern.__name__;
        if (symbols.includes(_name4)) {
          return true;
        }
        log({
          name: _name4,
          ellipsis: ellipsis
        });
        if (ellipsis) {
          bindings['...'].symbols[_name4] = bindings['...'].symbols[_name4] || [];
          bindings['...'].symbols[_name4].push(code);
        }
        bindings.symbols[_name4] = code;
        if (!bindings.symbols[_name4]) ;
        return true;
      }
      if (pattern instanceof Pair && code instanceof Pair) {
        log('>> 12');
        log({
          a: 12,
          code: code && code.toString(),
          pattern: pattern.toString()
        });
        if (code.cdr === _nil) {
          // last item in in call using in recursive calls on
          // last element of the list
          // case of pattern (p . rest) and code (0)
          var rest_pattern = pattern.car instanceof LSymbol && pattern.cdr instanceof LSymbol;
          if (rest_pattern) {
            // fix for SRFI-26 in recursive call of (b) ==> (<> . x)
            // where <> is symbol
            if (!traverse(pattern.car, code.car, pattern_names, ellipsis)) {
              return false;
            }
            log('>> 12 | 1');
            var _name5 = pattern.cdr.valueOf();
            if (!(_name5 in bindings.symbols)) {
              bindings.symbols[_name5] = _nil;
            }
            _name5 = pattern.car.valueOf();
            if (!(_name5 in bindings.symbols)) {
              bindings.symbols[_name5] = code.car;
            }
            return true;
          }
        }
        log({
          pattern: pattern.toString(),
          code: code.toString()
        });
        // case (x y) ===> (var0 var1 ... warn) where var1 match nil
        if (pattern.cdr instanceof Pair && pattern.car instanceof LSymbol && pattern.cdr.cdr instanceof Pair && pattern.cdr.car instanceof LSymbol && LSymbol.is(pattern.cdr.cdr.car, ellipsis_symbol) && pattern.cdr.cdr.cdr instanceof Pair && !LSymbol.is(pattern.cdr.cdr.cdr.car, ellipsis_symbol) && traverse(pattern.car, code.car, pattern_names, ellipsis) && traverse(pattern.cdr.cdr.cdr, code.cdr, pattern_names, ellipsis)) {
          var _name6 = pattern.cdr.car.__name__;
          log({
            pattern: pattern.car.toString(),
            code: code.car.toString(),
            name: _name6
          });
          if (symbols.includes(_name6)) {
            return true;
          }
          bindings['...'].symbols[_name6] = null;
          return true;
        }
        log('recur');
        if (traverse(pattern.car, code.car, pattern_names, ellipsis) && traverse(pattern.cdr, code.cdr, pattern_names, ellipsis)) {
          return true;
        }
      } else if (pattern === _nil && (code === _nil || code === undefined)) {
        // undefined is case when you don't have body ...
        // and you do recursive call
        return true;
      } else if (pattern.car instanceof Pair && LSymbol.is(pattern.car.car, ellipsis_symbol)) {
        // pattern (...)
        throw new Error('syntax: invalid usage of ellipsis');
      } else {
        return false;
      }
    }
    /* eslint-enable complexity */
    if (traverse(pattern, code)) {
      return bindings;
    }
  }
  // ----------------------------------------------------------------------
  // :: This function is called after syntax-rules macro is evaluated
  // :: and if there are any gensyms added by macro they need to restored
  // :: to original symbols
  // ----------------------------------------------------------------------
  function clear_gensyms(node, gensyms) {
    function traverse(node) {
      if (node instanceof Pair) {
        if (!gensyms.length) {
          return node;
        }
        var car = traverse(node.car);
        var cdr = traverse(node.cdr);
        // TODO: check if it's safe to modify the list
        //       some funky modify of code can happen in macro
        return new Pair(car, cdr);
      } else if (node instanceof LSymbol) {
        var replacement = gensyms.find(function (gensym) {
          return gensym.gensym === node;
        });
        if (replacement) {
          return LSymbol(replacement.name);
        }
        return node;
      } else {
        return node;
      }
    }
    return traverse(node);
  }
  // ----------------------------------------------------------------------
  function transform_syntax() {
    var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
    var bindings = options.bindings,
      expr = options.expr,
      scope = options.scope,
      symbols = options.symbols,
      names = options.names,
      ellipsis_symbol = options.ellipsis;
    var gensyms = {};
    function valid_symbol(symbol) {
      if (symbol instanceof LSymbol) {
        return true;
      }
      return ['string', 'symbol'].includes(_typeof(symbol));
    }
    function transform(symbol) {
      if (!valid_symbol(symbol)) {
        var t = type(symbol);
        throw new Error("syntax: internal error, need symbol got ".concat(t));
      }
      var name = symbol.valueOf();
      if (name === ellipsis_symbol) {
        throw new Error('syntax: internal error, ellipis not transformed');
      }
      // symbols are gensyms from nested syntax-rules
      var n_type = _typeof(name);
      if (['string', 'symbol'].includes(n_type)) {
        if (name in bindings.symbols) {
          return bindings.symbols[name];
        } else if (n_type === 'string' && name.match(/\./)) {
          // calling method on pattern symbol #83
          var parts = name.split('.');
          var first = parts[0];
          if (first in bindings.symbols) {
            return Pair.fromArray([LSymbol('.'), bindings.symbols[first]].concat(parts.slice(1).map(function (x) {
              return LString(x);
            })));
          }
        }
      }
      if (symbols.includes(name)) {
        return LSymbol(name);
      }
      return rename(name);
    }
    function log(x) {
      /* c8 ignore next 3 */
      if (is_debug()) {
        console.log(x);
      }
    }
    function rename(name) {
      if (!gensyms[name]) {
        var ref = scope.ref(name);
        var gensym_name = gensym(name);
        if (ref) {
          var value = scope.get(name);
          scope.set(gensym_name, value);
        } else {
          var _value = scope.get(name, {
            throwError: false
          });
          // value is not in scope, but it's JavaScript object
          if (typeof _value !== 'undefined') {
            scope.set(gensym_name, _value);
          }
        }
        // keep names so they can be restored after evaluation
        // if there are free symbols as output
        // kind of hack
        names.push({
          name: name,
          gensym: gensym_name
        });
        gensyms[name] = gensym_name;
        // we need to check if name is a string, because it can be
        // gensym from nested syntax-rules
        if (typeof name === 'string' && name.match(/\./)) {
          var _name$split$filter = name.split('.').filter(Boolean),
            _name$split$filter2 = _toArray(_name$split$filter),
            first = _name$split$filter2[0],
            rest = _name$split$filter2.slice(1);
          // save JavaScript dot notation for Env::get
          if (gensyms[first]) {
            hidden_prop(gensym_name, '__object__', [gensyms[first]].concat(_toConsumableArray(rest)));
          }
        }
      }
      return gensyms[name];
    }
    function transform_ellipsis_expr(expr, bindings, state) {
      var next = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : function () {};
      var nested = state.nested;
      log(' ==> ' + expr.toString(true));
      log(bindings);
      if (expr instanceof LSymbol) {
        var name = expr.valueOf();
        log('[t 1');
        if (bindings[name]) {
          if (bindings[name] instanceof Pair) {
            var _bindings$name = bindings[name],
              car = _bindings$name.car,
              cdr = _bindings$name.cdr;
            if (nested) {
              var caar = car.car,
                cadr = car.cdr;
              if (cadr !== _nil) {
                next(name, new Pair(cadr, _nil));
              }
              return caar;
            }
            if (cdr !== _nil) {
              next(name, cdr);
            }
            return car;
          } else if (bindings[name] instanceof Array) {
            next(name, bindings[name].slice(1));
            return bindings[name][0];
          }
        }
        return transform(name);
      }
      if (expr instanceof Pair) {
        if (expr.car instanceof LSymbol && expr.cdr instanceof Pair && LSymbol.is(expr.cdr.car, ellipsis_symbol)) {
          log('[t 2');
          var _name7 = expr.car.valueOf();
          var item = bindings[_name7];
          log({
            expr: expr.toString(true),
            name: _name7,
            bindings: bindings,
            item: item
          });
          if (item === null) {
            return;
          } else if (item) {
            log({
              b: bindings[_name7].toString()
            });
            if (item instanceof Pair) {
              log('[t 2 Pair ' + nested);
              log({
                ______: item.toString()
              });
              var _car = item.car,
                _cdr = item.cdr;
              if (nested) {
                if (_cdr !== _nil) {
                  log('|| next 1');
                  next(_name7, _cdr);
                }
                log({
                  car: _car.toString()
                });
                return _car;
              } else {
                if (_car.cdr !== _nil) {
                  log('|| next 2');
                  next(_name7, new Pair(_car.cdr, _cdr));
                }
                log({
                  car: _car.car.toString()
                });
                return _car.car;
              }
            } else if (item instanceof Array) {
              log('[t 2 Array ' + nested);
              if (nested) {
                next(_name7, item.slice(1));
                return Pair.fromArray(item);
              } else {
                var _rest5 = item.slice(1);
                if (_rest5.length) {
                  next(_name7, _rest5);
                }
                return item[0];
              }
            } else {
              return item;
            }
          }
        }
        log('[t 3 recur ' + expr.toString());
        var head = transform_ellipsis_expr(expr.car, bindings, state, next);
        var rest = transform_ellipsis_expr(expr.cdr, bindings, state, next);
        return new Pair(head, rest);
      }
      return expr;
    }
    function have_binding(biding, skip_nulls) {
      var values = Object.values(biding);
      var symbols = Object.getOwnPropertySymbols(biding);
      if (symbols.length) {
        values.push.apply(values, _toConsumableArray(symbols.map(function (x) {
          return biding[x];
        })));
      }
      return values.length && values.every(function (x) {
        if (x === null) {
          return !skip_nulls;
        }
        return x instanceof Pair || x === _nil || x instanceof Array && x.length;
      });
    }
    function get_names(object) {
      return Object.keys(object).concat(Object.getOwnPropertySymbols(object));
    }
    /* eslint-disable complexity */
    function traverse(expr) {
      var _ref23 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        disabled = _ref23.disabled;
      log('traverse>> ' + toString(expr));
      if (expr instanceof Pair) {
        // escape ellispsis from R7RS e.g. (... ...)
        if (!disabled && expr.car instanceof Pair && LSymbol.is(expr.car.car, ellipsis_symbol)) {
          return traverse(expr.car.cdr, {
            disabled: true
          });
        }
        if (expr.cdr instanceof Pair && LSymbol.is(expr.cdr.car, ellipsis_symbol) && !disabled) {
          log('>> 1');
          var _symbols2 = bindings['...'].symbols;
          // skip expand list of pattern was (x y ... z)
          // and code was (x z) so y == null
          var values = Object.values(_symbols2);
          if (values.length && values.every(function (x) {
            return x === null;
          })) {
            return traverse(expr.cdr.cdr, {
              disabled: disabled
            });
          }
          var keys = get_names(_symbols2);
          // case of list as first argument ((x . y) ...) or (x ... ...)
          // we need to recursively process the list
          // if we have pattern (_ (x y z ...) ...) and code (foo (1 2) (1 2))
          // x an y will be arrays of [1 1] and [2 2] and z will be array
          // of rest, x will also have it's own mapping to 1 and y to 2
          // in case of usage outside of ellipsis list e.g.: (x y)
          var is_spread = expr.car instanceof LSymbol && LSymbol.is(expr.cdr.cdr.car, ellipsis_symbol);
          if (expr.car instanceof Pair || is_spread) {
            // lists is free ellipsis on pairs ((???) ...)
            // TODO: will this work in every case? Do we need to handle
            // nesting here?
            if (bindings['...'].lists[0] === _nil) {
              return _nil;
            }
            var new_expr = expr.car;
            if (is_spread) {
              new_expr = new Pair(expr.car, new Pair(expr.cdr.car, _nil));
            }
            log('>> 2');
            var result;
            if (keys.length) {
              log('>> 2 (a)');
              var _bind = _objectSpread({}, _symbols2);
              result = _nil;
              var _loop = function _loop() {
                if (!have_binding(_bind)) {
                  return "break";
                }
                var new_bind = {};
                var next = function next(key, value) {
                  // ellipsis decide it what should be the next value
                  // there are two cases ((a . b) ...) and (a ...)
                  new_bind[key] = value;
                };
                var car = transform_ellipsis_expr(new_expr, _bind, {
                  nested: true
                }, next);
                // undefined can be null caused by null binding
                // on empty ellipsis
                if (car !== undefined) {
                  if (is_spread) {
                    if (result === _nil) {
                      result = car;
                    } else {
                      result = result.append(car);
                    }
                  } else {
                    result = new Pair(car, result);
                  }
                }
                _bind = new_bind;
              };
              while (true) {
                var _ret = _loop();
                if (_ret === "break") break;
              }
              if (result !== _nil && !is_spread) {
                result = result.reverse();
              }
              // case of (list) ... (rest code)
              if (expr.cdr.cdr !== _nil && !LSymbol.is(expr.cdr.cdr.car, ellipsis_symbol)) {
                var _rest6 = traverse(expr.cdr.cdr, {
                  disabled: disabled
                });
                return result.append(_rest6);
              }
              return result;
            } else {
              log('>> 3');
              var car = transform_ellipsis_expr(expr.car, _symbols2, {
                nested: true
              });
              if (car) {
                return new Pair(car, _nil);
              }
              return _nil;
            }
          } else if (expr.car instanceof LSymbol) {
            log('>> 4');
            if (LSymbol.is(expr.cdr.cdr.car, ellipsis_symbol)) {
              // case (x ... ...)
              log('>> 4 (a)');
            } else {
              log('>> 4 (b)');
            }
            // case: (x ...)
            var name = expr.car.__name__;
            var _bind2 = _defineProperty({}, name, _symbols2[name]);
            var _is_null = _symbols2[name] === null;
            var _result2 = _nil;
            var _loop2 = function _loop2() {
              if (!have_binding(_bind2, true)) {
                log({
                  bind: _bind2
                });
                return "break";
              }
              var new_bind = {};
              var next = function next(key, value) {
                new_bind[key] = value;
              };
              var value = transform_ellipsis_expr(expr, _bind2, {
                nested: false
              }, next);
              log({
                value: value.toString()
              });
              if (typeof value !== 'undefined') {
                _result2 = new Pair(value, _result2);
              }
              _bind2 = new_bind;
            };
            while (true) {
              var _ret2 = _loop2();
              if (_ret2 === "break") break;
            }
            if (_result2 !== _nil) {
              _result2 = _result2.reverse();
            }
            // case if (x ... y ...) second spread is not processed
            // and (??? . x) last symbol
            // by ellipsis transformation
            if (expr.cdr instanceof Pair) {
              if (expr.cdr.cdr instanceof Pair || expr.cdr.cdr instanceof LSymbol) {
                var node = traverse(expr.cdr.cdr, {
                  disabled: disabled
                });
                if (_is_null) {
                  return node;
                }
                log('<<<< 1');
                _result2.append(node);
              }
            }
            log('<<<< 2');
            return _result2;
          }
        }
        var head = traverse(expr.car, {
          disabled: disabled
        });
        var rest;
        var is_syntax;
        if (expr.car instanceof LSymbol) {
          var value = scope.get(expr.car, {
            throwError: false
          });
          is_syntax = value instanceof Macro && value.__name__ === 'syntax-rules';
        }
        if (is_syntax) {
          if (expr.cdr.car instanceof LSymbol) {
            rest = new Pair(traverse(expr.cdr.car, {
              disabled: disabled
            }), new Pair(expr.cdr.cdr.car, traverse(expr.cdr.cdr.cdr, {
              disabled: disabled
            })));
          } else {
            rest = new Pair(expr.cdr.car, traverse(expr.cdr.cdr, {
              disabled: disabled
            }));
          }
          log('REST >>>> ' + rest.toString());
        } else {
          rest = traverse(expr.cdr, {
            disabled: disabled
          });
        }
        log({
          a: true,
          car: toString(expr.car),
          cdr: toString(expr.cdr),
          head: toString(head),
          rest: toString(rest)
        });
        return new Pair(head, rest);
      }
      if (expr instanceof LSymbol) {
        if (disabled && LSymbol.is(expr, ellipsis_symbol)) {
          return expr;
        }
        var _symbols3 = Object.keys(bindings['...'].symbols);
        var _name8 = expr.literal(); // TODO: slow
        if (_symbols3.includes(_name8)) {
          var msg = "missing ellipsis symbol next to name `".concat(_name8, "'");
          throw new Error("syntax-rules: ".concat(msg));
        }
        var _value2 = transform(expr);
        if (typeof _value2 !== 'undefined') {
          return _value2;
        }
      }
      return expr;
    }
    return traverse(expr, {});
  }
  // ----------------------------------------------------------------------
  // :: Check for nullish values
  // ----------------------------------------------------------------------
  function is_null(value) {
    return is_undef(value) || value === _nil || value === null;
  }
  // ----------------------------------------------------------------------
  function is_function(o) {
    return typeof o === 'function' && typeof o.bind === 'function';
  }
  // ----------------------------------------------------------------------
  function is_continuation(o) {
    return o instanceof Continuation;
  }
  // ----------------------------------------------------------------------
  function is_context(o) {
    return o instanceof LambdaContext;
  }
  // ----------------------------------------------------------------------
  function is_parameter(o) {
    return o instanceof Parameter;
  }
  // ----------------------------------------------------------------------
  function is_pair(o) {
    return o instanceof Pair;
  }
  // ----------------------------------------------------------------------
  function is_env(o) {
    return o instanceof Environment;
  }
  // ----------------------------------------------------------------------
  function is_callable(o) {
    return is_function(o) || is_continuation(o) || is_parameter(o);
  }
  // ----------------------------------------------------------------------
  function is_promise(o) {
    if (o instanceof QuotedPromise) {
      return false;
    }
    if (o instanceof Promise) {
      return true;
    }
    return o && is_function(o.then);
  }
  // ----------------------------------------------------------------------
  function is_undef(value) {
    return typeof value === 'undefined';
  }
  // ----------------------------------------------------------------------
  // :: Function utilities
  // ----------------------------------------------------------------------
  function box(object) {
    // We only need to box lips data and arrays. Object don't need
    // to be boxed, but values from objects will be boxed when accessed.
    switch (_typeof(object)) {
      case 'string':
        return LString(object);
      case 'bigint':
        return LNumber(object);
      case 'number':
        if (Number.isNaN(object)) {
          return nan;
        } else {
          return LNumber(object);
        }
    }
    return object;
  }
  // ----------------------------------------------------------------------
  function map_object(object, fn) {
    var props = Object.getOwnPropertyNames(object);
    var symbols = Object.getOwnPropertySymbols(object);
    props.concat(symbols).forEach(function (key) {
      var value = fn(object[key]);
      // check if property is read only, happen with webpack
      // and __esModule, it can happen for other properties as well
      var descriptor = Object.getOwnPropertyDescriptor(object, key);
      if (!descriptor || descriptor.writable && object[key] !== value) {
        object[key] = value;
      }
    });
    return object;
  }
  // ----------------------------------------------------------------------
  function unbox(object) {
    // LCharacter is unboxable #233
    var lips_type = [LString, LNumber].some(function (x) {
      return object instanceof x;
    });
    if (lips_type) {
      return object.valueOf();
    }
    if (object instanceof Array) {
      return object.map(unbox);
    }
    if (object instanceof QuotedPromise) {
      delete object.then;
    }
    if (is_plain_object(object)) {
      return map_object(object, unbox);
    }
    return object;
  }
  // ----------------------------------------------------------------------
  function patch_value(value, context) {
    if (value instanceof Pair) {
      value.markCycles();
      return quote(value);
    }
    if (is_function(value)) {
      // original function can be restored using unbind function
      // only real JS function require to be bound
      if (context) {
        return bind(value, context);
      }
    }
    return box(value);
  }
  // ----------------------------------------------------------------------
  // :: Function gets original function that was binded with props
  // ----------------------------------------------------------------------
  function unbind(obj) {
    if (is_bound(obj)) {
      return obj[__fn__];
    }
    return obj;
  }
  // ----------------------------------------------------------------------
  // :: Function binds with context that can be optionally unbind
  // :: get original function with unbind
  // ----------------------------------------------------------------------
  function bind(fn, context) {
    if (fn[Symbol["for"]('__bound__')]) {
      return fn;
    }
    var bound = fn.bind(context);
    var props = Object.getOwnPropertyNames(fn);
    var _iterator9 = _createForOfIteratorHelper(props),
      _step9;
    try {
      for (_iterator9.s(); !(_step9 = _iterator9.n()).done;) {
        var prop = _step9.value;
        if (filter_fn_names(prop)) {
          try {
            bound[prop] = fn[prop];
          } catch (e) {
            // ignore error from express.js while accessing bodyParser
          }
        }
      }
    } catch (err) {
      _iterator9.e(err);
    } finally {
      _iterator9.f();
    }
    hidden_prop(bound, '__fn__', fn);
    hidden_prop(bound, '__context__', context);
    hidden_prop(bound, '__bound__', true);
    if (is_native_function(fn)) {
      hidden_prop(bound, '__native__', true);
    }
    if (is_plain_object(context) && is_lambda(fn)) {
      hidden_prop(bound, '__method__', true);
    }
    bound.valueOf = function () {
      return fn;
    };
    return bound;
  }
  // ----------------------------------------------------------------------
  // Function used to check if function should not get unboxed arguments,
  // so you can call Object.getPrototypeOf for lips data types
  // this is case, see dir function and #73
  // ----------------------------------------------------------------------
  function is_object_bound(obj) {
    return is_bound(obj) && obj[Symbol["for"]('__context__')] === Object;
  }
  // ----------------------------------------------------------------------
  function is_bound(obj) {
    return !!(is_function(obj) && obj[__fn__]);
  }
  // ----------------------------------------------------------------------
  function lips_context(obj) {
    if (is_function(obj)) {
      var context = obj[__context__];
      if (context && (context === lips || context.constructor && context.constructor.__class__)) {
        return true;
      }
    }
    return false;
  }
  // ----------------------------------------------------------------------
  function is_port(obj) {
    return obj instanceof InputPort || obj instanceof OutputPort;
  }
  // ----------------------------------------------------------------------
  function is_port_method(obj) {
    if (is_function(obj)) {
      if (is_port(obj[__context__])) {
        return true;
      }
    }
    return false;
  }
  // ----------------------------------------------------------------------
  // Hidden props
  // ----------------------------------------------------------------------
  var __context__ = Symbol["for"]('__context__');
  var __fn__ = Symbol["for"]('__fn__');
  var __data__ = Symbol["for"]('__data__');
  var __ref__ = Symbol["for"]('__ref__');
  var __cycles__ = Symbol["for"]('__cycles__');
  var __class__ = Symbol["for"]('__class__');
  var __method__ = Symbol["for"]('__method__');
  var __prototype__ = Symbol["for"]('__prototype__');
  var __lambda__ = Symbol["for"]('__lambda__');
  // ----------------------------------------------------------------------
  // :: Function bind fn with context but it also move all props
  // :: mostly used for Object function
  // ----------------------------------------------------------------------
  var exluded_names = ['name', 'length', 'caller', 'callee', 'arguments', 'prototype'];
  function filter_fn_names(name) {
    return !exluded_names.includes(name);
  }
  // ----------------------------------------------------------------------
  function hidden_prop(obj, name, value) {
    Object.defineProperty(obj, Symbol["for"](name), {
      get: function get() {
        return value;
      },
      set: function set() {},
      configurable: false,
      enumerable: false
    });
  }
  // ----------------------------------------------------------------------
  function set_fn_length(fn, length) {
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
  }
  // ----------------------------------------------------------------------
  function is_lambda(obj) {
    return obj && obj[__lambda__];
  }
  // ----------------------------------------------------------------------
  function is_method(obj) {
    return obj && obj[__method__];
  }
  // ----------------------------------------------------------------------
  function is_raw_lambda(fn) {
    return is_lambda(fn) && !fn[__prototype__] && !is_method(fn) && !is_port_method(fn);
  }
  // ----------------------------------------------------------------------
  function is_native_function(fn) {
    var _native = Symbol["for"]('__native__');
    return is_function(fn) && fn.toString().match(/\{\s*\[native code\]\s*\}/) && (fn.name.match(/^bound /) && fn[_native] === true || !fn.name.match(/^bound /) && !fn[_native]);
  }
  // ----------------------------------------------------------------------
  // :: function that return macro for let, let* and letrec
  // ----------------------------------------------------------------------
  function let_macro(symbol) {
    var name;
    switch (symbol) {
      case Symbol["for"]('letrec'):
        name = 'letrec';
        break;
      case Symbol["for"]('let'):
        name = 'let';
        break;
      case Symbol["for"]('let*'):
        name = 'let*';
        break;
      default:
        throw new Error('Invalid let_macro value');
    }
    return Macro.defmacro(name, function (code, options) {
      var dynamic_env = options.dynamic_env;
      var error = options.error,
        macro_expand = options.macro_expand,
        use_dynamic = options.use_dynamic;
      var args;
      // named let:
      // (let iter ((x 10)) (iter (- x 1))) -> (let* ((iter (lambda (x) ...
      if (code.car instanceof LSymbol) {
        if (!(code.cdr.car instanceof Pair || code.cdr.car === _nil)) {
          throw new Error('let require list of pairs');
        }
        var params;
        if (code.cdr.car === _nil) {
          args = _nil;
          params = _nil;
        } else {
          params = code.cdr.car.map(function (pair) {
            return pair.car;
          });
          args = code.cdr.car.map(function (pair) {
            return pair.cdr.car;
          });
        }
        return Pair.fromArray([LSymbol('letrec'), [[code.car, Pair(LSymbol('lambda'), Pair(params, code.cdr.cdr))]], Pair(code.car, args)]);
      } else if (macro_expand) {
        // Macro.defmacro are special macros that should return lips code
        // here we use evaluate, so we need to check special flag set by
        // macroexpand to prevent evaluation of code in normal let
        return;
      }
      var self = this;
      args = global_env.get('list->array')(code.car);
      var env = self.inherit(name);
      var values, var_body_env;
      if (name === 'let*') {
        var_body_env = env;
      } else if (name === 'let') {
        values = []; // collect potential promises
      }
      var i = 0;
      function exec() {
        var output = new Pair(new LSymbol('begin'), code.cdr);
        return _evaluate(output, {
          env: env,
          dynamic_env: env,
          use_dynamic: use_dynamic,
          error: error
        });
      }
      return function loop() {
        var pair = args[i++];
        dynamic_env = name === 'let*' ? env : self;
        if (!pair) {
          // resolve all promises
          if (values && values.length) {
            var v = values.map(function (x) {
              return x.value;
            });
            var promises = v.filter(is_promise);
            if (promises.length) {
              return promise_all(v).then(function (arr) {
                for (var i = 0, len = arr.length; i < len; ++i) {
                  env.set(values[i].name, arr[i]);
                }
              }).then(exec);
            } else {
              var _iterator10 = _createForOfIteratorHelper(values),
                _step10;
              try {
                for (_iterator10.s(); !(_step10 = _iterator10.n()).done;) {
                  var _step10$value = _step10.value,
                    _name9 = _step10$value.name,
                    _value3 = _step10$value.value;
                  env.set(_name9, _value3);
                }
              } catch (err) {
                _iterator10.e(err);
              } finally {
                _iterator10.f();
              }
            }
          }
          return exec();
        } else {
          if (name === 'let') {
            var_body_env = self;
          } else if (name === 'letrec') {
            var_body_env = env;
          }
          var value = _evaluate(pair.cdr.car, {
            env: var_body_env,
            dynamic_env: dynamic_env,
            use_dynamic: use_dynamic,
            error: error
          });
          if (name === 'let*') {
            var_body_env = env = var_body_env.inherit('let*[' + i + ']');
          }
          if (values) {
            values.push({
              name: pair.car,
              value: value
            });
            return loop();
          } else {
            return unpromise(value, function (value) {
              env.set(pair.car, value);
              return loop();
            });
          }
        }
      }();
    });
  }
  // -------------------------------------------------------------------------
  function parallel(name, fn) {
    return new Macro(name, function (code) {
      var _ref24 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        use_dynamic = _ref24.use_dynamic,
        error = _ref24.error;
      var env = this;
      var dynamic_env = this;
      var results = [];
      var node = code;
      while (node instanceof Pair) {
        results.push(_evaluate(node.car, {
          env: env,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic,
          error: error
        }));
        node = node.cdr;
      }
      var havePromises = results.filter(is_promise).length;
      if (havePromises) {
        return promise_all(results).then(fn.bind(this));
      } else {
        return fn.call(this, results);
      }
    });
  }
  // -------------------------------------------------------------------------
  function guard_math_call(fn) {
    for (var _len6 = arguments.length, args = new Array(_len6 > 1 ? _len6 - 1 : 0), _key6 = 1; _key6 < _len6; _key6++) {
      args[_key6 - 1] = arguments[_key6];
    }
    args.forEach(function (arg) {
      typecheck('', arg, 'number');
    });
    return fn.apply(void 0, args);
  }
  // ----------------------------------------------------------------------
  function pipe() {
    var _this6 = this;
    for (var _len7 = arguments.length, fns = new Array(_len7), _key7 = 0; _key7 < _len7; _key7++) {
      fns[_key7] = arguments[_key7];
    }
    fns.forEach(function (fn, i) {
      typecheck('pipe', fn, 'function', i + 1);
    });
    return function () {
      for (var _len8 = arguments.length, args = new Array(_len8), _key8 = 0; _key8 < _len8; _key8++) {
        args[_key8] = arguments[_key8];
      }
      return fns.reduce(function (args, f) {
        return [f.apply(_this6, args)];
      }, args)[0];
    };
  }
  // -------------------------------------------------------------------------
  function compose() {
    for (var _len9 = arguments.length, fns = new Array(_len9), _key9 = 0; _key9 < _len9; _key9++) {
      fns[_key9] = arguments[_key9];
    }
    fns.forEach(function (fn, i) {
      typecheck('compose', fn, 'function', i + 1);
    });
    return pipe.apply(void 0, _toConsumableArray(fns.reverse()));
  }
  // -------------------------------------------------------------------------
  // :: fold functions generator
  // -------------------------------------------------------------------------
  function fold(name, fold) {
    var self = this;
    return function recur(fn, init) {
      typecheck(name, fn, 'function');
      for (var _len10 = arguments.length, lists = new Array(_len10 > 2 ? _len10 - 2 : 0), _key10 = 2; _key10 < _len10; _key10++) {
        lists[_key10 - 2] = arguments[_key10];
      }
      if (lists.some(is_null)) {
        if (typeof init === 'number') {
          return LNumber(init);
        }
        return init;
      } else {
        return fold.call.apply(fold, [self, recur, fn, init].concat(lists));
      }
    };
  }
  // -------------------------------------------------------------------------
  function limit_math_op(n, fn) {
    // + 1 so it include function in guard_math_call
    return limit(n + 1, curry(guard_math_call, fn));
  }
  // -------------------------------------------------------------------------
  // :: some functional magic
  // -------------------------------------------------------------------------
  var single_math_op = curry(limit_math_op, 1);
  var binary_math_op = curry(limit_math_op, 2);
  // -------------------------------------------------------------------------
  function reduce_math_op(fn) {
    var init = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;
    return function () {
      for (var _len11 = arguments.length, args = new Array(_len11), _key11 = 0; _key11 < _len11; _key11++) {
        args[_key11] = arguments[_key11];
      }
      if (init !== null) {
        args = [init].concat(_toConsumableArray(args));
      }
      return args.reduce(binary_math_op(fn));
    };
  }
  // -------------------------------------------------------------------------
  function curry(fn) {
    for (var _len12 = arguments.length, init_args = new Array(_len12 > 1 ? _len12 - 1 : 0), _key12 = 1; _key12 < _len12; _key12++) {
      init_args[_key12 - 1] = arguments[_key12];
    }
    typecheck('curry', fn, 'function');
    var len = fn.length;
    return function () {
      var args = init_args.slice();
      function call() {
        for (var _len13 = arguments.length, more_args = new Array(_len13), _key13 = 0; _key13 < _len13; _key13++) {
          more_args[_key13] = arguments[_key13];
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
  }
  // -------------------------------------------------------------------------
  // return function with limited number of arguments
  function limit(n, fn) {
    typecheck('limit', fn, 'function', 2);
    return function () {
      for (var _len14 = arguments.length, args = new Array(_len14), _key14 = 0; _key14 < _len14; _key14++) {
        args[_key14] = arguments[_key14];
      }
      return fn.apply(void 0, _toConsumableArray(args.slice(0, n)));
    };
  }
  // -------------------------------------------------------------------------
  // :: Character object representation
  // -------------------------------------------------------------------------
  function LCharacter(_char7) {
    if (typeof this !== 'undefined' && !(this instanceof LCharacter) || typeof this === 'undefined') {
      return new LCharacter(_char7);
    }
    if (_char7 instanceof LString) {
      _char7 = _char7.valueOf();
    }
    var name;
    if (Array.from(_char7).length > 1) {
      // this is name
      _char7 = _char7.toLowerCase();
      if (LCharacter.__names__[_char7]) {
        name = _char7;
        _char7 = LCharacter.__names__[_char7];
      } else {
        // this should never happen
        // parser don't allow not defined named characters
        throw new Error('Internal: Unknown named character');
      }
    } else {
      name = LCharacter.__rev_names__[_char7];
    }
    Object.defineProperty(this, '__char__', {
      value: _char7,
      enumerable: true
    });
    if (name) {
      Object.defineProperty(this, '__name__', {
        value: name,
        enumerable: true
      });
    }
  }
  LCharacter.__names__ = characters;
  LCharacter.__rev_names__ = {};
  Object.keys(LCharacter.__names__).forEach(function (key) {
    var value = LCharacter.__names__[key];
    LCharacter.__rev_names__[value] = key;
  });
  LCharacter.prototype.toUpperCase = function () {
    return LCharacter(this.__char__.toUpperCase());
  };
  LCharacter.prototype.toLowerCase = function () {
    return LCharacter(this.__char__.toLowerCase());
  };
  LCharacter.prototype.toString = function () {
    return '#\\' + (this.__name__ || this.__char__);
  };
  LCharacter.prototype.valueOf = LCharacter.prototype.serialize = function () {
    return this.__char__;
  };
  // -------------------------------------------------------------------------
  // :: String wrapper that handle copy and in place change
  // -------------------------------------------------------------------------
  function LString(string) {
    if (typeof this !== 'undefined' && !(this instanceof LString) || typeof this === 'undefined') {
      return new LString(string);
    }
    if (string instanceof Array) {
      this.__string__ = string.map(function (x, i) {
        typecheck('LString', x, 'character', i + 1);
        return x.toString();
      }).join('');
    } else {
      this.__string__ = string.valueOf();
    }
  }
  {
    var ignore = ['length', 'constructor'];
    var _keys = Object.getOwnPropertyNames(String.prototype).filter(function (name) {
      return !ignore.includes(name);
    });
    var wrap = function wrap(fn) {
      return function () {
        for (var _len15 = arguments.length, args = new Array(_len15), _key15 = 0; _key15 < _len15; _key15++) {
          args[_key15] = arguments[_key15];
        }
        return fn.apply(this.__string__, args);
      };
    };
    var _iterator11 = _createForOfIteratorHelper(_keys),
      _step11;
    try {
      for (_iterator11.s(); !(_step11 = _iterator11.n()).done;) {
        var key = _step11.value;
        LString.prototype[key] = wrap(String.prototype[key]);
      }
    } catch (err) {
      _iterator11.e(err);
    } finally {
      _iterator11.f();
    }
  }
  LString.prototype[Symbol.iterator] = /*#__PURE__*/_regeneratorRuntime.mark(function _callee13() {
    var chars, _i4, _chars, _char8;
    return _regeneratorRuntime.wrap(function _callee13$(_context13) {
      while (1) {
        switch (_context13.prev = _context13.next) {
          case 0:
            chars = Array.from(this.__string__);
            _i4 = 0, _chars = chars;
          case 2:
            if (!(_i4 < _chars.length)) {
              _context13.next = 9;
              break;
            }
            _char8 = _chars[_i4];
            _context13.next = 6;
            return LCharacter(_char8);
          case 6:
            _i4++;
            _context13.next = 2;
            break;
          case 9:
          case "end":
            return _context13.stop();
        }
      }
    }, _callee13, this);
  });
  LString.prototype.serialize = function () {
    return this.valueOf();
  };
  LString.isString = function (x) {
    return x instanceof LString || typeof x === 'string';
  };
  LString.prototype.get = function (n) {
    typecheck('LString::get', n, 'number');
    return Array.from(this.__string__)[n.valueOf()];
  };
  LString.prototype.cmp = function (string) {
    typecheck('LString::cmp', string, 'string');
    var a = this.valueOf();
    var b = string.valueOf();
    if (a < b) {
      return -1;
    } else if (a === b) {
      return 0;
    } else {
      return 1;
    }
  };
  LString.prototype.lower = function () {
    return LString(this.__string__.toLowerCase());
  };
  LString.prototype.upper = function () {
    return LString(this.__string__.toUpperCase());
  };
  LString.prototype.set = function (n, _char9) {
    typecheck('LString::set', n, 'number');
    typecheck('LString::set', _char9, ['string', 'character']);
    n = n.valueOf();
    if (_char9 instanceof LCharacter) {
      _char9 = _char9.__char__;
    }
    var string = [];
    if (n > 0) {
      string.push(this.__string__.substring(0, n));
    }
    string.push(_char9);
    if (n < this.__string__.length - 1) {
      string.push(this.__string__.substring(n + 1));
    }
    this.__string__ = string.join('');
  };
  Object.defineProperty(LString.prototype, "length", {
    get: function get() {
      return this.__string__.length;
    }
  });
  LString.prototype.clone = function () {
    return LString(this.valueOf());
  };
  LString.prototype.fill = function (_char10) {
    typecheck('LString::fill', _char10, ['string', 'character']);
    if (_char10 instanceof LCharacter) {
      _char10 = _char10.toString();
    }
    var len = this.__string__.length;
    this.__string__ = new Array(len + 1).join(_char10);
  };
  // -------------------------------------------------------------------------
  // :: Number wrapper that handle BigNumbers
  // -------------------------------------------------------------------------
  function LNumber(n) {
    var force = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
    if (n instanceof LNumber) {
      return n;
    }
    if (typeof this !== 'undefined' && !(this instanceof LNumber) || typeof this === 'undefined') {
      return new LNumber(n, force);
    }
    if (typeof n === 'undefined') {
      throw new Error('Invalid LNumber constructor call');
    }
    var _type = LNumber.getType(n);
    if (LNumber.types[_type]) {
      return LNumber.types[_type](n, force);
    }
    var parsable = n instanceof Array && LString.isString(n[0]) && LNumber.isNumber(n[1]);
    if (n instanceof LNumber) {
      return LNumber(n.value);
    }
    if (!LNumber.isNumber(n) && !parsable) {
      throw new Error("You can't create LNumber from ".concat(type(n)));
    }
    // prevent infinite loop https://github.com/indutny/bn.js/issues/186
    if (n === null) {
      n = 0;
    }
    var value;
    if (parsable) {
      var _n = n,
        _n2 = _slicedToArray(_n, 2),
        str = _n2[0],
        radix = _n2[1];
      if (str instanceof LString) {
        str = str.valueOf();
      }
      if (radix instanceof LNumber) {
        radix = radix.valueOf();
      }
      var sign = str.match(/^([+-])/);
      var minus = false;
      if (sign) {
        str = str.replace(/^[+-]/, '');
        if (sign[1] === '-') {
          minus = true;
        }
      }
    }
    if (Number.isNaN(n)) {
      return LFloat(n);
    } else if (typeof BigInt !== 'undefined') {
      if (typeof n !== 'bigint') {
        if (parsable) {
          var prefix;
          // default number base (radix) supported by BigInt constructor
          switch (radix) {
            case 8:
              prefix = '0o';
              break;
            case 16:
              prefix = '0x';
              break;
            case 2:
              prefix = '0b';
              break;
            case 10:
              prefix = '';
              break;
          }
          if (typeof prefix === 'undefined') {
            // non standard radix we convert by hand
            var n_radix = BigInt(radix);
            value = _toConsumableArray(str).map(function (x, i) {
              return BigInt(parseInt(x, radix)) * pow(n_radix, BigInt(i));
            }).reduce(function (a, b) {
              return a + b;
            });
          } else {
            value = BigInt(prefix + str);
          }
        } else {
          value = BigInt(n);
        }
        if (minus) {
          value *= BigInt(-1);
        }
      } else {
        value = n;
      }
      return LBigInteger(value, true);
    } else if (typeof BN !== 'undefined' && !(n instanceof BN)) {
      if (n instanceof Array) {
        return LBigInteger(_construct(BN, _toConsumableArray(n)));
      }
      return LBigInteger(new BN(n));
    } else if (parsable) {
      this.constant(parseInt(str, radix), 'integer');
    } else {
      this.constant(n, 'integer');
    }
  }
  // -------------------------------------------------------------------------
  LNumber.prototype.constant = function (value, type) {
    Object.defineProperty(this, '__value__', {
      value: value,
      enumerable: true
    });
    Object.defineProperty(this, '__type__', {
      value: type,
      enumerable: true
    });
  };
  // -------------------------------------------------------------------------
  LNumber.types = {
    "float": function float(n) {
      return new LFloat(n);
    },
    complex: function complex(n) {
      var force = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
      if (!LNumber.isComplex(n)) {
        n = {
          im: 0,
          re: n
        };
      }
      return new LComplex(n, force);
    },
    rational: function rational(n) {
      var force = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
      if (!LNumber.isRational(n)) {
        n = {
          num: n,
          denom: 1
        };
      }
      return new LRational(n, force);
    }
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.serialize = function () {
    return this.__value__;
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.isNaN = function () {
    return Number.isNaN(this.__value__);
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.gcd = function (b) {
    // ref: https://rosettacode.org/wiki/Greatest_common_divisor#JavaScript
    var a = this.abs();
    b = b.abs();
    if (b.cmp(a) === 1) {
      var temp = a;
      a = b;
      b = temp;
    }
    while (true) {
      a = a.rem(b);
      if (a.cmp(0) === 0) {
        return b;
      }
      b = b.rem(a);
      if (b.cmp(0) === 0) {
        return a;
      }
    }
  };
  // -------------------------------------------------------------------------
  LNumber.isFloat = function isFloat(n) {
    return n instanceof LFloat || Number(n) === n && n % 1 !== 0;
  };
  // -------------------------------------------------------------------------
  LNumber.isNumber = function (n) {
    return n instanceof LNumber || LNumber.isNative(n) || LNumber.isBN(n);
  };
  // -------------------------------------------------------------------------
  LNumber.isComplex = function (n) {
    if (!n) {
      return false;
    }
    var ret = n instanceof LComplex || (LNumber.isNumber(n.im) || Number.isNaN(n.im)) && (LNumber.isNumber(n.re) || Number.isNaN(n.re));
    return ret;
  };
  // -------------------------------------------------------------------------
  LNumber.isRational = function (n) {
    if (!n) {
      return false;
    }
    return n instanceof LRational || LNumber.isNumber(n.num) && LNumber.isNumber(n.denom);
  };
  // -------------------------------------------------------------------------
  LNumber.isInteger = function (n) {
    if (!(LNumber.isNative(n) || n instanceof LNumber)) {
      return false;
    }
    if (LNumber.isFloat(n)) {
      return false;
    }
    if (LNumber.isRational(n)) {
      return false;
    }
    if (LNumber.isComplex(n)) {
      return false;
    }
    return true;
  };
  // -------------------------------------------------------------------------
  LNumber.isNative = function (n) {
    return typeof n === 'bigint' || typeof n === 'number';
  };
  // -------------------------------------------------------------------------
  LNumber.isBigInteger = function (n) {
    return n instanceof LBigInteger || typeof n === 'bigint' || LNumber.isBN(n);
  };
  // -------------------------------------------------------------------------
  LNumber.isBN = function (n) {
    return typeof BN !== 'undefined' && n instanceof BN;
  };
  // -------------------------------------------------------------------------
  LNumber.getArgsType = function (a, b) {
    if (a instanceof LFloat || b instanceof LFloat) {
      return LFloat;
    }
    if (a instanceof LBigInteger || b instanceof LBigInteger) {
      return LBigInteger;
    }
    return LNumber;
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.toString = function (radix) {
    if (Number.isNaN(this.__value__)) {
      return '+nan.0';
    }
    if (radix >= 2 && radix < 36) {
      return this.__value__.toString(radix);
    }
    return this.__value__.toString();
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.asType = function (n) {
    var _type = LNumber.getType(this);
    return LNumber.types[_type] ? LNumber.types[_type](n) : LNumber(n);
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.isBigNumber = function () {
    return typeof this.__value__ === 'bigint' || typeof BN !== 'undefined' && !(this.value instanceof BN);
  };
  // -------------------------------------------------------------------------
  ['floor', 'ceil', 'round'].forEach(function (fn) {
    LNumber.prototype[fn] = function () {
      if (this["float"] || LNumber.isFloat(this.__value__)) {
        return LNumber(Math[fn](this.__value__));
      } else {
        return LNumber(Math[fn](this.valueOf()));
      }
    };
  });
  // -------------------------------------------------------------------------
  LNumber.prototype.valueOf = function () {
    if (LNumber.isNative(this.__value__)) {
      return Number(this.__value__);
    } else if (LNumber.isBN(this.__value__)) {
      return this.__value__.toNumber();
    }
  };
  // -------------------------------------------------------------------------
  // Type coercion matrix
  // -------------------------------------------------------------------------
  var matrix = function () {
    var i = function i(a, b) {
      return [a, b];
    };
    return {
      bigint: {
        bigint: i,
        "float": function float(a, b) {
          return [LFloat(a.valueOf()), b];
        },
        rational: function rational(a, b) {
          return [{
            num: a,
            denom: 1
          }, b];
        },
        complex: function complex(a, b) {
          return [{
            im: 0,
            re: a
          }, b];
        }
      },
      integer: {
        integer: i,
        "float": function float(a, b) {
          return [LFloat(a.valueOf()), b];
        },
        rational: function rational(a, b) {
          return [{
            num: a,
            denom: 1
          }, b];
        },
        complex: function complex(a, b) {
          return [{
            im: 0,
            re: a
          }, b];
        }
      },
      "float": {
        bigint: function bigint(a, b) {
          return [a, b && LFloat(b.valueOf())];
        },
        integer: function integer(a, b) {
          return [a, b && LFloat(b.valueOf())];
        },
        "float": i,
        rational: function rational(a, b) {
          return [a, b && LFloat(b.valueOf())];
        },
        complex: function complex(a, b) {
          return [{
            re: a,
            im: LFloat(0)
          }, b];
        }
      },
      complex: {
        bigint: complex('bigint'),
        integer: complex('integer'),
        "float": complex('float'),
        rational: complex('rational'),
        complex: function complex(a, b) {
          var _LNumber$coerce = LNumber.coerce(a.__re__, b.__re__),
            _LNumber$coerce2 = _slicedToArray(_LNumber$coerce, 2),
            a_re = _LNumber$coerce2[0],
            b_re = _LNumber$coerce2[1];
          var _LNumber$coerce3 = LNumber.coerce(a.__im__, b.__im__),
            _LNumber$coerce4 = _slicedToArray(_LNumber$coerce3, 2),
            a_im = _LNumber$coerce4[0],
            b_im = _LNumber$coerce4[1];
          return [{
            im: a_im,
            re: a_re
          }, {
            im: b_im,
            re: b_re
          }];
        }
      },
      rational: {
        bigint: function bigint(a, b) {
          return [a, b && {
            num: b,
            denom: 1
          }];
        },
        integer: function integer(a, b) {
          return [a, b && {
            num: b,
            denom: 1
          }];
        },
        "float": function float(a, b) {
          return [LFloat(a.valueOf()), b];
        },
        rational: i,
        complex: function complex(a, b) {
          return [{
            im: coerce(a.__type__, b.__im__.__type__, 0)[0],
            re: coerce(a.__type__, b.__re__.__type__, a)[0]
          }, {
            im: coerce(a.__type__, b.__im__.__type__, b.__im__)[0],
            re: coerce(a.__type__, b.__re__.__type__, b.__re__)[0]
          }];
        }
      }
    };
    function complex(type) {
      return function (a, b) {
        return [{
          im: coerce(type, a.__im__.__type__, 0, a.__im__)[1],
          re: coerce(type, a.__re__.__type__, 0, a.__re__)[1]
        }, {
          im: coerce(type, a.__im__.__type__, 0, 0)[1],
          re: coerce(type, b.__type__, 0, b)[1]
        }];
      };
    }
  }();
  // -------------------------------------------------------------------------
  function coerce(type_a, type_b, a, b) {
    return matrix[type_a][type_b](a, b);
  }
  // -------------------------------------------------------------------------
  LNumber.coerce = function (a, b) {
    var a_type = LNumber.getType(a);
    var b_type = LNumber.getType(b);
    if (!matrix[a_type]) {
      throw new Error("LNumber::coerce unknown lhs type ".concat(a_type));
    } else if (!matrix[a_type][b_type]) {
      throw new Error("LNumber::coerce unknown rhs type ".concat(b_type));
    }
    var tmp = matrix[a_type][b_type](a, b);
    return tmp.map(function (n) {
      return LNumber(n, true);
    });
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.coerce = function (n) {
    if (!(typeof n === 'number' || n instanceof LNumber)) {
      throw new Error("LNumber: you can't coerce ".concat(type(n)));
    }
    if (typeof n === 'number') {
      n = LNumber(n);
    }
    return LNumber.coerce(this, n);
  };
  // -------------------------------------------------------------------------
  LNumber.getType = function (n) {
    if (n instanceof LNumber) {
      return n.__type__;
    }
    if (LNumber.isFloat(n)) {
      return 'float';
    }
    if (LNumber.isComplex(n)) {
      return 'complex';
    }
    if (LNumber.isRational(n)) {
      return 'rational';
    }
    if (typeof n === 'number') {
      return 'integer';
    }
    if (typeof BigInt !== 'undefined' && typeof n !== 'bigint' || typeof BN !== 'undefined' && !(n instanceof BN)) {
      return 'bigint';
    }
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.isFloat = function () {
    return !!(LNumber.isFloat(this.__value__) || this["float"]);
  };
  // -------------------------------------------------------------------------
  var mapping = {
    'add': '+',
    'sub': '-',
    'mul': '*',
    'div': '/',
    'rem': '%',
    'or': '|',
    'and': '&',
    'neg': '~',
    'shl': '>>',
    'shr': '<<'
  };
  var rev_mapping = {};
  Object.keys(mapping).forEach(function (key) {
    rev_mapping[mapping[key]] = key;
    LNumber.prototype[key] = function (n) {
      return this.op(mapping[key], n);
    };
  });
  // -------------------------------------------------------------------------
  LNumber._ops = {
    '*': function _(a, b) {
      return a * b;
    },
    '+': function _(a, b) {
      return a + b;
    },
    '-': function _(a, b) {
      if (typeof b === 'undefined') {
        return -a;
      }
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
  // -------------------------------------------------------------------------
  LNumber.prototype.op = function (op, n) {
    if (typeof n === 'undefined') {
      return LNumber(LNumber._ops[op](this.valueOf()));
    }
    if (typeof n === 'number') {
      n = LNumber(n);
    }
    if (Number.isNaN(this.__value__) && !LNumber.isComplex(n) || !LNumber.isComplex(this) && Number.isNaN(n.__value__)) {
      return LNumber(NaN);
    }
    var _this$coerce = this.coerce(n),
      _this$coerce2 = _slicedToArray(_this$coerce, 2),
      a = _this$coerce2[0],
      b = _this$coerce2[1];
    if (a._op) {
      return a._op(op, b);
    }
    return LNumber(LNumber._ops[op](a, b));
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.sqrt = function () {
    var value = this.valueOf();
    if (this.cmp(0) < 0) {
      var im = Math.sqrt(-value);
      return LComplex({
        re: 0,
        im: im
      });
    }
    return LNumber(Math.sqrt(value));
  };
  // -------------------------------------------------------------------------
  var pow = function pow(a, b) {
    var e = typeof a === 'bigint' ? BigInt(1) : 1;
    return new Array(Number(b)).fill(0).reduce(function (x) {
      return x * a;
    }, e);
  };
  // -------------------------------------------------------------------------
  // use native exponential operator if possible (it's way faster)
  // -------------------------------------------------------------------------
  var exp_op = new Function('a,b', 'return a ** b');
  try {
    if (exp_op(2, 2) === 4) {
      pow = exp_op;
    }
  } catch (e) {
    // ignore
  }
  // -------------------------------------------------------------------------
  LNumber.prototype.pow = function (n) {
    var value;
    var _this$coerce3 = this.coerce(n),
      _this$coerce4 = _slicedToArray(_this$coerce3, 2),
      a = _this$coerce4[0],
      b = _this$coerce4[1];
    if (LNumber.isNative(a.__value__) && LNumber.isNative(b.__value__)) {
      value = pow(a.__value__, b.__value__);
    } else if (LNumber.isBN(a.__value__) && LNumber.isBN(b.__value__)) {
      value = this.__value__.pow(n.__value__);
    } else if (a.pow) {
      return a.pow(b);
    }
    return LNumber(value);
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.abs = function () {
    var value = this.__value__;
    if (LNumber.isNative(this.__value__)) {
      if (value < 0) {
        value = -value;
      }
    } else if (LNumber.isBN(value)) {
      value.iabs();
    }
    return new LNumber(value);
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.isOdd = function () {
    if (LNumber.isNative(this.__value__)) {
      if (this.isBigNumber()) {
        return this.__value__ % BigInt(2) === BigInt(1);
      }
      if (this.__type__ === 'float') {
        throw new Error('Invalid number float');
      }
      return this.__value__ % 2 === 1;
    } else if (LNumber.isBN(this.__value__)) {
      return this.__value__.isOdd();
    }
    throw new Error("Invalid number ".concat(this.__type__));
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.isEven = function () {
    return !this.isOdd();
  };
  // -------------------------------------------------------------------------
  LNumber.prototype.cmp = function (n) {
    var _this$coerce5 = this.coerce(n),
      _this$coerce6 = _slicedToArray(_this$coerce5, 2),
      a = _this$coerce6[0],
      b = _this$coerce6[1];
    function cmp(a, b) {
      if (a.__value__ < b.__value__) {
        return -1;
      } else if (a.__value__ === b.__value__) {
        return 0;
      } else {
        return 1;
      }
    }
    if (a.__type__ === 'bigint') {
      if (LNumber.isNative(a.__value__)) {
        return cmp(a, b);
      } else if (LNumber.isBN(a.__value__)) {
        return this.__value__.cmp(b.__value__);
      }
    } else if (a instanceof LFloat) {
      return cmp(a, b);
    }
  };
  // -------------------------------------------------------------------------
  // :: COMPLEX TYPE
  // -------------------------------------------------------------------------
  function LComplex(n) {
    var force = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
    if (typeof this !== 'undefined' && !(this instanceof LComplex) || typeof this === 'undefined') {
      return new LComplex(n, force);
    }
    if (n instanceof LComplex) {
      return LComplex({
        im: n.__im__,
        re: n.__re__
      });
    }
    if (LNumber.isNumber(n) && force) {
      if (!force) {
        return Number(n);
      }
    } else if (!LNumber.isComplex(n)) {
      var msg = "Invalid constructor call for LComplex expect &(:im <num> :re <num>) object but got ".concat(toString(n));
      throw new Error(msg);
    }
    var im = n.im instanceof LNumber ? n.im : LNumber(n.im);
    var re = n.re instanceof LNumber ? n.re : LNumber(n.re);
    this.constant(im, re);
  }
  // -------------------------------------------------------------------------
  LComplex.prototype = Object.create(LNumber.prototype);
  LComplex.prototype.constructor = LComplex;
  // -------------------------------------------------------------------------
  LComplex.prototype.constant = function (im, re) {
    Object.defineProperty(this, '__im__', {
      value: im,
      enumerable: true
    });
    Object.defineProperty(this, '__re__', {
      value: re,
      enumerable: true
    });
    Object.defineProperty(this, '__type__', {
      value: 'complex',
      enumerable: true
    });
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.serialize = function () {
    return {
      re: this.__re__,
      im: this.__im__
    };
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.toRational = function (n) {
    if (LNumber.isFloat(this.__im__) && LNumber.isFloat(this.__re__)) {
      var im = LFloat(this.__im__).toRational(n);
      var re = LFloat(this.__re__).toRational(n);
      return LComplex({
        im: im,
        re: re
      });
    }
    return this;
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.pow = function (n) {
    throw new Error('Not yet implemented');
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.add = function (n) {
    return this.complex_op('add', n, function (a_re, b_re, a_im, b_im) {
      return {
        re: a_re.add(b_re),
        im: a_im.add(b_im)
      };
    });
  };
  // -------------------------------------------------------------------------
  // :: factor is used in / and modulus
  // -------------------------------------------------------------------------
  LComplex.prototype.factor = function () {
    // fix rounding when calculating (/ 1.0 1/10+1/10i)
    if (this.__im__ instanceof LFloat || this.__im__ instanceof LFloat) {
      var re = this.__re__,
        im = this.__im__;
      var x, y;
      if (re instanceof LFloat) {
        x = re.toRational().mul(re.toRational());
      } else {
        x = re.mul(re);
      }
      if (im instanceof LFloat) {
        y = im.toRational().mul(im.toRational());
      } else {
        y = im.mul(im);
      }
      return x.add(y);
    } else {
      return this.__re__.mul(this.__re__).add(this.__im__.mul(this.__im__));
    }
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.modulus = function () {
    return this.factor().sqrt();
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.conjugate = function () {
    return LComplex({
      re: this.__re__,
      im: this.__im__.sub()
    });
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.sqrt = function () {
    var r = this.modulus();
    // code based ok Kawa Scheme source code (file DComplex.java)
    // Copyright (c) 1997  Per M.A. Bothner.
    // Released under MIT License
    var re, im;
    if (r.cmp(0) === 0) {
      re = im = r;
    } else if (this.__re__.cmp(0) === 1) {
      re = LFloat(0.5).mul(r.add(this.__re__)).sqrt();
      im = this.__im__.div(re).div(2);
    } else {
      im = LFloat(0.5).mul(r.sub(this.__re__)).sqrt();
      if (this.__im__.cmp(0) === -1) {
        im = im.sub();
      }
      re = this.__im__.div(im).div(2);
    }
    return LComplex({
      im: im,
      re: re
    });
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.div = function (n) {
    if (LNumber.isNumber(n) && !LNumber.isComplex(n)) {
      if (!(n instanceof LNumber)) {
        n = LNumber(n);
      }
      var _re = this.__re__.div(n);
      var _im = this.__im__.div(n);
      return LComplex({
        re: _re,
        im: _im
      });
    } else if (!LNumber.isComplex(n)) {
      throw new Error('[LComplex::div] Invalid value');
    }
    if (this.cmp(n) === 0) {
      var _this$coerce7 = this.coerce(n),
        _this$coerce8 = _slicedToArray(_this$coerce7, 2),
        _a = _this$coerce8[0],
        _b = _this$coerce8[1];
      var ret = _a.__im__.div(_b.__im__);
      return ret.coerce(_b.__re__)[0];
    }
    var _this$coerce9 = this.coerce(n),
      _this$coerce10 = _slicedToArray(_this$coerce9, 2),
      a = _this$coerce10[0],
      b = _this$coerce10[1];
    var denom = b.factor();
    var conj = b.conjugate();
    var num = a.mul(conj);
    if (!LNumber.isComplex(num)) {
      return num.div(denom);
    }
    var re = num.__re__.op('/', denom);
    var im = num.__im__.op('/', denom);
    return LComplex({
      re: re,
      im: im
    });
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.sub = function (n) {
    return this.complex_op('sub', n, function (a_re, b_re, a_im, b_im) {
      return {
        re: a_re.sub(b_re),
        im: a_im.sub(b_im)
      };
    });
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.mul = function (n) {
    return this.complex_op('mul', n, function (a_re, b_re, a_im, b_im) {
      var ret = {
        re: a_re.mul(b_re).sub(a_im.mul(b_im)),
        im: a_re.mul(b_im).add(b_re.mul(a_im))
      };
      return ret;
    });
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.complex_op = function (name, n, fn) {
    var _this7 = this;
    var calc = function calc(re, im) {
      var result = fn(_this7.__re__, re, _this7.__im__, im);
      if ('im' in result && 're' in result) {
        if (result.im.cmp(0) === 0) {
          return result.re;
        }
        return LComplex(result, true);
      }
      return result;
    };
    if (typeof n === 'undefined') {
      return calc();
    }
    if (LNumber.isNumber(n) && !LNumber.isComplex(n)) {
      if (!(n instanceof LNumber)) {
        n = LNumber(n);
      }
      var _im2 = n.asType(0);
      n = {
        __im__: _im2,
        __re__: n
      };
    } else if (!LNumber.isComplex(n)) {
      throw new Error("[LComplex::".concat(name, "] Invalid value"));
    }
    var re = n.__re__ instanceof LNumber ? n.__re__ : this.__re__.asType(n.__re__);
    var im = n.__im__ instanceof LNumber ? n.__im__ : this.__im__.asType(n.__im__);
    return calc(re, im);
  };
  // -------------------------------------------------------------------------
  LComplex._op = {
    '+': 'add',
    '-': 'sub',
    '*': 'mul',
    '/': 'div'
  };
  // -------------------------------------------------------------------------
  LComplex.prototype._op = function (op, n) {
    var fn = LComplex._op[op];
    return this[fn](n);
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.cmp = function (n) {
    var _this$coerce11 = this.coerce(n),
      _this$coerce12 = _slicedToArray(_this$coerce11, 2),
      a = _this$coerce12[0],
      b = _this$coerce12[1];
    var _a$__re__$coerce = a.__re__.coerce(b.__re__),
      _a$__re__$coerce2 = _slicedToArray(_a$__re__$coerce, 2),
      re_a = _a$__re__$coerce2[0],
      re_b = _a$__re__$coerce2[1];
    var re_cmp = re_a.cmp(re_b);
    if (re_cmp !== 0) {
      return re_cmp;
    } else {
      var _a$__im__$coerce = a.__im__.coerce(b.__im__),
        _a$__im__$coerce2 = _slicedToArray(_a$__im__$coerce, 2),
        im_a = _a$__im__$coerce2[0],
        im_b = _a$__im__$coerce2[1];
      return im_a.cmp(im_b);
    }
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.valueOf = function () {
    return [this.__re__, this.__im__].map(function (x) {
      return x.valueOf();
    });
  };
  // -------------------------------------------------------------------------
  LComplex.prototype.toString = function () {
    var result;
    if (this.__re__.cmp(0) !== 0) {
      result = [toString(this.__re__)];
    } else {
      result = [];
    }
    // NaN and inf already have sign
    var im = this.__im__.valueOf();
    var inf = [Number.NEGATIVE_INFINITY, Number.POSITIVE_INFINITY].includes(im);
    var im_str = toString(this.__im__);
    if (!inf && !Number.isNaN(im)) {
      var zero_check = this.__im__.cmp(0);
      if (zero_check < 0 || zero_check === 0 && this.__im__._minus) {
        result.push('-');
      } else {
        result.push('+');
      }
      im_str = im_str.replace(/^-/, '');
    }
    result.push(im_str);
    result.push('i');
    return result.join('');
  };
  // -------------------------------------------------------------------------
  // :: FLOAT TYPE
  // -------------------------------------------------------------------------
  function LFloat(n) {
    if (typeof this !== 'undefined' && !(this instanceof LFloat) || typeof this === 'undefined') {
      return new LFloat(n);
    }
    if (!LNumber.isNumber(n)) {
      throw new Error('Invalid constructor call for LFloat');
    }
    if (n instanceof LNumber) {
      return LFloat(n.valueOf());
    }
    if (typeof n === 'number') {
      if (Object.is(n, -0)) {
        Object.defineProperty(this, '_minus', {
          value: true
        });
      }
      this.constant(n, 'float');
    }
  }
  // -------------------------------------------------------------------------
  LFloat.prototype = Object.create(LNumber.prototype);
  LFloat.prototype.constructor = LFloat;
  // -------------------------------------------------------------------------
  LFloat.prototype.toString = function () {
    if (this.__value__ === Number.NEGATIVE_INFINITY) {
      return '-inf.0';
    }
    if (this.__value__ === Number.POSITIVE_INFINITY) {
      return '+inf.0';
    }
    if (Number.isNaN(this.__value__)) {
      return '+nan.0';
    }
    var str = this.__value__.toString();
    if (!LNumber.isFloat(this.__value__) && !str.match(/e/i)) {
      var result = str + '.0';
      return this._minus ? '-' + result : result;
    }
    return str.replace(/^([0-9]+)e/, '$1.0e');
  };
  // -------------------------------------------------------------------------
  LFloat.prototype._op = function (op, n) {
    if (n instanceof LNumber) {
      n = n.__value__;
    }
    var fn = LNumber._ops[op];
    if (op === '/' && this.__value__ === 0 && n === 0) {
      return NaN;
    }
    return LFloat(fn(this.__value__, n));
  };
  // -------------------------------------------------------------------------
  // same approximation as in guile scheme
  LFloat.prototype.toRational = function () {
    var n = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;
    if (n === null) {
      return toRational(this.__value__.valueOf());
    }
    return approxRatio(n.valueOf())(this.__value__.valueOf());
  };
  // -------------------------------------------------------------------------
  LFloat.prototype.sqrt = function () {
    var value = this.valueOf();
    if (this.cmp(0) < 0) {
      var im = LFloat(Math.sqrt(-value));
      return LComplex({
        re: 0,
        im: im
      });
    }
    return LFloat(Math.sqrt(value));
  };
  // -------------------------------------------------------------------------
  LFloat.prototype.abs = function () {
    var value = this.valueOf();
    if (value < 0) {
      value = -value;
    }
    return LFloat(value);
  };
  // -------------------------------------------------------------------------
  // ref: https://rosettacode.org/wiki/Convert_decimal_number_to_rational
  // -------------------------------------------------------------------------
  var toRational = approxRatio(1e-10);
  function approxRatio(eps) {
    return function (n) {
      var gcde = function gcde(e, x, y) {
          var _gcd = function _gcd(a, b) {
            return b < e ? a : _gcd(b, a % b);
          };
          if (Number.isNaN(x) || Number.isNaN(y)) {
            return NaN;
          }
          return _gcd(Math.abs(x), Math.abs(y));
        },
        c = gcde(eps ? eps : 1 / 10000, 1, n);
      return LRational({
        num: Math.floor(n / c),
        denom: Math.floor(1 / c)
      });
    };
  }
  // -------------------------------------------------------------------------
  // :: Source: Kawa gnu.math.RatNum.java
  // :: This algorithm is by Alan Bawden. It has been transcribed
  // :: with permission from Kawa copyright M.A. Bothner.
  // :: which was transcribed from from C-Gambit, copyright Marc Feeley.
  // -------------------------------------------------------------------------
  function rationalize(x, y) {
    var a = x.sub(y);
    var b = x.add(y);
    var result;
    if (a.cmp(b) > 0) {
      result = simplest_rational2(b, a);
    } else if (b.cmp(a) <= 0) {
      result = a;
    } else if (a.cmp(0) > 0) {
      result = simplest_rational2(a, b);
    } else if (y.cmp(0) < 0) {
      result = LNumber(simplest_rational2(b.sub(), a.sub())).sub();
    } else {
      result = LNumber(0);
    }
    if (LNumber.isFloat(y) || LNumber.isFloat(x)) {
      return LFloat(result);
    }
    return result;
  }
  // -------------------------------------------------------------------------
  function simplest_rational2(x, y) {
    var fx = LNumber(x).floor();
    var fy = LNumber(y).floor();
    if (x.cmp(fx) < 1) {
      return fx;
    } else if (fx.cmp(fy) === 0) {
      var n = LNumber(1).div(y.sub(fy));
      var d = LNumber(1).div(x.sub(fx));
      return fx.add(LNumber(1).div(simplest_rational2(n, d)));
    } else {
      return fx.add(LNumber(1));
    }
  }
  // -------------------------------------------------------------------------
  function LRational(n) {
    var force = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
    if (typeof this !== 'undefined' && !(this instanceof LRational) || typeof this === 'undefined') {
      return new LRational(n, force);
    }
    if (!LNumber.isRational(n)) {
      throw new Error('Invalid constructor call for LRational');
    }
    var num, denom;
    if (n instanceof LRational) {
      num = LNumber(n.__num__);
      denom = LNumber(n.__denom__);
    } else {
      num = LNumber(n.num);
      denom = LNumber(n.denom);
    }
    if (!force && denom.cmp(0) !== 0) {
      var is_integer = num.op('%', denom).cmp(0) === 0;
      if (is_integer) {
        return LNumber(num.div(denom));
      }
    }
    this.constant(num, denom);
  }
  // -------------------------------------------------------------------------
  LRational.prototype = Object.create(LNumber.prototype);
  LRational.prototype.constructor = LRational;
  // -------------------------------------------------------------------------
  LRational.prototype.constant = function (num, denom) {
    Object.defineProperty(this, '__num__', {
      value: num,
      enumerable: true
    });
    Object.defineProperty(this, '__denom__', {
      value: denom,
      enumerable: true
    });
    Object.defineProperty(this, '__type__', {
      value: 'rational',
      enumerable: true
    });
  };
  // -------------------------------------------------------------------------
  LRational.prototype.serialize = function () {
    return {
      num: this.__num__,
      denom: this.__denom__
    };
  };
  // -------------------------------------------------------------------------
  LRational.prototype.pow = function (n) {
    var cmp = n.cmp(0);
    if (cmp === 0) {
      return LNumber(1);
    }
    if (cmp === -1) {
      n = n.sub();
      var num = this.__denom__.pow(n);
      var denom = this.__num__.pow(n);
      return LRational({
        num: num,
        denom: denom
      });
    }
    var result = this;
    n = n.valueOf();
    while (n > 1) {
      result = result.mul(this);
      n--;
    }
    return result;
  };
  // -------------------------------------------------------------------------
  LRational.prototype.sqrt = function () {
    var num = this.__num__.sqrt();
    var denom = this.__denom__.sqrt();
    if (num instanceof LFloat || denom instanceof LFloat) {
      return num.div(denom);
    }
    return LRational({
      num: num,
      denom: denom
    });
  };
  // -------------------------------------------------------------------------
  LRational.prototype.abs = function () {
    var num = this.__num__;
    var denom = this.__denom__;
    if (num.cmp(0) === -1) {
      num = num.sub();
    }
    if (denom.cmp(0) !== 1) {
      denom = denom.sub();
    }
    return LRational({
      num: num,
      denom: denom
    });
  };
  // -------------------------------------------------------------------------
  LRational.prototype.cmp = function (n) {
    return LNumber(this.valueOf(), true).cmp(n);
  };
  // -------------------------------------------------------------------------
  LRational.prototype.toString = function () {
    var gcd = this.__num__.gcd(this.__denom__);
    var num, denom;
    if (gcd.cmp(1) !== 0) {
      num = this.__num__.div(gcd);
      if (num instanceof LRational) {
        num = LNumber(num.valueOf(true));
      }
      denom = this.__denom__.div(gcd);
      if (denom instanceof LRational) {
        denom = LNumber(denom.valueOf(true));
      }
    } else {
      num = this.__num__;
      denom = this.__denom__;
    }
    var minus = this.cmp(0) < 0;
    if (minus) {
      if (num.abs().cmp(denom.abs()) === 0) {
        return num.toString();
      }
    } else if (num.cmp(denom) === 0) {
      return num.toString();
    }
    return num.toString() + '/' + denom.toString();
  };
  // -------------------------------------------------------------------------
  LRational.prototype.valueOf = function (exact) {
    if (this.__denom__.cmp(0) === 0) {
      if (this.__num__.cmp(0) < 0) {
        return Number.NEGATIVE_INFINITY;
      }
      return Number.POSITIVE_INFINITY;
    }
    if (exact) {
      return LNumber._ops['/'](this.__num__.value, this.__denom__.value);
    }
    return LFloat(this.__num__.valueOf()).div(this.__denom__.valueOf());
  };
  // -------------------------------------------------------------------------
  LRational.prototype.mul = function (n) {
    if (!(n instanceof LNumber)) {
      n = LNumber(n); // handle (--> 1/2 (mul 2))
    }
    if (LNumber.isRational(n)) {
      var num = this.__num__.mul(n.__num__);
      var denom = this.__denom__.mul(n.__denom__);
      return LRational({
        num: num,
        denom: denom
      });
    }
    var _LNumber$coerce5 = LNumber.coerce(this, n),
      _LNumber$coerce6 = _slicedToArray(_LNumber$coerce5, 2),
      a = _LNumber$coerce6[0],
      b = _LNumber$coerce6[1];
    return a.mul(b);
  };
  // -------------------------------------------------------------------------
  LRational.prototype.div = function (n) {
    if (!(n instanceof LNumber)) {
      n = LNumber(n); // handle (--> 1/2 (div 2))
    }
    if (LNumber.isRational(n)) {
      var num = this.__num__.mul(n.__denom__);
      var denom = this.__denom__.mul(n.__num__);
      return LRational({
        num: num,
        denom: denom
      });
    }
    var _LNumber$coerce7 = LNumber.coerce(this, n),
      _LNumber$coerce8 = _slicedToArray(_LNumber$coerce7, 2),
      a = _LNumber$coerce8[0],
      b = _LNumber$coerce8[1];
    var ret = a.div(b);
    return ret;
  };
  // -------------------------------------------------------------------------
  LRational.prototype._op = function (op, n) {
    return this[rev_mapping[op]](n);
  };
  // -------------------------------------------------------------------------
  LRational.prototype.sub = function (n) {
    if (typeof n === 'undefined') {
      return this.mul(-1);
    }
    if (!(n instanceof LNumber)) {
      n = LNumber(n); // handle (--> 1/2 (sub 1))
    }
    if (LNumber.isRational(n)) {
      var num = n.__num__.sub();
      var denom = n.__denom__;
      return this.add(LRational({
        num: num,
        denom: denom
      }));
    }
    if (!(n instanceof LNumber)) {
      n = LNumber(n).sub();
    } else {
      n = n.sub();
    }
    var _LNumber$coerce9 = LNumber.coerce(this, n),
      _LNumber$coerce10 = _slicedToArray(_LNumber$coerce9, 2),
      a = _LNumber$coerce10[0],
      b = _LNumber$coerce10[1];
    return a.add(b);
  };
  // -------------------------------------------------------------------------
  LRational.prototype.add = function (n) {
    if (!(n instanceof LNumber)) {
      n = LNumber(n); // handle (--> 1/2 (add 1))
    }
    if (LNumber.isRational(n)) {
      var a_denom = this.__denom__;
      var b_denom = n.__denom__;
      var a_num = this.__num__;
      var b_num = n.__num__;
      var denom, num;
      if (a_denom !== b_denom) {
        num = b_denom.mul(a_num).add(b_num.mul(a_denom));
        denom = a_denom.mul(b_denom);
      } else {
        num = a_num.add(b_num);
        denom = a_denom;
      }
      return LRational({
        num: num,
        denom: denom
      });
    }
    if (LNumber.isFloat(n)) {
      return LFloat(this.valueOf()).add(n);
    }
    var _LNumber$coerce11 = LNumber.coerce(this, n),
      _LNumber$coerce12 = _slicedToArray(_LNumber$coerce11, 2),
      a = _LNumber$coerce12[0],
      b = _LNumber$coerce12[1];
    return a.add(b);
  };
  // -------------------------------------------------------------------------
  function LBigInteger(n, _native2) {
    if (typeof this !== 'undefined' && !(this instanceof LBigInteger) || typeof this === 'undefined') {
      return new LBigInteger(n, _native2);
    }
    if (n instanceof LBigInteger) {
      return LBigInteger(n.__value__, n._native);
    }
    if (!LNumber.isBigInteger(n)) {
      throw new Error('Invalid constructor call for LBigInteger');
    }
    this.constant(n, 'bigint');
    Object.defineProperty(this, '_native', {
      value: _native2
    });
  }
  // -------------------------------------------------------------------------
  LBigInteger.prototype = Object.create(LNumber.prototype);
  LBigInteger.prototype.constructor = LBigInteger;
  // -------------------------------------------------------------------------
  LBigInteger.bn_op = {
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
  LBigInteger.prototype.serialize = function () {
    return this.__value__.toString();
  };
  // -------------------------------------------------------------------------
  LBigInteger.prototype._op = function (op, n) {
    if (typeof n === 'undefined') {
      if (LNumber.isBN(this.__value__)) {
        op = LBigInteger.bn_op[op];
        return LBigInteger(this.__value__.clone()[op](), false);
      }
      return LBigInteger(LNumber._ops[op](this.__value__), true);
    }
    if (LNumber.isBN(this.__value__) && LNumber.isBN(n.__value__)) {
      op = LBigInteger.bn_op[op];
      return LBigInteger(this.__value__.clone()[op](n), false);
    }
    var ret = LNumber._ops[op](this.__value__, n.__value__);
    if (op === '/') {
      var is_integer = this.op('%', n).cmp(0) === 0;
      if (is_integer) {
        return LNumber(ret);
      }
      return LRational({
        num: this,
        denom: n
      });
    }
    // use native calculation because it's real bigint value
    return LBigInteger(ret, true);
  };
  // -------------------------------------------------------------------------
  LBigInteger.prototype.sqrt = function () {
    var value;
    var minus = this.cmp(0) < 0;
    if (LNumber.isNative(this.__value__)) {
      value = LNumber(Math.sqrt(minus ? -this.valueOf() : this.valueOf()));
    } else if (LNumber.isBN(this.__value__)) {
      value = minus ? this.__value__.neg().sqrt() : this.__value__.sqrt();
    }
    if (minus) {
      return LComplex({
        re: 0,
        im: value
      });
    }
    return value;
  };
  // -------------------------------------------------------------------------
  LNumber.NaN = LNumber(NaN);
  // -------------------------------------------------------------------------
  // :: Port abstraction - read should be a function that return next line
  // -------------------------------------------------------------------------
  function InputPort(read) {
    var _this8 = this;
    if (typeof this !== 'undefined' && !(this instanceof InputPort) || typeof this === 'undefined') {
      return new InputPort(read);
    }
    typecheck('InputPort', read, 'function');
    read_only(this, '__type__', text_port);
    var parser;
    Object.defineProperty(this, '__parser__', {
      enumerable: true,
      get: function get() {
        return parser;
      },
      set: function set(value) {
        typecheck('InputPort::__parser__', value, 'parser');
        parser = value;
      }
    });
    this._read = read;
    this._with_parser = this._with_init_parser.bind(this, /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee14() {
      var line;
      return _regeneratorRuntime.wrap(function _callee14$(_context14) {
        while (1) {
          switch (_context14.prev = _context14.next) {
            case 0:
              if (_this8.char_ready()) {
                _context14.next = 5;
                break;
              }
              _context14.next = 3;
              return _this8._read();
            case 3:
              line = _context14.sent;
              parser = new Parser(line, {
                env: _this8
              });
            case 5:
              return _context14.abrupt("return", _this8.__parser__);
            case 6:
            case "end":
              return _context14.stop();
          }
        }
      }, _callee14);
    })));
    this.char_ready = function () {
      return !!this.__parser__ && this.__parser__.__lexer__.peek() !== eof;
    };
    this._make_defaults();
  }
  InputPort.prototype._make_defaults = function () {
    this.read = this._with_parser(function (parser) {
      return parser.read_object();
    });
    this.read_line = this._with_parser(function (parser) {
      return parser.__lexer__.read_line();
    });
    this.read_char = this._with_parser(function (parser) {
      return parser.__lexer__.read_char();
    });
    this.read_string = this._with_parser(function (parser, number) {
      if (!LNumber.isInteger(number)) {
        var _type4 = LNumber.getType(number);
        typeErrorMessage('read-string', _type4, 'integer');
      }
      return parser.__lexer__.read_string(number.valueOf());
    });
    this.peek_char = this._with_parser(function (parser) {
      return parser.__lexer__.peek_char();
    });
  };
  InputPort.prototype._with_init_parser = function (make_parser, fn) {
    var self = this;
    return /*#__PURE__*/_asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee15() {
      var parser,
        _len16,
        args,
        _key16,
        _args17 = arguments;
      return _regeneratorRuntime.wrap(function _callee15$(_context15) {
        while (1) {
          switch (_context15.prev = _context15.next) {
            case 0:
              _context15.next = 2;
              return make_parser.call(self);
            case 2:
              parser = _context15.sent;
              for (_len16 = _args17.length, args = new Array(_len16), _key16 = 0; _key16 < _len16; _key16++) {
                args[_key16] = _args17[_key16];
              }
              return _context15.abrupt("return", fn.apply(void 0, [parser].concat(args)));
            case 5:
            case "end":
              return _context15.stop();
          }
        }
      }, _callee15);
    }));
  };
  InputPort.prototype.is_open = function () {
    return this._with_parser !== null;
  };
  InputPort.prototype.close = function () {
    var _this9 = this;
    this.__parser__ = null;
    // make content garbage collected, we assign null,
    // because the value is in prototype
    this._with_parser = null;
    ['read', 'close', 'read_char', 'peek-char', 'read_line'].forEach(function (name) {
      _this9[name] = function () {
        throw new Error('input-port: port is closed');
      };
    });
    this.char_ready = function () {
      return false;
    };
  };
  InputPort.prototype.toString = function () {
    return '#<input-port>';
  };
  // -------------------------------------------------------------------------
  function OutputPort(write) {
    if (typeof this !== 'undefined' && !(this instanceof OutputPort) || typeof this === 'undefined') {
      return new OutputPort(write);
    }
    typecheck('OutputPort', write, 'function');
    read_only(this, '__type__', text_port);
    this.write = write;
  }
  OutputPort.prototype.is_open = function () {
    return this._closed !== true;
  };
  OutputPort.prototype.close = function () {
    Object.defineProperty(this, '_closed', {
      get: function get() {
        return true;
      },
      set: function set() {},
      configurable: false,
      enumerable: false
    });
    this.write = function () {
      throw new Error('output-port: port is closed');
    };
  };
  OutputPort.prototype.flush = function () {
    // do nothing
  };
  OutputPort.prototype.toString = function () {
    return '#<output-port>';
  };
  // -------------------------------------------------------------------------
  var BufferedOutputPort = /*#__PURE__*/function (_OutputPort) {
    _inherits(BufferedOutputPort, _OutputPort);
    var _super2 = _createSuper(BufferedOutputPort);
    function BufferedOutputPort(fn) {
      var _this10;
      _classCallCheck(this, BufferedOutputPort);
      _this10 = _super2.call(this, function () {
        var _this11;
        return (_this11 = _this10)._write.apply(_this11, arguments);
      });
      typecheck('BufferedOutputPort', fn, 'function');
      read_only(_assertThisInitialized(_this10), '_fn', fn, {
        hidden: true
      });
      read_only(_assertThisInitialized(_this10), '_buffer', [], {
        hidden: true
      });
      return _this10;
    }
    _createClass(BufferedOutputPort, [{
      key: "flush",
      value: function flush() {
        if (this._buffer.length) {
          this._fn(this._buffer.join(''));
          this._buffer.length = 0;
        }
      }
    }, {
      key: "_write",
      value: function _write() {
        var _this12 = this;
        for (var _len17 = arguments.length, args = new Array(_len17), _key17 = 0; _key17 < _len17; _key17++) {
          args[_key17] = arguments[_key17];
        }
        if (args.length) {
          args.forEach(function (arg) {
            _this12._buffer.push(arg);
          });
          var last_value = this._buffer[this._buffer.length - 1];
          if (last_value.match(/\n$/)) {
            this._buffer[this._buffer.length - 1] = last_value.replace(/\n$/, '');
            this.flush();
          }
        }
      }
    }]);
    return BufferedOutputPort;
  }(OutputPort); // -------------------------------------------------------------------------
  function OutputStringPort(toString) {
    var _this13 = this;
    if (typeof this !== 'undefined' && !(this instanceof OutputStringPort) || typeof this === 'undefined') {
      return new OutputStringPort(toString);
    }
    typecheck('OutputStringPort', toString, 'function');
    read_only(this, '__type__', text_port);
    read_only(this, '__buffer__', []);
    this.write = function (x) {
      if (!LString.isString(x)) {
        x = toString(x);
      } else {
        x = x.valueOf();
      }
      _this13.__buffer__.push(x);
    };
  }
  OutputStringPort.prototype = Object.create(OutputPort.prototype);
  OutputStringPort.prototype.constructor = OutputStringPort;
  OutputStringPort.prototype.toString = function () {
    return '#<output-port (string)>';
  };
  OutputStringPort.prototype.valueOf = function () {
    return this.__buffer__.map(function (x) {
      return x.valueOf();
    }).join('');
  };
  // -------------------------------------------------------------------------
  function OutputFilePort(filename, fd) {
    var _this14 = this;
    if (typeof this !== 'undefined' && !(this instanceof OutputFilePort) || typeof this === 'undefined') {
      return new OutputFilePort(filename, fd);
    }
    typecheck('OutputFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
    read_only(this, '_fd', fd.valueOf(), {
      hidden: true
    });
    read_only(this, '__type__', text_port);
    this.write = function (x) {
      if (!LString.isString(x)) {
        x = toString(x);
      } else {
        x = x.valueOf();
      }
      _this14.fs().write(_this14._fd, x, function (err) {
        if (err) {
          throw err;
        }
      });
    };
  }
  OutputFilePort.prototype = Object.create(OutputPort.prototype);
  OutputFilePort.prototype.constructor = OutputFilePort;
  OutputFilePort.prototype.fs = function () {
    if (!this._fs) {
      this._fs = this.internal('fs');
    }
    return this._fs;
  };
  OutputFilePort.prototype.internal = function (name) {
    return user_env.get('**internal-env**').get(name);
  };
  OutputFilePort.prototype.close = function () {
    var _this15 = this;
    return new Promise(function (resolve, reject) {
      _this15.fs().close(_this15._fd, function (err) {
        if (err) {
          reject(err);
        } else {
          read_only(_this15, '_fd', null, {
            hidden: true
          });
          OutputPort.prototype.close.call(_this15);
          resolve();
        }
      });
    });
  };
  OutputFilePort.prototype.toString = function () {
    return "#<output-port ".concat(this.__filename__, ">");
  };
  // -------------------------------------------------------------------------
  function InputStringPort(string, env) {
    var _this16 = this;
    if (typeof this !== 'undefined' && !(this instanceof InputStringPort) || typeof this === 'undefined') {
      return new InputStringPort(string);
    }
    typecheck('InputStringPort', string, 'string');
    env = env || global_env;
    string = string.valueOf();
    this._with_parser = this._with_init_parser.bind(this, function () {
      if (!_this16.__parser__) {
        _this16.__parser__ = new Parser(string, {
          env: env
        });
      }
      return _this16.__parser__;
    });
    read_only(this, '__type__', text_port);
    this._make_defaults();
  }
  InputStringPort.prototype.char_ready = function () {
    return true;
  };
  InputStringPort.prototype = Object.create(InputPort.prototype);
  InputStringPort.prototype.constructor = InputStringPort;
  InputStringPort.prototype.toString = function () {
    return "#<input-port (string)>";
  };
  // -------------------------------------------------------------------------
  function InputByteVectorPort(bytevectors) {
    if (typeof this !== 'undefined' && !(this instanceof InputByteVectorPort) || typeof this === 'undefined') {
      return new InputByteVectorPort(bytevectors);
    }
    typecheck('InputByteVectorPort', bytevectors, 'uint8array');
    read_only(this, '__vector__', bytevectors);
    read_only(this, '__type__', binary_port);
    var index = 0;
    Object.defineProperty(this, '__index__', {
      enumerable: true,
      get: function get() {
        return index;
      },
      set: function set(value) {
        typecheck('InputByteVectorPort::__index__', value, 'number');
        if (value instanceof LNumber) {
          value = value.valueOf();
        }
        if (typeof value === 'bigint') {
          value = Number(value);
        }
        if (Math.floor(value) !== value) {
          throw new Error('InputByteVectorPort::__index__ value is ' + 'not integer');
        }
        index = value;
      }
    });
  }
  InputByteVectorPort.prototype = Object.create(InputPort.prototype);
  InputByteVectorPort.prototype.constructor = InputByteVectorPort;
  InputByteVectorPort.prototype.toString = function () {
    return "#<input-port (bytevector)>";
  };
  InputByteVectorPort.prototype.close = function () {
    var _this17 = this;
    read_only(this, '__vector__', _nil);
    ['read_u8', 'close', 'peek_u8', 'read_u8_vector'].forEach(function (name) {
      _this17[name] = function () {
        throw new Error('Input-binary-port: port is closed');
      };
    });
    this.char_ready = function () {
      return false;
    };
  };
  InputByteVectorPort.prototype.u8_ready = function () {
    return true;
  };
  InputByteVectorPort.prototype.peek_u8 = function () {
    if (this.__index__ >= this.__vector__.length) {
      return eof;
    }
    return this.__vector__[this.__index__];
  };
  InputByteVectorPort.prototype.skip = function () {
    if (this.__index__ <= this.__vector__.length) {
      ++this.__index__;
    }
  };
  InputByteVectorPort.prototype.read_u8 = function () {
    var _byte = this.peek_u8();
    this.skip();
    return _byte;
  };
  InputByteVectorPort.prototype.read_u8_vector = function (len) {
    if (typeof len === 'undefined') {
      len = this.__vector__.length;
    } else if (len > this.__index__ + this.__vector__.length) {
      len = this.__index__ + this.__vector__.length;
    }
    if (this.peek_u8() === eof) {
      return eof;
    }
    return this.__vector__.slice(this.__index__, len);
  };
  // -------------------------------------------------------------------------
  function OutputByteVectorPort() {
    if (typeof this !== 'undefined' && !(this instanceof OutputByteVectorPort) || typeof this === 'undefined') {
      return new OutputByteVectorPort();
    }
    read_only(this, '__type__', binary_port);
    read_only(this, '_buffer', [], {
      hidden: true
    });
    this.write = function (x) {
      typecheck('write', x, ['number', 'uint8array']);
      if (LNumber.isNumber(x)) {
        this._buffer.push(x.valueOf());
      } else {
        var _this$_buffer;
        (_this$_buffer = this._buffer).push.apply(_this$_buffer, _toConsumableArray(Array.from(x)));
      }
    };
    Object.defineProperty(this, '__buffer__', {
      enumerable: true,
      get: function get() {
        return Uint8Array.from(this._buffer);
      }
    });
  }
  OutputByteVectorPort.prototype = Object.create(OutputPort.prototype);
  OutputByteVectorPort.prototype.constructor = OutputByteVectorPort;
  OutputByteVectorPort.prototype.close = function () {
    OutputPort.prototype.close.call(this);
    read_only(this, '_buffer', null, {
      hidden: true
    });
  };
  OutputByteVectorPort.prototype._close_guard = function () {
    if (this._closed) {
      throw new Error('output-port: binary port is closed');
    }
  };
  OutputByteVectorPort.prototype.write_u8 = function (_byte2) {
    typecheck('OutputByteVectorPort::write_u8', _byte2, 'number');
    this.write(_byte2);
  };
  OutputByteVectorPort.prototype.write_u8_vector = function (vector) {
    typecheck('OutputByteVectorPort::write_u8_vector', vector, 'uint8array');
    this.write(vector);
  };
  OutputByteVectorPort.prototype.toString = function () {
    return '#<output-port (bytevector)>';
  };
  OutputByteVectorPort.prototype.valueOf = function () {
    return this.__buffer__;
  };
  // -------------------------------------------------------------------------
  function InputFilePort(content, filename) {
    if (typeof this !== 'undefined' && !(this instanceof InputFilePort) || typeof this === 'undefined') {
      return new InputFilePort(content, filename);
    }
    InputStringPort.call(this, content);
    typecheck('InputFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
  }
  InputFilePort.prototype = Object.create(InputStringPort.prototype);
  InputFilePort.prototype.constructor = InputFilePort;
  InputFilePort.prototype.toString = function () {
    return "#<input-port (".concat(this.__filename__, ")>");
  };
  // -------------------------------------------------------------------------
  function InputBinaryFilePort(content, filename) {
    if (typeof this !== 'undefined' && !(this instanceof InputBinaryFilePort) || typeof this === 'undefined') {
      return new InputBinaryFilePort(content, filename);
    }
    InputByteVectorPort.call(this, content);
    typecheck('InputBinaryFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
  }
  InputBinaryFilePort.prototype = Object.create(InputByteVectorPort.prototype);
  InputBinaryFilePort.prototype.constructor = InputBinaryFilePort;
  InputBinaryFilePort.prototype.toString = function () {
    return "#<input-binary-port (".concat(this.__filename__, ")>");
  };
  // -------------------------------------------------------------------------
  function OutputBinaryFilePort(filename, fd) {
    var _this18 = this;
    if (typeof this !== 'undefined' && !(this instanceof OutputBinaryFilePort) || typeof this === 'undefined') {
      return new OutputBinaryFilePort(filename, fd);
    }
    typecheck('OutputBinaryFilePort', filename, 'string');
    read_only(this, '__filename__', filename);
    read_only(this, '_fd', fd.valueOf(), {
      hidden: true
    });
    read_only(this, '__type__', binary_port);
    var fs, Buffer;
    this.write = function (x) {
      typecheck('write', x, ['number', 'uint8array']);
      var buffer;
      if (!fs) {
        fs = _this18.internal('fs');
      }
      if (!Buffer) {
        Buffer = _this18.internal('Buffer');
      }
      if (LNumber.isNumber(x)) {
        buffer = Buffer.from([x.valueOf()]);
      } else {
        buffer = Buffer.from(Array.from(x));
      }
      return new Promise(function (resolve, reject) {
        fs.write(_this18._fd, buffer, function (err) {
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        });
      });
    };
  }
  OutputBinaryFilePort.prototype = Object.create(OutputFilePort.prototype);
  OutputBinaryFilePort.prototype.constructor = OutputBinaryFilePort;
  OutputBinaryFilePort.prototype.write_u8 = function (_byte3) {
    typecheck('OutputByteVectorPort::write_u8', _byte3, 'number');
    this.write(_byte3);
  };
  OutputBinaryFilePort.prototype.write_u8_vector = function (vector) {
    typecheck('OutputByteVectorPort::write_u8_vector', vector, 'uint8array');
    this.write(vector);
  };
  // -------------------------------------------------------------------------
  var binary_port = Symbol["for"]('binary');
  var text_port = Symbol["for"]('text');
  var eof = new EOF();
  function EOF() {}
  EOF.prototype.toString = function () {
    return '#<eof>';
  };
  // -------------------------------------------------------------------------
  // Simpler way to create interpreter with interaction-environment
  // -------------------------------------------------------------------------
  function Interpreter(name) {
    var _this19 = this;
    var _ref27 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
      stderr = _ref27.stderr,
      stdin = _ref27.stdin,
      stdout = _ref27.stdout,
      _ref27$command_line = _ref27.command_line,
      command_line = _ref27$command_line === void 0 ? null : _ref27$command_line,
      obj = _objectWithoutProperties(_ref27, _excluded3);
    if (typeof this !== 'undefined' && !(this instanceof Interpreter) || typeof this === 'undefined') {
      return new Interpreter(name, _objectSpread({
        stdin: stdin,
        stdout: stdout,
        stderr: stderr,
        command_line: command_line
      }, obj));
    }
    if (typeof name === 'undefined') {
      name = 'anonymous';
    }
    this.__env__ = user_env.inherit(name, obj);
    this.__env__.set('parent.frame', doc('parent.frame', function () {
      return _this19.__env__;
    }, global_env.__env__['parent.frame'].__doc__));
    var defaults_name = '**interaction-environment-defaults**';
    this.set(defaults_name, get_props(obj).concat(defaults_name));
    var inter = internal_env.inherit("internal-".concat(name));
    if (is_port(stdin)) {
      inter.set('stdin', stdin);
    }
    if (is_port(stderr)) {
      inter.set('stderr', stderr);
    }
    if (is_port(stdout)) {
      inter.set('stdout', stdout);
    }
    inter.set('command-line', command_line);
    set_interaction_env(this.__env__, inter);
  }
  // -------------------------------------------------------------------------
  Interpreter.prototype.exec = function (code) {
    var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
    var _options$use_dynamic = options.use_dynamic,
      use_dynamic = _options$use_dynamic === void 0 ? false : _options$use_dynamic,
      dynamic_env = options.dynamic_env,
      env = options.env;
    typecheck('Interpreter::exec', code, ['string', 'array'], 1);
    typecheck('Interpreter::exec', use_dynamic, 'boolean', 2);
    // simple solution to overwrite this variable in each interpreter
    // before evaluation of user code
    if (!env) {
      env = this.__env__;
    }
    if (!dynamic_env) {
      dynamic_env = env;
    }
    global_env.set('**interaction-environment**', this.__env__);
    return exec(code, {
      env: env,
      dynamic_env: dynamic_env,
      use_dynamic: use_dynamic
    });
  };
  // -------------------------------------------------------------------------
  Interpreter.prototype.get = function (value) {
    var result = this.__env__.get(value);
    if (is_function(result)) {
      return result.bind(this.__env__);
    }
    return result;
  };
  // -------------------------------------------------------------------------
  Interpreter.prototype.set = function (name, value) {
    return this.__env__.set(name, value);
  };
  // -------------------------------------------------------------------------
  Interpreter.prototype.constant = function (name, value) {
    return this.__env__.constant(name, value);
  };
  // -------------------------------------------------------------------------
  // Lips Exception used in error function
  // -------------------------------------------------------------------------
  function LipsError(message, args) {
    this.name = 'LipsError';
    this.message = message;
    this.args = args;
    this.stack = new Error().stack;
  }
  LipsError.prototype = new Error();
  LipsError.prototype.constructor = LipsError;
  // -------------------------------------------------------------------------
  // :: Environment constructor (parent and name arguments are optional)
  // -------------------------------------------------------------------------
  function Environment(obj, parent, name) {
    if (arguments.length === 1) {
      if (_typeof(arguments[0]) === 'object') {
        obj = arguments[0];
        parent = null;
      } else if (typeof arguments[0] === 'string') {
        obj = {};
        parent = null;
        name = arguments[0];
      }
    }
    this.__docs__ = new Map();
    this.__env__ = obj;
    this.__parent__ = parent;
    this.__name__ = name || 'anonymous';
  }
  // -------------------------------------------------------------------------
  Environment.prototype.list = function () {
    return get_props(this.__env__);
  };
  // -------------------------------------------------------------------------
  Environment.prototype.fs = function () {
    return this.get('**fs**');
  };
  // -------------------------------------------------------------------------
  Environment.prototype.unset = function (name) {
    if (name instanceof LSymbol) {
      name = name.valueOf();
    }
    if (name instanceof LString) {
      name = name.valueOf();
    }
    delete this.__env__[name];
  };
  // -------------------------------------------------------------------------
  Environment.prototype.inherit = function (name) {
    var obj = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
    if (_typeof(name) === "object") {
      obj = name;
    }
    if (!name || _typeof(name) === "object") {
      name = 'child of ' + (this.__name__ || 'unknown');
    }
    return new Environment(obj || {}, this, name);
  };
  // -------------------------------------------------------------------------
  // :: Lookup function for variable doc strings
  // -------------------------------------------------------------------------
  Environment.prototype.doc = function (name) {
    var value = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;
    var dump = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;
    if (name instanceof LSymbol) {
      name = name.__name__;
    }
    if (name instanceof LString) {
      name = name.valueOf();
    }
    if (value) {
      if (!dump) {
        value = trim_lines(value);
      }
      this.__docs__.set(name, value);
      return this;
    }
    if (this.__docs__.has(name)) {
      return this.__docs__.get(name);
    }
    if (this.__parent__) {
      return this.__parent__.doc(name);
    }
  };
  // -------------------------------------------------------------------------
  // :: Function creates frame environment for usage in functions
  // :: frames are used to it's easier to find environments of the functions
  // :: in scope chain, they are dummy environments just for lookup
  // -------------------------------------------------------------------------
  Environment.prototype.new_frame = function (fn, args) {
    var frame = this.inherit('__frame__');
    frame.set('parent.frame', doc('parent.frame', function () {
      var n = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 1;
      n = n.valueOf();
      var scope = frame.__parent__;
      if (!is_env(scope)) {
        return _nil;
      }
      if (n <= 0) {
        return scope;
      }
      var parent_frame = scope.get('parent.frame');
      return parent_frame(n - 1);
    }, global_env.__env__['parent.frame'].__doc__));
    args.callee = fn;
    frame.set('arguments', args);
    return frame;
  };
  // -------------------------------------------------------------------------
  Environment.prototype._lookup = function (symbol) {
    if (symbol instanceof LSymbol) {
      symbol = symbol.__name__;
    }
    if (symbol instanceof LString) {
      symbol = symbol.valueOf();
    }
    if (this.__env__.hasOwnProperty(symbol)) {
      return Value(this.__env__[symbol]);
    }
    if (this.__parent__) {
      return this.__parent__._lookup(symbol);
    }
  };
  // -------------------------------------------------------------------------
  Environment.prototype.toString = function () {
    return '#<environment:' + this.__name__ + '>';
  };
  // -------------------------------------------------------------------------
  Environment.prototype.clone = function () {
    var _this20 = this;
    // duplicate refs
    var env = {};
    // TODO: duplicated Symbols
    Object.keys(this.__env__).forEach(function (key) {
      env[key] = _this20.__env__[key];
    });
    return new Environment(env, this.__parent__, this.__name__);
  };
  // -------------------------------------------------------------------------
  Environment.prototype.merge = function (env) {
    var name = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 'merge';
    typecheck('Environment::merge', env, 'environment');
    return this.inherit(name, env.__env__);
  };
  // -------------------------------------------------------------------------
  // Value returned in lookup if found value in env and in promise_all
  // -------------------------------------------------------------------------
  function Value(value) {
    if (typeof this !== 'undefined' && !(this instanceof Value) || typeof this === 'undefined') {
      return new Value(value);
    }
    this.value = value;
  }
  // -------------------------------------------------------------------------
  Value.isUndefined = function (x) {
    return x instanceof Value && typeof x.value === 'undefined';
  };
  // -------------------------------------------------------------------------
  Value.prototype.valueOf = function () {
    return this.value;
  };
  // -------------------------------------------------------------------------
  // :: Different object than value used as object for (values)
  // -------------------------------------------------------------------------
  function Values(values) {
    if (values.length) {
      if (values.length === 1) {
        return values[0];
      }
    }
    if (typeof this !== 'undefined' && !(this instanceof Values) || typeof this === 'undefined') {
      return new Values(values);
    }
    this.__values__ = values;
  }
  Values.prototype.toString = function () {
    return this.__values__.map(function (x) {
      return toString(x);
    }).join('\n');
  };
  Values.prototype.valueOf = function () {
    return this.__values__;
  };
  // -------------------------------------------------------------------------
  Environment.prototype.get = function (symbol) {
    var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
    // we keep original environment as context for bind
    // so print will get user stdout
    typecheck('Environment::get', symbol, ['symbol', 'string']);
    var _options$throwError = options.throwError,
      throwError = _options$throwError === void 0 ? true : _options$throwError;
    var name = symbol;
    if (name instanceof LSymbol || name instanceof LString) {
      name = name.valueOf();
    }
    var value = this._lookup(name);
    if (value instanceof Value) {
      if (Value.isUndefined(value)) {
        return undefined;
      }
      return patch_value(value.valueOf());
    }
    var parts;
    if (symbol instanceof LSymbol && symbol[LSymbol.object]) {
      // dot notation symbols from syntax-rules that are gensyms
      parts = symbol[LSymbol.object];
    } else if (typeof name === 'string') {
      parts = name.split('.').filter(Boolean);
    }
    if (parts && parts.length > 0) {
      var _parts = parts,
        _parts2 = _toArray(_parts),
        first = _parts2[0],
        rest = _parts2.slice(1);
      value = this._lookup(first);
      if (rest.length) {
        try {
          if (value instanceof Value) {
            value = value.valueOf();
          } else {
            value = get(root, first);
            if (is_function(value)) {
              value = unbind(value);
            }
          }
          if (typeof value !== 'undefined') {
            // object accessor
            return get.apply(void 0, [value].concat(_toConsumableArray(rest)));
          }
        } catch (e) {
          throw e;
        }
      } else if (value instanceof Value) {
        return patch_value(value.valueOf());
      }
      value = get(root, name);
    }
    if (typeof value !== 'undefined') {
      return value;
    }
    if (throwError) {
      throw new Error("Unbound variable `" + name.toString() + "'");
    }
  };
  // -------------------------------------------------------------------------
  Environment.prototype.set = function (name, value) {
    var doc = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;
    typecheck('Environment::set', name, ['string', 'symbol']);
    if (LNumber.isNumber(value)) {
      value = LNumber(value);
    }
    if (name instanceof LSymbol) {
      name = name.__name__;
    }
    if (name instanceof LString) {
      name = name.valueOf();
    }
    this.__env__[name] = value;
    if (doc) {
      this.doc(name, doc, true);
    }
    return this;
  };
  // -------------------------------------------------------------------------
  // For internal use only
  // -------------------------------------------------------------------------
  Environment.prototype.constant = function (name, value) {
    var _this21 = this;
    if (this.__env__.hasOwnProperty(name)) {
      throw new Error("Environment::constant: ".concat(name, " already exists"));
    }
    if (arguments.length === 1 && is_plain_object(arguments[0])) {
      var obj = arguments[0];
      Object.keys(obj).forEach(function (key) {
        _this21.constant(name, obj[key]);
      });
    } else {
      Object.defineProperty(this.__env__, name, {
        value: value,
        enumerable: true
      });
    }
    return this;
  };
  // -------------------------------------------------------------------------
  Environment.prototype.has = function (name) {
    return this.__env__.hasOwnProperty(name);
  };
  // -------------------------------------------------------------------------
  Environment.prototype.ref = function (name) {
    var env = this;
    while (true) {
      if (!env) {
        break;
      }
      if (env.has(name)) {
        return env;
      }
      env = env.__parent__;
    }
  };
  // -------------------------------------------------------------------------
  Environment.prototype.parents = function () {
    var env = this;
    var result = [];
    while (env) {
      result.unshift(env);
      env = env.__parent__;
    }
    return result;
  };
  // -------------------------------------------------------------------------
  // :: Quote function used to pause evaluation from Macro
  // -------------------------------------------------------------------------
  function quote(value) {
    if (is_promise(value)) {
      return value.then(quote);
    }
    if (value instanceof Pair || value instanceof LSymbol) {
      value[__data__] = true;
    }
    return value;
  }
  // -------------------------------------------------------------------------------
  var native_lambda = parse(tokenize("(lambda ()\n                                      \"[native code]\"\n                                      (throw \"Invalid Invocation\"))"))[0];
  // -------------------------------------------------------------------------------
  var get = doc('get', function get(object) {
    var value;
    for (var _len18 = arguments.length, args = new Array(_len18 > 1 ? _len18 - 1 : 0), _key18 = 1; _key18 < _len18; _key18++) {
      args[_key18 - 1] = arguments[_key18];
    }
    var len = args.length;
    while (args.length) {
      // if arg is symbol someone probably want to get __fn__ from binded function
      if (is_function(object) && _typeof(args[0]) !== 'symbol') {
        object = unbind(object);
      }
      var arg = args.shift();
      var name = unbox(arg);
      // the value was set to false to prevent resolving
      // by Real Promises #153
      if (name === 'then' && object instanceof QuotedPromise) {
        value = QuotedPromise.prototype.then;
      } else if (name === '__code__' && is_function(object) && typeof object.__code__ === 'undefined') {
        value = native_lambda;
      } else {
        value = object[name];
      }
      if (typeof value === 'undefined') {
        if (args.length) {
          throw new Error("Try to get ".concat(args[0], " from undefined"));
        }
        return value;
      } else {
        var context;
        if (args.length - 1 < len) {
          context = object;
        }
        value = patch_value(value, context);
      }
      object = value;
    }
    return value;
  }, "(. obj . args)\n    (get obj . args)\n\n    This function uses an object as a base and keeps using arguments to get the\n    property of JavaScript object. Arguments need to be a strings.\n    e.g. `(. console \"log\")` if you use any function inside LIPS it\n    will be weakly bound (can be rebound), so you can call this log function\n    without problem unlike in JavaScript when you use\n    `var log = console.log`.\n    `get` is an alias because . doesn't work everywhere, e.g. you can't\n    pass it as an argument.");
  // -------------------------------------------------------------------------
  // Function gets internal protected data
  // -------------------------------------------------------------------------
  function internal(env, name) {
    var internal_env = interaction(env, '**internal-env**');
    return internal_env.get(name);
  }
  // -------------------------------------------------------------------------
  // Get variable from interaction environment
  // -------------------------------------------------------------------------
  function interaction(env, name) {
    var interaction_env = env.get('**interaction-environment**');
    return interaction_env.get(name);
  }
  // -------------------------------------------------------------------------
  var internal_env = new Environment({
    stdout: new BufferedOutputPort(function () {
      var _console;
      (_console = console).log.apply(_console, arguments);
    }),
    // ------------------------------------------------------------------
    stderr: new BufferedOutputPort(function () {
      var _console2;
      (_console2 = console).error.apply(_console2, arguments);
    }),
    'command-line': [],
    // ------------------------------------------------------------------
    stdin: InputPort(function () {
      return Promise.resolve(prompt(''));
    }),
    // those will be compiled by babel regex plugin
    'letter-unicode-regex': /(?:[A-Za-z\xAA\xB5\xBA\xC0-\xD6\xD8-\xF6\xF8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u037F\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u052F\u0531-\u0556\u0559\u0560-\u0588\u05D0-\u05EA\u05EF-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u0860-\u086A\u0870-\u0887\u0889-\u088E\u08A0-\u08C9\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0980\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u09FC\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0AF9\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C39\u0C3D\u0C58-\u0C5A\u0C5D\u0C60\u0C61\u0C80\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D04-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D54-\u0D56\u0D5F-\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E86-\u0E8A\u0E8C-\u0EA3\u0EA5\u0EA7-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F5\u13F8-\u13FD\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u16F1-\u16F8\u1700-\u1711\u171F-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1878\u1880-\u1884\u1887-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191E\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19B0-\u19C9\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4C\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1C80-\u1C88\u1C90-\u1CBA\u1CBD-\u1CBF\u1CE9-\u1CEC\u1CEE-\u1CF3\u1CF5\u1CF6\u1CFA\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312F\u3131-\u318E\u31A0-\u31BF\u31F0-\u31FF\u3400-\u4DBF\u4E00-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA69D\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA7CA\uA7D0\uA7D1\uA7D3\uA7D5-\uA7D9\uA7F2-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA8FD\uA8FE\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uA9E0-\uA9E4\uA9E6-\uA9EF\uA9FA-\uA9FE\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA7E-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uAB30-\uAB5A\uAB5C-\uAB69\uAB70-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]|\uD800[\uDC00-\uDC0B\uDC0D-\uDC26\uDC28-\uDC3A\uDC3C\uDC3D\uDC3F-\uDC4D\uDC50-\uDC5D\uDC80-\uDCFA\uDE80-\uDE9C\uDEA0-\uDED0\uDF00-\uDF1F\uDF2D-\uDF40\uDF42-\uDF49\uDF50-\uDF75\uDF80-\uDF9D\uDFA0-\uDFC3\uDFC8-\uDFCF]|\uD801[\uDC00-\uDC9D\uDCB0-\uDCD3\uDCD8-\uDCFB\uDD00-\uDD27\uDD30-\uDD63\uDD70-\uDD7A\uDD7C-\uDD8A\uDD8C-\uDD92\uDD94\uDD95\uDD97-\uDDA1\uDDA3-\uDDB1\uDDB3-\uDDB9\uDDBB\uDDBC\uDE00-\uDF36\uDF40-\uDF55\uDF60-\uDF67\uDF80-\uDF85\uDF87-\uDFB0\uDFB2-\uDFBA]|\uD802[\uDC00-\uDC05\uDC08\uDC0A-\uDC35\uDC37\uDC38\uDC3C\uDC3F-\uDC55\uDC60-\uDC76\uDC80-\uDC9E\uDCE0-\uDCF2\uDCF4\uDCF5\uDD00-\uDD15\uDD20-\uDD39\uDD80-\uDDB7\uDDBE\uDDBF\uDE00\uDE10-\uDE13\uDE15-\uDE17\uDE19-\uDE35\uDE60-\uDE7C\uDE80-\uDE9C\uDEC0-\uDEC7\uDEC9-\uDEE4\uDF00-\uDF35\uDF40-\uDF55\uDF60-\uDF72\uDF80-\uDF91]|\uD803[\uDC00-\uDC48\uDC80-\uDCB2\uDCC0-\uDCF2\uDD00-\uDD23\uDE80-\uDEA9\uDEB0\uDEB1\uDF00-\uDF1C\uDF27\uDF30-\uDF45\uDF70-\uDF81\uDFB0-\uDFC4\uDFE0-\uDFF6]|\uD804[\uDC03-\uDC37\uDC71\uDC72\uDC75\uDC83-\uDCAF\uDCD0-\uDCE8\uDD03-\uDD26\uDD44\uDD47\uDD50-\uDD72\uDD76\uDD83-\uDDB2\uDDC1-\uDDC4\uDDDA\uDDDC\uDE00-\uDE11\uDE13-\uDE2B\uDE80-\uDE86\uDE88\uDE8A-\uDE8D\uDE8F-\uDE9D\uDE9F-\uDEA8\uDEB0-\uDEDE\uDF05-\uDF0C\uDF0F\uDF10\uDF13-\uDF28\uDF2A-\uDF30\uDF32\uDF33\uDF35-\uDF39\uDF3D\uDF50\uDF5D-\uDF61]|\uD805[\uDC00-\uDC34\uDC47-\uDC4A\uDC5F-\uDC61\uDC80-\uDCAF\uDCC4\uDCC5\uDCC7\uDD80-\uDDAE\uDDD8-\uDDDB\uDE00-\uDE2F\uDE44\uDE80-\uDEAA\uDEB8\uDF00-\uDF1A\uDF40-\uDF46]|\uD806[\uDC00-\uDC2B\uDCA0-\uDCDF\uDCFF-\uDD06\uDD09\uDD0C-\uDD13\uDD15\uDD16\uDD18-\uDD2F\uDD3F\uDD41\uDDA0-\uDDA7\uDDAA-\uDDD0\uDDE1\uDDE3\uDE00\uDE0B-\uDE32\uDE3A\uDE50\uDE5C-\uDE89\uDE9D\uDEB0-\uDEF8]|\uD807[\uDC00-\uDC08\uDC0A-\uDC2E\uDC40\uDC72-\uDC8F\uDD00-\uDD06\uDD08\uDD09\uDD0B-\uDD30\uDD46\uDD60-\uDD65\uDD67\uDD68\uDD6A-\uDD89\uDD98\uDEE0-\uDEF2\uDFB0]|\uD808[\uDC00-\uDF99]|\uD809[\uDC80-\uDD43]|\uD80B[\uDF90-\uDFF0]|[\uD80C\uD81C-\uD820\uD822\uD840-\uD868\uD86A-\uD86C\uD86F-\uD872\uD874-\uD879\uD880-\uD883][\uDC00-\uDFFF]|\uD80D[\uDC00-\uDC2E]|\uD811[\uDC00-\uDE46]|\uD81A[\uDC00-\uDE38\uDE40-\uDE5E\uDE70-\uDEBE\uDED0-\uDEED\uDF00-\uDF2F\uDF40-\uDF43\uDF63-\uDF77\uDF7D-\uDF8F]|\uD81B[\uDE40-\uDE7F\uDF00-\uDF4A\uDF50\uDF93-\uDF9F\uDFE0\uDFE1\uDFE3]|\uD821[\uDC00-\uDFF7]|\uD823[\uDC00-\uDCD5\uDD00-\uDD08]|\uD82B[\uDFF0-\uDFF3\uDFF5-\uDFFB\uDFFD\uDFFE]|\uD82C[\uDC00-\uDD22\uDD50-\uDD52\uDD64-\uDD67\uDD70-\uDEFB]|\uD82F[\uDC00-\uDC6A\uDC70-\uDC7C\uDC80-\uDC88\uDC90-\uDC99]|\uD835[\uDC00-\uDC54\uDC56-\uDC9C\uDC9E\uDC9F\uDCA2\uDCA5\uDCA6\uDCA9-\uDCAC\uDCAE-\uDCB9\uDCBB\uDCBD-\uDCC3\uDCC5-\uDD05\uDD07-\uDD0A\uDD0D-\uDD14\uDD16-\uDD1C\uDD1E-\uDD39\uDD3B-\uDD3E\uDD40-\uDD44\uDD46\uDD4A-\uDD50\uDD52-\uDEA5\uDEA8-\uDEC0\uDEC2-\uDEDA\uDEDC-\uDEFA\uDEFC-\uDF14\uDF16-\uDF34\uDF36-\uDF4E\uDF50-\uDF6E\uDF70-\uDF88\uDF8A-\uDFA8\uDFAA-\uDFC2\uDFC4-\uDFCB]|\uD837[\uDF00-\uDF1E]|\uD838[\uDD00-\uDD2C\uDD37-\uDD3D\uDD4E\uDE90-\uDEAD\uDEC0-\uDEEB]|\uD839[\uDFE0-\uDFE6\uDFE8-\uDFEB\uDFED\uDFEE\uDFF0-\uDFFE]|\uD83A[\uDC00-\uDCC4\uDD00-\uDD43\uDD4B]|\uD83B[\uDE00-\uDE03\uDE05-\uDE1F\uDE21\uDE22\uDE24\uDE27\uDE29-\uDE32\uDE34-\uDE37\uDE39\uDE3B\uDE42\uDE47\uDE49\uDE4B\uDE4D-\uDE4F\uDE51\uDE52\uDE54\uDE57\uDE59\uDE5B\uDE5D\uDE5F\uDE61\uDE62\uDE64\uDE67-\uDE6A\uDE6C-\uDE72\uDE74-\uDE77\uDE79-\uDE7C\uDE7E\uDE80-\uDE89\uDE8B-\uDE9B\uDEA1-\uDEA3\uDEA5-\uDEA9\uDEAB-\uDEBB]|\uD869[\uDC00-\uDEDF\uDF00-\uDFFF]|\uD86D[\uDC00-\uDF38\uDF40-\uDFFF]|\uD86E[\uDC00-\uDC1D\uDC20-\uDFFF]|\uD873[\uDC00-\uDEA1\uDEB0-\uDFFF]|\uD87A[\uDC00-\uDFE0]|\uD87E[\uDC00-\uDE1D]|\uD884[\uDC00-\uDF4A])/,
    'numeral-unicode-regex': /(?:[0-9\xB2\xB3\xB9\xBC-\xBE\u0660-\u0669\u06F0-\u06F9\u07C0-\u07C9\u0966-\u096F\u09E6-\u09EF\u09F4-\u09F9\u0A66-\u0A6F\u0AE6-\u0AEF\u0B66-\u0B6F\u0B72-\u0B77\u0BE6-\u0BF2\u0C66-\u0C6F\u0C78-\u0C7E\u0CE6-\u0CEF\u0D58-\u0D5E\u0D66-\u0D78\u0DE6-\u0DEF\u0E50-\u0E59\u0ED0-\u0ED9\u0F20-\u0F33\u1040-\u1049\u1090-\u1099\u1369-\u137C\u16EE-\u16F0\u17E0-\u17E9\u17F0-\u17F9\u1810-\u1819\u1946-\u194F\u19D0-\u19DA\u1A80-\u1A89\u1A90-\u1A99\u1B50-\u1B59\u1BB0-\u1BB9\u1C40-\u1C49\u1C50-\u1C59\u2070\u2074-\u2079\u2080-\u2089\u2150-\u2182\u2185-\u2189\u2460-\u249B\u24EA-\u24FF\u2776-\u2793\u2CFD\u3007\u3021-\u3029\u3038-\u303A\u3192-\u3195\u3220-\u3229\u3248-\u324F\u3251-\u325F\u3280-\u3289\u32B1-\u32BF\uA620-\uA629\uA6E6-\uA6EF\uA830-\uA835\uA8D0-\uA8D9\uA900-\uA909\uA9D0-\uA9D9\uA9F0-\uA9F9\uAA50-\uAA59\uABF0-\uABF9\uFF10-\uFF19]|\uD800[\uDD07-\uDD33\uDD40-\uDD78\uDD8A\uDD8B\uDEE1-\uDEFB\uDF20-\uDF23\uDF41\uDF4A\uDFD1-\uDFD5]|\uD801[\uDCA0-\uDCA9]|\uD802[\uDC58-\uDC5F\uDC79-\uDC7F\uDCA7-\uDCAF\uDCFB-\uDCFF\uDD16-\uDD1B\uDDBC\uDDBD\uDDC0-\uDDCF\uDDD2-\uDDFF\uDE40-\uDE48\uDE7D\uDE7E\uDE9D-\uDE9F\uDEEB-\uDEEF\uDF58-\uDF5F\uDF78-\uDF7F\uDFA9-\uDFAF]|\uD803[\uDCFA-\uDCFF\uDD30-\uDD39\uDE60-\uDE7E\uDF1D-\uDF26\uDF51-\uDF54\uDFC5-\uDFCB]|\uD804[\uDC52-\uDC6F\uDCF0-\uDCF9\uDD36-\uDD3F\uDDD0-\uDDD9\uDDE1-\uDDF4\uDEF0-\uDEF9]|\uD805[\uDC50-\uDC59\uDCD0-\uDCD9\uDE50-\uDE59\uDEC0-\uDEC9\uDF30-\uDF3B]|\uD806[\uDCE0-\uDCF2\uDD50-\uDD59]|\uD807[\uDC50-\uDC6C\uDD50-\uDD59\uDDA0-\uDDA9\uDFC0-\uDFD4]|\uD809[\uDC00-\uDC6E]|\uD81A[\uDE60-\uDE69\uDEC0-\uDEC9\uDF50-\uDF59\uDF5B-\uDF61]|\uD81B[\uDE80-\uDE96]|\uD834[\uDEE0-\uDEF3\uDF60-\uDF78]|\uD835[\uDFCE-\uDFFF]|\uD838[\uDD40-\uDD49\uDEF0-\uDEF9]|\uD83A[\uDCC7-\uDCCF\uDD50-\uDD59]|\uD83B[\uDC71-\uDCAB\uDCAD-\uDCAF\uDCB1-\uDCB4\uDD01-\uDD2D\uDD2F-\uDD3D]|\uD83C[\uDD00-\uDD0C]|\uD83E[\uDFF0-\uDFF9])/,
    'space-unicode-regex': /[\t-\r \xA0\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000\uFEFF]/
  }, undefined, 'internal');
  // -------------------------------------------------------------------------
  var nan = LNumber(NaN);
  var constants = {
    'true': true,
    'false': false,
    '#true': true,
    '#false': false,
    '#t': true,
    '#f': false,
    nil: _nil,
    'undefined': undefined,
    'null': null,
    'NaN': nan,
    '+nan.0': nan,
    '-nan.0': nan
  };
  // -------------------------------------------------------------------------
  var global_env = new Environment({
    eof: eof,
    undefined: undefined,
    // undefined as parser constant breaks most of the unit tests
    // ---------------------------------------------------------------------
    'peek-char': doc('peek-char', function () {
      var port = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;
      if (port === null) {
        port = internal(this, 'stdin');
      }
      typecheck_text_port('peek-char', port, 'input-port');
      return port.peek_char();
    }, "(peek-char port)\n\n        This function reads and returns a character from the string\n        port, or, if there is no more data in the string port, it\n        returns an EOF."),
    // ------------------------------------------------------------------
    'read-line': doc('read-line', function () {
      var port = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;
      if (port === null) {
        port = internal(this, 'stdin');
      }
      typecheck_text_port('read-line', port, 'input-port');
      return port.read_line();
    }, "(read-line port)\n\n        This function reads and returns the next line from the input\n        port."),
    // ------------------------------------------------------------------
    'read-char': doc('read-char', function () {
      var port = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : null;
      if (port === null) {
        port = internal(this, 'stdin');
      }
      typecheck_text_port('read-char', port, 'input-port');
      return port.read_char();
    }, "(read-char port)\n\n        This function reads and returns the next character from the\n        input port."),
    // ------------------------------------------------------------------
    read: doc('read', /*#__PURE__*/function () {
      var _read2 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee16() {
        var arg,
          env,
          _iteratorAbruptCompletion2,
          _didIteratorError2,
          _iteratorError2,
          _iterator2,
          _step2,
          value,
          port,
          _args18 = arguments;
        return _regeneratorRuntime.wrap(function _callee16$(_context16) {
          while (1) {
            switch (_context16.prev = _context16.next) {
              case 0:
                arg = _args18.length > 0 && _args18[0] !== undefined ? _args18[0] : null;
                env = this.env;
                if (!LString.isString(arg)) {
                  _context16.next = 31;
                  break;
                }
                _iteratorAbruptCompletion2 = false;
                _didIteratorError2 = false;
                _context16.prev = 5;
                _iterator2 = _asyncIterator(parse(arg, env));
              case 7:
                _context16.next = 9;
                return _iterator2.next();
              case 9:
                if (!(_iteratorAbruptCompletion2 = !(_step2 = _context16.sent).done)) {
                  _context16.next = 15;
                  break;
                }
                value = _step2.value;
                return _context16.abrupt("return", value);
              case 12:
                _iteratorAbruptCompletion2 = false;
                _context16.next = 7;
                break;
              case 15:
                _context16.next = 21;
                break;
              case 17:
                _context16.prev = 17;
                _context16.t0 = _context16["catch"](5);
                _didIteratorError2 = true;
                _iteratorError2 = _context16.t0;
              case 21:
                _context16.prev = 21;
                _context16.prev = 22;
                if (!(_iteratorAbruptCompletion2 && _iterator2["return"] != null)) {
                  _context16.next = 26;
                  break;
                }
                _context16.next = 26;
                return _iterator2["return"]();
              case 26:
                _context16.prev = 26;
                if (!_didIteratorError2) {
                  _context16.next = 29;
                  break;
                }
                throw _iteratorError2;
              case 29:
                return _context16.finish(26);
              case 30:
                return _context16.finish(21);
              case 31:
                if (arg === null) {
                  port = internal(env, 'stdin');
                } else {
                  port = arg;
                }
                typecheck_text_port('read', port, 'input-port');
                return _context16.abrupt("return", port.read.call(env));
              case 34:
              case "end":
                return _context16.stop();
            }
          }
        }, _callee16, this, [[5, 17, 21, 31], [22,, 26, 30]]);
      }));
      function read() {
        return _read2.apply(this, arguments);
      }
      return read;
    }(), "(read [string])\n\n        This function, if used with a string, will parse it and\n        return the LIPS code, if there is any. If called with a\n        port, it will parse the next item from the port. If called\n        without an input, it will read a string from standard input\n        (using the browser's prompt or a user defined input method)\n        and calls itself with that string. This function can be used\n        together with `eval` to evaluate code from a string."),
    // ------------------------------------------------------------------
    pprint: doc('pprint', function pprint(arg) {
      if (arg instanceof Pair) {
        arg = new lips.Formatter(arg.toString(true))["break"]().format();
        global_env.get('display').call(global_env, arg);
      } else {
        global_env.get('write').call(global_env, arg);
      }
      global_env.get('newline').call(global_env);
    }, "(pprint expression)\n\n        This function will pretty print its input to stdout. If it is called\n        with a non-list, it will just call the print function on its\n        input."),
    // ------------------------------------------------------------------
    print: doc('print', function print() {
      var display = global_env.get('display');
      var newline = global_env.get('newline');
      var use_dynamic = this.use_dynamic;
      var env = global_env;
      var dynamic_env = global_env;
      for (var _len19 = arguments.length, args = new Array(_len19), _key19 = 0; _key19 < _len19; _key19++) {
        args[_key19] = arguments[_key19];
      }
      args.forEach(function (arg) {
        call_function(display, [arg], {
          env: env,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic
        });
        call_function(newline, [], {
          env: env,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic
        });
      });
    }, "(print . args)\n\n        This function converts each input into a string and prints\n        the result to the standard output (by default it's the\n        console but it can be defined in user code). This function\n        calls `(newline)` after printing each input."),
    // ------------------------------------------------------------------
    format: doc('format', function format(str) {
      for (var _len20 = arguments.length, args = new Array(_len20 > 1 ? _len20 - 1 : 0), _key20 = 1; _key20 < _len20; _key20++) {
        args[_key20 - 1] = arguments[_key20];
      }
      typecheck('format', str, 'string');
      var re = /(~[as%~])/g;
      var m = str.match(/(~[as])/g);
      if (m && m.length > args.length) {
        throw new Error('Not enough arguments');
      }
      var i = 0;
      var repr = global_env.get('repr');
      str = str.replace(re, function (x) {
        var chr = x[1];
        if (chr === '~') {
          return '~';
        } else if (chr === '%') {
          return '\n';
        } else {
          var arg = args[i++];
          if (chr === 'a') {
            return repr(arg);
          } else {
            return repr(arg, true);
          }
        }
      });
      m = str.match(/~([\S])/);
      if (m) {
        throw new Error("format: Unrecognized escape sequence ".concat(m[1]));
      }
      return str;
    }, "(format string n1 n2 ...)\n\n        This function accepts a string template and replaces any\n        escape sequences in its inputs:\n\n        * ~a value as if printed with `display`\n        * ~s value as if printed with `write`\n        * ~% newline character\n        * ~~ literal tilde '~'\n\n        If there are missing inputs or other escape characters it\n        will error."),
    // ------------------------------------------------------------------
    display: doc('display', function display(arg) {
      var port = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;
      if (port === null) {
        port = internal(this, 'stdout');
      } else {
        typecheck('display', port, 'output-port');
      }
      var value = arg;
      if (!(port instanceof OutputBinaryFilePort)) {
        value = global_env.get('repr')(arg);
      }
      port.write.call(global_env, value);
    }, "(display string [port])\n\n        This function outputs the string to the standard output or\n        the port if given. No newline."),
    // ------------------------------------------------------------------
    'display-error': doc('display-error', function error() {
      var port = internal(this, 'stderr');
      var repr = global_env.get('repr');
      for (var _len21 = arguments.length, args = new Array(_len21), _key21 = 0; _key21 < _len21; _key21++) {
        args[_key21] = arguments[_key21];
      }
      var value = args.map(repr).join(' ');
      port.write.call(global_env, value);
      global_env.get('newline')(port);
    }, "(display-error . args)\n\n        Display an error message on stderr."),
    // ------------------------------------------------------------------
    '%same-functions': doc('%same-functions', function (a, b) {
      if (!is_function(a)) {
        return false;
      }
      if (!is_function(b)) {
        return false;
      }
      return unbind(a) === unbind(b);
    }, "(%same-functions a b)\n\n        A helper function that checks if the two input functions are\n        the same."),
    // ------------------------------------------------------------------
    help: doc(new Macro('help', function (code, _ref28) {
      var dynamic_env = _ref28.dynamic_env,
        use_dynamic = _ref28.use_dynamic,
        error = _ref28.error;
      var symbol;
      if (code.car instanceof LSymbol) {
        symbol = code.car;
      } else if (code.car instanceof Pair && code.car.car instanceof LSymbol) {
        symbol = code.car.car;
      } else {
        var env = this;
        dynamic_env = this;
        var ret = _evaluate(code.car, {
          env: env,
          error: error,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic
        });
        if (ret && ret.__doc__) {
          return ret.__doc__;
        }
        return;
      }
      var __doc__;
      var value = this.get(symbol);
      __doc__ = value && value.__doc__;
      if (__doc__) {
        return __doc__;
      }
      var ref = this.ref(symbol);
      if (ref) {
        __doc__ = ref.doc(symbol);
        if (__doc__) {
          return __doc__;
        }
      }
    }), "(help object)\n\n         This macro returns documentation for a function or macro.\n         You can save the function or macro in a variable and use it\n         here. But getting help for a variable requires passing the\n         variable in a `quote`."),
    // ------------------------------------------------------------------
    cons: doc('cons', function cons(car, cdr) {
      return new Pair(car, cdr);
    }, "(cons left right)\n\n        This function returns a new list with the first appended\n        before the second. If the second is not a list cons will\n        return a dotted pair."),
    // ------------------------------------------------------------------
    car: doc('car', function car(list) {
      typecheck('car', list, 'pair');
      return list.car;
    }, "(car pair)\n\n        This function returns the car (item 1) of the list."),
    // ------------------------------------------------------------------
    cdr: doc('cdr', function cdr(list) {
      typecheck('cdr', list, 'pair');
      return list.cdr;
    }, "(cdr pair)\n\n        This function returns the cdr (all but first) of the list."),
    // ------------------------------------------------------------------
    'set!': doc(new Macro('set!', function (code) {
      var _this22 = this;
      var _ref29 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        use_dynamic = _ref29.use_dynamic,
        rest = _objectWithoutProperties(_ref29, _excluded4);
      var dynamic_env = this;
      var env = this;
      var ref;
      var eval_args = _objectSpread(_objectSpread({}, rest), {}, {
        env: this,
        dynamic_env: dynamic_env,
        use_dynamic: use_dynamic
      });
      var value = _evaluate(code.cdr.car, eval_args);
      value = resolve_promises(value);
      function set(object, key, value) {
        if (is_promise(object)) {
          return object.then(function (key) {
            return set(object, key, value);
          });
        }
        if (is_promise(key)) {
          return key.then(function (key) {
            return set(object, key, value);
          });
        }
        if (is_promise(value)) {
          return value.then(function (value) {
            return set(object, key, value);
          });
        }
        env.get('set-obj!').call(env, object, key, value);
        return value;
      }
      if (code.car instanceof Pair && LSymbol.is(code.car.car, '.')) {
        var second = code.car.cdr.car;
        var third = code.car.cdr.cdr.car;
        var object = _evaluate(second, {
          env: this,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic,
          error: error
        });
        var key = _evaluate(third, {
          env: this,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic,
          error: error
        });
        return set(object, key, value);
      }
      if (!(code.car instanceof LSymbol)) {
        throw new Error('set! first argument need to be a symbol or ' + 'dot accessor that evaluate to object.');
      }
      var symbol = code.car.valueOf();
      ref = this.ref(code.car.__name__);
      // we don't return value because we only care about sync of set value
      // when value is a promise
      return unpromise(value, function (value) {
        if (!ref) {
          // case (set! fn.toString (lambda () "xxx"))
          var parts = symbol.split('.');
          if (parts.length > 1) {
            var key = parts.pop();
            var name = parts.join('.');
            var obj = _this22.get(name, {
              throwError: false
            });
            if (obj) {
              set(obj, key, value);
              return;
            }
          }
          throw new Error('Unbound variable `' + symbol + '\'');
        }
        ref.set(symbol, value);
      });
    }), "(set! name value)\n\n         Macro that can be used to set the value of the variable or slot (mutate it).\n         set! searches the scope chain until it finds first non empty slot and sets it."),
    // ------------------------------------------------------------------
    'unset!': doc(new Macro('set!', function (code) {
      if (!(code.car instanceof LSymbol)) {
        throw new Error('unset! first argument need to be a symbol or ' + 'dot accessor that evaluate to object.');
      }
      var symbol = code.car;
      var ref = this.ref(symbol);
      if (ref) {
        delete ref.__env__[symbol.__name__];
      }
    }), "(unset! name)\n\n         Function to delete the specified name from environment.\n         Trying to access the name afterwards will error."),
    // ------------------------------------------------------------------
    'set-car!': doc('set-car!', function (slot, value) {
      typecheck('set-car!', slot, 'pair');
      slot.car = value;
    }, "(set-car! obj value)\n\n         Function that sets the car (first item) of the list/pair to specified value.\n         The old value is lost."),
    // ------------------------------------------------------------------
    'set-cdr!': doc('set-cdr!', function (slot, value) {
      typecheck('set-cdr!', slot, 'pair');
      slot.cdr = value;
    }, "(set-cdr! obj value)\n\n         Function that sets the cdr (tail) of the list/pair to specified value.\n         It will destroy the list. The old tail is lost."),
    // ------------------------------------------------------------------
    'empty?': doc('empty?', function (x) {
      return typeof x === 'undefined' || x === _nil;
    }, "(empty? object)\n\n         Function that returns #t if value is nil (an empty list) or undefined."),
    // ------------------------------------------------------------------
    gensym: doc('gensym', gensym, "(gensym)\n\n         Generates a unique symbol that is not bound anywhere,\n         to use with macros as meta name."),
    // ------------------------------------------------------------------
    load: doc('load', function load(file, env) {
      typecheck('load', file, 'string');
      var g_env = this;
      if (g_env.__name__ === '__frame__') {
        g_env = g_env.__parent__;
      }
      if (!(env instanceof Environment)) {
        if (g_env === global_env) {
          // this is used for let-env + load
          // this may be obsolete when there is env arg
          env = g_env;
        } else {
          env = this.get('**interaction-environment**');
        }
      }
      // TODO: move **module-path** to internal env
      var PATH = '**module-path**';
      var module_path = global_env.get(PATH, {
        throwError: false
      });
      file = file.valueOf();
      if (!file.match(/.[^.]+$/)) {
        file += '.scm';
      }
      var IS_BIN = file.match(/\.xcb$/);
      function run(code) {
        if (IS_BIN) {
          code = unserialize_bin(code);
        } else {
          if (type(code) === 'buffer') {
            code = code.toString();
          }
          code = code.replace(/^#!.*/, '');
          if (code.match(/^\{/)) {
            code = unserialize(code);
          }
        }
        return exec(code, {
          env: env
        });
      }
      function fetch(file) {
        return root.fetch(file).then(function (res) {
          return IS_BIN ? res.arrayBuffer() : res.text();
        }).then(function (code) {
          if (IS_BIN) {
            code = new Uint8Array(code);
          }
          return code;
        });
      }
      if (is_node()) {
        return new Promise( /*#__PURE__*/function () {
          var _ref30 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee17(resolve, reject) {
            var path, cmd, _args19;
            return _regeneratorRuntime.wrap(function _callee17$(_context17) {
              while (1) {
                switch (_context17.prev = _context17.next) {
                  case 0:
                    path = nodeRequire('path');
                    if (!module_path) {
                      _context17.next = 6;
                      break;
                    }
                    module_path = module_path.valueOf();
                    file = path.join(module_path, file);
                    _context17.next = 12;
                    break;
                  case 6:
                    cmd = g_env.get('command-line', {
                      throwError: false
                    });
                    if (!cmd) {
                      _context17.next = 11;
                      break;
                    }
                    _context17.next = 10;
                    return cmd();
                  case 10:
                    _args19 = _context17.sent;
                  case 11:
                    if (_args19 && _args19 !== _nil) {
                      process.cwd();
                      file = path.join(path.dirname(_args19.car.valueOf()), file);
                    }
                  case 12:
                    global_env.set(PATH, path.dirname(file));
                    nodeRequire('fs').readFile(file, function (err, data) {
                      if (err) {
                        reject(err);
                        global_env.set(PATH, module_path);
                      } else {
                        try {
                          run(data).then(function () {
                            resolve();
                            global_env.set(PATH, module_path);
                          })["catch"](reject);
                        } catch (e) {
                          reject(e);
                        }
                      }
                    });
                  case 14:
                  case "end":
                    return _context17.stop();
                }
              }
            }, _callee17);
          }));
          return function (_x13, _x14) {
            return _ref30.apply(this, arguments);
          };
        }());
      }
      if (module_path) {
        module_path = module_path.valueOf();
        file = module_path + '/' + file.replace(/^\.?\/?/, '');
      }
      return fetch(file).then(function (code) {
        global_env.set(PATH, file.replace(/\/[^/]*$/, ''));
        return run(code);
      }).then(function () {})["finally"](function () {
        global_env.set(PATH, module_path);
      });
    }, "(load filename)\n        (load filename environment)\n\n        Fetches the file (from disk or network) and evaluates its content as LIPS code.\n        If the second argument is provided and it's an environment the evaluation\n        will happen in that environment."),
    // ------------------------------------------------------------------
    'do': doc(new Macro('do', /*#__PURE__*/function () {
      var _ref31 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee19(code, _ref32) {
        var use_dynamic, error, self, dynamic_env, scope, vars, test, body, eval_args, node, item, _loop3;
        return _regeneratorRuntime.wrap(function _callee19$(_context19) {
          while (1) {
            switch (_context19.prev = _context19.next) {
              case 0:
                use_dynamic = _ref32.use_dynamic, error = _ref32.error;
                self = this;
                dynamic_env = self;
                scope = self.inherit('do');
                vars = code.car;
                test = code.cdr.car;
                body = code.cdr.cdr;
                if (body !== _nil) {
                  body = new Pair(LSymbol('begin'), body);
                }
                eval_args = {
                  env: self,
                  dynamic_env: dynamic_env,
                  use_dynamic: use_dynamic,
                  error: error
                };
                node = vars;
              case 10:
                if (!(node !== _nil)) {
                  _context19.next = 21;
                  break;
                }
                item = node.car;
                _context19.t0 = scope;
                _context19.t1 = item.car;
                _context19.next = 16;
                return _evaluate(item.cdr.car, eval_args);
              case 16:
                _context19.t2 = _context19.sent;
                _context19.t0.set.call(_context19.t0, _context19.t1, _context19.t2);
                node = node.cdr;
                _context19.next = 10;
                break;
              case 21:
                eval_args = {
                  env: scope,
                  dynamic_env: dynamic_env,
                  error: error
                };
                _loop3 = /*#__PURE__*/_regeneratorRuntime.mark(function _callee18() {
                  var node, next, _item, value, symbols;
                  return _regeneratorRuntime.wrap(function _callee18$(_context18) {
                    while (1) {
                      switch (_context18.prev = _context18.next) {
                        case 0:
                          if (!(body !== _nil)) {
                            _context18.next = 3;
                            break;
                          }
                          _context18.next = 3;
                          return lips.evaluate(body, eval_args);
                        case 3:
                          node = vars;
                          next = {};
                        case 5:
                          if (!(node !== _nil)) {
                            _context18.next = 15;
                            break;
                          }
                          _item = node.car;
                          if (!(_item.cdr.cdr !== _nil)) {
                            _context18.next = 12;
                            break;
                          }
                          _context18.next = 10;
                          return _evaluate(_item.cdr.cdr.car, eval_args);
                        case 10:
                          value = _context18.sent;
                          next[_item.car.valueOf()] = value;
                        case 12:
                          node = node.cdr;
                          _context18.next = 5;
                          break;
                        case 15:
                          symbols = Object.getOwnPropertySymbols(next);
                          Object.keys(next).concat(symbols).forEach(function (key) {
                            scope.set(key, next[key]);
                          });
                        case 17:
                        case "end":
                          return _context18.stop();
                      }
                    }
                  }, _callee18);
                });
              case 23:
                _context19.next = 25;
                return _evaluate(test.car, eval_args);
              case 25:
                _context19.t3 = _context19.sent;
                if (!(_context19.t3 === false)) {
                  _context19.next = 30;
                  break;
                }
                return _context19.delegateYield(_loop3(), "t4", 28);
              case 28:
                _context19.next = 23;
                break;
              case 30:
                if (!(test.cdr !== _nil)) {
                  _context19.next = 34;
                  break;
                }
                _context19.next = 33;
                return _evaluate(test.cdr.car, eval_args);
              case 33:
                return _context19.abrupt("return", _context19.sent);
              case 34:
              case "end":
                return _context19.stop();
            }
          }
        }, _callee19, this);
      }));
      return function (_x15, _x16) {
        return _ref31.apply(this, arguments);
      };
    }()), "(do ((<var> <init> <next>)) (test return) . body)\n\n         Iteration macro that evaluates the expression body in scope of the variables.\n         On each loop it changes the variables according to the <next> expression and runs\n         test to check if the loop should continue. If test is a single value, the macro\n         will return undefined. If the test is a pair of expressions the macro will\n         evaluate and return the second expression after the loop exits."),
    // ------------------------------------------------------------------
    'if': doc(new Macro('if', function (code, _ref33) {
      var error = _ref33.error,
        use_dynamic = _ref33.use_dynamic;
      var dynamic_env = this;
      var env = this;
      var eval_args = {
        env: env,
        dynamic_env: dynamic_env,
        use_dynamic: use_dynamic,
        error: error
      };
      var resolve = function resolve(cond) {
        if (cond === false) {
          return _evaluate(code.cdr.cdr.car, eval_args);
        } else {
          return _evaluate(code.cdr.car, eval_args);
        }
      };
      if (code === _nil) {
        throw new Error('too few expressions for `if`');
      }
      var cond = _evaluate(code.car, eval_args);
      return unpromise(cond, resolve);
    }), "(if cond true-expr false-expr)\n\n         Macro that evaluates cond expression and if the value is true, it\n         evaluates and returns true-expression, if not it evaluates and returns\n         false-expression."),
    // ------------------------------------------------------------------
    'let-env': new Macro('let-env', function (code) {
      var options = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
      var dynamic_env = options.dynamic_env,
        use_dynamic = options.use_dynamic,
        error = options.error;
      typecheck('let-env', code, 'pair');
      var ret = _evaluate(code.car, {
        env: this,
        dynamic_env: dynamic_env,
        error: error,
        use_dynamic: use_dynamic
      });
      return unpromise(ret, function (value) {
        typecheck('let-env', value, 'environment');
        return _evaluate(Pair(LSymbol('begin'), code.cdr), {
          env: value,
          dynamic_env: dynamic_env,
          error: error
        });
      });
    }, "(let-env env . body)\n\n        Special macro that evaluates body in context of given environment\n        object."),
    // ------------------------------------------------------------------
    'letrec': doc(let_macro(Symbol["for"]('letrec')), "(letrec ((a value-a) (b value-b) ...) . body)\n\n         Macro that creates a new environment, then evaluates and assigns values to\n         names and then evaluates the body in context of that environment.\n         Values are evaluated sequentially and the next value can access the\n         previous values/names."),
    // ---------------------------------------------------------------------
    'letrec*': doc(let_macro(Symbol["for"]('letrec')), "(letrec* ((a value-a) (b value-b) ...) . body)\n\n         Same as letrec but the order of execution of the binding is guaranteed,\n         so you can use recursive code as well as referencing the previous binding.\n\n         In LIPS both letrec and letrec* behave the same."),
    // ---------------------------------------------------------------------
    'let*': doc(let_macro(Symbol["for"]('let*')), "(let* ((a value-a) (b value-b) ...) . body)\n\n         Macro similar to `let`, but the subsequent bindings after the first\n         are evaluated in the environment including the previous let variables,\n         so you can define one variable, and use it in the next's definition."),
    // ---------------------------------------------------------------------
    'let': doc(let_macro(Symbol["for"]('let')), "(let ((a value-a) (b value-b) ...) . body)\n\n         Macro that creates a new environment, then evaluates and assigns values to names,\n         and then evaluates the body in context of that environment.  Values are evaluated\n         sequentially but you can't access previous values/names when the next are\n         evaluated. You can only get them in the body of the let expression.  (If you want\n         to define multiple variables and use them in each other's definitions, use\n         `let*`.)"),
    // ------------------------------------------------------------------
    'begin*': doc(parallel('begin*', function (values) {
      return values.pop();
    }), "(begin* . body)\n\n         This macro is a parallel version of begin. It evaluates each expression\n         in the body and if it's a promise it will await it in parallel and return\n         the value of the last expression (i.e. it uses Promise.all())."),
    // ------------------------------------------------------------------
    shuffle: doc(function (arg) {
      typecheck('shuffle', arg, ['pair', 'nil', 'array']);
      var random = global_env.get('random');
      if (arg === _nil) {
        return _nil;
      }
      if (Array.isArray(arg)) {
        return shuffle(arg.slice(), random);
      }
      var arr = global_env.get('list->array')(arg);
      arr = shuffle(arr, random);
      return global_env.get('array->list')(arr);
    }, "(shuffle obj)\n\n        Order items in vector or list in random order."),
    // ------------------------------------------------------------------
    begin: doc(new Macro('begin', function (code, options) {
      var eval_args = _objectSpread(_objectSpread({}, options), {}, {
        env: this
      });
      var arr = global_env.get('list->array')(code);
      var result;
      return function loop() {
        if (arr.length) {
          var _code = arr.shift();
          var ret = _evaluate(_code, eval_args);
          return unpromise(ret, function (value) {
            result = value;
            return loop();
          });
        } else {
          return result;
        }
      }();
    }), "(begin . args)\n\n         Macro that runs a list of expressions in order and returns the value\n         of the last one. It can be used in places where you can only have a\n         single expression, like (if)."),
    // ------------------------------------------------------------------
    'ignore': new Macro('ignore', function (code, options) {
      var eval_args = _objectSpread(_objectSpread({}, options), {}, {
        env: this,
        dynamic_env: this
      });
      _evaluate(new Pair(new LSymbol('begin'), code), eval_args);
    }, "(ignore . body)\n\n        Macro that will evaluate the expression and swallow any promises that may\n        be created. It will discard any value that may be returned by the last body\n        expression. The code should have side effects and/or when it's promise\n        it should resolve to undefined."),
    // ------------------------------------------------------------------
    'call/cc': doc(Macro.defmacro('call/cc', function (code) {
      var eval_args = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};
      var args = _objectSpread({
        env: this
      }, eval_args);
      return unpromise(_evaluate(code.car, args), function (result) {
        if (is_function(result)) {
          return result(new Continuation(null));
        }
      });
    }), "(call/cc proc)\n\n         Call-with-current-continuation.\n\n         NOT SUPPORTED BY LIPS RIGHT NOW"),
    // ------------------------------------------------------------------
    parameterize: doc(new Macro('parameterize', function (code, options) {
      var dynamic_env = options.dynamic_env;
      var env = dynamic_env.inherit('parameterize').new_frame(null, {});
      var eval_args = _objectSpread(_objectSpread({}, options), {}, {
        env: this
      });
      var params = code.car;
      if (!is_pair(params)) {
        var t = type(params);
        throw new Error("Invalid syntax for parameterize expecting pair got ".concat(t));
      }
      function next() {
        var body = new Pair(new LSymbol('begin'), code.cdr);
        return _evaluate(body, _objectSpread(_objectSpread({}, eval_args), {}, {
          dynamic_env: env
        }));
      }
      return function loop() {
        var pair = params.car;
        var name = pair.car.valueOf();
        return unpromise(_evaluate(pair.cdr.car, eval_args), function (value) {
          var param = dynamic_env.get(name, {
            throwError: false
          });
          if (!is_parameter(param)) {
            throw new Error("Unknown parameter ".concat(name));
          }
          env.set(name, param.inherit(value));
          if (!is_null(params.cdr)) {
            params = params.cdr;
            return loop();
          } else {
            return next();
          }
        });
      }();
    }), "(parameterize ((name value) ...)\n\n         Macro that change the dynamic variable created by make-parameter."),
    // ------------------------------------------------------------------
    'make-parameter': doc(new Macro('make-parameter', function (code, eval_args) {
      eval_args.dynamic_env;
      var init = _evaluate(code.car, eval_args);
      var fn;
      if (code.cdr.car instanceof Pair) {
        fn = _evaluate(code.cdr.car, eval_args);
      }
      return new Parameter(init, fn);
    }), "(make-parameter init converter)\n\n    Function creates new dynamic variable that can be custimized with parameterize\n    macro. The value should be assigned to a variable e.g.:\n\n    (define radix (make-parameter 10))\n\n    The result value is a procedure that return the value of dynamic variable."),
    // ------------------------------------------------------------------
    define: doc(Macro.defmacro('define', function (code, eval_args) {
      var env = this;
      if (code.car instanceof Pair && code.car.car instanceof LSymbol) {
        var new_code = new Pair(new LSymbol("define"), new Pair(code.car.car, new Pair(new Pair(new LSymbol("lambda"), new Pair(code.car.cdr, code.cdr)))));
        return new_code;
      } else if (eval_args.macro_expand) {
        // prevent evaluation in macroexpand
        return;
      }
      eval_args.dynamic_env = this;
      eval_args.env = env;
      var value = code.cdr.car;
      var new_expr;
      if (value instanceof Pair) {
        value = _evaluate(value, eval_args);
        new_expr = true;
      } else if (value instanceof LSymbol) {
        value = env.get(value);
      }
      typecheck('define', code.car, 'symbol');
      return unpromise(value, function (value) {
        if (env.__name__ === Syntax.__merge_env__) {
          env = env.__parent__;
        }
        if (new_expr && (is_function(value) && is_lambda(value) || value instanceof Syntax || is_parameter(value))) {
          value.__name__ = code.car.valueOf();
          if (value.__name__ instanceof LString) {
            value.__name__ = value.__name__.valueOf();
          }
        }
        var __doc__;
        if (code.cdr.cdr instanceof Pair && LString.isString(code.cdr.cdr.car)) {
          __doc__ = code.cdr.cdr.car.valueOf();
        }
        env.set(code.car, value, __doc__, true);
      });
    }), "(define name expression)\n         (define name expression \"doc string\")\n         (define (function-name . args) . body)\n\n         Macro for defining values. It can be used to define variables,\n         or functions. If the first argument is list it will create a function\n         with name being first element of the list. This form expands to\n         `(define function-name (lambda args body))`"),
    // ------------------------------------------------------------------
    'set-obj!': doc('set-obj!', function (obj, key, value) {
      var options = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
      var obj_type = _typeof(obj);
      if (is_null(obj) || obj_type !== 'object' && obj_type !== 'function') {
        var msg = typeErrorMessage('set-obj!', type(obj), ['object', 'function']);
        throw new Error(msg);
      }
      typecheck('set-obj!', key, ['string', 'symbol', 'number']);
      obj = unbind(obj);
      key = key.valueOf();
      if (arguments.length === 2) {
        delete obj[key];
      } else if (is_prototype(obj) && is_function(value)) {
        obj[key] = unbind(value);
        obj[key][__prototype__] = true;
      } else if (is_function(value) || is_native(value) || value === _nil) {
        obj[key] = value;
      } else {
        obj[key] = value && !is_prototype(value) ? value.valueOf() : value;
      }
      if (props) {
        var _value4 = obj[key];
        Object.defineProperty(obj, key, _objectSpread(_objectSpread({}, options), {}, {
          value: _value4
        }));
      }
    }, "(set-obj! obj key value)\n        (set-obj! obj key value props)\n\n        Function set a property of a JavaScript object. props should be a vector of pairs,\n        passed to Object.defineProperty."),
    // ------------------------------------------------------------------
    'null-environment': doc('null-environment', function () {
      return global_env.inherit('null');
    }, "(null-environment)\n\n        Returns a clean environment with only the standard library."),
    // ------------------------------------------------------------------
    'values': doc('values', function values() {
      for (var _len22 = arguments.length, args = new Array(_len22), _key22 = 0; _key22 < _len22; _key22++) {
        args[_key22] = arguments[_key22];
      }
      return Values(args);
    }, "(values a1 a2 ...)\n\n        If called with more then one element it will create a special\n        Values object that can be used in the call-with-values function."),
    // ------------------------------------------------------------------
    'call-with-values': doc('call-with-values', function (producer, consumer) {
      typecheck('call-with-values', producer, 'function', 1);
      typecheck('call-with-values', consumer, 'function', 2);
      var maybe = producer.apply(this);
      if (maybe instanceof Values) {
        return consumer.apply(this, maybe.valueOf());
      }
      return consumer.call(this, maybe);
    }, "(call-with-values producer consumer)\n\n        Calls the producer procedure with no arguments, then calls the\n        consumer procedure with the returned value as an argument -- unless\n        the returned value is a special Values object created by (values), if it is\n        the values are unpacked and the consumer is called with multiple arguments."),
    // ------------------------------------------------------------------
    'current-environment': doc('current-environment', function () {
      if (this.__name__ === '__frame__') {
        return this.__parent__;
      }
      return this;
    }, "(current-environment)\n\n        Function that returns the current environment (they're first-class objects!)"),
    // ------------------------------------------------------------------
    'parent.frame': doc('parent.frame', function () {
      return user_env;
    }, "(parent.frame)\n\n        Returns the parent environment if called from inside a function.\n        If no parent frame can be found it returns nil."),
    // ------------------------------------------------------------------
    'eval': doc('eval', function (code, env) {
      var _this23 = this;
      env = env || this.get('current-environment').call(this);
      return _evaluate(code, {
        env: env,
        dynamic_env: env,
        error: function error(e) {
          var error = global_env.get('display-error');
          error.call(_this23, e.message);
          if (e.code) {
            var stack = e.code.map(function (line, i) {
              return "[".concat(i + 1, "]: ").concat(line);
            }).join('\n');
            error.call(_this23, stack);
          }
        }
      });
    }, "(eval expr)\n        (eval expr environment)\n\n        Function that evaluates LIPS Scheme code. If the second argument is provided\n        it will be the environment that the code is evaluated in."),
    // ------------------------------------------------------------------
    lambda: new Macro('lambda', function (code) {
      var _ref34 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        use_dynamic = _ref34.use_dynamic,
        error = _ref34.error;
      var self = this;
      var __doc__;
      if (code.cdr instanceof Pair && LString.isString(code.cdr.car) && code.cdr.cdr !== _nil) {
        __doc__ = code.cdr.car.valueOf();
      }
      function lambda() {
        // lambda got scopes as context in apply
        var _ref35 = is_context(this) ? this : {
            dynamic_env: self
          },
          dynamic_env = _ref35.dynamic_env;
        var env = self.inherit('lambda');
        dynamic_env = dynamic_env.inherit('lambda');
        if (this && !is_context(this)) {
          if (this && !this.__instance__) {
            Object.defineProperty(this, '__instance__', {
              enumerable: false,
              get: function get() {
                return true;
              },
              set: function set() {},
              configurable: false
            });
          }
          env.set('this', this);
        }
        // arguments and arguments.callee inside lambda function
        for (var _len23 = arguments.length, args = new Array(_len23), _key23 = 0; _key23 < _len23; _key23++) {
          args[_key23] = arguments[_key23];
        }
        if (this instanceof LambdaContext) {
          var options = {
            throwError: false
          };
          env.set('arguments', this.env.get('arguments', options));
          env.set('parent.frame', this.env.get('parent.frame', options));
        } else {
          // this case is for lambda as callback function in JS; e.g. setTimeout
          var _args = args.slice();
          _args.callee = lambda;
          _args.env = env;
          env.set('arguments', _args);
        }
        function set(name, value) {
          env.__env__[name.__name__] = value;
          dynamic_env.__env__[name.__name__] = value;
        }
        var name = code.car;
        var i = 0;
        if (name instanceof LSymbol || name !== _nil) {
          while (true) {
            if (name.car !== _nil) {
              if (name instanceof LSymbol) {
                // rest argument,  can also be first argument
                var value = quote(Pair.fromArray(args.slice(i), false));
                set(name, value);
                break;
              } else if (is_pair(name)) {
                var _value5 = args[i];
                set(name.car, _value5);
              }
            }
            if (name.cdr === _nil) {
              break;
            }
            i++;
            name = name.cdr;
          }
        }
        var rest = __doc__ ? code.cdr.cdr : code.cdr;
        var output = new Pair(new LSymbol('begin'), rest);
        return _evaluate(output, {
          env: env,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic,
          error: error
        });
      }
      var length = code.car instanceof Pair ? code.car.length() : null;
      lambda.__code__ = new Pair(new LSymbol('lambda'), code);
      lambda[__lambda__] = true;
      if (!(code.car instanceof Pair)) {
        return doc(lambda, __doc__, true); // variable arguments
      }
      // wrap and decorate with __doc__
      return doc(set_fn_length(lambda, length), __doc__, true);
    }, "(lambda (a b) body)\n        (lambda args body)\n        (lambda (a b . rest) body)\n\n        The lambda macro creates a new anonymous function. If the first element of\n        the body is a string and there is more elements the string is used as the\n        documentation string, that can be read using (help fn)."),
    'macroexpand': new Macro('macroexpand', macro_expand()),
    'macroexpand-1': new Macro('macroexpand-1', macro_expand(true)),
    // ------------------------------------------------------------------
    'define-macro': doc(new Macro(macro, function (macro, _ref36) {
      var use_dynamic = _ref36.use_dynamic,
        error = _ref36.error;
      if (macro.car instanceof Pair && macro.car.car instanceof LSymbol) {
        var name = macro.car.car.__name__;
        var __doc__;
        if (LString.isString(macro.cdr.car) && macro.cdr.cdr instanceof Pair) {
          __doc__ = macro.cdr.car.valueOf();
        }
        var makro_instance = Macro.defmacro(name, function (code) {
          var env = new Environment({}, this, 'defmacro');
          var name = macro.car.cdr;
          var arg = code;
          while (true) {
            if (name === _nil) {
              break;
            }
            if (name instanceof LSymbol) {
              env.__env__[name.__name__] = arg;
              break;
            } else if (name.car !== _nil) {
              if (arg === _nil) {
                env.__env__[name.car.__name__] = _nil;
              } else {
                if (arg.car instanceof Pair) {
                  arg.car[__data__] = true;
                }
                env.__env__[name.car.__name__] = arg.car;
              }
            }
            if (name.cdr === _nil) {
              break;
            }
            if (arg !== _nil) {
              arg = arg.cdr;
            }
            name = name.cdr;
          }
          var eval_args = {
            env: env,
            dynamic_env: env,
            use_dynamic: use_dynamic,
            error: error
          };
          // evaluate macro
          if (macro.cdr instanceof Pair) {
            // this eval will return lips code
            var rest = __doc__ ? macro.cdr.cdr : macro.cdr;
            var result = rest.reduce(function (result, node) {
              return _evaluate(node, eval_args);
            });
            return unpromise(result, function (result) {
              if (_typeof(result) === 'object') {
                delete result[__data__];
              }
              return result;
            });
          }
        }, __doc__, true);
        makro_instance.__code__ = new Pair(new LSymbol('define-macro'), macro);
        this.set(name, makro_instance);
      }
    }), "(define-macro (name . args) body)\n\n         The meta-macro, that creates new macros. If the return value is a list structure\n         it will be evaluated where the macro is invoked from. You can use quasiquote `\n         and unquote , and unquote-splicing ,@ inside to create an expression that will be\n         evaluated at runtime. Macros works like this: if you pass any expression to a\n         macro the arguments will not be evaluated unless the macro's body explicitly\n         calls (eval) on it. Because of this a macro can manipulate the expression\n         (arguments) as lists."),
    // ------------------------------------------------------------------
    'syntax-rules': new Macro('syntax-rules', function (macro, options) {
      var use_dynamic = options.use_dynamic,
        error = options.error;
      var env = this;
      function get_identifiers(node) {
        var symbols = [];
        while (node !== _nil) {
          var x = node.car;
          symbols.push(x.valueOf());
          node = node.cdr;
        }
        return symbols;
      }
      function validate_identifiers(node) {
        while (node !== _nil) {
          var x = node.car;
          if (!(x instanceof LSymbol)) {
            throw new Error('syntax-rules: wrong identifier');
          }
          node = node.cdr;
        }
      }
      if (macro.car instanceof LSymbol) {
        validate_identifiers(macro.cdr.car);
      } else {
        validate_identifiers(macro.car);
      }
      var syntax = new Syntax(function (code, _ref37) {
        var macro_expand = _ref37.macro_expand;
        var scope = env.inherit('syntax');
        var dynamic_env = scope;
        var var_scope = this;
        // for macros that define variables used in macro (2 levels nestting)
        if (var_scope.__name__ === Syntax.__merge_env__) {
          // copy refs for defined gynsyms
          var _props2 = Object.getOwnPropertySymbols(var_scope.__env__);
          _props2.forEach(function (symbol) {
            var_scope.__parent__.set(symbol, var_scope.__env__[symbol]);
          });
          var_scope = var_scope.__parent__;
        }
        var eval_args = {
          env: scope,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic,
          error: error
        };
        var ellipsis, rules, symbols;
        if (macro.car instanceof LSymbol) {
          ellipsis = macro.car;
          symbols = get_identifiers(macro.cdr.car);
          rules = macro.cdr.cdr;
        } else {
          ellipsis = '...';
          symbols = get_identifiers(macro.car);
          rules = macro.cdr;
        }
        try {
          while (rules !== _nil) {
            var rule = rules.car.car;
            var expr = rules.car.cdr.car;
            log(rule);
            var bindings = extract_patterns(rule, code, symbols, ellipsis, {
              expansion: this,
              define: env
            });
            if (bindings) {
              /* c8 ignore next 5 */
              if (is_debug()) {
                console.log(JSON.stringify(symbolize(bindings), true, 2));
                console.log('PATTERN: ' + rule.toString(true));
                console.log('MACRO: ' + code.toString(true));
              }
              // name is modified in transform_syntax
              var names = [];
              var new_expr = transform_syntax({
                bindings: bindings,
                expr: expr,
                symbols: symbols,
                scope: scope,
                lex_scope: var_scope,
                names: names,
                ellipsis: ellipsis
              });
              log('OUPUT>>> ' + new_expr.toString());
              if (new_expr) {
                expr = new_expr;
              }
              var new_env = var_scope.merge(scope, Syntax.__merge_env__);
              if (macro_expand) {
                return {
                  expr: expr,
                  scope: new_env
                };
              }
              var result = _evaluate(expr, _objectSpread(_objectSpread({}, eval_args), {}, {
                env: new_env
              }));
              // Hack: update the result if there are generated
              //       gensyms that should be literal symbols
              // TODO: maybe not the part move when literal elisps may
              //       be generated, maybe they will need to be mark somehow
              return clear_gensyms(result, names);
            }
            rules = rules.cdr;
          }
        } catch (e) {
          e.message += " in macro: ".concat(macro.toString(true));
          throw e;
        }
        throw new Error("Invalid Syntax ".concat(code.toString(true)));
      }, env);
      syntax.__code__ = macro;
      return syntax;
    }, "(syntax-rules () (pattern expression) ...)\n\n        Base of hygienic macros, it will return a new syntax expander\n        that works like Lisp macros."),
    // ------------------------------------------------------------------
    quote: doc(new Macro('quote', function (arg) {
      return quote(arg.car);
    }), "(quote expression) or 'expression\n\n         Macro that returns a single LIPS expression as data (it won't evaluate the\n         argument). It will return a list if put in front of LIPS code.\n         And if put in front of a symbol it will return the symbol itself, not the value\n         bound to that name."),
    'unquote-splicing': doc('unquote-splicing', function () {
      throw new Error("You can't call `unquote-splicing` outside of quasiquote");
    }, "(unquote-splicing code) or ,@code\n\n        Special form used in the quasiquote macro. It evaluates the expression inside and\n        splices the list into quasiquote's result. If it is not the last element of the\n        expression, the computed value must be a pair."),
    'unquote': doc('unquote', function () {
      throw new Error("You can't call `unquote` outside of quasiquote");
    }, "(unquote code) or ,code\n\n        Special form used in the quasiquote macro. It evaluates the expression inside and\n        substitutes the value into quasiquote's result."),
    // ------------------------------------------------------------------
    quasiquote: Macro.defmacro('quasiquote', function (arg, env) {
      var use_dynamic = env.use_dynamic,
        error = env.error;
      var self = this;
      //var max_unquote = 1;
      var dynamic_env = self;
      // -----------------------------------------------------------------
      function is_struct(value) {
        return value instanceof Pair || is_plain_object(value) || Array.isArray(value);
      }
      // -----------------------------------------------------------------
      function resolve_pair(pair, fn) {
        var test = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : is_struct;
        if (pair instanceof Pair) {
          var car = pair.car;
          var cdr = pair.cdr;
          if (test(car)) {
            car = fn(car);
          }
          if (test(cdr)) {
            cdr = fn(cdr);
          }
          if (is_promise(car) || is_promise(cdr)) {
            return promise_all([car, cdr]).then(function (_ref38) {
              var _ref39 = _slicedToArray(_ref38, 2),
                car = _ref39[0],
                cdr = _ref39[1];
              return new Pair(car, cdr);
            });
          } else {
            return new Pair(car, cdr);
          }
        }
        return pair;
      }
      // -----------------------------------------------------------------
      function join(eval_pair, value) {
        if (eval_pair instanceof Pair) {
          if (value !== _nil) {
            eval_pair.append(value);
          }
        } else {
          eval_pair = new Pair(eval_pair, value);
        }
        return eval_pair;
      }
      // -----------------------------------------------------------------
      function unquoted_arr(arr) {
        return !!arr.filter(function (value) {
          return value instanceof Pair && LSymbol.is(value.car, /^(unquote|unquote-splicing)$/);
        }).length;
      }
      // -----------------------------------------------------------------
      function quote_vector(arr, unquote_cnt, max_unq) {
        return arr.reduce(function (acc, x) {
          if (!(x instanceof Pair)) {
            acc.push(x);
            return acc;
          }
          if (LSymbol.is(x.car, 'unquote-splicing')) {
            var result;
            if (unquote_cnt + 1 < max_unq) {
              result = recur(x.cdr, unquote_cnt + 1, max_unq);
            } else {
              result = _evaluate(x.cdr.car, {
                env: self,
                use_dynamic: use_dynamic,
                dynamic_env: dynamic_env,
                error: error
              });
            }
            if (!(result instanceof Pair)) {
              throw new Error("Expecting list ".concat(type(x), " found"));
            }
            return acc.concat(result.to_array());
          }
          acc.push(recur(x, unquote_cnt, max_unq));
          return acc;
        }, []);
      }
      // -----------------------------------------------------------------
      function quote_object(object, unquote_cnt, max_unq) {
        var result = {};
        unquote_cnt++;
        Object.keys(object).forEach(function (key) {
          var value = object[key];
          if (value instanceof Pair) {
            if (LSymbol.is(value.car, 'unquote-splicing')) {
              throw new Error("You can't call `unquote-splicing` " + "inside object");
            }
            var output;
            if (unquote_cnt < max_unq) {
              output = recur(value.cdr.car, unquote_cnt, max_unq);
            } else {
              output = _evaluate(value.cdr.car, {
                env: self,
                dynamic_env: dynamic_env,
                use_dynamic: use_dynamic,
                error: error
              });
            }
            result[key] = output;
          } else {
            result[key] = value;
          }
        });
        if (Object.isFrozen(object)) {
          Object.freeze(result);
        }
        return result;
      }
      // -----------------------------------------------------------------
      function unquote_splice(pair, unquote_cnt, max_unq) {
        if (unquote_cnt < max_unq) {
          return new Pair(new Pair(pair.car.car, recur(pair.car.cdr, unquote_cnt, max_unq)), _nil);
        }
        var lists = [];
        return function next(node) {
          var value = _evaluate(node.car, {
            env: self,
            dynamic_env: dynamic_env,
            use_dynamic: use_dynamic,
            error: error
          });
          lists.push(value);
          if (node.cdr instanceof Pair) {
            return next(node.cdr);
          }
          return unpromise(lists, function (arr) {
            if (arr.some(function (x) {
              return !(x instanceof Pair);
            })) {
              if (pair.cdr instanceof Pair && LSymbol.is(pair.cdr.car, '.') && pair.cdr.cdr instanceof Pair && pair.cdr.cdr.cdr === _nil) {
                return pair.cdr.cdr.car;
              }
              if (!(pair.cdr === _nil || pair.cdr instanceof Pair)) {
                var msg = "You can't splice atom inside list";
                throw new Error(msg);
              }
              if (arr.length > 1) {
                var _msg = "You can't splice multiple atoms inside list";
                throw new Error(_msg);
              }
              if (!(pair.cdr instanceof Pair && arr[0] === _nil)) {
                return arr[0];
              }
            }
            // don't create Cycles
            arr = arr.map(function (eval_pair) {
              if (splices.has(eval_pair)) {
                return eval_pair.clone();
              } else {
                splices.add(eval_pair);
                return eval_pair;
              }
            });
            var value = recur(pair.cdr, 0, 1);
            if (value === _nil && arr[0] === _nil) {
              return undefined;
            }
            return unpromise(value, function (value) {
              if (arr[0] === _nil) {
                return value;
              }
              if (arr.length === 1) {
                return join(arr[0], value);
              }
              var result = arr.reduce(function (result, eval_pair) {
                return join(result, eval_pair);
              });
              return join(result, value);
            });
          });
        }(pair.car.cdr);
      }
      // -----------------------------------------------------------------
      var splices = new Set();
      function recur(pair, unquote_cnt, max_unq) {
        if (pair instanceof Pair) {
          if (pair.car instanceof Pair) {
            if (LSymbol.is(pair.car.car, 'unquote-splicing')) {
              return unquote_splice(pair, unquote_cnt + 1, max_unq);
            }
            if (LSymbol.is(pair.car.car, 'unquote')) {
              // + 2 - one for unquote and one for unquote splicing
              if (unquote_cnt + 2 === max_unq && pair.car.cdr instanceof Pair && pair.car.cdr.car instanceof Pair && LSymbol.is(pair.car.cdr.car.car, 'unquote-splicing')) {
                var rest = pair.car.cdr;
                return new Pair(new Pair(new LSymbol('unquote'), unquote_splice(rest, unquote_cnt + 2, max_unq)), _nil);
              } else if (pair.car.cdr instanceof Pair && pair.car.cdr.cdr !== _nil) {
                if (pair.car.cdr.car instanceof Pair) {
                  // values inside unquote are lists
                  var result = [];
                  return function recur(node) {
                    if (node === _nil) {
                      return Pair.fromArray(result);
                    }
                    return unpromise(_evaluate(node.car, {
                      env: self,
                      dynamic_env: dynamic_env,
                      use_dynamic: use_dynamic,
                      error: error
                    }), function (next) {
                      result.push(next);
                      return recur(node.cdr);
                    });
                  }(pair.car.cdr);
                } else {
                  // same as in guile if (unquote 1 2 3) it should be
                  // spliced - scheme spec say it's unspecify but it
                  // work like in CL
                  return pair.car.cdr;
                }
              }
            }
          }
          if (LSymbol.is(pair.car, 'quasiquote')) {
            var cdr = recur(pair.cdr, unquote_cnt, max_unq + 1);
            return new Pair(pair.car, cdr);
          }
          if (LSymbol.is(pair.car, 'quote')) {
            return new Pair(pair.car, recur(pair.cdr, unquote_cnt, max_unq));
          }
          if (LSymbol.is(pair.car, 'unquote')) {
            unquote_cnt++;
            if (unquote_cnt < max_unq) {
              return new Pair(new LSymbol('unquote'), recur(pair.cdr, unquote_cnt, max_unq));
            }
            if (unquote_cnt > max_unq) {
              throw new Error("You can't call `unquote` outside " + "of quasiquote");
            }
            if (pair.cdr instanceof Pair) {
              if (pair.cdr.cdr !== _nil) {
                if (pair.cdr.car instanceof Pair) {
                  // TODO: test if this part is needed
                  // this part was duplicated in previous section
                  // if (LSymbol.is(pair.car.car, 'unquote')) {
                  // so this probably can be removed
                  var _result3 = [];
                  // evaluate all values in unquote
                  return function recur(node) {
                    if (node === _nil) {
                      return Pair.fromArray(_result3);
                    }
                    return unpromise(_evaluate(node.car, {
                      env: self,
                      dynamic_env: dynamic_env,
                      use_dynamic: use_dynamic,
                      error: error
                    }), function (next) {
                      _result3.push(next);
                      return recur(node.cdr);
                    });
                  }(pair.cdr);
                } else {
                  return pair.cdr;
                }
              } else {
                return _evaluate(pair.cdr.car, {
                  env: self,
                  dynamic_env: dynamic_env,
                  error: error
                });
              }
            } else {
              return pair.cdr;
            }
          }
          return resolve_pair(pair, function (pair) {
            return recur(pair, unquote_cnt, max_unq);
          });
        } else if (is_plain_object(pair)) {
          return quote_object(pair, unquote_cnt, max_unq);
        } else if (pair instanceof Array) {
          return quote_vector(pair, unquote_cnt, max_unq);
        }
        return pair;
      }
      // -----------------------------------------------------------------
      function clear(node) {
        if (node instanceof Pair) {
          delete node[__data__];
          if (!node.haveCycles('car')) {
            clear(node.car);
          }
          if (!node.haveCycles('cdr')) {
            clear(node.cdr);
          }
        }
      }
      // -----------------------------------------------------------------
      if (is_plain_object(arg.car) && !unquoted_arr(Object.values(arg.car))) {
        return quote(arg.car);
      }
      if (Array.isArray(arg.car) && !unquoted_arr(arg.car)) {
        return quote(arg.car);
      }
      if (arg.car instanceof Pair && !arg.car.find('unquote') && !arg.car.find('unquote-splicing') && !arg.car.find('quasiquote')) {
        return quote(arg.car);
      }
      var x = recur(arg.car, 0, 1);
      return unpromise(x, function (value) {
        // clear nested data for tests
        clear(value);
        return quote(value);
      });
    }, "(quasiquote list)\n\n        Similar macro to `quote` but inside it you can use special expressions (unquote\n        x) abbreviated to ,x that will evaluate x and insert its value verbatim or\n        (unquote-splicing x) abbreviated to ,@x that will evaluate x and splice the value\n        into the result. Best used with macros but it can be used outside."),
    // ------------------------------------------------------------------
    clone: doc('clone', function clone(list) {
      typecheck('clone', list, 'pair');
      return list.clone();
    }, "(clone list)\n\n        Function that returns a clone of the list, that does not share any pairs with the\n        original, so the clone can be safely mutated without affecting the original."),
    // ------------------------------------------------------------------
    append: doc('append', function append() {
      var _global_env$get;
      for (var _len24 = arguments.length, items = new Array(_len24), _key24 = 0; _key24 < _len24; _key24++) {
        items[_key24] = arguments[_key24];
      }
      items = items.map(function (item) {
        if (item instanceof Pair) {
          return item.clone();
        }
        return item;
      });
      return (_global_env$get = global_env.get('append!')).call.apply(_global_env$get, [this].concat(_toConsumableArray(items)));
    }, "(append item ...)\n\n        Function that creates a new list with each argument appended end-to-end.\n        It will always return a new list and not modify its arguments."),
    // ------------------------------------------------------------------
    'append!': doc('append!', function () {
      var is_list = global_env.get('list?');
      for (var _len25 = arguments.length, items = new Array(_len25), _key25 = 0; _key25 < _len25; _key25++) {
        items[_key25] = arguments[_key25];
      }
      return items.reduce(function (acc, item) {
        typecheck('append!', acc, ['nil', 'pair']);
        if ((item instanceof Pair || item === _nil) && !is_list(item)) {
          throw new Error('append!: Invalid argument, value is not a list');
        }
        if (is_null(item)) {
          return acc;
        }
        if (acc === _nil) {
          if (item === _nil) {
            return _nil;
          }
          return item;
        }
        return acc.append(item);
      }, _nil);
    }, "(append! arg1 ...)\n\n        Destructive version of append, it can modify the lists in place. It returns\n        a new list where each argument is appended to the end. It may modify\n        lists added as arguments."),
    // ------------------------------------------------------------------
    reverse: doc('reverse', function reverse(arg) {
      typecheck('reverse', arg, ['array', 'pair', 'nil']);
      if (arg === _nil) {
        return _nil;
      }
      if (arg instanceof Pair) {
        var arr = global_env.get('list->array')(arg).reverse();
        return global_env.get('array->list')(arr);
      } else if (Array.isArray(arg)) {
        return arg.reverse();
      } else {
        throw new Error(typeErrorMessage('reverse', type(arg), 'array or pair'));
      }
    }, "(reverse list)\n\n        Function that reverses the list or array. If value is not a list\n        or array it will error."),
    // ------------------------------------------------------------------
    nth: doc('nth', function nth(index, obj) {
      typecheck('nth', index, 'number');
      typecheck('nth', obj, ['array', 'pair']);
      if (obj instanceof Pair) {
        var node = obj;
        var count = 0;
        while (count < index) {
          if (!node.cdr || node.cdr === _nil || node.haveCycles('cdr')) {
            return _nil;
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
    }, "(nth index obj)\n\n        Function that returns the nth element of the list or array.\n        If used with a non-indexable value it will error."),
    // ------------------------------------------------------------------
    list: doc('list', function list() {
      for (var _len26 = arguments.length, args = new Array(_len26), _key26 = 0; _key26 < _len26; _key26++) {
        args[_key26] = arguments[_key26];
      }
      return args.reverse().reduce(function (list, item) {
        return new Pair(item, list);
      }, _nil);
    }, "(list . args)\n\n        Function that creates a new list out of its arguments."),
    // ------------------------------------------------------------------
    substring: doc('substring', function substring(string, start, end) {
      typecheck('substring', string, 'string');
      typecheck('substring', start, 'number');
      typecheck('substring', end, ['number', 'undefined']);
      return string.substring(start.valueOf(), end && end.valueOf());
    }, "(substring string start end)\n\n        Function that returns the slice of the string starting at start and ending\n        with end."),
    // ------------------------------------------------------------------
    concat: doc('concat', function concat() {
      for (var _len27 = arguments.length, args = new Array(_len27), _key27 = 0; _key27 < _len27; _key27++) {
        args[_key27] = arguments[_key27];
      }
      args.forEach(function (arg, i) {
        return typecheck('concat', arg, 'string', i + 1);
      });
      return args.join('');
    }, "(concat . strings)\n\n        Function that creates a new string by joining its arguments."),
    // ------------------------------------------------------------------
    join: doc('join', function join(separator, list) {
      typecheck('join', separator, 'string');
      typecheck('join', list, ['pair', 'nil']);
      return global_env.get('list->array')(list).join(separator);
    }, "(join separator list)\n\n        Function that returns a string by joining elements of the list using separator."),
    // ------------------------------------------------------------------
    split: doc('split', function split(separator, string) {
      typecheck('split', separator, ['regex', 'string']);
      typecheck('split', string, 'string');
      return global_env.get('array->list')(string.split(separator));
    }, "(split separator string)\n\n        Function that creates a list by splitting string by separator which can\n        be a string or regular expression."),
    // ------------------------------------------------------------------
    replace: doc('replace', function replace(pattern, replacement, string) {
      typecheck('replace', pattern, ['regex', 'string']);
      typecheck('replace', replacement, ['string', 'function']);
      typecheck('replace', string, 'string');
      return string.replace(pattern, replacement);
    }, "(replace pattern replacement string)\n\n        Function that changes pattern to replacement inside string. Pattern can be a\n        string or regex and replacement can be function or string. See Javascript\n        String.replace()."),
    // ------------------------------------------------------------------
    match: doc('match', function match(pattern, string) {
      typecheck('match', pattern, ['regex', 'string']);
      typecheck('match', string, 'string');
      var m = string.match(pattern);
      return m ? global_env.get('array->list')(m) : false;
    }, "(match pattern string)\n\n        Function that returns a match object from JavaScript as a list or #f if\n        no match."),
    // ------------------------------------------------------------------
    search: doc('search', function search(pattern, string) {
      typecheck('search', pattern, ['regex', 'string']);
      typecheck('search', string, 'string');
      return string.search(pattern);
    }, "(search pattern string)\n\n        Function that returns the first found index of the pattern inside a string."),
    // ------------------------------------------------------------------
    repr: doc('repr', function repr(obj, quote) {
      return toString(obj, quote);
    }, "(repr obj)\n\n        Function that returns a LIPS code representation of the object as a string."),
    // ------------------------------------------------------------------
    'escape-regex': doc('escape-regex', function (string) {
      typecheck('escape-regex', string, 'string');
      return escape_regex(string.valueOf());
    }, "(escape-regex string)\n\n        Function that returns a new string where all special operators used in regex,\n        are escaped with backslashes so they can be used in the RegExp constructor\n        to match a literal string."),
    // ------------------------------------------------------------------
    env: doc('env', function env(env) {
      env = env || this.env;
      var names = Object.keys(env.__env__).map(LSymbol);
      var result;
      if (names.length) {
        result = Pair.fromArray(names);
      } else {
        result = _nil;
      }
      if (env.__parent__ instanceof Environment) {
        return global_env.get('env').call(this, env.__parent__).append(result);
      }
      return result;
    }, "(env)\n        (env obj)\n\n        Function that returns a list of names (functions, macros and variables)\n        that are bound in the current environment or one of its parents."),
    // ------------------------------------------------------------------
    'new': doc('new', function (obj) {
      for (var _len28 = arguments.length, args = new Array(_len28 > 1 ? _len28 - 1 : 0), _key28 = 1; _key28 < _len28; _key28++) {
        args[_key28 - 1] = arguments[_key28];
      }
      var instance = _construct(unbind(obj), _toConsumableArray(args.map(function (x) {
        return unbox(x);
      })));
      return instance;
    }, "(new obj . args)\n\n        Function that creates new JavaScript instance of an object."),
    // ------------------------------------------------------------------
    'typecheck': doc(typecheck, "(typecheck label value type [position])\n\n         Checks the type of value and errors if the type is not one allowed.  Type can be\n         string or list of strings. The position optional argument is used to create a\n         proper error message for the nth argument of function calls."),
    // ------------------------------------------------------------------
    'unset-special!': doc('unset-special!', function (symbol) {
      typecheck('remove-special!', symbol, 'string');
      delete specials.remove(symbol.valueOf());
    }, "(unset-special! name)\n\n        Function that removes a special symbol from parser added by `set-special!`,\n        name must be a string."),
    // ------------------------------------------------------------------
    'set-special!': doc('set-special!', function (seq, name) {
      var type = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : specials.LITERAL;
      typecheck('set-special!', seq, 'string', 1);
      typecheck('set-special!', name, 'symbol', 2);
      specials.append(seq.valueOf(), name, type);
    }, "(set-special! symbol name [type])\n\n        Add a special symbol to the list of transforming operators by the parser.\n        e.g.: `(add-special! \"#\" 'x)` will allow to use `#(1 2 3)` and it will be\n        transformed into (x (1 2 3)) so you can write x macro that will process\n        the list. 3rd argument is optional, and it can be one of two values:\n        lips.specials.LITERAL, which is the default behavior, or\n        lips.specials.SPLICE which causes the value to be unpacked into the expression.\n        This can be used for e.g. to make `#(1 2 3)` into (x 1 2 3) that is needed\n        by # that defines vectors."),
    // ------------------------------------------------------------------
    'get': get,
    '.': get,
    // ------------------------------------------------------------------
    'unbind': doc(unbind, "(unbind fn)\n\n         Function that removes the weak 'this' binding from a function so you\n         can get properties from the actual function object."),
    // ------------------------------------------------------------------
    type: doc(type, "(type object)\n\n         Function that returns the type of an object as string."),
    // ------------------------------------------------------------------
    'debugger': doc('debugger', function () {
      /* eslint-disable */
      debugger;
      /* eslint-enable */
    }, "(debugger)\n\n        Function that triggers the JavaScript debugger (e.g. the browser devtools)\n        using the \"debugger;\" statement. If a debugger is not running this\n        function does nothing."),
    // ------------------------------------------------------------------
    'in': doc('in', function (a, b) {
      if (a instanceof LSymbol || a instanceof LString || a instanceof LNumber) {
        a = a.valueOf();
      }
      return a in unbox(b);
    }, "(in key value)\n\n        Function that uses the Javascript \"in\" operator to check if key is\n        a valid property in the value."),
    // ------------------------------------------------------------------
    'instanceof': doc('instanceof', function (type, obj) {
      return obj instanceof unbind(type);
    }, "(instanceof type obj)\n\n        Predicate that tests if the obj is an instance of type."),
    // ------------------------------------------------------------------
    'prototype?': doc('prototype?', is_prototype, "(prototype? obj)\n\n         Predicate that tests if value is a valid JavaScript prototype,\n         i.e. calling (new) with it will not throw '<x> is not a constructor'."),
    // ------------------------------------------------------------------
    'macro?': doc('macro?', function (obj) {
      return obj instanceof Macro;
    }, "(macro? expression)\n\n        Predicate that tests if value is a macro."),
    // ------------------------------------------------------------------
    'function?': doc('function?', is_function, "(function? expression)\n\n         Predicate that tests if value is a callable function."),
    // ------------------------------------------------------------------
    'real?': doc('real?', function (value) {
      if (type(value) !== 'number') {
        return false;
      }
      if (value instanceof LNumber) {
        return value.isFloat();
      }
      return LNumber.isFloat(value);
    }, "(real? number)\n\n        Predicate that tests if value is a real number (not complex)."),
    // ------------------------------------------------------------------
    'number?': doc('number?', function (x) {
      return Number.isNaN(x) || LNumber.isNumber(x);
    }, "(number? expression)\n\n        Predicate that tests if value is a number or NaN value."),
    // ------------------------------------------------------------------
    'string?': doc('string?', function (obj) {
      return LString.isString(obj);
    }, "(string? expression)\n\n        Predicate that tests if value is a string."),
    // ------------------------------------------------------------------
    'pair?': doc('pair?', function (obj) {
      return obj instanceof Pair;
    }, "(pair? expression)\n\n        Predicate that tests if value is a pair or list structure."),
    // ------------------------------------------------------------------
    'regex?': doc('regex?', function (obj) {
      return obj instanceof RegExp;
    }, "(regex? expression)\n\n        Predicate that tests if value is a regular expression."),
    // ------------------------------------------------------------------
    'null?': doc('null?', function (obj) {
      return is_null(obj);
    }, "(null? expression)\n\n        Predicate that tests if value is null-ish (i.e. undefined, nil, or\n        Javascript null)."),
    // ------------------------------------------------------------------
    'boolean?': doc('boolean?', function (obj) {
      return typeof obj === 'boolean';
    }, "(boolean? expression)\n\n        Predicate that tests if value is a boolean (#t or #f)."),
    // ------------------------------------------------------------------
    'symbol?': doc('symbol?', function (obj) {
      return obj instanceof LSymbol;
    }, "(symbol? expression)\n\n        Predicate that tests if value is a LIPS symbol."),
    // ------------------------------------------------------------------
    'array?': doc('array?', function (obj) {
      return obj instanceof Array;
    }, "(array? expression)\n\n        Predicate that tests if value is an array."),
    // ------------------------------------------------------------------
    'object?': doc('object?', function (obj) {
      return obj !== _nil && obj !== null && !(obj instanceof LCharacter) && !(obj instanceof RegExp) && !(obj instanceof LString) && !(obj instanceof Pair) && !(obj instanceof LNumber) && _typeof(obj) === 'object' && !(obj instanceof Array);
    }, "(object? expression)\n\n        Predicate that tests if value is an plain object (not another LIPS type)."),
    // ------------------------------------------------------------------
    flatten: doc('flatten', function flatten(list) {
      typecheck('flatten', list, 'pair');
      return list.flatten();
    }, "(flatten list)\n\n        Returns a shallow list from tree structure (pairs)."),
    // ------------------------------------------------------------------
    'array->list': doc('array->list', function (array) {
      typecheck('array->list', array, 'array');
      return Pair.fromArray(array);
    }, "(array->list array)\n\n        Function that converts a JavaScript array to a LIPS cons list."),
    // ------------------------------------------------------------------
    'tree->array': doc('tree->array', to_array('tree->array', true), "(tree->array list)\n\n         Function that converts a LIPS cons tree structure into a JavaScript array."),
    // ------------------------------------------------------------------
    'list->array': doc('list->array', to_array('list->array'), "(list->array list)\n\n         Function that converts a LIPS list into a JavaScript array."),
    // ------------------------------------------------------------------
    apply: doc('apply', function apply(fn) {
      for (var _len29 = arguments.length, args = new Array(_len29 > 1 ? _len29 - 1 : 0), _key29 = 1; _key29 < _len29; _key29++) {
        args[_key29 - 1] = arguments[_key29];
      }
      typecheck('apply', fn, 'function', 1);
      var last = args.pop();
      typecheck('apply', last, ['pair', 'nil'], args.length + 2);
      args = args.concat(global_env.get('list->array').call(this, last));
      return fn.apply(this, prepare_fn_args(fn, args));
    }, "(apply fn list)\n\n        Function that calls fn with the list of arguments."),
    // ------------------------------------------------------------------
    length: doc('length', function length(obj) {
      if (!obj || obj === _nil) {
        return 0;
      }
      if (obj instanceof Pair) {
        return obj.length();
      }
      if ("length" in obj) {
        return obj.length;
      }
    }, "(length expression)\n\n        Function that returns the length of the object. The object can be a LIPS\n        list or any object that has a \"length\" property. Returns undefined if the\n        length could not be found."),
    // ------------------------------------------------------------------
    'string->number': doc('string->number', function (arg) {
      var radix = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 10;
      typecheck('string->number', arg, 'string', 1);
      typecheck('string->number', radix, 'number', 2);
      arg = arg.valueOf();
      radix = radix.valueOf();
      if (arg.match(rational_bare_re) || arg.match(rational_re)) {
        return parse_rational(arg, radix);
      } else if (arg.match(complex_bare_re) || arg.match(complex_re)) {
        return parse_complex(arg, radix);
      } else {
        var valid_bare = radix === 10 && !arg.match(/e/i) || radix === 16;
        if (arg.match(int_bare_re) && valid_bare || arg.match(int_re)) {
          return parse_integer(arg, radix);
        }
        if (arg.match(float_re)) {
          return parse_float(arg);
        }
      }
      return false;
    }, "(string->number number [radix])\n\n        Function that parses a string into a number."),
    // ------------------------------------------------------------------
    'try': doc(new Macro('try', function (code, _ref40) {
      var _this24 = this;
      var use_dynamic = _ref40.use_dynamic,
        error = _ref40.error;
      return new Promise(function (resolve, reject) {
        var catch_clause, finally_clause;
        if (LSymbol.is(code.cdr.car.car, 'catch')) {
          catch_clause = code.cdr.car;
          if (code.cdr.cdr instanceof Pair && LSymbol.is(code.cdr.cdr.car.car, 'finally')) {
            finally_clause = code.cdr.cdr.car;
          }
        } else if (LSymbol.is(code.cdr.car.car, 'finally')) {
          finally_clause = code.cdr.car;
        }
        if (!(finally_clause || catch_clause)) {
          throw new Error('try: invalid syntax');
        }
        var _next = resolve;
        if (finally_clause) {
          _next = function next(result, cont) {
            // prevent infinite loop when finally throw exception
            _next = reject;
            unpromise(_evaluate(new Pair(new LSymbol('begin'), finally_clause.cdr), args), function () {
              cont(result);
            });
          };
        }
        var args = {
          env: _this24,
          use_dynamic: use_dynamic,
          dynamic_env: _this24,
          error: function (_error) {
            function error(_x17) {
              return _error.apply(this, arguments);
            }
            error.toString = function () {
              return _error.toString();
            };
            return error;
          }(function (e) {
            var env = _this24.inherit('try');
            if (catch_clause) {
              env.set(catch_clause.cdr.car.car, e);
              var args = {
                env: env,
                error: error
              };
              args.dynamic_env = _this24;
              unpromise(_evaluate(new Pair(new LSymbol('begin'), catch_clause.cdr.cdr), args), function (result) {
                _next(result, resolve);
              });
            } else {
              _next(e, error);
            }
          })
        };
        var result = _evaluate(code.car, args);
        if (is_promise(result)) {
          result.then(function (result) {
            _next(result, resolve);
          })["catch"](args.error);
        } else {
          _next(result, resolve);
        }
      });
    }), "(try expr (catch (e) code))\n         (try expr (catch (e) code) (finally code))\n         (try expr (finally code))\n\n         Macro that executes expr and catches any exceptions thrown. If catch is provided\n         it's executed when an error is thrown. If finally is provided it's always\n         executed at the end."),
    // ------------------------------------------------------------------
    'raise': doc('raise', function (obj) {
      throw obj;
    }, "(raise obj)\n\n        Throws the object verbatim (no wrapping an a new Error)."),
    'throw': doc('throw', function (message) {
      throw new Error(message);
    }, "(throw string)\n\n        Throws a new exception."),
    // ------------------------------------------------------------------
    find: doc('find', function find(arg, list) {
      typecheck('find', arg, ['regex', 'function']);
      typecheck('find', list, ['pair', 'nil']);
      if (is_null(list)) {
        return _nil;
      }
      var fn = matcher('find', arg);
      return unpromise(fn(list.car), function (value) {
        if (value && value !== _nil) {
          return list.car;
        }
        return find(arg, list.cdr);
      });
    }, "(find fn list)\n        (find regex list)\n\n        Higher-order function that finds the first value for which fn return true.\n        If called with a regex it will create a matcher function."),
    // ------------------------------------------------------------------
    'for-each': doc('for-each', function (fn) {
      var _global_env$get2;
      typecheck('for-each', fn, 'function');
      for (var _len30 = arguments.length, lists = new Array(_len30 > 1 ? _len30 - 1 : 0), _key30 = 1; _key30 < _len30; _key30++) {
        lists[_key30 - 1] = arguments[_key30];
      }
      lists.forEach(function (arg, i) {
        typecheck('for-each', arg, ['pair', 'nil'], i + 1);
      });
      // we need to use call(this because babel transpile this code into:
      // var ret = map.apply(void 0, [fn].concat(lists));
      // it don't work with weakBind
      var ret = (_global_env$get2 = global_env.get('map')).call.apply(_global_env$get2, [this, fn].concat(lists));
      if (is_promise(ret)) {
        return ret.then(function () {});
      }
    }, "(for-each fn . lists)\n\n        Higher-order function that calls function `fn` on each\n        value of the argument. If you provide more than one list\n        it will take each value from each list and call `fn` function\n        with that many arguments as number of list arguments."),
    // ------------------------------------------------------------------
    map: doc('map', function map(fn) {
      var _this25 = this;
      for (var _len31 = arguments.length, lists = new Array(_len31 > 1 ? _len31 - 1 : 0), _key31 = 1; _key31 < _len31; _key31++) {
        lists[_key31 - 1] = arguments[_key31];
      }
      typecheck('map', fn, 'function');
      var is_list = global_env.get('list?');
      lists.forEach(function (arg, i) {
        typecheck('map', arg, ['pair', 'nil'], i + 1);
        // detect cycles
        if (arg instanceof Pair && !is_list.call(_this25, arg)) {
          throw new Error("map: argument ".concat(i + 1, " is not a list"));
        }
      });
      if (lists.length === 0) {
        return _nil;
      }
      if (lists.some(function (x) {
        return x === _nil;
      })) {
        return _nil;
      }
      var args = lists.map(function (l) {
        return l.car;
      });
      var env = this.env,
        dynamic_env = this.dynamic_env,
        use_dynamic = this.use_dynamic;
      var result = call_function(fn, args, {
        env: env,
        dynamic_env: dynamic_env,
        use_dynamic: use_dynamic
      });
      return unpromise(result, function (head) {
        return unpromise(map.call.apply(map, [_this25, fn].concat(_toConsumableArray(lists.map(function (l) {
          return l.cdr;
        })))), function (rest) {
          return new Pair(head, rest);
        });
      });
    }, "(map fn . lists)\n\n        Higher-order function that calls function `fn` with each\n        value of the list. If you provide more then one list as argument\n        it will take each value from each list and call `fn` function\n        with that many argument as number of list arguments. The return\n        values of the fn calls are accumulated in a result list and\n        returned by map."),
    // ------------------------------------------------------------------
    'list?': doc('list?', function (obj) {
      var node = obj;
      while (true) {
        if (node === _nil) {
          return true;
        }
        if (!(node instanceof Pair)) {
          return false;
        }
        if (node.haveCycles('cdr')) {
          return false;
        }
        node = node.cdr;
      }
    }, "(list? obj)\n\n        Predicate that tests if value is a proper linked list structure.\n        The car of each pair can be any value. It returns false on cyclic lists.\""),
    // ------------------------------------------------------------------
    some: doc('some', function some(fn, list) {
      typecheck('some', fn, 'function');
      typecheck('some', list, ['pair', 'nil']);
      if (is_null(list)) {
        return false;
      } else {
        return unpromise(fn(list.car), function (value) {
          return value || some(fn, list.cdr);
        });
      }
    }, "(some fn list)\n\n        Higher-order function that calls fn on each element of the list.\n        It stops and returns true when fn returns true for a value.\n        If none of the values give true, some will return false.\n        Analogous to Python any(map(fn, list))."),
    // ------------------------------------------------------------------
    fold: doc('fold', fold('fold', function (fold, fn, init) {
      for (var _len32 = arguments.length, lists = new Array(_len32 > 3 ? _len32 - 3 : 0), _key32 = 3; _key32 < _len32; _key32++) {
        lists[_key32 - 3] = arguments[_key32];
      }
      typecheck('fold', fn, 'function');
      lists.forEach(function (arg, i) {
        typecheck('fold', arg, ['pair', 'nil'], i + 1);
      });
      if (lists.some(function (x) {
        return x === _nil;
      })) {
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
    }), "(fold fn init . lists)\n\n         Function fold is left-to-right reversal of reduce. It call `fn`\n         on each pair of elements of the list and returns a single value.\n         e.g. it computes (fn 'a 'x (fn 'b 'y (fn 'c 'z 'foo)))\n         for: (fold fn 'foo '(a b c) '(x y z))"),
    // ------------------------------------------------------------------
    pluck: doc('pluck', function pluck() {
      for (var _len33 = arguments.length, keys = new Array(_len33), _key33 = 0; _key33 < _len33; _key33++) {
        keys[_key33] = arguments[_key33];
      }
      return function (obj) {
        keys = keys.map(function (x) {
          return x instanceof LSymbol ? x.__name__ : x;
        });
        if (keys.length === 0) {
          return _nil;
        } else if (keys.length === 1) {
          var _keys2 = keys,
            _keys3 = _slicedToArray(_keys2, 1),
            _key34 = _keys3[0];
          return obj[_key34];
        }
        var result = {};
        keys.forEach(function (key) {
          result[key] = obj[key];
        });
        return result;
      };
    }, "(pluck . strings)\n\n        If called with a single string it will return a function that when\n        called with an object will return that key from the object.\n        If called with more then one string the returned function will\n        create a new object by copying all properties from the given object."),
    // ------------------------------------------------------------------
    reduce: doc('reduce', fold('reduce', function (reduce, fn, init) {
      var _this26 = this;
      for (var _len34 = arguments.length, lists = new Array(_len34 > 3 ? _len34 - 3 : 0), _key35 = 3; _key35 < _len34; _key35++) {
        lists[_key35 - 3] = arguments[_key35];
      }
      typecheck('reduce', fn, 'function');
      lists.forEach(function (arg, i) {
        typecheck('reduce', arg, ['pair', 'nil'], i + 1);
      });
      if (lists.some(function (x) {
        return x === _nil;
      })) {
        return init;
      }
      return unpromise(fn.apply(void 0, _toConsumableArray(lists.map(function (l) {
        return l.car;
      })).concat([init])), function (value) {
        return reduce.call.apply(reduce, [_this26, fn, value].concat(_toConsumableArray(lists.map(function (l) {
          return l.cdr;
        }))));
      });
    }), "(reduce fn init list . lists)\n\n         Higher-order function that takes each element of the list and calls\n         the fn with result of previous call or init and the next element\n         of the list until each element is processed, and returns a single value\n         as result of last call to `fn` function.\n         e.g. it computes (fn 'c 'z (fn 'b 'y (fn 'a 'x 'foo)))\n         for: (reduce fn 'foo '(a b c) '(x y z))"),
    // ------------------------------------------------------------------
    filter: doc('filter', function filter(arg, list) {
      typecheck('filter', arg, ['regex', 'function']);
      typecheck('filter', list, ['pair', 'nil']);
      var array = global_env.get('list->array')(list);
      var result = [];
      var fn = matcher('filter', arg);
      return function loop(i) {
        function next(value) {
          if (value && value !== _nil) {
            result.push(item);
          }
          return loop(++i);
        }
        if (i === array.length) {
          return Pair.fromArray(result);
        }
        var item = array[i];
        return unpromise(fn(item), next);
      }(0);
    }, "(filter fn list)\n        (filter regex list)\n\n        Higher-order function that calls `fn` for each element of the list\n        and return a new list for only those elements for which fn returns\n        a truthy value. If called with a regex it will create a matcher function."),
    // ------------------------------------------------------------------
    compose: doc(compose, "(compose . fns)\n\n         Higher-order function that creates a new function that applies all functions\n         from right to left and returns the last value. Reverse of pipe.\n         e.g.:\n         ((compose (curry + 2) (curry * 3)) 10) --> (+ 2 (* 3 10)) --> 32"),
    pipe: doc(pipe, "(pipe . fns)\n\n         Higher-order function that creates a new function that applies all functions\n         from left to right and returns the last value. Reverse of compose.\n         e.g.:\n         ((pipe (curry + 2) (curry * 3)) 10) --> (* 3 (+ 2 10)) --> 36"),
    curry: doc(curry, "(curry fn . args)\n\n         Higher-order function that creates a curried version of the function.\n         The result function will have partially applied arguments and it\n         will keep returning one-argument functions until all arguments are provided,\n         then it calls the original function with the accumulated arguments.\n\n         e.g.:\n         (define (add a b c d) (+ a b c d))\n         (define add1 (curry add 1))\n         (define add12 (add 2))\n         (display (add12 3 4))"),
    // ------------------------------------------------------------------
    // Numbers
    // ------------------------------------------------------------------
    gcd: doc('gcd', function gcd() {
      for (var _len35 = arguments.length, args = new Array(_len35), _key36 = 0; _key36 < _len35; _key36++) {
        args[_key36] = arguments[_key36];
      }
      typecheck_args('lcm', args, 'number');
      return args.reduce(function (result, item) {
        return result.gcd(item);
      });
    }, "(gcd n1 n2 ...)\n\n        Function that returns the greatest common divisor of the arguments."),
    // ------------------------------------------------------------------
    lcm: doc('lcm', function lcm() {
      for (var _len36 = arguments.length, args = new Array(_len36), _key37 = 0; _key37 < _len36; _key37++) {
        args[_key37] = arguments[_key37];
      }
      typecheck_args('lcm', args, 'number');
      // ref: https://rosettacode.org/wiki/Least_common_multiple#JavaScript
      var n = args.length,
        a = abs(args[0]);
      for (var i = 1; i < n; i++) {
        var b = abs(args[i]),
          c = a;
        while (a && b) {
          a > b ? a %= b : b %= a;
        }
        a = abs(c * args[i]) / (a + b);
      }
      return LNumber(a);
    }, "(lcm n1 n2 ...)\n\n        Function that returns the least common multiple of the arguments."),
    // ------------------------------------------------------------------
    'odd?': doc('odd?', single_math_op(function (num) {
      return LNumber(num).isOdd();
    }), "(odd? number)\n\n         Checks if number is odd."),
    // ------------------------------------------------------------------
    'even?': doc('even?', single_math_op(function (num) {
      return LNumber(num).isEven();
    }), "(even? number)\n\n         Checks if number is even."),
    // ------------------------------------------------------------------
    // math functions
    '*': doc('*', reduce_math_op(function (a, b) {
      return LNumber(a).mul(b);
    }, LNumber(1)), "(* . numbers)\n\n        Multiplies all numbers passed as arguments. If single value is passed\n        it will return that value."),
    // ------------------------------------------------------------------
    '+': doc('+', reduce_math_op(function (a, b) {
      return LNumber(a).add(b);
    }, LNumber(0)), "(+ . numbers)\n\n        Sums all numbers passed as arguments. If single value is passed it will\n        return that value."),
    // ------------------------------------------------------------------
    '-': doc('-', function () {
      for (var _len37 = arguments.length, args = new Array(_len37), _key38 = 0; _key38 < _len37; _key38++) {
        args[_key38] = arguments[_key38];
      }
      if (args.length === 0) {
        throw new Error('-: procedure require at least one argument');
      }
      typecheck_args('-', args, 'number');
      if (args.length === 1) {
        return LNumber(args[0]).sub();
      }
      if (args.length) {
        return args.reduce(binary_math_op(function (a, b) {
          return LNumber(a).sub(b);
        }));
      }
    }, "(- n1 n2 ...)\n        (- n)\n\n        Subtracts n2 and subsequent numbers from n1. If only one argument is passed\n        it will negate the value."),
    // ------------------------------------------------------------------
    '/': doc('/', function () {
      for (var _len38 = arguments.length, args = new Array(_len38), _key39 = 0; _key39 < _len38; _key39++) {
        args[_key39] = arguments[_key39];
      }
      if (args.length === 0) {
        throw new Error('/: procedure require at least one argument');
      }
      typecheck_args('/', args, 'number');
      if (args.length === 1) {
        return LNumber(1).div(args[0]);
      }
      return args.reduce(binary_math_op(function (a, b) {
        return LNumber(a).div(b);
      }));
    }, "(/ n1 n2 ...)\n        (/ n)\n\n        Divides n1 by n2 and subsequent arguments one by one. If single argument\n        is passed it will calculate (/ 1 n)."),
    // ------------------------------------------------------------------
    abs: doc('abs', single_math_op(function (n) {
      return LNumber(n).abs();
    }), "(abs number)\n\n         Function that returns the absolute value (magnitude) of number."),
    // ------------------------------------------------------------------
    truncate: doc('truncate', function (n) {
      typecheck('truncate', n, 'number');
      if (LNumber.isFloat(n)) {
        if (n instanceof LNumber) {
          n = n.valueOf();
        }
        return LFloat(truncate(n));
      }
      return n;
    }, "(truncate n)\n\n        Function that returns the integer part (floor) of a real number."),
    // ------------------------------------------------------------------
    sqrt: doc('sqrt', single_math_op(function (n) {
      return LNumber(n).sqrt();
    }), "(sqrt number)\n\n         Function that returns the square root of the number."),
    // ------------------------------------------------------------------
    '**': doc('**', binary_math_op(function (a, b) {
      a = LNumber(a);
      b = LNumber(b);
      if (b.cmp(0) === -1) {
        return LFloat(1).div(a).pow(b.sub());
      }
      return a.pow(b);
    }), "(** a b)\n\n         Function that calculates number a to to the power of b."),
    // ------------------------------------------------------------------
    '1+': doc('1+', single_math_op(function (number) {
      return LNumber(number).add(1);
    }), "(1+ number)\n\n         Function that adds 1 to the number and return result."),
    // ------------------------------------------------------------------
    '1-': doc(single_math_op(function (number) {
      return LNumber(number).sub(1);
    }), "(1- number)\n\n         Function that subtracts 1 from the number and return result."),
    // ------------------------------------------------------------------
    '%': doc('%', function (a, b) {
      typecheck_args('%', [a, b], 'number');
      return LNumber(a).rem(b);
    }, "(% n1 n2)\n\n        Function returns the remainder of n1/n2 (modulo)."),
    // ------------------------------------------------------------------
    // Booleans
    '==': doc('==', function () {
      for (var _len39 = arguments.length, args = new Array(_len39), _key40 = 0; _key40 < _len39; _key40++) {
        args[_key40] = arguments[_key40];
      }
      typecheck_args('==', args, 'number');
      return seq_compare(function (a, b) {
        return LNumber(a).cmp(b) === 0;
      }, args);
    }, "(== x1 x2 ...)\n\n        Function that compares its numerical arguments and checks if they are\n        all equal."),
    // ------------------------------------------------------------------
    '>': doc('>', function () {
      for (var _len40 = arguments.length, args = new Array(_len40), _key41 = 0; _key41 < _len40; _key41++) {
        args[_key41] = arguments[_key41];
      }
      typecheck_args('>', args, 'number');
      return seq_compare(function (a, b) {
        return LNumber(a).cmp(b) === 1;
      }, args);
    }, "(> x1 x2 x3 ...)\n\n        Function that compares its numerical arguments and checks if they are\n        monotonically decreasing, i.e. x1 > x2 and x2 > x3 and so on."),
    // ------------------------------------------------------------------
    '<': doc('<', function () {
      for (var _len41 = arguments.length, args = new Array(_len41), _key42 = 0; _key42 < _len41; _key42++) {
        args[_key42] = arguments[_key42];
      }
      typecheck_args('<', args, 'number');
      return seq_compare(function (a, b) {
        return LNumber(a).cmp(b) === -1;
      }, args);
    }, "(< x1 x2 ...)\n\n        Function that compares its numerical arguments and checks if they are\n        monotonically increasing, i.e. x1 < x2 and x2 < x3 and so on."),
    // ------------------------------------------------------------------
    '<=': doc('<=', function () {
      for (var _len42 = arguments.length, args = new Array(_len42), _key43 = 0; _key43 < _len42; _key43++) {
        args[_key43] = arguments[_key43];
      }
      typecheck_args('<=', args, 'number');
      return seq_compare(function (a, b) {
        return [0, -1].includes(LNumber(a).cmp(b));
      }, args);
    }, "(<= x1 x2 ...)\n\n        Function that compares its numerical arguments and checks if they are\n        monotonically nondecreasing, i.e. x1 <= x2 and x2 <= x3 and so on."),
    // ------------------------------------------------------------------
    '>=': doc('>=', function () {
      for (var _len43 = arguments.length, args = new Array(_len43), _key44 = 0; _key44 < _len43; _key44++) {
        args[_key44] = arguments[_key44];
      }
      typecheck_args('>=', args, 'number');
      return seq_compare(function (a, b) {
        return [0, 1].includes(LNumber(a).cmp(b));
      }, args);
    }, "(>= x1 x2 ...)\n\n        Function that compares its numerical arguments and checks if they are\n        monotonically nonincreasing, i.e. x1 >= x2 and x2 >= x3 and so on."),
    // ------------------------------------------------------------------
    'eq?': doc('eq?', equal, "(eq? a b)\n\n         Function that compares two values if they are identical."),
    // ------------------------------------------------------------------
    or: doc(new Macro('or', function (code, _ref41) {
      var use_dynamic = _ref41.use_dynamic,
        error = _ref41.error;
      var args = global_env.get('list->array')(code);
      var self = this;
      var dynamic_env = self;
      if (!args.length) {
        return false;
      }
      var result;
      return function loop() {
        function next(value) {
          result = value;
          if (result !== false) {
            return result;
          } else {
            return loop();
          }
        }
        if (!args.length) {
          if (result !== false) {
            return result;
          } else {
            return false;
          }
        } else {
          var arg = args.shift();
          var value = _evaluate(arg, {
            env: self,
            dynamic_env: dynamic_env,
            use_dynamic: use_dynamic,
            error: error
          });
          return unpromise(value, next);
        }
      }();
    }), "(or . expressions)\n\n         Macro that executes the values one by one and returns the first that is\n         a truthy value. If there are no expressions that evaluate to true it\n         returns false."),
    // ------------------------------------------------------------------
    and: doc(new Macro('and', function (code) {
      var _ref42 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
        use_dynamic = _ref42.use_dynamic,
        error = _ref42.error;
      var args = global_env.get('list->array')(code);
      var self = this;
      var dynamic_env = self;
      if (!args.length) {
        return true;
      }
      var result;
      var eval_args = {
        env: self,
        dynamic_env: dynamic_env,
        use_dynamic: use_dynamic,
        error: error
      };
      return function loop() {
        function next(value) {
          result = value;
          if (result === false) {
            return false;
          } else {
            return loop();
          }
        }
        if (!args.length) {
          if (result !== false) {
            return result;
          } else {
            return false;
          }
        } else {
          var arg = args.shift();
          return unpromise(_evaluate(arg, eval_args), next);
        }
      }();
    }), "(and . expressions)\n\n         Macro that evaluates each expression in sequence and if any value returns false\n         it will stop and return false. If each value returns true it will return the\n         last value. If it's called without arguments it will return true."),
    // bit operations
    '|': doc('|', function (a, b) {
      return LNumber(a).or(b);
    }, "(| a b)\n\n        Function that calculates the bitwise or operation."),
    '&': doc('&', function (a, b) {
      return LNumber(a).and(b);
    }, "(& a b)\n\n        Function that calculates the bitwise and operation."),
    '~': doc('~', function (a) {
      return LNumber(a).neg();
    }, "(~ number)\n\n        Function that calculates the bitwise inverse (flip all the bits)."),
    '>>': doc('>>', function (a, b) {
      return LNumber(a).shr(b);
    }, "(>> a b)\n\n        Function that right shifts the value a by value b bits."),
    '<<': doc('<<', function (a, b) {
      return LNumber(a).shl(b);
    }, "(<< a b)\n\n        Function that left shifts the value a by value b bits."),
    not: doc('not', function not(value) {
      if (is_null(value)) {
        return true;
      }
      return !value;
    }, "(not object)\n\n        Function that returns the Boolean negation of its argument.")
  }, undefined, 'global');
  var user_env = global_env.inherit('user-env');
  // -------------------------------------------------------------------------
  function set_interaction_env(interaction, internal) {
    interaction.constant('**internal-env**', internal);
    interaction.doc('**internal-env**', "**internal-env**\n\n         Constant used to hide stdin, stdout and stderr so they don't interfere\n         with variables with the same name. Constants are an internal type\n         of variable that can't be redefined, defining a variable with the same name\n         will throw an error.");
    global_env.set('**interaction-environment**', interaction);
  }
  // -------------------------------------------------------------------------
  set_interaction_env(user_env, internal_env);
  global_env.doc('**interaction-environment**', "**interaction-environment**\n\n    Internal dynamic, global variable used to find interpreter environment.\n    It's used so the read and write functions can locate **internal-env**\n    that contains the references to stdin, stdout and stderr.");
  // -------------------------------------------------------------------------
  (function () {
    var map = {
      ceil: 'ceiling'
    };
    ['floor', 'round', 'ceil'].forEach(function (fn) {
      var name = map[fn] ? map[fn] : fn;
      global_env.set(name, doc(name, function (value) {
        typecheck(name, value, 'number');
        if (value instanceof LNumber) {
          return value[fn]();
        }
      }, "(".concat(name, " number)\n\n            Function that calculates the ").concat(name, " of a number.")));
    });
  })();
  // -------------------------------------------------------------------------
  // ref: https://stackoverflow.com/a/4331218/387194
  function allPossibleCases(arr) {
    if (arr.length === 1) {
      return arr[0];
    } else {
      var result = [];
      // recur with the rest of array
      var allCasesOfRest = allPossibleCases(arr.slice(1));
      for (var i = 0; i < allCasesOfRest.length; i++) {
        for (var j = 0; j < arr[0].length; j++) {
          result.push(arr[0][j] + allCasesOfRest[i]);
        }
      }
      return result;
    }
  }

  // -------------------------------------------------------------------------
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
  }

  // -------------------------------------------------------------------------
  // cadr caddr cadadr etc.
  combinations(['d', 'a'], 2, 5).forEach(function (spec) {
    var s = spec.split('');
    var chars = s.slice().reverse();
    var code = s.map(function (c) {
      return "(c".concat(c, "r");
    }).join(' ') + ' arg' + ')'.repeat(s.length);
    var name = 'c' + spec + 'r';
    global_env.set(name, doc(name, function (arg) {
      return chars.reduce(function (list, type) {
        typecheck(name, list, 'pair');
        if (type === 'a') {
          return list.car;
        } else {
          return list.cdr;
        }
      }, arg);
    }, "(".concat(name, " arg)\n\n        Function that calculates ").concat(code)));
  });
  // -----------------------------------------------------------------------------
  function reversseFind(dir, fn) {
    var parts = dir.split(path$2.sep).filter(Boolean);
    for (var i = parts.length; i--;) {
      var _path;
      var p = (_path = path$2).join.apply(_path, ['/'].concat(_toConsumableArray(parts.slice(0, i + 1))));
      if (fn(p)) {
        return p;
      }
    }
  }

  // -----------------------------------------------------------------------------
  function nodeModuleFind(dir) {
    return reversseFind(dir, function (dir) {
      return fs.existsSync(path$2.join(dir, 'node_modules'));
    });
  }

  // -------------------------------------------------------------------------
  function is_node() {
    return typeof global !== 'undefined' && global.global === global;
  }

  // -------------------------------------------------------------------------
  function node_specific() {
    return _node_specific.apply(this, arguments);
  } // -------------------------------------------------------------------------
  /* c8 ignore next 11 */
  function _node_specific() {
    _node_specific = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee23() {
      var _yield$import, createRequire, moduleURL, __dirname, __filename;
      return _regeneratorRuntime.wrap(function _callee23$(_context23) {
        while (1) {
          switch (_context23.prev = _context23.next) {
            case 0:
              _context23.next = 2;
              return Promise.resolve().then(function () { return empty$1; });
            case 2:
              _yield$import = _context23.sent;
              createRequire = _yield$import.createRequire;
              nodeRequire = createRequire((typeof document === 'undefined' && typeof location === 'undefined' ? new (require('u' + 'rl').URL)('file:' + __filename).href : typeof document === 'undefined' ? location.href : (document.currentScript && document.currentScript.src || new URL('lips.js', document.baseURI).href)));
              _context23.next = 7;
              return Promise.resolve().then(function () { return empty$1; });
            case 7:
              fs = _context23.sent;
              _context23.next = 10;
              return Promise.resolve().then(function () { return path$1; });
            case 10:
              path$2 = _context23.sent;
              global_env.set('global', global);
              global_env.set('self', global);
              global_env.set('window', undefined);
              moduleURL = new URL((typeof document === 'undefined' && typeof location === 'undefined' ? new (require('u' + 'rl').URL)('file:' + __filename).href : typeof document === 'undefined' ? location.href : (document.currentScript && document.currentScript.src || new URL('lips.js', document.baseURI).href)));
              __dirname = path$2.dirname(moduleURL.pathname);
              __filename = path$2.basename(moduleURL.pathname);
              global_env.set('__dirname', __dirname);
              global_env.set('__filename', __filename);
              // ---------------------------------------------------------------------
              global_env.set('require.resolve', doc('require.resolve', function (path) {
                typecheck('require.resolve', path, 'string');
                var name = path.valueOf();
                return nodeRequire.resolve(name);
              }, "(require.resolve path)\n\n        Returns the path relative to the current module.\n\n        Only available when LIPS is running under Node.js."));
              // ---------------------------------------------------------------------
              global_env.set('require', doc('require', function (module) {
                typecheck('require', module, 'string');
                module = module.valueOf();
                var root = process.cwd();
                var value;
                try {
                  if (module.match(/^\s*\./)) {
                    value = nodeRequire(path$2.join(root, module));
                  } else {
                    var dir = nodeModuleFind(root);
                    if (dir) {
                      value = nodeRequire(path$2.join(dir, 'node_modules', module));
                    } else {
                      value = nodeRequire(module);
                    }
                  }
                } catch (e) {
                  value = nodeRequire(module);
                }
                return patch_value(value, global);
              }, "(require module)\n\n        Function used inside Node.js to import a module."));
            case 21:
            case "end":
              return _context23.stop();
          }
        }
      }, _callee23);
    }));
    return _node_specific.apply(this, arguments);
  }
  if (is_node()) {
    node_specific();
  } else if (typeof window !== 'undefined' && window === root) {
    global_env.set('window', window);
    global_env.set('global', undefined);
    global_env.set('self', window);
  } else if (typeof self !== 'undefined' && typeof WorkerGlobalScope !== 'undefined') {
    global_env.set('self', self);
    global_env.set('window', undefined);
    global_env.set('global', undefined);
  }
  // -------------------------------------------------------------------------
  function typeErrorMessage(fn, got, expected) {
    var position = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
    var postfix = fn ? " in expression `".concat(fn, "`") : '';
    if (position !== null) {
      postfix += " (argument ".concat(position, ")");
    }
    if (is_function(expected)) {
      return "Invalid type: got ".concat(got).concat(postfix);
    }
    if (expected instanceof Array) {
      if (expected.length === 1) {
        var first = expected[0].toLowerCase();
        expected = 'a' + ('aeiou'.includes(first) ? 'n ' : ' ') + expected[0];
      } else {
        var last = expected[expected.length - 1];
        expected = expected.slice(0, -1).join(', ') + ' or ' + last;
      }
    }
    return "Expecting ".concat(expected, ", got ").concat(got).concat(postfix);
  }
  // -------------------------------------------------------------------------
  function typecheck_args(fn, args, expected) {
    args.forEach(function (arg, i) {
      typecheck(fn, arg, expected, i + 1);
    });
  }
  // -------------------------------------------------------------------------
  function typecheck_text_port(fn, arg, type) {
    typecheck(fn, arg, type);
    if (arg.__type__ === binary_port) {
      throw new Error(typeErrorMessage(fn, 'binary-port', 'textual-port'));
    }
  }
  // -------------------------------------------------------------------------
  function typecheck(fn, arg, expected) {
    var position = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : null;
    fn = fn.valueOf();
    var arg_type = type(arg).toLowerCase();
    if (is_function(expected)) {
      if (!expected(arg)) {
        throw new Error(typeErrorMessage(fn, arg_type, expected, position));
      }
      return;
    }
    var match = false;
    if (expected instanceof Pair) {
      expected = expected.to_array();
    }
    if (expected instanceof Array) {
      expected = expected.map(function (x) {
        return x.valueOf();
      });
    }
    if (expected instanceof Array) {
      expected = expected.map(function (x) {
        return x.valueOf().toLowerCase();
      });
      if (expected.includes(arg_type)) {
        match = true;
      }
    } else {
      expected = expected.valueOf().toLowerCase();
    }
    if (!match && arg_type !== expected) {
      throw new Error(typeErrorMessage(fn, arg_type, expected, position));
    }
  }
  // -------------------------------------------------------------------------
  function self_evaluated(obj) {
    var type = _typeof(obj);
    return ['string', 'function'].includes(type) || _typeof(obj) === 'symbol' || obj instanceof QuotedPromise || obj instanceof LSymbol || obj instanceof LNumber || obj instanceof LString || obj instanceof RegExp;
  }
  // -------------------------------------------------------------------------
  function is_native(obj) {
    return obj instanceof LNumber || obj instanceof LString || obj instanceof LCharacter;
  }
  // -------------------------------------------------------------------------
  function has_own_symbol(obj, symbol) {
    if (obj === null) {
      return false;
    }
    return _typeof(obj) === 'object' && symbol in Object.getOwnPropertySymbols(obj);
  }
  // -------------------------------------------------------------------------
  function is_iterator(obj, symbol) {
    if (has_own_symbol(obj, symbol) || has_own_symbol(obj.__proto__, symbol)) {
      return is_function(obj[symbol]);
    }
  }

  // -------------------------------------------------------------------------
  function memoize(fn) {
    var memo = new WeakMap();
    return function (arg) {
      var result = memo.get(arg);
      if (!result) {
        result = fn(arg);
      }
      return result;
    };
  }
  // -------------------------------------------------------------------------
  /* eslint-disable no-func-assign */
  type = memoize(type);
  /* eslint-enable no-func-assign */
  // -------------------------------------------------------------------------
  function type(obj) {
    var t = type_constants.get(obj);
    if (t) {
      return t;
    }
    if (_typeof(obj) === 'object') {
      for (var _i5 = 0, _Object$entries2 = Object.entries(type_mapping); _i5 < _Object$entries2.length; _i5++) {
        var _Object$entries2$_i = _slicedToArray(_Object$entries2[_i5], 2),
          _key45 = _Object$entries2$_i[0],
          value = _Object$entries2$_i[1];
        if (obj instanceof value) {
          return _key45;
        }
      }
      if (obj.__instance__) {
        obj.__instance__ = false;
        if (obj.__instance__) {
          if (is_function(obj.toType)) {
            return obj.toType();
          }
          if (is_debug()) {
            obj.__instance__;
          }
          return 'instance';
        }
      }
      if (obj.constructor) {
        if (obj.constructor.__class__) {
          return obj.constructor.__class__;
        }
        if (obj.constructor === Object) {
          if (is_iterator(obj, Symbol.iterator)) {
            return 'iterator';
          }
          if (is_iterator(obj, Symbol.asyncIterator)) {
            return 'async-iterator';
          }
        }
        if (obj.constructor.name === '') {
          return 'object';
        }
        return obj.constructor.name.toLowerCase();
      }
    }
    return _typeof(obj);
  }
  // -------------------------------------------------------------------------
  // :; wrap tree of Promises with single Promise or return argument as is
  // :: if tree have no Promises
  // -------------------------------------------------------------------------
  function resolve_promises(arg) {
    var promises = [];
    traverse(arg);
    if (promises.length) {
      return resolve(arg);
    }
    return arg;
    function traverse(node) {
      if (is_promise(node)) {
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
    function promise(_x18) {
      return _promise.apply(this, arguments);
    }
    function _promise() {
      _promise = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee20(node) {
        var pair;
        return _regeneratorRuntime.wrap(function _callee20$(_context20) {
          while (1) {
            switch (_context20.prev = _context20.next) {
              case 0:
                _context20.t0 = Pair;
                if (!node.haveCycles('car')) {
                  _context20.next = 5;
                  break;
                }
                _context20.t1 = node.car;
                _context20.next = 8;
                break;
              case 5:
                _context20.next = 7;
                return resolve(node.car);
              case 7:
                _context20.t1 = _context20.sent;
              case 8:
                _context20.t2 = _context20.t1;
                if (!node.haveCycles('cdr')) {
                  _context20.next = 13;
                  break;
                }
                _context20.t3 = node.cdr;
                _context20.next = 16;
                break;
              case 13:
                _context20.next = 15;
                return resolve(node.cdr);
              case 15:
                _context20.t3 = _context20.sent;
              case 16:
                _context20.t4 = _context20.t3;
                pair = new _context20.t0(_context20.t2, _context20.t4);
                if (node[__data__]) {
                  pair[__data__] = true;
                }
                return _context20.abrupt("return", pair);
              case 20:
              case "end":
                return _context20.stop();
            }
          }
        }, _callee20);
      }));
      return _promise.apply(this, arguments);
    }
    function resolve(node) {
      if (node instanceof Array) {
        return promise_all(node.map(resolve));
      }
      if (node instanceof Pair && promises.length) {
        return promise(node);
      }
      return node;
    }
  }
  // -------------------------------------------------------------------------
  function evaluate_args(rest, _ref43) {
    var use_dynamic = _ref43.use_dynamic,
      options = _objectWithoutProperties(_ref43, _excluded5);
    var args = [];
    var node = rest;
    markCycles(node);
    function next() {
      return args;
    }
    return function loop() {
      if (node instanceof Pair) {
        var arg = _evaluate(node.car, _objectSpread({
          use_dynamic: use_dynamic
        }, options));
        if (use_dynamic) {
          // NOTE: why native function need bind to env?
          arg = unpromise(arg, function (arg) {
            if (is_native_function(arg)) {
              return arg.bind(dynamic_env);
            }
            return arg;
          });
        }
        return unpromise(resolve_promises(arg), function (arg) {
          args.push(arg);
          if (node.haveCycles('cdr')) {
            return next();
          }
          node = node.cdr;
          return loop();
        });
      } else if (node === _nil) {
        return next();
      } else {
        throw new Error('Syntax Error: improper list found in apply');
      }
    }();
  }
  // -------------------------------------------------------------------------
  function evaluate_syntax(macro, code, eval_args) {
    var value = macro.invoke(code, eval_args);
    return unpromise(resolve_promises(value), function (value) {
      if (value instanceof Pair) {
        value.markCycles();
      }
      return quote(value);
    });
  }
  // -------------------------------------------------------------------------
  function evaluate_macro(macro, code, eval_args) {
    function finalize(result) {
      if (result instanceof Pair) {
        result.markCycles();
        return result;
      }
      return quote(result);
    }
    var value = macro.invoke(code, eval_args);
    return unpromise(resolve_promises(value), function ret(value) {
      if (!value || value && value[__data__] || self_evaluated(value)) {
        return value;
      } else {
        return unpromise(_evaluate(value, eval_args), finalize);
      }
    });
  }
  // -------------------------------------------------------------------------
  function prepare_fn_args(fn, args) {
    if (is_bound(fn) && !is_object_bound(fn) && (!lips_context(fn) || is_port_method(fn))) {
      args = args.map(unbox);
    }
    if (!is_raw_lambda(fn) && args.some(is_lips_function) && !is_lips_function(fn) && !is_array_method(fn)) {
      // we unbox values from callback functions #76
      // calling map on array should not unbox the value
      var result = [],
        i = args.length;
      var _loop4 = function _loop4() {
        var arg = args[i];
        if (is_lips_function(arg)) {
          wrapper = function wrapper() {
            for (var _len44 = arguments.length, args = new Array(_len44), _key46 = 0; _key46 < _len44; _key46++) {
              args[_key46] = arguments[_key46];
            }
            return unpromise(arg.apply(this, args), unbox);
          }; // make wrapper work like output of bind
          hidden_prop(wrapper, '__bound__', true);
          hidden_prop(wrapper, '__fn__', arg);
          // copy prototype from function to wrapper
          // so this work when calling new from JavaScript
          // case of Preact that pass LIPS class as argument
          // to h function
          wrapper.prototype = arg.prototype;
          result[i] = wrapper;
        } else {
          result[i] = arg;
        }
      };
      while (i--) {
        var wrapper;
        _loop4();
      }
      args = result;
    }
    return args;
  }
  function call_function(fn, args) {
    var _ref44 = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {},
      env = _ref44.env,
      dynamic_env = _ref44.dynamic_env,
      use_dynamic = _ref44.use_dynamic;
    var scope = env === null || env === void 0 ? void 0 : env.new_frame(fn, args);
    var dynamic_scope = dynamic_env === null || dynamic_env === void 0 ? void 0 : dynamic_env.new_frame(fn, args);
    var context = new LambdaContext({
      env: scope,
      use_dynamic: use_dynamic,
      dynamic_env: dynamic_scope
    });
    return resolve_promises(fn.apply(context, args));
  }

  // -------------------------------------------------------------------------
  function apply(fn, args) {
    var _ref45 = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : {},
      env = _ref45.env,
      dynamic_env = _ref45.dynamic_env,
      use_dynamic = _ref45.use_dynamic,
      _ref45$error = _ref45.error,
      error = _ref45$error === void 0 ? function () {} : _ref45$error;
    args = evaluate_args(args, {
      env: env,
      dynamic_env: dynamic_env,
      error: error,
      use_dynamic: use_dynamic
    });
    return unpromise(args, function (args) {
      if (is_raw_lambda(fn)) {
        // lambda need environment as context
        // normal functions are bound to their contexts
        fn = unbind(fn);
      }
      args = prepare_fn_args(fn, args);
      var _args = args.slice();
      var result = call_function(fn, _args, {
        env: env,
        dynamic_env: dynamic_env,
        use_dynamic: use_dynamic
      });
      return unpromise(result, function (result) {
        if (result instanceof Pair) {
          result.markCycles();
          return quote(result);
        }
        return box(result);
      }, error);
    });
  }
  // -------------------------------------------------------------------------
  // :: Parameters for make-parameter and parametrize
  // -------------------------------------------------------------------------
  var _p_name__ = /*#__PURE__*/new WeakMap();
  var Parameter = /*#__PURE__*/function () {
    function Parameter(init) {
      var fn = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;
      var name = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : null;
      _classCallCheck(this, Parameter);
      _defineProperty(this, "__value__", void 0);
      _defineProperty(this, "__fn__", void 0);
      _classPrivateFieldInitSpec(this, _p_name__, {
        writable: true,
        value: void 0
      });
      this.__value__ = init;
      if (fn) {
        if (!is_function(fn)) {
          throw new Error('Section argument to Parameter need to be function ' + "".concat(type(fn), " given"));
        }
        this.__fn__ = fn;
      }
      if (name) {
        _classPrivateFieldSet(this, _p_name__, name);
      }
    }
    _createClass(Parameter, [{
      key: "__name__",
      get: function get() {
        return _classPrivateFieldGet(this, _p_name__);
      },
      set: function set(name) {
        _classPrivateFieldSet(this, _p_name__, name);
        if (this.__fn__) {
          this.__fn__.__name__ = "fn-".concat(name);
        }
      }
    }, {
      key: "invoke",
      value: function invoke() {
        if (is_function(this.__fn__)) {
          return this.__fn__(this.__value__);
        }
        return this.__value__;
      }
    }, {
      key: "inherit",
      value: function inherit(value) {
        return new Parameter(value, this.__fn__, this.__name__);
      }
    }]);
    return Parameter;
  }(); // -------------------------------------------------------------------------
  var LambdaContext = /*#__PURE__*/function () {
    function LambdaContext(payload) {
      _classCallCheck(this, LambdaContext);
      _defineProperty(this, "env", void 0);
      _defineProperty(this, "dynamic_env", void 0);
      _defineProperty(this, "use_dynamic", void 0);
      Object.assign(this, payload);
    }
    _createClass(LambdaContext, [{
      key: "__name__",
      get: function get() {
        return this.env.__name__;
      }
    }, {
      key: "__parent__",
      get: function get() {
        return this.env.__parent__;
      }
    }, {
      key: "get",
      value: function get() {
        var _this$env;
        return (_this$env = this.env).get.apply(_this$env, arguments);
      }
    }]);
    return LambdaContext;
  }(); // -------------------------------------------------------------------------
  function search_param(env, param) {
    var candidate = env.get(param.__name__, {
      throwError: false
    });
    if (is_parameter(candidate) && candidate !== param) {
      return candidate;
    }
    var top_env = user_env.get('**interaction-environment**');
    while (true) {
      var parent = env.get('parent.frame', {
        throwError: false
      });
      env = parent(0);
      if (env === top_env) {
        break;
      }
      candidate = env.get(param.__name__, {
        throwError: false
      });
      if (is_parameter(candidate) && candidate !== param) {
        return candidate;
      }
    }
    return param;
  }

  // -------------------------------------------------------------------------
  // :: Continuations object from call/cc
  // -------------------------------------------------------------------------
  var Continuation = /*#__PURE__*/function () {
    function Continuation(k) {
      _classCallCheck(this, Continuation);
      _defineProperty(this, "__value__", void 0);
      this.__value__ = k;
    }
    _createClass(Continuation, [{
      key: "invoke",
      value: function invoke() {
        if (this.__value__ === null) {
          throw new Error('Continuations are not implemented yet');
        }
      }
    }]);
    return Continuation;
  }(); // -------------------------------------------------------------------------
  var noop = function noop() {};
  // -------------------------------------------------------------------------
  function _evaluate(code) {
    var _ref46 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
      env = _ref46.env,
      dynamic_env = _ref46.dynamic_env,
      use_dynamic = _ref46.use_dynamic,
      _ref46$error = _ref46.error,
      error = _ref46$error === void 0 ? noop : _ref46$error,
      rest = _objectWithoutProperties(_ref46, _excluded6);
    return function (rest) {
      try {
        if (!is_env(dynamic_env)) {
          dynamic_env = env === true ? user_env : env || user_env;
        }
        if (use_dynamic) {
          env = dynamic_env;
        } else if (env === true) {
          env = user_env;
        } else {
          env = env || global_env;
        }
        var eval_args = {
          env: env,
          dynamic_env: dynamic_env,
          use_dynamic: use_dynamic,
          error: error
        };
        var value;
        if (is_null(code)) {
          return code;
        }
        if (code instanceof LSymbol) {
          return env.get(code);
        }
        if (!(code instanceof Pair)) {
          return code;
        }
        var first = code.car;
        var rest = code.cdr;
        if (first instanceof Pair) {
          value = resolve_promises(_evaluate(first, eval_args));
          if (is_promise(value)) {
            return value.then(function (value) {
              if (!is_callable(value)) {
                throw new Error(type(value) + ' ' + env.get('repr')(value) + ' is not callable while evaluating ' + code.toString());
              }
              return _evaluate(new Pair(value, code.cdr), eval_args);
            });
            // else is later in code
          } else if (!is_callable(value)) {
            throw new Error(type(value) + ' ' + env.get('repr')(value) + ' is not callable while evaluating ' + code.toString());
          }
        }
        if (first instanceof LSymbol) {
          value = env.get(first);
        } else if (is_function(first)) {
          value = first;
        }
        var result;
        if (value instanceof Syntax) {
          result = evaluate_syntax(value, code, eval_args);
        } else if (value instanceof Macro) {
          result = evaluate_macro(value, rest, eval_args);
        } else if (is_function(value)) {
          result = apply(value, rest, eval_args);
        } else if (is_parameter(value)) {
          var param = search_param(dynamic_env, value);
          if (is_null(code.cdr)) {
            result = param.invoke();
          } else {
            return unpromise(_evaluate(code.cdr.car, eval_args), function (value) {
              param.__value__ = value;
            });
          }
        } else if (is_continuation(value)) {
          result = value.invoke();
        } else if (code instanceof Pair) {
          value = first && first.toString();
          throw new Error("".concat(type(first), " ").concat(value, " is not a function"));
        } else {
          return code;
        }
        // escape promise feature #54
        var __promise__ = env.get(Symbol["for"]('__promise__'), {
          throwError: false
        });
        if (__promise__ === true && is_promise(result)) {
          // fix #139 evaluate the code inside the promise that is not data.
          // When promise is not quoted it happen automatically, when returning
          // promise from evaluate.
          result = result.then(function (result) {
            if (result instanceof Pair && !value[__data__]) {
              return _evaluate(result, eval_args);
            }
            return result;
          });
          return new QuotedPromise(result);
        }
        return result;
      } catch (e) {
        error && error.call(env, e, code);
      }
    }(rest);
  }
  // -------------------------------------------------------------------------
  var compile = exec_collect(function (code) {
    return code;
  });
  // -------------------------------------------------------------------------
  var exec = exec_collect(function (code, value) {
    return value;
  });
  // -------------------------------------------------------------------------
  function exec_collect(collect_callback) {
    return /*#__PURE__*/function () {
      var _exec_lambda = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime.mark(function _callee21(arg) {
        var _ref47,
          env,
          dynamic_env,
          use_dynamic,
          results,
          input,
          _iteratorAbruptCompletion3,
          _didIteratorError3,
          _iteratorError3,
          _iterator3,
          _step3,
          code,
          value,
          _args24 = arguments;
        return _regeneratorRuntime.wrap(function _callee21$(_context21) {
          while (1) {
            switch (_context21.prev = _context21.next) {
              case 0:
                _ref47 = _args24.length > 1 && _args24[1] !== undefined ? _args24[1] : {}, env = _ref47.env, dynamic_env = _ref47.dynamic_env, use_dynamic = _ref47.use_dynamic;
                if (!is_env(dynamic_env)) {
                  dynamic_env = env === true ? user_env : env || user_env;
                }
                if (env === true) {
                  env = user_env;
                } else {
                  env = env || user_env;
                }
                results = [];
                input = Array.isArray(arg) ? arg : parse(arg);
                _iteratorAbruptCompletion3 = false;
                _didIteratorError3 = false;
                _context21.prev = 7;
                _iterator3 = _asyncIterator(input);
              case 9:
                _context21.next = 11;
                return _iterator3.next();
              case 11:
                if (!(_iteratorAbruptCompletion3 = !(_step3 = _context21.sent).done)) {
                  _context21.next = 25;
                  break;
                }
                code = _step3.value;
                value = _evaluate(code, {
                  env: env,
                  dynamic_env: dynamic_env,
                  use_dynamic: use_dynamic,
                  error: function error(e, code) {
                    if (e && e.message) {
                      if (e.message.match(/^Error:/)) {
                        var re = /^(Error:)\s*([^:]+:\s*)/;
                        // clean duplicated Error: added by JS
                        e.message = e.message.replace(re, '$1 $2');
                      }
                      if (code) {
                        // LIPS stack trace
                        if (!(e.__code__ instanceof Array)) {
                          e.__code__ = [];
                        }
                        e.__code__.push(code.toString(true));
                      }
                    }
                    throw e;
                  }
                });
                _context21.t0 = results;
                _context21.t1 = collect_callback;
                _context21.t2 = code;
                _context21.next = 19;
                return value;
              case 19:
                _context21.t3 = _context21.sent;
                _context21.t4 = (0, _context21.t1)(_context21.t2, _context21.t3);
                _context21.t0.push.call(_context21.t0, _context21.t4);
              case 22:
                _iteratorAbruptCompletion3 = false;
                _context21.next = 9;
                break;
              case 25:
                _context21.next = 31;
                break;
              case 27:
                _context21.prev = 27;
                _context21.t5 = _context21["catch"](7);
                _didIteratorError3 = true;
                _iteratorError3 = _context21.t5;
              case 31:
                _context21.prev = 31;
                _context21.prev = 32;
                if (!(_iteratorAbruptCompletion3 && _iterator3["return"] != null)) {
                  _context21.next = 36;
                  break;
                }
                _context21.next = 36;
                return _iterator3["return"]();
              case 36:
                _context21.prev = 36;
                if (!_didIteratorError3) {
                  _context21.next = 39;
                  break;
                }
                throw _iteratorError3;
              case 39:
                return _context21.finish(36);
              case 40:
                return _context21.finish(31);
              case 41:
                return _context21.abrupt("return", results);
              case 42:
              case "end":
                return _context21.stop();
            }
          }
        }, _callee21, null, [[7, 27, 31, 41], [32,, 36, 40]]);
      }));
      function exec_lambda(_x19) {
        return _exec_lambda.apply(this, arguments);
      }
      return exec_lambda;
    }();
  }
  // -------------------------------------------------------------------------
  function balanced(code) {
    var maching_pairs = {
      '[': ']',
      '(': ')'
    };
    var tokens;
    if (typeof code === 'string') {
      tokens = tokenize(code);
    } else {
      tokens = code.map(function (x) {
        return x && x.token ? x.token : x;
      });
    }
    var open_tokens = Object.keys(maching_pairs);
    var brackets = Object.values(maching_pairs).concat(open_tokens);
    tokens = tokens.filter(function (token) {
      return brackets.includes(token);
    });
    var stack = new Stack();
    var _iterator12 = _createForOfIteratorHelper(tokens),
      _step12;
    try {
      for (_iterator12.s(); !(_step12 = _iterator12.n()).done;) {
        var token = _step12.value;
        if (open_tokens.includes(token)) {
          stack.push(token);
        } else if (!stack.is_empty()) {
          // closing token
          var last = stack.top();
          // last on stack need to match
          var closing_token = maching_pairs[last];
          if (token === closing_token) {
            stack.pop();
          } else {
            throw new Error("Syntax error: missing closing ".concat(closing_token));
          }
        } else {
          // closing bracket without opening
          throw new Error("Syntax error: not matched closing ".concat(token));
        }
      }
    } catch (err) {
      _iterator12.e(err);
    } finally {
      _iterator12.f();
    }
    return stack.is_empty();
  }

  // -------------------------------------------------------------------------
  function fworker(fn) {
    // ref: https://stackoverflow.com/a/10372280/387194
    var str = '(' + fn.toString() + ')()';
    var URL = window.URL || window.webkitURL;
    var blob;
    try {
      blob = new Blob([str], {
        type: 'application/javascript'
      });
    } catch (e) {
      // Backwards-compatibility
      var BlobBuilder = window.BlobBuilder || window.WebKitBlobBuilder || window.MozBlobBuilder;
      blob = new BlobBuilder();
      blob.append(str);
      blob = blob.getBlob();
    }
    return new root.Worker(URL.createObjectURL(blob));
  }

  // -------------------------------------------------------------------------
  function is_dev() {
    return lips.version.match(/^(\{\{VER\}\}|DEV)$/);
  }

  // -------------------------------------------------------------------------
  function get_current_script() {
    if (is_node()) {
      return;
    }
    var script;
    if (document.currentScript) {
      script = document.currentScript;
    } else {
      var scripts = document.querySelectorAll('script');
      if (!scripts.length) {
        return;
      }
      script = scripts[scripts.length - 1];
    }
    var url = script.getAttribute('src');
    return url;
  }

  // -------------------------------------------------------------------------
  var current_script = get_current_script();

  // -------------------------------------------------------------------------
  function bootstrap() {
    var url = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
    var std = 'dist/std.xcb';
    if (url === '') {
      if (current_script) {
        url = current_script.replace(/[^/]*$/, 'std.xcb');
      } else if (is_dev()) {
        url = "https://cdn.jsdelivr.net/gh/jcubic/lips@devel/".concat(std);
      } else {
        url = "https://cdn.jsdelivr.net/npm/@jcubic/lips@".concat(lips.version, "/").concat(std);
      }
    }
    var load = global_env.get('load');
    return load.call(user_env, url, global_env);
  }
  // -------------------------------------------------------------------------
  function Worker(url) {
    this.url = url;
    var worker = this.worker = fworker(function () {
      var interpreter;
      var init;
      // string, numbers, booleans
      self.addEventListener('message', function (response) {
        var data = response.data;
        var id = data.id;
        if (data.type !== 'RPC' || id === null) {
          return;
        }
        function send_result(result) {
          self.postMessage({
            id: id,
            type: 'RPC',
            result: result
          });
        }
        function send_error(message) {
          self.postMessage({
            id: id,
            type: 'RPC',
            error: message
          });
        }
        if (data.method === 'eval') {
          if (!init) {
            send_error('Worker RPC: LIPS not initialized, call init first');
            return;
          }
          init.then(function () {
            // we can use ES6 inside function that's converted to blob
            var code = data.params[0];
            var use_dynamic = data.params[1];
            interpreter.exec(code, {
              use_dynamic: use_dynamic
            }).then(function (result) {
              result = result.map(function (value) {
                return value && value.valueOf();
              });
              send_result(result);
            })["catch"](function (error) {
              send_error(error);
            });
          });
        } else if (data.method === 'init') {
          var url = data.params[0];
          if (typeof url !== 'string') {
            send_error('Worker RPC: url is not a string');
          } else {
            importScripts("".concat(url, "/dist/lips.min.js"));
            interpreter = new lips.Interpreter('worker');
            init = bootstrap(url);
            init.then(function () {
              send_result(true);
            });
          }
        }
      });
    });
    this.rpc = function () {
      var id = 0;
      return function rpc(method, params) {
        var _id = ++id;
        return new Promise(function (resolve, reject) {
          worker.addEventListener('message', function handler(response) {
            var data = response.data;
            if (data && data.type === 'RPC' && data.id === _id) {
              if (data.error) {
                reject(data.error);
              } else {
                resolve(data.result);
              }
              worker.removeEventListener('message', handler);
            }
          });
          worker.postMessage({
            type: 'RPC',
            method: method,
            id: _id,
            params: params
          });
        });
      };
    }();
    this.rpc('init', [url])["catch"](function (error) {
      console.error(error);
    });
    this.exec = function (code, _ref48) {
      var _ref48$use_dynamic = _ref48.use_dynamic,
        use_dynamic = _ref48$use_dynamic === void 0 ? false : _ref48$use_dynamic;
      return this.rpc('eval', [code, use_dynamic]);
    };
  }

  // -------------------------------------------------------------------------
  // :: Serialization
  // -------------------------------------------------------------------------
  var serialization_map = {
    'pair': function pair(_ref49) {
      var _ref50 = _slicedToArray(_ref49, 2),
        car = _ref50[0],
        cdr = _ref50[1];
      return Pair(car, cdr);
    },
    'number': function number(value) {
      if (LString.isString(value)) {
        return LNumber([value, 10]);
      }
      return LNumber(value);
    },
    'regex': function regex(_ref51) {
      var _ref52 = _slicedToArray(_ref51, 2),
        pattern = _ref52[0],
        flag = _ref52[1];
      return new RegExp(pattern, flag);
    },
    'nil': function nil() {
      return _nil;
    },
    'symbol': function symbol(value) {
      if (LString.isString(value)) {
        return LSymbol(value);
      } else if (Array.isArray(value)) {
        return LSymbol(Symbol["for"](value[0]));
      }
    },
    'string': LString,
    'character': LCharacter
  };
  // -------------------------------------------------------------------------
  // class mapping to create smaller JSON
  var available_class = Object.keys(serialization_map);
  var class_map = {};
  for (var _i6 = 0, _Object$entries3 = Object.entries(available_class); _i6 < _Object$entries3.length; _i6++) {
    var _Object$entries3$_i = _slicedToArray(_Object$entries3[_i6], 2),
      i = _Object$entries3$_i[0],
      cls = _Object$entries3$_i[1];
    class_map[cls] = +i;
  }
  function mangle_name(name) {
    return class_map[name];
  }
  function resolve_name(i) {
    return available_class[i];
  }
  // -------------------------------------------------------------------------
  function serialize(data) {
    return JSON.stringify(data, function (key, value) {
      var v0 = this[key];
      if (v0) {
        if (v0 instanceof RegExp) {
          return {
            '@': mangle_name('regex'),
            '#': [v0.source, v0.flags]
          };
        }
        var cls = mangle_name(v0.constructor.__class__);
        if (!is_undef(cls)) {
          return {
            '@': cls,
            '#': v0.serialize()
          };
        }
      }
      return value;
    });
  }
  // -------------------------------------------------------------------------
  function unserialize(string) {
    return JSON.parse(string, function (_, object) {
      if (object && _typeof(object) === 'object') {
        if (!is_undef(object['@'])) {
          var cls = resolve_name(object['@']);
          if (serialization_map[cls]) {
            return serialization_map[cls](object['#']);
          }
        }
      }
      return object;
    });
  }

  // -------------------------------------------------------------------------
  // binary serialization using CBOR binary data format
  // -------------------------------------------------------------------------
  var cbor = function () {
    var types = {
      'pair': Pair,
      'symbol': LSymbol,
      'number': LNumber,
      'string': LString,
      'character': LCharacter,
      'nil': _nil.constructor,
      'regex': RegExp
    };
    function serializer(Class, fn) {
      return {
        deserialize: fn,
        Class: Class
      };
    }
    var encoder = new Encoder();
    var cbor_serialization_map = {};
    for (var _i7 = 0, _Object$entries4 = Object.entries(serialization_map); _i7 < _Object$entries4.length; _i7++) {
      var _Object$entries4$_i = _slicedToArray(_Object$entries4[_i7], 2),
        name = _Object$entries4$_i[0],
        fn = _Object$entries4$_i[1];
      var Class = types[name];
      cbor_serialization_map[name] = serializer(Class, fn);
    }
    // add CBOR data mapping
    var tag = 43311;
    Object.keys(cbor_serialization_map).forEach(function (type) {
      var data = cbor_serialization_map[type];
      if (typeof data === 'function') {
        var _Class = data;
        addExtension({
          Class: _Class,
          tag: tag,
          encode: function encode(instance, _encode) {
            _encode(instance.serialize());
          },
          decode: function decode(data) {
            return new _Class(data);
          }
        });
      } else {
        var deserialize = data.deserialize,
          _Class2 = data.Class;
        addExtension({
          Class: _Class2,
          tag: tag,
          encode: function encode(instance, _encode2) {
            if (instance instanceof RegExp) {
              return _encode2([instance.source, instance.flags]);
            }
            _encode2(instance.serialize());
          },
          decode: function decode(data) {
            return deserialize(data);
          }
        });
      }
      tag++;
    });
    return encoder;
  }();

  // -------------------------------------------------------------------------
  function merge_uint8_array() {
    for (var _len45 = arguments.length, args = new Array(_len45), _key47 = 0; _key47 < _len45; _key47++) {
      args[_key47] = arguments[_key47];
    }
    if (args.length > 1) {
      var len = args.reduce(function (acc, arr) {
        return acc + arr.length;
      }, 0);
      var result = new Uint8Array(len);
      var offset = 0;
      args.forEach(function (item) {
        result.set(item, offset);
        offset += item.length;
      });
      return result;
    } else if (args.length) {
      return args[0];
    }
  }

  // -------------------------------------------------------------------------
  function encode_magic() {
    var VERSION = 1;
    var encoder = new TextEncoder('utf-8');
    return encoder.encode("LIPS".concat(VERSION.toString().padStart(3, ' ')));
  }

  // -------------------------------------------------------------------------
  var MAGIC_LENGTH = 7;

  // -------------------------------------------------------------------------
  function decode_magic(obj) {
    var decoder = new TextDecoder('utf-8');
    var prefix = decoder.decode(obj.slice(0, MAGIC_LENGTH));
    var name = prefix.substring(0, 4);
    if (name === 'LIPS') {
      var m = prefix.match(/^(....).*([0-9]+)$/);
      if (m) {
        return {
          type: m[1],
          version: Number(m[2])
        };
      }
    }
    return {
      type: 'unknown'
    };
  }

  // -------------------------------------------------------------------------
  function serialize_bin(obj) {
    var magic = encode_magic();
    var payload = cbor.encode(obj);
    return merge_uint8_array(magic, pack_1(payload, {
      magic: false
    }));
  }

  // -------------------------------------------------------------------------
  function unserialize_bin(data) {
    var _decode_magic = decode_magic(data),
      type = _decode_magic.type,
      version = _decode_magic.version;
    if (type === 'LIPS' && version === 1) {
      var arr = unpack_1(data.slice(MAGIC_LENGTH), {
        magic: false
      });
      return cbor.decode(arr);
    } else {
      throw new Error("Invalid file format ".concat(type));
    }
  }

  // -------------------------------------------------------------------------
  function execError(e) {
    console.error(e.message || e);
    if (e.code) {
      console.error(e.code.map(function (line, i) {
        return "[".concat(i + 1, "]: ").concat(line);
      }));
    }
  }

  // -------------------------------------------------------------------------
  function init() {
    var lips_mimes = ['text/x-lips', 'text/x-scheme'];
    var bootstrapped;
    function load(script) {
      return new Promise(function (resolve) {
        var src = script.getAttribute('src');
        if (src) {
          return fetch(src).then(function (res) {
            return res.text();
          }).then(exec).then(resolve)["catch"](function (e) {
            execError(e);
            resolve();
          });
        } else {
          return exec(script.innerHTML).then(resolve)["catch"](function (e) {
            execError(e);
            resolve();
          });
        }
      });
    }
    function loop() {
      return new Promise(function (resolve) {
        var scripts = Array.from(document.querySelectorAll('script'));
        return function loop() {
          var script = scripts.shift();
          if (!script) {
            resolve();
          } else {
            var type = script.getAttribute('type');
            if (lips_mimes.includes(type)) {
              var bootstrap_attr = script.getAttribute('bootstrap');
              if (!bootstrapped && typeof bootstrap_attr === 'string') {
                return bootstrap(bootstrap_attr).then(function () {
                  return load(script);
                }).then(loop);
              } else {
                return load(script).then(loop);
              }
            } else if (type && type.match(/lips|lisp/)) {
              console.warn('Expecting ' + lips_mimes.join(' or ') + ' found ' + type);
            }
            return loop();
          }
        }();
      });
    }
    if (!window.document) {
      return Promise.resolve();
    } else if (currentScript) {
      var script = currentScript;
      var bootstrap_attr = script.getAttribute('bootstrap');
      if (typeof bootstrap_attr === 'string') {
        return bootstrap(bootstrap_attr).then(function () {
          bootstrapped = true;
          return loop();
        });
      }
    }
    return loop();
  }
  // this can't be in init function, because it need to be in script context
  var currentScript = typeof window !== 'undefined' && window.document && document.currentScript;
  // -------------------------------------------------------------------------
  if (typeof window !== 'undefined') {
    contentLoaded(window, init);
  }
  // -------------------------------------------------------------------------
  var banner = function () {
    // Rollup tree-shaking is removing the variable if it's normal string because
    // obviously 'Mon, 15 Jan 2024 12:39:38 +0000' == '{{' + 'DATE}}'; can be removed
    // but disabling Tree-shaking is adding lot of not used code so we use this
    // hack instead
    var date = LString('Mon, 15 Jan 2024 12:39:38 +0000').valueOf();
    var _date = date === '{{' + 'DATE}}' ? new Date() : new Date(date);
    var _format = function _format(x) {
      return x.toString().padStart(2, '0');
    };
    var _year = _date.getFullYear();
    var _build = [_year, _format(_date.getMonth() + 1), _format(_date.getDate())].join('-');
    var banner = "\n  __ __                          __\n / / \\ \\       _    _  ___  ___  \\ \\\n| |   \\ \\     | |  | || . \\/ __>  | |\n| |    > \\    | |_ | ||  _/\\__ \\  | |\n| |   / ^ \\   |___||_||_|  <___/  | |\n \\_\\ /_/ \\_\\                     /_/\n\nLIPS Interpreter DEV (".concat(_build, ") <https://lips.js.org>\nCopyright (c) 2018-").concat(_year, " Jakub T. Jankiewicz\n\nType (env) to see environment with functions macros and variables. You can also\nuse (help name) to display help for specific function or macro, (apropos name)\nto display list of matched names in environment and (dir object) to list\nproperties of an object.\n").replace(/^.*\n/, '');
    return banner;
  }();
  // -------------------------------------------------------------------------
  // to be used with string function when code is minified
  // -------------------------------------------------------------------------
  read_only(Ahead, '__class__', 'ahead');
  read_only(Pair, '__class__', 'pair');
  read_only(Nil, '__class__', 'nil');
  read_only(Pattern, '__class__', 'pattern');
  read_only(Formatter, '__class__', 'formatter');
  read_only(Macro, '__class__', 'macro');
  read_only(Syntax, '__class__', 'syntax');
  read_only(Environment, '__class__', 'environment');
  read_only(InputPort, '__class__', 'input-port');
  read_only(OutputPort, '__class__', 'output-port');
  read_only(BufferedOutputPort, '__class__', 'output-port');
  read_only(OutputStringPort, '__class__', 'output-string-port');
  read_only(InputStringPort, '__class__', 'input-string-port');
  read_only(InputFilePort, '__class__', 'input-file-port');
  read_only(OutputFilePort, '__class__', 'output-file-port');
  read_only(LipsError, '__class__', 'lips-error');
  [LNumber, LComplex, LRational, LFloat, LBigInteger].forEach(function (cls) {
    read_only(cls, '__class__', 'number');
  });
  read_only(LCharacter, '__class__', 'character');
  read_only(LSymbol, '__class__', 'symbol');
  read_only(LString, '__class__', 'string');
  read_only(QuotedPromise, '__class__', 'promise');
  read_only(Parameter, '__class__', 'parameter');
  // -------------------------------------------------------------------------
  var lips = {
    version: 'DEV',
    banner: banner,
    date: 'Mon, 15 Jan 2024 12:39:38 +0000',
    exec: exec,
    // unwrap async generator into Promise<Array>
    parse: compose(uniterate_async, parse),
    tokenize: tokenize,
    evaluate: _evaluate,
    compile: compile,
    serialize: serialize,
    unserialize: unserialize,
    serialize_bin: serialize_bin,
    unserialize_bin: unserialize_bin,
    bootstrap: bootstrap,
    Environment: Environment,
    env: user_env,
    Worker: Worker,
    Interpreter: Interpreter,
    balanced_parenthesis: balanced,
    balancedParenthesis: balanced,
    balanced: balanced,
    Macro: Macro,
    Syntax: Syntax,
    Pair: Pair,
    Values: Values,
    QuotedPromise: QuotedPromise,
    Error: LipsError,
    quote: quote,
    InputPort: InputPort,
    OutputPort: OutputPort,
    BufferedOutputPort: BufferedOutputPort,
    InputFilePort: InputFilePort,
    OutputFilePort: OutputFilePort,
    InputStringPort: InputStringPort,
    OutputStringPort: OutputStringPort,
    InputByteVectorPort: InputByteVectorPort,
    OutputByteVectorPort: OutputByteVectorPort,
    InputBinaryFilePort: InputBinaryFilePort,
    OutputBinaryFilePort: OutputBinaryFilePort,
    Formatter: Formatter,
    Parser: Parser,
    Lexer: Lexer,
    specials: specials,
    repr: repr,
    nil: _nil,
    eof: eof,
    LSymbol: LSymbol,
    LNumber: LNumber,
    LFloat: LFloat,
    LComplex: LComplex,
    LRational: LRational,
    LBigInteger: LBigInteger,
    LCharacter: LCharacter,
    LString: LString,
    Parameter: Parameter,
    rationalize: rationalize
  };
  global_env.set('lips', lips);

  var empty = {};

  var empty$1 = /*#__PURE__*/Object.freeze({
    __proto__: null,
    'default': empty
  });

  // Copyright Joyent, Inc. and other Node contributors.
  //
  // Permission is hereby granted, free of charge, to any person obtaining a
  // copy of this software and associated documentation files (the
  // "Software"), to deal in the Software without restriction, including
  // without limitation the rights to use, copy, modify, merge, publish,
  // distribute, sublicense, and/or sell copies of the Software, and to permit
  // persons to whom the Software is furnished to do so, subject to the
  // following conditions:
  //
  // The above copyright notice and this permission notice shall be included
  // in all copies or substantial portions of the Software.
  //
  // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  // OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  // MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
  // NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
  // DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  // OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
  // USE OR OTHER DEALINGS IN THE SOFTWARE.

  // resolves . and .. elements in a path array with directory names there
  // must be no slashes, empty elements, or device names (c:\) in the array
  // (so also no leading and trailing slashes - it does not distinguish
  // relative and absolute paths)
  function normalizeArray(parts, allowAboveRoot) {
    // if the path tries to go above the root, `up` ends up > 0
    var up = 0;
    for (var i = parts.length - 1; i >= 0; i--) {
      var last = parts[i];
      if (last === '.') {
        parts.splice(i, 1);
      } else if (last === '..') {
        parts.splice(i, 1);
        up++;
      } else if (up) {
        parts.splice(i, 1);
        up--;
      }
    }

    // if the path is allowed to go above the root, restore leading ..s
    if (allowAboveRoot) {
      for (; up--; up) {
        parts.unshift('..');
      }
    }

    return parts;
  }

  // Split a filename into [root, dir, basename, ext], unix version
  // 'root' is just a slash, or nothing.
  var splitPathRe =
      /^(\/?|)([\s\S]*?)((?:\.{1,2}|[^\/]+?|)(\.[^.\/]*|))(?:[\/]*)$/;
  var splitPath = function(filename) {
    return splitPathRe.exec(filename).slice(1);
  };

  // path.resolve([from ...], to)
  // posix version
  function resolve() {
    var resolvedPath = '',
        resolvedAbsolute = false;

    for (var i = arguments.length - 1; i >= -1 && !resolvedAbsolute; i--) {
      var path = (i >= 0) ? arguments[i] : '/';

      // Skip empty and invalid entries
      if (typeof path !== 'string') {
        throw new TypeError('Arguments to path.resolve must be strings');
      } else if (!path) {
        continue;
      }

      resolvedPath = path + '/' + resolvedPath;
      resolvedAbsolute = path.charAt(0) === '/';
    }

    // At this point the path should be resolved to a full absolute path, but
    // handle relative paths to be safe (might happen when process.cwd() fails)

    // Normalize the path
    resolvedPath = normalizeArray(filter(resolvedPath.split('/'), function(p) {
      return !!p;
    }), !resolvedAbsolute).join('/');

    return ((resolvedAbsolute ? '/' : '') + resolvedPath) || '.';
  }
  // path.normalize(path)
  // posix version
  function normalize(path) {
    var isPathAbsolute = isAbsolute(path),
        trailingSlash = substr(path, -1) === '/';

    // Normalize the path
    path = normalizeArray(filter(path.split('/'), function(p) {
      return !!p;
    }), !isPathAbsolute).join('/');

    if (!path && !isPathAbsolute) {
      path = '.';
    }
    if (path && trailingSlash) {
      path += '/';
    }

    return (isPathAbsolute ? '/' : '') + path;
  }
  // posix version
  function isAbsolute(path) {
    return path.charAt(0) === '/';
  }

  // posix version
  function join() {
    var paths = Array.prototype.slice.call(arguments, 0);
    return normalize(filter(paths, function(p, index) {
      if (typeof p !== 'string') {
        throw new TypeError('Arguments to path.join must be strings');
      }
      return p;
    }).join('/'));
  }


  // path.relative(from, to)
  // posix version
  function relative(from, to) {
    from = resolve(from).substr(1);
    to = resolve(to).substr(1);

    function trim(arr) {
      var start = 0;
      for (; start < arr.length; start++) {
        if (arr[start] !== '') break;
      }

      var end = arr.length - 1;
      for (; end >= 0; end--) {
        if (arr[end] !== '') break;
      }

      if (start > end) return [];
      return arr.slice(start, end - start + 1);
    }

    var fromParts = trim(from.split('/'));
    var toParts = trim(to.split('/'));

    var length = Math.min(fromParts.length, toParts.length);
    var samePartsLength = length;
    for (var i = 0; i < length; i++) {
      if (fromParts[i] !== toParts[i]) {
        samePartsLength = i;
        break;
      }
    }

    var outputParts = [];
    for (var i = samePartsLength; i < fromParts.length; i++) {
      outputParts.push('..');
    }

    outputParts = outputParts.concat(toParts.slice(samePartsLength));

    return outputParts.join('/');
  }

  var sep = '/';
  var delimiter = ':';

  function dirname(path) {
    var result = splitPath(path),
        root = result[0],
        dir = result[1];

    if (!root && !dir) {
      // No dirname whatsoever
      return '.';
    }

    if (dir) {
      // It has a dirname, strip trailing slash
      dir = dir.substr(0, dir.length - 1);
    }

    return root + dir;
  }

  function basename(path, ext) {
    var f = splitPath(path)[2];
    // TODO: make this comparison case-insensitive on windows?
    if (ext && f.substr(-1 * ext.length) === ext) {
      f = f.substr(0, f.length - ext.length);
    }
    return f;
  }


  function extname(path) {
    return splitPath(path)[3];
  }
  var path = {
    extname: extname,
    basename: basename,
    dirname: dirname,
    sep: sep,
    delimiter: delimiter,
    relative: relative,
    join: join,
    isAbsolute: isAbsolute,
    normalize: normalize,
    resolve: resolve
  };
  function filter (xs, f) {
      if (xs.filter) return xs.filter(f);
      var res = [];
      for (var i = 0; i < xs.length; i++) {
          if (f(xs[i], i, xs)) res.push(xs[i]);
      }
      return res;
  }

  // String.prototype.substr - negative index don't work in IE8
  var substr = 'ab'.substr(-1) === 'b' ?
      function (str, start, len) { return str.substr(start, len) } :
      function (str, start, len) {
          if (start < 0) start = str.length + start;
          return str.substr(start, len);
      }
  ;

  var path$1 = /*#__PURE__*/Object.freeze({
    __proto__: null,
    resolve: resolve,
    normalize: normalize,
    isAbsolute: isAbsolute,
    join: join,
    relative: relative,
    sep: sep,
    delimiter: delimiter,
    dirname: dirname,
    basename: basename,
    extname: extname,
    'default': path
  });

  return lips;

}));
