"use strict";

function _toConsumableArray(arr) { return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray(arr) || _nonIterableSpread(); }
function _nonIterableSpread() { throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }
function _iterableToArray(iter) { if (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null || iter["@@iterator"] != null) return Array.from(iter); }
function _arrayWithoutHoles(arr) { if (Array.isArray(arr)) return _arrayLikeToArray(arr); }
function _slicedToArray(arr, i) { return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest(); }
function _nonIterableRest() { throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }
function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }
function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i]; return arr2; }
function _iterableToArrayLimit(r, l) { var t = null == r ? null : "undefined" != typeof Symbol && r[Symbol.iterator] || r["@@iterator"]; if (null != t) { var e, n, i, u, a = [], f = !0, o = !1; try { if (i = (t = t.call(r)).next, 0 === l) { if (Object(t) !== t) return; f = !1; } else for (; !(f = (e = i.call(t)).done) && (a.push(e.value), a.length !== l); f = !0); } catch (r) { o = !0, n = r; } finally { try { if (!f && null != t["return"] && (u = t["return"](), Object(u) !== u)) return; } finally { if (o) throw n; } } return a; } }
function _arrayWithHoles(arr) { if (Array.isArray(arr)) return arr; }
var _React = React,
  useRef = _React.useRef,
  useState = _React.useState,
  useEffect = _React.useEffect,
  useMemo = _React.useMemo;
var fuse_options = {
  includeScore: true,
  threshold: 0.1,
  keys: ['doc', {
    name: 'name',
    weight: 2
  }]
};
var App = function App() {
  var _useState = useState(''),
    _useState2 = _slicedToArray(_useState, 2),
    term = _useState2[0],
    setTerm = _useState2[1];
  var _useState3 = useState({
      data: [],
      index: null
    }),
    _useState4 = _slicedToArray(_useState3, 2),
    docs = _useState4[0],
    setDocs = _useState4[1];
  var default_list = useMemo(function () {
    return docs.data.map(function (item) {
      return {
        item: item
      };
    });
  }, [docs]);
  var length = useMemo(function () {
    var lengths = docs.data.map(function (_ref) {
      var doc = _ref.doc;
      var lenghts = doc.split('\n').map(function (line) {
        return line.length;
      });
      return Math.max.apply(Math, _toConsumableArray(lenghts));
    });
    return Math.max.apply(Math, _toConsumableArray(lengths));
  }, [docs]);
  useEffect(function () {
    var data = get_docs_strings();
    data.sort(function (a, b) {
      return a.name.localeCompare(b.name);
    });
    var index = Fuse.createIndex(fuse_options.keys, data);
    setDocs({
      data: data,
      index: index
    });
  }, []);
  function handleChange(event) {
    setTerm(event.target.value);
  }
  var fuse = new Fuse(docs.data, fuse_options, docs.index);
  var result = term.trim() ? fuse.search(term) : default_list;
  return /*#__PURE__*/React.createElement("div", {
    id: "search",
    style: {
      '--length': length
    }
  }, /*#__PURE__*/React.createElement("div", {
    className: "input"
  }, /*#__PURE__*/React.createElement("label", {
    htmlFor: "term"
  }, "Search"), /*#__PURE__*/React.createElement("input", {
    id: "term",
    onChange: handleChange,
    value: term
  })), /*#__PURE__*/React.createElement("ul", null, result.map(function (_ref2) {
    var item = _ref2.item;
    return /*#__PURE__*/React.createElement("li", null, /*#__PURE__*/React.createElement("h2", null, item.name), /*#__PURE__*/React.createElement("pre", null, item.doc));
  })));
};
var root = ReactDOM.createRoot(document.getElementById('root'));
root.render( /*#__PURE__*/React.createElement(App, null));
function skip_internal(_ref3) {
  var _ref4 = _slicedToArray(_ref3, 1),
    name = _ref4[0];
  return name.match(/^%/) === null;
}
function map_docs(pairs) {
  return pairs.filter(skip_internal).map(function (_ref5) {
    var _ref6 = _slicedToArray(_ref5, 2),
      name = _ref6[0],
      fn = _ref6[1];
    return {
      name: name,
      doc: fn === null || fn === void 0 ? void 0 : fn.__doc__
    };
  }).filter(function (object) {
    return object.doc;
  });
}
function get_docs_strings() {
  var env = lips.env.__parent__.__env__;
  return map_docs(Object.entries(env));
}

