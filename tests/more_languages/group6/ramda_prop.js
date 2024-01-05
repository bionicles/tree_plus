import _curry2 from "./internal/_curry2.js";
import _isInteger from "./internal/_isInteger.js";
import nth from "./nth.js";

/**
 * Returns a function that when supplied an object returns the indicated
 * property of that object, if it exists.
 *
 * @func
 * @memberOf R
 * @since v0.1.0
 * @category Object
 * @typedefn Idx = String | Int | Symbol
 * @sig Idx -> {s: a} -> a | Undefined
 * @param {String|Number} p The property name or array index
 * @param {Object} obj The object to query
 * @return {*} The value at `obj.p`.
 * @see R.path, R.props, R.pluck, R.project, R.nth
 * @example
 *
 *      R.prop('x', {x: 100}); //=> 100
 *      R.prop('x', {}); //=> undefined
 *      R.prop(0, [100]); //=> 100
 *      R.compose(R.inc, R.prop('x'))({ x: 3 }) //=> 4
 */

var prop = _curry2(function prop(p, obj) {
  if (obj == null) {
    return;
  }
  return _isInteger(p) ? nth(p, obj) : obj[p];
});
export default prop;

/**
 * Solves equations of the form a * x = b
 * @param {{
 *  z: number
 * }} x
 * EDGE CASE LOREM IPSUM YADDA YADDA
 */
function foo(x) {}

import _curry2 from './internal/_curry2.js';
import _isArray from './internal/_isArray.js';
import _map from './internal/_map.js';
import _assoc from './internal/_assoc.js';

/**
 *
 * Deconstructs an array field from the input documents to output a document for each element.
 * Each output document is the input document with the value of the array field replaced by the element.
 *
 * @func
 * @memberOf R
 * @since v0.28.0
 * @category Object
 * @sig String -> {k: [v]} -> [{k: v}]
 * @param {String} key The key to determine which property of the object should be unwound.
 * @param {Object} object The object containing the list to unwind at the property named by the key.
 * @return {List} A list of new objects, each having the given key associated to an item from the unwound list.
 * @example
 *
 * R.unwind('hobbies', {
 *   name: 'alice',
 *   hobbies: ['Golf', 'Hacking'],
 *   colors: ['red', 'green'],
 * });
 * // [
 * //   { name: 'alice', hobbies: 'Golf', colors: ['red', 'green'] },
 * //   { name: 'alice', hobbies: 'Hacking', colors: ['red', 'green'] }
 * // ]
 */

var unwind = _curry2(function(key, object) { // edge case here because the wrapped function is anonymous
  // If key is not in object or key is not as a list in object
  if (!(key in object && _isArray(object[key]))) {
    return [object];
  }
  // Map over object[key] which is a list and assoc each element with key
  return _map(function(item) {
    return _assoc(key, item, object);
  }, object[key]);
});

export default unwind;