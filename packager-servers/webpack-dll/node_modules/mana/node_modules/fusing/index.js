'use strict';

var predefine = require('predefine')
  , slice = Array.prototype.slice
  , emits = require('emits')
  , path = require('path');

/**
 * Fuses the prototypes of two base classes in to one single class.
 *
 * @param {Function} Base Base function.
 * @param {Function} inherits The function where the base needs to inherit from.
 * @param {Object} options Configure how the inheritance is done.
 * @returns {Function} Base.
 * @api private
 */
module.exports = function fuse(Base, inherits, options) {
  options = options || {};

  if ('function' === typeof inherits) {
    Base.prototype.__proto__ = inherits.prototype;
  } else if ('object' === typeof inherits) {
    options = inherits;
    inherits = null;
  }

  /**
   * Add a new property to the prototype which is not enumerable but still
   * writable.
   *
   * @type {Function}
   * @public
   */
  Base.writable = predefine(Base.prototype, predefine.WRITABLE);

  /**
   * Add a new property to the prototype which is not enumerable but only
   * readable.
   *
   * @type {Function}
   * @public
   */
  Base.readable = predefine(Base.prototype, {
    configurable: false,
    enumerable: false,
    writable: false
  });

  /**
   * Add a new property to the prototype which is not enumerable but only
   * a getter.
   *
   * @type {Function}
   * @public
   */
  Base.get = function get(method, getter) {
    Object.defineProperty(Base.prototype, method, {
      configurable: false,
      enumerable: false,
      get: getter
    });

    return get;
  };

  /**
   * Add a new property to the prototype which is not enumerable but only
   * a getter and setter.
   *
   * @type {Function}
   * @public
   */
  Base.set = function set(method, getter, setter) {
    Object.defineProperty(Base.prototype, method, {
      configurable: false,
      enumerable: false,
      get: getter,
      set: setter
    });

    return set;
  };

  /**
   * Reset the constructor so it points to the Base class.
   *
   * @type {Function}
   * @api public
   */
  Base.writable('constructor', Base);

  /**
   * Spice up
   *
   * @api public
   */
  var fused = Base.prototype.fuse;
  Base.writable('fuse', function fuse(args) {
    var writable = predefine(this, predefine.WRITABLE);

    if (!this.writable) writable('writable', writable);
    if (!this.readable) writable('readable', predefine(this));

    if (fused) this.fuse = fused;

    //
    // Inheritance is optional, so only execute it when it's an actual function.
    //
    if ('function' === typeof inherits) {
      inherits.apply(this, args || arguments);
    }
  });

  /**
   * Make the Base class extendable using Backbone's .extend pattern.
   *
   * @type {Function}
   * @api public
   */
  Base.extend = predefine.extend;

  /**
   * Expose the predefine.
   *
   * @type {Function}
   * @api public
   */
  Base.predefine = predefine;

  //
  // Allow inheritance without adding additional default methods to the
  // prototype.
  //
  if (options.defaults === false) return Base;

  //
  // Inherit some methods from common module we use.
  //
  if (options.mixin !== false) Base.readable('mixin', predefine.mixin);
  if (options.merge !== false) Base.readable('merge', predefine.merge);
  if (options.emits !== false) Base.readable('emits', emits);

  return Base;
};
