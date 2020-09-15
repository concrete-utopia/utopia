'use strict';

var predefine = require('predefine')
  , slice = Array.prototype.slice
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

  Base.writable = predefine(Base.prototype, predefine.WRITABLE);
  Base.readable = predefine(Base.prototype, {
    configurable: false,
    enumerable: false,
    writable: false
  });

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
  Base.writable('fuse', function fuse(args) {
    var writable = predefine(this, predefine.WRITABLE);

    if (!this.writable) writable('writable', writable);
    if (!this.readable) writable('readable', predefine(this));

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
  // Inherit some methods from the predefine module
  //
  if (options.mixin !== false) Base.readable('mixin', predefine.mixin);
  if (options.merge !== false) Base.readable('merge', predefine.merge);

  /**
   * Simple emit wrapper that returns a function that emits an event once it's
   * called
   *
   * ```js
   * example.on('close', example.emits('close'));
   * ```
   *
   * @param {String} event Name of the event that we should emit.
   * @param {Function} parser The last argument, if it's a function is a arg parser
   * @api public
   */
  if (options.emits !== false)
  Base.readable('emits', function emits() {
    var args = slice.call(arguments, 0)
      , self = this
      , parser;

    //
    // Automatically prefix the event that we `emit`
    //
    if ('string' === typeof options.prefix) {
      args[0] = options.prefix + args[0];
    }

    //
    // Assume that if the last given argument is a function, it would be
    // a parser.
    //
    if ('function' === typeof args[args.length - 1]) {
      parser = args.pop();
    }

    return function emit(arg) {
      if (!self.listeners(args[0]).length) return false;

      if (parser) {
        arg = parser.apply(self, arguments);
        if (!Array.isArray(arg)) arg = [arg];
      } else {
        arg = slice.call(arguments, 0);
      }

      return self.emit.apply(self, args.concat(arg));
    };
  });

  /**
   * Compile iterator to resolve paths to various resources supplied to the module.
   *
   * @param {String} dir base directory
   * @param {Object} stack original collection
   * @param {Mixed} object reference to resource
   * @returns {Function} iterator
   * @api private
   */
  if (options.resolve !== false)
  Base.writable('resolve', function resolve(dir, stack) {
    return function resolver(object) {
      if ('string' === typeof stack[object]) {
        stack[object] = path.join(dir, stack[object]);
      }
    };
  });

  return Base;
};
