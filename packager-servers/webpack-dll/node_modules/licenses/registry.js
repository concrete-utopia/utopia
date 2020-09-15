'use strict';

var debug = require('debug')('licenses::npm');

/**
 * Parser for npm based license information.
 *
 * @constructor
 * @api public
 */
module.exports = require('./parser').extend({
  /**
   * The name of this parser.
   *
   * @type {String}
   * @private
   */
  name: 'npm',

  /**
   * Parse the npm license information from the package.
   *
   * @param {Object} data The package.json or npm package contents.
   * @param {Object} options Optional options.
   * @param {Function} next Continuation.
   * @api public
   */
  parse: function parse(data, options, next) {
    data = this.get(data);

    if ('function' === typeof options) {
      next = options;
      options = {};
    }

    //
    // We cannot detect a license so we call the callback without any arguments
    // which symbolises a failed attempt.
    //
    if (!data) return next();

    debug('found %s in the package contents', data);

    // @TODO handle the edge case where people give us an URL instead of an
    // actual license.
    next(undefined, this.normalize(data));
  },

  /**
   * Return the possible location of license information.
   *
   * @param {Object} data The object that should contain the license.
   * @returns {String}
   * @api private
   */
  license: function licenses(data) {
    if ('string' === typeof data && data) return data;
    if ('type' in data && data.type) return data.type;

    //
    // Common typo's
    //
    if ('type:' in data && data['type:']) return data['type:'];

    return;
  },

  /**
   * Is npm based license detection an option for this package.
   *
   * @param {Object} data The package.json or npm package contents.
   * @returns {Boolean}
   * @api public
   */
  supported: function supported(data) {
    return !!this.get(data);
  },

  /**
   * Retrieve the possible locations of the license information.
   *
   * @param {Object} data The package.json or npm package contents.
   * @returns {Array}
   * @api private
   */
  get: function get(data) {
    var parser = this
      , matches = [];

    //
    // Another npm oddity, it allows licenses to be specified in to different
    // properties. Because why the fuck not?
    //
    ['license', 'licenses'].forEach(function each(key) {
      if ('string' === typeof data[key]) {
        return matches.push(data[key]);
      }

      if (Array.isArray(data[key])) {
        return Array.prototype.push.apply(
          matches,
          data[key].map(function map(item) {
            return parser.license(item);
          }).filter(Boolean)
        );
      }

      if ('object' === typeof data[key] && parser.license(data[key])) {
        return Array.prototype.push.apply(
          matches,
          [parser.license(data[key])]
        );
      }
    });

    if (matches.length) return matches;
  }
});
