'use strict';

var debug = require('debug')('licenses::content');

module.exports = require('./parser').extend({
  /**
   * The name of this parser.
   *
   * @type {String}
   * @private
   */
  name: 'content',

  /**
   * Parse the markdown information from the package.
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

    //
    // Optimize the matches by trying to locate where the licensing information
    // starts in the given content. Usually, we, as developers add it at the
    // bottom of our README.md files and prefix it with "LICENSE" as header.
    //
    if (data.file && /readme/i.test(data.file)) {
      data.content.split('\n')
        .some(function some(line, index, lines) {
          if (
            /^.{0,7}\s{0,}(?:licen[cs]e[s]?|copyright).{0,2}\s{0,}$/gim.test(
              line.trim())
          ) {
            data.content = lines.slice(index).join('\n');
            debug('matched %s as license header, slicing data', JSON.stringify(line));
          return true;
        }

        return false;
      });
    }

    var license = this.scan(data.content);
    if (!license) {
      license = this.test(data.content);

      if (license) debug('used regexp to detect %s in content', license);
    } else {
      debug('license file scan resulted in %s as matching license', license);
    }


    next(undefined, this.normalize(license));
  },

  /**
   * Is content based license detection an option for this package.
   *
   * @param {Object} data The package.json or npm package contents.
   * @returns {Boolean}
   * @api public
   */
  supported: function supported(data) {
    return !!this.get(data);
  },

  /**
   * Retrieve the only possible location of data. Which is the `readme` content
   * but that's only available for packages that are retrieved through npm.
   *
   * @param {Object} data The package.json or npm package contents.
   */
  get: function get(data) {
    if ('string' === typeof data) return { content: data };
    if (data.readme) return { content: data.readme, file: 'readme' };
    if (data.content) return data;
  }
});
