'use strict';

var debug = require('debug')('licenses::github')
  , url = require('url');

/**
 * Parser for github based URL.
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
  name: 'github',

  /**
   * All the filenames that we're interested in from Github that can potentially
   * contain the license information.
   *
   * The extensions which are added are
   *
   * @type {Array}
   * @api private
   */
  filenames: [
    'license',
    'licence',
    'readme',
  ].concat([
    'markdown', 'mdown', 'md', 'textile', 'rdoc', 'org', 'creole', 'mediawiki',
    'rst', 'asciidoc', 'adoc', 'asc', 'pod'
  ].reduce(function flatten(slim, extension) {
    slim.push('license.'+ extension, 'readme.'+ extension, 'licence.'+ extension);
    return slim;
  }, [])),

  /**
   * Parse the github information from the package.
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

    var githulk = options.githulk || this.githulk
      , project = data.user +'/'+ data.repo
      , parser = this;

    githulk.repository.moved(project, function moved(err, github, changed) {
      if (err) return next(err);
      if (changed) project = github.user +'/'+ github.repo;

      githulk.repository.contents(project, function contents(err, files) {
        if (err || !files || !files.length) return next(err);

        //
        // Check if we have any compatible.
        //
        files = files.filter(function filter(file) {
          var name = file.name.toLowerCase();

          // No size, not really useful for matching.
          if (file.size <= 0) return false;

          // Fast case, direct match.
          if (!!~parser.filenames.indexOf(name)) return true;

          // Slow case, partial match.
          return parser.filenames.some(function some(filename) {
            return !!~name.indexOf(filename);
          });
        }).sort(function sort(a, b) {
          if (a.name > b.name) return 1;
          if (b.name < b.name) return -1;
          return 0;
        });

        if (!files.length) return next();

        //
        // Stored the matching license.
        //
        var license;

        //
        // Fetch and parse the 'raw' content of the file so we can parse it.
        //
        parser.async.doWhilst(function does(next) {
          var file = files.shift();

          debug('searching %s for license information', file.name);

          githulk.repository.raw(project, {
            path: file.name
          }, function raw(err, data) {
            if (err) return next(err);

            parser.parsers.content.parse({
              content: Array.isArray(data) ? data[0] : data,
              file: file.name
            }, function parse(err, data) {
              license = data;

              if (license) debug('extracted %s from %s', data, file.name);
              next(err);
            });
          });
        }, function select() {
          return !license && files.length;
        }, function done(err) {
          next(err, license);
        });
      });
    });
  },

  /**
   * Is github based license detection an option for this package.
   *
   * @param {Object} data The package.json or npm package contents.
   * @returns {Boolean}
   * @api public
   */
  supported: function supported(data) {
    return !!this.get(data);
  },

  /**
   * Get the actual contents that we're interested in, in this case it's the
   * location of a potential github URL.
   *
   * @param {Object} data The package.json or the npm package contents.
   * @return {String} Returns the URL or undefined.
   * @api private
   */
  get: function get() {
    return this.githulk.project.apply(this, arguments);
  }
});
