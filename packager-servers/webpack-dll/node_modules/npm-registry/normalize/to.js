'use strict';

var extract = require('extract-github').url
  , toString = Object.prototype.toString
  , crypto = require('crypto')
  , url = require('url');

//
// to.js is a small collection of parsers and utilities that makes it easier to
// normalize the data structures that are returned from The npm Registry API.
//

/**
 * Get accurate type information for the given JavaScript class.
 *
 * @param {Mixed} of The thing who's type class we want to figure out.
 * @returns {String} lowercase variant of the name.
 * @api private
 */
exports.type = function type(of) {
  return toString.call(of).slice(8, -1).toLowerCase();
};

/**
 * Decide the best way of merging license data.
 *
 * @param {Mixed} data
 * @param {Mixed} fallback
 * @returns {Mixed} The one that should be merged.
 */
exports.licenses = function licenses(data, fallback) {
  var fblicenses = fallback.licenses
    , dlicenses = data.licenses
    , fbok, dok;

  fbok = Array.isArray(fblicenses) && fblicenses.every(function every(license) {
    return 'string' === exports.type(license);
  });

  dok = Array.isArray(dlicenses) && dlicenses.every(function every(license) {
    return 'string' === exports.type(license);
  });

  if (dok && !fbok) return dlicenses;
  if (fbok && !dok) return fblicenses;

  return dlicenses || fblicenses;
};

/**
 * Create a gravatar for the given email.
 *
 * @param {Object} data Object that has an `email` property.
 * @returns {Object} The data object.
 * @private
 */
exports.gravatar = function gravatar(data) {
  var email = (
    'string' === typeof data.email
      ? data.email
      : ''
  ).toLowerCase().trim();

  if (!email || (data.gravatar && !~data.gravatar.indexOf('?'))) {
    return data; // Gravatar's are constructed from email addresses.
  }

  data.gravatar_id = crypto.createHash('md5').update(email).digest('hex');
  data.gravatar = 'https://secure.gravatar.com/avatar/'+ data.gravatar_id;

   return data;
};

/**
 * Default to something from github.
 *
 * @param {String} path The path we should append to the string.
 * @returns {Function}
 * @api public
 */
exports.github = function github(path) {
  return function githubtransform(data) {
    var type = exports.type(data)
      , existing = extract(data);

    if (!existing) {
      if (!this.github) return undefined;

      existing = 'https://github.com/'+ this.github.user +'/'+ this.github.repo;
      if (path) existing += '/'+ path;
    } else {
      //
      // Normalize the URL to something useful. As it could be that we were
      // given some of the following URL structures:
      //
      // - git@github.com:primus/primus.git
      // - git://github.com/primus/primus.git#branch
      // - https://github.com/primus/primus.git
      // - git+ssh://git@github.com/primus/primus.git#branch
      //
      existing = existing.replace(/^git@/, '').replace(/com\:/, 'com/');
      existing = url.parse(existing);
      existing.protocol = !existing.protocol || 'git:' === existing.protocol ? 'http:' : existing.protocol;
      existing.slashes = true;
      existing.path = existing.pathname = (existing.pathname || existing.path || '').replace(/\.git$/, '');
      existing.hash = null;
      existing = url.format(existing);
    }

    if ('object' !== type) return {
      url: existing
    };

    data.url = existing;
    delete data.web;

    return data;
  };
};
