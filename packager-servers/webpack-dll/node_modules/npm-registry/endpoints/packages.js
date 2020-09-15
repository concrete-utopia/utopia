'use strict';

var debug = require('debug')('npmjs::packages')
  , normalize = require('../normalize')
  , licenses = require('licenses')
  , semver = require('../semver');

/**
 * Get all package information.
 *
 * @constructor
 * @param {Registry} api Reference to the wrapping registry.
 * @api private
 */
function Packages(api) {
  this.api = api;

  this.send = api.send.bind(api);
  this.view = api.view.bind(api);
}

/**
 * Get information from the npm package. If the name contains an `@` char we
 * assume that the user wants to get a specific version instead.
 * Example:
 *
 * - primus@0.1.1 would retrieve primus version 0.1.1
 *
 * @param {String} name The name of the node module.
 * @param {Function} fn The callback.
 * @returns {Assign}
 * @api public
 */
Packages.prototype.get = function get(name, fn) {
  return this.send(name.replace('@', '/'), fn).map(normalize.packages);
};

/**
 * Get all packages that are depended upon a given package name.
 *
 * @param {String} name The name of the node module.
 * @param {Function} fn The callback
 * @returns {Assign}
 * @api public
 */
Packages.prototype.depended = function depended(name, fn) {
  return this.view('dependedUpon', {
    key: name
  }, fn)
  .map(this.api.map.simple)
  .filter(Boolean);
};

/**
 * Find out which users have starred the given package.
 *
 * @param {String} name The name of the node module.
 * @param {Function} fn The callback
 * @returns {Assign}
 * @api public
 */
Packages.prototype.starred = function starred(name, fn) {
  return this.view('browseStarPackage', {
    key: name
  }, fn).map(function map(data) {
    return data[2];
  });
};

/**
 * Find all packages that matches the giving keywords.
 *
 * @param {String} name The keyword.
 * @param {Function} fn The callback.
 * @returns {Assign}
 * @api public
 */
Packages.prototype.keyword = function keyword(name, fn) {
  return this.view('byKeyword', {
    key: name
  }, fn)
  .map(this.api.map.simple)
  .filter(Boolean);
};

/**
 * Retrieve all release specific information for the given package name.
 *
 * @param {String} name The package name.
 * @param {Function} fn The callback.
 * @api public
 */
Packages.prototype.releases = function releases(name, fn) {
  var api = this.api;

  return this.details(name, fn).emits(function emit(data, add) {
    if (!data.versions) return;

    //
    // Add all versions of the given module.
    //
    Object.keys(data.versions).forEach(function addmore(version) {
      var release = data.versions[version];
      release.date = data.time[version];

      add(normalize.packages(release, data));
    });

    //
    // Also add each tag to the releases.
    //
    if ('dist-tags' in data) Object.keys(data['dist-tags']).forEach(function (key) {
      if (key in data.versions) return; // Prevent duplicates

      var version = data['dist-tags'][key]
        , release;

      //
      // It's possible that the tag does not exist in the versions object. This
      // is some odd npm edge case.
      //
      // Lesson learned: Never trust npm data structures.
      //
      if (!version || !(version in data.versions)) return;

      release = api.merge({}, data.versions[version]);

      //
      // The JSON.parse(JSON.stringify)) is needed to create a full clone of the
      // data structure as we're adding tags. That would be override during the
      // `reduce` procedure.
      //
      release.date = data.time[version];
      release.tag = key;

      add(normalize.packages(release, data));
    });

    return false;
  }).reduce(function reduce(memo, release) {
    memo[release.tag || release.version] = release;
    return memo;
  }, {});
};

/**
 * Get a specific release of a package.
 *
 * @param {String} name The name of the package
 * @param {String} version A valid version number or tag from the package.
 * @param {Function} fn The callback
 * @returns {Assign} Assignment
 * @api public
 */
Packages.prototype.release = function release(name, version, fn) {
  return this.details(name +'/'+ version, fn).map(normalize.packages);
};

/**
 * Get a version for a specific release.
 *
 * @param {String} name The name of the package.
 * @param {String} range The semver version range we should retrieve.
 * @param {Function} fn The callback
 * @returns {Assign} Assignment
 * @api public
 */
Packages.prototype.range = function ranged(name, range, fn) {
  if (!semver.validRange(range)) return fn(new Error('Invalid semver range'));

  return this.releases(name, function releases(err, versions) {
    if (err) return fn(err);

    if (range in versions) {
      debug('found and direct range (%s) match for %s', range, name);
      return fn(undefined, versions[range]);
    }

    var version = semver.maxSatisfying(Object.keys(versions), range);

    debug('max satisfying version for %s is %s', name, version);
    fn(undefined, versions[version]);
  });
};

/**
 * Retrieve additional details for the package information. This a lot slower
 * than a simple `.get` but much more detailed and accurate as it uses custom
 * parsers and mapping operations to parse the data as good as possible.
 *
 * @TODO Extract missing descriptions from github.
 * @TODO Merge profile information from github / authors.
 *
 * @param {String} name The name of the node module.
 * @param {Function} fn The callback.
 * @returns {Assign}
 * @api public
 */
Packages.prototype.details = function details(name, fn) {
  var packages = this;

  return this.get(name, fn).async.map(function map(data, next) {
    licenses(data, {
      githulk: packages.api.githulk,
      npmjs: packages
    }, function parsed(err, licenses) {
      data.licenses = licenses;

      if (err) debug('failed to detect license: %s', err.message);
      return next(err, data);
    });
  });
};

//
// Expose the module.
//
module.exports = Packages;
