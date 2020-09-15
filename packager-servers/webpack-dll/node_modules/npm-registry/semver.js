'use strict';

var semver = require('semver');

//
// Introduce semver on the module's exports so we can still use all it's
// methods while improving and extending it.
//
Object.keys(semver).forEach(function introduce(method) {
  exports[method] = semver[method];
});

/**
 * Test if a given version number is a pre-release.
 *
 * @param {String} version The version number.
 * @returns {Boolean}
 * @api public
 */
exports.prerelease = function prerelease(version) {
  var parsed = semver.parse(version);

  return parsed
  && parsed.prerelease
  && parsed.prerelease.length;
};

/**
 * A more versitile maxSatisfying method for semver.
 *
 * @param {Array} versions Array of version numbers to get the greatest from.
 * @param {String} range The version number it should find.
 * @param {boolean} strict Strict match.
 * @returns {String|Undefined} Version number that matches.
 * @api public
 */
exports.maxSatisfying = function maxSatisfying(versions, range, strict) {
  //
  // Make sure we only get valid version numbers
  //
  versions = versions.filter(function filter (version) {
    return semver.valid(version);
  });

  range = typeof range === 'string'
    ? range.trim()
    : range;

  //
  // Exact version & range match.
  //
  if (semver.valid(range) && ~versions.indexOf(range)) {
    return range;
  }

  if (strict && (!range || range === '*')) {
    var version = semver.maxSatisfying(versions.map(function map(version) {
      return !exports.prerelease(version) ? version : null;
    }), range);

    if (version) return version;
  }

  return semver.maxSatisfying(versions, range);
};
