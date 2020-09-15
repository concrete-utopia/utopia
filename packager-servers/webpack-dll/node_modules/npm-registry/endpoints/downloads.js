'use strict';

/**
 * Download download stats from the npm server.
 *
 * @constructor
 * @param {Mana} api The actual API instance.
 * @api private
 */
function Downloads(api) {
  this.send = api.send.bind(api);
  this.api = api;
}

/**
 * Get the total amount of downloads for a given period. If no package name has
 * been supplied the total of all packages will be returned.
 *
 * @param {String} period The period you want to select.
 * @param {String} pkg Optional name of the package.
 * @param {Function} fn Completion callback.
 * @returns {Assign}
 * @api public
 */
Downloads.prototype.totals = function point(period, pkg, fn) {
  if ('function' === typeof pkg) {
    fn = pkg;
    pkg = null;
  }

  return this.send(['downloads', 'point', period, pkg], {
    api: this.api.statservice
  }, fn).map(function map(data) {
    //
    // There is this annoying edge-case in the npm downloads API where they do
    // not return the correct information for newly published packages. These
    // newly published packages are missing the `package` and `downloads` field.
    // We've normalized to their sensible defaults so we can just ignore the
    // fact that they are unable to correctly estimate the download count for
    // a new module.
    //
    data.package = data.package || pkg;
    data.downloads = data.downloads || 0;

    return data;
  });
};

/**
 * Get the download statics for range of days.
 *
 * @param {String} period The period you want to select.
 * @param {String} pkg Optional name of the package.
 * @param {Function} fn Completion callback.
 * @returns {Assign}
 * @api public
 */
Downloads.prototype.range = function ranged(period, pkg, fn) {
  if ('function' === typeof pkg) {
    fn = pkg;
    pkg = null;
  }

  return this.send(['downloads', 'range', period, pkg], {
    api: this.api.statservice
  }, fn);
};

//
// Exposes the Downloads API.
//
module.exports = Downloads;
