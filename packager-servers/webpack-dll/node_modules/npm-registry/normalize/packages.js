'use strict';

var creation = '2010-01-14T01:41:08-08:00'  // The date that the registry got spec'd.
  , extract = require('extract-github')
  , semver = require('../semver')
  , to = require('./to');

/**
 * Normalize package data.
 *
 * @param {Object} data The package data.
 * @param {Object} fallback Optional data structure to fallback
 * @returns {Object} The cleaned up data structure.
 * @api public
 */
function packages(data, fallback) {
  if (!data || 'object' !== to.type(data)) return {};

  var releases = Object.keys(data.versions || data.times || {})
    , latest;

  releases = releases.filter(function clean(version) {
    try { return !!semver.valid(version, true); }
    catch (e) { return false; }
  }).sort(function sort(a, b) {
    return semver.gt(a, b, true) ? -1 : 1;
  }).reduce(function reduce(result, release) {
    result[release] = data.versions[release]._npmUser;
    return result;
  }, {});

  //
  // Clean up the dist-tags before we can figure out the latest package.
  //
  if ('object' !== typeof data['dist-tags']) data['dist-tags'] = {};
  if (!('latest' in data['dist-tags'])) data['dist-tags'].latest = releases[0];

  latest = (data.versions || {})[data['dist-tags'].latest] || {};

  if (to.type(fallback) !== 'object') {
    fallback = latest;

    //
    // The fastest way of creating a clone of an object.
    //
    try { fallback = JSON.parse(JSON.stringify(fallback)); }
    catch (e) {}
  }

  //
  // These can not be transformed to a normal value that easily so we set them
  // first.
  //
  data._id = data.name = data.name || data._id || fallback.name || fallback._id;
  data.license = data.license || fallback.license;
  data.licenses = to.licenses(data, fallback);
  data.github = extract(data);
  data.releases = releases;
  data.latest = latest;

  [
    { key: '_npmUser',              value: {}, parse: to.gravatar                 },
    { key: 'bugs',                  value: {}, parse: to.github('issues')         },
    { key: 'bundledDependencies',   value: []                                     },
    { key: 'dependencies',          value: {}                                     },
    { key: 'description',           value: ''                                     },
    { key: 'devDependencies',       value: {}                                     },
    { key: 'engines',               value: {}                                     },
    { key: 'homepage',              value: {}, parse: to.github()                 },
    { key: 'keywords',              value: []                                     },
    { key: 'maintainers',           value: [], parse: to.gravatar                 },
    { key: 'optionalDependencies',  value: {}                                     },
    { key: 'peerDependencies',      value: {}                                     },
    { key: 'readme',                value: ''                                     },
    { key: 'readmeFilename',        value: ''                                     },
    { key: 'repository',            value: {}, parse: to.github()                 },
    { key: 'scripts',               value: {}                                     },
    { key: 'time',                  value: {}                                     },
    { key: 'version',               value: ''                                     },
    { key: 'versions',              value: {}                                     }
  ].forEach(function each(transform) {
    var key = transform.key;

    data[key] = data[key] || fallback[key] || transform.value;

    //
    // If there's an additional data transformer run that over the structure.
    //
    if (transform.parse && data[key]) {
      if (Array.isArray(data[key])) {
        data[key] = data[key].map(transform.parse.bind(data));
      } else {
        data[key] = transform.parse.call(data, data[key]);
      }
    }

    //
    // Additional check to ensure that the field has the correct value. Or we
    // will default to our normal value.
    //
    if (to.type(data[key]) !== to.type(transform.value)) {
      data[key] = transform.value;
    }
  });

  //
  // Transform keywords in to an array.
  //
  if ('string' === typeof data.keywords) data.keywords.split(/[\s|,]{1,}/);
  if (!Array.isArray(data.keywords)) delete data.keywords;

  //
  // Add modification and creation as real date objects to the data structure.
  // They are hidden in a `time` object.
  //
  if (!data.modified || !data.created) {
    data.modified = data.modified || data.mtime;
    data.created = data.created || data.ctime;

    if (data.time.modified && !data.modified) data.modified = data.time.modified;
    if (data.time.created && !data.created) data.created = data.time.created;

    if (!data.modified && releases[0] in data.time) {
      data.modified = data.time[releases[0]];
    }

    if (!data.created && releases[releases.length -1] in data.time) {
      data.created = data.time[releases[releases.length -1]];
    }

    data.modified = data.modified || creation;
    data.created = data.created || creation;
  }

  //
  // Transform all dates to valid Date instances.
  //
  if ('string' === typeof data.modified) data.modified = new Date(data.modified);
  if ('string' === typeof data.created) data.created = new Date(data.created);

  Object.keys(data.time).forEach(function normalize(version) {
    data.time[version] = new Date(data.time[version]);
  });

  //
  // data.users is actually the people who've starred this module using npm.star
  // nobody in their right minds would have known that if you know what you're
  // looking for.
  //
  data.starred = Object.keys(data.users || fallback.users || {});

  //
  // Clean up the data structure with information that is not needed or is
  // pointlessly recursive. Or should not be defined if it's empty.
  //
  if (!data.readmeFilename) delete data.readmeFile;
  if (data._attachments) delete data._attachments;

  //
  // Another npm oddety, if you don't have a README file it will just add `no
  // README data found` as content instead of actually solving this at the view
  // level of a website.
  //
  if (!data.readme || /no readme data found/i.test(data.readme)) delete data.readme;

  //
  // It could be that a given module has been (forcefully) unpublished by the
  // registry.
  //
  data.unpublished = data._deleted === true || !!data.time.unpublished;

  // @TODO reuse github information for missing bugs fields.
  // @TODO normalize .web / .url in repo, license author etc.
  // @TODO reuse github for homepage.
  return data;
}

//
// Expose the normalizer
//
module.exports = packages;
