'use strict';

var extract = require('extract-github')
  , to = require('./to');

/**
 * Normalize user profile information.
 *
 * @param {Object} data The profile data.
 * @returns {Object} The cleaned up data structure.
 * @api public
 */
function users(data) {
  if (!data || 'object' !== to.type(data)) return {};

  //
  // Clean up github information by extracting github user name from the URL and
  // reconstructing it. People can put anything in here, so we just want to make
  // sure that WE decided on the our internal structure, not the users.
  //
  if (data.github) {
    if ('string' !== typeof data.github) {
      delete data.github;
    } else {
      data.github = (extract(data.github) || {}).user;
    }

    //
    // The user didn't specify a homepage, we're going to default to their
    // Github profile as that is the most interesting for most developers.
    //
    if ('string' === typeof data.github && !data.homepage) {
      data.homepage = 'https://github.com/'+ data.github;
    }
  }

  //
  // It's possible that the twitter profile is stored with an `@` prefix or with
  // a full URL. We only need the twitter handle as we can construct the URL's
  // our self.
  //
  if (data.twitter) {
    var twitter = /twitter.com\/[#!@\/]{0,}([^\/]+)\/?/gi;

    if ('string' !== typeof data.twitter) {
      delete data.twitter;
    } else if (data.twitter.indexOf('@') === 0) {
      data.twitter = data.twitter.slice(1);
    } else if (twitter.test(data.twitter)) {
      data.twitter = twitter.exec(data.twiter)[1];
    } else {
      data.twitter = data.twitter
        .replace(/^@*(.*)/, '$1')
        .replace(/^https?:\/\/twitter.com\//i, '');
    }
  }

  //
  // Make sure we follow the same gravatar URL pattern as the maintainers in the
  // packages.
  //
  to.gravatar(data);

  //
  // The fields is way to opinionated and should have existed in the npm's
  // website instead. We're going to remove this as it's only duplicate
  // bullshit.
  //
  delete data.fields;

  return data;
}

module.exports = users;
