'use strict';

var normalize = require('../normalize');

/**
 * Access users based request information.
 *
 * @constructor
 * @param {Registry} api Reference to the wrapping registry.
 * @api private
 */
function Users(api) {
  this.api = api;
  this.send = api.send.bind(api);
  this.view = api.view.bind(api);
}

/**
 * Add a user as maintainer of a package.
 *
 * @param {String} name The user's name who needs to own the package.
 * @param {String} pkg The module it should become an owner off.
 * @param {Function} fn The callback.
 * @returns {Assign}
 * @api public
 */
Users.prototype.add = function add(name, pkg, fn) {
  return this.send(name, {
    method: 'PUT',
    json: {}
  }, fn);
};

/**
 * Create a new npm account.
 *
 * @param {String} username The desired user name.
 * @param {String} email Accounts email.
 * @param {String} password Account password.
 * @param {Function} fn Callback.
 * @returns {Assign}
 * @api public
 */
Users.prototype.create = function create(username, email, password, fn) {
  password = (password || '').toString().trim();
  username = (username || '').toString().trim();
  email = (email || '').toString().trim();

  //
  // @TODO this will break our Assign return flow, making chaining impossible.
  //
  if (!password) return fn(new Error('Missing password'));
  if (!~password.indexOf(':')) return fn(new Error('Password cannot contain a `:`'));
  if (!email) return fn(new Error('Missing email'));
  if (!~email.indexOf('@')) return fn(new Error('Invalid email address'));
  if (!username) return fn(new Error('Missing username'));

  return this.send('/-/user/org.couchdb.user:'+ encodeURIComponent(username), {
    method: 'PUT',
    json: {
        _id: 'org.couchdb.user:'+ username
      , date: (new Date()).toISOString()
      , email: email
      , name: username
      , password: password
      , roles: []
      , type: 'user'
    }
  }, fn);
};

/**
 * Update the user.
 *
 * @param {String} username The user's name.
 * @param {Object} fields The fields we want to add.
 * @param {Function} fn Completion callback.
 * @returns {Assign}
 * @api public
 */
Users.prototype.update = function update(username, fields, fn) {
  username = '/-/user/org.couchdb.user:'+ encodeURIComponent(username);

  var users = this;

  /**
   * Small helper function to handle revisions in CouchDB.
   *
   * @param {Error} err An optional error object.
   * @param {String} _rev The current rev in the CouchDB.
   * @param {Object} data Optional data.
   * @api private
   */
  function rev(err, _rev, data) {
    if (err) return fn(err);

    return users.send(username +'/-rev/'+ _rev, {
      method: 'PUT',
      json: users.api.merge(data || {}, fields)
    }, fn);
  }

  if ('_rev' in fields) return rev(undefined, fields._rev);
  return this.send(username, rev);
};

/**
 * List all packages for the given name.
 *
 * @param {String} name The user's name who's packages we want to list.
 * @param {Function} fn The callback.
 * @returns {Assign}
 * @api public
 */
Users.prototype.list = function list(name, fn) {
  return this.view('browseAuthors', {
    key: name
  }, fn)
  .map(this.api.map.simple)
  .filter(Boolean);
};

/**
 * List all packages that the user has starred.
 *
 * @param {String} name The user's name
 * @param {Function} fn The callback.
 * @returns {Assign}
 * @api public
 */
Users.prototype.starred = function starred(name, fn) {
  return this.view('browseStarUser', {
    key: name
  }, fn)
  .map(this.api.map.simple)
  .filter(Boolean);
};

/**
 * Get profile information.
 *
 * @param {String} username The user's name.
 * @param {Function} fn The callback.
 * @returns {Assign}
 * @api public
 */
Users.prototype.get = function get(username, fn) {
  username = '/-/user/org.couchdb.user:'+ encodeURIComponent(username);

  return this.send(username, fn).map(normalize.users);
};

/**
 * Sync ownership of npm modules with another account. This is useful if you
 * have one base owner of modules like a corporate account and you want to
 * on-board a new user.
 *
 * @param {String} source The user's packages that needs to be synced.
 * @param {String} target The user who needs to have ownership.
 * @param {Object} options Configuration of the sync process.
 * @returns {Assign}
 * @api public
 */
Users.prototype.sync = function sync(source, target, options, fn) {
  if ('function' === typeof options) {
    fn = options;
    options = null;
  }

  options = options || {};
  options.add = 'add' in options ? options.add : true;
  options.packages = 'packages' in options ? options.packages : false;

  var user = this;

  this.list(source, function (err, packages) {
    user.api.async.each(packages, function (name, next) {
      user.add(target, name, next);
    }, fn);
  });
};

//
// Expose module.
//
module.exports = Users;
