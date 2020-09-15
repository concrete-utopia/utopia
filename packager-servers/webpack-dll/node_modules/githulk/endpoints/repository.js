'use strict';

function Repository(api) {
  this.send = api.send.bind(api);
  this.api = api;
}

/**
 * Get the README contents of an project.
 *
 * @param {String} project The project details.
 * @param {Object} options Optional options.
 * @param {function} fn The callback
 * @returns {Assign}
 * @api public
 */
Repository.prototype.get = function get(args) {
  args = this.api.args(arguments);

  var project = this.api.project(args.str)
    , options = args.options || {};

  return this.send(
    ['repos', project.user, project.repo],
    options,
    args.fn
  );
};

/**
 * Get the README contents of an project.
 *
 * @param {String} project The project details.
 * @param {Object} options Optional options.
 * @param {function} fn The callback
 * @returns {Assign}
 * @api public
 */
Repository.prototype.readme = function readme(args) {
  args = this.api.args(arguments);

  var project = this.api.project(args.str)
    , options = args.options || {};

  options.headers = options.headers || {};
  options.headers.Accept = this.api.accepts(options.headers.Accept || 'html');

  return this.send(
    ['repos', project.user, project.repo, 'readme'],
    options,
    args.fn
  );
};

/**
 * Retrieve the raw contents of a file from the repository.
 *
 * @param {String} project The project details.
 * @param {Object} options Optional options.
 * @param {Function} fn The Callback.
 * @api private
 */
Repository.prototype.raw = function raw(args) {
  args = this.api.args(arguments);

  var project = this.api.project(args.str)
    , options = args.options || {};

  options.headers = options.headers || {};
  options.headers.Accept = options.headers.Accept || 'text/plain';
  options.branch = options.branch || 'master';
  options.api = 'https://raw.github.com/';

  return this.send(
    [project.user, project.repo, options.branch, options.path],
    options,
    args.fn
  );
};

/**
 * Retrieve the contents of a file or directory.
 *
 * @param {String} project The project details.
 * @param {Object} options Optional options.
 * @param {Function} fn The Callback.
 * @returns {Assign}
 * @api public
 */
Repository.prototype.contents = function contents(args) {
  args = this.api.args(arguments);

  var project = this.api.project(args.str)
    , options = args.options || {};

  options.path = 'contents/'+ (options.path || '');

  return this.send(
    ['repos', project.user, project.repo, options.path],
    options,
    args.fn
  );
};

/**
 * It's possible that a user has moved the repository to a new location.
 * Github automatically redirects you when you access the old page. But it
 * doesn't provide any redirection for API calls causing them to fail with
 * 404's.
 *
 * In order to detect the correct repository location we need to do a HEAD
 * check of the public github URL and use the location header as source URL
 * when we're presented with a 301 status code.
 *
 * @param {String} project The project details.
 * @param {Function} fn The Callback.
 * @returns {Assign}
 * @api public
 */
Repository.prototype.moved = function moved(args) {
  args = this.api.args(arguments);

  var project = this.api.project(args.str)
    , api = this.api;

  return this.send([project.user, project.repo], {
    api: 'https://github.com/',
    method: 'HEAD'
  }, function gothead(err, data) {
    if (err) return args.fn(err);

    var parsed = api.project(data[0].res.request.href)
      , changed = parsed.user !== project.user
        || parsed.repo !== project.repo;

    args.fn(undefined, parsed, changed);
  });
};

//
// Expose the repository API
//
module.exports = Repository;
