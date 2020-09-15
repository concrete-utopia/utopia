'use strict';

var EventEmitter = require('eventemitter3')
  , diagnostics = require('diagnostics')
  , request = require('request')
  , qs = require('querystring')
  , ms = require('millisecond')
  , Assign = require('assign')
  , Token = require('./token')
  , fuse = require('fusing')
  , back = require('back/reconnect')
  , url = require('url');

//
// Cached variables to improve performance.
//
var toString = Object.prototype.toString
  , slice = Array.prototype.slice;

/**
 * Give me mana.
 *
 * @constructor
 * @api public
 */
function Mana() {
  this.fuse();

  this.fnqueue = Object.create(null);   // Callback queue.
  this.authHeader = 'Authorization';    // Default auth header
  this.remaining = 0;                   // How many API calls are remaining.
  this.ratelimit = 0;                   // The amount of API calls allowed.
  this.ratereset = 0;                   // In how many seconds is the rate limit reset.
  this.tokens = [];                     // Our default API tokens.

  //
  // Create a debug method that correctly prefixes log messages with `mana` and
  // the name of the implementer.
  //
  this.debug = diagnostics('mana:'+ this.name);

  if ('function' === this.type(this.initialise)) this.initialise.apply(this, arguments);
  if ('function' === this.type(this.initialize)) this.initialize.apply(this, arguments);

  //
  // This is a required option, we cannot continue properly if we don't have an
  // API to connect to. We add a stack so you can hopefully see where the
  // instance was initialised that doesn't have the required `api` property
  //
  if (!this.api) {
    this.debug('Missing a required `api` property %s', (new Error()).stack);
  }

  //
  // We support rolling OAuth tokens as some services are absurdly rate limited
  // and a way to avoid these limits is to use a rolling token system where it
  // iterates over the api tokens and gets the next working one.
  //
  if ('string' === typeof this.tokens) this.tokens = this.tokens.split(',');
  if (Array.isArray(this.tokens) && this.tokens.length) {
    this.tokenizer().roll();
  }
}

fuse(Mana, EventEmitter);

/**
 * Default configuration for the build-in randomized exponential back off.
 *
 * @type {String|Number}
 * @public
 */
Mana.prototype.maxdelay = '60 seconds'; // Max duration of exponential back off
Mana.prototype.mindelay = '100 ms';     // Min duration of exponential back off
Mana.prototype.retries = 3;             // Allowed back off attempts.
Mana.prototype.factor = 2;              // Back off factor.

/**
 * The prefix for authorization headers for when multiple tokens can be used.
 *
 * @type {String}
 * @public
 */
Mana.prototype.prefix = 'Token ';

/**
 * The default request timeout, to prevent long running requests without any way
 * of aborting it.
 *
 * @type {String|Number}
 * @public
 */
Mana.prototype.timeout = '20 seconds';

/**
 * Maximum amount of sockets we can use for pooling.
 *
 * @type {Number}
 * @public
 */
Mana.prototype.maxSockets = 444;

/**
 * Should the requests be make with strictSSL enabled.
 *
 * @type {Boolean}
 * @public
 */
Mana.prototype.strictSSL = false;

/**
 * Expose the current version number of our Mana package so it's re-used in our
 * user agent string.
 *
 * @type {String} The version number
 * @public
 */
Mana.prototype.version = require('./package.json').version;

/**
 * The name of the module so we can re-use it in our user agent string.
 *
 * @type {String} The name of the module.
 * @public
 */
Mana.prototype.name = require('./package.json').name;

/**
 * The prefix of the CouchDB view/design doc. This defaults to a rewriter that's
 * used by npm. You should implement it as well, because it's much more pleasing
 * to my eye balls.
 *
 * @type {String} The prefix
 * @public
 */
Mana.prototype._view = '/-/_view/';

/**
 * Extract and compile a querystring.
 *
 * @param {Object} options The supplied options where we extract parameters.
 * @param {Array|Object} allowed Parameters we allow.
 * @returns {String}
 * @api public
 */
Mana.prototype.querystring = function querystringify(options, allowed) {
  var json = this.json(options, allowed)
    , query = qs.stringify(json);

  return query ? '?'+ query : '';
};

/**
 * Create an object of parameters that could get posted to a server.
 *
 * @param {Object} options The supplied options where we extract parameters.
 * @param {Array|Object} allowed Parameters we allow.
 * @returns {Object}
 * @api public
 */
Mana.prototype.json = function jsonify(options, allowed) {
  var object = this.type(allowed) === 'object'
    , mana = this
    , data = {};

  //
  // Extract the keys and default values from the `allowed` argument. If we've
  // been given an object, extract the keys and assume that the values of the
  // object are the default values for the query. If the value has been set to
  // `undefined` we will not use it as a default value and ignore the param.
  // We're re-using the `data` variable as `defaults` object as it's
  // already empty so all our `key in defaults` check will fail if we're given
  // an array.
  //
  var keys = object ? Object.keys(allowed) : allowed
    , defaults = object ? allowed : data;

  keys.forEach(function each(key) {
    var value = key in defaults && mana.type(defaults[key]) !== 'undefined'
      , has = key in options;

    if (!has && !value) return; // No defaults, no value, bail out.

    data[key] = has ? options[key] : defaults[key];

    //
    // This key was intended as query string value, remove it from our options
    // object.
    //
    delete options[key];
  });

  return data;
};

/**
 * Simple bail out function while still maintaining the async flow.
 *
 * @param {Function} fn The callback.
 * @param {Error} err Optional error argument.
 * @param {Mixed} data The data that should be written.
 * @returns {Assign}
 * @api private
 */
Mana.prototype.bail = function bail(fn, err, data) {
  var assign = new Assign(this, fn);

  (
    global.setImmediate
    ? global.setImmediate
    : global.setTimeout
  )(function immediate() {
    if (err) return assign.destroy(err);
    assign.write(data, { end: true });
  });

  return assign;
};

/**
 * Shake'n'roll the tokens to get a token with the highest likelihood of
 * working.
 *
 * @returns {Boolean} Successful shake, set a new token.
 * @api private
 */
Mana.prototype.roll = function roll() {
  var mana = this
    , tokens;

  //
  // There is no need to re-roll, we still have remaning tokens.
  //
  if (this.remaining) return true;

  //
  // Find the current token in our token set so we can update it's `ratelimit`
  // and `ratereset` value's
  //
  this.tokens.some(function some(token) {
    if (mana.authorization !== token.authorization) return false;

    token.returned(mana);
    return true;
  });

  tokens = this.tokens.filter(function filter(token) {
    return token.available();
  }).sort(function sort(b, a) {
    if (a.remaining !== b.remaining) {
      if (a.remaining < b.remaining) return -1;
      if (a.remaining > b.remaining) return 1;
      return 0;
    }

    if (a.ratelimit < b.ratelimit) return -1;
    if (a.ratelimit > b.ratelimit) return 1;
    return 0;
  });

  if (!tokens.length) return false;

  this.authorization = tokens[0].authorization;
  return true;
};

/**
 * Transform the tokens array to Token instances.
 *
 * @api private
 */
Mana.prototype.tokenizer = function tokenizer() {
  var mana = this;

  mana.tokens = mana.tokens.filter(function filter(token, index, tokens) {
    return Boolean(token) && tokens.indexOf(token) === index;
  }).map(function tokenizing(OAuth) {
    if (OAuth instanceof Token) return OAuth;

    return new Token(OAuth, mana.prefix);
  });

  return this;
};

/**
 * Return a function that will call all queued callbacks that wants to hit the
 * same endpoint.
 *
 * @param {String} urid The Unique Request Identifier.
 * @return {Function} A callback which calls all queued functions.
 * @api private
 */
Mana.prototype.all = function all(urid) {
  var mana = this;

  this.debug('adding an `all` callback for urid %s', urid);

  return function all(err, data) {
    if (!(urid in mana.fnqueue)) {
      if (!err) return mana.debug('No queued callbacks for urid %s, ignoring data.', urid);
      return mana.debug('No queued callback for urid %s but received error: ', err.message);
    }

    //
    // Get the queued function list before calling the callbacks so we don't
    // create a recursive loop if the first function calls the same API endpoint
    // again and causes it to get queued.
    //
    var queue = mana.fnqueue[urid];
    delete mana.fnqueue[urid];

    queue.forEach(function each(fn) {
      fn(err, data);

      if (fn.assign) fn.assign.destroy();
      delete fn.assign;
    });
  };
};

/**
 * Check if we're already fetching data for the given request.
 *
 * @param {String} urid The Unique Request Identifier.
 * @returns {Array|Undefined} Potential list of callbacks to call.
 * @api private
 */
Mana.prototype.fetching = function fetching(urid) {
  return this.fnqueue[urid];
};

/**
 * Add a new callback to the queue.
 *
 * @param {String} urid The Unique Request Identifier.
 * @param {function} fn The callback to queue.
 * @param {Assign} assign The assignment we returned.
 * @returns {Assign} The assignment.
 * @api private
 */
Mana.prototype.push = function push(urid, fn, assign) {
  fn.assign = assign;

  if (!this.fnqueue[urid]) {
    this.fnqueue[urid] = [fn];
  } else {
    this.fnqueue[urid].push(fn);
  }

  return assign;
};

/**
 * Parse the given arguments because we don't want to do an optional queue check
 * for every single API endpoint.
 *
 * @param {Arguments} args Arguments.
 * @returns {Object} type => based object.
 * @api public
 */
Mana.prototype.args = function parser(args) {
  if ('object' === this.type(args)) return args;

  var alias = {
    'function': 'fn',       // My preferred callback name.
    'object':   'options',  // Objects are usually options.
    'string':   'str',      // Shorter to write.
    'number':   'nr'        // ditto.
  }, mana = this;

  return slice.call(args, 0).reduce(function parse(data, value) {
    var type = mana.type(value);
    data[type] = value;

    if (type in alias) {
      data[alias[type]] = data[type];
    }

    return data;
  }, Object.create(null));
};

/**
 * Get accurate type information for the given JavaScript class.
 *
 * @param {Mixed} of The thing who's type class we want to figure out.
 * @returns {String} Lowercase variant of the name.
 * @api public
 */
Mana.prototype.type = function type(of) {
  return toString.call(of).slice(8, -1).toLowerCase();
};

/**
 * Downgrade the list of given mirrors so we can query against a different
 * server when our default api endpoint is down.
 *
 * @param {Array} mirrors The list of mirrors we can query against.
 * @param {Function} fn The callback.
 * @api private
 */
Mana.prototype.downgrade = function downgrade(mirrors, fn) {
  var source = mirrors[0]
    , errors = [];

  //
  // Remove duplicates as we don't want to test against the same server twice as
  // we already received an error. An instant retry isn't that useful in most
  // cases as we should give the server some time to cool down.
  //
  mirrors = mirrors.filter(function dedupe(item, i, all) {
    if (!item) return false; // Removes undefined, null and other garbage.
    return all.indexOf(item) === i;
  });

  (function recursive(err) {
    var api = mirrors.shift();

    //
    // If we received an error, we probably want to queue it up so we know what
    // triggered the decision to downgrade to a different API endpoint.
    //
    if (err) errors.push(err);

    //
    // We got a valid api endpoint that we can query against.
    //
    if (api) return fn(undefined, api, recursive, errors);

    //
    // No valid api endpoints available, the callback should start an back off
    // operation against the default provided source.
    //
    fn(
      new Error('No more API endpoints available, everything is down'),
      source,
      recursive,
      errors
    );
  }());

  return this;
};

/**
 * Send a request to a CouchDB based view endpoint.
 *
 * @param {String} str The name of the CouchDB view.
 * @param {Object} options The query string options for the URL.
 * @param {function} fn The callback.
 * @returns {Assignment}
 * @api public
 */
Mana.prototype.view = function view(args) {
  args = this.args(arguments);

  var query = {};

  args.str = this._view + args.str +'?';

  //
  // We're querying the view based on a known or part of a known key.
  //
  if (args.options.key) {
    query.startkey = JSON.stringify([args.options.key]);
    query.endkey   = JSON.stringify([args.options.key, {}]);
  }

  query.group_level = 'group_level' in args.options ? args.options.group_level : 3;
  query.descending = 'descending' in args.options ? args.options.descending : false;
  query.stale = 'stale' in args.options ? args.options.stale : 'update_after';

  //
  // Optional query arguments.
  //
  if ('limit' in args.options) query.limit = args.options.limit;
  if ('skip' in args.options) query.skip = args.options.skip;

  return this.send(args.str + qs.stringify(query), args.fn).emits(function emits(data, add) {
    if (!('rows' in data)) return;

    //
    // CouchDB view queries return an Array with matching rows and their data.
    // In order to make this easier to query we're going to pre-chunk these rows
    // so every `map` and `reduce` method from Assign receives these rows
    // instead of this silly data format.
    //
    data.rows.forEach(function each(row) {
      add(row.key);
    });
  });
};

/**
 * Simple wrapper around a possible cache interface.
 *
 * @param {String} str The method of the cache we want to invoke (get/set)
 * @param {Object} object The data that needs to be stored.
 * @param {Function} fn The optional callback.
 * @api private
 */
Mana.prototype.fireforget = function fireforget(args) {
  args = this.args(arguments);
  args.fn = args.fn || function nope() {};

  var key = args.object.key
    , mana = this;

  //
  // Force asynchronous execution of cache retrieval without starving I/O
  //
  (
    global.setImmediate   // Only available since node 0.10
    ? global.setImmediate
    : global.setTimeout
  )(function immediate() {
    if (key && mana.cache) {
      if ('get' === args.string) {
        if (mana.cache.get.length === 1) {
          return args.fn(undefined, mana.cache.get(key));
        } else {
          return mana.cache.get(key, args.fn);
        }
      } else if ('set' === args.string) {
        if (mana.cache.set.length === 2) {
          return args.fn(undefined, mana.cache.set(key, args.object));
        } else {
          return mana.cache.set(key, args.object, args.fn);
        }
      }
    }

    //
    // Nothing, no cache or matching methods.
    //
    args.fn();
  });

  return this;
};

/**
 * Query against a given API endpoint.
 *
 * @param {Arguments} args
 * @returns {Assign}
 * @api public
 */
Mana.prototype.send = function send(args) {
  args = this.args(arguments);
  args.options = args.options || {};

  //
  // Automatically assume that people want to transform an array in to
  // joined/paths/names if no string is supplied but we do have an array.
  //
  if (!args.str && args.array) {
    args.str = args.array.filter(Boolean).join('/');
  }

  var mana = this
    , options = {}
    , assign = args.options.assign || new Assign(this)
    , mirrors = [ args.options.api || this.api ].concat(this.mirrors || []);

  options.method = ('method' in args.options ? args.options.method : 'GET').toUpperCase();
  options.timeout = ms('timeout' in args.options ? args.options.timeout : this.timeout);
  options.strictSSL = 'strictSSL' in args.options ? args.options.strictSSL : this.strictSSL;
  options.headers = 'headers' in args.options ? args.options.headers : {};
  options.maxSockets = 'maxSockets' in args.options ? args.options.maxSockets : this.maxSockets;

  //
  // Remark: We want to ensure this can be not set at all
  //
  if ('proxy' in args.options || typeof this.proxy !== 'undefined') {
    options.proxy = 'proxy' in args.options ? args.options.proxy : this.proxy;
  }

  //
  // Exponential back off configuration.
  //
  args.options.backoff = {
    minDelay: ms('mindelay' in options ? args.options.mindelay : this.mindelay),
    maxDelay: ms('maxdelay' in options ? args.options.maxdelay : this.maxdelay),
    retries: 'retries' in options ? args.options.retires : this.retries,
    factor: 'factor' in options ? args.options.factor : this.factor
  };

  //
  // We've been supplied a bunch of parameters, assume that they need to be send
  // to the server. If we receive a `GET` request it will be appended as
  // query string all other cases we assume that it should be send as request
  // body.
  //
  if (args.options.params) {
    if ('GET' === options.method) {
      args.str += this.querystring(this.merge(
          url.parse(args.str, true).query,
          args.options
        ),
        args.options.params
    );
    } else {
      options.json = this.json(args.options, args.options.params);
    }
  }

  //
  // Now that all modifications are complete we can finally assign the callback
  // to `assign` instance so it can generate the correct request id.
  //
  var allCb = this.all(args.str);

  if (assign.fn) {
    // A callback already exists.
    var prevCb = assign.fn;
    assign.finally(function () {
      prevCb.apply(this, arguments);
      allCb.apply(this, arguments);
    });
  } else {
    assign.finally(allCb);
  }

  //
  // Optimization: Check if we're already running a request for this given API
  // endpoint so we can have the given callback executed when this request
  // finishes. This allows us to get a response faster for the callback and
  // reduce requests on the actual API.
  //
  if (args.fn) {
    if (options.method === 'GET' && this.fetching(args.str)) {
      return this.push(args.str, args.fn, assign);
    } else {
      this.push(args.str, args.fn);
    }
  }

  //
  // Add some extra HTTP headers so it would be easier to get a normal response
  // from the server.
  //
  [
    {
      key: 'User-Agent',
      value: this.name +'/'+ this.version +' node/'+ process.version
    }, {
      key: this.authHeader,
      value: this.authorization
    }, {
      key: 'Accept',
      value: 'application/json'
    }
  ].forEach(function each(header) {
    if (
         header.key in options.headers                // Already defined.
      || header.key.toLowerCase() in options.headers  // Alternate.
      || !header.value                                // No value, ignore this.
    ) return;

    options.headers[header.key] = header.value;
  });

  mana.fireforget('get', {
    key: 'GET' === options.method ? args.str : null
  }, function fn(err, cache) {
    //
    // We simply do not care about cache issues, but we're gonna make sure we
    // make the cache result undefined as we don't know what was retrieved.
    //
    if (err || 'object' !== mana.type(cache)) cache = undefined;

    if (cache && cache.etag) {
      //
      // As we're caching data from a remote server we don't know if our cache
      // has been expired or not so we need to send a non-match header to the
      // API server and hope that we trigger an 304 request so we can use our
      // cached result.
      //
      options.headers['If-None-Match'] = cache.etag;
    }

    mana.downgrade(mirrors, function downgraded(err, root, next, errors) {
      /**
       * Handle the requests.
       *
       * @param {Error} err Optional error argument.
       * @param {Object} res HTTP response object.
       * @param {String} body The registry response.
       * @api private
       */
      function parse(err, res, body) {
        mana.debug('Response headers %j', res && res.headers || {});
        assign.emit('headers', res && res.headers || {});

        if (err) {
          mana.debug('Received an error (%s) for URL %s', err.message, options.uri);

          err.url = options.uri;          // The URL we accessed.
          err.statusCode = 500;           // Force status code.
          err.errors = errors;            // Previous errors.
          err.body = body;                // The response body.
          err.data = {};                  // The parsed data.
          err.remaining = mana.remaining; // Rate remaining.
          err.ratereset = mana.ratereset; // Rate reset.
          err.ratelimit = mana.ratelimit; // Rate limit.

          return assign.destroy(err);
        }

        //
        // Even if we get an error response, it could still contain an updated
        // rate limit, so make sure we parse that out before we start handling
        // potential errors.
        //
        var ratereset = +res.headers['x-ratelimit-reset']
          , ratelimit = +res.headers['x-ratelimit-limit']
          , remaining = +res.headers['x-ratelimit-remaining'];

        if (!isNaN(ratereset)) mana.ratereset = ratereset;
        if (!isNaN(ratelimit)) mana.ratelimit =  ratelimit;
        if (!isNaN(remaining)) {
          mana.remaining = remaining;
          mana.debug('Only %d API request remaining', remaining);
        }

        //
        // We had a successful cache hit, use our cached result as response
        // body.
        //
        if (304 === res.statusCode && cache) {
          mana.debug('CACHE HIT, using cached data for URL', options.uri);
          assign.add(res.headers);
          assign.write(cache.data, { end: !args.options.next });
          return args.options.next && args.options.next(res, assign, args);
        }

        //
        // This 403 was a result of a reached API limit, but we've got multiple
        // API / OAuth tokens that we can use to request this API. So we're
        // going to roll the dice and hopefully get a working API key
        //
        if (
             mana.tokens.length
           && (
             (403 === res.statusCode && 0 === mana.remaining)
             || 401 === res.statusCode
           )
        ) {
          if (401 === res.statusCode) {
            mana.debug('Received a 401 on our access `%s` token, trying another token', mana.authorization);
          } else {
            mana.debug('Weve reached our API limit with `%s`, trying another token', mana.authorization);
          }

          if (mana.roll()) {
            options.headers.Authorization = mana.authorization;
            mana.debug('Successfully switched from authorization tokens');

            //
            // We've upgrade the header with new authorization information so
            // attempt again with the same URL.
            //
            return downgraded(undefined, root, next, errors);
          }
        }

        if (
          !(res.statusCode >= 200 && res.statusCode < 300)
          && 404 !== res.statusCode && 409 !== res.statusCode
        ) {

          //
          // Assume that the server is returning an unknown response and that we
          // should try a different server.
          //
          mana.debug('Received an invalid statusCode (%s) for URL %s %s', res.statusCode, options.uri, body);
          err = new Error('Received a non 200 status code: '+ res.statusCode);

          err.url = options.uri;          // The URL we accessed.
          err.statusCode = 500;           // Force status code.
          err.errors = errors;            // Previous errors.
          err.body = body;                // The response body.
          err.data = {};                  // Parsed response.
          err.remaining = mana.remaining; // Rate remaining.
          err.ratereset = mana.ratereset; // Rate reset.
          err.ratelimit = mana.ratelimit; // Rate limit.

          return next(err);
        }

        //
        // In this case I prefer to manually parse the JSON response as it
        // allows us to return more readable error messages.
        //
        var data = body;

        if (
             data
          && 'string' === typeof data
          && !~options.headers.Accept.indexOf('text')
          && !~options.headers.Accept.indexOf('html')
          && 'HEAD' !== options.method
        ) {
          try { data = JSON.parse(body); }
          catch (e) {
            //
            // Failed to parse response, it was something completely different
            // then we originally expected so it could be that the server
            // is down and returned an error page, so we need to continue to
            // a different server.
            //
            mana.debug('Failed to parse JSON: %s for URL %s', e.message, options.uri);

            e.url = options.uri;          // The URL we accessed.
            e.statusCode = 500;           // Force status code.
            e.errors = errors;            // Previous errors.
            e.body = body;                // The response body.
            e.data = {};                  // Parsed response.
            e.remaining = mana.remaining; // Rate remaining.
            e.ratereset = mana.ratereset; // Rate reset.
            e.ratelimit = mana.ratelimit; // Rate limit.

            return next(e);
          }
        }

        //
        // Special case for 404 requests, it's technically not an error, but
        // it's also not a valid response from the server so if we're going to
        // write it to our Assign instance it could cause issues as the data
        // format might differ. So instead we're going to call this as an error.
        //
        if ((res.statusCode === 404 || res.statusCode === 409)
          && 'HEAD' !== options.method) {
          err = new Error('Invalid status code: ' + res.statusCode);

          err.url = options.uri;           // URL of the request.
          err.statusCode = res.statusCode; // Status code.
          err.errors = errors;             // Previous errors.
          err.body = body;                 // The response body.
          err.data = data;                 // Parsed response.
          err.remaining = mana.remaining;  // Rate remaining.
          err.ratereset = mana.ratereset;  // Rate reset.
          err.ratelimit = mana.ratelimit;  // Rate limit.

          return assign.destroy(err);
        }

        //
        // Check if we need to cache the data internally. Caching is done using
        // a fire and forget pattern so we don't slow down in returning this
        // result. Most cache implementation would be done in a fraction of
        // a second anyways. We should also only cache GET requests as POST
        // responses body usually referrer back to the content that got posted.
        //
        if (res.headers.etag && 'GET' === options.method) {
          if (cache) mana.debug('CACHE MISS, updating cache for URL %s', options.uri);
          mana.fireforget('set', {
            etag: res.headers.etag,
            key: args.str,
            data: data
          });
        }

        //
        // Add headers for debugging purposes.
        //
        assign.add(res.headers);

        if ('HEAD' !== options.method) assign.write(data, { end: !args.options.next });
        else assign.write({ res: res, data: data }, { end: !args.options.next });
        if (args.options.next) args.options.next(res, assign, args);
      }

      //
      // The error indicates that we've ran out of mirrors, so we should try
      // a back off operation against the default npm registry, which is
      // provided by the callback. If the back off fails, we should completely
      // give up and return an useful error back to the client.
      //
      if (!err) {
        options.uri = url.resolve(root, args.str);
        mana.debug('requesting url %j', options);
        return request(options, parse);
      }

      back(function toTheFuture(failure, backoff) {
        args.options.backoff = backoff;

        mana.debug(
          'Starting request again to %s after back off attempt %s/%s',
          options.uri,
          backoff.attempt,
          backoff.retries
        );

        if (!failure) return request(options, parse);

        //
        // Okay, we can assume that shit is seriously wrong here.
        //
        mana.debug('We failed to fetch %s, all servers are down.', options.uri);
        failure = new Error('Failed to process request: '+ err.message);

        failure.url = options.url;          // URL we connected with.
        failure.statusCode = 500;           // Returned status code.
        failure.errors = errors;            // Reference to errors.
        failure.body = '';                  // Returned body>
        failure.data = {};                  // Parsed response.
        failure.remaining = mana.remaining; // Rate remaining.
        failure.ratereset = mana.ratereset; // Rate reset.
        failure.ratelimit = mana.ratelimit; // Rate limit.

        assign.destroy(failure);
      }, args.options.backoff);
    });
  });

  return assign;
};

/**
 * Drink this magical elixir and auto"magically" introduce the correct lazy loaded
 * methods.
 *
 * @param {String} module
 * @returns {Mana}
 */
Mana.drink = function drink(module) {
  var debug = diagnostics('mana')
    , path = require('path')
    , fs = require('fs')
    , Potion = this;

  //
  // The module filename is the name absolute position of the file that wants to
  // use mana as api-client base.
  //
  var directory = path.dirname(module.filename);

  fs.readdirSync(path.join(directory, 'endpoints')).filter(function filter(name) {
    return path.extname(name) === '.js';
  }).forEach(function each(name) {
    var lowercase = name.slice(0, -3).toLowerCase()
      , uppercasefirst = lowercase.slice(0, 1).toUpperCase() + lowercase.slice(1);

    Potion.predefine.lazy(Potion.prototype, lowercase, function defined() {
      return new Potion[uppercasefirst](this);
    });

    debug('registered endpoint %s', lowercase);
    Potion[uppercasefirst] = require(path.join(directory, 'endpoints', name));

    if ('function' !== typeof Potion[uppercasefirst]) {
      throw new Error('You forgot to add module.exports on your module: '+ name);
    }
  });

  try {
    var data = require(path.join(directory, 'package.json'));

    Potion.prototype.version = data.version || Potion.prototype.version;
    Potion.prototype.name = data.name || Potion.prototype.name;

    debug('updated potion.version to %s', Potion.prototype.version);
    debug('updated potion.name to %s', Potion.prototype.name);
  } catch (e) {
    debug('failed to parse project package.json, manually set `name`, `version`');
  }

  //
  // Expose the module on in our preferred way.
  //
  return module.exports = Potion;
};

/**
 * Handy shorthand for creating a new instance of an API without having to using
 * the `new` constructor. This allows immediate creation after requiring:
 *
 * ```js
 * require('mana').new(options);
 * ```
 *
 * @returns {Mana} The generated mana instance.
 * @api public
 */
Mana.new = function create(options) {
  return new this(options);
};

//
// Expose our token interface on the module.
//
Mana.Token = Token;

//
// Expose this module.
//
module.exports = Mana;
