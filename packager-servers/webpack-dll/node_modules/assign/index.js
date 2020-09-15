'use strict';

var slice = Array.prototype.slice
  , fuse = require('fusing');

function noop() {
  /* You just wasted a second reading this comment, you're welcome */
}

//
// Generate a "random" "undefined" variable so we know when we need to ignore
// a row during an assignment operation.
//
var undef = '_assign_undef:'+ [1, 1, 1, 1].map(function generator() {
  return Math.random().toString(36).substring(2).toUpperCase();
}).concat([process.pid]).join('-');

/**
 * Data processor. Map/Reduce as promise like api. *buzzword*
 *
 * @constructor
 * @param {Mixed} context A reference to it self.
 * @param {Function} fn The callback function when data is completed.
 * @api private
 */
function Assignment(context, fn) {
  if (!(this instanceof Assignment)) return new Assignment(context, fn);

  if ('function' === typeof context) {
    fn = context;
    context = null;
  }

  var writable = Assignment.predefine(this, Assignment.predefine.WRITABLE);

  writable('and', context || this); // Chaining.
  writable('fn', fn || noop);       // Completion callback.
  writable('_async', false);        // Async processing indicator.
  writable('length', 0);            // The amount of rows we've processed so far.
  writable('result', null);         // Stores the reduced result.
  writable('rows', []);             // Reference to the processed data.
  writable('flow', []);             // Our internal flow/parse structure.
  writable('_tasks', 0);            // The amount of parallel write tasks we run.
  writable('_ends', false);         // Should end when all task are completed.
  writable('extras', []);           // Extra arguments for the callback.
}

fuse(Assignment, require('stream'), {
  defaults: false
});

/**
 * Mark the next function that we're adding as an async processing function.
 *
 * @type {Assignment}
 * @public
 */
Assignment.writable('async', {
  get: function get() {
    this._async = true;
    return this;
  }
}, true);

/**
 * Start a map operation on the received data. This map operation will most
 * likely transform the row it received.
 *
 * ```js
 * assignment.map(function map(row) {
 *  return {
 *    id: row.id,
 *    hash: crypto.createHash('md5').update(row.data).digest('hex')
 *  };
 * });
 * ```
 *
 * @param {Function} fn
 * @returns {This}
 * @api public
 */
Assignment.readable('map', function setmap(fn, options) {
  var assign = this;

  if (!assign.flow) return assign;

  /**
   * Simple wrapper around the actual mapping function that processes the
   * content.
   *
   * @param {Mixed} row The data to process.
   * @param {Function} next Call the next flow.
   * @param {Function} done Fuck it, we're done.
   * @api private
   */
  function map(row, next, done) {
    if (!map.async) return next(undefined, fn(row, assign.length));

    if (fn.length === 2) fn(row, next);
    else fn(row, assign.length, next);
  }

  map.async = assign._async;  // Should we do this async.
  map.assignment = 'map';     // Process type.
  assign.flow.push(map);      // Store.
  assign._async = false;      // Reset.

  return assign;
});

/**
 * Filter the result out of the set.
 *
 * @param {Function} fn
 * @returns {Assignment}
 * @api public
 */
Assignment.readable('filter', function setfilter(fn) {
  var assign = this;

  if (!assign.flow) return assign;

  /**
   * Simple wrapper around the actual filter function that processes the
   * content.
   *
   * @param {Mixed} row The data to process.
   * @param {Function} next Call the next flow.
   * @param {Function} done Fuck it, we're done.
   * @api private
   */
  function filter(row, next, done) {
    if (!filter.async) return next(undefined, fn(row, assign.length));

    if (fn.length === 2) fn(row, next);
    else fn(row, assign.length, next);
  }

  filter.async = assign._async;  // Should we do this async.
  filter.assignment = 'filter';  // Process type.
  assign.flow.push(filter);      // Store.
  assign._async = false;         // Reset.

  return assign;
});

/**
 * Reduce the results to a single value.
 *
 * @param {Function} fn
 * @returns {This}
 * @api public
 */
Assignment.readable('reduce', function setreduce(fn, initial) {
  var assign = this;

  if (!assign.flow) return assign;

  /**
   * Simple wrapper around the actual reducing function that processes the
   * content.
   *
   * @param {Mixed} row The data to process.
   * @param {Function} next Call the next flow.
   * @param {Function} done Fuck it, we're done.
   * @api private
   */
  function reduce(row, next, done) {
    if (!reduce.async) {
      assign.result = fn(assign.result, row, assign.length);
      return next();
    }

    function processed(err, data) {
      assign.result = data;
      next(err);
    }

    if (fn.length === 3) fn(assign.result, row, processed);
    else fn(assign.result, row, assign.length, processed);
  }

  reduce.async = assign._async; // Should we do this async.
  reduce.assignment = 'reduce'; // Process type.
  assign.flow.push(reduce);     // Store.
  assign._async = false;        // Reset.

  if (arguments.length === 2) {
    assign.result = initial;
  }

  return assign;
});

/**
 * The emit allows you to split up the data in to multiple rows that will be
 * processed by the assignment flow.
 *
 * ```js
 * assignment.emits(function scan(row, emit) {
 *  if (row.foo) emit(row.foo);
 *  if (row.bar) emit(row.bar);
 *
 *  return false; // discard row.
 * });
 * ```
 *
 * @param {Function} fn
 * @returns {This}
 * @api public
 */
Assignment.readable('emits', function setemits(fn) {
  var assign = this;

  if (!assign.flow) return assign;

  /**
   * Push new data to the stack.
   *
   * @api private
   */
  function push() {
    assign.write(slice.call(arguments, 0), {
      skip: 'emits',
      slice: this
    });

    return push;
  }

  /**
   * Simple wrapper around the actual mapping function that processes the
   * content.
   *
   * @param {Mixed} row The data to process.
   * @param {Function} next Call the next flow.
   * @param {Function} done Fuck it, we're done.
   * @api private
   */
  function emits(row, next, done, index) {
    if (!emits.async) {
      fn(row, push.bind(index));
      return done();
    }

    function processed(err, moar) {
      if (err) return next(err);
      done();
    }

    if (fn.length === 3) fn(row, push.bind(index), processed);
    else fn(row, push.bind(index), assign.length, processed);
  }

  emits.async = assign._async; // Should we do this async.
  emits.assignment = 'emits';  // Process type.
  assign.flow.push(emits);     // Store.
  assign._async = false;       // Reset.

  return assign;
});

/**
 * We've received a new chunk of data that we should process. If we don't
 * receive an `end` boolean we assume that we've received a chunk that needs to
 * processed instead.
 *
 * ```js
 * assignment.write([{}]);
 * assignment.write([{}], true);
 * ```
 *
 * @param {Mixed} data The data we need to consume and process.
 * @param {Boolean} options This was the last fragment of data we will receive.
 * @param {Function} fn Optional callback for when data is processed.
 * @returns {Boolean}
 * @api private
 */
Assignment.readable('write', function write(data, options) {
  //
  // We cannot process the information when we don't have a flow anymore. This
  // is an indication that we've been fully destroyed and all writes should be
  // ignored. So simply returning false should be sufficient.
  //
  if (!this.flow) return false;
  options = options || {};

  var assign = this;

  //
  // Check if we need to end and clean up the assignment once we've completed all
  // currently running tasks.
  //
  if (options.end) this._ends = true;

  //
  // Starting another processing task.
  //
  ++assign._tasks;

  assign.each(data, function iterate(row, index, done) {
    assign.length++; // Gives us some intel on how many rows we've processed

    assign.each(assign.flow, function flowing(fn, index, next) {
      if ('skip' in options && options.skip === fn.assignment) return next();
      if ('slice' in options && options.slice >= index) return next();

      fn(row, function processed(err, data) {
        if (err) return done(err);

        if ('filter' === fn.assignment) {
          if (!data) row = undef;
          return next();
        }

        if (arguments.length === 2 && row !== undef) {
          row = data;
        }

        next();
      }, done, index);
    }, function finished(err) {
      if (undef !== row) assign.rows.push(row);
      done(err);
    });
  }, function finished(err) {
    --assign._tasks;  // Finished another task.
    if (err) return assign.destroy(err);

    if (assign._ends && assign._tasks === 0 && assign.fn) {
      assign.fn.apply(assign.and, [
        err,
        assign.result || assign.rows
      ].concat(assign.extras));
      assign.fn = noop();

      return assign.destroy();
    }
  });

  return true;
});

/**
 * End the assignment.
 *
 * @param {Mixed} data The data to consume.
 * @api private
 */
Assignment.readable('end', function end(data) {
  return this.write(data, {
    end: true
  });
});

/**
 * Asynchrounous forEach. Because iterating is for bad-asses.
 *
 * @param {Array} data The data to iterate over.
 * @param {Function} iterator Called for each added data point.
 * @param {Function} done Completion callback.
 * @api private
 */
Assignment.readable('each', function each(data, iterator, done) {
  var mapper = []
    , index = 0;

  done = done || noop;
  if (undefined === data) return done(undefined, mapper);

  //
  // Our asynciterator3000, which asyncly iterates an.. Array! It's amazing,
  // it's fantastic, it's the most amazing line of code I've written all day.
  // HUZZAY, HUZZZAAY.
  //
  (function next(data) {
    if (!data || !data.length) {
      done(undefined, mapper);
      return mapper.length = 0;
    }

    iterator(data.shift(), index++, function iterators(err, row) {
      if (err) return done(err);

      mapper.push(row);
      next(data);
    });
  }(Array.isArray(data) ? data.slice(0) : [ data ]));

  return this;
});

/**
 * Assign an additional argument for the completion callback.
 *
 * @param {Mixed} arg Additional arg.
 * @returns {Assignment}
 * @api public
 */
Assignment.readable('add', function add(arg) {
  this.extras.push(arg);
  return this;
});

/**
 * Once all operations are done, call this callback.
 *
 * @param {Function} fn The callback that is called once the assignment is done.
 * @returns {This}
 * @api public
 */
Assignment.readable('finally', function final(fn) {
  this.fn = 'function' === typeof fn ? fn : this.fn;

  return this;
});

/**
 * Destroy the assignment. We're done with processing the data.
 *
 * @param {Error} err We're destroyed because we've received an error.
 * @returns {Boolean}
 * @api private
 */
Assignment.readable('destroy', function destroy(err) {
  if (!this.rows) return false;
  if (err && this.fn) this.fn(err);

  this.and = this.flow = this.fn = this.rows = this.extras = null;
  return true;
});

//
// Expose the module.
//
module.exports = Assignment;
