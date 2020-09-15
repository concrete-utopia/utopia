/*
 * FlushWritable
 * Copyright 2014 Tom Frost
 */

var EventEmitter = require('events').EventEmitter,
	Writable = require('stream').Writable,
	util = require('util');

/**
 * FlushWritable is a drop-in replacement for stream.Writable that implements
 * the Transform stream's _flush() method.  FlushWritable is meant to be
 * extended, just like stream.Writable.  However, in the child class's
 * prototype, a method called _flush(cb) can be defined that will halt the
 * firing of the 'finish' event until the callback is called.  If the callback
 * if called with a truthy first argument, 'error' is emitted instead.
 * @param {Object} [opts] Options to configure this Writable stream.  See the
 *      Node.js docs for stream.Writable.
 * @constructor
 */
function FlushWritable(opts) {
	Writable.call(this, opts);
}
util.inherits(FlushWritable, Writable);

FlushWritable.prototype.emit = function(evt) {
	if (evt === 'finish' && this._flush && !Writable.prototype._flush) {
		this._flush(function(err) {
			if (err)
				EventEmitter.prototype.emit.call(this, 'error', err);
			else
				EventEmitter.prototype.emit.call(this, 'finish');
		}.bind(this));
	}
	else {
		var args = Array.prototype.slice.call(arguments);
		EventEmitter.prototype.emit.apply(this, args);
	}
};

module.exports = FlushWritable;
