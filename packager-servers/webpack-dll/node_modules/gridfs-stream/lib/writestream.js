
/**
 * Module dependencies
 */

var util = require('util');
//var Writable  = require('stream').Writable;

// This is a workaround to implement a _flush method for Writable (like for Transform) to emit the 'finish' event only after all data has been flushed to the underlying system (GridFS). See https://www.npmjs.com/package/flushwritable and https://github.com/joyent/node/issues/7348
var FlushWritable = require('flushwritable');

/**
 * expose
 * @ignore
 */

module.exports = exports = GridWriteStream;

/**
 * GridWriteStream
 *
 * @param {Grid} grid
 * @param {Object} options (optional)
 */

function GridWriteStream (grid, options) {
	if (!(this instanceof GridWriteStream))
		return new GridWriteStream(grid, options);

	FlushWritable.call(this);
	this._opened = false;
	this._opening = false;
	this._writable = true;
	this._closing = false;
	this._destroyed = false;
	this._errorEmitted = false;
	this._grid = grid;

	// a bit backwards compatible
	if (typeof options === 'string') {
		options = { filename: options };
	}
	this.options = options || {};
	if(this.options._id) {
		this.id = grid.tryParseObjectId(this.options._id);

		if(!this.id) {
			this.id = this.options._id;
		}
	}

	this.name = this.options.filename;  // This may be undefined, that's okay

	if (!this.id) {
		//_id not passed or unparsable? This is a new file!
		this.id = new grid.mongo.ObjectID();
		this.name = this.name || '';  // A new file needs a name
	}

	this.mode = 'w'; //Mongodb v2 driver have disabled w+ because of possible data corruption. So only allow `w` for now.

	// The value of this.name may be undefined. GridStore treats that as a missing param
	// in the call signature, which is what we want.
	this._store = new grid.mongo.GridStore(grid.db, this.id, this.name, this.mode, this.options);

	this._delayedWrite = null;
	this._delayedFlush = null;
	this._delayedClose = null;

	var self = this;

	self._open();
}

/**
 * Inherit from stream.Writable (FlushWritable for workaround to defer finish until all data flushed)
 * @ignore
 */

util.inherits(GridWriteStream, FlushWritable);

// private api

/**
 * _open
 *
 * @api private
 */

GridWriteStream.prototype._open = function () {
	if (this._opened) return;
	if (this._opening) return;
	this._opening = true;

	var self = this;
	this._store.open(function (err, gs) {
		self._opening = false;
		if (err) return self._error(err);
		self._opened = true;
		self.emit('open');

		// If _close was called during _store opening, then it was delayed until now, so do the close now
		if (self._delayedClose) {
			var closed = self._delayedClose.cb;
			self._delayedClose = null;
			return self._closeInternal(closed);
		}

		// If _flush was called during _store opening, then it was delayed until now, so do the flush now (it's necessarily an empty GridFS file, no _write could have been called and have finished)
		if (self._delayedFlush) {
			var flushed = self._delayedFlush;
			self._delayedFlush = null;
			return self._flushInternal(flushed);
		}

		// If _write was called during _store opening, then it was delayed until now, so do the write now (_flush could not have been called yet as _write has not finished yet)
		if (self._delayedWrite) {
			var delayedWrite = self._delayedWrite;
			self._delayedWrite = null;
			return self._writeInternal(delayedWrite.chunk, delayedWrite.encoding, delayedWrite.done);
		}
	});
}

/**
 * _writeInternal
 *
 * @api private
 */

GridWriteStream.prototype._writeInternal = function (chunk, encoding, done) {
	// If destroy or error no more data will be written.
	if (!this._writable) return;

	var self = this;
	// Write the chunk to the GridStore. The write head automatically moves along with each write.
	this._store.write(chunk, function (err, store) {
		if (err) return self._error(err);

		// Emit the write head position
		self.emit('progress', store.position);

		// We are ready to receive a new chunk from the writestream - call done().
		done();
	});
}

/**
 * _write
 *
 * @api private
 */

GridWriteStream.prototype._write = function (chunk, encoding, done) {
	if (this._opening) {
		// if we are still opening the store, then delay the write until it is open.
		this._delayedWrite = {chunk: chunk, encoding: encoding, done: done};
		return;
	}

	// otherwise, do the write now
	this._writeInternal(chunk, encoding, done);
}

/**
 * _flushInternal
 *
 * @api private
 */

GridWriteStream.prototype._flushInternal = function (flushed) {
	this._close(flushed);
}

/**
 * _flush
 *
 * @api private
 */

GridWriteStream.prototype._flush = function (flushed) {
	// _flush is called when all _write() have finished (even if no _write() was called (empty GridFS file))

	if (this._opening) {
		// if we are still opening the store, then delay the flush until it is open.
		this._delayedFlush = flushed;
		return;
	}

	// otherwise, do the flush now
	this._flushInternal(flushed);
}


/**
 * _closeInternal
 *
 * @api private
 */

GridWriteStream.prototype._closeInternal = function (cb) {
	if (!this._opened) return;
	if (this._closing) return;
	this._closing = true;

	var self = this;
	this._store.close(function (err, file) {
		self._closing = false;
		self._opened = false;
		if (err) return self._error(err);
		self.emit('close', file);

		if (cb) cb();
	});
}

/**
 * _close
 *
 * @api private
 */

GridWriteStream.prototype._close = function _close (cb) {
	if (this._opening) {
		// if we are still opening the store, then delay the close until it is open.
		this._delayedClose = { cb: cb };
		return;
	}

	// otherwise, do the close now
	this._closeInternal(cb);
}

/**
 * _error
 *
 * @api private
 */

GridWriteStream.prototype._error = function _error (err) {
	// Stop receiving more data to write, emit `error` and close the store
	if (this._errorEmitted) return;
	this._errorEmitted = true;

	this._writable = false;
	this.emit('error', err);
	this._close();
}

// public api

/**
 * destroy
 *
 * @api public
 */

GridWriteStream.prototype.destroy = function destroy (err) {
	// Abort the write stream, even if write not completed
	if (this._destroyed) return;
	this._destroyed = true;

	var self = this;
	process.nextTick(function() {
		self._error(err);
	});
}


/**
 * destroySoon
 *
 * @api public
 * @deprecated just use destroy()
 */

GridWriteStream.prototype.destroySoon = function destroySoon () {
	return this.destroy();
};