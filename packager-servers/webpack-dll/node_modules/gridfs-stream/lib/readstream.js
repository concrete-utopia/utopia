
/**
 * Module dependencies
 */

var util = require('util');
var Readable  = require('stream').Readable;

/**
 * expose
 * @ignore
 */

module.exports = exports = GridReadStream;

/**
 * GridReadStream
 *
 * @param {Grid} grid
 * @param {Object} options
 */

function GridReadStream (grid, options) {
  if (!(this instanceof GridReadStream))
    return new GridReadStream(grid, options);

  Readable.call(this);
  this._opened = false;
  this._opening = false;
  this._closing = false;
  this._end = false;
  this._needToPush = false;

  this._grid = grid;

  // a bit backwards compatible
  if (typeof options === 'string') {
    options = { filename: options };
  }

  this.options = options || {};

  if(options._id) {
    this.id = grid.tryParseObjectId(options._id);

    if(!this.id) {
      this.id = options._id;
    }
  }

  this.name = this.options.filename || '';
  this.mode = 'r';

  // If chunk size specified use it for read chunk size otherwise default to 255k (GridStore default). chunkSize and chunk_size in mongodb api so check both.
  this._chunkSize = this.options.chunkSize || this.options.chunk_size || 1024 * 255;

  this.range = this.options.range || { startPos: 0, endPos: undefined };
  if (typeof(this.range.startPos) === 'undefined') {
    this.range.startPos = 0;
  }

  this._currentPos = this.range.startPos;

  var options = {};
  for (var i in this.options) { options[i] = this.options[i]; }
  options.root || (options.root = this._grid.curCol);

  this._store = new grid.mongo.GridStore(grid.db, this.id || new grid.mongo.ObjectID(), this.name, this.mode, options);
  // Workaround for Gridstore issue https://github.com/mongodb/node-mongodb-native/pull/930
  if (!this.id) {
    // var REFERENCE_BY_FILENAME = 0,
    this._store.referenceBy = 0;
  }

  var self = this;

  //Close the store once `end` received
  this.on('end', function() {
    self._end = true;
    self._close()
  });

  process.nextTick(function() {
    self._open();
  });
}

/**
 * Inherit from stream.Readable
 * @ignore
 */

util.inherits(GridReadStream, Readable);

/**
 * _open
 *
 * @api private
 */

GridReadStream.prototype._open = function _open () {
  if (this._opening) return;
  this._opening = true;

  var self = this;

  // Open the sore
  this._store.open(function (err, gs) {
    if (err) return self._error(err);

    // Find the length of the file by setting the head to the end of the file and requesting the position
    self._store.seek(0, self._grid.mongo.GridStore.IO_SEEK_END, function(err) {
        if (err) return self._error(err);

        // Request the position of the end of the file
        self._store.tell(function(err, position) {
        if (err) return self._error(err);

            // Calculate the correct end position either from EOF or end of range. Also handle incorrect range request.
            if (!self.range.endPos || self.range.endPos > position-1) {self.range.endPos = position - 1};

            // Set the read head to the beginning of the file or start position if specified
            self._store.seek(self.range.startPos, self._grid.mongo.GridStore.IO_SEEK_SET, function(err) {
              if (err) return self._error(err);

              // The store is now open
              self.emit('open');
              self._opened = true;

              // If `_read()` was already called then we need to start pushing data to the stream. Otherwise `_read()` will handle this once called from stream.
              if (self._needToPush) self._push();
            });
        });
    });
  });
}

/**
 * _read
 *
 * @api private
 */

// `_read()` will be called when the stream wants to pull more data in
// The advisory `size` argument is ignored in this case and user specified use or default to 255kk.
GridReadStream.prototype._read = function _read (size) {
  var self = this;

  // Set `_needToPush` to true because the store may still be closed if data is immediately piped. Once the store is open `_needToPush` is checked and _push() called if necessary.
  self._needToPush = true;

  // The store must be open
  if (!this._opened) return;

  // Read data from GridStore and push to stream
  self._push();
}

/**
 * _push
 *
 * @api private
 */

GridReadStream.prototype._push = function _push () {
  var self = this;

  // Do not continue if the store is closed
  if (!this._opened) return self._error('Unable to push data. Expected gridstore to be open');

  // Check if EOF, if the full requested range has been pushed or if the stream must be destroyed. If so than push EOF-signalling `null` chunk
  if ( !this._store.eof() && (self._currentPos <= self.range.endPos) && !this._end) {

    // Determine the chunk size for the read from GridStore
    // Use default chunk size or user specified
    var readChunkSize = self._chunkSize
    // Override the chunk size if the chunk size is more than the size that is left until EOF/range
    if (self.range.endPos-self._currentPos < self._chunkSize) {readChunkSize = self.range.endPos - self._currentPos + 1};

    // Read the chunk from GridSore. Head moves automatically after each read.
    self._store.read(readChunkSize,function(err, data) {

      // If error stop and close the store
      if (err) return self._error(err);

      // Advance the current position of the read head
      self._currentPos += data.length;

      // Push data
      if (!self._end) self.push(data)
    })


  } else {
    // Push EOF-signalling `null` chunk
    this._end = true;
    self.push(null);
  }
}

/**
 * _close
 *
 * @api private
 */

GridReadStream.prototype._close = function _close () {
  var self = this;
  if (!self._opened) return;
  if (self._closing) return;
  this._closing = true;

  // Close the store and emit `close` event
  self._store.close(function (err) {
    if (err) return self._error(err);
    self.emit('close');
  });
}

/**
 * _error
 *
 * @api private
 */

GridReadStream.prototype._error = function _error (err) {
  // Set end true so that no further reads from GridSotre are possible and close the store
  this._end = true;

  // Emit the error event
  this.emit('error', err);

  // Close the gridsore if an error is received.
  this._close()
}

/**
 * destroy
 *
 * @api public
 */

GridReadStream.prototype.destroy = function destroy () {
  // Set end true so that no further reads from GridSotre are possible and close the store
  this._end = true;
  this._close();
}
