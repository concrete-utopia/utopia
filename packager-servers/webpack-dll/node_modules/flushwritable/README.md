# FlushWritable [![Build Status](https://travis-ci.org/TomFrost/FlushWritable.svg?branch=master)](https://travis-ci.org/TomFrost/FlushWritable)
A Writable stream that flushes before emitting finish.

Sponsored by [Leadnomics](http://www.leadnomics.com).

## What it is
Node.js's Streams API is a fantastic tool, but has a nagging shortcoming:
while the Transform stream implements a `_flush` method that is called before
its final events are fired, the Writable stream does not.  So if you're
buffering rows to be INSERTed into a SQL table rather than slowly writing one
at a time, or you're buffering bytes for a transfer to S3, there is no way of
flushing those buffers to the target data store before the `finish` event is
emitted.

**FlushWritable is a drop-in replacement for stream.Writable** that implements
a `_flush` call that behaves exactly how Transform._flush does.  It's called
with a callback, waits for the callback to be called, and _then_ fires
`finish` (or `error` if an error was passed).  No additional execution after
the `finish` event, no implementing nonstandard event types, no chaining a
shell Transform stream before the Writable to hijack its `_flush` call.  And
it's fully futureproof against the Node.js team actually adding a `_flush`
method to the native stream.Writable in a later version of Node, so you don't
have to worry about your code breaking on upgrade.

## How does it work?
It's pretty simple.  Writable is an EventEmitter.  FlushWritable extends
Writable and overrides EventEmitter.emit in its own prototype, listening for a
request that `finish` be emitted.  When that comes in, it blocks that event
from emitting, and calls `_flush` if it's defined.

The callback it passes to `_flush` will trigger `finish` to actually be
emitted.  If that callback is called with a truthy first argument, `error` is
emitted instead.  All other events pass right through and are emitted as
expected.  If a future version of node adds a `Writable.prototype._flush`
method, the whole thing short-circuits and native functionality takes over.

## Installation
In your project folder, type:

	npm install flushwritable --save

## Usage
Just extend FlushWritable instead of stream.Writable in your write stream, and
feel free to define a `_flush(cb)` function!

```javascript
var FlushWritable = require('flushwritable'),
    util = require('util');

function MyWriteStream(opts) {
    FlushWritable.call(this, opts);
    this._buffer = [];
}
util.inherits(MyWriteStream, FlushWritable);

MyWriteStream.prototype._flush = function(cb) {
	writeBufferSomewhere(this._buffer, cb);
};

MyWriteStream.prototype._write = function(data, encoding, cb) {
	this._buffer.push(data);
	cb();
};
```

## License
FlushWritable is distributed under the MIT license.

## Credits
FlushWritable was created by Tom Frost at Leadnomics in 2014.
