/*
 * FlushWritable
 * Copyright 2014 Tom Frost
 */

var FlushWritable = require('../lib/FlushWritable'),
	Readable = require('stream').Readable,
	util = require('util'),
	should = require('should');

function TestWritable() {
	FlushWritable.call(this);
}
util.inherits(TestWritable, FlushWritable);

TestWritable.prototype._flush = function(cb) {
	this.flushCalled = true;
	setTimeout(function() {
		cb(this.err);
	}.bind(this), 10);
};

TestWritable.prototype._write = function(data, encoding, cb) {
	cb();
};

function TestReadable() {
	this.i = 0;
	Readable.call(this);
}
util.inherits(TestReadable, Readable);

TestReadable.prototype._read = function() {
	if (this.i++ === 2)
		this.push(null);
	else
		this.push('foo');
};

describe('FlushWritable', function() {
	it('should call _flush prior to emitting finish', function(done) {
		var r = new TestReadable(),
			w = new TestWritable(),
			finished = false;
		w.on('finish', function() {
			finished = true;
		});
		r.pipe(w);
		setTimeout(function() {
			w.should.have.property('flushCalled');
			finished.should.eql(false);
			setTimeout(function() {
				finished.should.eql(true);
				done();
			}, 10);
		}, 5);
	});
	it('should emit error instead of finish for errors in cb', function(done) {
		var r = new TestReadable(),
			w = new TestWritable(),
			finished = false,
			errored = false;
		w.on('finish', function() {
			finished = true;
		});
		w.on('error', function() {
			errored = true;
		});
		w.err = new Error('bar');
		r.pipe(w);
		setTimeout(function() {
			finished.should.eql(false);
			errored.should.eql(true);
			done();
		}, 15);
	});
	it('should finish immediately if no _flush is defined', function(done) {
		var r = new TestReadable(),
			w = new TestWritable(),
			finished = false;
		w.on('finish', function() {
			finished = true;
		});
		w._flush = undefined;
		r.pipe(w);
		setTimeout(function() {
			finished.should.eql(true);
			done();
		}, 5);
	});
});
