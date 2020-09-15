describe('Assign', function () {
  'use strict';

  var Assignment = require('./')
    , assume = require('assume');

  it('is exported as a function', function () {
    assume(Assignment).is.a('function');
  });

  it('doenst require the new keyword to construct', function () {
    var assign = Assignment();

    assume(assign).is.instanceOf(Assignment);
  });

  it('is an Stream instance', function () {
    assume(new Assignment()).is.instanceOf(require('stream'));
  });

  describe('constructor', function () {
    var obj = {};
    var assign = new Assignment(obj);

    it('sets the context argument as `and`', function () {
      assume(assign.and).equals(obj);
    });

    it('defaults to a `nope` function when none is provided', function () {
      assume(assign.fn).is.a('function');
    });

    it('calls the supplied callback with the supplied context', function (next) {
      assign.finally(function () {
        assume(this).equals(obj);
        next();
      });

      assign.end();
    });
  });

  describe('.length', function () {
    it('indicates the amount of writes we\'ve received', function (done) {
      var assign = new Assignment(done);

      assume(assign.length).equals(0);
      assign.write(1);

      assume(assign.length).equals(1);
      assign.write(1);

      assume(assign.length).equals(2);
      assign.end();
    });
  });

  describe('#end', function () {
    it('does not write undefined when called without data', function (next) {
      var assign = new Assignment(function (err, data) {
        if (err) return next(err);
        assume(data).to.have.length(0);

        next();
      });

      assign.end();
    });
  });

  describe('#add', function () {
    it('adds an extra argument', function (next) {
      var assign = new Assignment(function (err, data, foo) {
        assume(data).to.have.length(0);
        assume(foo).equals('foo');

        next();
      });

      assign.add('foo');
      assign.end();
    });

    it('supports multiple arguments', function (next) {
      var assign = new Assignment(function (err, data, foo, bar) {
        assume(data).to.have.length(0);
        assume(foo).equals('foo');
        assume(bar).equals(111);

        next();
      });

      assign.add('foo');
      assign.add(111);
      assign.end();
    });
  });

  describe('#filter', function () {
    it('receives the written data', function (done) {
      var assign = new Assignment(function (err, data) {
        assume(data).is.a('array');
        assume(data).to.have.length(2);
        assume(data[0]).equals('foo');
        assume(data[1]).equals('foo');

        done(err);
      });

      assign.filter(function map(data) {
        return 'foo' === data;
      });

      assign.write('foo');
      assign.write('foo');
      assign.write('bar', {
        end: true
      });
    });

    it('allows multiple filter operations', function (done) {
      var assign = new Assignment(function (err, data) {
        assume(data).to.have.length(1);
        assume(data[0]).equals(true);

        done(err);
      });

      assign.filter(Boolean);
      assign.filter(function map(data) {
        return true === data;
      });

      assign.write(0);
      assign.write(1);
      assign.write(false);
      assign.write(true);
      assign.write(undefined);
      assign.write(null, {
        end: true
      });
    });
  });

  describe('#map', function () {
    it('receives the written data', function (done) {
      var assign = new Assignment(function (err, data) {
        assume(data).is.a('array');
        assume(data).to.have.length(2);
        assume(data[0]).equals('foo');
        assume(data[1]).equals('foo');

        done(err);
      });

      assign.map(function map(data) {
        assume(data.foo).includes('bar');
        return 'foo';
      });

      assign.write({ foo: 'bar' });
      assign.write({ foo: 'barmitswa' }, {
        end: true
      });
    });

    it('allows multiple map operations', function (done) {
      var assign = new Assignment(function (err, data) {
        assume(data[0]).equals('bar');
        assume(data[1]).equals('bar');

        done(err);
      });

      assign.map(function map(data) {
        return 'foo';
      });

      assign.map(function map(data) {
        assume(data).equals('foo');
        return 'bar';
      });

      assign.write({ foo: 'bar' });
      assign.write({ foo: 'barmitswa' }, {
        end: true
      });
    });

    describe('.async', function () {
      it('processes the results async', function (done) {
        var assign = new Assignment(function (err, data) {
          assume(data[0]).equals('bar');

          done(err);
        });

        assign.async.map(function (data, index, next) {
          setTimeout(function () {
            next(undefined, data.foo);
          }, 10);
        });

        assign.write({ foo: 'bar'}, {
          end: true
        });
      });

      it('processes and combines data in order', function (done) {
        var assign = new Assignment(function (err, data) {
          assume(data[0]).equals(0);
          assume(data[1]).equals(1);

          done(err);
        });

        assign.async.map(function (data, index, next) {
          if (1 === index) return setTimeout(function () {
            next(undefined, index);
          }, 100);

          next(undefined, index);
        });

        assign.write('foo');
        assign.write({foo: 'bar' }, {
          end: true
        });
      });
    });
  });
});
