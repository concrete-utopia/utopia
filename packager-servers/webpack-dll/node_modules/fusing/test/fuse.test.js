describe('fuse', function () {
  'use strict';

  var EventEmitter = require('events').EventEmitter
    , chai = require('chai')
    , fuse = require('../')
    , expect = chai.expect;

  it('exports it self as function', function() {
    expect(fuse).to.be.a('function');
  });

  it('returns the Base', function () {
    function Base() {} function Case() {}

    expect(fuse(Base, Case)).to.equal(Base);
  });

  it('does optional inherit', function () {
    function Base() {}

    expect(fuse(Base).prototype).to.equal(Base.prototype);
  });

  it('exposes the extend method', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    expect(Base.extend).to.be.a('function');
  });

  it('exposes the mixin method', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    expect(Base.prototype.mixin).to.be.a('function');
  });

  it('exposes the merge method', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    expect(Base.prototype.merge).to.be.a('function');
  });

  it('adds writable and readable methods to the class', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    expect(Base.writable).to.be.a('function');
    expect(Base.readable).to.be.a('function');

    expect(Base.prototype.foo).to.equal(undefined);
    expect(Base.prototype.bar).to.equal(undefined);

    Base.readable('foo', 'foo');
    Base.writable('bar', 'bar');

    expect(Base.prototype.foo).to.equal('foo');
    expect(Base.prototype.bar).to.equal('bar');
  });

  it('sets the constructor back to the Base', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    expect(Base.prototype.constructor).to.equal(Base);
    expect(new Base()).to.be.instanceOf(Base);
    expect(new Base()).to.be.instanceOf(Case);
  });

  it('doesnt add the default methods if we dont want it', function () {
    function Base() {} function Case() {}
    fuse(Base, Case, { defaults: false });

    var base = new Base();

    expect(base).to.be.instanceOf(Base);
    expect(base).to.be.instanceOf(Case);

    expect(base.emits).to.equal(undefined);
    expect(base.mixin).to.equal(undefined);
  });

  it('allows disabling of individual methods', function () {
    function Base() {} function Case() {}
    fuse(Base, Case, { mixin: false });

    var base = new Base();

    expect(base).to.be.instanceOf(Base);
    expect(base).to.be.instanceOf(Case);

    expect(base.emits).to.be.a('function');
    expect(base.mixin).to.equal(undefined);
  });

  it('accepts options as second argument', function () {
    function Base() {}
    fuse(Base, { mixin: false });

    var base = new Base();

    expect(base.emits).to.be.a('function');
    expect(base.mixin).to.equal(undefined);
  });

  describe('.emits', function () {
    it('adds the emits function to the prototype', function () {
      function Base() {} function Case() {}
      fuse(Base, Case);

      expect(Base.prototype.emits).to.be.a('function');
    });

    it('returns a function that emits the given event', function (done) {
      function Base() {}
      fuse(Base, EventEmitter);

      var base = new Base()
        , emits = base.emits('event');

      base.once('event', function (data) {
        expect(data).to.equal('foo');
        done();
      });

      emits('foo');
    });

    it('accepts a parser method that transforms the emitted values', function (done) {
      function Base() {}
      fuse(Base, EventEmitter);

      var base = new Base()
        , emits = base.emits('event', function (arg) {
            expect(arg).to.equal('bar');
            return 'foo';
          });

      base.once('event', function (data) {
        expect(data).to.equal('foo');
        done();
      });

      emits('bar');
    });

    it('curries arguments', function (done) {
      function Base() {}
      fuse(Base, EventEmitter);

      var base = new Base()
        , emits = base.emits('event', 'foo');

      base.once('event', function (data, foo) {
        expect(data).to.equal('foo');
        expect(foo).to.equal('bar');
        done();
      });

      emits('bar');
    });

    it('can prefix events', function (done) {
      function Base() {}
      fuse(Base, EventEmitter, {
        prefix: 'foo::'
      });

      var base = new Base()
        , emits = base.emits('event', 'foo');

      base.once('foo::event', function (data, foo) {
        expect(data).to.equal('foo');
        expect(foo).to.equal('bar');
        done();
      });

      emits('bar');
    });
  });

  describe('.fuse', function () {
    it('adds a .fuse method to the prototype', function () {
      function Base() {} function Case() {}
      fuse(Base, Case);

      expect(Base.prototype.fuse).to.be.a('function');
    });

    it('initialises the inherited constructor', function (done) {
      function Base() {
        this.fuse();

        expect(this.bar).to.equal('foo');
        done();
      }

      function Case() {
        this.bar = 'foo';
      }

      fuse(Base, Case);
      new Base();
    });

    it('adds readable and writable props', function (done) {
      function Base() {
        this.fuse();

        expect(this.writable).to.be.a('function');
        expect(this.readable).to.be.a('function');
        expect(this.foo).to.equal(undefined);

        this.readable('foo', 'bar');
        this.writable('bar', 'foo');

        expect(this.foo).to.equal('bar');
        expect(this.bar).to.equal('foo');

        expect(Object.keys(this).length).to.equal(0);

        this.bar = 'bar';
        expect(this.bar).to.equal('bar');

        try { this.foo = 'foo'; }
        catch (e) { done(); }
      }

      function Case() {}

      expect(Base.prototype.readable).to.equal(undefined);
      expect(Base.prototype.writable).to.equal(undefined);

      fuse(Base, Case);
      new Base();
    });

    it('applies the arguments to the inherited constructor', function (done) {
      function Base() {
        this.fuse(arguments);

        expect(this.bar).to.equal('foo');
        done();
      }

      function Case(foo, bar) {
        expect(foo).to.equal('foo');
        expect(bar).to.equal('bar');

        this.bar = 'foo';
      }

      fuse(Base, Case);
      new Base('foo', 'bar');
    });
  });
});
