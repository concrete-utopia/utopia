describe('fuse', function () {
  'use strict';

  var EventEmitter = require('events').EventEmitter
    , assume = require('assume')
    , fuse = require('./');

  it('exports it self as function', function() {
    assume(fuse).to.be.a('function');
  });

  it('returns the Base', function () {
    function Base() {} function Case() {}

    assume(fuse(Base, Case)).to.equal(Base);
  });

  it('does optional inherit', function () {
    function Base() {}

    assume(fuse(Base).prototype).to.equal(Base.prototype);
  });

  it('exposes the extend method', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    assume(Base.extend).to.be.a('function');
  });

  it('exposes the mixin method', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    assume(Base.prototype.mixin).to.be.a('function');
  });

  it('exposes the merge method', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    assume(Base.prototype.merge).to.be.a('function');
  });

  it('adds writable and readable methods to the class', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    assume(Base.writable).to.be.a('function');
    assume(Base.readable).to.be.a('function');

    assume(Base.prototype.foo).to.equal(undefined);
    assume(Base.prototype.bar).to.equal(undefined);

    Base.readable('foo', 'foo');
    Base.writable('bar', 'bar');

    assume(Base.prototype.foo).to.equal('foo');
    assume(Base.prototype.bar).to.equal('bar');
  });

  it('adds get and set methods to the class', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    assume(Base.get).to.be.a('function');
    assume(Base.set).to.be.a('function');

    assume(Base.prototype.foo).to.equal(undefined);
    assume(Base.prototype.bar).to.equal(undefined);

    var x = 'bar';
    Base.get('foo', function () { return 'foo'; });
    Base.set('bar', function () { return x; }, function (y) { return x = y; });

    var base = new Base();

    assume(base.foo).to.equal('foo');
    assume(base.bar).to.equal('bar');

    x = 'foo';
    assume(base.bar).to.equal('foo');

    base.bar = 'baz';
    assume(base.bar).to.equal('baz');
  });

  it('sets the constructor back to the Base', function () {
    function Base() {} function Case() {}
    fuse(Base, Case);

    assume(Base.prototype.constructor).to.equal(Base);
    assume(new Base()).to.be.instanceOf(Base);
    assume(new Base()).to.be.instanceOf(Case);
  });

  it('doesnt add the default methods if we dont want it', function () {
    function Base() {} function Case() {}
    fuse(Base, Case, { defaults: false });

    var base = new Base();

    assume(base).to.be.instanceOf(Base);
    assume(base).to.be.instanceOf(Case);

    assume(base.emits).to.equal(undefined);
    assume(base.mixin).to.equal(undefined);
  });

  it('allows disabling of individual methods', function () {
    function Base() {} function Case() {}
    fuse(Base, Case, { mixin: false });

    var base = new Base();

    assume(base).to.be.instanceOf(Base);
    assume(base).to.be.instanceOf(Case);

    assume(base.emits).to.be.a('function');
    assume(base.mixin).to.equal(undefined);
  });

  it('accepts options as second argument', function () {
    function Base() {}
    fuse(Base, { mixin: false });

    var base = new Base();

    assume(base.emits).to.be.a('function');
    assume(base.mixin).to.equal(undefined);
  });

  describe('.emits', function () {
    it('adds the emits function to the prototype', function () {
      function Base() {} function Case() {}
      fuse(Base, Case);

      assume(Base.prototype.emits).to.be.a('function');
    });

    it('returns a function that emits the given event', function (done) {
      function Base() {}
      fuse(Base, EventEmitter);

      var base = new Base()
        , emits = base.emits('event');

      base.once('event', function (data) {
        assume(data).to.equal('foo');
        done();
      });

      emits('foo');
    });

    it('accepts a parser method that transforms the emitted values', function (done) {
      function Base() {}
      fuse(Base, EventEmitter);

      var base = new Base()
        , emits = base.emits('event', function (arg) {
            assume(arg).to.equal('bar');
            return 'foo';
          });

      base.once('event', function (data) {
        assume(data).to.equal('foo');
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
        assume(data).to.equal('foo');
        assume(foo).to.equal('bar');
        done();
      });

      emits('bar');
    });
  });

  describe('.fuse', function () {
    it('adds a .fuse method to the prototype', function () {
      function Base() {} function Case() {}
      fuse(Base, Case);

      assume(Base.prototype.fuse).to.be.a('function');
    });

    it('initialises the inherited constructor', function (done) {
      function Base() {
        this.fuse();

        assume(this.bar).to.equal('foo');
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

        assume(this.writable).to.be.a('function');
        assume(this.readable).to.be.a('function');
        assume(this.foo).to.equal(undefined);

        this.readable('foo', 'bar');
        this.writable('bar', 'foo');

        assume(this.foo).to.equal('bar');
        assume(this.bar).to.equal('foo');

        assume(Object.keys(this).length).to.equal(0);

        this.bar = 'bar';
        assume(this.bar).to.equal('bar');

        try { this.foo = 'foo'; }
        catch (e) { done(); }
      }

      function Case() {}

      assume(Base.prototype.readable).to.equal(undefined);
      assume(Base.prototype.writable).to.equal(undefined);

      fuse(Base, Case);
      new Base();
    });

    it('applies the arguments to the inherited constructor', function (done) {
      function Base() {
        this.fuse(arguments);

        assume(this.bar).to.equal('foo');
        done();
      }

      function Case(foo, bar) {
        assume(foo).to.equal('foo');
        assume(bar).to.equal('bar');

        this.bar = 'foo';
      }

      fuse(Base, Case);
      new Base('foo', 'bar');
    });
  });

  it('allows fusing a fused fuse', function () {
    function Bar() {
      console.log('bar');
    }

    function Fuse(x) {
      if (!x) throw new Error('missing args');

      this.fuse();
      console.log(x);
    }

    fuse(Fuse, Bar);

    function Foo() {
      this.fuse(['hi']);
    }

    fuse(Foo, Fuse);

    var y = new Foo();
  });
});
