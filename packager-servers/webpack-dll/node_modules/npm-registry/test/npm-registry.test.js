describe('npm-registry', function () {
  'use strict';

  var chai = require('chai')
    , expect = chai.expect;

  var Registry = require('../')
    , registry = new Registry();

  //
  // The module name we want to use for testing, it shouldn't matter which
  // package we use but having the option to configure this is nice.
  //
  var module = 'eventemitter3';

  it('exposes an object of mirrors', function () {
    expect(Registry.mirrors).to.be.a('object');
    expect(Object.keys(Registry.mirrors).length).to.be.above(1);
  });

  it('sets authorization information when provided with with user/pass', function () {
    expect(registry.authorization).to.equal(undefined);

    var reg = new Registry({ user: 'foo', password: 'bar' });

    expect(reg.authorization).to.not.equal(undefined);
    expect(reg.authorization).to.be.a('string');
  });

  it('defaults to Nodejitsu\'s replica', function () {
    expect(registry.api).to.equal(Registry.mirrors.nodejitsu);
  });

  it('has a customizable registry', function () {
    var reg = new Registry({ registry: Registry.mirrors.strongloop });
    expect(reg.api).to.equal(Registry.mirrors.strongloop);
  });

  it('sets api mirrors by default', function () {
    var mirrors = Object.keys(Registry.mirrors);

    expect(registry.mirrors).to.be.a('array');
    expect(registry.mirrors.length).to.equal(mirrors.length);

    mirrors.forEach(function (key) {
      expect(registry.mirrors).to.contain(Registry.mirrors[key]);
    });
  });

});
