describe('.downloads', function () {
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

  it('has a downloads endpoint', function () {
    expect(registry.downloads).to.be.a('object');
  });

  describe('#totals', function () {
    it('retrieves a modules download stats', function (next) {
      registry.downloads.totals('last-week', module, function (err, data) {
        data = Array.isArray(data) ? data[0] : data;
        if (err) return next(err);

        expect(data.package).to.equal(module);
        expect(data.downloads).to.be.a('number');

        next();
      });
    });

    it('receives download stats for all modules', function (next) {
      registry.downloads.totals('last-week', function (err, data) {
        data = Array.isArray(data) ? data[0] : data;
        if (err) return next(err);

        expect(data.downloads).to.be.a('number');

        next();
      });
    });
  });

  describe('#range', function () {
    it('retrieves a modules download stats', function (next) {
      registry.downloads.range('last-week', module, function (err, data) {
        data = Array.isArray(data) ? data[0] : data;
        if (err) return next(err);

        expect(data.package).to.equal(module);
        expect(data.downloads).to.be.a('array');
        expect(data.downloads).to.have.length(7);

        data.downloads.forEach(function (data) {
          expect(data.downloads).to.be.a('number');
        });

        next();
      });
    });

    it('receives download stats for all modules', function (next) {
      registry.downloads.range('last-week', function (err, data) {
        data = Array.isArray(data) ? data[0] : data;
        if (err) return next(err);

        expect(data.downloads).to.be.a('array');
        expect(data.downloads).to.have.length(7);

        data.downloads.forEach(function (data) {
          expect(data.downloads).to.be.a('number');
        });

        next();
      });
    });
  });
});
