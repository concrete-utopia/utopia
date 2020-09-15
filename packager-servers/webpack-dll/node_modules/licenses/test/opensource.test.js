describe('opensource', function () {
  'use strict';

  var chai = require('chai')
    , expect = chai.expect;

  var opensource = require('../opensource')
    , Parser = require('../parser')
    , parser = new Parser();

  it('exposes the full license info as array', function () {
    expect(opensource.full).to.be.a('array');
  });

  it('exposes the full license info as object', function () {
    expect(opensource.licenses).to.be.a('object');
    expect(Object.keys(opensource.licenses).length).to.equal(opensource.full.length);
  });

  describe('license integrity', function () {
    it('has the required id, name, full fields', function () {
      opensource.full.forEach(function (license) {
        ['name', 'id', 'full'].forEach(function (field) {
          expect(license).to.have.property(field);
          expect(license[field].trim()).to.equal(license[field]);
        });
      });
    });

    it('has a JavaScript friendly id', function () {
      opensource.full.forEach(function (license) {
        var res = (new Function('var license = {}; return license.'+ license.id +' || "foo"'))();

        expect(res).to.equal('foo');
      });
    });

    describe('url', function () {
      var request = require('request');

      opensource.full.filter(function filter(license) {
        return !!license.url;
      }).forEach(function each(license) {
        it('has a valid license url for: '+ license.full, function (next) {
          this.timeout(10000);

          request({
            uri: license.url,
            followRedirect: false,
            headers: {
              'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
              'Accept-Encoding': 'gzip,deflate,sdch',
              'Accept-Language': 'en-US,en;q=0.8',
              'Cache-Control': 'no-cache',
              'Connection': 'keep-alive',
              'DNT': '1',
              'Pragma': 'no-cache',
              'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.152 Safari/537.36'
            }
          }, function done(err, res, body) {
            if (err) return next(err);
            if (res.statusCode !== 200) return next(new Error('Invalid statusCode: '+ res.statusCode));

            next();
          });
        });
      });
    });

    describe('tldr', function () {
      var request = require('request');

      opensource.full.filter(function filter(license) {
        return !!license.tldr;
      }).forEach(function each(license) {
        it('has a valid license tldr for: '+ license.full, function (next) {
          this.timeout(10000);

          request({
            uri: license.tldr,
            followRedirect: false,
            headers: {
              'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
              'Accept-Encoding': 'gzip,deflate,sdch',
              'Accept-Language': 'en-US,en;q=0.8',
              'Cache-Control': 'no-cache',
              'Connection': 'keep-alive',
              'DNT': '1',
              'Pragma': 'no-cache',
              'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/33.0.1750.152 Safari/537.36'
            }
          }, function done(err, res, body) {
            if (err) return next(err);
            if (res.statusCode !== 200) return next(new Error('Invalid statusCode: '+ res.statusCode));

            next();
          });
        });
      });
    });

    describe('file', function () {
      var path = require('path')
        , fs = require('fs');

      opensource.full.filter(function file(license) {
        return !!license.file;
      }).forEach(function each(license) {
        var content = fs.readFileSync(path.join(__dirname, '../licenses/'+ license.file), 'utf-8');

        it(license.name +' can be found through its license file content', function () {
          expect(parser.scan(content)[0]).to.equal(license.name);
        });
      });
    });
  });
});
