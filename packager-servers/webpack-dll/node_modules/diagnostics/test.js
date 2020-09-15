describe('diagnostics', function () {
  'use strict';

  var assume = require('assume')
    , debug = require('./');

  beforeEach(function () {
    process.env.DEBUG = '';
    process.env.DIAGNOSTICS = '';
  });

  it('is exposed as function', function () {
    assume(debug).to.be.a('function');
  });

  describe('.to', function (next) {
    it('globally overrides the stream', function () {
      debug.to({
        write: function write(line) {
          assume(line).to.contain('foo');
          assume(line).to.contain('bar');

          debug.to(process.stdout);
          next();
        }
      });

      var log = debug('foo');
      log('bar');
    });
  });
});
