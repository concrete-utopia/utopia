describe('millisecond', function () {
  'use strict';

  var assume = require('assume')
    , ms = require('./');

  it('should preserve strings that represent numbers', function () {
    assume(ms('100')).to.equal(100);
    assume(ms('10')).to.equal(10);
    assume(ms('1')).to.equal(1);
    assume(ms('0')).to.equal(0);
  });

  it('should bail out if the input string is too long', function () {
    var str = ''
      , i = 0;

    for (; i < 10000; i++) str += '5';
    str += ' minutes';

    assume(ms(str)).to.equal(0);
  });

  it('should return 0 if invalid', function () {
    assume(ms('Hello mom')).to.equal(0);
  });

  it('should parse numbers', function () {
    assume(ms(100)).to.equal(100);
  });

  it('should convert ms to ms', function () {
    assume(ms('100ms')).to.equal(100);
  });

  it('should convert s to ms', function () {
    assume(ms('1s')).to.equal(1000);
    assume(ms('1sec')).to.equal(1000);
    assume(ms('1secs')).to.equal(1000);
    assume(ms('1second')).to.equal(1000);
    assume(ms('1seconds')).to.equal(1000);
  });

  it('should convert from m to ms', function () {
    assume(ms('1m')).to.equal(60000);
    assume(ms('1min')).to.equal(60000);
    assume(ms('1mins')).to.equal(60000);
    assume(ms('1minute')).to.equal(60000);
    assume(ms('1minutes')).to.equal(60000);
  });

  it('should convert from h to ms', function () {
    assume(ms('1h')).to.equal(3600000);
    assume(ms('1hr')).to.equal(3600000);
    assume(ms('1hrs')).to.equal(3600000);
    assume(ms('1hour')).to.equal(3600000);
    assume(ms('1hours')).to.equal(3600000);
  });

  it('should convert d to ms', function () {
    assume(ms('2d')).to.equal(172800000);
    assume(ms('2day')).to.equal(172800000);
    assume(ms('2days')).to.equal(172800000);
  });

  it('should convert w to ms', function () {
    assume(ms('1w')).to.equal(604800000);
    assume(ms('1wk')).to.equal(604800000);
    assume(ms('1wks')).to.equal(604800000);
    assume(ms('1week')).to.equal(604800000);
    assume(ms('1weeks')).to.equal(604800000);
  });

  it('should convert y to ms', function () {
    assume(ms('1y')).to.equal(31536000000);
    assume(ms('1yr')).to.equal(31536000000);
    assume(ms('1yrs')).to.equal(31536000000);
    assume(ms('1year')).to.equal(31536000000);
    assume(ms('1years')).to.equal(31536000000);
  });

  it('should work with decimals', function () {
    assume(ms('1.5h')).to.equal(5400000);
  });

  it('should work with multiple spaces', function () {
    assume(ms('1   s')).to.equal(1000);
  });

  it('should be case-insensitive', function () {
    assume(ms('1.5H')).to.equal(5400000);
  });

  it('should work with numbers starting with .', function () {
    assume(ms('.5ms')).to.equal(0.5);
  });
});
