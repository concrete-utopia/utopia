'use strict';

var argh = require('argh').argv
  , all = require('./all.json')
  , licenses = require('../')
  , async = require('async');

//
// Check if we need to continue where we last started. The package name will be
// in the
//
if (argh.continue) {
  var index = all.indexOf(argh.continue);

  if (index === -1) throw new Error('Sorry, cant slice and dice data: '+ argh.continue +' is not a valid package name');

  //
  // Slice and dice the data.
  //
  all = all.slice(index);
}

//
// Allows us to run against a maximum amount of package names.
//
if (argh.end) all = all.slice(0, argh.end);

//
// Some stats about the parsing process.
//
var stats = {
  packages: all.length,
  detected: 0,
  failed: 0
};

//
// Stores the parsed packages and the licenses we've detected.
//
var parsed = {};

//
// Stores the parsed packages that we were unable to detect.
//
var failed = [];

console.log('');
console.log('Detecting the license of %s packages', all.length);
if (argh.continue) console.log('Starting with package: %s', argh.continue);
console.log('');

//
// Run, test all the packages.
//
async.eachSeries(all, function parse(name, next) {
  if (argh.debug) console.log('Starting to parse: \x1B[36m%s\x1B[39m', name);

  licenses(name, {
    order: argh.order ? argh.order.split(',') : ['npm', 'content', 'github'],
    registry: argh.registry
  }, function detected(err, licenses, using) {
    if (err) return next(err);

    if (!licenses || !licenses.length) {
      console.log('Unable to detect license for: ', name);
      failed.push(name);
      stats.failed++;
    } else {
      console.log('Package \x1B[36m%s\x1B[39m is licensed under \x1B[32m%s\x1B[39m using %s', name, licenses, using);
      parsed[name] = { licenses: licenses, using: using };
      stats.detected++;
    }

    next();
  });
}, function done(err) {
  if (err) throw err;

  console.log('');
  console.log('Parsed all packages, storing results in results.json');
  console.log('');
  require('fs').writeFileSync(__dirname +'/results.json', JSON.stringify(
    parsed,
    null,
    2
  ));
  require('fs').writeFileSync(__dirname +'/failed.json', JSON.stringify(
    failed,
    null,
    2
  ));

  console.log('');
  console.log('stats:', stats);
  console.log('');
});
