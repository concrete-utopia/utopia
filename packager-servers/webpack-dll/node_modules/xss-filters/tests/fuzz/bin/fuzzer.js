/*
Copyright (c) 2015, Yahoo! Inc. All rights reserved.
Copyrights licensed under the New BSD License.
See the accompanying LICENSE file for terms.

Author: Stuart Larsen <stuartlarsen@yahoo-inc.com>
*/
var async = require('async');
var redis = require("redis"),
    client = redis.createClient();
var Browser = require('zombie');

var URL = "http://localhost:1339";

var fs = require('fs');
var seeds = fs.readFileSync('seed.xss').toString().split("\n");

Browser.extend(function(browser) {
  browser.on('console', function(level, message) {
    client.sadd("injections", message);
  });

  browser.on('xhr', function(e, url) {
    client.sadd("injections", e + url);
  });

  browser.on('redirect', function(request, response) {
    client.sadd('injections', request + " " + response);
  });
});

String.prototype.replaceAt=function(index, character) {
    return this.substr(0, index) + character + this.substr(index+character.length);
};

function randomTestcase() {
  return seeds[Math.floor(Math.random() * seeds.length)];
}

function insertIntoRedis(testcase, next) {
  async.auto({
    id: function(next) {
      client.incr("testcases_counter", next);
    },

    insert: ["id", function(next, results) {
      var id = results.id;

      client.set("testcases:" + id, testcase, next);

    }]
  }, next);
}

function runHeadless(id, next) {
  Browser.visit(URL + "/testcase/" + id, function (e, browser) {
      next();
  });
}

var unicodeCharacters = 65000; // size of first unicode bmp

async.forever(function(next) {
  var testcase = randomTestcase();
  async.timesSeries(testcase.length, function(i, next) {
    async.timesSeries(unicodeCharacters, function(s, next) {

      var testcase_tmp = testcase;
      var specialC = String.fromCharCode(s);

      testcase_tmp= testcase_tmp.replaceAt(i, specialC);

      console.log(i, si, testcase_tmp);
      insertIntoRedis(testcase_tmp, function(err, results) {
        var id = results.id;
        runHeadless(id, next);
      });
    }, next);
  }, next);
}, function() {
  console.log('all done!');
});
