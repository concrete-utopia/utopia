/*
Copyright (c) 2015, Yahoo! Inc. All rights reserved.
Copyrights licensed under the New BSD License.
See the accompanying LICENSE file for terms.

Author: Stuart Larsen <stuartlarsen@yahoo-inc.com>
*/
var express = require('express');

var redis = require('redis'),
    client = redis.createClient();
var async = require('async');

var xssFilters = require('../../dist/xss-filters');

var app = express();

function buildResponse(id, fuzz) {
  return "<html> <body> <h1> Fuzz Test! </h1>" +

  "<script>window.alert = function(msg) { console.log(\"" + id + ":\"+ msg) } </script>" +
  "<script>window.prompt = function(msg) { console.log(\"" + id + ":\"+ msg) } </script>" +

  // "<script>alert('xssssss')</script>" +
  // "<SCRIPT SRC='http://yahoo.com'></SCRIPT>" +

  "<div style='border: 1px solid'> " +
    "<h2> xssFilters.inHTMLData(fuzz) </h1>" +
    "<div> " + xssFilters.inHTMLData(fuzz) + " </div> " +
  "</div>" +


  "<div style='border: 1px solid'> " +
  "<h2> xssFilters.inHTMLComment(fuzz) </h2>" +
  "<!-- " + xssFilters.inHTMLComment(fuzz) + " --> " +
  "</div>" +


  "<div style='border: 1px solid'> " +
  "<h2> xssFilters.inSingleQuotedAttr(fuzz) </h2> " +
  "<input value='" + xssFilters.inSingleQuotedAttr(fuzz) + "'/>" +
  "</div>" +


  "<div style='border: 1px solid'> " +
  "<h2> xssFilters.inDoubleQuotedAttr(fuzz) </h2> "+
  "<input value=\"" + xssFilters.inDoubleQuotedAttr(fuzz) + "\"/>" +
  "</div>" +


  "<div style='border: 1px solid'> " +
  "<h2> xssFilters.inUnQuotedAttr(fuzz) </h2> " +
  "<input value=" + xssFilters.inUnQuotedAttr(fuzz) + "'/>" +
  "</body> </html>";
}

function getTestcase(id, next) {
  client.get('testcases:'+id, function(err, response) {
    next(err, response);
  });
}

app.get('/', function(req, res) {
  res.send('okay');

});

app.get('/testcase/:id', function(req, res) {
  console.log("/testcase/" + req.params.id);

  getTestcase(req.params.id, function(err, response) {
    var out = buildResponse(req.params.id, response);
    console.log(req.params.id, response);
    res.send(out);
  });
});

console.log("Listening on :1339");
app.listen(1339, function(err) { if (err){ console.log(err);}});
