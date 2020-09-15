var express = require('express');
var cluster = require('..');

cluster({count: 5, verbose: true}, function() {
  var app = express();
  app.get('/', function(req, res) {
    res.send('ok');
  });
  return app.listen(0xbeef);
});
