var http = require('http');
var cluster = require('..');

cluster(function() {
  var server = http.createServer(function(req, res) {
    res.end('ok');
  });
  return server.listen(0xbeef);
}, {verbose: true, respawn: false});
