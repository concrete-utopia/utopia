var net = require('net');
var cluster = require('..');

cluster(function() {
  var server = net.createServer(function(conn) {
    conn.end('hi!\n');
  });
  return server.listen(0xbeef);
}, {verbose: true, respawn: false});
