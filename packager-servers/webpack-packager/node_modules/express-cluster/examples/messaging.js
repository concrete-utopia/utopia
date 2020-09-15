var http = require('http');
var cluster = require('..');

cluster(function() {
  var server = http.createServer(function(req, res) {
    process.send('ok');
    res.end('ok');
  });
  process.on('message', function(msg) {
    console.log('worker with pid', process.pid, 'received', msg, 'from master');
  });
  return server.listen(0xbeef);
}, {
  verbose: true,
  respawn: false,
  workerListener: function(msg) {
    console.log('master with pid', process.pid, 'received', msg, 'from worker');
    this.send('master acked your message');
  }
});
