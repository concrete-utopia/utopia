var usage   = require('../');
var assert  = require('assert');

suite('Usage', function() {
  test('invalid pid', function(done) {
    usage.lookup(1232323, function(err, result) {
      assert.ok(err.message);
      done();
    });
  });

  test('valid pid', function(done) {
    usage.lookup(process.pid, function(err, result) {
      assert.ifError(err);
      assert.ok(result.cpu >= 0);      
      assert.ok(result.memory > 0);
      assert.ok(result.memoryInfo.rss >= 0);
      assert.ok(result.memoryInfo.vsize >= 0);
      done();
    });
  });

  if(process.platform == 'linux') {
    test('valid pid - with keepHistory', function(done) {
      var options = { keepHistory: true };
      usage.lookup(process.pid, options, function(err, result) {
        assert.ifError(err);
        assert.ok(result.cpu >= 0);
        assert.ok(result.cpuInfo.pcpu >= 0);
        assert.ok(result.cpuInfo.pcpuUser >= 0);
        assert.ok(result.cpuInfo.pcpuSystem >= 0);
        
        assert.ok(result.memory > 0);
        assert.ok(result.memoryInfo.rss > 0);
        assert.ok(result.memoryInfo.vsize > 0);
        
        for(var lc=0; lc<999999; lc++) {
          Math.random();
        }

        setTimeout(function() {
          usage.lookup(process.pid, options, checkCpuTime);
        }, 200);
      });

      function checkCpuTime (err, result) {
        assert.ifError(err);
        assert.ok(result.cpu >= 0);
        assert.ok(result.cpuInfo.pcpu >= 0);
        assert.ok(result.cpuInfo.cpuTime >= 0);
        assert.ok(result.cpuInfo.pcpuUser >= 0);
        assert.ok(result.cpuInfo.pcpuSystem >= 0);
        
        assert.ok(result.memory > 0);
        assert.ok(result.memoryInfo.rss > 0);
        assert.ok(result.memoryInfo.vsize > 0);
        usage.clearHistory();
        
        usage.lookup(process.pid, options, checkCpuTime);
        done();
      }
    });
  }
});
