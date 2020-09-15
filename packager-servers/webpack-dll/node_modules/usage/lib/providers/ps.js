var exec = require('child_process').exec;

module.exports = function() {
  return {
    lookup: lookup
  };

  function lookup(pid, options, callback) {
    if(typeof options == 'function') {
      callback = options;
      options = {};
    }
    options = options || {};

    exec('ps -o "rss,vsize,pcpu" -p ' + pid, function(err, stdout, stderr) {
      if (err || stderr) return callback(err || stderr);

      try {
        callback(null, parsePS(pid, stdout));
      } catch(ex) {
        callback(ex);
      }
    });
  }
};

function parsePS(pid, output) {
  var lines = output.trim().split('\n');
  if (lines.length !== 2) {
    throw new Error('INVALID_PID');
  }

  var matcher = /[ ]*([0-9]*)[ ]*([0-9]*)[ ]*([0-9\.]*)/;
  var result = lines[1].match(matcher);

  if(result) {
    return {
      memory: parseInt(result[1]) * 1024,
      memoryInfo: {
        rss: parseFloat(result[1]) * 1024,
        vsize: parseFloat(result[2]) * 1024
      },
      cpu: parseFloat(result[3])
    };
  } else {
    throw new Error('PS_PARSE_ERROR');
  }
}
