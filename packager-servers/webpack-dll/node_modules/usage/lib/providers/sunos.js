module.exports = function() {
  var sysinfo = require('../../sysinfo.js')();
  return {
    lookup: lookup
  };

  function lookup(pid, options, callback) {
    if(typeof options == 'function') {
      callback = options;
      options = {};
    }
    options = options || {};

    sysinfo.getUsage(pid, callback);
  };
};
