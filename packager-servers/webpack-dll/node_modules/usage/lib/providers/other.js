module.exports = function(sysinfo) {
  return {
    lookup: lookup
  };

  function lookup(pid, options, callback) {
    if(typeof options == 'function') {
      callback = options;
      options = {};
    }
    options = options || {};

    var helpUrl = "https://github.com/arunoda/node-usage/issues";
    var message = 'Unsupported OS. Please submit an issue: ';
    callback(new Error(message + helpUrl));
  }
};
