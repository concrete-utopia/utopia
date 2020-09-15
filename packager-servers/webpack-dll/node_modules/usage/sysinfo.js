var format = require('util').format;

module.exports = function() {
  try {
    return require('bindings')('sysinfo');
  } catch(ex) {
    // let's try to find a pre-compiled binary

    var availableVersionPaths = [
      // currently 0.10.x is enough (currently look for Meteor)
      [/v0\.10\..*/, '0.10']
    ];

    var platform = (process.platform == "solaris")? "sunos": process.platform;
    var arch = process.arch;
    var version = process.version;
    var versionPath;

    for(var lc=0; lc<availableVersionPaths.length; lc++) {
      var pathMatcher = availableVersionPaths[lc][0];
      if(pathMatcher.test(version)) {
        versionPath = availableVersionPaths[lc][1];
        break;
      }
    }

    var path = format('./compiled/%s/%s/%s/sysinfo.node',
      platform, arch, versionPath);

    try {
      return require(path);
    } catch(ex) {
      console.warn(
        'usage: cannot find pre-compiled binary for: %s/%s/%s',
        platform, arch, version
      );
      throw ex;
    }
  }
};
