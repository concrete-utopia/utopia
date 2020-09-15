var providers = {
  'linux': ['linux', 'ps'],
  'sunos': ['sunos', 'ps'],
  'darwin': ['ps'],
  'freebsd': ['ps'],
  'win': ['other']
};

var format = require('util').format;

module.exports = function() {
  var platform = (process.platform == "solaris")? "sunos": process.platform;
  var potentialProviders = providers[platform];

  if(!potentialProviders) {
    return require('./other')();
  } else {
    for(var lc=0; lc<potentialProviders.length; lc++) {
      var providerName = potentialProviders[lc];
      try {
        var provider = require("./" + providerName);
        return provider();
      } catch (ex) {
        var message = "usage: failed loading provider `%s`";
        console.warn(format(message, providerName));
      }
    }

    console.warn("usage: no potential provider found!")
    return require('./other')();
  }
}
