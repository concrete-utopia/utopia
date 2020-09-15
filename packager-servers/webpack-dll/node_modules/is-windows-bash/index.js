fs = require('fs')

function isValidEnvironment() {
  return (process &&
          process.platform &&
          process.platform.toLowerCase() === 'linux' &&
          process.env &&
          process.env.SHELL &&
          process.env.SHELL.toLowerCase() === '/bin/bash');
}

function isWindowsBash() {
  if (isValidEnvironment()) {
    try {
      var data = fs.readFileSync('/proc/version', 'utf-8');
      return data.toLowerCase().indexOf('microsoft') > -1;
    } catch (err) {
      return false;
    }
  }

  return false;
}

module.exports = isWindowsBash;
