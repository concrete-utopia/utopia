var exec = require('child_process').exec

module.exports = function(packages, packagePath) {
  return new Promise(function(resolve, reject) {
    const allPackages = packages.join(' ')
    const yarnAddCommand = `yarn add ${allPackages} --production --ignore-optional --ignore-scripts --non-interactive --no-bin-links --no-lockfile --ignore-engines`
    exec(
      `mkdir -p ${packagePath} && cd ${packagePath} && yarn init --yes && ${yarnAddCommand}`,
      function(err, stdout, stderr) {
        if (err) {
          reject(err.message.indexOf('versions') >= 0 ? new Error('INVALID_VERSION') : err)
        } else {
          resolve()
        }
      },
    )
  })
}
