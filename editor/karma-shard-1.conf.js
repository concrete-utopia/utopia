const baseConfigFn = require('./karma-base.conf')

module.exports = function (config) {
  baseConfigFn(config)

  config.set({
    // list of files / patterns to load in the browser
    files: [...config.files, './src/**/[a-fA-F]*.spec.browser2.+(ts|tsx)'],
  })
}
