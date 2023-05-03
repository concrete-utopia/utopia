const baseConfigFn = require('./karma-shard-base.conf')

module.exports = function (config) {
  baseConfigFn(config)

  config.set({
    // list of files / patterns to load in the browser
    files: [...config.files, './src/**/[a-cA-C]*.spec.browser2.+(ts|tsx)'],
  })
}
