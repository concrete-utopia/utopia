const baseConfigFn = require('./karma-base.conf')

module.exports = function (config) {
  baseConfigFn(config)

  config.set({
    // list of files / patterns to load in the browser
    files: [...config.files, './src/**/*.spec.hydrogen.browser2.+(ts|tsx)'],
    client: {
      mocha: {
        timeout: 30000,
      },
    },
  })
}
