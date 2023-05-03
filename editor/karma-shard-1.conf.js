const baseConfigFn = require('./karma-base.conf')

const isGithubActionsEnvironment = process.env.CI === 'true'

module.exports = function (config) {
  baseConfigFn(config)

  config.set({
    // list of files / patterns to load in the browser
    files: [...config.files, './src/**/[a-cA-C]*.spec.browser2.+(ts|tsx)'],
    parallelOptions: {
      ...config.parallelOptions,
      executors: isGithubActionsEnvironment ? cpuCores : cpuCores / 4,
    },
  })
}
