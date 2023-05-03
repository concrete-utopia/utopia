const baseConfigFn = require('./karma-base.conf')

const isGithubActionsEnvironment = process.env.CI === 'true'

const os = require('os')
const cpuCores = os.cpus().length

module.exports = function (config) {
  baseConfigFn(config)

  config.set({
    parallelOptions: {
      ...config.parallelOptions,
      executors: isGithubActionsEnvironment ? cpuCores : cpuCores / 4,
    },
  })
}
