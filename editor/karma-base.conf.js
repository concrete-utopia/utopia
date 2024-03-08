if (!('CHROME_BIN' in process.env)) {
  process.env.CHROME_BIN = require('puppeteer').executablePath() // Puppeteer v19.6.0 uses Chromium 110.0.5479.0
}

const isGithubActionsEnvironment = process.env.CI === 'true'

const os = require('os')
const cpuCores = os.cpus().length

const webpack = require('webpack')
var webpackConfig = require('./webpack.config')
delete webpackConfig['entry']

// handles stack-utils looking for 'module'
webpackConfig['resolve']['fallback']['module'] = false

webpackConfig['plugins'].push(
  new webpack.DefinePlugin({
    'process.env.RTL_SKIP_AUTO_CLEANUP': 'undefined',
  }),
)

module.exports = function (config) {
  const baseReporters = ['mocha', 'time-stats']
  var reporters = [...baseReporters, 'short-console-messages']
  if (config.debug) {
    reporters = [...baseReporters]
  }
  if (config.fullOutput) {
    reporters = [...baseReporters, 'full-console-messages']
  }
  config.set({
    plugins: [
      require('karma-parallel'),
      'karma-webpack',
      'karma-mocha',
      'karma-chrome-launcher',
      'karma-viewport',
      'karma-mocha-reporter',
      require('./test/karma-custom-reporter/short-console-messages'),
      require('./test/karma-custom-reporter/full-console-messages'),
      'karma-time-stats-reporter',
    ],
    reporters: reporters,
    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',

    browserNoActivityTimeout: 1000000,

    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    // NOTE: 'parallel' must be the first framework in the list
    frameworks: ['parallel', 'mocha', 'viewport'],
    webpack: webpackConfig,

    // list of files / patterns to load in the browser
    files: [
      './mocha-setup-beforeall.js',
      './karma-setup.js', // this must run before importing the editor-entry-point-imports module
      './src/templates/editor-entry-point-imports.tsx', // we load the real editor entry point first, to make sure the environment matches the real environment, and also to avoid diverging circular dependencies
      {
        pattern: './resources/editor/**/*.png',
        watched: false,
        served: true,
        included: false,
        nocache: false,
      },
      {
        pattern: './resources/editor/**/*.css',
        watched: false,
        served: true,
        included: false,
        nocache: false,
      },
    ],
    proxies: {
      '/editor/css/': '/base/resources/editor/css',
      '/editor/icons': '/base/resources/editor/icons',
      '/editor/cursors': '/base/resources/editor/cursors',
      '/editor/fills': '/base/resources/editor/fills',
      '/editor/avatars': '/base/resources/editor/avatars',
    },

    browsers: config.debug ? [] : ['ChromeHeadless'],
    // browsers: ['Chrome'],

    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
      // Use webpack to bundle our tests files
      './karma-setup.js': ['webpack'],
      './src/**/*.ts': ['webpack'],
      './src/**/*.tsx': ['webpack'],
    },
    client: {
      mocha: {
        timeout: config.debug ? 1000000 : 10000,
      },
    },
    parallelOptions: {
      executors: isGithubActionsEnvironment ? cpuCores : cpuCores / 2,
      shardStrategy: 'round-robin', // if we need a customShardStrategy, see https://github.com/joeljeske/karma-parallel
    },

    timeStatsReporter: {
      reportTimeStats: true, // Print Time Stats (histogram)

      binSize: 100, // Bin size for histogram (in milliseconds)

      slowThreshold: 500, // The threshold for what is considered a slow test (in milliseconds).
      // This is also the max value for last bin histogram
      // Note that this will automatically be rounded up to be evenly divisible by binSize

      reportSlowestTests: true, // Print top slowest tests

      showSlowTestRankNumber: false, // Displays rank number next to slow tests, e.g. `1) Slow Test`

      longestTestsCount: 5, // Number of top slowest tests to list
      // Set to `Infinity` to show all slow tests. Useful in combination with `reportOnlyBeyondThreshold` as `true`

      reportOnlyBeyondThreshold: false, // Only report tests that are slower than threshold
    },
  })
}
