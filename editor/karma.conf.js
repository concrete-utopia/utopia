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
  config.set({
    plugins: [
      'karma-webpack',
      'karma-mocha',
      'karma-chrome-launcher',
      'karma-viewport',
      'karma-mocha-reporter',
      require('./test/karma-custom-reporter/short-console-messages'),
    ],
    reporters: config.debug ? ['mocha'] : ['mocha', 'short-console-messages'],
    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',

    browserNoActivityTimeout: 1000000,

    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['mocha', 'viewport'],
    webpack: webpackConfig,

    // list of files / patterns to load in the browser
    files: [
      './mocha-setup-beforeall.js',
      './karma-setup.js',
      './src/**/*.spec.browser2.+(ts|tsx)',
      {
        pattern: './resources/editor/**/*.png',
        watched: false,
        served: true,
        included: false,
        nocache: false,
      },
    ],
    proxies: {
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
  })
}
