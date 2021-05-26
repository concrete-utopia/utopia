var webpackConfig = require('./webpack.config')
delete webpackConfig['entry']

module.exports = function (config) {
  config.set({
    plugins: ['karma-webpack', 'karma-mocha', 'karma-chrome-launcher'],

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',

    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['webpack', 'mocha'],
    webpack: webpackConfig,

    // list of files / patterns to load in the browser
    // Here I'm including all of the the Jest tests which are all under the __tests__ directory.
    // You may need to tweak this patter to find your test files/
    // files: ['./karma-setup.js', 'src/.*\\.spec\\.(jsx?|tsx?)$'],
    files: ['./karma-setup.js', './src/components/canvas/ui-jsx-canvas-errors.browser2.spec.ts'],

    // browsers: ['ChromeHeadless'],
    browsers: ['Chrome'],

    // preprocess matching files before serving them to the browser
    // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
    preprocessors: {
      // Use webpack to bundle our tests files
      './karma-setup.js': ['webpack'],
      './src/**/*.ts': ['webpack'],
    },
  })
}
