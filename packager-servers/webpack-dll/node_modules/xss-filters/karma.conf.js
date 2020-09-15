module.exports = function(config) {

  if (process.env.TRAVIS && !process.env.SAUCE_USERNAME || !process.env.SAUCE_ACCESS_KEY) {
    console.warn('No SAUCE credentials found (missing SAUCE_USERNAME and SAUCE_ACCESS_KEY env variables). Skipping SauceLabs testing.');
    return;
  }

  // Browsers to run on Sauce Labs
  var customLaunchers = {
    'SL_Chrome': {
      base: 'SauceLabs',
      browserName: 'chrome',
      version: '31'
    },
    'SL_ChromeLatest': {
      base: 'SauceLabs',
      browserName: 'chrome'
    },
    // Neither json3@socket.io.js@karma nor chai/expect.js work in IE 7
    // 'SL_InternetExplorer7': {
    //   base: 'SauceLabs',
    //   browserName: 'internet explorer',
    //   version: '7'
    // },
    'SL_InternetExplorer8': {
      base: 'SauceLabs',
      browserName: 'internet explorer',
      version: '8'
    },
    'SL_InternetExplorer9': {
      base: 'SauceLabs',
      browserName: 'internet explorer',
      version: '9'
    },
    'SL_InternetExplorer10': {
      base: 'SauceLabs',
      browserName: 'internet explorer',
      version: '10'
    },
    'SL_InternetExplorerLatest': {
      base: 'SauceLabs',
      browserName: 'internet explorer'
    },
    'SL_FireFox31': {
      base: 'SauceLabs',
      browserName: 'firefox',
      version: '31'
    },
    'SL_FireFoxLatest': {
      base: 'SauceLabs',
      browserName: 'firefox',
    },
    'SL_Safari5': {
      base: 'SauceLabs',
      browserName: 'safari',
      version: '5'
    },
    'SL_SafariLatest': {
      base: 'SauceLabs',
      browserName: 'safari',
      platform: 'OS X 10.11'
    },
    // The iPhone simulator takes more than 540s
    // 'SL_IPhone5_1': {
    //   base: 'SauceLabs',
    //   browserName: 'iphone',
    //   version: '5.1'
    // },
    // 'SL_IPhoneLatest': {
    //   base: 'SauceLabs',
    //   browserName: 'iphone'
    // },
    // The android 2.3 simulator has problem accessing the sauce-connected karma server
    // 'SL_Android2_3': {
    //   base: 'SauceLabs',
    //   browserName: 'Android',
    //   version: '2.3'
    // },
    'SL_Android4': {
      base: 'SauceLabs',
      browserName: 'Android',
      version: '4'
    },
    'SL_AndroidLatest': {
      base: 'SauceLabs',
      browserName: 'Android'
    }
  };

  var jobId = process.env.TRAVIS_JOB_NUMBER || new Date().getTime();

  config.set({

    // base path that will be used to resolve all patterns (eg. files, exclude)
    basePath: '',


    // frameworks to use
    // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
    frameworks: ['mocha'],


    // list of files / patterns to load in the browser
    files: [
      'node_modules/expect.js/index.js',
      'dist/xss-filters.min.js',
      'tests/polyfills.js',
      'tests/utils.js',
      'tests/unit/*.js'
    ],


    // test results reporter to use
    // possible values: 'dots', 'progress'
    // available reporters: https://npmjs.org/browse/keyword/karma-reporter
    reporters: ['dots', 'saucelabs'],


    // web server port
    port: 9876,

    colors: true,

    // level of logging
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,

    sauceLabs: {
      testName: 'Unit testing xss-filters',
      recordScreenshots: false,
      tags: ['xss-filters', jobId],
      tunnelIdentifier: jobId,
      build: process.env.TRAVIS_BUILD_NUMBER || null,
      startConnect: false
    },
    captureTimeout: 30000,
    customLaunchers: customLaunchers,

    // start these browsers
    // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
    browsers: Object.keys(customLaunchers),
    singleRun: true
  });
};
