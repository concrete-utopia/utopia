const path = require('path')
const webpack = require('webpack')

function srcPath(subdir) {
  return path.join(__dirname, 'src', subdir)
}

module.exports = {
  entry: './src/benchmarks.benchmark.ts',
  target: 'node',
  module: {
    rules: [
      {
        include: (filePath) => {
          if (filePath.includes('node_modules')) {
            // We need to use the ts-loader to load the utopia-api module,
            // but nothing else from node_modules
            return filePath.includes('/utopia-api/')
          } else {
            return true
          }
        },
        test: /\.tsx?$/,
        use: [
          {
            loader: 'ts-loader',
            options: {
              transpileOnly: true,
            },
          },
        ],
      },
      // CSS Loading.
      { test: /\.css$/, use: 'null-loader' },
    ],
  },
  plugins: [
    // setting up the various process.env.VARIABLE replacements
    new webpack.EnvironmentPlugin([
      'REACT_APP_ENVIRONMENT_CONFIG',
      'REACT_APP_AUTH0_CLIENT_ID',
      'REACT_APP_AUTH0_ENDPOINT',
      'REACT_APP_COMMIT_HASH',

      // !! optional env vars should be added in the webpack.EnvironmentPlugin below providing a default value for them instead than here
    ]),

    new webpack.EnvironmentPlugin({
      GOOGLE_WEB_FONTS_KEY: '', // providing an empty default for GOOGLE_WEB_FONTS_KEY for now
      REACT_APP_BROWSER_TEST_DEBUG: 'false',
    }),

    new webpack.ProvidePlugin({ BrowserFS: 'browserfs' }), // weirdly, the browserfs/dist/shims/fs shim assumes a global BrowserFS being available
  ],
  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.json', '.ttf'],
    symlinks: true, // We set this to false as we have symlinked some common code from the website project
    alias: {
      uuiui: srcPath('uuiui'),
      'worker-imports': path.resolve(__dirname, 'src/core/workers/worker-import-utils.ts'),
      'uuiui-deps': srcPath('uuiui-deps'),
      'react/jsx-runtime': require.resolve('./node_modules/react/jsx-runtime'),
      react: require.resolve('./node_modules/react'),
      localforage: require.resolve('./src/utils/fake-localforage.ts'),
    },
    fallback: {
      path: require.resolve('path-browserify'),
      os: false,
      stream: require.resolve('stream-browserify'), // needed for jszip
      constants: false,
    },
  },
  output: {
    filename: 'benchmark.js',
    path: path.resolve(__dirname, 'dist'),
  },
  experiments: {
    topLevelAwait: true,
  },
}
