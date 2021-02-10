const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin')
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin')
const ForkTsCheckerAsyncOverlayWebpackPlugin = require('fork-ts-checker-async-overlay-webpack-plugin')
const CleanTerminalPlugin = require('clean-terminal-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const ScriptExtHtmlWebpackPlugin = require('script-ext-html-webpack-plugin')
const InterpolateHtmlPlugin = require('react-dev-utils/InterpolateHtmlPlugin')
const TerserPlugin = require('terser-webpack-plugin')
const path = require('path')
const webpack = require('webpack')

const Production = 'production'
const Staging = 'staging'
const Development = 'development'

const verbose = process.env.VERBOSE === 'true'
const performance = process.env.PERFORMANCE === 'true' // This is for performance testing, which combines the dev server with production config
const hot = !performance && process.env.HOT === 'true' // For running the webpack-dev-server in hot mode
const mode = process.env.WEBPACK_MODE || (performance ? Production : Development) // Default to 'development' unless we are running the performance test
const actualMode = mode === Staging ? Production : mode
const isDev = mode === Development || performance
const isStaging = mode === Staging
const isProd = !(isDev || isStaging)
const isProdOrStaging = isProd || isStaging

const runCompiler = isDev && process.env.RUN_COMPILER !== 'false' // For when you want to run the compiler in a separate tab

// eslint-disable-next-line no-console
console.log(
  `Running with options: mode - ${mode}, hot - ${hot}, performance - ${performance}, runCompiler - ${runCompiler}`,
)

function srcPath(subdir) {
  return path.join(__dirname, 'src', subdir)
}

// Webpack bug - you have to use the same hash pattern for chunkFilename as filename
// Bug report: https://github.com/webpack/webpack/issues/10724
// Options are:
// 1. [hash] - this is a new string generated with every build, and is the only option in hot mode
// 2. [chunkhash] - this is a hash of the chunk itself, so if the chunk stays the same then so does the hash
// 3. [contenthash] - this is a hash of the extracted data of the chunk, which appears to only be useful when
//                    using the ExtractedTextPlugin - https://v4.webpack.js.org/plugins/extract-text-webpack-plugin/
const hashPattern = hot ? '[hash]' : '[chunkhash]'

const BaseDomain = isProd ? 'https://cdn.utopia.app' : isStaging ? 'https://cdn.utopia.pizza' : ''
const VSCodeBaseDomain = BaseDomain === '' ? '${window.location.origin}' : BaseDomain

const config = {
  mode: actualMode,

  entry: {
    editor: hot
      ? ['react-hot-loader/patch', './src/templates/editor-entry-point.tsx']
      : './src/templates/editor-entry-point.tsx',
    preview: hot
      ? ['react-hot-loader/patch', './src/templates/preview.tsx']
      : './src/templates/preview.tsx',
    propertyControlsInfo: hot
      ? ['react-hot-loader/patch', './src/templates/property-controls-info.tsx']
      : './src/templates/property-controls-info.tsx',
    monacoEditorIframe: hot
      ? ['react-hot-loader/patch', './src/templates/monaco-editor-iframe.tsx']
      : './src/templates/monaco-editor-iframe.tsx',
    vsCodeEditorOuterIframe: hot
      ? ['react-hot-loader/patch', './src/templates/vscode-editor-outer-iframe.tsx']
      : './src/templates/vscode-editor-outer-iframe.tsx',
    tsWorker: './src/core/workers/ts/ts.worker.ts',
    parserPrinterWorker: './src/core/workers/parser-printer/parser-printer.worker.ts',
    linterWorker: './src/core/workers/linter/linter.worker.ts',
    watchdogWorker: './src/core/workers/watchdog.worker.ts',
  },

  output: {
    crossOriginLoading: 'anonymous',
    filename: (chunkData) => {
      const name = chunkData.chunk.name
      const nameOnly =
        name === 'tsWorker' ||
        name === 'parserPrinterWorker' ||
        name === 'linterWorker' ||
        name === 'watchdogWorker'
      return nameOnly ? '[name].js' : `[name].${hashPattern}.js`
    },
    chunkFilename: `[id].${hashPattern}.js`,
    path: __dirname + '/lib',
    library: 'utopia',
    libraryTarget: 'umd',
    publicPath: `${BaseDomain}/editor/`,
    globalObject: 'this',
  },

  plugins: [
    new CleanTerminalPlugin(),
    new MonacoWebpackPlugin(),

    // This plugin was not really built for multiple output webpack projects like ours
    // Therefore you have to add it multiple times to generate a new file per output,
    // deciding which modules to include or exclude each time.
    new HtmlWebpackPlugin({
      // First run it to generate the editor's index.html
      chunks: ['editor'],
      inject: 'head', // Add the script tags to the end of the <head>
      scriptLoading: 'defer',
      template: './src/templates/index.html',
      minify: false,
    }),
    new HtmlWebpackPlugin({
      // Run it again to generate the preview.html
      chunks: ['preview'],
      inject: 'head', // Add the script tags to the end of the <head>
      scriptLoading: 'defer',
      template: './src/templates/preview.html',
      filename: 'preview.html',
      minify: false,
    }),
    new HtmlWebpackPlugin({
      // Run it again to generate the preview.html
      chunks: ['propertyControlsInfo'],
      inject: 'head', // Add the script tags to the end of the <head>
      scriptLoading: 'defer',
      template: './src/templates/property-controls-info.html',
      filename: 'property-controls-info.html',
      minify: false,
    }),
    new HtmlWebpackPlugin({
      // Run it again to generate the preview.html
      chunks: ['monacoEditorIframe'],
      inject: 'head', // Add the script tags to the end of the <head>
      scriptLoading: 'defer',
      template: './src/templates/monaco-editor-iframe.html',
      filename: 'monaco-editor-iframe.html',
      minify: false,
    }),
    new HtmlWebpackPlugin({
      chunks: ['vsCodeEditorOuterIframe'],
      inject: 'head', // Add the script tags to the end of the <head>
      scriptLoading: 'defer',
      template: './src/templates/vscode-editor-outer-iframe.html',
      filename: 'vscode-editor-outer-iframe/index.html',
      minify: false,
    }),
    new HtmlWebpackPlugin({
      chunks: [],
      inject: 'head', // Add the script tags to the end of the <head>
      scriptLoading: 'defer',
      template: './src/templates/vscode-editor-inner-iframe.html',
      filename: 'vscode-editor-inner-iframe/index.html',
      minify: false,
    }),
    new ScriptExtHtmlWebpackPlugin({
      // Support CORS so we can use the CDN endpoint from either of the domains
      custom: {
        test: /\.js$/,
        attribute: 'crossorigin',
        value: 'anonymous',
      },
    }),
    new InterpolateHtmlPlugin(HtmlWebpackPlugin, {
      // This plugin replaces variables of the form %VARIABLE% with the value provided in this object
      UTOPIA_SHA: process.env.UTOPIA_SHA || 'nocommit',
      UTOPIA_DOMAIN: BaseDomain,
      VSCODE_DOMAIN: VSCodeBaseDomain,
    }),

    // Optionally run the TS compiler in a different thread, but as part of the webpack build still
    ...(runCompiler
      ? [
          new ForkTsCheckerAsyncOverlayWebpackPlugin({
            checkerPlugin: new ForkTsCheckerWebpackPlugin({
              memoryLimit: 4096,
            }),
          }),
        ]
      : []),

    // Needed when running the webpack-dev-server in hot mode
    ...(hot ? [new webpack.HotModuleReplacementPlugin()] : []),
    ...(isProdOrStaging
      ? [
          new CleanWebpackPlugin({
            // Clean up TS transpile results after bundling
            cleanAfterEveryBuildPatterns: ['lib/*/'],
          }),
        ]
      : []),
  ],

  resolve: {
    // Add '.ts' and '.tsx' as resolvable extensions.
    extensions: ['.ts', '.tsx', '.js', '.json', '.ttf'],
    symlinks: false, // We set this to false as we have symlinked some common code from the website project
    alias: {
      uuiui: srcPath('uuiui'),
      'uuiui-deps': srcPath('uuiui-deps'),
      fs: require.resolve('./node_modules/browserfs/dist/shims/fs'),
      process: require.resolve('./node_modules/browserfs/dist/shims/process'),
      react: require.resolve('./node_modules/react'),

      // Support running the profiler against production build of react
      ...(performance ? { 'scheduler/tracing': 'scheduler/tracing-profiling' } : {}),

      // Extra aliases required when using react's hot loading
      ...(hot
        ? {
            'react-dom$': '@hot-loader/react-dom/profiling',
          }
        : {}),
    },
  },

  externals: {
    domtoimage: 'domtoimage',
    'source-map-support': 'should-never-succeed', // I don't know what this is?
  },

  module: {
    // FIXME The below is deprecated, but linter worker relies on a few packages that throw warnings if this isn't set
    // The modules: typescript and eslint-plugin-react
    // The warning: "Critical dependency: the request of a dependency is an expression"
    // Relevant SO: https://stackoverflow.com/questions/42908116/webpack-critical-dependency-the-request-of-a-dependency-is-an-expression
    exprContextCritical: false,

    rules: [
      // Match typescript
      {
        exclude: /node_modules(?!\/utopia-api)/,
        test: /\.tsx?$/,
        use: [
          {
            loader: 'ts-loader',
            options: {
              transpileOnly: true,
            },
          },

          // Required when using react's hot loading
          ...(hot ? [{ loader: 'react-hot-loader/webpack' }] : []),
        ],
      },

      // CSS Loading (used by monaco)
      { test: /\.css$/, use: ['style-loader', 'css-loader'] },

      {
        test: /node_modules[/\\]eslint4b[/\\]dist[/\\].*.js$/,
        use: [
          {
            loader: 'babel-loader',
            options: {
              plugins: ['@babel/plugin-transform-dotall-regex'],
            },
          },
        ],
      },

      // Patch for `babel-eslint` -- accessing `global` causes build error.
      {
        test: /node_modules[/\\]babel-eslint[/\\]lib[/\\]analyze-scope\.js$/,
        use: [
          {
            loader: 'string-replace-loader',
            options: {
              search: 'require("./patch-eslint-scope")',
              replace: 'Object',
            },
          },
        ],
      },

      // Fonts
      {
        test: /\.ttf$/,
        use: ['file-loader'],
      },
    ],
  },

  devServer: isDev
    ? {
        host: '0.0.0.0',
        port: 8088,
        contentBase: path.join(__dirname, 'resources'),
        watchContentBase: true, // Watch the above folder for changes too
        overlay: {
          warnings: true,
          errors: true,
        },
        disableHostCheck: true, // Because we are proxying this
        stats: {
          assets: false,
          colors: true,
          version: false,
          hash: false,
          timings: false,
          chunks: false,
          chunkModules: false,
        },
        hot: hot,
      }
    : {},

  optimization: {
    minimize: isProd,
    minimizer: isProd
      ? [
          new TerserPlugin({
            cache: true,
            parallel: true,
            sourceMap: true,
            terserOptions: {
              ecma: 8,
            },
          }),
        ]
      : [],
    moduleIds: 'hashed', // "Short hashes as ids for better long term caching."
    splitChunks: {
      name: false, // "It is recommended to set splitChunks.name to false for production builds so that it doesn't change names unnecessarily."
      chunks(chunk) {
        // exclude workers until we figure out a way to chunk those
        return (
          chunk.name !== 'tsWorker' &&
          chunk.name !== 'parserPrinterWorker' &&
          chunk.name !== 'linterWorker' &&
          chunk.name !== 'watchdogWorker'
        )
      },
      minSize: 10000, // Minimum size before chunking
      maxAsyncRequests: isProdOrStaging ? 6 : Infinity,
      maxInitialRequests: isProdOrStaging ? 6 : Infinity,
    },
  },

  performance: {
    hints: false, // We get it webpack, our asse(t)s are huge, you don't need to tell us every time
  },

  // Caching to speed up subsequent builds. We don't want this for regular production mode, but do want this for
  // the performance test (which still sets mode: 'production')
  cache: isDev,

  // Use default source maps in dev mode, or attach a source map if in prod
  devtool: isProd ? 'source-map' : 'eval',
}

if (verbose) {
  // eslint-disable-next-line no-console
  console.log(`Final config: \n${JSON.stringify(config, null, 2)}`)
}

module.exports = config
