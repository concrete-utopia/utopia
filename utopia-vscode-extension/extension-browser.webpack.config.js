const path = require('path')

module.exports = {
  context: __dirname,
  mode: 'development',
  target: 'webworker',
  entry: {
    extension: './src/extension.ts',
  },
  resolve: {
    mainFields: ['module', 'main'],
    extensions: ['.ts', '.js'],
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          {
            loader: 'ts-loader',
            options: {
              compilerOptions: {
                sourceMap: true,
                declaration: false,
              },
            },
          },
        ],
      },
    ],
  },
  externals: {
    vscode: 'commonjs vscode', // ignored because it doesn't exist
  },
  performance: {
    hints: false,
  },
  output: {
    filename: 'extension.js',
    path: path.join(__dirname, 'dist', 'browser'),
    libraryTarget: 'commonjs',
  },
  devtool: 'source-map',
}
