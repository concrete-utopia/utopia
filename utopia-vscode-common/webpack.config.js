const path = require('path')

module.exports = {
  context: __dirname,
  mode: 'development',
  target: 'web',
  entry: {
    utopiaVscodeCommon: './src/index.ts',
  },
  resolve: {
    mainFields: ['main'],
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
  performance: {
    hints: false,
  },
  output: {
    filename: 'utopia-vscode-common.js',
    path: path.join(__dirname, 'dist', 'browser'),
    libraryTarget: 'umd',
  },
  devtool: 'source-map',
}
