const path = require('path')
const nodeExternals = require('webpack-node-externals')

module.exports = {
  context: __dirname,
  mode: 'development',
  entry: './src/index.ts',
  resolve: {
    extensions: ['.ts', '.js'],
  },
  externals: [nodeExternals()],
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
                declaration: true,
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
    filename: 'index.js',
    path: path.join(__dirname, 'dist'),
    libraryTarget: 'commonjs',
  },
  devtool: 'source-map',
}
