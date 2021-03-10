const path = require('path')

module.exports = {
  context: __dirname,
  mode: 'development',
  entry: './src/index.ts',
  resolve: {
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
