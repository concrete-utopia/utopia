module.exports = {
  presets: [
    ['@babel/preset-env', { targets: { node: 'current' } }],
    ['@babel/preset-typescript', { jsx: 'react' }],
    '@babel/react',
  ],
}
