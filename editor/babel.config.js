module.exports = {
  presets: [
    ['@babel/preset-typescript', { jsx: 'react' }],
    ['@babel/preset-env', { targets: { node: 'current' } }],
  ],
}
