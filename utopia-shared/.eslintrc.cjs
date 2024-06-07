module.exports = {
  extends: ['eslint-config-utopia'],
  parserOptions: {
    tsconfigRootDir: __dirname,
    project: ['./tsconfig.json'],
  },
}
