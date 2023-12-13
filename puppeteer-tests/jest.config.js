module.exports = {
  preset: 'ts-jest',
//   globalSetup: './src/comments/setup.js',
  testEnvironment: 'node',
  // No need to reference "test/todo-list.test.js"
  // because it's detected by the default value of testRegex
  // https://jestjs.io/docs/configuration#testregex-string--arraystring
//   globalTeardown: './src/comments/teardown.js',
}
