export default {
  //   preset: "ts-jest",
  transform: {
    '\\.[jt]sx?$': 'babel-jest',
  },
  transformIgnorePatterns: ['node_modules/(?!@ngrx|(?!deck.gl)|ng-dynamic)'], // fix for Jest running url-join tests failing due to imports
}
