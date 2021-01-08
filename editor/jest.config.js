module.exports = {
  projects: [
    '../utopia-api',
    {
      testEnvironment: 'jest-environment-jsdom-global',
      preset: 'ts-jest',
      testPathIgnorePatterns: ['/lib/', '/node_modules/', '/.github-test-projects/'],
      testRegex: 'src/.*\\.spec\\.(jsx?|tsx?)$',
      moduleDirectories: ['src', 'node_modules', '<rootDir>/node_modules'],
      moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
      moduleNameMapper: {
        '^domtoimage$': '<rootDir>/src/utils/stubs/dom-to-image-stub.ts',
        '^platform-detect$': '<rootDir>/src/utils/stubs/platform-detect-stub.ts',
        '\\.(css)$': '<rootDir>/test/jest/__mocks__/styleMock.js',
      },
      globals: {
        'ts-jest': {
          isolatedModules: true,
        },
      },
      roots: ['src', 'node_modules', '<rootDir>/node_modules'],
      transformIgnorePatterns: ['/node_modules/(?!utopia-api)'], // this lets ts-jest work on `/node_modules/utopia-api` which is a simlink to `../utopia-api`.
    },
    {
      testEnvironment: '@jest-runner/electron/environment',
      runner: '@jest-runner/electron',
      preset: 'ts-jest',
      testPathIgnorePatterns: ['/lib/', '/node_modules/', '/.github-test-projects/'],
      testRegex: 'src/.*\\.spec\\.browser\\.(jsx?|tsx?)$',
      moduleDirectories: ['src', 'node_modules', '<rootDir>/node_modules'],
      moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
      moduleNameMapper: {
        '^domtoimage$': '<rootDir>/src/utils/stubs/dom-to-image-stub.ts',
        '^platform-detect$': '<rootDir>/src/utils/stubs/platform-detect-stub.ts',
        '\\.(css)$': '<rootDir>/test/jest/__mocks__/styleMock.js',
      },
      globals: {
        'ts-jest': {
          isolatedModules: true,
        },
      },
      roots: ['src', 'node_modules', '<rootDir>/node_modules'],
    },
  ],
}
