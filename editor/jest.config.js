const rules = [
  {
    match:
      /^Warning: Invalid value for prop `(\w*)` on <(\w*)> tag. Either remove it from the element, or pass a string or number value to keep it in the DOM./,
    group:
      'Warning: Invalid value for prop *** on *** tag. Either remove it from the element, or pass a string or number value to keep it in the DOM.',
    level: 'error',
  },
  {
    match:
      /^Warning: React.createElement: type is invalid -- expected a string \(for built-in components\) or a class\/function \(for composite components\) but got: undefined./,
    group: 'Warning: React.createElement called with undefined',
    level: 'error',
  },
  {
    match: /^Warning: Each child in a list should have a unique "key" prop./,
    group: 'Warning: Each child in a list should have a unique "key" prop.',
    level: 'error',
  },
  {
    match: /^Warning: componentWillReceiveProps has been renamed, and is not recommended for use./,
    group:
      'Depreciation Warning: componentWillReceiveProps has been renamed, and is not recommended for use.',
    level: 'error',
  },
  {
    match:
      /^Warning: useLayoutEffect does nothing on the server, because its effect cannot be encoded into the server renderer's output format./,
    group:
      'Warning: React.useLayoutEffect used in the tests – should replace with useIsoLayoutEffect',
    level: 'error',
  },
  {
    match:
      /^Warning: FIXME: Cannot update a component \(`OpenFileEditor`\) while rendering a different component \(`UiJsxCanvas`\)./,
    group:
      'Warning: FIXME: Cannot update a component (`OpenFileEditor`) while rendering a different component (`UiJsxCanvas`). We should actually fix this!',
    level: 'error',
  },
  {
    match:
      /^Warning: FIXME: Cannot update a component .* while rendering a different component .*./,
    group:
      'Warning: FIXME: Cannot update a component **** while rendering a different component ****. We should actually fix this!',
    level: 'error',
  },
  {
    match: /Check the render method of .*Tippy/,
    group: 'Warning: FIXME: Function components cannot be given refs. Fix Tippy!',
    level: 'error',
  },
  {
    match:
      /Cannot log after tests are done. Did you forget to wait for something async in your test\?/,
    group:
      'Warning: FIXME: Cannot log after tests are done. Did you forget to wait for something async in your test?',
    level: 'error',
  },
  {
    match: /Warning: An update to OpenFileEditor inside a test was not wrapped in act/,
    group: 'Warning: FIXME: An update to OpenFileEditor inside a test was not wrapped in act',
    level: 'error',
  },
  {
    match: /^Warning: React does not recognize the `(\w*)` prop on a DOM element./,
    group: 'Warning: React does not recognize the **** prop on a DOM element.',
    level: 'warning',
  },
  {
    match:
      /^Warning: Invalid prop `(.*)` supplied to `React.Fragment`. React.Fragment can only have `key` and `children` props./,
    group:
      'Warning: Invalid prop *** supplied to `React.Fragment`. React.Fragment can only have `key` and `children` props.',
    level: 'warning',
  },
  {
    match: /gles2_cmd_decoder\.cc/,
    group: 'Chromium GL errors',
    level: 'warning',
  },
  {
    match: /Bundler Fatal Error.*ignore me/,
    group: 'Bundler test output',
    level: 'log',
  },
  {
    match: /WE EXPECT AN ERROR MESSAGE ON THE CONSOLE - DONT WORRY ITS FOR A TEST/,
    group: 'Bundler test output',
    level: 'log',
  },
]

module.exports = {
  reporters: [
    ['jest-clean-console-reporter', { rules: rules }],
    '@jest/reporters/build/SummaryReporter',
  ],
  projects: [
    '../utopia-api',
    {
      testEnvironment: './jest-environment',
      testPathIgnorePatterns: ['/lib/', '/node_modules/', '/.github-test-projects/'],
      testRegex: 'src/.*\\.spec\\.(tsx?)$',
      moduleFileExtensions: ['ts', 'tsx', 'js', 'jsx', 'json', 'node'],
      moduleNameMapper: {
        '^platform-detect$': '<rootDir>/src/utils/stubs/platform-detect-stub.ts',
        '\\.(css)$': '<rootDir>/test/jest/__mocks__/styleMock.js',
        'utopia-api/core': '<rootDir>/node_modules/utopia-api/dist/core.js',
        'worker-imports': '<rootDir>/src/core/workers/__mocks__/worker-import-utils.ts',
        '../utils/vite-hmr-config': '<rootDir>/src/utils/__mocks__/vite-hmr-config.ts',
        'react-dnd': '<rootDir>/test/jest/__mocks__/react-dnd.js',
        'react-dnd-html5-backend': '<rootDir>/test/jest/__mocks__/react-dnd-html5-backend.js',
        '^react$': '<rootDir>/node_modules/react/index.js',
        '^react-dom$': '<rootDir>/node_modules/react-dom/index.js',
        '^@mhsdesign/jit-browser-tailwindcss$':
          '<rootDir>/src/utils/__mocks__/jit-browser-tailwindcss.js',
        '\\.(jpg|ico|jpeg|png|gif|eot|otf|webp|svg|ttf|woff|woff2|mp4|webm|wav|mp3|m4a|aac|oga)$':
          '<rootDir>/test/jest/__mocks__/styleMock.js',
      },
      transform: {
        '\\.[jt]sx?$': 'babel-jest',
      },
      roots: ['src'],
      // Note yjs and lib0 are included in this as they must be transformed to work
      // within jest as the type of modules they expose by default are not the kind that it expects.
      transformIgnorePatterns: ['<rootDir>/node_modules/.pnpm/(?!file+..+|yjs|lib0)'],
      setupFiles: ['./jest-setup-beforeall.js'],
    },
  ],
}
