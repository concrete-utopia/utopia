const noDispatchUseEffectPlugin = require('./.eslint-plugin-local')
module.exports = {
  extends: ['eslint-config-utopia'],
  parserOptions: {
    tsconfigRootDir: __dirname,
    project: ['./tsconfig.json'],
  },
  plugins: ['eslint-plugin-local'],
  rules: {
    'local/no-dispatch-use-effect': 'error',
    '@typescript-eslint/no-floating-promises': 'error',
    '@typescript-eslint/strict-boolean-expressions': [
      'error',
      {
        allowString: false,
        allowNumber: false,
        allowNullableObject: false,
        allowNullableBoolean: true,
      },
    ],
    // Note: Required by the return-await rule, you must disable the base rule as it can report incorrect errors
    'no-return-await': 'off',
    '@typescript-eslint/return-await': 'error',
  },
}
