module.exports = {
  extends: ['./.eslintrc-lite.js'],
  parserOptions: {
    tsconfigRootDir: __dirname,
    project: ['./tsconfig.json'],
  },
  rules: {
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
    'no-return-await': 'off',
    '@typescript-eslint/return-await': 'error',
  },
}
