declare module '@storybook/react'
declare module '@storybook/addon-actions'

declare module 'react-float-affixed'

declare module 'pegjs-otf'

declare module 'json-schema-ref-parser'

declare module '*.cpeg'

declare module 'babel-plugin-transform-react-jsx'
declare module 'babel-plugin-syntax-jsx'
declare module '@babel/standalone'
declare module '@babel/plugin-transform-modules-commonjs'
declare module '@babel/plugin-proposal-export-namespace-from'
declare module '@babel/plugin-proposal-class-properties'

declare module 'lodash.clamp' {
  export default clamp = (number: number, lower: number, upper: number) => number
}

declare module 'lodash.debounce' {
  export default debounce =
    (
      func: (...args: any[]) => any,
      wait: number,
      options?: { leading?: boolean; maxWait?: number },
    ) =>
    (...args: any[]) =>
      any
}

declare module 'jest-matcher-deep-close-to'

declare namespace DropboxTypes {
  export type Dropbox = any
}

declare module 'string-hash'

declare module 'settle-promise'
declare module '@babel/code-frame'
declare module 'source-map'

declare module 'react-error-overlay'

declare module 'monaco-editor/esm/vs/editor/standalone/browser/simpleServices'
declare module 'monaco-editor/esm/vs/editor/standalone/browser/standaloneServices'

declare module 'eslint4b'
declare module 'babel-eslint'
declare module 'strip-ansi'

declare module 'react-merge-refs'

declare module 'platform-detect'

declare module 'friendly-words' {
  export const predicates: Array<string>
  export const objects: Array<string>
  export const teams: Array<string>
  export const collections: Array<string>
}

declare module 'npm-package-arg'

declare module '@svgr/plugin-jsx'

declare module '@root/encoding/base64'

declare module 'react/jsx-runtime'

/**
 * the __webpack_public_path__ is a global variable supported by Webpack that can be used to dynamically change the value of import.meta.url
 */
declare var __webpack_public_path__: string

/**
 * in karma-setup.js we set window.KarmaTestEnvironment to true. this way our tests and code can figure out if they are inside Karma
 */
interface Window {
  KarmaTestEnvironment: boolean | undefined
}

declare module 'eslint-plugin-react' {
  export const rules: { [key: string]: any } = {}
}
declare module 'eslint-plugin-react-hooks' {
  export const rules: { [key: string]: any } = {}
}
declare module 'eslint-plugin-jsx-a11y' {
  export const rules: { [key: string]: any } = {}
}
declare module 'eslint-plugin-import/lib/rules/first'
declare module 'eslint-plugin-import/lib/rules/no-amd'
declare module 'eslint-plugin-import/lib/rules/no-webpack-loader-syntax'
declare module 'keycode'
