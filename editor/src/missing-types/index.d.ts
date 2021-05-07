declare module 'eases'
declare module 'eases/linear'
declare module 'eases/quad-in'
declare module 'eases/quad-out'
declare module 'eases/quad-in-out'

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

declare module 'lodash.clamp' {
  export const clamp = (number: number, lower: number, upper: number) => number
  export const clamp = (number: number, upper: number) => number
}

declare module 'draft-js-custom-styles'
declare module 'draft-js-export-html'

declare module 'resize-observer-polyfill'

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

declare module 'console-feed'
declare module 'console-feed/lib/Hook/parse'

declare module 'react-merge-refs'

declare module 'platform-detect'

declare module 'friendly-words' {
  export const predicates: Array<string>
  export const objects: Array<string>
  export const teams: Array<string>
  export const collections: Array<string>
}

declare module 'react-windowed-select' {
  import { StateManager } from 'react-select'
  export default StateManager
}

declare module 'npm-package-arg'

declare module '@svgr/plugin-jsx'
