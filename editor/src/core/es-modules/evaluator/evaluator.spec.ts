import * as fileNoImports from '../test-cases/file-no-imports.json'
import * as fileWithImports from '../test-cases/file-with-imports.json'
import * as fileWithEsModule from '../test-cases/file-with-es-module.json'
import { evaluator } from './evaluator'
import { NO_OP } from '../../shared/utils'
import type { PackagerServerResponse } from '../../shared/npm-dependency-types'
import {
  isPackagerServerFileDescriptor,
  isPackagerServerFileEntry,
} from '../../shared/npm-dependency-types'
import type { Optic } from '../../shared/optics/optics'
import {
  filtered,
  fromField,
  fromTypeGuard,
  traverseArray,
} from '../../shared/optics/optic-creators'
import { unsafeGet } from '../../shared/optics/optic-utilities'

function getFileContentsOptic(filename: string): Optic<PackagerServerResponse, string> {
  return fromField<PackagerServerResponse, 'contents'>('contents')
    .compose(traverseArray())
    .compose(fromTypeGuard(isPackagerServerFileEntry))
    .compose(fromField('fileEntry'))
    .compose(filtered((entry) => entry.filename === filename))
    .compose(fromField('fileContents'))
    .compose(fromTypeGuard(isPackagerServerFileDescriptor))
    .compose(fromField('content'))
}

describe('ESModule Evaluator', () => {
  it('evaluates a module that has no imports', () => {
    const indexJSContents = unsafeGet(
      getFileContentsOptic('/node_modules/mypackage/index.js'),
      fileNoImports as PackagerServerResponse,
    )
    const result = evaluator(
      '/node_modules/mypackage/index.js',
      indexJSContents,
      { exports: {} },
      () => ({}),
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })

  it('evaluates a module that has one import', () => {
    const indexJSContents = unsafeGet(
      getFileContentsOptic('/node_modules/mypackage/index.js'),
      fileNoImports as PackagerServerResponse,
    )
    const fakeRequire = () => {
      return { hello: 'hello!' }
    }
    const result = evaluator(
      '/node_modules/mypackage/index.js',
      indexJSContents,
      { exports: {} },
      fakeRequire,
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })

  it('evaluates a module that uses module.exports instead of the exports object', () => {
    const codeUsingModuleExportsJSContents = unsafeGet(
      getFileContentsOptic('/node_modules/mypackage/code-using-module-exports.js'),
      fileWithImports as PackagerServerResponse,
    )
    const fakeRequire = () => {
      return { hello: 'hello!' }
    }
    const result = evaluator(
      '/node_modules/mypackage/code-using-module-exports.js',
      codeUsingModuleExportsJSContents,
      { exports: {} },
      fakeRequire,
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })

  it('when trying to evaluate an ES Module, it works', () => {
    const indexJSContents = unsafeGet(
      getFileContentsOptic('/node_modules/mypackage/index.js'),
      fileWithEsModule,
    )
    const fakeRequire = NO_OP
    const result = evaluator(
      '/node_modules/mypackage/index.js',
      indexJSContents,
      { exports: {} },
      fakeRequire,
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })
})
