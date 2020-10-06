import * as fileNoImports from '../test-cases/file-no-imports.json'
import * as fileWithImports from '../test-cases/file-with-imports.json'
import * as fileWithEsModule from '../test-cases/file-with-es-module.json'
import { evaluator } from './evaluator'
import { NO_OP } from '../../shared/utils'

describe('ESModule Evaluator', () => {
  it('evaluates a module that has no imports', () => {
    const mainFile = fileNoImports.contents['/node_modules/mypackage/index.js'].content
    const result = evaluator(
      '/node_modules/mypackage/index.js',
      mainFile,
      { exports: {} },
      () => ({}),
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })

  it('evaluates a module that has one import', () => {
    const mainFile = fileWithImports.contents['/node_modules/mypackage/index.js'].content
    const fakeRequire = () => {
      return { hello: 'hello!' }
    }
    const result = evaluator(
      '/node_modules/mypackage/index.js',
      mainFile,
      { exports: {} },
      fakeRequire,
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })

  it('evaluates a module that uses module.exports instead of the exports object', () => {
    const mainFile =
      fileWithImports.contents['/node_modules/mypackage/code-using-module-exports.js'].content
    const fakeRequire = () => {
      return { hello: 'hello!' }
    }
    const result = evaluator(
      '/node_modules/mypackage/code-using-module-exports.js',
      mainFile,
      { exports: {} },
      fakeRequire,
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })

  it('when trying to evaluate an ES Module, it works', () => {
    const mainFile = fileWithEsModule.contents['/node_modules/mypackage/index.js'].content
    const fakeRequire = NO_OP
    const result = evaluator(
      '/node_modules/mypackage/index.js',
      mainFile,
      { exports: {} },
      fakeRequire,
    )
    expect(result.exports).toHaveProperty('hello')
    expect((result.exports as any).hello).toEqual('hello!')
  })
})
