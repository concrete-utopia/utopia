import * as fileNoImports from '../test-cases/file-no-imports.json'
import * as fileWithImports from '../test-cases/file-with-imports.json'
import { evaluator } from './evaluator'

describe('ESModule Evaluator', () => {
  it('evalautes a module that has no imports', () => {
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

  it('evalautes a module that has one import', () => {
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

  it('evalautes a module that uses module.exports instead of the exports object', () => {
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
})
