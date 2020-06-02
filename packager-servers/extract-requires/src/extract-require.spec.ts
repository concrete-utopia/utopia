import * as path from 'path'
import { resolveRequirePath } from './extract-require'

describe('resolveRequirePath', () => {
  it('resolves a module in a sibling folder', () => {
    const absoluteUrl = path.resolve('./test-folder/test-a/index.js')
    const result = resolveRequirePath(absoluteUrl)
    const expectedResult = [
      path.resolve('./test-folder/test-a/index.js'),
      path.resolve('./test-folder/test-a/node_modules/test-package-b/index.js'),
      path.resolve('./test-folder/test-a/node_modules/test-package-a/index.js'),
    ]
    expect(result).toEqual(expectedResult)
  })
})
