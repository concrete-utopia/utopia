import * as path from 'path'
import { resolveRequirePath } from './extract-require'

describe('resolveRequirePath', () => {
  it('resolves a module in a sibling folder', () => {
    const absoluteUrl = path.resolve('./test-folder/test-a/node_modules/test-package-b/index.js')
    const result = resolveRequirePath(absoluteUrl, 'test-package-a')
    const targetAbsoluteUrl = path.resolve(
      './test-folder/test-a/node_modules/test-package-a/index.js',
    )
    expect(result).toEqual(targetAbsoluteUrl)
  })
})
