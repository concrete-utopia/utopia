import * as fileWithImports from '../test-cases/file-with-imports.json'
import { mangleNodeModulePaths } from './merge-modules'
import { createNodeModules } from './test-utils'

describe('Merge Node Modules', () => {
  it('moves all transitive dependencies under an inner node modules', () => {
    const mangledModules = mangleNodeModulePaths(
      'mypackage',
      createNodeModules(fileWithImports.contents),
    )

    // these two files are not moved
    expect(mangledModules['/node_modules/mypackage/index.js']).toBeDefined()
    expect(mangledModules['/node_modules/mypackage/package.json']).toBeDefined()

    // these files are moved under /node_modules/mypackage/node_modules/
    expect(
      mangledModules['/node_modules/mypackage/node_modules/otherpackage/dist/index.js'],
    ).toBeDefined()
    expect(
      mangledModules['/node_modules/mypackage/node_modules/otherpackage/package.json'],
    ).toBeDefined()
  })
})
