import * as moduleResolutionExamples from '../test-cases/module-resolution-examples.json'
import { resolveModule } from './module-resolution'
import { createNodeModules } from './test-utils'
import { FriendlyEsModuleErrorMessage } from './package-manager'

describe('ES Package Manager Module Resolution', () => {
  function resolve(toImport: string): string | null {
    return resolveModule(
      createNodeModules(moduleResolutionExamples.contents),
      '/node_modules/mypackage/src/moduleA.js',
      toImport,
    )
  }
  it('resolves all possible non-relative import locations', () => {
    expect(resolve('srcFolderNodeModuleFile')).toEqual(
      '/node_modules/mypackage/src/node_modules/srcFolderNodeModuleFile.js',
    )
    expect(resolve('srcFolderNodeModuleFolderPackageJson')).toEqual(
      '/node_modules/mypackage/src/node_modules/srcFolderNodeModuleFolderPackageJson/actualModuleB.js',
    )
    expect(resolve('srcFolderNodeModuleFolderIndexJs')).toEqual(
      '/node_modules/mypackage/src/node_modules/srcFolderNodeModuleFolderIndexJs/index.js',
    )
    expect(resolve('packageRootNodeModuleFile')).toEqual(
      '/node_modules/mypackage/node_modules/packageRootNodeModuleFile.js',
    )
    expect(resolve('packageRootNodeModuleFolderPackageJson')).toEqual(
      '/node_modules/mypackage/node_modules/packageRootNodeModuleFolderPackageJson/dist/actualModuleB.js',
    )
    expect(resolve('packageRootNodeModuleFolderIndexJs')).toEqual(
      '/node_modules/mypackage/node_modules/packageRootNodeModuleFolderIndexJs/index.js',
    )
    expect(resolve('globalNodeModuleFile')).toEqual('/node_modules/globalNodeModuleFile.js')
    expect(resolve('globalNodeModuleFolderPackageJson')).toEqual(
      '/node_modules/globalNodeModuleFolderPackageJson/lib/actualModuleB.js',
    )
    expect(resolve('globalNodeModuleFolderIndexJs')).toEqual(
      '/node_modules/globalNodeModuleFolderIndexJs/index.js',
    )
    expect(resolve('@namespaced/module')).toEqual('/node_modules/@namespaced/module/index.js')
    expect(resolve('@othernamespace/othermodule')).toEqual(
      '/node_modules/@othernamespace/othermodule/dist/actual.js',
    )
    expect(resolve('@othernamespace/module-with-main-entry-pointing-to-directory')).toEqual(
      '/node_modules/@othernamespace/module-with-main-entry-pointing-to-directory/dist/index.js',
    )
    expect(resolve('doesnt-exist-module')).toEqual(null)
  })

  it('resolves all relative import locations', () => {
    expect(resolve('./moduleB')).toEqual('/node_modules/mypackage/src/moduleB.js')
    expect(resolve('./moduleB.js')).toEqual('/node_modules/mypackage/src/moduleB.js')
    expect(resolve('./folder/moduleC')).toEqual('/node_modules/mypackage/src/folder/moduleC.js')
    expect(resolve('./folder/moduleC.js')).toEqual('/node_modules/mypackage/src/folder/moduleC.js')
    expect(resolve('./folder/moduleD')).toEqual('/node_modules/mypackage/src/folder/moduleD.jsx')
    expect(resolve('./folder/moduleD.jsx')).toEqual(
      '/node_modules/mypackage/src/folder/moduleD.jsx',
    )
    expect(resolve('./folder/../folder/moduleD')).toEqual(
      '/node_modules/mypackage/src/folder/moduleD.jsx',
    )
    // notice the // in the import path
    expect(resolve('./folder/../folder//moduleD')).toEqual(
      '/node_modules/mypackage/src/folder/moduleD.jsx',
    )
    expect(resolve('../src/folder/moduleD')).toEqual(
      '/node_modules/mypackage/src/folder/moduleD.jsx',
    )
    expect(resolve('../../mypackage/src/folder/moduleD')).toEqual(
      '/node_modules/mypackage/src/folder/moduleD.jsx',
    )
    expect(resolve('../../mypackage/src/folder/moduleE')).toEqual(
      '/node_modules/mypackage/src/folder/moduleE.json',
    )
    expect(resolve('./folderWithPackageJson')).toEqual(
      '/node_modules/mypackage/src/folderWithPackageJson/actualModuleE.js',
    )
    expect(resolve('./folderWithBrokenPackageJson')).toEqual(
      '/node_modules/mypackage/src/folderWithBrokenPackageJson/index.js',
    )
    expect(resolve('./nonexistentImport')).toEqual(null)
    expect(resolve('./folderWithPackageJsonWithMissingFile')).toEqual(null)
    expect(resolve('./folderWithpackageJsonMissingField')).toEqual(null)
  })

  it('resolves absolute relative imports', () => {
    expect(resolve('/node_modules/mypackage/src/folder/moduleD')).toEqual(
      '/node_modules/mypackage/src/folder/moduleD.jsx',
    )
  })

  it('throws a friendly error when finding a package.json that has no main entry and only has a module entry', () => {
    expect(() => resolve('es-module-only-package')).toThrowError(FriendlyEsModuleErrorMessage)
  })

  // it('loads self references', () => {
  //   expect(
  //     resolveModule(
  //       createNodeModules(moduleResolutionExamples.contents),
  //       'my-package-with-package-root/src/deep-folder/moduleA.js',
  //       'my-package-with-package-root',
  //     ),
  //   ).toEqual('my-package-with-package-root/index.js')
  //   expect(
  //     resolveModule(
  //       createNodeModules(moduleResolutionExamples.contents),
  //       'my-package-with-package-root/src/deep-folder/moduleA.js',
  //       'my-package-with-package-root/src/other-folder/moduleB',
  //     ),
  //   ).toEqual('my-package-with-package-root/src/other-folder/moduleB.js')
  // })
})
