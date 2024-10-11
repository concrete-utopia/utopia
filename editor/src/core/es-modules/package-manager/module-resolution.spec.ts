import type { ProjectContentTreeRoot } from '../../../components/assets'
import { contentsToTree } from '../../../components/assets'
import {
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../../shared/project-file-types'
import * as moduleResolutionExamples from '../test-cases/module-resolution-examples.json'
import { isResolveSuccess, resolveModule } from './module-resolution'
import { createNodeModules } from './test-utils'

const sampleProjectContents: ProjectContentTreeRoot = contentsToTree({
  'jsconfig.json': textFile(
    textFileContents(
      `
    {
      "compilerOptions": {
        "paths": {
          "stars/**/*.stars": [
            "a/**/*.js"
          ],
          "~/*": [
            "app/*"
          ]
        }
      }
    }`,
      unparsed,
      RevisionsState.CodeAhead,
    ),
    null,
    null,
    0,
  ),
  '/a/some/nested/file.js': textFile(
    textFileContents('export const Cake = "tasty"', unparsed, RevisionsState.CodeAhead),
    null,
    null,
    0,
  ),
  '/app/card.js': textFile(
    textFileContents('export const Card = "card"', unparsed, RevisionsState.CodeAhead),
    null,
    null,
    0,
  ),
  '/src/thing.js': textFile(
    textFileContents('export const Thing = 1', unparsed, RevisionsState.CodeAhead),
    null,
    null,
    0,
  ),
  '/src/icon.png': {
    type: 'ASSET_FILE',
  },
  '/src/simple.css': textFile(
    textFileContents('.utopiaClass { background-color: red; }', unparsed, RevisionsState.CodeAhead),
    null,
    null,
    0,
  ),
})

describe('ES Package Manager Module Resolution', () => {
  function resolve(importOrigin: string, toImport: string): string | null {
    const resolveResult = resolveModule(
      sampleProjectContents,
      createNodeModules(moduleResolutionExamples.contents),
      importOrigin,
      toImport,
    )
    if (isResolveSuccess(resolveResult)) {
      return resolveResult.success.path
    } else {
      return null
    }
  }

  it('resolves mapped file paths', () => {
    expect(resolve('/src/startingFile.js', '~/card')).toEqual('/app/card.js')
    expect(resolve('/src/startingFile.js', 'stars/some/nested/file.stars')).toEqual(
      '/a/some/nested/file.js',
    )
  })

  function testNonRelativeResolve(toImport: string, expectedResult: string | null): void {
    it(`resolves non-relative path ${toImport}`, () => {
      expect(resolve('/node_modules/mypackage/src/moduleA.js', toImport)).toEqual(expectedResult)
    })
  }

  testNonRelativeResolve(
    'srcFolderNodeModuleFile',
    '/node_modules/mypackage/src/node_modules/srcFolderNodeModuleFile.js',
  )
  testNonRelativeResolve(
    'srcFolderNodeModuleFolderPackageJson',
    '/node_modules/mypackage/src/node_modules/srcFolderNodeModuleFolderPackageJson/actualModuleB.js',
  )
  testNonRelativeResolve(
    'srcFolderNodeModuleFolderIndexJs',
    '/node_modules/mypackage/src/node_modules/srcFolderNodeModuleFolderIndexJs/index.js',
  )
  testNonRelativeResolve(
    'packageRootNodeModuleFile',
    '/node_modules/mypackage/node_modules/packageRootNodeModuleFile.js',
  )
  testNonRelativeResolve(
    'packageRootNodeModuleFolderPackageJson',
    '/node_modules/mypackage/node_modules/packageRootNodeModuleFolderPackageJson/dist/actualModuleB.js',
  )
  testNonRelativeResolve(
    'packageRootNodeModuleFolderIndexJs',
    '/node_modules/mypackage/node_modules/packageRootNodeModuleFolderIndexJs/index.js',
  )
  testNonRelativeResolve('globalNodeModuleFile', '/node_modules/globalNodeModuleFile.js')
  testNonRelativeResolve(
    'globalNodeModuleFolderPackageJson',
    '/node_modules/globalNodeModuleFolderPackageJson/lib/actualModuleB.js',
  )
  testNonRelativeResolve(
    'globalNodeModuleFolderIndexJs',
    '/node_modules/globalNodeModuleFolderIndexJs/index.js',
  )
  testNonRelativeResolve('@namespaced/module', '/node_modules/@namespaced/module/index.js')
  testNonRelativeResolve(
    '@othernamespace/othermodule',
    '/node_modules/@othernamespace/othermodule/dist/actual.js',
  )
  testNonRelativeResolve(
    '@othernamespace/module-with-main-entry-pointing-to-directory',
    '/node_modules/@othernamespace/module-with-main-entry-pointing-to-directory/dist/index.js',
  )
  testNonRelativeResolve('doesnt-exist-module', null)

  function testRelativeResolve(toImport: string, expectedResult: string | null): void {
    it(`resolves relative path ${toImport}`, () => {
      expect(resolve('/node_modules/mypackage/src/moduleA.js', toImport)).toEqual(expectedResult)
    })
  }

  testRelativeResolve('./moduleB', '/node_modules/mypackage/src/moduleB.js')
  testRelativeResolve('./moduleB.js', '/node_modules/mypackage/src/moduleB.js')
  testRelativeResolve('./folder/moduleC', '/node_modules/mypackage/src/folder/moduleC.js')
  testRelativeResolve('./folder/moduleC.js', '/node_modules/mypackage/src/folder/moduleC.js')
  testRelativeResolve('./folder/moduleD', '/node_modules/mypackage/src/folder/moduleD.jsx')
  testRelativeResolve('./folder/moduleD.jsx', '/node_modules/mypackage/src/folder/moduleD.jsx')
  testRelativeResolve(
    './folder/../folder/moduleD',
    '/node_modules/mypackage/src/folder/moduleD.jsx',
  )
  // notice the // in the import path
  testRelativeResolve(
    './folder/../folder//moduleD',
    '/node_modules/mypackage/src/folder/moduleD.jsx',
  )
  testRelativeResolve('../src/folder/moduleD', '/node_modules/mypackage/src/folder/moduleD.jsx')
  testRelativeResolve(
    '../../mypackage/src/folder/moduleD',
    '/node_modules/mypackage/src/folder/moduleD.jsx',
  )
  testRelativeResolve(
    '../../mypackage/src/folder/moduleE',
    '/node_modules/mypackage/src/folder/moduleE.json',
  )
  testRelativeResolve(
    './folderWithPackageJson',
    '/node_modules/mypackage/src/folderWithPackageJson/actualModuleE.js',
  )
  testRelativeResolve(
    './folderWithBrokenPackageJson',
    '/node_modules/mypackage/src/folderWithBrokenPackageJson/index.js',
  )
  testRelativeResolve('./nonexistentImport', null)
  testRelativeResolve('./folderWithPackageJsonWithMissingFile', null)
  testRelativeResolve('./folderWithpackageJsonMissingField', null)

  it('resolves absolute relative imports', () => {
    expect(
      resolve(
        '/node_modules/mypackage/src/moduleA.js',
        '/node_modules/mypackage/src/folder/moduleD',
      ),
    ).toEqual('/node_modules/mypackage/src/folder/moduleD.jsx')
  })

  it('throws a friendly error when finding a package.json that has no main entry and only has a module entry', () => {
    expect(resolve('/node_modules/mypackage/src/moduleA.js', 'es-module-only-package')).toEqual(
      '/node_modules/es-module-only-package/index.js',
    )
  })

  it('resolves local project files that may require a loader', () => {
    expect(resolve('/src/app.js', './icon.png')).toEqual('/src/icon.png')
    expect(resolve('/src/app.js', './folder/../icon.png')).toEqual('/src/icon.png')
    expect(resolve('/src/app.js', './simple.css')).toEqual('/src/simple.css')
    expect(resolve('/src/app.js', './thing.js')).toEqual('/src/thing.js')
  })

  it('resolves the Browser field', () => {
    expect(resolve('/src/app.js', 'module-with-browser-module-and-main-fields')).toEqual(
      '/node_modules/module-with-browser-module-and-main-fields/index.browser.js',
    )
  })

  it('resolves the Browser field local file replacement', () => {
    expect(
      resolve('/node_modules/module-with-browser-replacements/index.module.js', './localFile.js'),
    ).toEqual('/node_modules/module-with-browser-replacements/localFile.shim.js')
  })

  it('resolves the Browser field module-to-module replacement', () => {
    expect(
      resolve('/node_modules/module-with-browser-replacements/index.module.js', 'node-only-module'),
    ).toEqual('/node_modules/other-module/index.js')
  })

  it('resolves the Browser field module-to-file replacement', () => {
    expect(
      resolve(
        '/node_modules/module-with-browser-replacements/index.module.js',
        'node-only-module-2',
      ),
    ).toEqual('/node_modules/module-with-browser-replacements/local.shim.js')
  })

  it('resolves the Browser field chained replacement', () => {
    expect(
      resolve(
        '/node_modules/module-with-browser-replacements-chained/index.module.js',
        'some-module',
      ),
    ).toEqual('/node_modules/module-with-browser-replacements-chained/local-shim.js')
  })

  it('resolves the Browser field ignore replacement', () => {
    const resolveResult = resolveModule(
      sampleProjectContents,
      createNodeModules(moduleResolutionExamples.contents),
      '/node_modules/module-with-browser-replacements-ignore-module/index.module.js',
      'some-module',
    )
    expect(resolveResult.type).toEqual('RESOLVE_SUCCESS_IGNORE_MODULE')
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
