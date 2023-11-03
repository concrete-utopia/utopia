import { CSSLoader } from './css-loader'
import { DefaultLoader } from './default-loader'
import { FileLoader } from './file-loader'
import type { ModuleLoader } from './loader-types'
import { applyLoaders } from './loaders'
import { JSONLoader } from './json-loader'

function nameForModuleLoader(moduleLoader: ModuleLoader): string {
  if (moduleLoader === CSSLoader) {
    return 'CSS Loader'
  } else if (moduleLoader === DefaultLoader) {
    return 'Default Loader'
  } else if (moduleLoader === FileLoader) {
    return 'File Loader'
  } else if (moduleLoader === JSONLoader) {
    return 'JSON Loader'
  } else {
    throw new Error('Invalid loader')
  }
}

describe('Applying loaders', () => {
  function verifyCorrectLoaderUsed(
    expectedLoader: ModuleLoader,
    fileTypes: Array<string>,
    fileContents: string,
  ) {
    fileTypes.map((fileType) =>
      it(`Applies the ${nameForModuleLoader(
        expectedLoader,
      )} for files with suffix ${fileType}`, () => {
        const filename = `/src/somefile${fileType}`
        const loadModuleResult = applyLoaders(filename, fileContents)
        expect(loadModuleResult).toEqual(expectedLoader.load(filename, fileContents))
      }),
    )
  }

  verifyCorrectLoaderUsed(CSSLoader, ['.css'], '.utopiaClass { background-color: red; }')
  verifyCorrectLoaderUsed(
    DefaultLoader,
    ['.cjs', '.mjs', '.js', '.jsx', '.ts', '.tsx', '.d.ts'],
    '{}',
  )
  verifyCorrectLoaderUsed(JSONLoader, ['.json'], '{}')
  verifyCorrectLoaderUsed(
    FileLoader,
    ['.avif', '.bmp', '.gif', '.jpg', '.jpeg', '.png', '.glb', '.data', '.cica', '.svg'],
    '',
  )
})
