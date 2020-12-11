import { CSSLoader } from './css-loader'
import { DefaultLoader } from './default-loader'
import { FileLoader } from './file-loader'
import { SVGLoader } from './svg-loader'
import { ModuleLoader } from './loader-types'
import { applyLoaders } from './loaders'

function nameForModuleLoader(moduleLoader: ModuleLoader): string {
  if (moduleLoader === CSSLoader) {
    return 'CSS Loader'
  } else if (moduleLoader === DefaultLoader) {
    return 'Default Loader'
  } else if (moduleLoader === FileLoader) {
    return 'File Loader'
  } else if (moduleLoader === SVGLoader) {
    return 'SVG Loader'
  } else {
    fail('Invalid loader')
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
  verifyCorrectLoaderUsed(DefaultLoader, ['.js', '.jsx', '.ts', '.tsx', '.d.ts', '.json'], '{}')
  verifyCorrectLoaderUsed(FileLoader, ['.avif', '.bmp', '.gif', '.jpg', '.jpeg', '.png'], '')
  verifyCorrectLoaderUsed(SVGLoader, ['.svg'], '<svg/>')
})
