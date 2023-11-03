import type { LoadModule, MatchFile, ModuleLoader } from './loader-types'
import { loadModuleResult } from './loader-types'
import * as Babel from '@babel/standalone'
import BabelTransformCommonJS from '@babel/plugin-transform-modules-commonjs'
import ReactTransformPlugin from 'babel-plugin-transform-react-jsx'
import svgToJSX from '@svgr/plugin-jsx'
import { svgToBase64 } from '../shared/file-utils'

const matchFile: MatchFile = (filename: string) => {
  return filename.endsWith('.svg')
}

const loadModule: LoadModule = (filename: string, contents: string) => {
  /*
  // Use @svgr/plugin-jsx to generate the code for a react component that matches https://create-react-app.dev/docs/adding-images-fonts-and-files/#adding-svgs
  const defaultExport = `export default '${svgToBase64(contents)}';`
  const firstPass = svgToJSX(
    contents,
    {
      expandProps: 'end',
      namedExport: 'ReactComponent',
      jsx: {
        babelConfig: {
          plugins: [ReactTransformPlugin],
        },
      },
    },
    {
      componentName: 'Component',
      caller: { previousExport: defaultExport },
    },
  )
  */

  // Temporarily disabled the SVG to React component conversion as it's failing with SVG parser errors on even the most basic SVGs.
  // Also as this is receiving empty `contents` values for `ASSET_FILE` instances we don't want those to trigger failures either.
  const codeToTransform = `import * as React from 'react'
export default () => null
`

  // In theory we should be able to pass in the below plugins and presets into the above call via jsx.babelConfig,
  // as outlined in https://github.com/gregberge/svgr/blob/7eb5ef668c64ce07ab47dcdc543f8d835d5d5596/packages/plugin-jsx/README.md#applying-custom-transformations
  // Unfortunately that fails when trying to pass in the standard presets, so to work around it we're having to run babel manually on the result
  const loadedContents = Babel.transform(codeToTransform, {
    presets: ['es2015'],
    plugins: [BabelTransformCommonJS, ReactTransformPlugin],
    sourceType: 'module',
    sourceFileName: 'unknown',
  }).code

  return loadModuleResult(filename + '.js', loadedContents)
}

export const SVGLoader: ModuleLoader = {
  match: matchFile,
  load: loadModule,
}
