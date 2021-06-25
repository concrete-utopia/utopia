import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from '../../../uuiui'
import * as UUIUIDeps from '../../../uuiui-deps'
import * as React from 'react'
import * as ReactDOM from 'react-dom'
import * as EmotionReact from '@emotion/react'
import * as EmotionStyled from '@emotion/styled'

import * as editorPackageJSON from '../../../../package.json'
import * as utopiaAPIPackageJSON from 'utopia-api/package'
import { NO_OP } from '../../shared/utils'

export interface BuiltInDependency {
  moduleName: string
  nodeModule: any
  version: string
}

function builtInDependency(
  moduleName: string,
  baseModule: any,
  version: string,
): BuiltInDependency {
  if (baseModule.default != null) {
    return {
      moduleName: moduleName,
      nodeModule: baseModule,
      version: version,
    }
  } else {
    // if the module does not have a default, spread one in there to simulate Synthetic Defaults
    return {
      moduleName: moduleName,
      nodeModule: {
        ...baseModule,
        default: baseModule,
      },
      version: version,
    }
  }
}

// Prevent ReactDOM.render from running in the canvas/editor because it's
// entirely likely to cause either havoc or just break.
export const SafeReactDOM = (mode: 'preview' | 'canvas') => {
  if (mode === 'canvas') {
    return {
      ...ReactDOM,
      render: NO_OP,
    }
  } else {
    return ReactDOM
  }
}

export const BuiltInDependencies = (mode: 'preview' | 'canvas'): Array<BuiltInDependency> => [
  builtInDependency('utopia-api', UtopiaAPI, utopiaAPIPackageJSON.version),
  builtInDependency('uuiui', UUIUI, editorPackageJSON.version),
  builtInDependency('uuiui-deps', UUIUIDeps, editorPackageJSON.version),
  builtInDependency('react', React, editorPackageJSON.dependencies.react),
  builtInDependency('react-dom', SafeReactDOM(mode), editorPackageJSON.dependencies['react-dom']),
  builtInDependency(
    '@emotion/react',
    EmotionReact,
    editorPackageJSON.dependencies['@emotion/react'],
  ),
  builtInDependency(
    '@emotion/core',
    EmotionReact,
    editorPackageJSON.dependencies['@emotion/react'],
  ),
  builtInDependency(
    '@emotion/styled',
    EmotionStyled,
    editorPackageJSON.dependencies['@emotion/styled'],
  ),
]
