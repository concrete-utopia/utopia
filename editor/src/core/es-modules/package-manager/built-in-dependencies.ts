import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from '../../../uuiui'
import * as UUIUIDeps from '../../../uuiui-deps'
import * as React from 'react'
import * as ReactDOM from 'react-dom'
import * as EmotionReact from '@emotion/react'
import * as EmotionStyled from '@emotion/styled'

import * as editorPackageJSON from '../../../../package.json'
import * as utopiaAPIPackageJSON from '../../../../../utopia-api/package.json'
import { NO_OP } from '../../shared/utils'

interface BuiltInDependency {
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
export const SafeReactDOM = (isPreview: boolean) => {
  if (isPreview) {
    return ReactDOM
  } else {
    return {
      ...ReactDOM,
      render: NO_OP,
    }
  }
}

const BuiltInDependencies = (isPreview: boolean): Array<BuiltInDependency> => [
  builtInDependency('utopia-api', UtopiaAPI, utopiaAPIPackageJSON.version),
  builtInDependency('uuiui', UUIUI, editorPackageJSON.version),
  builtInDependency('uuiui-deps', UUIUIDeps, editorPackageJSON.version),
  builtInDependency('react', React, editorPackageJSON.dependencies.react),
  builtInDependency(
    'react-dom',
    SafeReactDOM(isPreview),
    editorPackageJSON.dependencies['react-dom'],
  ),
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

function findBuiltInForName(moduleName: string, isPreview: boolean): BuiltInDependency | undefined {
  return BuiltInDependencies(isPreview).find((builtIn) => builtIn.moduleName === moduleName)
}

export function isBuiltInDependency(moduleName: string, isPreview: boolean): boolean {
  return findBuiltInForName(moduleName, isPreview) != null
}

export function resolveBuiltInDependency(moduleName: string, isPreview: boolean): any | undefined {
  return findBuiltInForName(moduleName, isPreview)?.nodeModule
}

export function versionForBuiltInDependency(
  moduleName: string,
  isPreview: boolean,
): string | undefined {
  return findBuiltInForName(moduleName, isPreview)?.version
}
