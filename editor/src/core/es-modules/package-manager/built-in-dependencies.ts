import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from '../../../uuiui'
import * as UUIUIDeps from '../../../uuiui-deps'
import * as React from 'react'
import * as ReactDOM from 'react-dom'
import * as EmotionReact from '@emotion/react'

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
  return {
    moduleName: moduleName,
    nodeModule: {
      ...baseModule,
      default: baseModule,
    },
    version: version,
  }
}

// Prevent ReactDOM.render from running in the canvas/editor because it's
// entirely likely to cause either havoc or just break.
export const SafeReactDOM = {
  ...ReactDOM,
  render: NO_OP,
}

const BuiltInDependencies: Array<BuiltInDependency> = [
  builtInDependency('utopia-api', UtopiaAPI, utopiaAPIPackageJSON.version),
  builtInDependency('uuiui', UUIUI, editorPackageJSON.version),
  builtInDependency('uuiui-deps', UUIUIDeps, editorPackageJSON.version),
  builtInDependency('react', React, editorPackageJSON.dependencies.react),
  builtInDependency('react-dom', SafeReactDOM, editorPackageJSON.dependencies['react-dom']),
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
]

function findBuiltInForName(moduleName: string): BuiltInDependency | undefined {
  return BuiltInDependencies.find((builtIn) => builtIn.moduleName === moduleName)
}

export function isBuiltInDependency(moduleName: string): boolean {
  return findBuiltInForName(moduleName) != null
}

export function resolveBuiltInDependency(moduleName: string): any | undefined {
  return findBuiltInForName(moduleName)?.nodeModule
}

export function versionForBuiltInDependency(moduleName: string): string | undefined {
  return findBuiltInForName(moduleName)?.version
}
