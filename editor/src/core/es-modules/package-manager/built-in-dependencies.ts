import * as UtopiaAPI from 'utopia-api'
import * as UUIUI from 'uuiui'
import * as UUIUIDeps from 'uuiui-deps'
import * as React from 'react'
import * as ReactDOM from 'react-dom'

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

// TODO Figure out how to sync this with our package.json
const BuiltInDependencies: Array<BuiltInDependency> = [
  builtInDependency('utopia-api', UtopiaAPI, '0.4.2'),
  builtInDependency('uuiui', UUIUI, '0.1.0'),
  builtInDependency('uuiui-deps', UUIUIDeps, '0.1.0'),
  builtInDependency('react', React, '17.0.0-rc.1'),
  builtInDependency('react-dom', ReactDOM, '17.0.0-rc.1'),
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
