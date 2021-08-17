import { BuiltInDependencies, BuiltInDependency } from './built-in-dependencies-list'

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
