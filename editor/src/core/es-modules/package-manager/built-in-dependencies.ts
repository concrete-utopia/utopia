import { BuiltInDependencies, BuiltInDependency } from './built-in-dependencies-list'

function findBuiltInForName(
  moduleName: string,
  mode: 'preview' | 'canvas',
): BuiltInDependency | undefined {
  return BuiltInDependencies(mode).find((builtIn) => builtIn.moduleName === moduleName)
}

export function isBuiltInDependency(moduleName: string, mode: 'preview' | 'canvas'): boolean {
  return findBuiltInForName(moduleName, mode) != null
}

export function resolveBuiltInDependency(
  moduleName: string,
  mode: 'preview' | 'canvas',
): any | undefined {
  return findBuiltInForName(moduleName, mode)?.nodeModule
}

export function versionForBuiltInDependency(
  moduleName: string,
  mode: 'preview' | 'canvas',
): string | undefined {
  return findBuiltInForName(moduleName, mode)?.version
}
