import type { BuiltInDependency, BuiltInDependencies } from './built-in-dependencies-list'

function findBuiltInForName(
  builtInDependencies: BuiltInDependencies,
  moduleName: string,
): BuiltInDependency | undefined {
  return builtInDependencies.find((builtIn) => builtIn.moduleName === moduleName)
}

export function isBuiltInDependency(
  builtInDependencies: BuiltInDependencies,
  moduleName: string,
): boolean {
  return findBuiltInForName(builtInDependencies, moduleName) != null
}

export function resolveBuiltInDependency(
  builtInDependencies: BuiltInDependencies,
  moduleName: string,
): any | undefined {
  return findBuiltInForName(builtInDependencies, moduleName)?.nodeModule
}

export function versionForBuiltInDependency(
  builtInDependencies: BuiltInDependencies,
  moduleName: string,
): string | undefined {
  return findBuiltInForName(builtInDependencies, moduleName)?.version
}
