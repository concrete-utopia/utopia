import type {
  PossiblyUnversionedNpmDependency,
  RequestedNpmDependency,
} from '../shared/npm-dependency-types'
import { resolvedNpmDependency, unversionedNpmDependency } from '../shared/npm-dependency-types'
import type { NodeModules } from '../shared/project-file-types'
import { isEsCodeFile } from '../shared/project-file-types'
import { fastForEach } from '../shared/utils'
import { parseVersionPackageJsonFile } from '../../utils/package-parser-utils'
import { forEachRight } from '../shared/either'
import { versionForBuiltInDependency } from '../es-modules/package-manager/built-in-dependencies'
import type { BuiltInDependencies } from '../es-modules/package-manager/built-in-dependencies-list'

function parseDependencyVersionFromNodeModules(
  nodeModules: NodeModules,
  dependencyName: string,
): string | null {
  let version: string | null = null
  const packageJsonFile = nodeModules[`/node_modules/${dependencyName}/package.json`]
  if (packageJsonFile != null && isEsCodeFile(packageJsonFile)) {
    const parseResult = parseVersionPackageJsonFile(packageJsonFile.fileContents)
    forEachRight(parseResult, (resolvedVersion) => {
      version = resolvedVersion
    })
  }
  return version
}

export function resolvedDependencyVersions(
  dependencies: Array<RequestedNpmDependency>,
  files: NodeModules,
  builtInDependencies: BuiltInDependencies,
): Array<PossiblyUnversionedNpmDependency> {
  let result: Array<PossiblyUnversionedNpmDependency> = []
  fastForEach(dependencies, (dependency) => {
    const builtInVersion = versionForBuiltInDependency(builtInDependencies, dependency.name)
    const version = builtInVersion ?? parseDependencyVersionFromNodeModules(files, dependency.name)
    if (version == null) {
      result.push(unversionedNpmDependency(dependency.name))
    } else {
      result.push(resolvedNpmDependency(dependency.name, version))
    }
  })
  return result
}
