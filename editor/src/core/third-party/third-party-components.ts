import {
  DependenciesDescriptors,
  DependencyDescriptor,
  DependencyBoundDescriptors,
} from './third-party-types'
import { AntdComponents } from './antd-components'
import { satisfies } from 'semver'
import {
  PossiblyUnversionedNpmDependency,
  RequestedNpmDependency,
  resolvedNpmDependency,
  unversionedNpmDependency,
} from '../shared/npm-dependency-types'
import { NodeModules, isEsCodeFile } from '../shared/project-file-types'
import { fastForEach } from '../shared/utils'
import { parseVersionPackageJsonFile } from '../../utils/package-parser-utils'
import { forEachRight } from '../shared/either'
import { UtopiaApiComponents } from './utopia-api-components'
import { versionForBuiltInDependency } from '../es-modules/package-manager/built-in-dependencies'

const ThirdPartyComponents: DependenciesDescriptors = {
  antd: AntdComponents,
  'utopia-api': UtopiaApiComponents,
}

export function getThirdPartyComponents(
  dependencyName: string,
  dependencyVersion: string,
): DependencyDescriptor | null {
  if (dependencyName in ThirdPartyComponents) {
    const boundsDescriptors: DependencyBoundDescriptors = ThirdPartyComponents[dependencyName]
    const dependencyBounds = Object.keys(boundsDescriptors)
    for (const bounds of dependencyBounds) {
      if (satisfies(dependencyVersion, bounds)) {
        return boundsDescriptors[bounds]
      }
    }
    return null
  } else {
    return null
  }
}

export function parseDependencyVersionFromNodeModules(
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
): Array<PossiblyUnversionedNpmDependency> {
  let result: Array<PossiblyUnversionedNpmDependency> = []
  fastForEach(dependencies, (dependency) => {
    const builtInVersion = versionForBuiltInDependency(dependency.name)
    const version = builtInVersion ?? parseDependencyVersionFromNodeModules(files, dependency.name)
    if (version == null) {
      result.push(unversionedNpmDependency(dependency.name))
    } else {
      result.push(resolvedNpmDependency(dependency.name, version))
    }
  })
  return result
}
