import {
  DependenciesDescriptors,
  DependencyDescriptor,
  DependencyBoundDescriptors,
} from './third-party-types'
import { AntdComponents } from './antd-components'
import { satisfies } from 'semver'
import { NpmDependency } from '../shared/npm-dependency-types'
import { NodeModules, isEsCodeFile } from '../shared/project-file-types'
import { fastForEach } from '../shared/utils'
import { parseVersionPackageJsonFile } from '../../utils/package-parser-utils'
import { forEachRight } from '../shared/either'

const ThirdPartyComponents: DependenciesDescriptors = {
  antd: AntdComponents,
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

export function resolvedDependencyVersions(
  dependencies: Array<NpmDependency>,
  files: NodeModules,
): Array<NpmDependency> {
  let result: Array<NpmDependency> = []
  fastForEach(dependencies, (dependency) => {
    const packageJsonFile = files[`/node_modules/${dependency.name}/package.json`]
    if (packageJsonFile != null && isEsCodeFile(packageJsonFile)) {
      const parseResult = parseVersionPackageJsonFile(packageJsonFile.fileContents)
      forEachRight(parseResult, (resolvedVersion) => {
        result.push({ name: dependency.name, version: resolvedVersion })
      })
    }
  })
  return result
}
