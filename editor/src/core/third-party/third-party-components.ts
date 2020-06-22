import {
  DependenciesDescriptors,
  DependencyDescriptor,
  DependencyBoundDescriptors,
} from './third-party-types'
import { AntdComponents } from './antd-components'
import { satisfies } from 'semver'

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
