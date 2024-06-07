import type {
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { resolvedNpmDependency } from '../../core/shared/npm-dependency-types'
import { DefaultThirdPartyControlDefinitions } from '../../core/third-party/third-party-controls'
import { simpleDefaultProjectPreParsed } from '../../sample-projects/sample-project-utils.test-utils'
import {
  ComponentDescriptorDefaults,
  defaultComponentDescriptor,
  type PropertyControlsInfo,
} from '../custom-code/code-file'
import {
  clearInsertableComponentGroupUniqueIDs,
  getComponentGroups,
  getDependencyStatus,
} from './project-components'

describe('getComponentGroups', () => {
  it('handles a sub-dependency correctly', () => {
    const packageStatus: PackageStatusMap = {
      '@heroicons/react': { status: 'loaded' },
    }
    const dependencies: Array<PossiblyUnversionedNpmDependency> = [
      resolvedNpmDependency('@heroicons/react', '1.0.5'),
    ]

    const propertyControlsInfo: PropertyControlsInfo = {
      '@heroicons/react/solid': {
        BeakerIcon: {
          properties: {},
          supportsChildren: false,
          preferredChildComponents: [],
          variants: [],
          source: defaultComponentDescriptor(),
          ...ComponentDescriptorDefaults,
        },
      },
    }
    const actualResult = getComponentGroups(
      'insert',
      packageStatus,
      propertyControlsInfo,
      simpleDefaultProjectPreParsed().projectContents,
      dependencies,
      '/src/app.js',
    ).map(clearInsertableComponentGroupUniqueIDs)

    expect(actualResult).toMatchSnapshot()
  })
})

describe('getDependencyStatus', () => {
  it('shows the dependency as loaded when property controls exist for a sub-dependency', () => {
    const propertyControlsInfo: PropertyControlsInfo = {
      '@heroicons/react/solid': {
        BeakerIcon: {
          properties: {},
          supportsChildren: false,
          preferredChildComponents: [],
          variants: [],
          source: defaultComponentDescriptor(),
          ...ComponentDescriptorDefaults,
        },
      },
    }
    const packageStatus: PackageStatusMap = {
      '@heroicons/react': {
        status: 'loaded',
      },
    }
    const result = getDependencyStatus(
      packageStatus,
      propertyControlsInfo,
      '@heroicons/react',
      'version-lookup',
    )
    expect(result).toEqual('loaded')
  })
})
