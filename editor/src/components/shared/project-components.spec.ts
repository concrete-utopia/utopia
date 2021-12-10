import { right } from '../../core/shared/either'
import {
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
  resolvedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { DefaultThirdPartyControlDefinitions } from '../../core/third-party/third-party-controls'
import { simpleDefaultProjectPreParsed } from '../../sample-projects/sample-project-utils.test-utils'
import { PropertyControlsInfo } from '../custom-code/code-file'
import { getComponentGroups, getDependencyStatus } from './project-components'

describe('getComponentGroups', () => {
  it('returns all the various default groups', () => {
    const packageStatus: PackageStatusMap = {
      antd: { status: 'loaded' },
    }
    const dependencies: Array<PossiblyUnversionedNpmDependency> = [
      resolvedNpmDependency('antd', '4.1.0'),
    ]

    const propertyControlsInfo: PropertyControlsInfo = {
      antd: DefaultThirdPartyControlDefinitions.antd,
    }
    const actualResult = getComponentGroups(
      packageStatus,
      propertyControlsInfo,
      simpleDefaultProjectPreParsed().projectContents,
      dependencies,
      '/src/app.js',
    )

    expect(actualResult).toMatchSnapshot()
  })
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
          propertyControls: right({}),
          insertOptions: [],
        },
      },
    }
    const actualResult = getComponentGroups(
      packageStatus,
      propertyControlsInfo,
      simpleDefaultProjectPreParsed().projectContents,
      dependencies,
      '/src/app.js',
    )

    expect(actualResult).toMatchSnapshot()
  })
})

describe('getDependencyStatus', () => {
  it('shows the dependency as loaded when property controls exist for a sub-dependency', () => {
    const propertyControlsInfo: PropertyControlsInfo = {
      '@heroicons/react/solid': {
        BeakerIcon: {
          propertyControls: right({}),
          insertOptions: [],
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
