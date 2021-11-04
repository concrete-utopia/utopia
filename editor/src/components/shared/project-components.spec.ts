import {
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
  resolvedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { DefaultThirdPartyControlDefinitions } from '../../core/third-party/third-party-controls'
import { simpleDefaultProjectPreParsed } from '../../sample-projects/sample-project-utils.test-utils'
import { PropertyControlsInfo } from '../custom-code/code-file'
import { getComponentGroups } from './project-components'

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
})
