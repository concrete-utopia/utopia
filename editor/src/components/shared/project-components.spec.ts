import { getControlsForExternalDependencies } from '../../core/property-controls/property-controls-utils'
import {
  PackageStatusMap,
  PossiblyUnversionedNpmDependency,
  resolvedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { defaultProject } from '../../sample-projects/sample-project-utils'
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
    const propertyControlsInfo: PropertyControlsInfo = getControlsForExternalDependencies(
      dependencies,
    )
    const actualResult = getComponentGroups(
      packageStatus,
      propertyControlsInfo,
      defaultProject().projectContents,
      dependencies,
      '/src/app.js',
    )

    expect(actualResult).toMatchSnapshot()
  })
})
