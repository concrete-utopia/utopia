import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import {
  RequirementResolutionResult,
  type RequirementCheck,
  type RequirementCheckResult,
} from '../utopia-requirements-types'
import Semver from 'semver'
import { getProjectDependencies } from '../../../../../components/assets'

const SUPPORTED_REACT_VERSION_RANGE = '16.8.0 - 18.x'

export default class CheckReactRequirement implements RequirementCheck {
  check(projectContents: ProjectContentTreeRoot): RequirementCheckResult {
    const projectDependencies = getProjectDependencies(projectContents)
    const reactVersion = projectDependencies?.react
    if (reactVersion == null) {
      return {
        resolution: RequirementResolutionResult.Critical,
        resultText: 'React is not in dependencies',
      }
    }
    const isMatching = Semver.intersects(reactVersion, SUPPORTED_REACT_VERSION_RANGE)
    return {
      resolution: isMatching
        ? RequirementResolutionResult.Passed
        : RequirementResolutionResult.Critical,
      resultText: isMatching ? 'React version is ok' : 'React version is not in supported range',
      resultValue: reactVersion,
    }
  }
}
