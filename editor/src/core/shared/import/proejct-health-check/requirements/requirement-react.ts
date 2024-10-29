import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import type { ProjectRequirements } from '../utopia-requirements-types'
import {
  RequirementResolutionResult,
  type RequirementCheck,
  type RequirementCheckResult,
} from '../utopia-requirements-types'
import { getPackageJson, getPackageLockJson } from '../check-utopia-requirements'
import Semver from 'semver'

const SUPPORTED_REACT_VERSION_RANGE = '16.8.0 - 18.x'

export default class CheckReactRequirement implements RequirementCheck {
  getRequirementName(): keyof ProjectRequirements {
    return 'reactVersion'
  }
  getStartText(): string {
    return 'Checking React version'
  }
  check(projectContents: ProjectContentTreeRoot): RequirementCheckResult {
    const parsedPackageLockJson = getPackageLockJson(projectContents)
    // check package-lock.json first
    let reactVersion = parsedPackageLockJson?.dependencies?.react
    if (reactVersion == null) {
      const parsedPackageJson = getPackageJson(projectContents)
      // then check package.json
      reactVersion = parsedPackageJson?.dependencies?.react
    }
    if (reactVersion == null) {
      return {
        resolution: RequirementResolutionResult.Critical,
        resultText: 'React is not in dependencies',
      }
    }
    const isMatching = Semver.intersects(reactVersion, SUPPORTED_REACT_VERSION_RANGE)
    return {
      resolution: isMatching
        ? RequirementResolutionResult.Found
        : RequirementResolutionResult.Critical,
      resultText: isMatching ? 'React version is ok' : 'React version is not in supported range',
      resultValue: reactVersion,
    }
  }
}
