import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import {
  RequirementResolutionResult,
  type RequirementCheck,
  type RequirementCheckResult,
} from '../utopia-requirements-types'
import { getProjectDependencies } from '../../../../../components/assets'

const serverPackagesRestrictionList: RegExp[] = [/^next/, /^remix/, /^astro/, /^svelte/]

export default class CheckServerPackages implements RequirementCheck {
  check(projectContents: ProjectContentTreeRoot): RequirementCheckResult {
    const projectDependencies = getProjectDependencies(projectContents) ?? {}
    const serverPackages = Object.keys(projectDependencies).filter((packageName) =>
      serverPackagesRestrictionList.some((restriction) => restriction.test(packageName)),
    )
    if (serverPackages.length > 0) {
      return {
        resolution: RequirementResolutionResult.Critical,
        resultText: 'Server packages found',
        resultValue: serverPackages.join(', '),
      }
    }
    return {
      resolution: RequirementResolutionResult.Passed,
      resultText: 'No server packages found',
    }
  }
}
