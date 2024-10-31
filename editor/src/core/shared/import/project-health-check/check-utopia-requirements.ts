import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import CheckPackageJson from './requirements/requirement-package-json'
import CheckLanguage from './requirements/requirement-language'
import CheckReactVersion from './requirements/requirement-react'
import { RequirementResolutionResult } from './utopia-requirements-types'
import type { ProjectRequirement, RequirementCheck } from './utopia-requirements-types'
import {
  initialTexts,
  notifyCheckingRequirement,
  notifyResolveRequirement,
} from './utopia-requirements-service'
import CheckStoryboard from './requirements/requirement-storyboard'
import CheckServerPackages from './requirements/requirement-server-packages'

export function checkAndFixUtopiaRequirements(
  dispatch: EditorDispatch,
  parsedProjectContents: ProjectContentTreeRoot,
): { result: RequirementResolutionResult; fixedProjectContents: ProjectContentTreeRoot } {
  const checks: Record<ProjectRequirement, RequirementCheck> = {
    storyboard: new CheckStoryboard(),
    packageJsonEntries: new CheckPackageJson(),
    language: new CheckLanguage(),
    reactVersion: new CheckReactVersion(),
    serverPackages: new CheckServerPackages(),
  }
  let projectContents = parsedProjectContents
  let result: RequirementResolutionResult = RequirementResolutionResult.Passed
  // iterate over all checks, updating the project contents as we go
  for (const [name, check] of Object.entries(checks)) {
    const checkName = name as ProjectRequirement
    notifyCheckingRequirement(dispatch, checkName, initialTexts[checkName])
    const checkResult = check.check(projectContents)
    if (checkResult.resolution === RequirementResolutionResult.Critical) {
      result = RequirementResolutionResult.Critical
    }
    notifyResolveRequirement(
      dispatch,
      checkName,
      checkResult.resolution,
      checkResult.resultText,
      checkResult.resultValue,
    )
    projectContents = checkResult.newProjectContents ?? projectContents
  }
  return { result: result, fixedProjectContents: projectContents }
}
