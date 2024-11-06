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
import { objectFilter } from '../../object-utils'

let requirementsToChecks: Record<ProjectRequirement, RequirementCheck> | undefined
export function getChecks(): Record<ProjectRequirement, RequirementCheck> {
  if (requirementsToChecks == null) {
    requirementsToChecks = {
      packageJsonEntries: new CheckPackageJson(),
      language: new CheckLanguage(),
      reactVersion: new CheckReactVersion(),
      storyboard: new CheckStoryboard(),
      serverPackages: new CheckServerPackages(),
    }
  }
  return requirementsToChecks
}

export function checkAndFixUtopiaRequirementsPreParsed(
  dispatch: EditorDispatch,
  projectContents: ProjectContentTreeRoot,
): { result: RequirementResolutionResult; fixedProjectContents: ProjectContentTreeRoot } {
  return checkAndFixUtopiaRequirements(
    dispatch,
    projectContents,
    objectFilter((check) => check.stage === 'pre-parsed', getChecks()),
  )
}

export function checkAndFixUtopiaRequirementsParsed(
  dispatch: EditorDispatch,
  parsedProjectContents: ProjectContentTreeRoot,
): { result: RequirementResolutionResult; fixedProjectContents: ProjectContentTreeRoot } {
  return checkAndFixUtopiaRequirements(
    dispatch,
    parsedProjectContents,
    objectFilter((check) => check.stage === 'parsed', getChecks()),
  )
}

function checkAndFixUtopiaRequirements(
  dispatch: EditorDispatch,
  projectContents: ProjectContentTreeRoot,
  checks: Partial<Record<ProjectRequirement, RequirementCheck>>,
): { result: RequirementResolutionResult; fixedProjectContents: ProjectContentTreeRoot } {
  let updatedProjectContents = projectContents
  let result: RequirementResolutionResult = RequirementResolutionResult.Passed
  // iterate over all checks, updating the project contents as we go
  for (const [name, check] of Object.entries(checks)) {
    const checkName = name as ProjectRequirement
    notifyCheckingRequirement(dispatch, checkName, initialTexts[checkName])
    const checkResult = check.check(updatedProjectContents)
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
    updatedProjectContents = checkResult.newProjectContents ?? updatedProjectContents
  }
  return { result: result, fixedProjectContents: updatedProjectContents }
}
