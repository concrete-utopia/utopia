import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import CheckPackageJson from './requirements/requirement-package-json'
import CheckLanguage from './requirements/requirement-language'
import CheckReactVersion from './requirements/requirement-react'
import { RequirementResolutionResult } from './utopia-requirements-types'
import type {
  ProjectRequirement,
  RequirementCheck,
  RequirementCheckStage,
  RequirementsByStage,
} from './utopia-requirements-types'
import { notifyCheckingRequirement, notifyResolveRequirement } from './utopia-requirements-service'
import CheckStoryboard from './requirements/requirement-storyboard'
import CheckServerPackages from './requirements/requirement-server-packages'

let requirementsToCheck: RequirementsByStage | undefined
export function getRequirementsToCheck(): RequirementsByStage {
  if (requirementsToCheck == null) {
    requirementsToCheck = {
      preParse: {
        packageJsonEntries: new CheckPackageJson(),
        language: new CheckLanguage(),
        reactVersion: new CheckReactVersion(),
      },
      postParse: {
        storyboard: new CheckStoryboard(),
        serverPackages: new CheckServerPackages(),
      },
    }
  }
  return requirementsToCheck
}

export function checkAndFixUtopiaRequirementsPreParsed(
  dispatch: EditorDispatch,
  projectContents: ProjectContentTreeRoot,
): { result: RequirementResolutionResult; fixedProjectContents: ProjectContentTreeRoot } {
  return checkAndFixUtopiaRequirements(dispatch, projectContents, getRequirementsToCheck().preParse)
}

export function checkAndFixUtopiaRequirementsParsed(
  dispatch: EditorDispatch,
  parsedProjectContents: ProjectContentTreeRoot,
): { result: RequirementResolutionResult; fixedProjectContents: ProjectContentTreeRoot } {
  return checkAndFixUtopiaRequirements(
    dispatch,
    parsedProjectContents,
    getRequirementsToCheck().postParse,
  )
}

export function getRequirementStage(requirement: ProjectRequirement): RequirementCheckStage {
  if (requirement in getRequirementsToCheck().preParse) {
    return 'preParse'
  }
  return 'postParse'
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
    notifyCheckingRequirement(dispatch, checkName, check.initialText)
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
