import { importCheckRequirementAndFix, ImportOperationResult } from '../import-operation-types'
import { notifyOperationFinished, notifyOperationStarted } from '../import-operation-service'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import { updateProjectRequirements } from '../../../../components/editor/actions/action-creators'
import type { ProjectRequirement, ProjectRequirements } from './utopia-requirements-types'
import {
  emptyProjectRequirements,
  RequirementResolutionResult,
  RequirementResolutionStatus,
} from './utopia-requirements-types'

const initialTexts: Record<ProjectRequirement, string> = {
  storyboard: 'Checking storyboard.js',
  packageJsonEntries: 'Checking package.json',
  language: 'Checking project language',
  reactVersion: 'Checking React version',
}

export function updateProjectRequirementsStatus(
  dispatch: EditorDispatch,
  projectRequirements: Partial<ProjectRequirements>,
) {
  setTimeout(() => {
    dispatch([updateProjectRequirements(projectRequirements)])
  }, 0)
}

export function resetRequirementsResolutions(dispatch: EditorDispatch) {
  let projectRequirements = emptyProjectRequirements()
  updateProjectRequirementsStatus(dispatch, projectRequirements)
  notifyOperationStarted(dispatch, {
    type: 'checkRequirements',
    children: Object.keys(projectRequirements).map((key) =>
      importCheckRequirementAndFix(
        key as ProjectRequirement,
        initialTexts[key as ProjectRequirement],
      ),
    ),
  })
}

export function notifyCheckingRequirement(
  dispatch: EditorDispatch,
  requirement: ProjectRequirement,
  text: string,
) {
  updateProjectRequirementsStatus(dispatch, {
    [requirement]: {
      status: RequirementResolutionStatus.Pending,
    },
  })
  notifyOperationStarted(dispatch, {
    type: 'checkRequirementAndFix',
    id: requirement,
    text: text,
  })
}

export function notifyResolveRequirement(
  dispatch: EditorDispatch,
  requirementName: ProjectRequirement,
  resolution: RequirementResolutionResult,
  text: string,
  value?: string,
) {
  updateProjectRequirementsStatus(dispatch, {
    [requirementName]: {
      status: RequirementResolutionStatus.Done,
      resolution: resolution,
      value: value,
    },
  })
  const result =
    resolution === RequirementResolutionResult.Found ||
    resolution === RequirementResolutionResult.Fixed
      ? ImportOperationResult.Success
      : resolution === RequirementResolutionResult.Partial
      ? ImportOperationResult.Warn
      : ImportOperationResult.Error
  notifyOperationFinished(
    dispatch,
    {
      type: 'checkRequirementAndFix',
      id: requirementName,
      text: text,
      resolution: resolution,
    },
    result,
  )
}

export function updateRequirements(
  dispatch: EditorDispatch,
  existingRequirements: ProjectRequirements,
  incomingRequirements: Partial<ProjectRequirements>,
): ProjectRequirements {
  let result = { ...existingRequirements }
  for (const incomingRequirement of Object.keys(incomingRequirements)) {
    const incomingRequirementName = incomingRequirement as ProjectRequirement
    result[incomingRequirementName] = {
      ...result[incomingRequirementName],
      ...incomingRequirements[incomingRequirementName],
    }
  }

  const aggregatedDoneStatus = getAggregatedStatus(result)
  if (aggregatedDoneStatus != null) {
    notifyOperationFinished(dispatch, { type: 'checkRequirements' }, aggregatedDoneStatus)
  }

  return result
}

function getAggregatedStatus(
  requirementsResolutions: ProjectRequirements,
): ImportOperationResult | null {
  for (const resolution of Object.values(requirementsResolutions)) {
    if (resolution.status != RequirementResolutionStatus.Done) {
      return null
    }
    if (resolution.resolution == RequirementResolutionResult.Critical) {
      return ImportOperationResult.Error
    }
    if (resolution.resolution == RequirementResolutionResult.Partial) {
      return ImportOperationResult.Warn
    }
  }
  return ImportOperationResult.Success
}
export { RequirementResolutionResult }
