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

let editorDispatch: EditorDispatch | null = null

const initialTexts: Record<ProjectRequirement, string> = {
  storyboard: 'Checking storyboard.js',
  packageJsonEntries: 'Checking package.json',
  language: 'Checking project language',
  reactVersion: 'Checking React version',
}

export function resetRequirementsResolutions(dispatch: EditorDispatch) {
  editorDispatch = dispatch
  let projectRequirements = emptyProjectRequirements()
  editorDispatch?.([updateProjectRequirements(projectRequirements)])
  notifyOperationStarted({
    type: 'checkRequirements',
    children: Object.keys(projectRequirements).map((key) =>
      importCheckRequirementAndFix(
        key as ProjectRequirement,
        initialTexts[key as ProjectRequirement],
      ),
    ),
  })
}

export function notifyCheckingRequirement(requirement: ProjectRequirement, text: string) {
  editorDispatch?.([
    updateProjectRequirements({
      [requirement]: {
        status: RequirementResolutionStatus.Pending,
      },
    }),
  ])
  notifyOperationStarted({
    type: 'checkRequirementAndFix',
    id: requirement,
    text: text,
  })
}

export function notifyResolveRequirement(
  requirementName: ProjectRequirement,
  resolution: RequirementResolutionResult,
  text: string,
  value?: string,
) {
  editorDispatch?.([
    updateProjectRequirements({
      [requirementName]: {
        status: RequirementResolutionStatus.Done,
        resolution: resolution,
        value: value,
      },
    }),
  ])
  const result =
    resolution === RequirementResolutionResult.Found ||
    resolution === RequirementResolutionResult.Fixed
      ? ImportOperationResult.Success
      : resolution === RequirementResolutionResult.Partial
      ? ImportOperationResult.Warn
      : ImportOperationResult.Error
  notifyOperationFinished(
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
    setTimeout(() => {
      notifyOperationFinished({ type: 'checkRequirements' }, aggregatedDoneStatus)
    }, 0)
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
