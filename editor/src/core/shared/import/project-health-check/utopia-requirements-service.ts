import {
  importCheckRequirementAndFixPostParse,
  importCheckRequirementAndFixPreParse,
  ImportOperationResult,
} from '../import-operation-types'
import { notifyOperationFinished, notifyOperationStarted } from '../import-operation-service'
import type { EditorDispatch } from '../../../../components/editor/action-types'
import { updateProjectRequirements } from '../../../../components/editor/actions/action-creators'
import type {
  PostParseValidationRequirement,
  PreParseValidationRequirement,
  ProjectRequirement,
  ProjectRequirements,
} from './utopia-requirements-types'
import {
  emptyProjectRequirements,
  RequirementResolutionResult,
  RequirementResolutionStatus,
} from './utopia-requirements-types'
import { isFeatureEnabled } from '../../../../utils/feature-switches'
import { getRequirementStage, getRequirementsToCheck } from './check-utopia-requirements'
import { objectFilter } from '../../object-utils'

export function updateProjectRequirementsStatus(
  dispatch: EditorDispatch,
  projectRequirements: Partial<ProjectRequirements>,
) {
  if (!isFeatureEnabled('Import Wizard')) {
    return
  }
  setTimeout(() => {
    dispatch([updateProjectRequirements(projectRequirements)])
  }, 0)
}

export function resetRequirementsResolutions(dispatch: EditorDispatch) {
  let projectRequirements = emptyProjectRequirements()
  updateProjectRequirementsStatus(dispatch, projectRequirements)
}

export function startPreParseValidation(dispatch: EditorDispatch) {
  const checks = getRequirementsToCheck().preParse
  notifyOperationStarted(dispatch, {
    type: 'checkRequirementsPreParse',
    children: Object.keys(checks).map((key) =>
      importCheckRequirementAndFixPreParse(
        key as ProjectRequirement,
        checks[key as PreParseValidationRequirement].initialText,
      ),
    ),
  })
}

export function startPostParseValidation(dispatch: EditorDispatch) {
  const checks = getRequirementsToCheck().postParse
  notifyOperationStarted(dispatch, {
    type: 'checkRequirementsPostParse',
    children: Object.keys(checks).map((key) =>
      importCheckRequirementAndFixPostParse(
        key as ProjectRequirement,
        checks[key as PostParseValidationRequirement].initialText,
      ),
    ),
  })
}

export function notifyCheckingRequirement(
  dispatch: EditorDispatch,
  requirement: ProjectRequirement,
  text: string,
) {
  const stage = getRequirementStage(requirement)
  updateProjectRequirementsStatus(dispatch, {
    [requirement]: {
      status: RequirementResolutionStatus.Pending,
    },
  })
  notifyOperationStarted(dispatch, {
    type:
      stage === 'preParse' ? 'checkRequirementAndFixPreParse' : 'checkRequirementAndFixPostParse',
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
  const stage = getRequirementStage(requirementName)
  updateProjectRequirementsStatus(dispatch, {
    [requirementName]: {
      status: RequirementResolutionStatus.Done,
      resolution: resolution,
      value: value,
    },
  })
  const result =
    resolution === RequirementResolutionResult.Passed ||
    resolution === RequirementResolutionResult.Fixed
      ? ImportOperationResult.Success
      : resolution === RequirementResolutionResult.Partial
      ? ImportOperationResult.Warn
      : ImportOperationResult.Error
  notifyOperationFinished(
    dispatch,
    {
      type:
        stage === 'preParse' ? 'checkRequirementAndFixPreParse' : 'checkRequirementAndFixPostParse',
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
  let requirements = { ...existingRequirements }
  for (const incomingRequirement of Object.keys(incomingRequirements)) {
    const incomingRequirementName = incomingRequirement as ProjectRequirement
    requirements[incomingRequirementName] = {
      ...requirements[incomingRequirementName],
      ...incomingRequirements[incomingRequirementName],
    }
  }

  notifyAggregatedResult(dispatch, incomingRequirements)

  return requirements
}

function notifyAggregatedResult(
  dispatch: EditorDispatch,
  incomingRequirements: Partial<ProjectRequirements>,
): void {
  const incomingPreParseRequirements = objectFilter(
    (_, key) => getRequirementStage(key as ProjectRequirement) === 'preParse',
    incomingRequirements,
  )
  if (Object.keys(incomingPreParseRequirements).length > 0) {
    const aggregatedStatus = getAggregatedStatus(incomingPreParseRequirements)
    if (aggregatedStatus != null) {
      notifyOperationFinished(dispatch, { type: 'checkRequirementsPreParse' }, aggregatedStatus)
    }
  }

  const incomingPostParseRequirements = objectFilter(
    (_, key) => getRequirementStage(key as ProjectRequirement) === 'postParse',
    incomingRequirements,
  )
  if (Object.keys(incomingPostParseRequirements).length > 0) {
    const aggregatedStatus = getAggregatedStatus(incomingPostParseRequirements)
    if (aggregatedStatus != null) {
      notifyOperationFinished(dispatch, { type: 'checkRequirementsPostParse' }, aggregatedStatus)
    }
  }
}

function getAggregatedStatus(
  requirementsResolutions: Partial<ProjectRequirements>,
): ImportOperationResult | null {
  for (const resolution of Object.values(requirementsResolutions)) {
    if (resolution.resolution == RequirementResolutionResult.Critical) {
      return ImportOperationResult.Error
    }
  }
  for (const resolution of Object.values(requirementsResolutions)) {
    if (resolution.resolution == RequirementResolutionResult.Partial) {
      return ImportOperationResult.Warn
    }
  }
  for (const resolution of Object.values(requirementsResolutions)) {
    if (resolution.status != RequirementResolutionStatus.Done) {
      return null
    }
  }
  return ImportOperationResult.Success
}
export { RequirementResolutionResult }
