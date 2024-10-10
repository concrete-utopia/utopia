import { importCheckRequirementAndFix, ImportOperationResult } from './import-operation-types'
import { notifyOperationFinished, notifyOperationStarted } from './import-operation-service'

let requirementsResolutions: Record<string, RequirementResolution> = {
  storyboard: initialResolution(),
  packageJsonEntries: initialResolution(),
  language: initialResolution(),
  reactVersion: initialResolution(),
}

type Requirement = keyof typeof requirementsResolutions

const initialTexts: Record<Requirement, string> = {
  storyboard: 'Checking storyboard.js',
  packageJsonEntries: 'Checking package.json',
  language: 'Checking project language',
  reactVersion: 'Checking React version',
}

function initialResolution(): RequirementResolution {
  return {
    status: RequirementResolutionStatus.NotStarted,
  }
}

export function resetRequirementsResolutions() {
  requirementsResolutions = Object.fromEntries(
    Object.keys(requirementsResolutions).map((key) => [key, initialResolution()]),
  ) as Record<Requirement, RequirementResolution>
  notifyOperationStarted({
    type: 'checkRequirements',
    children: Object.keys(requirementsResolutions).map((key) =>
      importCheckRequirementAndFix(key as Requirement, initialTexts[key as Requirement]),
    ),
  })
}

export function notifyCheckingRequirement(requirement: Requirement, text: string) {
  requirementsResolutions[requirement].status = RequirementResolutionStatus.Pending
  notifyOperationStarted({
    type: 'checkRequirementAndFix',
    id: requirement,
    text: text,
  })
}

export function notifyResolveRequirement(
  requirementName: Requirement,
  resolution: RequirementResolutionResult,
  text: string,
  value?: string,
) {
  requirementsResolutions[requirementName] = {
    status: RequirementResolutionStatus.Done,
    resolution: resolution,
    value: value,
  }
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
  const aggregatedDoneStatus = getAggregatedStatus()
  if (aggregatedDoneStatus != null) {
    notifyOperationFinished({ type: 'checkRequirements' }, aggregatedDoneStatus)
  }
}

function getAggregatedStatus(): ImportOperationResult | null {
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

enum RequirementResolutionStatus {
  NotStarted = 'not-started',
  Pending = 'pending',
  Done = 'done',
}

export enum RequirementResolutionResult {
  Found = 'found',
  Fixed = 'fixed',
  Partial = 'partial',
  Critical = 'critical',
}

type RequirementResolution = {
  status: RequirementResolutionStatus
  value?: string
  resolution?: RequirementResolutionResult
}
