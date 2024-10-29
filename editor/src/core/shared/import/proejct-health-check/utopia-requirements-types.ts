import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'

export const RequirementResolutionResult = {
  Found: 'found',
  Fixed: 'fixed',
  Partial: 'partial',
  Critical: 'critical',
} as const

export type RequirementResolutionResult =
  (typeof RequirementResolutionResult)[keyof typeof RequirementResolutionResult]

export const RequirementResolutionStatus = {
  NotStarted: 'not-started',
  Pending: 'pending',
  Done: 'done',
}

export type RequirementResolutionStatus =
  (typeof RequirementResolutionStatus)[keyof typeof RequirementResolutionStatus]

export function emptyRequirementResolution(): RequirementResolution {
  return {
    status: RequirementResolutionStatus.NotStarted,
    value: null,
    resolution: null,
  }
}

export function emptyProjectRequirements(): ProjectRequirements {
  return newProjectRequirements(
    emptyRequirementResolution(),
    emptyRequirementResolution(),
    emptyRequirementResolution(),
    emptyRequirementResolution(),
  )
}

export interface RequirementResolution {
  status: RequirementResolutionStatus
  value?: string | null
  resolution?: RequirementResolutionResult | null
}

export function requirementResolution(
  status: RequirementResolutionStatus,
  value?: string | null,
  resolution?: RequirementResolutionResult | null,
): RequirementResolution {
  return {
    status,
    value,
    resolution,
  }
}

export interface ProjectRequirements {
  storyboard: RequirementResolution
  packageJsonEntries: RequirementResolution
  language: RequirementResolution
  reactVersion: RequirementResolution
}

export type ProjectRequirement = keyof ProjectRequirements

export function newProjectRequirements(
  storyboard: RequirementResolution,
  packageJsonEntries: RequirementResolution,
  language: RequirementResolution,
  reactVersion: RequirementResolution,
): ProjectRequirements {
  return {
    storyboard,
    packageJsonEntries,
    language,
    reactVersion,
  }
}

export interface RequirementCheck {
  check: (projectContents: ProjectContentTreeRoot) => {
    resolution: RequirementResolutionResult
    resultText: string
    resultValue?: string
    newProjectContents?: ProjectContentTreeRoot | null
  }
  getRequirementName: () => keyof ProjectRequirements
  getStartText: () => string
}

export type RequirementCheckResult = ReturnType<RequirementCheck['check']>
