import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'

export const RequirementResolutionResult = {
  // the requirement was passed without any fixes
  Passed: 'passed',
  // the requirement was found to be missing, but was fixed
  Fixed: 'fixed',
  // the requirement was found to be missing, but it's not critical
  Partial: 'partial',
  // the requirement was found to be missing and we cannot continue with the project
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
  serverPackages: RequirementResolution
}

export type ProjectRequirement = keyof ProjectRequirements

export function newProjectRequirements(
  storyboard: RequirementResolution,
  packageJsonEntries: RequirementResolution,
  language: RequirementResolution,
  reactVersion: RequirementResolution,
  serverPackages: RequirementResolution,
): ProjectRequirements {
  return {
    storyboard,
    packageJsonEntries,
    language,
    reactVersion,
    serverPackages,
  }
}

export interface RequirementCheckResult {
  resolution: RequirementResolutionResult
  resultText: string
  resultValue?: string
  newProjectContents?: ProjectContentTreeRoot | null
}

export interface RequirementCheck {
  check: (projectContents: ProjectContentTreeRoot) => RequirementCheckResult
}
