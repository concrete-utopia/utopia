import type { GithubRepo } from '../../../components/editor/store/editor-state'
import type { RequirementResolutionResult } from './project-health-check/utopia-requirements-types'

type ImportOperationData = {
  text?: string
  id?: string | null
  timeStarted?: number | null
  timeDone?: number | null
  result?: ImportOperationResult
  children?: ImportOperation[]
}

export const ImportOperationResult = {
  Success: 'success',
  Warn: 'warn',
  // an error that shows "continue anyway" button
  Error: 'error',
  // an error that we can't recover from
  CriticalError: 'criticalError',
} as const

export type ImportOperationResult =
  (typeof ImportOperationResult)[keyof typeof ImportOperationResult]

export type ImportLoadBranch = {
  type: 'loadBranch'
  branchName?: string
  githubRepo?: GithubRepo
} & ImportOperationData

type ImportRefreshDependencies = ImportOperationData & {
  type: 'refreshDependencies'
}

export type ImportFetchDependency = ImportOperationData & {
  type: 'fetchDependency'
  dependencyName: string
  dependencyVersion: string
  id: string
}

type ImportParseFiles = {
  type: 'parseFiles'
} & ImportOperationData

export type ImportCheckRequirementAndFixPostParse = ImportOperationData & {
  type: 'checkRequirementAndFixPostParse'
  resolution?: RequirementResolutionResult
  text: string
  id: string
}

export function importCheckRequirementAndFixPostParse(
  id: string,
  text: string,
): ImportCheckRequirementAndFixPostParse {
  return {
    type: 'checkRequirementAndFixPostParse',
    text: text,
    id: id,
  }
}

export type ImportCheckRequirementAndFixPreParse = ImportOperationData & {
  type: 'checkRequirementAndFixPreParse'
  resolution?: RequirementResolutionResult
  text: string
  id: string
}

export function importCheckRequirementAndFixPreParse(
  id: string,
  text: string,
): ImportCheckRequirementAndFixPreParse {
  return {
    type: 'checkRequirementAndFixPreParse',
    text: text,
    id: id,
  }
}

type ImportCheckRequirementsPostParse = ImportOperationData & {
  type: 'checkRequirementsPostParse'
}

type ImportCheckRequirementsPreParse = ImportOperationData & {
  type: 'checkRequirementsPreParse'
}

export type ImportOperation =
  | ImportLoadBranch
  | ImportRefreshDependencies
  | ImportParseFiles
  | ImportFetchDependency
  | ImportCheckRequirementAndFixPreParse
  | ImportCheckRequirementAndFixPostParse
  | ImportCheckRequirementsPreParse
  | ImportCheckRequirementsPostParse

export type ImportOperationType = ImportOperation['type']

export type ImportState = {
  importStatus: ImportStatus
  importOperations: ImportOperation[]
}

export function newImportState(importStatus: ImportStatus, importOperations: ImportOperation[]) {
  return {
    importStatus: importStatus,
    importOperations: importOperations,
  }
}

export function emptyImportState(): ImportState {
  return newImportState({ status: 'not-started' }, [])
}

export type ImportStatus =
  | ImportStatusNotStarted
  | ImportStatusInProgress
  | ImportStatusDone
  | ImportStatusPaused

export type ImportStatusNotStarted = { status: 'not-started' }
export type ImportStatusInProgress = { status: 'in-progress' }
export type ImportStatusDone = { status: 'done' }
export type ImportStatusPaused = { status: 'paused'; onResume: () => void }

export const ImportOperationAction = {
  Add: 'add',
  Remove: 'remove',
  Update: 'update',
  Replace: 'replace',
} as const

export type ImportOperationAction =
  (typeof ImportOperationAction)[keyof typeof ImportOperationAction]

export type TotalImportResult = {
  importStatus: ImportStatus
  result: ImportOperationResult
}
