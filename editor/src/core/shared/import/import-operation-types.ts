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

type ImportLoadBranch = {
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

export type ImportCheckRequirementAndFix = ImportOperationData & {
  type: 'checkRequirementAndFix'
  resolution?: RequirementResolutionResult
  text: string
  id: string
}

export function importCheckRequirementAndFix(
  id: string,
  text: string,
): ImportCheckRequirementAndFix {
  return {
    type: 'checkRequirementAndFix',
    text: text,
    id: id,
  }
}

type ImportCheckRequirements = ImportOperationData & {
  type: 'checkRequirements'
}

export type ImportOperation =
  | ImportLoadBranch
  | ImportRefreshDependencies
  | ImportParseFiles
  | ImportFetchDependency
  | ImportCheckRequirementAndFix
  | ImportCheckRequirements

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
