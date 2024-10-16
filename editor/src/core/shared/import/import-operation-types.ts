import type { GithubRepo } from '../../../components/editor/store/editor-state'
import type { RequirementResolutionResult } from './proejct-health-check/utopia-requirements-types'

type ImportOperationData = {
  text?: string
  id?: string | null
  timeStarted?: number | null
  timeDone?: number | null
  result?: ImportOperationResult
  error?: string
  children?: ImportOperation[]
}

export enum ImportOperationResult {
  Success = 'success',
  Error = 'error',
  Warn = 'warn',
}

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

export enum ImportOperationAction {
  Add = 'add',
  Remove = 'remove',
  Update = 'update',
  Replace = 'replace',
}
