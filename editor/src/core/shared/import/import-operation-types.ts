import type { GithubRepo } from '../../../components/editor/store/editor-state'
import type { RequirementResolutionResult } from './utopia-requirements-service'

type ImportOperationData = {
  text?: string
  id?: string | null
  timeStarted?: number | null
  timeDone?: number | null
  result?: ImportOperationResult
  error?: string
  parentOperationType?: ImportOperationType
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

type ImportRefreshDependencies = {
  type: 'refreshDependencies'
} & ImportOperationData

type ImportFetchDependency = ImportOperationData & {
  type: 'fetchDependency'
  dependencyName: string
  dependencyVersion: string
  id: string
}

type ImportParseFiles = {
  type: 'parseFiles'
} & ImportOperationData

type ImportCheckRequirementAndFix = ImportOperationData & {
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

type ImportCheckRequirements = {
  type: 'checkRequirements'
} & ImportOperationData

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
