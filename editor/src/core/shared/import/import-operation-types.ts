import type { GithubRepo } from '../../../components/editor/store/editor-state'

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

type UtopiaRequirementResolution = 'found' | 'fixed' | 'critical' | 'partial'

export type ImportOperationResult = 'success' | 'error' | 'warn'

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

type ImportCheckUtopiaRequirementAndFix = ImportOperationData & {
  type: 'checkUtopiaRequirementAndFix'
  resolution?: UtopiaRequirementResolution
  text: string
  id: string
}

export function importCheckUtopiaRequirementAndFix(
  id: string,
  text: string,
): ImportCheckUtopiaRequirementAndFix {
  return {
    type: 'checkUtopiaRequirementAndFix',
    text: text,
    id: id,
  }
}

type ImportCheckUtopiaRequirements = {
  type: 'checkUtopiaRequirements'
} & ImportOperationData

export type ImportOperation =
  | ImportLoadBranch
  | ImportRefreshDependencies
  | ImportParseFiles
  | ImportFetchDependency
  | ImportCheckUtopiaRequirementAndFix
  | ImportCheckUtopiaRequirements

export type ImportOperationType = ImportOperation['type']

export type ImportOperationAction = 'add' | 'remove' | 'update' | 'replace'
