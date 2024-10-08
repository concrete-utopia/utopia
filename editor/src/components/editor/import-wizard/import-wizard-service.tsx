import type { EditorDispatch } from '../action-types'
import { setImportWizardOpen, updateImportOperations } from '../actions/action-creators'
import type { GithubRepo } from '../store/editor-state'

export function startImportWizard(dispatch: EditorDispatch) {
  dispatch([
    setImportWizardOpen(true),
    updateImportOperations(
      [
        {
          name: 'loadBranch',
          branchName: '',
          githubRepo: {
            owner: '',
            repository: '',
          },
        },
        {
          name: 'loadRepositories',
          githubRepos: [],
        },
      ],
      'add',
    ),
  ])
}

export function showImportWizard(dispatch: EditorDispatch) {
  dispatch([setImportWizardOpen(true)])
}

export function hideImportWizard(dispatch: EditorDispatch) {
  dispatch([setImportWizardOpen(false)])
}

export function updateOperation(dispatch: EditorDispatch, operation: ImportOperation) {
  dispatch([updateImportOperations([operation], 'update')])
}

export function addOperation(dispatch: EditorDispatch, operation: ImportOperation) {
  dispatch([updateImportOperations([operation], 'add')])
}

export function removeOperation(dispatch: EditorDispatch, operation: ImportOperation) {
  dispatch([updateImportOperations([operation], 'remove')])
}

export function notifyOperationStarted(dispatch: EditorDispatch, operation: ImportOperation) {
  const operationWithTime = {
    ...operation,
    timeStarted: Date.now(),
  }
  dispatch([updateImportOperations([operationWithTime], 'update')])
}

export function notifyOperationFinished(
  dispatch: EditorDispatch,
  operation: ImportOperation,
  result: ImportOperationResult,
) {
  const operationWithTime = {
    ...operation,
    timeDone: Date.now(),
    result: result,
  }
  dispatch([updateImportOperations([operationWithTime], 'update')])
}

type ImportOperationData = {
  timeStarted?: number
  timeDone?: number
  result?: ImportOperationResult
  error?: string
}

type ImportOperationResult = 'success' | 'error' | 'partial'

type ImportLoadBranch = {
  name: 'loadBranch'
  branchName: string
  githubRepo: GithubRepo
} & ImportOperationData

type ImportLoadRepositories = {
  name: 'loadRepositories'
  githubRepos: GithubRepo[]
} & ImportOperationData

export type ImportOperation = ImportLoadBranch | ImportLoadRepositories

export type ImportOperationType = 'add' | 'remove' | 'update'
