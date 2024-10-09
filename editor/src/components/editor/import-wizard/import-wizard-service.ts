import { assertNever } from '../../../core/shared/utils'
import type { EditorDispatch } from '../action-types'
import { setImportWizardOpen, updateImportOperations } from '../actions/action-creators'
import type { GithubRepo } from '../store/editor-state'

let editorDispatch: EditorDispatch | null = null

export function startImportWizard(dispatch: EditorDispatch) {
  editorDispatch = dispatch
  dispatch([
    setImportWizardOpen(true),
    updateImportOperations(
      [
        {
          type: 'loadBranch',
        },
        {
          type: 'parseFiles',
        },
        {
          type: 'checkUtopiaRequirements',
        },
        {
          type: 'refreshDependencies',
        },
      ],
      'replace',
    ),
  ])
}

export function showImportWizard() {
  editorDispatch?.([setImportWizardOpen(true)])
}

export function hideImportWizard() {
  editorDispatch?.([setImportWizardOpen(false)])
}

export function updateOperation(operation: ImportOperation) {
  editorDispatch?.([updateImportOperations([operation], 'update')])
}

export function addOperation(operation: ImportOperation) {
  editorDispatch?.([updateImportOperations([operation], 'add')])
}

export function removeOperation(operation: ImportOperation) {
  editorDispatch?.([updateImportOperations([operation], 'remove')])
}

export function notifyOperationStarted(operation: ImportOperation) {
  const operationWithTime = {
    ...operation,
    timeStarted: Date.now(),
  }
  editorDispatch?.([updateImportOperations([operationWithTime], 'update')])
}

export function notifyOperationFinished(operation: ImportOperation, result: ImportOperationResult) {
  const operationWithTime = {
    ...operation,
    timeDone: Date.now(),
    result: result,
  }
  editorDispatch?.([updateImportOperations([operationWithTime], 'update')])
}

export function areSameOperation(existing: ImportOperation, incoming: ImportOperation): boolean {
  if (existing.id == null || incoming.id == null) {
    return existing.type === incoming.type
  }
  return existing.id === incoming.id
}

type ImportOperationData = {
  id?: string | null
  timeStarted?: number
  timeDone?: number
  result?: ImportOperationResult
  error?: string
  parentOperationType?: ImportOperationType
  children?: ImportOperation[]
}

type ImportOperationResult = 'success' | 'error' | 'partial'

type ImportLoadBranch = {
  type: 'loadBranch'
  branchName?: string
  githubRepo?: GithubRepo
} & ImportOperationData

type ImportRefreshDependencies = {
  type: 'refreshDependencies'
} & ImportOperationData

type ImportFetchDependency = {
  type: 'fetchDependency'
  dependencyName: string
  dependencyVersion: string
} & ImportOperationData

type ImportParseFiles = {
  type: 'parseFiles'
} & ImportOperationData

type ImportCreateStoryboard = {
  type: 'createStoryboard'
} & ImportOperationData

type ImportCreatePackageJsonEntry = {
  type: 'createPackageJsonEntry'
} & ImportOperationData

type ImportCheckUtopiaRequirements = {
  type: 'checkUtopiaRequirements'
} & ImportOperationData

export type ImportOperation =
  | ImportLoadBranch
  | ImportRefreshDependencies
  | ImportParseFiles
  | ImportCreateStoryboard
  | ImportFetchDependency
  | ImportCreatePackageJsonEntry
  | ImportCheckUtopiaRequirements

type ImportOperationType = ImportOperation['type']

export type ImportOperationAction = 'add' | 'remove' | 'update' | 'replace'

export function getImportOperationText(operation: ImportOperation) {
  switch (operation.type) {
    case 'loadBranch':
      return `Loading branch ${operation.githubRepo?.owner}/${operation.githubRepo?.repository}@${operation.branchName}`
    case 'fetchDependency':
      return `Fetching ${operation.dependencyName}@${operation.dependencyVersion}`
    case 'parseFiles':
      return 'Parsing files'
    case 'createStoryboard':
      return 'Creating storyboard file'
    case 'refreshDependencies':
      return 'Fetching dependencies'
    case 'createPackageJsonEntry':
      return 'Creating package.json entry'
    case 'checkUtopiaRequirements':
      return 'Checking Utopia requirements'
    default:
      assertNever(operation)
  }
}

function getParentArray(root: ImportOperation[], operation: ImportOperation): ImportOperation[] {
  if (operation.parentOperationType == null) {
    return root
  }
  const parentIndex = root.findIndex((op) => op.type === operation.parentOperationType)
  if (parentIndex === -1) {
    return root
  }
  const parent = root[parentIndex]
  if (parent.children == null) {
    root[parentIndex] = {
      ...parent,
      children: [],
    }
  }
  return root[parentIndex].children ?? []
}

export function getUpdateOperationResult(
  existingOperations: ImportOperation[],
  incomingOperations: ImportOperation[],
  type: ImportOperationAction,
): ImportOperation[] {
  let operations: ImportOperation[] = existingOperations.map((operation) => ({
    ...operation,
    children: [...(operation.children ?? [])],
  }))
  switch (type) {
    case 'add':
      incomingOperations.forEach((operation) => {
        const parent = getParentArray(operations, operation)
        parent.push(operation)
      })
      break
    case 'remove':
      incomingOperations.forEach((operation) => {
        const parent = getParentArray(operations, operation)
        const idx = parent.findIndex((op) => areSameOperation(op, operation))
        if (idx >= 0) {
          parent.splice(idx, 1)
        }
      })
      break
    case 'update':
      incomingOperations.forEach((operation) => {
        const parent = getParentArray(operations, operation)
        const idx = parent.findIndex((op) => areSameOperation(op, operation))
        if (idx >= 0) {
          parent[idx] = {
            ...parent[idx],
            ...operation,
          }
        }
        // if not found, add it
        if (idx === -1) {
          parent.push(operation)
        }
      })
      break
    case 'replace':
      operations = [...incomingOperations]
      break
    default:
      assertNever(type)
  }
  return operations
}
