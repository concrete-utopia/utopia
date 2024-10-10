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

export function dispatchUpdateOperation(operation: ImportOperation) {
  editorDispatch?.([updateImportOperations([operation], 'update')])
}

export function notifyOperationStarted(operation: ImportOperation) {
  const operationWithTime = {
    ...operation,
    timeStarted: Date.now(),
    timeDone: null,
  }
  editorDispatch?.([updateImportOperations([operationWithTime], 'update')])
}

export function notifyOperationFinished(operation: ImportOperation, result: ImportOperationResult) {
  const timeDone = Date.now()
  const operationWithTime = {
    ...operation,
    timeDone: timeDone,
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

type ImportOperationType = ImportOperation['type']

export type ImportOperationAction = 'add' | 'remove' | 'update' | 'replace'

export const defaultParentTypes: Partial<Record<ImportOperationType, ImportOperationType>> = {
  checkUtopiaRequirementAndFix: 'checkUtopiaRequirements',
  fetchDependency: 'refreshDependencies',
}

function getParentArray(root: ImportOperation[], operation: ImportOperation): ImportOperation[] {
  const parentOperationType = operation.parentOperationType ?? defaultParentTypes[operation.type]
  if (parentOperationType == null) {
    return root
  }
  const parentIndex = root.findIndex((op) => op.type === parentOperationType)
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
