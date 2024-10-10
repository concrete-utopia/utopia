import { assertNever } from '../utils'
import type { EditorDispatch } from '../../../components/editor/action-types'
import {
  setImportWizardOpen,
  updateImportOperations,
} from '../../../components/editor/actions/action-creators'
import { ImportOperationAction } from './import-operation-types'
import type {
  ImportOperation,
  ImportOperationResult,
  ImportOperationType,
} from './import-operation-types'

let editorDispatch: EditorDispatch | null = null

export function startImportWizard(dispatch: EditorDispatch) {
  editorDispatch = dispatch
  dispatch([
    setImportWizardOpen(true),
    updateImportOperations(
      [
        { type: 'loadBranch' },
        { type: 'parseFiles' },
        { type: 'checkRequirements' },
        { type: 'refreshDependencies' },
      ],
      ImportOperationAction.Replace,
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
  editorDispatch?.([updateImportOperations([operation], ImportOperationAction.Update)])
}

export function notifyOperationStarted(operation: ImportOperation) {
  const operationWithTime = {
    ...operation,
    timeStarted: Date.now(),
    timeDone: null,
  }
  editorDispatch?.([updateImportOperations([operationWithTime], ImportOperationAction.Update)])
}

export function notifyOperationFinished(operation: ImportOperation, result: ImportOperationResult) {
  const timeDone = Date.now()
  const operationWithTime = {
    ...operation,
    timeDone: timeDone,
    result: result,
  }
  editorDispatch?.([updateImportOperations([operationWithTime], ImportOperationAction.Update)])
}

export function areSameOperation(existing: ImportOperation, incoming: ImportOperation): boolean {
  if (existing.id == null || incoming.id == null) {
    return existing.type === incoming.type
  }
  return existing.id === incoming.id
}

export const defaultParentTypes: Partial<Record<ImportOperationType, ImportOperationType>> = {
  checkRequirementAndFix: 'checkRequirements',
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
    case ImportOperationAction.Add:
      incomingOperations.forEach((operation) => {
        const parent = getParentArray(operations, operation)
        parent.push(operation)
      })
      break
    case ImportOperationAction.Remove:
      incomingOperations.forEach((operation) => {
        const parent = getParentArray(operations, operation)
        const idx = parent.findIndex((op) => areSameOperation(op, operation))
        if (idx >= 0) {
          parent.splice(idx, 1)
        }
      })
      break
    case ImportOperationAction.Update:
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
    case ImportOperationAction.Replace:
      operations = [...incomingOperations]
      break
    default:
      assertNever(type)
  }
  return operations
}
