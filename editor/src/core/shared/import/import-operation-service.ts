import { assertNever } from '../utils'
import type { EditorAction, EditorDispatch } from '../../../components/editor/action-types'
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
import { isFeatureEnabled } from '../../../utils/feature-switches'

let editorDispatch: EditorDispatch | null = null

export function startImportProcess(dispatch: EditorDispatch) {
  editorDispatch = dispatch
  const actions: EditorAction[] = [
    updateImportOperations(
      [
        { type: 'loadBranch' },
        { type: 'parseFiles' },
        { type: 'checkRequirements' },
        { type: 'refreshDependencies' },
      ],
      ImportOperationAction.Replace,
    ),
  ]
  if (isFeatureEnabled('Import Wizard')) {
    actions.push(setImportWizardOpen(true))
  }
  dispatch(actions)
}

export function hideImportWizard() {
  editorDispatch?.([setImportWizardOpen(false)])
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
} as const

function getParentArray(root: ImportOperation[], operation: ImportOperation): ImportOperation[] {
  const parentOperationType = defaultParentTypes[operation.type]
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
