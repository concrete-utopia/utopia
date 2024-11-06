import { assertNever } from '../utils'
import type { EditorAction, EditorDispatch } from '../../../components/editor/action-types'
import {
  setImportWizardOpen,
  updateImportOperations,
  updateImportStatus,
} from '../../../components/editor/actions/action-creators'
import { ImportOperationAction } from './import-operation-types'
import type {
  ImportOperation,
  ImportOperationType,
  ImportState,
  ImportStatus,
  TotalImportResult,
} from './import-operation-types'
import { ImportOperationResult } from './import-operation-types'
import { isFeatureEnabled } from '../../../utils/feature-switches'

export function startImportProcess(dispatch: EditorDispatch) {
  const actions: EditorAction[] = [
    updateImportStatus({ status: 'in-progress' }),
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

export function hideImportWizard(dispatch: EditorDispatch) {
  dispatch([setImportWizardOpen(false)])
}

export function notifyOperationStarted(dispatch: EditorDispatch, operation: ImportOperation) {
  const operationWithTime = {
    ...operation,
    timeStarted: Date.now(),
    timeDone: null,
  }
  if (!isFeatureEnabled('Import Wizard')) {
    return
  }
  setTimeout(() => {
    dispatch([updateImportOperations([operationWithTime], ImportOperationAction.Update)])
  }, 0)
}

export function notifyOperationFinished(
  dispatch: EditorDispatch,
  operation: ImportOperation,
  result: ImportOperationResult,
) {
  const timeDone = Date.now()
  const operationWithTime = {
    ...operation,
    timeDone: timeDone,
    result: result,
  }
  if (!isFeatureEnabled('Import Wizard')) {
    return
  }
  setTimeout(() => {
    dispatch([updateImportOperations([operationWithTime], ImportOperationAction.Update)])
  }, 0)
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

export function updateProjectImportStatus(dispatch: EditorDispatch, importStatus: ImportStatus) {
  dispatch([updateImportStatus(importStatus)])
}

export function getTotalImportStatusAndResult(importState: ImportState): TotalImportResult {
  const operations = importState.importOperations
  // if any operation is an error, the total result is immediately an error
  for (const operation of operations) {
    // with critical errors we are done immediately
    if (operation.result == ImportOperationResult.CriticalError) {
      return {
        result: ImportOperationResult.CriticalError,
        importStatus: { status: 'done' },
      }
    }
    // we continue on errors, to let the user decide
    if (operation.result == ImportOperationResult.Error) {
      return {
        result: ImportOperationResult.Error,
        importStatus: importState.importStatus,
      }
    }
  }
  // if any operation is a warning, the total result is a warning
  if (operations.some((op) => op.result == ImportOperationResult.Warn)) {
    return {
      result: ImportOperationResult.Warn,
      importStatus: importState.importStatus,
    }
  }
  return {
    result: ImportOperationResult.Success,
    importStatus: importState.importStatus,
  }
}

export function pauseImport(dispatch: EditorDispatch): Promise<void> {
  return new Promise((resolve) =>
    updateProjectImportStatus(dispatch, { status: 'paused', onResume: resolve }),
  )
}
