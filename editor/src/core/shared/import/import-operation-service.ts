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
import { sendDiscordMessage } from '../../../components/editor/server'
import type { DiscordMessage, DiscordMessageType } from 'utopia-shared/src/types'
import { getImportOperationText } from '../../../components/editor/import-wizard/import-wizard-helpers'

export function startImportProcess(dispatch: EditorDispatch) {
  const actions: EditorAction[] = [
    updateImportStatus({ status: 'in-progress' }),
    updateImportOperations(
      [
        { type: 'loadBranch' },
        { type: 'checkRequirementsPreParse' },
        { type: 'parseFiles' },
        { type: 'checkRequirementsPostParse' },
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

export function notifyOperationCriticalError(dispatch: EditorDispatch, operation: ImportOperation) {
  notifyOperationFinished(dispatch, operation, ImportOperationResult.CriticalError)
  setTimeout(() => updateProjectImportStatus(dispatch, { status: 'done' }), 0)
}

export function areSameOperation(existing: ImportOperation, incoming: ImportOperation): boolean {
  if (existing.id == null || incoming.id == null) {
    return existing.type === incoming.type
  }
  return existing.id === incoming.id
}

export const defaultParentTypes: Partial<Record<ImportOperationType, ImportOperationType>> = {
  checkRequirementAndFixPreParse: 'checkRequirementsPreParse',
  checkRequirementAndFixPostParse: 'checkRequirementsPostParse',
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

export function notifyImportStatusToDiscord(
  importState: ImportState,
  projectName: string,
  // this is for cases when the import was paused, but the user left the wizard mid-process
  forceNotify: boolean = false,
) {
  const totalImportStateAndResult = getTotalImportStatusAndResult(importState)
  if (totalImportStateAndResult.importStatus.status != 'done' && !forceNotify) {
    return
  }

  let messageType: DiscordMessageType
  let message: DiscordMessage

  const importBranchOp = importState.importOperations.find((op) => op.type == 'loadBranch')
  const githubRepo = importBranchOp?.githubRepo
  const githubBranch = importBranchOp?.branchName

  const operationsWithErrorsOrWarnings = importState.importOperations
    .flatMap((op) => [op, ...(op.children ?? [])])
    .filter((op) => op.result != null && op.result != ImportOperationResult.Success)
    .reduce((acc, op) => {
      if (op.result == null) {
        return acc
      }
      acc[op.result] = acc[op.result] ?? ''
      acc[op.result] += `- ${getImportOperationText(op)}\n`
      return acc
    }, {} as Record<ImportOperationResult, string>)

  const fields = {
    'Project Name': projectName,
    'Github URL':
      githubRepo != null ? `https://github.com/${githubRepo.owner}/${githubRepo.repository}` : '',
    'Github Branch': githubBranch ?? '',
    ...operationsWithErrorsOrWarnings,
  }

  switch (totalImportStateAndResult.result) {
    case ImportOperationResult.CriticalError:
      messageType = 'error'
      message = {
        title: 'Critical Error',
        description: 'The import process encountered a critical error and was aborted.',
      }
      break
    case ImportOperationResult.Error:
      messageType = 'error'
      message = {
        title: 'Error',
        description:
          totalImportStateAndResult.importStatus.status == 'done'
            ? 'The import process was completed with errors.'
            : 'The import process encountered an error and was aborted by the user.',
      }
      break
    case ImportOperationResult.Warn:
      messageType = 'warning'
      message = {
        title: 'Warning',
        description: 'The import process was completed with warnings.',
      }
      break
    case ImportOperationResult.Success:
      // no message
      return
    default:
      assertNever(totalImportStateAndResult.result)
  }

  void sendDiscordMessage('SITE_IMPORT', messageType, {
    ...message,
    fields: fields,
  })
}
