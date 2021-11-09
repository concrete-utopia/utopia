import React from 'react'
import type { ConsoleLog } from '../../components/editor/store/editor-state'
import {
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
  useSubscribeToPubSubAtom,
} from './atom-with-pub-sub'
import type { FancyError, RuntimeErrorInfo } from './code-exec-utils'
import { defaultIfNull } from './optional-utils'
import { reduxDevtoolsLogError } from './redux-devtools'

const EmptyArray: Array<RuntimeErrorInfo> = []

const ConsoleLogSizeLimit = 100
const EmptyConsoleLogs: Array<ConsoleLog> = []

const runtimeErrorsAtom = atomWithPubSub<Array<RuntimeErrorInfo>>({
  key: 'runtimeErrors',
  defaultValue: [],
})

export function useUpdateOnRuntimeErrors(
  referentiallyStableCallback: (newRuntimeErrors: Array<RuntimeErrorInfo>) => void,
): void {
  useSubscribeToPubSubAtom(runtimeErrorsAtom, referentiallyStableCallback)
}

export function useReadOnlyRuntimeErrors(): Array<RuntimeErrorInfo> {
  return usePubSubAtomReadOnly(runtimeErrorsAtom)
}

export function useWriteOnlyRuntimeErrors(): {
  addToRuntimeErrors: (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => void
  clearRuntimeErrors: () => void
} {
  const updateRuntimeErrors = usePubSubAtomWriteOnly(runtimeErrorsAtom)

  const onRuntimeError = React.useCallback(
    (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => {
      reduxDevtoolsLogError('Canvas Runtime Errors Reported', {
        editedFile,
        error,
        errorInfo,
        errorToString: error.toString(),
      })
      updateRuntimeErrors([
        {
          editedFile: editedFile,
          error: error,
          errorInfo: defaultIfNull(null, errorInfo),
        },
      ])
    },
    [updateRuntimeErrors],
  )

  const clearRuntimeErrors = React.useCallback(() => {
    updateRuntimeErrors((current) => {
      if (current.length !== 0) {
        reduxDevtoolsLogError('Canvas Runtime Errors Cleared by re-render', {
          deletedErrors: current,
        })
      }
      return EmptyArray
    })
  }, [updateRuntimeErrors])

  return {
    addToRuntimeErrors: onRuntimeError,
    clearRuntimeErrors: clearRuntimeErrors,
  }
}

const consoleLogsAtom = atomWithPubSub<Array<ConsoleLog>>({
  key: 'canvasConsoleLogs',
  defaultValue: [],
})

export function useUpdateOnConsoleLogs(
  referentiallyStableCallback: (newConsoleLogs: Array<ConsoleLog>) => void,
): void {
  useSubscribeToPubSubAtom(consoleLogsAtom, referentiallyStableCallback)
}

export function useReadOnlyConsoleLogs(): Array<ConsoleLog> {
  return usePubSubAtomReadOnly(consoleLogsAtom)
}

export function useWriteOnlyConsoleLogs(): {
  addToConsoleLogs: (log: ConsoleLog) => void
  clearConsoleLogs: () => void
} {
  const modifyLogs = usePubSubAtomWriteOnly(consoleLogsAtom)

  const clearConsoleLogs = React.useCallback(() => {
    modifyLogs(EmptyConsoleLogs)
  }, [modifyLogs])

  const addToConsoleLogs = React.useCallback(
    (log: ConsoleLog) => {
      modifyLogs((logs) => {
        let result = [...logs, log]
        while (result.length > ConsoleLogSizeLimit) {
          result.shift()
        }
        return result
      })
    },
    [modifyLogs],
  )

  return {
    addToConsoleLogs: addToConsoleLogs,
    clearConsoleLogs: clearConsoleLogs,
  }
}
