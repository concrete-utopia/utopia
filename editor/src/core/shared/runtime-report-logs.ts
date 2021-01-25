import * as React from 'react'
import { atom, useRecoilValue, useSetRecoilState } from 'recoil'
import type { ConsoleLog } from '../../components/editor/store/editor-state'
import Utils from '../../utils/utils'
import type { FancyError, RuntimeErrorInfo } from './code-exec-utils'

const EmptyArray: Array<RuntimeErrorInfo> = []

const ConsoleLogSizeLimit = 100
const EmptyConsoleLogs: Array<ConsoleLog> = []

const runtimeErrorsAtom = atom<Array<RuntimeErrorInfo>>({ key: 'runtimeErrorsAtom', default: [] })

export function useReadOnlyRuntimeErrors(): Array<RuntimeErrorInfo> {
  return useRecoilValue(runtimeErrorsAtom)
}

export function useWriteOnlyRuntimeErrors(): {
  onRuntimeError: (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => void
  clearRuntimeErrors: () => void
} {
  const setRuntimeErrors = useSetRecoilState(runtimeErrorsAtom)

  const onRuntimeError = React.useCallback(
    (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => {
      setRuntimeErrors([
        {
          editedFile: editedFile,
          error: error,
          errorInfo: Utils.defaultIfNull(null, errorInfo),
        },
      ])
    },
    [setRuntimeErrors],
  )

  const clearRuntimeErrors = React.useCallback(() => {
    setRuntimeErrors(EmptyArray)
  }, [setRuntimeErrors])

  return {
    onRuntimeError: onRuntimeError,
    clearRuntimeErrors: clearRuntimeErrors,
  }
}

const consoleLogsAtom = atom<Array<ConsoleLog>>({ key: 'consoleLogsAtom', default: [] })

export function useReadOnlyConsoleLogs(): Array<ConsoleLog> {
  return useRecoilValue(consoleLogsAtom)
}

export function useWriteOnlyConsoleLogs(): {
  addToConsoleLogs: (log: ConsoleLog) => void
  clearConsoleLogs: () => void
} {
  const setConsoleLogs = useSetRecoilState(consoleLogsAtom)

  const modifyLogs = React.useCallback(
    (updateLogs: (logs: Array<ConsoleLog>) => Array<ConsoleLog>) => {
      setConsoleLogs(updateLogs)
    },
    [setConsoleLogs],
  )

  const clearConsoleLogs = React.useCallback(() => {
    modifyLogs((_) => EmptyConsoleLogs)
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
