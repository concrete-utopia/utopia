import * as React from 'react'
import * as PubSub from 'pubsub-js'
import type { ConsoleLog } from '../../components/editor/store/editor-state'
import type { FancyError, RuntimeErrorInfo } from './code-exec-utils'
import { defaultIfNull } from './optional-utils'

const EmptyArray: Array<RuntimeErrorInfo> = []

const ConsoleLogSizeLimit = 100
const EmptyConsoleLogs: Array<ConsoleLog> = []

const runtimeErrorsAtom: React.MutableRefObject<Array<RuntimeErrorInfo>> = { current: [] }
const RuntimeErrorsPubSub = 'runtimeErrorsPubSub'

function usePubSub<T>(topic: string, referentiallyStableCallback: (newData: T) => void): void {
  const pubsubCallback = React.useCallback(
    (
      message: string,
      data: any, // TODO once eslint for hooks is updated, replace data: any with data: T
    ) => {
      referentiallyStableCallback(data)
    },
    [referentiallyStableCallback],
  )

  React.useEffect(() => {
    const token = PubSub.subscribe(topic, pubsubCallback)
    return function cleanup() {
      PubSub.unsubscribe(token)
    }
  }, [topic, pubsubCallback])
}

export function useUpdateOnRuntimeErrors(
  referentiallyStableCallback: (newRuntimeErrors: Array<RuntimeErrorInfo>) => void,
): void {
  usePubSub(RuntimeErrorsPubSub, referentiallyStableCallback)
}

export function useReadOnlyRuntimeErrors(): Array<RuntimeErrorInfo> {
  const [, forceUpdate] = React.useReducer((c) => c + 1, 0)
  useUpdateOnRuntimeErrors(
    React.useCallback((newRuntimeErrors) => {
      runtimeErrorsAtom.current = newRuntimeErrors
      forceUpdate()
    }, []),
  )
  return runtimeErrorsAtom.current
}

export function useWriteOnlyRuntimeErrors(): {
  onRuntimeError: (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => void
  clearRuntimeErrors: () => void
} {
  const onRuntimeError = React.useCallback(
    (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => {
      runtimeErrorsAtom.current = [
        {
          editedFile: editedFile,
          error: error,
          errorInfo: defaultIfNull(null, errorInfo),
        },
      ]
      PubSub.publish(RuntimeErrorsPubSub, runtimeErrorsAtom.current)
    },
    [],
  )

  const clearRuntimeErrors = React.useCallback(() => {
    runtimeErrorsAtom.current = EmptyArray
    PubSub.publish(RuntimeErrorsPubSub, runtimeErrorsAtom.current)
  }, [])

  return {
    onRuntimeError: onRuntimeError,
    clearRuntimeErrors: clearRuntimeErrors,
  }
}

const consoleLogsAtom: React.MutableRefObject<Array<ConsoleLog>> = { current: [] }
const ConsoleLogsPubSub = 'consoleLogsPubSub'

export function useUpdateOnConsoleLogs(
  referentiallyStableCallback: (newConsoleLogs: Array<ConsoleLog>) => void,
): void {
  usePubSub(ConsoleLogsPubSub, referentiallyStableCallback)
}

export function useReadOnlyConsoleLogs(): Array<ConsoleLog> {
  const [, forceUpdate] = React.useReducer((c) => c + 1, 0)
  useUpdateOnConsoleLogs(
    React.useCallback((newRuntimeErrors) => {
      consoleLogsAtom.current = newRuntimeErrors
      forceUpdate()
    }, []),
  )
  return consoleLogsAtom.current
}

export function useWriteOnlyConsoleLogs(): {
  addToConsoleLogs: (log: ConsoleLog) => void
  clearConsoleLogs: () => void
} {
  const modifyLogs = React.useCallback(
    (updateLogs: (logs: Array<ConsoleLog>) => Array<ConsoleLog>) => {
      consoleLogsAtom.current = updateLogs(consoleLogsAtom.current)
      PubSub.publish(ConsoleLogsPubSub, consoleLogsAtom.current)
    },
    [],
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
