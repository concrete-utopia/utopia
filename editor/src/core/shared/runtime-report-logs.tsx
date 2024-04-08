import React from 'react'
import type { ConsoleLog } from '../../components/editor/store/editor-state'
import {
  getAllCodeEditorErrors,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  parseFailureAsErrorMessages,
} from '../../components/editor/store/editor-state'
import {
  AlwaysTrue,
  atomWithPubSub,
  usePubSubAtomReadOnly,
  usePubSubAtomWriteOnly,
  useSubscribeToPubSubAtom,
} from './atom-with-pub-sub'
import type { FancyError, RuntimeErrorInfo } from './code-exec-utils'
import { defaultIfNull } from './optional-utils'
import { reduxDevtoolsLogError } from './redux-devtools'
import StackFrame from '../../third-party/react-error-overlay/utils/stack-frame'
import { filterOldPasses } from '../../components/canvas/canvas-wrapper-component'
import { useEditorState, Substores } from '../../components/editor/store/store-hook'
import type { ErrorMessage } from './error-messages'

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
  return usePubSubAtomReadOnly(runtimeErrorsAtom, AlwaysTrue)
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

let reactRouterErrorLogged: boolean = false

export function hasReactRouterErrorBeenLogged(): boolean {
  return reactRouterErrorLogged
}

export function setReactRouterErrorHasBeenLogged(value: boolean): void {
  reactRouterErrorLogged = value
}

const ReactRouterErrorPrefix = `React Router caught the following error during render`
const ReactRouterAwaitErrorPrefix = `<Await> caught the following error during render`

export function useUpdateOnConsoleLogs(
  referentiallyStableCallback: (newConsoleLogs: Array<ConsoleLog>) => void,
): void {
  useSubscribeToPubSubAtom(consoleLogsAtom, referentiallyStableCallback)
}

export function useReadOnlyConsoleLogs(): Array<ConsoleLog> {
  return usePubSubAtomReadOnly(consoleLogsAtom, AlwaysTrue)
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
      // For an error...
      if (log.method === 'error') {
        // ...where the first value...
        const firstLine = log.data[0]
        // ...is a string...
        if (typeof firstLine === 'string') {
          // ...and it starts with these prefixes...
          if (
            firstLine.startsWith(ReactRouterErrorPrefix) ||
            firstLine.startsWith(ReactRouterAwaitErrorPrefix)
          ) {
            // ...Mark these as having been seen.
            setReactRouterErrorHasBeenLogged(true)
          }
        }
      }
    },
    [modifyLogs],
  )

  return {
    addToConsoleLogs: addToConsoleLogs,
    clearConsoleLogs: clearConsoleLogs,
  }
}

export function getConsoleLogsForTests(): Array<ConsoleLog> {
  return consoleLogsAtom.currentValue
}

export interface OverlayError {
  error: FancyError
  unhandledRejection: boolean
  contextSize: number
  stackFrames: StackFrame[]
}

export interface ErrorOverlayRecords {
  errorRecords: ErrorMessage[]
  overlayErrors: OverlayError[]
}

export function useErrorOverlayRecords(): ErrorOverlayRecords {
  const utopiaParserErrors = useEditorState(
    Substores.fullStore,
    (store) => {
      return parseFailureAsErrorMessages(
        getOpenUIJSFileKey(store.editor),
        getOpenUIJSFile(store.editor),
      )
    },
    'ErrorOverlayComponent utopiaParserErrors',
  )
  const fatalCodeEditorErrors = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return getAllCodeEditorErrors(store.editor.codeEditorErrors, 'error', true)
    },
    'ErrorOverlayComponent fatalCodeEditorErrors',
  )

  const runtimeErrors = useReadOnlyRuntimeErrors()

  const overlayErrors: OverlayError[] = React.useMemo(() => {
    return runtimeErrors.map((runtimeError) => {
      const stackFrames =
        runtimeError.error.stackFrames != null
          ? runtimeError.error.stackFrames
          : [
              new StackFrame(
                'WARNING: This error has no Stack Frames, it might be coming from Utopia itself!',
              ),
            ]
      return {
        error: runtimeError.error,
        unhandledRejection: false,
        contextSize: 3, // magic number from react-error-overlay
        stackFrames: stackFrames,
      }
    })
  }, [runtimeErrors])

  const lintErrors = fatalCodeEditorErrors.filter((e) => e.source === 'eslint')
  const componentDescriptorErrors = fatalCodeEditorErrors.filter(
    (e) => e.source === 'component-descriptor',
  )
  // we start with the lint errors, since those show up the fastest. any subsequent error will go below in the error screen
  const errorRecords = filterOldPasses([
    ...lintErrors,
    ...utopiaParserErrors,
    ...componentDescriptorErrors,
  ])

  return { errorRecords, overlayErrors }
}
