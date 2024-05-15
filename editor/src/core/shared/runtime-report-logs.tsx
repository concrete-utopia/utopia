import React from 'react'
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

let reactRouterErrorLogged: boolean = false

export function hasReactRouterErrorBeenLogged(): boolean {
  return reactRouterErrorLogged
}

export function setReactRouterErrorHasBeenLogged(value: boolean): void {
  reactRouterErrorLogged = value
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

const ReactRouterErrorPrefix = `React Router caught the following error during render`
const ReactRouterAwaitErrorPrefix = `<Await> caught the following error during render`

export function listenForReactRouterErrors(targetConsole: Console): void {
  const targetConsoleAny = targetConsole as any

  // Remove any existing proxy first.
  if (targetConsoleAny.originalErrorMethod != null) {
    // Restore the original method.
    targetConsoleAny.error = targetConsoleAny.originalErrorMethod
    // Remove this field, thereby restoring this console to its original state.
    delete targetConsoleAny['originalErrorMethod']
  }

  // Squirrel away the original method for unpacking later.
  const originalMethod = targetConsoleAny.error
  targetConsoleAny.originalErrorMethod = originalMethod
  targetConsoleAny.error = function (...args: Array<any>) {
    // Call the original method first.
    originalMethod(...args)

    // If the first part of the log line is a string
    const firstLine = args[0]
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
}
