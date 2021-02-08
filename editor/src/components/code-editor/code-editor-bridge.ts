import * as React from 'react'
import * as fastDeepEquals from 'fast-deep-equal'
import { fastForEach } from '../../core/shared/utils'
import { SetFocus } from '../common/actions'

// Please try to have as few real imports here as possible, because this code runs in an iFrame

import type {
  OpenEditorTab,
  AddToast,
  SaveCurrentFile,
  SelectComponents,
  SendLinterRequestMessage,
  SetCodeEditorBuildErrors,
  SetCodeEditorVisibility,
  SetHighlightedView,
  UpdateFile,
} from '../editor/action-types'
import { useEditorState } from '../editor/store/store-hook'
import type { JSONStringifiedCodeEditorProps } from './code-editor-iframe-entry-point'
import type { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import type { ConsoleLog } from '../editor/store/editor-state'
import { useForceUpdate } from '../editor/hook-utils'

export type CodeEditorAction =
  | SelectComponents
  | SetHighlightedView
  | OpenEditorTab
  | UpdateFile
  | AddToast
  | SaveCurrentFile
  | SetCodeEditorBuildErrors
  | SetCodeEditorVisibility
  | SetFocus
  | SendLinterRequestMessage

interface SendCodeEditorActionMessage {
  type: 'SEND_CODE_EDITOR_ACTION'
  actions: Array<CodeEditorAction>
}

function sendCodeEditorActionMessage(
  actions: Array<CodeEditorAction>,
): SendCodeEditorActionMessage {
  return {
    type: 'SEND_CODE_EDITOR_ACTION',
    actions: actions,
  }
}

function isSendCodeEditorActionMessage(message: unknown): message is SendCodeEditorActionMessage {
  return (
    message != null &&
    (message as SendCodeEditorActionMessage)?.type === 'SEND_CODE_EDITOR_ACTION' &&
    Array.isArray((message as SendCodeEditorActionMessage).actions)
  )
}

function validateActions(actions: Array<CodeEditorAction>): void {
  fastForEach(actions, (action) => {
    switch (action.action) {
      case 'SELECT_COMPONENTS':
      case 'SET_HIGHLIGHTED_VIEW':
      case 'OPEN_FILE':
      case 'UPDATE_FILE':
      case 'SAVE_CURRENT_FILE':
      case 'ADD_TOAST':
      case 'SET_CODE_EDITOR_BUILD_ERRORS':
      case 'SET_CODE_EDITOR_VISIBILITY':
      case 'SET_FOCUS':
      case 'SEND_LINTER_REQUEST_MESSAGE':
        return
      default:
        const _exhaustiveCheck: never = action
        throw new Error(`Unhandled Code Editor Action ${JSON.stringify((action as any).action)}`)
    }
  })
}

interface RequestFullUpdateMessage {
  type: 'REQUEST_FULL_UPDATE'
}

const requestFullUpdateMessage = { type: 'REQUEST_FULL_UPDATE' }

function isRequestFullUpdateMessage(message: unknown): message is RequestFullUpdateMessage {
  return message != null && (message as RequestFullUpdateMessage)?.type === 'REQUEST_FULL_UPDATE'
}

function useListenToCodeEditorMessages(): void {
  const dispatch = useEditorState((store) => store.dispatch, 'useBridgeTowardsIframe dispatch')

  React.useEffect(() => {
    const listener = (messageEvent: MessageEvent) => {
      if (isSendCodeEditorActionMessage(messageEvent.data)) {
        const message = messageEvent.data
        const actions = message.actions
        validateActions(actions)
        dispatch(actions)
      }
    }
    window.addEventListener('message', listener)

    return function cleanup() {
      window.removeEventListener('message', listener)
    }
  }, [dispatch])
}

function useSendFullPropsAtMount(
  props: JSONStringifiedCodeEditorProps,
  ref: React.RefObject<HTMLIFrameElement>,
) {
  const propsRef = React.useRef<JSONStringifiedCodeEditorProps>(props)
  propsRef.current = props
  React.useEffect(
    () => {
      const listener = (event: MessageEvent) => {
        if (isRequestFullUpdateMessage(event.data)) {
          // eslint-disable-next-line no-unused-expressions
          ref.current?.contentWindow?.postMessage(sendMonacoFullPropsMessage(props), '*')
        }
      }
      window.addEventListener('message', listener)
      return function cleanup() {
        window.removeEventListener('message', listener)
      }
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [ref],
  )
}

function useSendPartialPropsWhenChanged(
  props: JSONStringifiedCodeEditorProps,
  ref: React.RefObject<HTMLIFrameElement>,
) {
  const lastPropsValueRef = React.useRef<JSONStringifiedCodeEditorProps>(props)

  React.useEffect(
    () => {
      let propsToUpdate: Partial<JSONStringifiedCodeEditorProps> = {}
      ;(Object.keys(props) as Array<keyof JSONStringifiedCodeEditorProps>).forEach((key) => {
        const newValue = props[key]
        const oldValue = lastPropsValueRef.current[key]
        if (oldValue !== newValue) {
          // insert the key - value into the props object we want to serialize and send down to the monaco iframe
          ;(propsToUpdate as any)[key] = newValue // sorry for the any!
        }
      })
      if (Object.keys(propsToUpdate).length > 0) {
        // we have props that are updated, let's send those (and only those) to monaco iframe
        // eslint-disable-next-line no-unused-expressions
        ref.current?.contentWindow?.postMessage(sendMonacoPartialPropsMessage(propsToUpdate), '*')
      }
      lastPropsValueRef.current = props
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [ref, ...Object.values(props)],
  )
}

function useSendPropUpdates(
  props: JSONStringifiedCodeEditorProps,
  ref: React.RefObject<HTMLIFrameElement>,
): void {
  useSendFullPropsAtMount(props, ref)
  useSendPartialPropsWhenChanged(props, ref)
}

export function useBridgeTowardsIframe(
  props: JSONStringifiedCodeEditorProps,
  ref: React.RefObject<HTMLIFrameElement>,
): {
  sendRuntimeErrors: (value: Array<RuntimeErrorInfo>) => void
  sendCanvasConsoleLogs: (value: Array<ConsoleLog>) => void
} {
  useListenToCodeEditorMessages()
  useSendPropUpdates(props, ref)

  const sendRuntimeErrors = React.useCallback(
    (value: Array<RuntimeErrorInfo>) => {
      // eslint-disable-next-line no-unused-expressions
      ref.current?.contentWindow?.postMessage(sendRuntimeErrorsMessage(value), '*')
    },
    [ref],
  )

  const sendCanvasConsoleLogs = React.useCallback(
    (value: Array<ConsoleLog>) => {
      // eslint-disable-next-line no-unused-expressions
      ref.current?.contentWindow?.postMessage(sendCanvasConsoleLogsMessage(value), '*')
    },
    [ref],
  )

  return { sendRuntimeErrors: sendRuntimeErrors, sendCanvasConsoleLogs: sendCanvasConsoleLogs }
}

interface SendMonacoFullPropsMessage {
  type: 'SEND_MONACO_FULL_PROPS'
  props: JSONStringifiedCodeEditorProps
}

export function sendMonacoFullPropsMessage(
  props: JSONStringifiedCodeEditorProps,
): SendMonacoFullPropsMessage {
  return {
    type: 'SEND_MONACO_FULL_PROPS',
    props: props,
  }
}

function isSendMonacoFullPropsMessage(message: unknown): message is SendMonacoFullPropsMessage {
  return (
    message != null &&
    (message as SendMonacoFullPropsMessage)?.type === 'SEND_MONACO_FULL_PROPS' &&
    typeof (message as SendMonacoFullPropsMessage)?.props === 'object'
  )
}

interface SendMonacoPartialPropsMessage {
  type: 'SEND_MONACO_PARTIAL_PROPS'
  partialProps: Partial<JSONStringifiedCodeEditorProps>
}

export function sendMonacoPartialPropsMessage(
  props: Partial<JSONStringifiedCodeEditorProps>,
): SendMonacoPartialPropsMessage {
  return {
    type: 'SEND_MONACO_PARTIAL_PROPS',
    partialProps: props,
  }
}

function isSendMonacoPartialPropsMessage(
  message: unknown,
): message is SendMonacoPartialPropsMessage {
  return (
    message != null &&
    (message as SendMonacoPartialPropsMessage)?.type === 'SEND_MONACO_PARTIAL_PROPS' &&
    typeof (message as SendMonacoPartialPropsMessage)?.partialProps === 'object'
  )
}

interface SendRuntimeErrorsMessage {
  type: 'SEND_RUNTIME_ERRORS'
  runtimeErrors: Array<RuntimeErrorInfo>
}

function sendRuntimeErrorsMessage(
  runtimeErrors: Array<RuntimeErrorInfo>,
): SendRuntimeErrorsMessage {
  return {
    type: 'SEND_RUNTIME_ERRORS',
    runtimeErrors: runtimeErrors,
  }
}

function isSendRuntimeErrorsMessage(message: unknown): message is SendRuntimeErrorsMessage {
  return (
    message != null &&
    (message as SendRuntimeErrorsMessage)?.type === 'SEND_RUNTIME_ERRORS' &&
    Array.isArray((message as SendRuntimeErrorsMessage)?.runtimeErrors)
  )
}

interface SendCanvasConsoleLogsMessage {
  type: 'SEND_CANVAS_CONSOLE_LOGS'
  canvasConsoleLogs: Array<ConsoleLog>
}

function sendCanvasConsoleLogsMessage(
  canvasConsoleLogs: Array<ConsoleLog>,
): SendCanvasConsoleLogsMessage {
  return {
    type: 'SEND_CANVAS_CONSOLE_LOGS',
    canvasConsoleLogs: canvasConsoleLogs,
  }
}

function isSendCanvasConsoleLogsMessage(message: unknown): message is SendCanvasConsoleLogsMessage {
  return (
    message != null &&
    (message as SendCanvasConsoleLogsMessage)?.type === 'SEND_CANVAS_CONSOLE_LOGS' &&
    Array.isArray((message as SendCanvasConsoleLogsMessage)?.canvasConsoleLogs)
  )
}

function useAskForFullUpdateOnMount() {
  return React.useEffect(() => {
    window.parent.postMessage(requestFullUpdateMessage, '*')
  }, [])
}

function usePropsFromMainEditor(): {
  propsFromMainEditor: JSONStringifiedCodeEditorProps | null
  runtimeErrors: Array<RuntimeErrorInfo>
  canvasConsoleLogs: Array<ConsoleLog>
} {
  const forceUpdate = useForceUpdate()

  const propsRef = React.useRef<JSONStringifiedCodeEditorProps | null>(null)
  const runtimeErrorsRef = React.useRef<Array<RuntimeErrorInfo>>([])
  const canvasConsoleLogsRef = React.useRef<Array<ConsoleLog>>([])

  React.useEffect(() => {
    const handleMessage = (event: MessageEvent) => {
      if (isSendMonacoFullPropsMessage(event.data)) {
        propsRef.current = event.data.props
        forceUpdate()
      } else if (isSendMonacoPartialPropsMessage(event.data) && propsRef.current != null) {
        propsRef.current = { ...propsRef.current, ...event.data.partialProps }
        forceUpdate()
      } else if (isSendRuntimeErrorsMessage(event.data)) {
        runtimeErrorsRef.current = event.data.runtimeErrors
        forceUpdate()
      } else if (isSendCanvasConsoleLogsMessage(event.data)) {
        canvasConsoleLogsRef.current = event.data.canvasConsoleLogs
        forceUpdate()
      }
    }
    window.addEventListener('message', handleMessage)
    return function cleanup() {
      window.removeEventListener('message', handleMessage)
    }
  }, [forceUpdate])

  return {
    propsFromMainEditor: propsRef.current,
    runtimeErrors: runtimeErrorsRef.current,
    canvasConsoleLogs: canvasConsoleLogsRef.current,
  }
}

export function useBridgeFromMainEditor(): {
  propsFromMainEditor: JSONStringifiedCodeEditorProps | null
  runtimeErrors: Array<RuntimeErrorInfo>
  canvasConsoleLogs: Array<ConsoleLog>
} {
  useAskForFullUpdateOnMount()
  return usePropsFromMainEditor()
}

export const BridgeTowardsMainEditor = {
  sendCodeEditorAction: (actions: Array<CodeEditorAction>): void => {
    window.parent.postMessage(sendCodeEditorActionMessage(actions), '*')
  },
}
