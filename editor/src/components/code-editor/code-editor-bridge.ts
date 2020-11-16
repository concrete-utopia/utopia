import * as React from 'react'
import { fastForEach } from '../../core/shared/utils'
import { SetFocus } from '../common/actions'
import type {
  OpenEditorTab,
  PushToast,
  SaveCurrentFile,
  SaveCursorPosition,
  SelectComponents,
  SendLinterRequestMessage,
  SetCodeEditorBuildErrors,
  SetCodeEditorVisibility,
  SetHighlightedView,
  UpdateFile,
} from '../editor/action-types'
import { useEditorState } from '../editor/store/store-hook'

export type CodeEditorAction =
  | SelectComponents
  | SetHighlightedView
  | OpenEditorTab
  | UpdateFile
  | PushToast
  | SaveCurrentFile
  | SetCodeEditorBuildErrors
  | SetCodeEditorVisibility
  | SetFocus
  | SaveCursorPosition
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
      case 'PUSH_TOAST':
      case 'SET_CODE_EDITOR_BUILD_ERRORS':
      case 'SET_CODE_EDITOR_VISIBILITY':
      case 'SET_FOCUS':
      case 'SAVE_CURSOR_POSITION':
      case 'SEND_LINTER_REQUEST_MESSAGE':
        return
      default:
        const _exhaustiveCheck: never = action
        throw new Error(`Unhandled Code Editor Action ${JSON.stringify((action as any).action)}`)
    }
  })
}

export function useBridgeTowardsIframe(): void {
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

export const BridgeTowardsMainEditor = {
  sendCodeEditorAction: (actions: Array<CodeEditorAction>): void => {
    window.postMessage(sendCodeEditorActionMessage(actions), '*')
  },
}
