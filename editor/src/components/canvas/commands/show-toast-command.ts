import { notice, Notice, NoticeLevel } from '../../common/notice'
import { EditorState } from '../../editor/store/editor-state'
import { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import { CommandFunctionResult, WhenToRun } from './commands'

export interface ShowToastCommand {
  type: 'SHOW_TOAST_COMMAND'
  whenToRun: WhenToRun
  notice: Notice
}

export function showToastCommand(
  message: string,
  level: NoticeLevel,
  id: string,
): ShowToastCommand {
  return {
    type: 'SHOW_TOAST_COMMAND',
    whenToRun: 'on-complete',
    notice: notice(message, level, false, id),
  }
}

export function runShowToastCommand(
  editorState: EditorState,
  command: ShowToastCommand,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult {
  if (commandLifecycle !== 'end-interaction') {
    return {
      commandDescription: 'Show a toast',
      editorStatePatches: [],
    }
  }

  const toasts = [...editorState.toasts.filter((t) => t.id !== command.notice.id), command.notice]
  return {
    commandDescription: 'Show a toast',
    editorStatePatches: [{ toasts: { $set: toasts } }],
  }
}
