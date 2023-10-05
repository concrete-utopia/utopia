import type { Notice, NoticeLevel } from '../../common/notice'
import { notice } from '../../common/notice'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import type { CommandFunctionResult, WhenToRun } from './commands'

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

export function addToastPatch(
  currentToasts: ReadonlyArray<Notice>,
  noticeToAdd: Notice,
): EditorStatePatch {
  const updatedToasts = [...currentToasts.filter((t) => t.id !== noticeToAdd.id), noticeToAdd]
  return { toasts: { $set: updatedToasts } }
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

  return {
    commandDescription: 'Show a toast',
    editorStatePatches: [addToastPatch(editorState.toasts, command.notice)],
  }
}
