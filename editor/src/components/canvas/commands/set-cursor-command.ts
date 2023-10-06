import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { CSSCursor } from '../canvas-types'
import type { BaseCommand, CommandFunction } from './commands'

export interface SetCursorCommand extends BaseCommand {
  type: 'SET_CURSOR_COMMAND'
  value: CSSCursor | null
}

export function setCursorCommand(value: CSSCursor | null): SetCursorCommand {
  return {
    type: 'SET_CURSOR_COMMAND',
    whenToRun: 'mid-interaction',
    value: value,
  }
}

export const runSetCursor: CommandFunction<SetCursorCommand> = (
  _: EditorState,
  command: SetCursorCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      cursor: { $set: command.value },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Update Canvas Cursor: ${command.value}`,
  }
}
