import { CanvasRectangle } from '../../../core/shared/math-utils'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface ShowOutlineHighlight extends BaseCommand {
  type: 'SHOW_OUTLINE_HIGHLIGHT'
  value: Array<CanvasRectangle>
}

export function showOutlineHighlight(
  transient: TransientOrNot,
  value: Array<CanvasRectangle>,
): ShowOutlineHighlight {
  return {
    type: 'SHOW_OUTLINE_HIGHLIGHT',
    transient: transient,
    value: value,
  }
}

export const runShowOutlineHighlight: CommandFunction<ShowOutlineHighlight> = (
  _: EditorState,
  command: ShowOutlineHighlight,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: {
      controls: {
        outlineHighlights: { $set: command.value },
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Show Outline Highlight`,
  }
}
