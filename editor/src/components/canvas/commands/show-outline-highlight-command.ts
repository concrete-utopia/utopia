import type { CanvasRectangle } from '../../../core/shared/math-utils'
import type { DerivedState, EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface ShowOutlineHighlight extends BaseCommand {
  type: 'SHOW_OUTLINE_HIGHLIGHT'
  value: Array<CanvasRectangle>
}

export function showOutlineHighlight(
  whenToRun: WhenToRun,
  value: Array<CanvasRectangle>,
): ShowOutlineHighlight {
  return {
    type: 'SHOW_OUTLINE_HIGHLIGHT',
    whenToRun: whenToRun,
    value: value,
  }
}

export const runShowOutlineHighlight: CommandFunction<ShowOutlineHighlight> = (
  _: EditorState,
  __: DerivedState,
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
