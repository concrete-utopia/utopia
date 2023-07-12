import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, CommandState, WhenToRun } from './commands'

export interface UpdateHighlightedViews extends BaseCommand {
  type: 'UPDATE_HIGHLIGHTED_VIEWS'
  value: Array<ElementPath>
}

export function updateHighlightedViews(
  whenToRun: WhenToRun,
  value: Array<ElementPath>,
): UpdateHighlightedViews {
  return {
    type: 'UPDATE_HIGHLIGHTED_VIEWS',
    whenToRun: whenToRun,
    value: value,
  }
}

export const runUpdateHighlightedViews: CommandFunction<UpdateHighlightedViews> = (
  _: EditorState,
  command: UpdateHighlightedViews,
  commandState: CommandState,
) => {
  const editorStatePatch = {
    highlightedViews: {
      $set: command.value,
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandState: commandState,
    commandDescription: `Update Highlighted Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}
