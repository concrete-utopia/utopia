import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

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
) => {
  const editorStatePatch = {
    highlightedViews: {
      $set: command.value,
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Update Highlighted Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}
