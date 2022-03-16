import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, TransientOrNot } from './commands'

export interface UpdateHighlightedViews extends BaseCommand {
  type: 'UPDATE_HIGHLIGHTED_VIEWS'
  value: Array<ElementPath>
}

export function updateHighlightedViews(
  transient: TransientOrNot,
  value: Array<ElementPath>,
): UpdateHighlightedViews {
  return {
    type: 'UPDATE_HIGHLIGHTED_VIEWS',
    transient: transient,
    value: value,
  }
}

export const runUpdateHighlightedViews: CommandFunction<UpdateHighlightedViews> = (
  _: EditorState,
  command: UpdateHighlightedViews,
) => {
  const editorStatePatch: EditorStatePatch = {
    highlightedViews: {
      $set: command.value,
    },
  }
  return {
    editorStatePatch: [],
    commandDescription: `Update Highlighted Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}
