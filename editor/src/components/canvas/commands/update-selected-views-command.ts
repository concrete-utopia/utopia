import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, PathMappings, TransientOrNot } from './commands'

export interface UpdateSelectedViews extends BaseCommand {
  type: 'UPDATE_SELECTED_VIEWS'
  value: Array<ElementPath>
}

export function updateSelectedViews(
  transient: TransientOrNot,
  value: Array<ElementPath>,
): UpdateSelectedViews {
  return {
    type: 'UPDATE_SELECTED_VIEWS',
    transient: transient,
    value: value,
  }
}

export const runUpdateSelectedViews: CommandFunction<UpdateSelectedViews> = (
  _: EditorState,
  pathMappings: PathMappings,
  command: UpdateSelectedViews,
) => {
  const editorStatePatch = {
    selectedViews: {
      $set: command.value,
    },
  }
  return {
    editorStatePatch: editorStatePatch,
    pathMappings: pathMappings,
    commandDescription: `Update Selected Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}
