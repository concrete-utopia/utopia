import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, CommandState, WhenToRun } from './commands'

export interface UpdateSelectedViews extends BaseCommand {
  type: 'UPDATE_SELECTED_VIEWS'
  value: Array<ElementPath>
}

export function updateSelectedViews(
  whenToRun: WhenToRun,
  value: Array<ElementPath>,
): UpdateSelectedViews {
  return {
    type: 'UPDATE_SELECTED_VIEWS',
    whenToRun: whenToRun,
    value: value,
  }
}

export const runUpdateSelectedViews: CommandFunction<UpdateSelectedViews> = (
  _: EditorState,
  command: UpdateSelectedViews,
  commandState: CommandState,
) => {
  const editorStatePatch: EditorStatePatch = {
    selectedViews: {
      $set: command.value,
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandState: commandState,
    commandDescription: `Update Selected Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}
