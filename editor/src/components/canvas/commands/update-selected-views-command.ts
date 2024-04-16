import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { nextSelectedTab } from '../../editor/editor-modes'
import { type EditorState, type EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'

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
  editorState: EditorState,
  command: UpdateSelectedViews,
) => {
  const editorStatePatch: EditorStatePatch = {
    selectedViews: {
      $set: command.value,
    },
    leftMenu: {
      selectedTab: {
        $set: nextSelectedTab(editorState),
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Update Selected Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}
