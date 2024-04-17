import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { type EditorState, type EditorStatePatch } from '../../editor/store/editor-state'
import { nextSelectedTab } from '../../navigator/left-pane/left-pane-utils'
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
        $set: nextSelectedTab(editorState.leftMenu.selectedTab, command.value),
      },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Update Selected Views: ${command.value.map(EP.toString).join(', ')}`,
  }
}
