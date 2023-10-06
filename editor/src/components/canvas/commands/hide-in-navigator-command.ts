import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction } from './commands'

export interface HideInNavigatorCommand extends BaseCommand {
  type: 'HIDE_IN_NAVIGATOR_COMMAND'
  elements: Array<ElementPath>
}

export function hideInNavigatorCommand(elements: Array<ElementPath>): HideInNavigatorCommand {
  return {
    type: 'HIDE_IN_NAVIGATOR_COMMAND',
    whenToRun: 'mid-interaction',
    elements: elements,
  }
}

export const runHideInNavigatorCommand: CommandFunction<HideInNavigatorCommand> = (
  _editor: EditorState,
  command: HideInNavigatorCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    navigator: {
      hiddenInNavigator: { $set: command.elements },
    },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Hide in navigator ${command.elements.map(EP.toString)}`,
  }
}
