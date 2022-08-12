import * as EP from '../../../core/shared/element-path'
import { EditorState, EditorStatePatch, ElementsToRerender } from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, WhenToRun } from './commands'

export interface SetElementsToRerenderCommand extends BaseCommand {
  type: 'SET_ELEMENTS_TO_RERENDER_COMMAND'
  value: ElementsToRerender
}

export function setElementsToRerenderCommand(
  value: ElementsToRerender,
): SetElementsToRerenderCommand {
  return {
    type: 'SET_ELEMENTS_TO_RERENDER_COMMAND',
    whenToRun: 'mid-interaction',
    value: value,
  }
}

export const runSetElementsToRerender: CommandFunction<SetElementsToRerenderCommand> = (
  _: EditorState,
  command: SetElementsToRerenderCommand,
) => {
  const editorStatePatch: EditorStatePatch = {
    canvas: { elementsToRerender: { $set: command.value } },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set Elements To Rerender: [${
      typeof command.value === 'string' ? command.value : command.value.map(EP.toString).join(', ')
    }]`,
  }
}
