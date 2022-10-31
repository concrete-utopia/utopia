import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
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

function includeDescendants(
  elementsToRerender: ElementsToRerender,
  metadata: ElementInstanceMetadataMap,
): ElementsToRerender {
  if (elementsToRerender === 'rerender-all-elements') {
    return elementsToRerender
  } else {
    const descendants = elementsToRerender.flatMap((path) =>
      // FIXME maybe move the dom walker calling code
      MetadataUtils.getDescendantPaths(metadata, path),
    )

    return elementsToRerender.concat(descendants)
  }
}

export const runSetElementsToRerender: CommandFunction<SetElementsToRerenderCommand> = (
  editorState: EditorState,
  command: SetElementsToRerenderCommand,
) => {
  const includingDescendents = includeDescendants(command.value, editorState.jsxMetadata)

  const editorStatePatch: EditorStatePatch = {
    canvas: { elementsToRerender: { $set: includingDescendents } },
  }
  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Set Elements To Rerender: [${
      typeof command.value === 'string' ? command.value : command.value.map(EP.toString).join(', ')
    }]`,
  }
}
