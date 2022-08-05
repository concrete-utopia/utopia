import { Spec } from 'immutability-helper'
import { EditorState } from 'src/components/editor/store/editor-state'
import { ElementPath } from 'src/core/shared/project-file-types'
import {
  BaseCommand,
  CommandFunction,
  CommandFunctionResult,
  TransientOrNot,
} from '../commands/commands'

export interface SetReparentedToPaths extends BaseCommand {
  type: 'SET_REPARENTED_TO_PATHS'
  reparentedToPaths: Array<ElementPath>
}

export function setReparentedToPaths(
  transient: TransientOrNot,
  reparentedToPaths: Array<ElementPath>,
): SetReparentedToPaths {
  return {
    type: 'SET_REPARENTED_TO_PATHS',
    transient: transient,
    reparentedToPaths: reparentedToPaths,
  }
}

export const runSetReparentedToPaths: CommandFunction<SetReparentedToPaths> = (
  editorState: EditorState,
  command: SetReparentedToPaths,
): CommandFunctionResult => {
  const editorStatePatch: Spec<EditorState> = {
    canvas: {
      controls: {
        reparentedToPaths: {
          $set: command.reparentedToPaths,
        },
      },
    },
  }

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Updated reparented to paths.`,
  }
}
