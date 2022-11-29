import { Spec } from 'immutability-helper'
import { EditorState } from '../../editor/store/editor-state'
import { ElementPath } from '../../../core/shared/project-file-types'
import { BaseCommand, CommandFunction, CommandFunctionResult, WhenToRun } from './commands'

export interface AddToReparentedToPaths extends BaseCommand {
  type: 'ADD_TO_REPARENTED_TO_PATHS'
  reparentedToPaths: Array<ElementPath>
}

export function addToReparentedToPaths(
  whenToRun: WhenToRun,
  reparentedToPaths: Array<ElementPath>,
): AddToReparentedToPaths {
  return {
    type: 'ADD_TO_REPARENTED_TO_PATHS',
    whenToRun: whenToRun,
    reparentedToPaths: reparentedToPaths,
  }
}

export const runAddToReparentedToPaths: CommandFunction<AddToReparentedToPaths> = (
  editorState: EditorState,
  command: AddToReparentedToPaths,
): CommandFunctionResult => {
  const editorStatePatch: Spec<EditorState> = {
    canvas: {
      controls: {
        reparentedToPaths: {
          $push: command.reparentedToPaths,
        },
      },
    },
  }

  return {
    editorStatePatches: [editorStatePatch],
    commandDescription: `Updated reparented to paths.`,
  }
}
