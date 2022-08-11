import { Spec } from 'immutability-helper'
import { EditorState } from 'src/components/editor/store/editor-state'
import { ElementPath } from 'src/core/shared/project-file-types'
import {
  BaseCommand,
  CommandFunction,
  CommandFunctionResult,
  WhenToRun,
} from '../commands/commands'

export interface addToReparentedToPaths extends BaseCommand {
  type: 'ADD_TO_REPARENTED_TO_PATHS'
  reparentedToPaths: Array<ElementPath>
}

export function addToReparentedToPaths(
  whenToRun: WhenToRun,
  reparentedToPaths: Array<ElementPath>,
): addToReparentedToPaths {
  return {
    type: 'ADD_TO_REPARENTED_TO_PATHS',
    whenToRun: whenToRun,
    reparentedToPaths: reparentedToPaths,
  }
}

export const runAddToReparentedToPaths: CommandFunction<addToReparentedToPaths> = (
  editorState: EditorState,
  command: addToReparentedToPaths,
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
