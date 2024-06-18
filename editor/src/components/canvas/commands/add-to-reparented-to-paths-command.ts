import type { Spec } from 'immutability-helper'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState } from '../../editor/store/editor-state'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'

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

export const runAddToReparentedToPaths = (
  command: AddToReparentedToPaths,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  if (commandLifecycle === 'end-interaction') {
    return {
      editorStatePatches: [],
      commandDescription: `Updated reparented to paths. No changes made.`,
    }
  }
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
