import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'

export interface UpdateFunctionCommand extends BaseCommand {
  type: 'UPDATE_FUNCTION_COMMAND'
  updateFunction: (
    editorState: EditorState,
    commandLifecycle: InteractionLifecycle,
  ) => Array<EditorStatePatch>
}

export function updateFunctionCommand(
  whenToRun: WhenToRun,
  updateFunction: (
    editorState: EditorState,
    commandLifecycle: InteractionLifecycle,
  ) => Array<EditorStatePatch>,
): UpdateFunctionCommand {
  return {
    type: 'UPDATE_FUNCTION_COMMAND',
    whenToRun: whenToRun,
    updateFunction: updateFunction,
  }
}

export const runUpdateFunctionCommand = (
  editorState: EditorState,
  command: UpdateFunctionCommand,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  return {
    editorStatePatches: command.updateFunction(editorState, commandLifecycle),
    commandDescription: `Update Callback`,
  }
}
