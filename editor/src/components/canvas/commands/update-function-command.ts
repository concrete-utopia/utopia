import * as EP from '../../../core/shared/element-path'
import { isUtopiaJSXComponent } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { withUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import type { BaseCommand, CommandFunctionResult, CommandState, WhenToRun } from './commands'
import { CommandFunction, getPatchForComponentChange } from './commands'

export interface UpdateFunctionCommand extends BaseCommand {
  type: 'UPDATE_FUNCTION_COMMAND'
  updateFunction: (
    editorState: EditorState,
    commandState: CommandState,
    commandLifecycle: InteractionLifecycle,
  ) => Array<EditorStatePatch>
}

export function updateFunctionCommand(
  whenToRun: WhenToRun,
  updateFunction: (
    editorState: EditorState,
    commandState: CommandState,
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
  commandState: CommandState,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
  return {
    editorStatePatches: command.updateFunction(editorState, commandState, commandLifecycle),
    commandState: commandState,
    commandDescription: `Update Callback`,
  }
}
