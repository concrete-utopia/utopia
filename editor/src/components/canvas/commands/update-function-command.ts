import * as EP from '../../../core/shared/element-path'
import { isUtopiaJSXComponent } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  EditorState,
  EditorStatePatch,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import {
  BaseCommand,
  CommandFunction,
  CommandFunctionResult,
  getPatchForComponentChange,
  WhenToRun,
} from './commands'

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
