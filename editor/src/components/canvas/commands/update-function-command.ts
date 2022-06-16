import * as EP from '../../../core/shared/element-path'
import { isUtopiaJSXComponent } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  EditorState,
  EditorStatePatch,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import { duplicate } from '../canvas-utils'
import {
  BaseCommand,
  CommandFunction,
  CommandFunctionResult,
  getPatchForComponentChange,
  TransientOrNot,
} from './commands'

export interface UpdateFunctionCommand extends BaseCommand {
  type: 'UPDATE_FUNCTION_COMMAND'
  updateFunction: (editorState: EditorState, transient: TransientOrNot) => Array<EditorStatePatch>
}

export function updateFunctionCommand(
  transient: TransientOrNot,
  updateFunction: (editorState: EditorState, transient: TransientOrNot) => Array<EditorStatePatch>,
): UpdateFunctionCommand {
  return {
    type: 'UPDATE_FUNCTION_COMMAND',
    transient: transient,
    updateFunction: updateFunction,
  }
}

export const runUpdateFunctionCommand = (
  editorState: EditorState,
  command: UpdateFunctionCommand,
  runningAsTransient: TransientOrNot,
): CommandFunctionResult => {
  return {
    editorStatePatches: command.updateFunction(editorState, runningAsTransient),
    commandDescription: `Update Callback`,
  }
}
