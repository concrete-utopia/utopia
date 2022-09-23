import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  removeElementAtPath,
} from '../../../components/editor/store/editor-state'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface RemoveElement extends BaseCommand {
  type: 'REMOVE_ELEMENT'
  path: ElementPath
}

export function removeElement(whenToRun: WhenToRun, path: ElementPath): RemoveElement {
  return {
    whenToRun: whenToRun,
    type: 'REMOVE_ELEMENT',
    path: path,
  }
}

export const runRemoveElement: CommandFunction<RemoveElement> = (
  editorState: EditorState,
  command: RemoveElement,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    command.path,
    editorState,
    (success, _underlyingElement, _underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)

      const withElementRemoved = removeElementAtPath(command.path, components)

      const editorStatePatchNewParentFile = getPatchForComponentChange(
        success.topLevelElements,
        withElementRemoved,
        success.imports,
        underlyingFilePath,
      )

      editorStatePatches = [editorStatePatchNewParentFile]
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Remove Element to ${EP.toString(command.path)}`,
  }
}
