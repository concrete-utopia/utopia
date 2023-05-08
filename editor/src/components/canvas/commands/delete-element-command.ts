import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface DeleteElement extends BaseCommand {
  type: 'DELETE_ELEMENT'
  target: ElementPath
  whenToRun: WhenToRun
}

export function deleteElement(whenToRun: WhenToRun, target: ElementPath): DeleteElement {
  return {
    type: 'DELETE_ELEMENT',
    whenToRun: whenToRun,
    target: target,
  }
}

export const runDeleteElement: CommandFunction<DeleteElement> = (
  editorState: EditorState,
  command: DeleteElement,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    (successTarget, underlyingElementTarget, _underlyingTarget, underlyingFilePathTarget) => {
      const components = getUtopiaJSXComponentsFromSuccess(successTarget)
      const withElementRemoved = removeElementAtPath(command.target, components)
      editorStatePatches = [
        getPatchForComponentChange(
          successTarget.topLevelElements,
          withElementRemoved,
          successTarget.imports,
          underlyingFilePathTarget,
        ),
      ]
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Delete Element ${EP.toUid(command.target)} `,
  }
}
