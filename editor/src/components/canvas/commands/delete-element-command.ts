import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import {
  forUnderlyingTargetFromEditorState,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { getPatchForComponentChange } from './commands'

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
      const withElementRemoved = removeElementAtPath(_underlyingTarget, components)
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
