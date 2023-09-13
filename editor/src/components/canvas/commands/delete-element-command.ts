import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { DerivedState, EditorState, EditorStatePatch } from '../../editor/store/editor-state'
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
  derivedState: DerivedState,
  command: DeleteElement,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    derivedState,
    (successTarget, _underlyingElementTarget, underlyingTarget, underlyingFilePathTarget) => {
      const components = getUtopiaJSXComponentsFromSuccess(successTarget)
      const withElementRemoved = removeElementAtPath(underlyingTarget, components)
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
