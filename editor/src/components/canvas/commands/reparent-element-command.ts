import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import {
  getElementPathFromInsertionPath,
  InsertionPath,
  insertionPathIsSlot,
} from '../../../components/editor/store/reparent-target'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface ReparentElement extends BaseCommand {
  type: 'REPARENT_ELEMENT'
  target: ElementPath
  newParent: InsertionPath
}

export function reparentElement(
  whenToRun: WhenToRun,
  target: ElementPath,
  newParent: InsertionPath,
): ReparentElement {
  return {
    type: 'REPARENT_ELEMENT',
    whenToRun: whenToRun,
    target: target,
    newParent: newParent,
  }
}

export const runReparentElement: CommandFunction<ReparentElement> = (
  editorState: EditorState,
  command: ReparentElement,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    command.target,
    editorState,
    (successTarget, underlyingElementTarget, _underlyingTarget, underlyingFilePathTarget) => {
      forUnderlyingTargetFromEditorState(
        getElementPathFromInsertionPath(command.newParent),
        editorState,
        (
          successNewParent,
          _underlyingElementNewParent,
          _underlyingTargetNewParent,
          underlyingFilePathNewParent,
        ) => {
          if (underlyingFilePathTarget === underlyingFilePathNewParent) {
            const components = getUtopiaJSXComponentsFromSuccess(successTarget)
            const withElementRemoved = removeElementAtPath(command.target, components)

            const insertionResult = insertElementAtPath(
              editorState.projectContents,
              underlyingFilePathTarget,
              command.newParent,
              underlyingElementTarget,
              withElementRemoved,
              null,
            )
            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              insertionResult.components,
              successTarget.imports,
              underlyingFilePathTarget,
            )

            editorStatePatches = [
              editorStatePatchOldParentFile,
              includeToastPatch(insertionResult.insertionDetails, editorState),
            ]
          } else {
            const componentsOldParent = getUtopiaJSXComponentsFromSuccess(successTarget)
            const withElementRemoved = removeElementAtPath(command.target, componentsOldParent)
            const componentsNewParent = getUtopiaJSXComponentsFromSuccess(successNewParent)

            const insertionResult = insertElementAtPath(
              editorState.projectContents,
              underlyingFilePathNewParent,
              command.newParent,
              underlyingElementTarget,
              componentsNewParent,
              null,
            )

            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              withElementRemoved,
              successTarget.imports,
              underlyingFilePathTarget,
            )

            const editorStatePatchNewParentFile = getPatchForComponentChange(
              successNewParent.topLevelElements,
              insertionResult.components,
              successNewParent.imports,
              underlyingFilePathNewParent,
            )

            editorStatePatches = [
              editorStatePatchOldParentFile,
              editorStatePatchNewParentFile,
              includeToastPatch(insertionResult.insertionDetails, editorState),
            ]
          }
        },
      )
    },
  )

  let parentDescription: string
  if (insertionPathIsSlot(command.newParent)) {
    parentDescription = `${EP.toUid(command.newParent.elementPath)} (${
      command.newParent.clause
    } clause)`
  } else {
    parentDescription = EP.toUid(command.newParent)
  }

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Reparent Element ${EP.toUid(
      command.target,
    )} to new parent ${parentDescription}`,
  }
}
