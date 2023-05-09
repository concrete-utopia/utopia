import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import {
  getElementPathFromInsertionPath,
  InsertionPath,
  isConditionalClauseInsertionPath,
} from '../../editor/store/insertion-path'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import { IndexPosition } from '../../../utils/utils'

export interface ReparentElement extends BaseCommand {
  type: 'REPARENT_ELEMENT'
  target: ElementPath
  newParent: InsertionPath
  indexPosition: IndexPosition | null
}

export function reparentElement(
  whenToRun: WhenToRun,
  target: ElementPath,
  newParent: InsertionPath,
  indexPosition?: IndexPosition | null,
): ReparentElement {
  return {
    type: 'REPARENT_ELEMENT',
    whenToRun: whenToRun,
    target: target,
    newParent: newParent,
    indexPosition: indexPosition ?? null,
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
              command.newParent,
              underlyingElementTarget,
              withElementRemoved,
              command.indexPosition,
            )
            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              insertionResult.components,
              mergeImports(
                underlyingFilePathTarget,
                successTarget.imports,
                insertionResult.importsToAdd,
              ),
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
              mergeImports(
                underlyingFilePathNewParent,
                successNewParent.imports,
                insertionResult.importsToAdd,
              ),
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
  if (isConditionalClauseInsertionPath(command.newParent)) {
    parentDescription = `${EP.toUid(command.newParent.intendedParentPath)} (${
      command.newParent.clause
    } clause)`
  } else {
    parentDescription = EP.toUid(command.newParent.intendedParentPath)
  }

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Reparent Element ${EP.toUid(
      command.target,
    )} to new parent ${parentDescription}`,
  }
}
