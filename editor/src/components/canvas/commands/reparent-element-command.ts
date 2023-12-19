import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import type { InsertionPath } from '../../editor/store/insertion-path'
import {
  getElementPathFromInsertionPath,
  isConditionalClauseInsertionPath,
} from '../../editor/store/insertion-path'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import {
  forUnderlyingTargetFromEditorState,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { getPatchForComponentChange } from './commands'
import type { IndexPosition } from '../../../utils/utils'
import { insertJSXElementChildren } from '../../../core/model/element-template-utils'

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
            const withElementRemoved = removeElementAtPath(
              command.target,
              components,
              successTarget.imports,
            )

            const insertionResult = insertJSXElementChildren(
              command.newParent,
              [underlyingElementTarget],
              withElementRemoved.components,
              command.indexPosition,
            )
            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              insertionResult.components,
              mergeImports(
                underlyingFilePathTarget,
                withElementRemoved.imports,
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
            const withElementRemoved = removeElementAtPath(
              command.target,
              componentsOldParent,
              successTarget.imports,
            )
            const componentsNewParent = getUtopiaJSXComponentsFromSuccess(successNewParent)

            const insertionResult = insertJSXElementChildren(
              command.newParent,
              [underlyingElementTarget],
              componentsNewParent,
              command.indexPosition,
            )

            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              withElementRemoved.components,
              withElementRemoved.imports,
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
