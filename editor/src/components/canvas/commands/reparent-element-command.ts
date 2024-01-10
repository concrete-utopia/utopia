import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import type { InsertionPath } from '../../editor/store/insertion-path'
import {
  getElementPathFromInsertionPath,
  isConditionalClauseInsertionPath,
} from '../../editor/store/insertion-path'
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
import type { IndexPosition } from '../../../utils/utils'
import { insertJSXElementChildren } from '../../../core/model/element-template-utils'
import type { JSXElement } from '../../../core/shared/element-template'
import { isJSXElement } from '../../../core/shared/element-template'

export interface ReparentElement extends BaseCommand {
  type: 'REPARENT_ELEMENT'
  target: ElementPath
  newParent: InsertionPath
  indexPosition: IndexPosition | null
  renameElement: string | null
}

export function reparentElement(
  whenToRun: WhenToRun,
  target: ElementPath,
  newParent: InsertionPath,
  indexPosition?: IndexPosition | null,
  renameElement?: string | null,
): ReparentElement {
  return {
    type: 'REPARENT_ELEMENT',
    whenToRun: whenToRun,
    target: target,
    newParent: newParent,
    indexPosition: indexPosition ?? null,
    renameElement: renameElement ?? null,
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
          const elementToInsert = isJSXElement(underlyingElementTarget)
            ? renameUnderlyingTargetIfNeeded(underlyingElementTarget, command.renameElement)
            : underlyingElementTarget

          if (underlyingFilePathTarget === underlyingFilePathNewParent) {
            const components = getUtopiaJSXComponentsFromSuccess(successTarget)
            const withElementRemoved = removeElementAtPath(command.target, components)

            const insertionResult = insertJSXElementChildren(
              command.newParent,
              [elementToInsert],
              withElementRemoved.components,
              command.indexPosition,
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
            const withElementRemoved = removeElementAtPath(
              command.target,
              componentsOldParent,
              successTarget.imports,
            )
            const componentsNewParent = getUtopiaJSXComponentsFromSuccess(successNewParent)

            const insertionResult = insertJSXElementChildren(
              command.newParent,
              [elementToInsert],
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

function renameUnderlyingTargetIfNeeded(
  underlyingTarget: JSXElement,
  renameElement: string | null,
): JSXElement {
  if (renameElement != null) {
    return {
      ...underlyingTarget,
      name: {
        ...underlyingTarget.name,
        baseVariable: renameElement,
      },
    }
  } else {
    return underlyingTarget
  }
}
