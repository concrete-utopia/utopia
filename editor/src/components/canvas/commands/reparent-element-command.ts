import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  ConditionalClause,
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  getElementPathFromReparentTargetParent,
  insertElementAtPath,
  removeElementAtPath,
  ReparentTargetParent,
  reparentTargetParentIsConditionalClause,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface ReparentElement extends BaseCommand {
  type: 'REPARENT_ELEMENT'
  target: ElementPath
  newParent: ReparentTargetParent<ElementPath>
}

export function reparentElement(
  whenToRun: WhenToRun,
  target: ElementPath,
  newParent: ReparentTargetParent<ElementPath>,
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
        getElementPathFromReparentTargetParent(command.newParent),
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

            const withElementInserted = insertElementAtPath(
              editorState.projectContents,
              underlyingFilePathTarget,
              command.newParent,
              underlyingElementTarget,
              withElementRemoved,
              null,
            )
            const editorStatePatchOldParentFile = getPatchForComponentChange(
              successTarget.topLevelElements,
              withElementInserted,
              successTarget.imports,
              underlyingFilePathTarget,
            )

            editorStatePatches = [editorStatePatchOldParentFile]
          } else {
            const componentsOldParent = getUtopiaJSXComponentsFromSuccess(successTarget)
            const withElementRemoved = removeElementAtPath(command.target, componentsOldParent)
            const componentsNewParent = getUtopiaJSXComponentsFromSuccess(successNewParent)

            const withElementInserted = insertElementAtPath(
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
              withElementInserted,
              successNewParent.imports,
              underlyingFilePathNewParent,
            )

            editorStatePatches = [editorStatePatchOldParentFile, editorStatePatchNewParentFile]
          }
        },
      )
    },
  )

  let parentDescription: string
  if (reparentTargetParentIsConditionalClause(command.newParent)) {
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
