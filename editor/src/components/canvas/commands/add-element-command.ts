import {
  getElementPathFromReparentTargetParent,
  ReparentTargetParent,
  reparentTargetToString,
} from '../../../components/editor/store/reparent-target'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
} from '../../../components/editor/store/editor-state'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { JSXElementChild } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'

export interface AddElement extends BaseCommand {
  type: 'ADD_ELEMENT'
  parentPath: ReparentTargetParent<ElementPath>
  element: JSXElementChild
}

export function addElement(
  whenToRun: WhenToRun,
  parentPath: ReparentTargetParent<ElementPath>,
  element: JSXElementChild,
): AddElement {
  return {
    whenToRun: whenToRun,
    type: 'ADD_ELEMENT',
    parentPath: parentPath,
    element: element,
  }
}

export const runAddElement: CommandFunction<AddElement> = (
  editorState: EditorState,
  command: AddElement,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    getElementPathFromReparentTargetParent(command.parentPath),
    editorState,
    (
      parentSuccess,
      _underlyingElementNewParent,
      _underlyingTargetNewParent,
      underlyingFilePathNewParent,
    ) => {
      const componentsNewParent = getUtopiaJSXComponentsFromSuccess(parentSuccess)

      const insertionResult = insertElementAtPath(
        editorState.projectContents,
        underlyingFilePathNewParent,
        command.parentPath,
        command.element,
        componentsNewParent,
        null,
      )
      const withElementInserted = insertionResult.components

      const editorStatePatchNewParentFile = getPatchForComponentChange(
        parentSuccess.topLevelElements,
        withElementInserted,
        parentSuccess.imports,
        underlyingFilePathNewParent,
      )

      editorStatePatches = [
        editorStatePatchNewParentFile,
        includeToastPatch(insertionResult.insertionDetails, editorState),
      ]
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Add Element to ${reparentTargetToString(command.parentPath)}`,
  }
}
