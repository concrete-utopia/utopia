import {
  getElementPathFromReparentTargetParent,
  InsertionPath,
  reparentTargetToString,
} from '../../editor/store/insertion-path'
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
import { IndexPosition } from '../../../utils/utils'

export interface AddElement extends BaseCommand {
  type: 'ADD_ELEMENT'
  parentPath: InsertionPath
  element: JSXElementChild
  indexPosition?: IndexPosition
}

export function addElement(
  whenToRun: WhenToRun,
  parentPath: InsertionPath,
  element: JSXElementChild,
  indexPosition?: IndexPosition,
): AddElement {
  return {
    whenToRun: whenToRun,
    type: 'ADD_ELEMENT',
    parentPath: parentPath,
    element: element,
    indexPosition: indexPosition,
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
        command.indexPosition ?? null,
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
