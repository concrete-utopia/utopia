import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  removeElementAtPath,
} from '../../../components/editor/store/editor-state'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { JSXElementChild } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface AddElement extends BaseCommand {
  type: 'ADD_ELEMENT'
  parentPath: ElementPath
  element: JSXElementChild
}

export function addElement(
  whenToRun: WhenToRun,
  parentPath: ElementPath,
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
    command.parentPath,
    editorState,
    (
      parentSuccess,
      _underlyingElementNewParent,
      _underlyingTargetNewParent,
      underlyingFilePathNewParent,
    ) => {
      const componentsNewParent = getUtopiaJSXComponentsFromSuccess(parentSuccess)

      const withElementInserted = insertElementAtPath(
        editorState.projectContents,
        underlyingFilePathNewParent,
        command.parentPath,
        command.element,
        componentsNewParent,
        null,
      )

      const editorStatePatchNewParentFile = getPatchForComponentChange(
        parentSuccess.topLevelElements,
        withElementInserted,
        parentSuccess.imports,
        underlyingFilePathNewParent,
      )

      editorStatePatches = [editorStatePatchNewParentFile]
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Add Element to ${EP.toString(command.parentPath)}`,
  }
}
