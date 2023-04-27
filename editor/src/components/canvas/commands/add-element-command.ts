import {
  getElementPathFromInsertionPath,
  InsertionPath,
  insertionPathToString,
} from '../../editor/store/insertion-path'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  insertElementAtPath_DEPRECATED,
} from '../../../components/editor/store/editor-state'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { JSXElementChild } from '../../../core/shared/element-template'
import { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import { IndexPosition } from '../../../utils/utils'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { UseNewInsertJsxElementChild } from '../canvas-utils'

export interface AddElement extends BaseCommand {
  type: 'ADD_ELEMENT'
  parentPath: InsertionPath
  element: JSXElementChild
  indexPosition?: IndexPosition
  importsToAdd?: Imports
  useNewInsertJSXElementChild: UseNewInsertJsxElementChild
}

export function addElement(
  whenToRun: WhenToRun,
  parentPath: InsertionPath,
  element: JSXElementChild,
  options: Partial<{
    indexPosition: IndexPosition
    importsToAdd: Imports
  }> = {},
  useNewInsertJSXElementChild: UseNewInsertJsxElementChild,
): AddElement {
  return {
    whenToRun: whenToRun,
    type: 'ADD_ELEMENT',
    parentPath: parentPath,
    element: element,
    indexPosition: options.indexPosition,
    importsToAdd: options.importsToAdd,
    useNewInsertJSXElementChild: useNewInsertJSXElementChild,
  }
}

export const runAddElement: CommandFunction<AddElement> = (
  editorState: EditorState,
  command: AddElement,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  forUnderlyingTargetFromEditorState(
    getElementPathFromInsertionPath(command.parentPath),
    editorState,
    (
      parentSuccess,
      _underlyingElementNewParent,
      _underlyingTargetNewParent,
      underlyingFilePathNewParent,
    ) => {
      const componentsNewParent = getUtopiaJSXComponentsFromSuccess(parentSuccess)

      const insertionResult =
        command.useNewInsertJSXElementChild === 'use-new-insertJSXElementChild'
          ? insertElementAtPath(
              command.parentPath,
              command.element,
              componentsNewParent,
              command.indexPosition ?? null,
            )
          : insertElementAtPath_DEPRECATED(
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
        mergeImports(
          underlyingFilePathNewParent,
          parentSuccess.imports,
          mergeImports(
            underlyingFilePathNewParent,
            insertionResult.importsToAdd,
            command.importsToAdd ?? {},
          ),
        ),
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
    commandDescription: `Add Element to ${insertionPathToString(command.parentPath)}`,
  }
}
