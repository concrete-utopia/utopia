import type { InsertionPath } from '../../editor/store/insertion-path'
import {
  getElementPathFromInsertionPath,
  insertionPathToString,
} from '../../editor/store/insertion-path'
import type { EditorState, EditorStatePatch } from '../../../components/editor/store/editor-state'
import { forUnderlyingTargetFromEditorState } from '../../../components/editor/store/editor-state'
import {
  getFilePathMappings,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import type { JSXElementChild } from '../../../core/shared/element-template'
import type { Imports } from '../../../core/shared/project-file-types'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { getPatchForComponentChange } from './commands'
import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import type { IndexPosition } from '../../../utils/utils'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { insertJSXElementChildren } from '../../../core/model/element-template-utils'

export interface AddElement extends BaseCommand {
  type: 'ADD_ELEMENT'
  parentPath: InsertionPath
  element: JSXElementChild
  indexPosition?: IndexPosition
  importsToAdd?: Imports
}

export function addElement(
  whenToRun: WhenToRun,
  parentPath: InsertionPath,
  element: JSXElementChild,
  options: Partial<{
    indexPosition: IndexPosition
    importsToAdd: Imports
  }> = {},
): AddElement {
  return {
    whenToRun: whenToRun,
    type: 'ADD_ELEMENT',
    parentPath: parentPath,
    element: element,
    indexPosition: options.indexPosition,
    importsToAdd: options.importsToAdd,
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

      const insertionResult = insertJSXElementChildren(
        command.parentPath,
        [command.element],
        componentsNewParent,
        command.indexPosition ?? null,
      )
      const withElementInserted = insertionResult.components

      const editorStatePatchNewParentFile = getPatchForComponentChange(
        parentSuccess.topLevelElements,
        withElementInserted,
        mergeImports(
          underlyingFilePathNewParent,
          getFilePathMappings(editorState.projectContents),
          parentSuccess.imports,
          command.importsToAdd ?? {},
        ).imports,
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
