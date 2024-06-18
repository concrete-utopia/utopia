import type { Spec } from 'immutability-helper'
import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import type { EditorState, EditorStatePatch } from '../../../components/editor/store/editor-state'
import { forUnderlyingTargetFromEditorState } from '../../../components/editor/store/editor-state'
import { insertJSXElementChildren } from '../../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import type { JSXElementChild } from '../../../core/shared/element-template'
import type { Imports } from '../../../core/shared/project-file-types'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import type { IndexPosition } from '../../../utils/utils'
import type { InsertionPath } from '../../editor/store/insertion-path'
import {
  getElementPathFromInsertionPath,
  insertionPathToString,
} from '../../editor/store/insertion-path'
import type { InteractionLifecycle } from '../canvas-strategies/canvas-strategy-types'
import {
  addToReparentedToPaths,
  runAddToReparentedToPaths,
} from './add-to-reparented-to-paths-command'
import type { BaseCommand, CommandFunctionResult, WhenToRun } from './commands'
import { getPatchForComponentChange } from './commands'

export interface AddElements extends BaseCommand {
  type: 'ADD_ELEMENTS'
  parentPath: InsertionPath
  elements: Array<JSXElementChild>
  indexPosition?: IndexPosition
  importsToAdd?: Imports
}

export function addElements(
  whenToRun: WhenToRun,
  parentPath: InsertionPath,
  elements: Array<JSXElementChild>,
  options: Partial<{
    indexPosition: IndexPosition
    importsToAdd: Imports
  }> = {},
): AddElements {
  return {
    whenToRun: whenToRun,
    type: 'ADD_ELEMENTS',
    parentPath: parentPath,
    elements: elements,
    indexPosition: options.indexPosition,
    importsToAdd: options.importsToAdd,
  }
}

export const runAddElements = (
  editorState: EditorState,
  command: AddElements,
  commandLifecycle: InteractionLifecycle,
): CommandFunctionResult => {
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
        command.elements,
        componentsNewParent,
        command.indexPosition ?? null,
      )
      const withElementsInserted = insertionResult.components

      const editorStatePatchNewParentFile = getPatchForComponentChange(
        parentSuccess.topLevelElements,
        withElementsInserted,
        mergeImports(underlyingFilePathNewParent, parentSuccess.imports, command.importsToAdd ?? {})
          .imports,
        underlyingFilePathNewParent,
      )

      const newElementPathsPatch: Array<Spec<EditorState>> = runAddToReparentedToPaths(
        addToReparentedToPaths('mid-interaction', insertionResult.insertedChildrenPaths),
        commandLifecycle,
      ).editorStatePatches

      editorStatePatches = [
        editorStatePatchNewParentFile,
        includeToastPatch(insertionResult.insertionDetails, editorState),
        ...newElementPathsPatch,
      ]
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Add Elements to ${insertionPathToString(command.parentPath)}`,
  }
}
