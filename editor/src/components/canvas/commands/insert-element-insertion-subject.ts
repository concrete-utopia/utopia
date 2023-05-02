import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { optionalMap } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { InsertionSubject } from '../../editor/editor-modes'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
} from '../../editor/store/editor-state'
import { InsertionPath } from '../../editor/store/insertion-path'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface InsertElementInsertionSubject extends BaseCommand {
  type: 'INSERT_ELEMENT_INSERTION_SUBJECT'
  subject: InsertionSubject
  insertionPath: InsertionPath
}

export function insertElementInsertionSubject(
  whenToRun: WhenToRun,
  subject: InsertionSubject,
  insertionPath: InsertionPath,
): InsertElementInsertionSubject {
  return {
    type: 'INSERT_ELEMENT_INSERTION_SUBJECT',
    whenToRun: whenToRun,
    subject: subject,
    insertionPath: insertionPath,
  }
}

export const runInsertElementInsertionSubject: CommandFunction<InsertElementInsertionSubject> = (
  editor: EditorState,
  command: InsertElementInsertionSubject,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  let selectedViews: Array<ElementPath> = []
  const { subject, insertionPath } = command

  forUnderlyingTargetFromEditorState(
    insertionPath.intendedParentPath,
    editor,
    (success, _element, _underlyingTarget, underlyingFilePath) => {
      const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)

      const insertionResult = insertElementAtPath(
        insertionPath,
        subject.element,
        utopiaComponents,
        null,
      )

      const updatedImports = mergeImports(
        underlyingFilePath,
        success.imports,
        mergeImports(underlyingFilePath, insertionResult.importsToAdd, subject.importsToAdd),
      )

      editorStatePatches.push(
        getPatchForComponentChange(
          success.topLevelElements,
          insertionResult.components,
          updatedImports,
          underlyingFilePath,
        ),
      )
      editorStatePatches.push(includeToastPatch(insertionResult.insertionDetails, editor))
      selectedViews.push(EP.appendToPath(insertionPath.intendedParentPath, subject.element.uid))
    },
  )

  if (selectedViews.length > 0) {
    editorStatePatches.push({
      selectedViews: {
        $set: selectedViews,
      },
    })
  }

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Insert element ${subject.element.uid} to parent ${optionalMap(
      EP.toUid,
      insertionPath.intendedParentPath,
    )}`,
  }
}
