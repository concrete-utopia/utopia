import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import {
  insertJSXElementChildren,
  renameJsxElementChild,
} from '../../../core/model/element-template-utils'
import {
  getFilePathMappings,
  getUtopiaJSXComponentsFromSuccess,
} from '../../../core/model/project-file-utils'
import * as EP from '../../../core/shared/element-path'
import { optionalMap } from '../../../core/shared/optional-utils'
import { type ElementPath } from '../../../core/shared/project-file-types'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import type { InsertionSubject } from '../../editor/editor-modes'
import type { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { forUnderlyingTargetFromEditorState } from '../../editor/store/editor-state'
import type { InsertionPath } from '../../editor/store/insertion-path'
import type { BaseCommand, CommandFunction, WhenToRun } from './commands'
import { getPatchForComponentChange } from './commands'
import { type JSXElement } from '../../../core/shared/element-template'

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

  let subjectElement: JSXElement = subject.element

  forUnderlyingTargetFromEditorState(
    insertionPath.intendedParentPath,
    editor,
    (success, _element, _underlyingTarget, underlyingFilePath) => {
      const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
      const updatedImports = mergeImports(
        underlyingFilePath,
        getFilePathMappings(editor.projectContents),
        success.imports,
        subject.importsToAdd,
      )

      subjectElement = renameJsxElementChild(subject.element, updatedImports.duplicateNameMapping)

      const insertionResult = insertJSXElementChildren(
        insertionPath,
        [subjectElement],
        utopiaComponents,
        null,
      )

      editorStatePatches.push(
        getPatchForComponentChange(
          success.topLevelElements,
          insertionResult.components,
          updatedImports.imports,
          underlyingFilePath,
        ),
      )
      editorStatePatches.push(includeToastPatch(insertionResult.insertionDetails, editor))
      selectedViews.push(EP.appendToPath(insertionPath.intendedParentPath, subjectElement.uid))
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
    commandDescription: `Insert element ${subjectElement.uid} to parent ${optionalMap(
      EP.toUid,
      insertionPath.intendedParentPath,
    )}`,
  }
}
