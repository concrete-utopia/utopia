import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import {
  emptyComments,
  jsxAttributeValue,
  jsxConditionalExpression,
} from '../../../core/shared/element-template'
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
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface InsertElementInsertionSubject extends BaseCommand {
  type: 'INSERT_ELEMENT_INSERTION_SUBJECT'
  subject: InsertionSubject
}

export function insertElementInsertionSubject(
  whenToRun: WhenToRun,
  subject: InsertionSubject,
): InsertElementInsertionSubject {
  return {
    type: 'INSERT_ELEMENT_INSERTION_SUBJECT',
    whenToRun: whenToRun,
    subject: subject,
  }
}

export const runInsertElementInsertionSubject: CommandFunction<InsertElementInsertionSubject> = (
  editor: EditorState,
  command: InsertElementInsertionSubject,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []
  let selectedViews: Array<ElementPath> = []
  const { subject } = command
  const parent =
    subject.parent?.target == null
      ? // action.parent == null means Canvas, which means storyboard root element
        getStoryboardElementPath(editor.projectContents, editor.canvas.openFile?.filename ?? null)
      : subject.parent.target ?? null

  forUnderlyingTargetFromEditorState(
    parent,
    editor,
    (success, _element, _underlyingTarget, underlyingFilePath) => {
      const utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
      const targetParent =
        parent == null
          ? // action.parent == null means Canvas, which means storyboard root element
            getStoryboardElementPath(editor.projectContents, underlyingFilePath)
          : parent

      if (targetParent == null) {
        return
      }

      const uid = generateUidWithExistingComponents(editor.projectContents)
      const elementToInsert = subject.wrapInConditional
        ? jsxConditionalExpression(
            uid,
            jsxAttributeValue(true, emptyComments),
            subject.element,
            jsxAttributeValue(null, emptyComments),
            emptyComments,
          )
        : subject.element
      const withElementInserted = insertElementAtPath(
        editor.projectContents,
        underlyingFilePath,
        targetParent,
        elementToInsert,
        utopiaComponents,
        null,
      )

      const updatedImports = mergeImports(underlyingFilePath, success.imports, subject.importsToAdd)

      editorStatePatches.push(
        getPatchForComponentChange(
          success.topLevelElements,
          withElementInserted,
          updatedImports,
          underlyingFilePath,
        ),
      )
      if (subject.wrapInConditional) {
        selectedViews.push(EP.appendToPath(EP.appendToPath(targetParent, uid), subject.element.uid))
      } else {
        selectedViews.push(EP.appendToPath(targetParent, subject.element.uid))
      }
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
      parent,
    )}`,
  }
}
