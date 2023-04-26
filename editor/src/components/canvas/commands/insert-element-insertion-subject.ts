import { includeToastPatch } from '../../../components/editor/actions/toast-helpers'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
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
import { childInsertionPath } from '../../editor/store/insertion-path'
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

      const insertionResult = insertElementAtPath(
        childInsertionPath(targetParent),
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
      selectedViews.push(EP.appendToPath(targetParent, subject.element.uid))
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
