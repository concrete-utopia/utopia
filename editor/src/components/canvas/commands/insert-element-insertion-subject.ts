import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import * as EP from '../../../core/shared/element-path'
import { optionalMap } from '../../../core/shared/optional-utils'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { ElementInsertionSubject } from '../../editor/editor-modes'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'

export interface InsertElementInsertionSubject extends BaseCommand {
  type: 'INSERT_ELEMENT_INSERTION_SUBJECT'
  subject: ElementInsertionSubject
}

export function insertElementInsertionSubject(
  whenToRun: WhenToRun,
  subject: ElementInsertionSubject,
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

      const withElementInserted = insertElementAtPath(
        editor.projectContents,
        underlyingFilePath,
        targetParent,
        subject.element,
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
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Insert element ${subject.element.uid} to parent ${optionalMap(
      EP.toUid,
      parent,
    )}`,
  }
}
