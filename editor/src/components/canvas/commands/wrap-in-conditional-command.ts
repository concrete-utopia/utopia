import {
  emptyComments,
  jsExpressionValue,
  jsxConditionalExpression,
  JSXElement,
} from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  modifyUnderlyingElementForOpenFile,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'

export interface WrapInConditionalCommand extends BaseCommand {
  type: 'WRAP_IN_CONDITIONAL'
  whenToRun: WhenToRun
  target: ElementPath
}

export function wrapInConditionalCommand(
  whenToRun: WhenToRun,
  target: ElementPath,
): WrapInConditionalCommand {
  return { type: 'WRAP_IN_CONDITIONAL', whenToRun: whenToRun, target: target }
}

export const runWrapInConditionalCommand: CommandFunction<WrapInConditionalCommand> = (
  editor: EditorState,
  command: WrapInConditionalCommand,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []

  forUnderlyingTargetFromEditorState(
    command.target,
    editor,
    (success, elementToWrap, _underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const withElementRemoved = removeElementAtPath(command.target, components)

      // Add the target as the child of the true case of a conditional
      const conditionalUID = generateUidWithExistingComponents(editor.projectContents)
      const conditional = jsxConditionalExpression(
        conditionalUID,
        jsExpressionValue(true, emptyComments),
        'true',
        elementToWrap,
        jsExpressionValue(null, emptyComments),
        emptyComments,
      )

      // Insert the conditional at the initial index
      const targetParent = EP.parentPath(command.target)
      const insertionResult = insertElementAtPath(
        editor.projectContents,
        underlyingFilePath,
        targetParent,
        conditional,
        withElementRemoved,
        null, // FIXME Find the index position of the original element
      )

      editorStatePatches.push(
        getPatchForComponentChange(
          success.topLevelElements,
          insertionResult.components,
          success.imports,
          underlyingFilePath,
        ),
      )

      const elementUID = EP.toUid(command.target)
      const conditionalPath = EP.appendToPath(targetParent, conditionalUID)
      const newPath = EP.appendToPath(conditionalPath, elementUID)

      editorStatePatches.push({
        selectedViews: {
          $set: [newPath],
        },
      })
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Wrapped Element ${EP.toUid(command.target)} in a conditional`,
  }
}
