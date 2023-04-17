import {
  emptyComments,
  jsExpressionValue,
  jsxConditionalExpression,
  JSXElementChild,
  jsxFragment,
} from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { InsertionSubjectWrapper } from '../../editor/editor-modes'
import { assertNever } from '../../../core/shared/utils'

type ContainerToWrapIn = InsertionSubjectWrapper

export interface WrapInContainerCommand extends BaseCommand {
  type: 'WRAP_IN_CONTAINER'
  whenToRun: WhenToRun
  target: ElementPath
  wrapperUID: string
  wrapper: ContainerToWrapIn
}

export function wrapInContainerCommand(
  whenToRun: WhenToRun,
  target: ElementPath,
  wrapperUID: string,
  wrapper: ContainerToWrapIn,
): WrapInContainerCommand {
  return {
    type: 'WRAP_IN_CONTAINER',
    whenToRun: whenToRun,
    target: target,
    wrapperUID: wrapperUID,
    wrapper: wrapper,
  }
}

export const runWrapInContainerCommand: CommandFunction<WrapInContainerCommand> = (
  editor: EditorState,
  command: WrapInContainerCommand,
) => {
  let editorStatePatches: Array<EditorStatePatch> = []

  forUnderlyingTargetFromEditorState(
    command.target,
    editor,
    (success, elementToWrap, _underlyingTarget, underlyingFilePath) => {
      const components = getUtopiaJSXComponentsFromSuccess(success)
      const withElementRemoved = removeElementAtPath(command.target, components)

      const wrapper = getInsertionSubjectWrapper(command.wrapper, command.wrapperUID, elementToWrap)

      // Insert the wrapper at the initial index
      const targetParent = EP.parentPath(command.target)
      const insertionResult = insertElementAtPath(
        editor.projectContents,
        underlyingFilePath,
        targetParent,
        wrapper,
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

      const wrapperPath = EP.appendToPath(targetParent, wrapper.uid)

      editorStatePatches.push({
        selectedViews: {
          $set: [wrapperPath],
        },
      })
    },
  )

  return {
    editorStatePatches: editorStatePatches,
    commandDescription: `Wrapped Element ${EP.toUid(command.target)} in a ${command.wrapper}`,
  }
}

const getInsertionSubjectWrapper = (
  insertionSubjectWrapper: InsertionSubjectWrapper,
  wrapperUID: string,
  elementToWrap: JSXElementChild,
) => {
  switch (insertionSubjectWrapper) {
    case 'conditional':
      return jsxConditionalExpression(
        wrapperUID,
        jsExpressionValue(true, emptyComments),
        'true',
        elementToWrap,
        jsExpressionValue(null, emptyComments),
        emptyComments,
      )
    case 'fragment':
      return jsxFragment(wrapperUID, [elementToWrap], true)
    default:
      assertNever(insertionSubjectWrapper)
  }
}
