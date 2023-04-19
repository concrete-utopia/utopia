import {
  emptyComments,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxConditionalExpression,
  jsxElement,
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
import { absolute } from '../../../utils/utils'
import {
  generateUidWithExistingComponents,
  getIndexInParent,
} from '../../../core/model/element-template-utils'
import { ProjectContentTreeRoot } from '../../assets'
import { JSXAttributesPart, JSXAttributesEntry } from '../../../core/shared/element-template'

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
      const indexInParent = getIndexInParent(
        success.topLevelElements,
        EP.dynamicPathToStaticPath(command.target),
      )
      const index = indexInParent >= 0 ? absolute(indexInParent) : null

      const wrapper = getInsertionSubjectWrapper(
        command.wrapper,
        command.wrapperUID,
        elementToWrap,
        editor.projectContents,
      )

      // Insert the wrapper at the initial index
      const targetParent = EP.parentPath(command.target)
      const insertionResult = insertElementAtPath(
        editor.projectContents,
        underlyingFilePath,
        targetParent,
        wrapper,
        withElementRemoved,
        index,
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
  projectContents: ProjectContentTreeRoot,
) => {
  switch (insertionSubjectWrapper) {
    case 'conditional':
      return jsxConditionalExpression(
        wrapperUID,
        jsExpressionValue(true, emptyComments),
        'true',
        elementToWrap,
        getInsertionSubjectWrapperConditionalFalseBranch(projectContents, elementToWrap),
        emptyComments,
      )
    case 'fragment':
      return jsxFragment(wrapperUID, [elementToWrap], true)
    default:
      assertNever(insertionSubjectWrapper)
  }
}

const falseBranchSideLength = 50

function getInsertionSubjectWrapperConditionalFalseBranch(
  projectContents: ProjectContentTreeRoot,
  trueBranch: JSXElementChild,
): JSXElementChild {
  function isStyleProp(p: JSXAttributesPart): boolean {
    return p.type === 'JSX_ATTRIBUTES_ENTRY' && p.key === 'style'
  }

  function getStyle(element: JSXElementChild): JSXAttributesEntry {
    const emptyStyle: JSXAttributesEntry = {
      type: 'JSX_ATTRIBUTES_ENTRY',
      key: 'style',
      value: jsExpressionValue({}, emptyComments),
      comments: emptyComments,
    }

    if (element.type !== 'JSX_ELEMENT') {
      return emptyStyle
    }
    const found = element.props.find(isStyleProp)
    if (found == null || found.type !== 'JSX_ATTRIBUTES_ENTRY') {
      return emptyStyle
    }
    return found
  }

  function getNumberProp(e: JSXAttributesEntry, key: string): number {
    if (e.value.type !== 'ATTRIBUTE_VALUE') {
      return 0
    }
    const value = e.value.value[key]
    if (typeof value !== 'number') {
      return 0
    }
    return value
  }

  const uid = generateUidWithExistingComponents(projectContents)

  const trueBranchStyle = getStyle(trueBranch)
  const left = getNumberProp(trueBranchStyle, 'left')
  const top = getNumberProp(trueBranchStyle, 'top')

  return jsxElement(
    'div',
    uid,
    jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: left,
          top: top,
          width: falseBranchSideLength,
          height: falseBranchSideLength,
        },
        emptyComments,
      ),
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [],
  )
}
