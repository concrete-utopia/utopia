import {
  emptyComments,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxConditionalExpression,
  jsxElement,
  JSXElementChild,
  jsxFragment,
} from '../../../core/shared/element-template'
import { ElementPath, Imports } from '../../../core/shared/project-file-types'
import {
  EditorState,
  EditorStatePatch,
  forUnderlyingTargetFromEditorState,
  insertElementAtPath_DEPRECATED,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { InsertionSubjectWrapper } from '../../editor/editor-modes'
import { assertNever } from '../../../core/shared/utils'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { absolute } from '../../../utils/utils'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import { ProjectContentTreeRoot } from '../../assets'
import { JSXAttributesPart, JSXAttributesEntry } from '../../../core/shared/element-template'
import { getIndexInParent } from '../../../core/model/element-template-utils'
import { childInsertionPath } from '../../editor/store/insertion-path'
import { jsxTextBlock } from '../../../core/shared/element-template'

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

      const { wrapper, imports } = getInsertionSubjectWrapper(
        command.wrapper,
        command.wrapperUID,
        elementToWrap,
        editor.projectContents,
      )

      // Insert the wrapper at the initial index
      const targetParent = EP.parentPath(command.target)
      const insertionResult = insertElementAtPath_DEPRECATED(
        editor.projectContents,
        underlyingFilePath,
        childInsertionPath(targetParent),
        wrapper,
        withElementRemoved,
        index,
      )

      editorStatePatches.push(
        getPatchForComponentChange(
          success.topLevelElements,
          insertionResult.components,
          mergeImports(
            underlyingFilePath,
            success.imports,
            mergeImports(underlyingFilePath, imports, insertionResult.importsToAdd),
          ),
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
): {
  wrapper: JSXElementChild
  imports: Imports
} => {
  switch (insertionSubjectWrapper) {
    case 'conditional':
      return {
        wrapper: jsxConditionalExpression(
          wrapperUID,
          jsExpressionValue(true, emptyComments),
          'true',
          elementToWrap,
          getInsertionSubjectWrapperConditionalFalseBranch(projectContents, elementToWrap),
          emptyComments,
        ),
        imports: {},
      }
    case 'fragment':
      return {
        wrapper: jsxFragment(wrapperUID, [elementToWrap], true),
        imports: {
          react: {
            importedAs: 'React',
            importedFromWithin: [],
            importedWithName: null,
          },
        },
      }
    default:
      assertNever(insertionSubjectWrapper)
  }
}

const defaultFalseBranchSideLength = 100
const defaultFalseBranchText = 'False branch'

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

  function getNumberProp(e: JSXAttributesEntry, key: string): number | null {
    if (e.value.type !== 'ATTRIBUTE_VALUE') {
      return null
    }
    const value = e.value.value[key]
    if (typeof value !== 'number') {
      return null
    }
    return value
  }

  const uid = generateUidWithExistingComponents(projectContents)
  const trueBranchStyle = getStyle(trueBranch)

  return jsxElement(
    'div',
    uid,
    jsxAttributesFromMap({
      style: jsExpressionValue(
        {
          position: 'absolute',
          left: getNumberProp(trueBranchStyle, 'left') ?? 0,
          top: getNumberProp(trueBranchStyle, 'top') ?? 0,
          width: Math.max(
            getNumberProp(trueBranchStyle, 'width') ?? 0,
            defaultFalseBranchSideLength,
          ),
          height: Math.max(
            getNumberProp(trueBranchStyle, 'height') ?? 0,
            defaultFalseBranchSideLength,
          ),
        },
        emptyComments,
      ),
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [jsxTextBlock(defaultFalseBranchText)],
  )
}
