import {
  emptyComments,
  isJSXAttributesEntry,
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
  insertElementAtPath,
  removeElementAtPath,
} from '../../editor/store/editor-state'
import { BaseCommand, CommandFunction, getPatchForComponentChange, WhenToRun } from './commands'
import * as EP from '../../../core/shared/element-path'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { InsertionSubjectWrapper } from '../../editor/editor-modes'
import { assertNever } from '../../../core/shared/utils'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { absolute } from '../../../utils/utils'
import {
  generateUidWithExistingComponents,
  getAllUniqueUids,
} from '../../../core/model/element-template-utils'
import { ProjectContentTreeRoot } from '../../assets'
import { JSXAttributesEntry } from '../../../core/shared/element-template'
import { getIndexInParent } from '../../../core/model/element-template-utils'
import { getInsertionPathWithSlotBehavior } from '../../editor/store/insertion-path'
import { jsxTextBlock } from '../../../core/shared/element-template'
import { CSSProperties } from 'react'
import { Property } from 'csstype'
import { generateConsistentUID } from '../../../core/shared/uid-utils'

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

      const insertionPath = getInsertionPathWithSlotBehavior(
        targetParent,
        editor.projectContents,
        editor.nodeModules.files,
        editor.canvas.openFile?.filename,
        editor.jsxMetadata,
      )
      if (insertionPath == null) {
        return // maybe this should throw instead?
      }

      const insertionResult = insertElementAtPath(
        editor.projectContents,
        insertionPath,
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

function getAttributesEntryProp<T>(e: JSXAttributesEntry, key: string): T | null {
  if (e.value.type !== 'ATTRIBUTE_VALUE') {
    return null
  }
  return e.value.value[key]
}

function getStyleAttributesEntry(element: JSXElementChild): JSXAttributesEntry {
  const emptyStyle: JSXAttributesEntry = {
    type: 'JSX_ATTRIBUTES_ENTRY',
    key: 'style',
    value: jsExpressionValue({}, emptyComments),
    comments: emptyComments,
  }

  if (element.type !== 'JSX_ELEMENT') {
    return emptyStyle
  }
  const found = element.props.find((p) => isJSXAttributesEntry(p) && p.key === 'style')
  if (found == null || found.type !== 'JSX_ATTRIBUTES_ENTRY') {
    return emptyStyle
  }
  return found
}

function getInsertionSubjectWrapperConditionalFalseBranch(
  projectContents: ProjectContentTreeRoot,
  trueBranch: JSXElementChild,
): JSXElementChild {
  const uid = generateConsistentUID(new Set(getAllUniqueUids(projectContents)), 'false-branch')
  const trueBranchStyle = getStyleAttributesEntry(trueBranch)

  let style: CSSProperties = {}
  const position = getAttributesEntryProp<Property.Position>(trueBranchStyle, 'position')
  if (position != null) {
    style.position = position
    if (style.position != null) {
      style.left = getAttributesEntryProp<number>(trueBranchStyle, 'left') ?? 0
      style.top = getAttributesEntryProp<number>(trueBranchStyle, 'top') ?? 0
    }
  }
  style.width = Math.max(
    getAttributesEntryProp<number>(trueBranchStyle, 'width') ?? 0,
    defaultFalseBranchSideLength,
  )
  style.height = Math.max(
    getAttributesEntryProp<number>(trueBranchStyle, 'height') ?? 0,
    defaultFalseBranchSideLength,
  )

  return jsxElement(
    'div',
    uid,
    jsxAttributesFromMap({
      style: jsExpressionValue(style, emptyComments),
      'data-uid': jsExpressionValue(uid, emptyComments),
    }),
    [jsxTextBlock(defaultFalseBranchText)],
  )
}
