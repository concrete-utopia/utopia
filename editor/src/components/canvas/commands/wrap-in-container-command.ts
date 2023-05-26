import {
  emptyComments,
  isJSXElement,
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
import * as PP from '../../../core/shared/property-path'
import { getUtopiaJSXComponentsFromSuccess } from '../../../core/model/project-file-utils'
import { InsertionSubjectWrapper } from '../../editor/editor-modes'
import { assertNever } from '../../../core/shared/utils'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { absolute } from '../../../utils/utils'
import { getAllUniqueUids } from '../../../core/model/element-template-utils'
import { ProjectContentTreeRoot } from '../../assets'
import { getIndexInParent } from '../../../core/model/element-template-utils'
import { getInsertionPathWithSlotBehavior } from '../../editor/store/insertion-path'
import { jsxTextBlock } from '../../../core/shared/element-template'
import { CSSProperties } from 'react'
import { Property } from 'csstype'
import { generateConsistentUID } from '../../../core/shared/uid-utils'
import { getSimpleAttributeAtPath } from '../../../core/model/element-metadata-utils'
import { forEachRight, right } from '../../../core/shared/either'
import { wait } from 'src/utils/utils.test-utils'

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

function getInsertionSubjectStyleFromConditionalTrueBranch(
  trueBranch: JSXElementChild,
): CSSProperties {
  // Get the various properties that make up the `style` property.
  let position: Property.Position | null = null
  let left: number | null = null
  let top: number | null = null
  let width: number = 0
  let height: number = 0
  if (isJSXElement(trueBranch)) {
    const positionProperty = getSimpleAttributeAtPath(
      right(trueBranch.props),
      PP.create('style', 'position'),
    )
    forEachRight(positionProperty, (value) => {
      position = value
    })
    const leftProperty = getSimpleAttributeAtPath(
      right(trueBranch.props),
      PP.create('style', 'left'),
    )
    forEachRight(leftProperty, (value) => {
      left = value
    })
    const topProperty = getSimpleAttributeAtPath(right(trueBranch.props), PP.create('style', 'top'))
    forEachRight(topProperty, (value) => {
      top = value
    })
    const widthProperty = getSimpleAttributeAtPath(
      right(trueBranch.props),
      PP.create('style', 'width'),
    )
    forEachRight(widthProperty, (value) => {
      width = value
    })
    const heightProperty = getSimpleAttributeAtPath(
      right(trueBranch.props),
      PP.create('style', 'height'),
    )
    forEachRight(heightProperty, (value) => {
      height = value
    })
  }
  width = Math.max(width, defaultFalseBranchSideLength)
  height = Math.max(height, defaultFalseBranchSideLength)

  // Build the style property from the properties.
  const style: CSSProperties = {}
  if (position != null) {
    style.position = position
    if (left != null) {
      style.left = left
    }
    if (top != null) {
      style.top = top
    }
  }
  style.width = width
  style.height = height
  return style
}

function getInsertionSubjectWrapperConditionalFalseBranch(
  projectContents: ProjectContentTreeRoot,
  trueBranch: JSXElementChild,
): JSXElementChild {
  const uid = generateConsistentUID(
    new Set(getAllUniqueUids(projectContents).uniqueIDs),
    'false-branch',
  )

  const style = getInsertionSubjectStyleFromConditionalTrueBranch(trueBranch)

  // Construct the element.
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
