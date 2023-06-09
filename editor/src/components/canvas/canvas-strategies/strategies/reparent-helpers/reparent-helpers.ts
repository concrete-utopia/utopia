import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
  maybeBranchConditionalCase,
} from '../../../../../core/model/conditionals'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { Either, foldEither, left, right } from '../../../../../core/shared/either'
import * as EP from '../../../../../core/shared/element-path'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
  JSXElementChild,
  JSXFragment,
  elementReferencesElsewhere,
  getElementReferencesElsewherePathsFromProps,
  emptyComments,
  isJSXElement,
  jsExpressionValue,
} from '../../../../../core/shared/element-template'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../../../assets'
import { AllElementProps, EditorState, ElementProps } from '../../../../editor/store/editor-state'
import {
  InsertionPath,
  childInsertionPath,
  conditionalClauseInsertionPath,
} from '../../../../editor/store/insertion-path'
import { CSSCursor } from '../../../canvas-types'
import { setCursorCommand } from '../../../commands/set-cursor-command'
import {
  InteractionCanvasState,
  StrategyApplicationResult,
  strategyApplicationResult,
} from '../../canvas-strategy-types'
import * as ObjectPath from 'object-path'
import * as PP from '../../../../../core/shared/property-path'
import { setJSXValuesAtPaths } from '../../../../../core/shared/jsx-attributes'
import { JSXElementCopyData } from '../../../../../utils/clipboard'
import { ElementPaste } from '../../../../editor/action-types'
import {
  eitherRight,
  fromField,
  traverseArray,
} from '../../../../../core/shared/optics/optic-creators'
import { modify, set } from '../../../../../core/shared/optics/optic-utilities'

export function isAllowedToReparent(
  projectContents: ProjectContentTreeRoot,
  startingMetadata: ElementInstanceMetadataMap,
  target: ElementPath,
): boolean {
  if (MetadataUtils.isElementGenerated(target)) {
    return false
  } else {
    const metadata = MetadataUtils.findElementByElementPath(startingMetadata, target)
    if (metadata == null) {
      const parentPath = EP.parentPath(target)
      const conditional = findMaybeConditionalExpression(parentPath, startingMetadata)
      if (conditional != null) {
        return maybeBranchConditionalCase(parentPath, conditional, target) != null
      }
      return false
    } else {
      return foldEither(
        (_) => true,
        (elementFromMetadata) => {
          return (
            !elementReferencesElsewhere(elementFromMetadata) &&
            MetadataUtils.targetHonoursPropsPosition(projectContents, metadata)
          )
        },
        metadata.element,
      )
    }
  }
}

export function canCopyElement(
  editor: EditorState,
  target: ElementPath,
): Either<string, ElementPath> {
  const metadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
  if (MetadataUtils.isElementGenerated(target)) {
    return left('Cannot copy generated element')
  }

  if (metadata == null) {
    const parentPath = EP.parentPath(target)
    const conditional = findMaybeConditionalExpression(parentPath, editor.jsxMetadata)
    if (conditional != null) {
      const branchCase = maybeBranchConditionalCase(parentPath, conditional, target)
      if (branchCase == null) {
        return left('Cannot copy empty branch')
      }
      return right(target)
    }
    return left('Cannot find element metadata')
  }

  return right(target)
}

export function replacePropsWithRuntimeValues<T extends JSXElementChild>(
  elementProps: ElementProps,
  element: T,
): T {
  if (!isJSXElement(element)) {
    return element
  }

  // gather property paths that are defined elsewhere
  const paths = getElementReferencesElsewherePathsFromProps(element, PP.create())

  // try and get the values from allElementProps, replace everything else with undefined
  const valuesAndPaths = paths.map((propertyPath) => ({
    path: propertyPath,
    value: jsExpressionValue(
      ObjectPath.get(elementProps, PP.getElements(propertyPath)),
      emptyComments,
    ),
  }))

  return foldEither(
    () => {
      return element
    },
    (updatedProps) => {
      return {
        ...element,
        props: updatedProps,
      }
    },
    setJSXValuesAtPaths(element.props, valuesAndPaths),
  )
}

export function ifAllowedToReparent(
  canvasState: InteractionCanvasState,
  startingMetadata: ElementInstanceMetadataMap,
  targets: Array<ElementPath>,
  ifAllowed: () => StrategyApplicationResult,
): StrategyApplicationResult {
  const allowed = targets.every((target) => {
    return isAllowedToReparent(canvasState.projectContents, startingMetadata, target)
  })
  if (allowed) {
    return ifAllowed()
  } else {
    return strategyApplicationResult([setCursorCommand(CSSCursor.NotPermitted)], {}, 'failure')
  }
}

export function replaceJSXElementCopyData(
  copyData: JSXElementCopyData,
  allElementProps: AllElementProps,
): JSXElementCopyData {
  let workingMetadata = copyData.targetOriginalContextMetadata
  let updatedElements: Array<ElementPaste> = []

  function replaceJSXElementChild(
    elementPath: ElementPath,
    element: JSXElementChild,
  ): JSXElementChild {
    if (element.type === 'JSX_ELEMENT') {
      const pathString = EP.toString(elementPath)
      const instance = workingMetadata[pathString]
      if (instance == null) {
        return element
      }
      const props = allElementProps[pathString]
      const updatedElement = props == null ? element : replacePropsWithRuntimeValues(props, element)

      workingMetadata[pathString] = set<ElementInstanceMetadata, JSXElementChild>(
        fromField<ElementInstanceMetadata, 'element'>('element').compose(eitherRight()),
        updatedElement,
        instance,
      )

      return modify<JSXElement, JSXElementChild>(
        fromField<JSXElement, 'children'>('children').compose(traverseArray()),
        (e) => replaceJSXElementChild(EP.appendToPath(elementPath, e.uid), e),
        updatedElement,
      )
    } else if (element.type === 'JSX_FRAGMENT') {
      return modify<JSXFragment, JSXElementChild>(
        fromField<JSXFragment, 'children'>('children').compose(traverseArray()),
        (e) => replaceJSXElementChild(EP.appendToPath(elementPath, e.uid), e),
        element,
      )
    } else if (element.type === 'JSX_CONDITIONAL_EXPRESSION') {
      return {
        ...element,
        whenTrue: replaceJSXElementChild(
          EP.appendToPath(elementPath, element.whenTrue.uid),
          element.whenTrue,
        ),
        whenFalse: replaceJSXElementChild(
          EP.appendToPath(elementPath, element.whenFalse.uid),
          element.whenFalse,
        ),
      }
    } else {
      return element
    }
  }

  function replaceElementPaste(element: ElementPaste) {
    updatedElements.push({
      ...element,
      element: replaceJSXElementChild(element.originalElementPath, element.element),
    })
  }

  copyData.elements.forEach(replaceElementPaste)

  return {
    type: 'ELEMENT_COPY',
    elements: updatedElements,
    targetOriginalContextMetadata: workingMetadata,
    targetOriginalContextElementPathTrees: copyData.targetOriginalContextElementPathTrees,
  }
}

export function getInsertionPathForReparentTarget(
  newParent: ElementPath,
  metadata: ElementInstanceMetadataMap,
): InsertionPath {
  const conditional = findMaybeConditionalExpression(newParent, metadata)
  if (conditional == null) {
    return childInsertionPath(newParent)
  }
  const clause = getConditionalActiveCase(newParent, conditional, metadata)
  if (clause == null) {
    return childInsertionPath(newParent)
  }
  return conditionalClauseInsertionPath(newParent, clause, 'replace')
}
