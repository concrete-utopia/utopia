import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
  maybeBranchConditionalCase,
} from '../../../../../core/model/conditionals'
import {
  MetadataUtils,
  getSimpleAttributeAtPath,
} from '../../../../../core/model/element-metadata-utils'
import type { Either } from '../../../../../core/shared/either'
import { foldEither, isLeft, left, right } from '../../../../../core/shared/either'
import * as EP from '../../../../../core/shared/element-path'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
  JSXElementChild,
  JSXFragment,
} from '../../../../../core/shared/element-template'
import {
  elementReferencesElsewhere,
  getElementReferencesElsewherePathsFromProps,
  emptyComments,
  isJSXElement,
  jsExpressionValue,
  jsxElementNameEquals,
} from '../../../../../core/shared/element-template'
import type { ElementPath, PropertyPath } from '../../../../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../../../../assets'
import type {
  AllElementProps,
  EditorState,
  ElementProps,
} from '../../../../editor/store/editor-state'
import type { InsertionPath } from '../../../../editor/store/insertion-path'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  replaceWithSingleElement,
} from '../../../../editor/store/insertion-path'
import { CSSCursor } from '../../../canvas-types'
import { setCursorCommand } from '../../../commands/set-cursor-command'
import type { InteractionCanvasState, StrategyApplicationResult } from '../../canvas-strategy-types'
import { strategyApplicationResult } from '../../canvas-strategy-types'
import * as PP from '../../../../../core/shared/property-path'
import type { ValueAtPath } from '../../../../../core/shared/jsx-attributes'
import { setJSXValuesAtPaths } from '../../../../../core/shared/jsx-attributes'
import type {
  ElementPasteWithMetadata,
  ReparentTargetForPaste,
} from '../../../../../utils/clipboard'
import type { ElementPaste } from '../../../../editor/action-types'
import {
  eitherRight,
  fromField,
  traverseArray,
} from '../../../../../core/shared/optics/optic-creators'
import { modify, set } from '../../../../../core/shared/optics/optic-utilities'
import type { IndexPosition } from '../../../../../utils/utils'
import Utils from '../../../../../utils/utils'
import type { CanvasPoint } from '../../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  isInfinityRectangle,
  zeroCanvasPoint,
  offsetPoint,
  canvasPoint,
  roundTo,
  zeroCanvasRect,
  zeroRectangle,
  zeroRectIfNullOrInfinity,
  roundPointToNearestHalf,
  roundPointTo,
} from '../../../../../core/shared/math-utils'
import type { MetadataSnapshots } from './reparent-property-strategies'
import type { BuiltInDependencies } from '../../../../../core/es-modules/package-manager/built-in-dependencies-list'
import type { ElementPathTrees } from '../../../../../core/shared/element-path-tree'
import type { CanvasCommand } from '../../../commands/commands'
import type { ToReparent } from '../reparent-utils'
import type { StaticReparentTarget } from './reparent-strategy-helpers'
import { mapDropNulls } from '../../../../../core/shared/array-utils'
import { treatElementAsFragmentLike } from '../fragment-like-helpers'
import { optionalMap } from '../../../../../core/shared/optional-utils'
import { setProperty } from '../../../commands/set-property-command'

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
        (elementFromMetadata) =>
          !elementReferencesElsewhere(elementFromMetadata) &&
          MetadataUtils.targetHonoursPropsPosition(projectContents, metadata),
        metadata.element,
      )
    }
  }
}

export function isAllowedToNavigatorReparent(
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
        (elementFromMetadata) =>
          MetadataUtils.targetHonoursPropsPosition(projectContents, metadata),
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
  const valuesAndPaths = collectValuesAtPathToReplace(elementProps, element)

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

export function collectValuesAtPathToReplace(
  elementProps: ElementProps,
  element: JSXElementChild,
): Array<ValueAtPath> {
  // gather property paths that are defined elsewhere
  const paths = getElementReferencesElsewherePathsFromProps(element, PP.create())

  // try and get the values from allElementProps, replace everything else with undefined
  return paths.map((propertyPath) => ({
    path: propertyPath,
    value: jsExpressionValue(Utils.path(PP.getElements(propertyPath), elementProps), emptyComments),
  }))
}

export function getReplacePropsWithRuntimeValuesCommands(
  elementProps: ElementProps,
  element: JSXElementChild,
  path: ElementPath,
): Array<CanvasCommand> {
  return collectValuesAtPathToReplace(elementProps, element).map((valueAtPath) =>
    setProperty(
      'always',
      path,
      valueAtPath.path as any,
      Utils.path(PP.getElements(valueAtPath.path), elementProps) as any,
    ),
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
  copyData: ElementPasteWithMetadata,
  allElementProps: AllElementProps,
): {
  copyDataReplaced: ElementPasteWithMetadata
  replacePropCommands: Array<CanvasCommand>
} | null {
  if (!copyData.elements.some((e) => elementReferencesElsewhere(e.element))) {
    return null
  }

  let workingMetadata = copyData.targetOriginalContextMetadata
  let updatedElements: Array<ElementPaste> = []
  let setPropCommands: Array<CanvasCommand> = []

  /**
   * This function only traverses the children array, it doesn't reach element that are generated (for example from `.map` calls)
   */

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
      setPropCommands.push(...getReplacePropsWithRuntimeValuesCommands(props, element, elementPath))

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
    copyDataReplaced: {
      elements: updatedElements,
      targetOriginalContextMetadata: workingMetadata,
    },
    replacePropCommands: setPropCommands,
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
  return conditionalClauseInsertionPath(newParent, clause, replaceWithSingleElement())
}

function areElementsInstancesOfTheSameComponent(
  firstInstance: ElementInstanceMetadata | null,
  secondInstance: ElementInstanceMetadata | null,
): boolean {
  if (
    firstInstance == null ||
    secondInstance == null ||
    isLeft(firstInstance.element) ||
    isLeft(secondInstance.element) ||
    !isJSXElement(firstInstance.element.value) ||
    !isJSXElement(secondInstance.element.value)
  ) {
    return false
  }

  return jsxElementNameEquals(firstInstance.element.value.name, secondInstance.element.value.name)
}

export function isElementRenderedBySameComponent(
  metadata: ElementInstanceMetadataMap,
  targetPath: ElementPath,
  instance: ElementInstanceMetadata | null,
): boolean {
  if (EP.isEmptyPath(targetPath)) {
    return false
  }

  const currentInstance = MetadataUtils.findElementByElementPath(
    metadata,
    EP.getContainingComponent(targetPath),
  )

  return (
    areElementsInstancesOfTheSameComponent(currentInstance, instance) ||
    isElementRenderedBySameComponent(metadata, EP.getContainingComponent(targetPath), instance)
  )
}

export function offsetPositionInPasteBoundingBox(
  originalElementPath: ElementPath,
  allOriginalElementPathsToPaste: Array<ElementPath>,
  originalMetadata: ElementInstanceMetadataMap,
): CanvasPoint {
  const copiedElementsBoundingBox = boundingRectangleArray(
    allOriginalElementPathsToPaste.map((path) =>
      MetadataUtils.getFrameOrZeroRectInCanvasCoords(path, originalMetadata),
    ),
  )

  const frame = MetadataUtils.getFrameOrZeroRectInCanvasCoords(
    originalElementPath,
    originalMetadata,
  )
  return copiedElementsBoundingBox != null
    ? canvasPoint({
        x: frame.x - copiedElementsBoundingBox.x,
        y: frame.y - copiedElementsBoundingBox.y,
      })
    : zeroCanvasPoint
}

export function absolutePositionForReparent(
  reparentedElementPath: ElementPath,
  allElementPathsToReparent: Array<ElementPath>,
  targetParent: ElementPath,
  metadata: MetadataSnapshots,
  allElementProps: AllElementProps,
  elementPathTrees: ElementPathTrees,
  canvasViewportCenter: CanvasPoint | 'keep-visible-position',
): CanvasPoint {
  const boundingBox = boundingRectangleArray(
    mapDropNulls((path) => {
      const globalFrame = MetadataUtils.getFrameInCanvasCoords(
        path,
        metadata.originalTargetMetadata,
      )
      if (globalFrame == null || isInfinityRectangle(globalFrame)) {
        return null
      }
      return globalFrame
    }, allElementPathsToReparent),
  )

  // when pasting multiselected elements let's keep their relative position to each other
  const multiselectOffset = offsetPositionInPasteBoundingBox(
    reparentedElementPath,
    allElementPathsToReparent,
    metadata.originalTargetMetadata,
  )

  if (boundingBox == null || isInfinityRectangle(boundingBox)) {
    return zeroCanvasPoint // fallback
  }

  if (EP.isStoryboardPath(targetParent)) {
    if (canvasViewportCenter === 'keep-visible-position') {
      return roundPointTo(offsetPoint(boundingBox, multiselectOffset), 0)
    } else {
      return roundPointTo(
        offsetPoint(
          canvasPoint({
            x: canvasViewportCenter.x - boundingBox.width / 2,
            y: canvasViewportCenter.y - boundingBox.height / 2,
          }),
          multiselectOffset,
        ),
        0,
      )
    }
  }

  const targetParentBounds = MetadataUtils.getFrameInCanvasCoords(
    targetParent,
    metadata.currentMetadata,
  )

  if (targetParentBounds == null || isInfinityRectangle(targetParentBounds)) {
    return roundPointTo(multiselectOffset, 0) // fallback
  }

  const deltaX = boundingBox.x - targetParentBounds.x
  const deltaY = boundingBox.y - targetParentBounds.y

  const elementInBoundsHorizontally = 0 <= deltaX && deltaX <= targetParentBounds.width
  const elementInBoundsVertically = 0 <= deltaY && deltaY <= targetParentBounds.height

  const horizontalCenter = roundTo((targetParentBounds.width - boundingBox.width) / 2, 0)
  const verticalCenter = roundTo((targetParentBounds.height - boundingBox.height) / 2, 0)

  const horizontalOffset = elementInBoundsHorizontally ? deltaX : horizontalCenter
  const verticalOffset = elementInBoundsVertically ? deltaY : verticalCenter

  const elementOffset = offsetPoint(
    canvasPoint({
      x: horizontalOffset,
      y: verticalOffset,
    }),
    multiselectOffset,
  )

  const isElementFragmentLike = treatElementAsFragmentLike(
    metadata.currentMetadata,
    allElementProps,
    elementPathTrees,
    targetParent,
  )

  if (!isElementFragmentLike) {
    return roundPointTo(elementOffset, 0)
  }

  const localFrame = zeroRectIfNullOrInfinity(
    MetadataUtils.findElementByElementPath(metadata.currentMetadata, targetParent)?.localFrame ??
      null,
  )

  // offset the element with the target parent's offset, since the target parent doesn't
  // provide bounds for absolute positioning
  return roundPointTo(
    offsetPoint(
      elementOffset,

      canvasPoint({
        x: localFrame.x,
        y: localFrame.y,
      }),
    ),
    0,
  )
}

export function absolutePositionForPaste(
  target: ReparentTargetForPaste,
  reparentedElementPath: ElementPath,
  allElementPathsToReparent: Array<ElementPath>,
  metadata: MetadataSnapshots,
  allElementProps: AllElementProps,
  elementPathTrees: ElementPathTrees,
  canvasViewportCenter: CanvasPoint,
): CanvasPoint {
  if (target.type === 'parent') {
    return absolutePositionForReparent(
      reparentedElementPath,
      allElementPathsToReparent,
      target.parentPath.intendedParentPath,
      metadata,
      allElementProps,
      elementPathTrees,
      canvasViewportCenter,
    )
  }

  const siblingBounds = MetadataUtils.getFrameInCanvasCoords(
    target.siblingPath,
    metadata.currentMetadata,
  )

  const parentBounds = EP.isStoryboardPath(target.parentPath.intendedParentPath)
    ? zeroCanvasRect
    : MetadataUtils.getFrameInCanvasCoords(
        target.parentPath.intendedParentPath,
        metadata.currentMetadata,
      )

  if (
    siblingBounds == null ||
    parentBounds == null ||
    isInfinityRectangle(siblingBounds) ||
    isInfinityRectangle(parentBounds)
  ) {
    return zeroCanvasPoint
  }

  return canvasPoint({
    x: siblingBounds.x - parentBounds.x + siblingBounds.width + 10,
    y: siblingBounds.y - parentBounds.y,
  })
}
