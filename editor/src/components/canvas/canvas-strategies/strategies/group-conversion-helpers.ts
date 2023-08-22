import type { CSSProperties } from 'react'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { StyleLayoutProp } from '../../../../core/layout/layout-helpers-new'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import {
  MetadataUtils,
  getZIndexOrderedViewsWithoutDirectChildren,
} from '../../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import {
  removeMarginProperties,
  removePaddingProperties,
} from '../../../../core/model/margin-and-padding'
import { arrayAccumulate, mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft, isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXAttributes,
  JSXConditionalExpression,
  JSXElement,
  JSXElementChild,
  JSXElementLike,
} from '../../../../core/shared/element-template'
import {
  emptyComments,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
  isNullJSXAttributeValue,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxElement,
  jsxElementName,
  jsxFragment,
} from '../../../../core/shared/element-template'
import type {
  CanvasPoint,
  CanvasRectangle,
  LocalPoint,
  LocalRectangle,
  Size,
} from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangleToLocalRectangle,
  canvasVector,
  forceFiniteRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  localRectangle,
  sizeFromRectangle,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import {
  fromField,
  fromTypeGuard,
  traverseArray,
} from '../../../../core/shared/optics/optic-creators'
import { modify } from '../../../../core/shared/optics/optic-utilities'
import { forceNotNull, optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { importAlias } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import { styleStringInArray } from '../../../../utils/common-constants'
import type { Absolute } from '../../../../utils/utils'
import { absolute, back } from '../../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../../assets'
import { notice } from '../../../common/notice'
import type { AddToast, ApplyCommandsAction } from '../../../editor/action-types'
import { applyCommandsAction, showToast } from '../../../editor/actions/action-creators'
import type { AllElementProps, NavigatorEntry } from '../../../editor/store/editor-state'
import {
  trueUpChildrenOfElementChanged,
  trueUpElementChanged,
} from '../../../editor/store/editor-state'
import {
  childInsertionPath,
  commonInsertionPathFromArray,
  replaceWithSingleElement,
} from '../../../editor/store/insertion-path'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import { cssPixelLength, isCSSNumber } from '../../../inspector/common/css-utils'
import {
  isHugFromStyleAttribute,
  nukeAllAbsolutePositioningPropsCommands,
  nukeSizingPropsForAxisCommand,
  setElementTopLeft,
} from '../../../inspector/inspector-common'
import { EdgePositionBottomRight } from '../../canvas-types'
import { addElement } from '../../commands/add-element-command'
import type { CanvasCommand } from '../../commands/commands'
import { deleteElement } from '../../commands/delete-element-command'
import { queueGroupTrueUp } from '../../commands/queue-group-true-up-command'
import type { SetCssLengthProperty } from '../../commands/set-css-length-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
  setValueKeepingOriginalUnit,
} from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import {
  getElementFragmentLikeType,
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  replaceNonDomElementWithFirstDomAncestorPath,
} from './fragment-like-helpers'
import { isEmptyGroup } from './group-helpers'
import type { AbsolutePin } from './resize-helpers'
import { ensureAtLeastTwoPinsForEdgePosition, isHorizontalPin } from './resize-helpers'
import { createMoveCommandsForElement } from './shared-move-strategies-helpers'
import { getConditionalActiveCase } from '../../../../core/model/conditionals'

const GroupImport: Imports = {
  'utopia-api': {
    importedAs: null,
    importedFromWithin: [importAlias('Group')],
    importedWithName: null,
  },
}

export function isAbsolutePositionedFrame(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): boolean {
  return (
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(metadata, elementPath),
    ) &&
    MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath).length > 0 &&
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ).every((childPath) =>
      MetadataUtils.isPositionAbsolute(MetadataUtils.findElementByElementPath(metadata, childPath)),
    )
  )
}

// when removing the parent from between the children and the grandparent
function offsetChildrenByVectorCommands(
  childInstances: Array<ElementInstanceMetadata>,
  offset: CanvasPoint | LocalPoint,
) {
  return childInstances.flatMap((child) =>
    setElementTopLeft(child, {
      top: child.specialSizeMeasurements.offset.y + offset.y,
      left: child.specialSizeMeasurements.offset.x + offset.x,
    }),
  )
}

// when putting new container between the children and the grandparent
function offsetChildrenByDelta(
  childInstances: Array<ElementInstanceMetadata>,
  boundingFrame: CanvasRectangle,
) {
  return childInstances.flatMap((child) =>
    child.globalFrame != null && isFiniteRectangle(child.globalFrame)
      ? setElementTopLeft(child, {
          top: child.globalFrame.y - boundingFrame.y,
          left: child.globalFrame.x - boundingFrame.x,
        })
      : [],
  )
}

export function convertFragmentToSizelessDiv(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(parentPath),
      jsxElement(
        jsxElementName('div', []),
        uid,
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue(uid, emptyComments) }),
        children,
      ),
      {
        indexPosition: absolute(
          MetadataUtils.getIndexInParent(metadata, elementPathTree, elementPath),
        ),
      },
    ),
  ]
}

export function convertFragmentToFrame(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
  convertIfStaticChildren:
    | 'do-not-convert-if-it-has-static-children'
    | 'convert-even-if-it-has-static-children',
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElementLike(element.element.value)) {
    return []
  }

  if (!isJSXFragment(element.element.value)) {
    // not a fragment, nothing to convert!
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  if (
    convertIfStaticChildren === 'do-not-convert-if-it-has-static-children' &&
    childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))
  ) {
    // if any children is not position: absolute, bail out from the conversion
    return []
  }

  const { children, uid } = element.element.value

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return []
  }

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = childrenBoundingFrame.x - parentBounds.x
  const top = childrenBoundingFrame.y - parentBounds.y

  const fragmentIsCurrentlyAbsolute = element.specialSizeMeasurements.position === 'absolute'

  const absoluteTopLeftProps = fragmentIsCurrentlyAbsolute
    ? ({ position: 'absolute', top: top, left: left } as const)
    : ({ contain: 'layout' } as const)

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(parentPath),
      jsxElement(
        jsxElementName('div', []),
        uid,
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue(uid, emptyComments),
          style: jsExpressionValue(
            {
              ...absoluteTopLeftProps,
              width: childrenBoundingFrame.width,
              height: childrenBoundingFrame.height,
            },
            emptyComments,
          ),
        }),
        children,
      ),
      {
        indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      },
    ),
    ...offsetChildrenByDelta(childInstances, childrenBoundingFrame),
  ]
}

export function convertFragmentToGroup(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElementLike(element.element.value)) {
    return []
  }

  if (!isJSXFragment(element.element.value)) {
    // not a fragment, nothing to convert!
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    // if any children is not position: absolute, bail out from the conversion
    return []
  }

  const { children, uid } = element.element.value
  // Remove the margins from the children.
  const toPropsOptic = traverseArray<JSXElementChild>()
    .compose(fromTypeGuard(isJSXElement))
    .compose(fromField('props'))
  const childrenWithoutMargins = modify(toPropsOptic, removeMarginProperties, children)

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return []
  }

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = childrenBoundingFrame.x - parentBounds.x
  const top = childrenBoundingFrame.y - parentBounds.y

  const fragmentIsCurrentlyAbsolute = element.specialSizeMeasurements.position === 'absolute'

  const absoluteTopLeftProps = fragmentIsCurrentlyAbsolute
    ? ({ position: 'absolute', top: top, left: left } as const)
    : ({ contain: 'layout' } as const)

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(parentPath),
      jsxElement(
        'Group',
        uid,
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue(uid, emptyComments),
          style: jsExpressionValue(
            {
              ...absoluteTopLeftProps,
              width: childrenBoundingFrame.width,
              height: childrenBoundingFrame.height,
            },
            emptyComments,
          ),
        }),
        childrenWithoutMargins,
      ),
      {
        indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
        importsToAdd: GroupImport,
      },
    ),
    ...offsetChildrenByDelta(childInstances, childrenBoundingFrame),
  ]
}

export function convertGroupToFragment(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), jsxFragment(uid, children, true), {
      indexPosition: absolute(
        MetadataUtils.getIndexInParent(metadata, elementPathTree, elementPath),
      ),
      importsToAdd: {
        react: {
          importedAs: 'React',
          importedFromWithin: [],
          importedWithName: null,
        },
      },
    }),
  ]
}

export function convertSizelessDivToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): CanvasCommand[] | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return null
  }

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = childrenBoundingFrame.x - parentBounds.x
  const top = childrenBoundingFrame.y - parentBounds.y

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  return [
    setProperty('always', elementPath, PP.create('style', 'position'), 'absolute'),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'top'),
      setExplicitCssValue(cssPixelLength(top)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'left'),
      setExplicitCssValue(cssPixelLength(left)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'width'),
      setExplicitCssValue(cssPixelLength(childrenBoundingFrame.width)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'height'),
      setExplicitCssValue(cssPixelLength(childrenBoundingFrame.height)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    ...offsetChildrenByDelta(childInstances, childrenBoundingFrame),
  ]
}

export function convertFrameToSizelessDivCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentOffset =
    MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements.offset ??
    zeroCanvasPoint

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  return [
    ...nukeAllAbsolutePositioningPropsCommands(elementPath),
    nukeSizingPropsForAxisCommand('vertical', elementPath),
    nukeSizingPropsForAxisCommand('horizontal', elementPath),
    ...offsetChildrenByVectorCommands(childInstances, parentOffset),
  ]
}

export function convertFrameToFragmentCommands(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  // if any children is not position: absolute, bail out from the conversion
  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    return []
  }

  const { children, uid } = instance.element.value

  const parentOffset =
    MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements.offset ??
    zeroCanvasPoint

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), jsxFragment(uid, children, true), {
      indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      importsToAdd: {
        react: {
          importedAs: 'React',
          importedFromWithin: [],
          importedWithName: null,
        },
      },
    }),
    ...offsetChildrenByVectorCommands(childInstances, parentOffset),
  ]
}

export function convertFrameToGroup(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElement(instance.element.value)) {
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  if (childInstances.length === 0) {
    // if the Frame has no children, it cannot become a Group
    return []
  }

  // if any children is not position: absolute, bail out from the conversion
  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    return []
  }

  const { children, uid, props } = instance.element.value
  // Remove the padding properties from the parent.
  const propsWithoutPadding = removePaddingProperties(props)
  // Remove the margin properties for the children of the newly created group.
  const toPropsOptic = traverseArray<JSXElementChild>()
    .compose(fromTypeGuard(isJSXElement))
    .compose(fromField('props'))
  const childrenWithoutMargins = modify(toPropsOptic, removeMarginProperties, children)
  const elementToAdd = jsxElement('Group', uid, propsWithoutPadding, childrenWithoutMargins)

  // Any margin may result in a shift when that margin is gone, so this shifts
  // in the opposite direction.
  const moveChildrenCommands = arrayAccumulate<CanvasCommand>((workingArray) => {
    for (const child of childrenWithoutMargins) {
      const targetPath = EP.appendToPath(elementPath, child.uid)
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, targetPath)
      if (elementMetadata != null) {
        const globalFrame = elementMetadata.globalFrame
        const localFrame = elementMetadata.localFrame
        const canvasMargin = elementMetadata.specialSizeMeasurements.margin
        if (
          (globalFrame == null || isFiniteRectangle(globalFrame)) &&
          (localFrame == null || isFiniteRectangle(localFrame))
        ) {
          const shiftLeftBy = canvasMargin.left ?? 0
          const shiftTopBy = canvasMargin.top ?? 0
          if (isRight(elementMetadata.element) && isJSXElement(elementMetadata.element.value)) {
            workingArray.push(
              ...createMoveCommandsForElement(
                elementMetadata.element.value,
                targetPath,
                targetPath,
                canvasVector({ x: shiftLeftBy, y: shiftTopBy }),
                localFrame,
                globalFrame,
                elementMetadata.specialSizeMeasurements.immediateParentBounds,
                elementMetadata.specialSizeMeasurements.parentFlexDirection,
              ).commands,
            )
          }
        }
      }
    }
  })

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), elementToAdd, {
      indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      importsToAdd: GroupImport,
    }),
    ...moveChildrenCommands,
    queueGroupTrueUp([trueUpChildrenOfElementChanged(elementPath)]),
  ]
}

export function convertGroupToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElement(instance.element.value)) {
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  // if any children is not position: absolute, bail out from the conversion
  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    return []
  }

  const { children, uid, props } = instance.element.value

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), jsxElement('div', uid, props, children), {
      indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
    }),
  ]
}

export type GroupChildElement = JSXElementLike | JSXConditionalExpression

function elementCanBeAGroupChild(element: JSXElementChild): element is GroupChildElement {
  return (
    isJSXElementLike(element) ||
    (isJSXConditionalExpression(element) &&
      !isNullJSXAttributeValue(element.whenTrue) && // do not allow grouping zero-sized conditionals
      !isNullJSXAttributeValue(element.whenFalse))
  )
}

export function groupConversionCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> | null {
  const fragmentLikeType = getElementFragmentLikeType(
    metadata,
    allElementProps,
    pathTrees,
    elementPath,
  )

  if (fragmentLikeType === 'fragment' || fragmentLikeType === 'conditional') {
    return null
  }

  if (fragmentLikeType === 'sizeless-div') {
    const convertCommands = convertSizelessDivToFrameCommands(
      metadata,
      allElementProps,
      pathTrees,
      elementPath,
    )
    if (convertCommands != null) {
      return convertCommands
    }
  }

  return null
}

export function createWrapInGroupActions(
  selectedViews: Array<ElementPath>,
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTrees: ElementPathTrees,
  navigatorTargets: Array<NavigatorEntry>,
): ApplyCommandsAction | AddToast {
  // this arkane knowledge of the ancients came from WRAP_IN_ELEMENT
  const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
    selectedViews,
    navigatorTargets,
  )

  // first, figure out the common ancestor
  const parentPath = commonInsertionPathFromArray(
    metadata,
    orderedActionTargets.map((actionTarget) => {
      return MetadataUtils.getReparentTargetOfTarget(metadata, actionTarget)
    }),
    replaceWithSingleElement(),
  )

  if (parentPath == null) {
    return showToast(notice('Wrap in Group failed: Could not find common parent.', 'ERROR'))
  }

  // if any target is a root element, refuse wrapping and show toast!
  const anyTargetIsARootElement = orderedActionTargets.some(EP.isRootElementOfInstance)
  if (anyTargetIsARootElement) {
    return showToast(notice("Root elements can't be wrapped into other elements", 'ERROR'))
  }

  const allTargetsCanBeGroupChildren = orderedActionTargets.every((path) => {
    return elementCanBeAGroupChild(MetadataUtils.getJsxElementChildFromMetadata(metadata, path))
  })
  if (!allTargetsCanBeGroupChildren) {
    return showToast(notice('Not all targets can be wrapped into a Group', 'ERROR'))
  }

  const anyTargetIsAnEmptyGroup = orderedActionTargets.some((e) => isEmptyGroup(metadata, e))
  if (anyTargetIsAnEmptyGroup) {
    return showToast(notice('Empty Groups cannot be wrapped into Groups', 'ERROR'))
  }

  // TODO if any target doesn't honour the size or offset prop, refuse wrapping and show toast!

  const globalBoundingBoxOfAllElementsToBeWrapped: CanvasRectangle = forceNotNull(
    'boundingRectangleArray was somehow null!',
    boundingRectangleArray(
      orderedActionTargets.map((p) => MetadataUtils.getFrameOrZeroRectInCanvasCoords(p, metadata)),
    ),
  )

  const newLocalRectangleForGroup: LocalRectangle = forceNotNull(
    'groupLocalRect was somehow null!',
    MetadataUtils.getFrameRelativeToTargetContainingBlock(
      parentPath.intendedParentPath,
      metadata,
      globalBoundingBoxOfAllElementsToBeWrapped,
    ),
  )

  const elementPropsOptic = fromTypeGuard<JSXElementLike, JSXElement>(isJSXElement).compose(
    fromField('props'),
  )

  function removeMarginsFromGroupChildElement(
    element: GroupChildElement,
    elementMetadata: ElementInstanceMetadata,
  ) {
    switch (element.type) {
      case 'JSX_ELEMENT':
      case 'JSX_FRAGMENT':
        // Remove any margin properties from the child.
        const elementLike = modify(elementPropsOptic, removeMarginProperties, element)
        return {
          element: elementLike,
          metadata: elementMetadata,
        }

      case 'JSX_CONDITIONAL_EXPRESSION':
        // Remove any margin properties from the branches.
        let conditional: JSXConditionalExpression = { ...element }
        if (isJSXElementLike(conditional.whenTrue)) {
          conditional.whenTrue = modify(
            elementPropsOptic,
            removeMarginProperties,
            conditional.whenTrue,
          )
        }
        if (isJSXElementLike(conditional.whenFalse)) {
          conditional.whenFalse = modify(
            elementPropsOptic,
            removeMarginProperties,
            conditional.whenFalse,
          )
        }
        return {
          element: conditional,
          metadata: elementMetadata,
        }
      default:
        assertNever(element)
    }
  }

  const childComponents: Array<{
    element: GroupChildElement
    metadata: ElementInstanceMetadata
  }> = orderedActionTargets.map(
    (p): { element: GroupChildElement; metadata: ElementInstanceMetadata } => {
      const foundMetadata = MetadataUtils.findElementByElementPath(metadata, p)
      const element = foundMetadata?.element
      if (
        foundMetadata == null ||
        element == null ||
        isLeft(element) ||
        !elementCanBeAGroupChild(element.value)
      ) {
        throw new Error(
          `Invariant violation: ElementInstanceMetadata.element found for ${EP.toString(
            p,
          )} cannot be a group child`,
        )
      }
      return removeMarginsFromGroupChildElement(element.value, foundMetadata)
    },
  )

  // delete all reparented elements first to avoid UID clashes
  const deleteCommands = orderedActionTargets.map((e) => deleteElement('always', e))

  // TODO this is horrible and temporary at best. Instead of this, we should fix layoutSystemForChildren for Fragments in fillGlobalContentBoxFromAncestors
  const targetParentIsFlex = MetadataUtils.isFlexLayoutedContainer(
    MetadataUtils.findElementByElementPath(
      metadata,
      replaceNonDomElementWithFirstDomAncestorPath(
        metadata,
        allElementProps,
        elementPathTrees,
        parentPath.intendedParentPath,
      ),
    ),
  )

  // if we insert the group into a Flex parent, do not make it position: absolute and do not give it left, top pins
  const maybePositionAbsolute: CSSProperties = targetParentIsFlex
    ? { contain: 'layout' }
    : { position: 'absolute', left: newLocalRectangleForGroup.x, top: newLocalRectangleForGroup.y }

  // create a group with all elements as children
  const group = jsxElement(
    'Group',
    generateUidWithExistingComponents(projectContents),
    jsxAttributesFromMap({
      style: jsExpressionValue(
        // set group size here so we don't have to true it up
        {
          ...maybePositionAbsolute,
          width: newLocalRectangleForGroup.width,
          height: newLocalRectangleForGroup.height,
        },
        emptyComments,
      ),
    }),
    childComponents.map((c) => c.element),
  )

  // if any group child was a child of the group's target parent, let's use the child's original index for the insertion
  const anyChildIndexInTargetParent: Absolute | undefined = mapDropNulls((child) => {
    const childIsTheChildOfTargetParent = EP.isParentOf(
      parentPath.intendedParentPath,
      child.metadata.elementPath,
    )
    if (!childIsTheChildOfTargetParent) {
      return null
    }

    const indexInParent = MetadataUtils.getIndexInParent(
      metadata,
      elementPathTrees,
      child.metadata.elementPath,
    )
    if (indexInParent < 0) {
      return null
    }
    return absolute(indexInParent)
  }, childComponents).at(0)

  const indexPosition = anyChildIndexInTargetParent ?? back()

  // insert a group in the common ancestor
  const insertGroupCommand = addElement('always', parentPath, group, {
    importsToAdd: GroupImport,
    indexPosition: indexPosition,
  })

  const groupPath = EP.appendToPath(parentPath.intendedParentPath, group.uid) // TODO does this work if the parentPath is a ConditionalClauseInsertionPath?

  const pinChangeCommands: ReadonlyArray<CanvasCommand> = arrayAccumulate((acc) => {
    orderedActionTargets.forEach((maybeTarget) => {
      return replaceFragmentLikePathsWithTheirChildrenRecursive(
        metadata,
        allElementProps,
        elementPathTrees,
        [maybeTarget],
      ).forEach((target) => {
        const expectedPathInsideGroup = forceNotNull(
          `invariant violation: no common path found between element and its descendants`,
          EP.replaceIfAncestor(target, EP.parentPath(maybeTarget), groupPath),
        )

        const foundMetadata = MetadataUtils.findElementByElementPath(metadata, target)

        acc.push(
          ...createPinChangeCommandsForElementBecomingGroupChild(
            metadata,
            foundMetadata,
            expectedPathInsideGroup,
            globalBoundingBoxOfAllElementsToBeWrapped,
            newLocalRectangleForGroup,
          ),
        )
      })
    })
  })

  const selectNewGroup = updateSelectedViews('always', [groupPath])

  return applyCommandsAction([
    ...deleteCommands,
    insertGroupCommand,
    ...pinChangeCommands,
    selectNewGroup,
    queueGroupTrueUp([trueUpElementChanged(groupPath)]),
  ])
}

export function createPinChangeCommandsForElementBecomingGroupChild(
  jsxMetadata: ElementInstanceMetadataMap,
  elementMetadata: ElementInstanceMetadata | null,
  expectedPath: ElementPath,
  globalBoundingBoxOfAllElementsToBeWrapped: CanvasRectangle,
  newLocalRectangleForGroup: LocalRectangle,
): Array<CanvasCommand> {
  if (
    elementMetadata == null ||
    isLeft(elementMetadata.element) ||
    !elementCanBeAGroupChild(elementMetadata.element.value)
  ) {
    throw new Error(
      `Invariant violation: ElementInstanceMetadata.element found for ${EP.toString(
        expectedPath,
      )} cannot be a group child`,
    )
  }

  const childLocalRect: LocalRectangle = canvasRectangleToLocalRectangle(
    forceFiniteRectangle(elementMetadata.globalFrame),
    globalBoundingBoxOfAllElementsToBeWrapped,
  )
  switch (elementMetadata.element.value.type) {
    case 'JSX_ELEMENT':
      return commandsForPinChangeCommandForElementBecomingGroup(
        expectedPath,
        elementMetadata.element.value.props,
        childLocalRect,
        newLocalRectangleForGroup,
      )
    case 'JSX_CONDITIONAL_EXPRESSION':
      const activeBranch = getConditionalActiveCase(
        elementMetadata.elementPath,
        elementMetadata.element.value,
        jsxMetadata,
      )
      const branch =
        activeBranch === 'true-case'
          ? elementMetadata.element.value.whenTrue
          : elementMetadata.element.value.whenFalse
      return isJSXElement(branch)
        ? commandsForPinChangeCommandForElementBecomingGroup(
            expectedPath,
            branch.props,
            childLocalRect,
            newLocalRectangleForGroup,
          )
        : []
    case 'JSX_FRAGMENT':
      return []
    default:
      assertNever(elementMetadata.element.value)
  }
}

/**
 * This is an "optimistic" variant of createPinChangeCommandsForElementBecomingGroupChild
 * that will create missing pins for a new group child when its metadata is not available.
 * It will be using the existing pins of the element, if present, or default to either
 * the center of the group frame (or fallback 0,0) for its position and the parent width/height
 * for dimensions.
 */
export function createPinChangeCommandsForElementInsertedIntoGroup(
  expectedPath: ElementPath,
  props: PropsOrJSXAttributes,
  groupRectangle: CanvasRectangle,
  newLocalRectangleForGroup: LocalRectangle,
): Array<CanvasCommand> {
  function propOrZero(prop: StyleLayoutProp): number {
    const maybeProp = getLayoutProperty(prop, props, styleStringInArray)
    return isRight(maybeProp) && isCSSNumber(maybeProp.value) ? maybeProp.value.value : 0
  }
  let childLocalRect: LocalRectangle = localRectangle({
    x: propOrZero('left'),
    y: propOrZero('top'),
    width: propOrZero('width'),
    height: propOrZero('height'),
  })

  if (childLocalRect.width > 0) {
    childLocalRect.x = (groupRectangle.width - childLocalRect.width) / 2
  } else {
    childLocalRect.width = groupRectangle.width
  }
  if (childLocalRect.height > 0) {
    childLocalRect.y = (groupRectangle.height - childLocalRect.height) / 2
  } else {
    childLocalRect.height = groupRectangle.height
  }

  return commandsForPinChangeCommandForElementBecomingGroup(
    expectedPath,
    props.value,
    childLocalRect,
    newLocalRectangleForGroup,
  )
}

function commandsForPinChangeCommandForElementBecomingGroup(
  expectedPath: ElementPath,
  elementProps: JSXAttributes,
  childRect: LocalRectangle,
  groupRect: LocalRectangle,
): Array<CanvasCommand> {
  return [
    // make the child `position: absolute`
    setProperty('always', expectedPath, PP.create('style', 'position'), 'absolute'),
    // set child pins to match their intended new local rectangle
    ...setElementPinsForLocalRectangleEnsureTwoPinsPerDimension(
      expectedPath,
      elementProps,
      childRect,
      sizeFromRectangle(groupRect),
      null,
    ),
  ]
}

function setElementPinsForLocalRectangleEnsureTwoPinsPerDimension(
  target: ElementPath,
  elementCurrentProps: JSXAttributes,
  localFrame: LocalRectangle,
  parentSize: Size,
  parentFlexDirection: FlexDirection | null,
): Array<CanvasCommand> {
  // ensure at least two pins per dimension
  const mustHavePins = ensureAtLeastTwoPinsForEdgePosition(
    right(elementCurrentProps),
    EdgePositionBottomRight,
  )

  function setPin(pin: AbsolutePin, value: number): SetCssLengthProperty {
    return setCssLengthProperty(
      'always',
      target,
      PP.create('style', pin),
      setValueKeepingOriginalUnit(
        value,
        isHorizontalPin(pin) ? parentSize.width : parentSize.height,
      ),
      parentFlexDirection,
      mustHavePins.includes(pin) ? 'create-if-not-existing' : 'do-not-create-if-doesnt-exist',
    )
  }

  function setPinPreserveHug(pin: 'width' | 'height', value: number): Array<SetCssLengthProperty> {
    const pinIsAlreadyHug = isHugFromStyleAttribute(elementCurrentProps, pin)

    if (pinIsAlreadyHug) {
      // we don't need to convert a Hug pin, do nothing here
      return []
    }
    return [setPin(pin, value)]
  }

  // TODO retarget Fragments
  const result = [
    setPin('left', localFrame.x),
    setPin('top', localFrame.y),
    setPin('right', parentSize.width - (localFrame.x + localFrame.width)),
    setPin('bottom', parentSize.height - (localFrame.y + localFrame.height)),
    ...setPinPreserveHug('width', localFrame.width),
    ...setPinPreserveHug('height', localFrame.height),
  ]
  return result
}
