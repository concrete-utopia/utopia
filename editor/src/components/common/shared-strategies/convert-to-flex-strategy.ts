import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { last, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import { isLeft } from '../../../core/shared/either'
import * as EP from '../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import { isJSXElementLike, jsxFragment } from '../../../core/shared/element-template'
import {
  boundingRectangleArray,
  getRectCenter,
  nullIfInfinity,
  zeroCanvasRect,
  type CanvasRectangle,
  anyRectanglesIntersect,
  zeroRectIfNullOrInfinity,
  roundTo,
} from '../../../core/shared/math-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { assertNever, fastForEach } from '../../../core/shared/utils'
import { absolute } from '../../../utils/utils'
import {
  getElementFragmentLikeType,
  isElementNonDOMElement,
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  replaceNonDOMElementPathsWithTheirChildrenRecursive,
} from '../../canvas/canvas-strategies/strategies/fragment-like-helpers'
import type { JSXFragmentConversion } from '../../canvas/canvas-strategies/strategies/group-conversion-helpers'
import {
  actuallyConvertFramentToFrame,
  getCommandsForConversionToDesiredType,
} from '../../canvas/canvas-strategies/strategies/group-conversion-helpers'
import { treatElementAsGroupLike } from '../../canvas/canvas-strategies/strategies/group-helpers'
import type { CanvasFrameAndTarget } from '../../canvas/canvas-types'
import type { CanvasCommand } from '../../canvas/commands/commands'
import { rearrangeChildren } from '../../canvas/commands/rearrange-children-command'
import { reorderElement } from '../../canvas/commands/reorder-element-command'
import { setProperty, setPropertyOmitNullProp } from '../../canvas/commands/set-property-command'
import { showToastCommand } from '../../canvas/commands/show-toast-command'
import type { AllElementProps } from '../../editor/store/editor-state'
import type { FlexDirection } from '../../inspector/common/css-utils'
import {
  childIs100PercentSizedInEitherDirection,
  convertWidthToFlexGrowOptionally,
  getSafeGroupChildConstraintsArray,
  nukeAllAbsolutePositioningPropsCommands,
  onlyChildIsSpan,
  sizeToVisualDimensions,
} from '../../inspector/inspector-common'
import { setHugContentForAxis } from '../../inspector/inspector-strategies/hug-contents-strategy'
import type { FlexAlignItems, FlexDirectionRowColumn } from './convert-strategies-common'
import { getChildrenPathsForContainer } from './convert-strategies-common'

function checkConstraintsForThreeElementRow(
  allElementProps: AllElementProps,
  firstChildPath: ElementPath,
  secondChildPath: ElementPath,
  thirdChildPath: ElementPath,
): boolean {
  // We're looking for this pattern of constraints:
  // - ['left', 'width']
  // - ['left', 'right']
  // - ['right', 'width']
  const firstChildConstraints = getSafeGroupChildConstraintsArray(allElementProps, firstChildPath)
  const secondChildConstraints = getSafeGroupChildConstraintsArray(allElementProps, secondChildPath)
  const thirdChildConstraints = getSafeGroupChildConstraintsArray(allElementProps, thirdChildPath)
  return (
    firstChildConstraints.length === 2 &&
    firstChildConstraints.includes('left') &&
    firstChildConstraints.includes('width') &&
    secondChildConstraints.length === 2 &&
    secondChildConstraints.includes('left') &&
    secondChildConstraints.includes('right') &&
    thirdChildConstraints.length === 2 &&
    thirdChildConstraints.includes('right') &&
    thirdChildConstraints.includes('width')
  )
}

function anyOverlappingFrames(...elementMetadataEntries: Array<ElementInstanceMetadata>): boolean {
  const globalFrames = mapDropNulls((metadata) => metadata.globalFrame, elementMetadataEntries)
  return anyRectanglesIntersect(globalFrames)
}

function laidOutAsRow(...elementMetadataEntries: Array<ElementInstanceMetadata>): boolean {
  if (anyOverlappingFrames(...elementMetadataEntries)) {
    return false
  } else {
    for (let frontIndex: number = 0; frontIndex < elementMetadataEntries.length - 1; frontIndex++) {
      const backIndex = frontIndex + 1
      const frontMetadata = elementMetadataEntries[frontIndex]
      const backMetadata = elementMetadataEntries[backIndex]
      if (
        zeroRectIfNullOrInfinity(frontMetadata.globalFrame).x >
        zeroRectIfNullOrInfinity(backMetadata.globalFrame).x
      ) {
        return false
      }
    }
    return true
  }
}

type CommandsOrNotApplicable = Array<CanvasCommand> | 'not-applicable'

function convertSingleChildWith100PercentSize(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  parentFlexDirection: FlexDirection | null,
  path: ElementPath,
  childrenPaths: Array<ElementPath>,
  direction: FlexDirectionRowColumn,
): CommandsOrNotApplicable {
  const firstChildPath = childrenPaths[0]

  const { childWidth100Percent, childHeight100Percent } = childIs100PercentSizedInEitherDirection(
    metadata,
    firstChildPath,
  )

  const optionalCenterAlignCommands = onlyChildIsSpan(metadata, childrenPaths)
    ? [
        setProperty('always', path, PP.create('style', 'alignItems'), 'center'),
        setProperty('always', path, PP.create('style', 'justifyContent'), 'center'),
        setHugContentForAxis('horizontal', firstChildPath, parentFlexDirection),
        setHugContentForAxis('vertical', firstChildPath, parentFlexDirection),
      ]
    : []

  if (childrenPaths.length === 1 && (childWidth100Percent || childHeight100Percent)) {
    return [
      ...ifElementIsFragmentLikeFirstConvertItToFrame(
        metadata,
        allElementProps,
        elementPathTree,
        path,
      ),
      setProperty('always', path, PP.create('style', 'display'), 'flex'),
      setProperty('always', path, PP.create('style', 'flexDirection'), direction),
      ...(childWidth100Percent
        ? []
        : [setHugContentForAxis('horizontal', path, parentFlexDirection)]),
      ...(childHeight100Percent
        ? []
        : [setHugContentForAxis('vertical', path, parentFlexDirection)]),
      ...childrenPaths.flatMap((child) => [
        ...nukeAllAbsolutePositioningPropsCommands(child),
        ...convertWidthToFlexGrowOptionally(metadata, child, direction),
      ]),
      ...optionalCenterAlignCommands,
    ]
  }

  return 'not-applicable'
}

function convertThreeElementGroupRow(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  path: ElementPath,
  childrenPaths: Array<ElementPath>,
  parentFlexDirection: FlexDirection | null,
  direction: FlexDirectionRowColumn,
): CommandsOrNotApplicable {
  if (childrenPaths.length === 3) {
    const childrenMetadata = mapDropNulls(
      (childPath) => MetadataUtils.findElementByElementPath(metadata, childPath),
      childrenPaths,
    )
    if (childrenMetadata.length === 3) {
      // This should make the logic independent of the ordering within the code,
      // as their logical order does not necessarily relate to their visual position.
      const childrenMetadataOrderedByHorizontalPosition = sortBy(
        childrenMetadata,
        (first, second) => {
          return (
            zeroRectIfNullOrInfinity(first.localFrame).x -
            zeroRectIfNullOrInfinity(second.localFrame).x
          )
        },
      )
      // Get the reordered metadata and paths.
      const [firstChildElement, secondChildElement, thirdChildElement] =
        childrenMetadataOrderedByHorizontalPosition
      const [firstChildPath, secondChildPath, thirdChildPath] =
        childrenMetadataOrderedByHorizontalPosition.map((meta) => meta.elementPath)
      if (laidOutAsRow(firstChildElement, secondChildElement, thirdChildElement)) {
        if (
          checkConstraintsForThreeElementRow(
            allElementProps,
            firstChildPath,
            secondChildPath,
            thirdChildPath,
          )
        ) {
          const firstFrame = zeroRectIfNullOrInfinity(firstChildElement.localFrame)
          const secondFrame = zeroRectIfNullOrInfinity(secondChildElement.localFrame)

          const gapValue = roundTo(secondFrame.x - firstFrame.width, 2)

          return [
            // Configure the parent element.
            ...getCommandsForConversionToDesiredType(
              metadata,
              elementPathTree,
              allElementProps,
              [path],
              'group',
              'frame',
            ),
            ...maybeAlignElementsToCenter(
              metadata,
              childrenPaths,
              path,
              direction,
              parentFlexDirection,
            ),
            setProperty('always', path, PP.create('style', 'display'), 'flex'),
            setProperty('always', path, PP.create('style', 'flexDirection'), 'row'),
            setProperty('always', path, PP.create('style', 'gap'), gapValue),
            // Configure and reorder the "first" element.
            ...nukeAllAbsolutePositioningPropsCommands(firstChildPath),
            setProperty('always', firstChildPath, PP.create('style', 'flexGrow'), 0),
            reorderElement('always', firstChildPath, absolute(0)),
            // Configure and reorder the "second" element.
            ...nukeAllAbsolutePositioningPropsCommands(secondChildPath),
            setProperty('always', secondChildPath, PP.create('style', 'flexGrow'), 1),
            reorderElement('always', secondChildPath, absolute(1)),
            // Configure and reorder the "third" element.
            ...nukeAllAbsolutePositioningPropsCommands(thirdChildPath),
            setProperty('always', thirdChildPath, PP.create('style', 'flexGrow'), 0),
            reorderElement('always', thirdChildPath, absolute(2)),
          ]
        }
      }
    }
  }

  return 'not-applicable'
}

export function convertLayoutToFlexCommands(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  elementPaths: Array<ElementPath>,
  allElementProps: AllElementProps,
): Array<CanvasCommand> {
  return elementPaths.flatMap((path) => {
    const parentInstance = MetadataUtils.findElementByElementPath(metadata, path)
    if (parentInstance == null) {
      return []
    }

    if (MetadataUtils.isConditionalFromMetadata(parentInstance)) {
      // we do not support retargeting to children of conditionals, the behavior is under design / development
      return [
        showToastCommand(
          'Cannot be converted to Flex yet',
          'NOTICE',
          'cannot-convert-children-to-flex',
        ),
      ]
    }

    const childrenPaths = getChildrenPathsForContainer(
      metadata,
      elementPathTree,
      path,
      allElementProps,
    )

    const parentFlexDirection =
      MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
        .parentFlexDirection ?? null

    if (childrenPaths.length === 0) {
      // fall back to a simple prop-setting without any kind of guessing
      return [setProperty('always', path, PP.create('style', 'display'), 'flex')]
    }

    const { direction, averageGap, sortedChildren, padding, alignItems } = guessMatchingFlexSetup(
      metadata,
      path,
      childrenPaths,
    )
    const sortedChildrenPaths = sortedChildren.map((c) => EP.dynamicPathToStaticPath(c.target))

    // Special case: We only have a single child which has a size of 100%.
    const possibleSingleChildWith100PercentSize = convertSingleChildWith100PercentSize(
      metadata,
      allElementProps,
      elementPathTree,
      parentFlexDirection,
      path,
      childrenPaths,
      direction,
    )
    if (possibleSingleChildWith100PercentSize !== 'not-applicable') {
      return possibleSingleChildWith100PercentSize
    }

    // Special case: Group with 3 child elements in a row with some specific `data-constraints`.
    const possibleThreeElementGroupRow = convertThreeElementGroupRow(
      metadata,
      allElementProps,
      elementPathTree,
      path,
      childrenPaths,
      parentFlexDirection,
      direction,
    )
    if (possibleThreeElementGroupRow !== 'not-applicable') {
      return possibleThreeElementGroupRow
    }

    const rearrangedChildrenPaths = rearrangedPathsWithFlexConversionMeasurementBoundariesIntact(
      metadata,
      allElementProps,
      elementPathTree,
      path,
      sortedChildrenPaths,
    )

    // FIXME: `childrenPaths` doesn't include text elements yet, and this causes `rearrangeChildren` throw an error
    const containerHasNoTextChildren = getElementTextChildrenCount(parentInstance) === 0

    const rearrangeCommands =
      rearrangedChildrenPaths != null && containerHasNoTextChildren
        ? [
            rearrangeChildren(
              'always',
              path,
              rearrangedChildrenPaths.map(EP.dynamicPathToStaticPath),
            ),
          ]
        : [
            showToastCommand(
              "Couldn't preserve visual order of children (yet)",
              'NOTICE',
              'cannot-convert-children-to-flex',
            ),
          ]

    return [
      ...ifElementIsFragmentLikeFirstConvertItToFrame(
        metadata,
        allElementProps,
        elementPathTree,
        path,
      ),
      setProperty('always', path, PP.create('style', 'display'), 'flex'),
      setProperty('always', path, PP.create('style', 'flexDirection'), direction),
      ...setPropertyOmitNullProp('always', path, PP.create('style', 'gap'), averageGap),
      setHugContentForAxis('horizontal', path, parentFlexDirection),
      setHugContentForAxis('vertical', path, parentFlexDirection),
      ...setPropertyOmitNullProp('always', path, PP.create('style', 'padding'), padding),
      ...setPropertyOmitNullProp('always', path, PP.create('style', 'alignItems'), alignItems),
      ...childrenPaths.flatMap((child) => [
        ...nukeAllAbsolutePositioningPropsCommands(child),
        ...sizeToVisualDimensions(metadata, elementPathTree, child),
      ]),
      ...rearrangeCommands,
      ...maybeAlignElementsToCenter(metadata, childrenPaths, path, direction, parentFlexDirection),
    ]
  })
}

function ifElementIsFragmentLikeFirstConvertItToFrame(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTrees: ElementPathTrees,
  target: ElementPath,
): Array<CanvasCommand> {
  const type = getElementFragmentLikeType(
    metadata,
    allElementProps,
    elementPathTrees,
    target,
    'sizeless-div-considered-fragment-like',
  )
  const elementInstace = MetadataUtils.findElementByElementPath(metadata, target)
  if (
    elementInstace == null ||
    isLeft(elementInstace.element) ||
    !isJSXElementLike(elementInstace.element.value)
  ) {
    return []
  }

  if (type == 'fragment' || type == 'sizeless-div' || treatElementAsGroupLike(metadata, target)) {
    const childInstances = mapDropNulls(
      (path) => MetadataUtils.findElementByElementPath(metadata, path),
      replaceFragmentLikePathsWithTheirChildrenRecursive(
        metadata,
        allElementProps,
        elementPathTrees,
        MetadataUtils.getChildrenPathsOrdered(elementPathTrees, target),
      ),
    )

    const childrenBoundingFrame =
      boundingRectangleArray(
        mapDropNulls(
          (rect) => nullIfInfinity(rect),
          childInstances.map((c) => c.globalFrame),
        ),
      ) ?? zeroCanvasRect

    const instance: JSXFragmentConversion = {
      element: jsxFragment(
        elementInstace.element.value.uid,
        elementInstace.element.value.children,
        false,
      ),
      childInstances: childInstances,
      childrenBoundingFrame: childrenBoundingFrame,
      specialSizeMeasurements: elementInstace.specialSizeMeasurements,
    }
    return actuallyConvertFramentToFrame(metadata, elementPathTrees, instance, target)
  }

  return []
}

const NEAR_CENTER_LINE_TOLERANCE_MULTIPLIER = 0.2

function isElementNearCenterLine(
  parentGlobalFrame: CanvasRectangle,
  parentFlexDirection: FlexDirectionRowColumn,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean {
  const childGlobalFrame = nullIfInfinity(
    MetadataUtils.getFrameInCanvasCoords(elementPath, metadata),
  )
  if (childGlobalFrame == null) {
    return false
  }

  const parentCenter = getRectCenter(parentGlobalFrame)
  const childCenter = getRectCenter(childGlobalFrame)
  switch (parentFlexDirection) {
    case 'column':
      return (
        Math.abs(childCenter.x - parentCenter.x) <
        parentGlobalFrame.width * NEAR_CENTER_LINE_TOLERANCE_MULTIPLIER
      )
    case 'row':
      return (
        Math.abs(childCenter.y - parentCenter.y) <
        parentGlobalFrame.height * NEAR_CENTER_LINE_TOLERANCE_MULTIPLIER
      )
    default:
      assertNever(parentFlexDirection)
  }
}

function maybeAlignElementsToCenter(
  metadata: ElementInstanceMetadataMap,
  childrenPaths: ElementPath[],
  targetPath: ElementPath,
  detectedDirection: FlexDirectionRowColumn,
  parentFlexDirection: FlexDirection | null,
) {
  if (onlyChildIsSpan(metadata, childrenPaths)) {
    return [
      setProperty('always', targetPath, PP.create('style', 'alignItems'), 'center'),
      setProperty('always', targetPath, PP.create('style', 'justifyContent'), 'center'),
      setHugContentForAxis('horizontal', childrenPaths[0], parentFlexDirection),
      setHugContentForAxis('vertical', childrenPaths[0], parentFlexDirection),
    ]
  }

  const parentGlobalFrame = nullIfInfinity(
    MetadataUtils.getFrameInCanvasCoords(targetPath, metadata),
  )
  if (parentGlobalFrame == null) {
    return []
  }

  const allChildrenAlongCenterLine = childrenPaths.every((child) =>
    isElementNearCenterLine(parentGlobalFrame, detectedDirection, metadata, child),
  )
  if (!allChildrenAlongCenterLine) {
    return []
  }

  return [
    setProperty('always', targetPath, PP.create('style', 'alignItems'), 'center'),
    setProperty('always', targetPath, PP.create('style', 'justifyContent'), 'flex-start'),
  ]
}

function guessMatchingFlexSetup(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
  children: Array<ElementPath>,
): {
  direction: FlexDirectionRowColumn
  sortedChildren: Array<CanvasFrameAndTarget>
  averageGap: number | null
  padding: string | null
  alignItems: FlexAlignItems | null
} {
  const result = guessLayoutDirection(metadata, target, children)

  if (result.sortedChildren.length === 0) {
    return { ...result, padding: null, alignItems: null }
  }

  const padding: string | null = guessPadding(
    result.direction,
    result.parentRect,
    result.sortedChildren,
  )

  const alignItems: FlexAlignItems | null = guessAlignItems(result.direction, result.sortedChildren)

  return { ...result, padding: padding, alignItems: alignItems }
}

function guessLayoutDirection(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
  children: Array<ElementPath>,
): {
  direction: FlexDirectionRowColumn
  sortedChildren: Array<CanvasFrameAndTarget>
  parentRect: CanvasRectangle
  averageGap: number | null
} {
  const parentRect = MetadataUtils.getFrameOrZeroRectInCanvasCoords(target, metadata)
  const firstGuess: FlexDirectionRowColumn = parentRect.width > parentRect.height ? 'row' : 'column'
  const firstGuessResult = detectConfigurationInDirection(
    metadata,
    children,
    firstGuess,
    parentRect,
  )
  if (firstGuessResult.childrenDontOverlap) {
    return firstGuessResult
  }
  const secondGuess = firstGuess === 'row' ? 'column' : 'row'
  const secondGuessResult = detectConfigurationInDirection(
    metadata,
    children,
    secondGuess,
    parentRect,
  )
  if (secondGuessResult.childrenDontOverlap) {
    return secondGuessResult
  }

  // since none of the directions are great, let's fall back to our first guess
  return firstGuessResult
}

function guessPadding(
  direction: FlexDirectionRowColumn,
  parentRect: CanvasRectangle,
  sortedChildren: Array<CanvasFrameAndTarget>,
): string | null {
  const firstChild = sortedChildren[0]
  const lastChild = last(sortedChildren)!

  const paddingLeft = firstChild.frame.x - parentRect.x
  const paddingRight =
    parentRect.x + parentRect.width - (lastChild?.frame.x + lastChild?.frame.width)
  const horizontalPadding = Math.max(0, Math.min(paddingLeft, paddingRight))
  const paddingTop = firstChild.frame.y - parentRect.y
  const paddingBottom =
    parentRect.y + parentRect.height - (lastChild?.frame.y + lastChild?.frame.height)
  const verticalPadding = Math.max(0, Math.min(paddingTop, paddingBottom))

  if (horizontalPadding === 0 && verticalPadding === 0) {
    return null
  }

  return `${appendPx(verticalPadding)} ${appendPx(horizontalPadding)}`
}

function appendPx(value: number): string {
  return value === 0 ? '0' : `${value}px`
}

function detectConfigurationInDirection(
  metadata: ElementInstanceMetadataMap,
  children: Array<ElementPath>,
  direction: FlexDirectionRowColumn,
  parentRect: CanvasRectangle,
): {
  childrenDontOverlap: boolean
  direction: FlexDirectionRowColumn
  sortedChildren: Array<CanvasFrameAndTarget>
  averageGap: number | null
  parentRect: CanvasRectangle
} {
  const childFrames: Array<CanvasFrameAndTarget> = children.map((child) => ({
    target: child,
    frame: MetadataUtils.getFrameOrZeroRectInCanvasCoords(child, metadata),
  }))
  const sortedChildren = sortBy(childFrames, (l, r) =>
    direction === 'row' ? l.frame.x - r.frame.x : l.frame.y - r.frame.y,
  )

  if (children.length < 2) {
    return {
      childrenDontOverlap: true,
      direction: direction,
      parentRect: parentRect,
      sortedChildren: sortedChildren,
      averageGap: null,
    }
  }

  let childrenDontOverlap: boolean = true
  let gapSum = 0

  fastForEach(sortedChildren, (child, i) => {
    if (i === 0) {
      return
    } else {
      const prevFrame: CanvasRectangle = sortedChildren[i - 1].frame
      const gap =
        direction === 'row'
          ? child.frame.x - (prevFrame.x + prevFrame.width)
          : child.frame.y - (prevFrame.y + prevFrame.height)

      gapSum += gap
      childrenDontOverlap = childrenDontOverlap && gap > -1
    }
  })

  const averageGap = Math.max(0, gapSum / (sortedChildren.length - 1))

  return {
    childrenDontOverlap: childrenDontOverlap,
    sortedChildren: sortedChildren,
    direction: direction,
    averageGap: averageGap === 0 ? null : averageGap,
    parentRect: parentRect,
  }
}

function guessAlignItems(
  direction: FlexDirectionRowColumn,
  children: Array<CanvasFrameAndTarget>,
): FlexAlignItems | null {
  if (children.length < 2) {
    return null
  }
  const leftOrTop: 'x' | 'y' = direction === 'column' ? 'x' : 'y'
  const widthOrHeight: 'width' | 'height' = direction === 'column' ? 'width' : 'height'

  let allAlignedAtStart: boolean = true
  let allAlignedAtCenter: boolean = true
  let allAlignedAtEnd: boolean = true

  for (let index = 1; index < children.length; index++) {
    const previousElement = children[index - 1].frame
    const currentElement = children[index].frame

    // check for flex-start
    if (previousElement[leftOrTop] !== currentElement[leftOrTop]) {
      allAlignedAtStart = false
    }

    // check for center
    if (
      previousElement[leftOrTop] + previousElement[widthOrHeight] / 2 !==
      currentElement[leftOrTop] + currentElement[widthOrHeight] / 2
    ) {
      allAlignedAtCenter = false
    }

    // check for flex-end
    if (
      previousElement[leftOrTop] + previousElement[widthOrHeight] !==
      currentElement[leftOrTop] + currentElement[widthOrHeight]
    ) {
      allAlignedAtEnd = false
    }
  }

  if (allAlignedAtStart) {
    return null // we omit flex-start as that is the default anyways. Improvement: check if it _is_ the default computed style!
  }
  if (allAlignedAtCenter) {
    return 'center'
  }
  if (allAlignedAtEnd) {
    return 'flex-end'
  }

  // fallback: null, which implicitly means a default of flex-start
  return null
}

interface NonDOMElementWithLeaves {
  element: ElementPath // path to a non-DOM element
  leaves: Set<string> // stringified element paths to the leaves in the tree of this element
}

interface TopLevelChildrenAndGroups {
  topLevelChildren: Set<string> // children that are DOM elements and are immediate children of a the parent
  nonDOMElementsWithLeaves: Array<NonDOMElementWithLeaves> // children that are non-DOM elements
}

function getTopLevelChildrenAndMeasurementBoundaries(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  parentPath: ElementPath,
): TopLevelChildrenAndGroups {
  let topLevelChildren: Array<string> = []
  let maesurementBoundaries: Array<NonDOMElementWithLeaves> = []

  const childrenPaths = MetadataUtils.getChildrenPathsOrdered(pathTrees, parentPath)

  for (const child of childrenPaths) {
    if (isElementNonDOMElement(metadata, allElementProps, pathTrees, child)) {
      maesurementBoundaries.push({
        element: child,
        leaves: new Set(
          replaceNonDOMElementPathsWithTheirChildrenRecursive(
            metadata,
            allElementProps,
            pathTrees,
            [child],
          ).map(EP.toString),
        ),
      })
    } else {
      topLevelChildren.push(EP.toString(child))
    }
  }

  return {
    topLevelChildren: new Set(topLevelChildren),
    nonDOMElementsWithLeaves: maesurementBoundaries,
  }
}

/**
 * Checks whether a prefix of `sortedChildren` is made up of the elements of `siblings`
 * If the elements of `siblings` is a prefix of `sortedChildren`, the prefix is dropped and the rest of `sortedChildren` is returned
 * Otherwise, null is returned, signaling failure
 */
function checkAllChildrenPartOfSingleGroup(
  siblings: Set<string>,
  sortedChildren: Array<ElementPath>,
): Array<ElementPath> | null {
  const workingSiblings = new Set([...siblings])
  let workingChildren = sortedChildren

  while (workingSiblings.size > 0) {
    if (workingChildren.length === 0) {
      // this is an invariant violation, since `siblings` should be a subset of `sortedChildren`
      return null
    }

    const child = workingChildren[0]
    const childPathString = EP.toString(child)

    if (!workingSiblings.has(childPathString)) {
      // this child was reordered here from another measurement unit, which we disallow for now
      return null
    }

    workingSiblings.delete(childPathString)
    workingChildren = workingChildren.slice(1)
  }
  return workingChildren
}

/**
 * returns a list of element paths, so that non-dom element children are swapped out for their
 */
function rearrangedPathsWithFlexConversionMeasurementBoundariesIntact(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  parentPath: ElementPath,
  sortedChildren: Array<ElementPath>,
): Array<ElementPath> | null {
  const childrenAndGroups = getTopLevelChildrenAndMeasurementBoundaries(
    metadata,
    allElementProps,
    pathTrees,
    parentPath,
  )

  let workingSortedChildren = sortedChildren
  let finalReorderedPaths: Array<ElementPath> = []

  while (workingSortedChildren.length > 0) {
    const child = workingSortedChildren[0]
    const childPathString = EP.toString(child)

    if (childrenAndGroups.topLevelChildren.has(childPathString)) {
      finalReorderedPaths.push(child)
      workingSortedChildren = workingSortedChildren.slice(1)
    } else {
      const measurementBoundaryWithChild = childrenAndGroups.nonDOMElementsWithLeaves.find((g) =>
        g.leaves.has(childPathString),
      )

      if (measurementBoundaryWithChild == null) {
        return null
      }

      const restOfChildren = checkAllChildrenPartOfSingleGroup(
        measurementBoundaryWithChild.leaves,
        workingSortedChildren,
      )

      if (restOfChildren == null) {
        return null
      }

      if (!EP.pathsEqual(finalReorderedPaths.at(-1) ?? null, child)) {
        finalReorderedPaths.push(measurementBoundaryWithChild.element)
      }

      workingSortedChildren = restOfChildren
    }
  }

  return finalReorderedPaths
}

function getElementTextChildrenCount(instance: ElementInstanceMetadata): number {
  if (
    isLeft(instance.element) ||
    !(
      instance.element.value.type === 'JSX_ELEMENT' ||
      instance.element.value.type === 'JSX_FRAGMENT'
    )
  ) {
    return 0
  }

  return instance.element.value.children.filter((e) => e.type === 'JSX_TEXT_BLOCK').length
}
