import { styleStringInArray } from '../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { LayoutEdgeProp, LayoutPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { framePointForPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { mapDropNulls, stripNulls } from '../../../../core/shared/array-utils'
import { isLeft, isRight, right } from '../../../../core/shared/either'
import {
  isJSXElement,
  type ElementInstanceMetadataMap,
  type JSXElement,
} from '../../../../core/shared/element-template'
import { roundRectangleToNearestWhole } from '../../../../core/shared/math-utils'
import type {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  LocalRectangle,
} from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  canvasVector,
  isInfinityRectangle,
  nullIfInfinity,
  pointDifference,
  zeroCanvasRect,
  zeroLocalRect,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import {
  EdgePositionRight,
  type CanvasFrameAndTarget,
  type EdgePosition,
  EdgePositionBottom,
} from '../../canvas-types'
import { pickPointOnRect, snapPoint } from '../../canvas-utils'
import type { AdjustCssLengthProperties } from '../../commands/adjust-css-length-command'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import { pointGuidelineToBoundsEdge } from '../../controls/guideline-helpers'
import type { AbsolutePin } from './resize-helpers'
import { resizeBoundingBox } from './resize-helpers'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { replaceNonDOMElementPathsWithTheirChildrenRecursive } from './fragment-like-helpers'
import type { CanvasCommand } from '../../commands/commands'
import type { ProjectContentTreeRoot } from '../../../../components/assets'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { addOrMergeIntendedBounds } from './shared-keyboard-strategy-helpers'
import type { InspectorStrategy } from '../../../../components/inspector/inspector-strategies/inspector-strategy'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { withUnderlyingTarget } from '../../../../components/editor/store/editor-state'
import {
  pushIntendedBoundsAndUpdateGroups,
  pushIntendedBoundsGroup,
} from '../../commands/push-intended-bounds-and-update-targets-command'

export function createResizeCommands(
  element: JSXElement,
  selectedElement: ElementPath,
  edgePosition: EdgePosition,
  drag: CanvasVector,
  elementGlobalFrame: CanvasRectangle | null,
  elementParentBounds: CanvasRectangle | null,
  elementParentFlexDirection: FlexDirection | null,
): { commands: AdjustCssLengthProperties[]; intendedBounds: CanvasFrameAndTarget | null } {
  const pins = pinsForEdgePosition(edgePosition)
  const properties = mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const negative =
      pin === 'right' ||
      pin === 'bottom' ||
      (pin === 'width' && edgePosition.x === 0) ||
      (pin === 'height' && edgePosition.y === 0)
    const value = getLayoutProperty(pin, right(element.props), styleStringInArray)
    if (isRight(value) && value.value != null) {
      // TODO what to do about missing properties?
      return lengthPropertyToAdjust(
        stylePropPathMappingFn(pin, styleStringInArray),
        (horizontal ? drag.x : drag.y) * (negative ? -1 : 1),
        horizontal ? elementParentBounds?.width : elementParentBounds?.height,
        'create-if-not-existing',
      )
    } else {
      return null
    }
  }, pins)

  const adjustPropertiesCommand = adjustCssLengthProperties(
    'always',
    selectedElement,
    elementParentFlexDirection,
    properties,
  )

  const intendedBounds: CanvasFrameAndTarget | null =
    elementGlobalFrame == null
      ? null
      : {
          frame: roundRectangleToNearestWhole(
            resizeBoundingBox(elementGlobalFrame, drag, edgePosition, null, 'non-center-based'),
          ),
          target: selectedElement,
        }
  return { commands: [adjustPropertiesCommand], intendedBounds }
}

function pinsForEdgePosition(edgePosition: EdgePosition): AbsolutePin[] {
  let horizontalPins: AbsolutePin[] = []
  let verticalPins: AbsolutePin[] = []

  if (edgePosition.x === 0) {
    horizontalPins = ['left', 'width']
  } else if (edgePosition.x === 1) {
    horizontalPins = ['right', 'width']
  }

  if (edgePosition.y === 0) {
    verticalPins = ['top', 'height']
  } else if (edgePosition.y === 1) {
    verticalPins = ['bottom', 'height']
  }

  return [...horizontalPins, ...verticalPins]
}

export function snapBoundingBox(
  elementsToSnapTo: ElementPath[],
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  draggedCorner: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  lockedAspectRatio: number | null,
  centerBased: 'center-based' | 'non-center-based',
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
) {
  const oppositeCorner: EdgePosition = {
    x: 1 - draggedCorner.x,
    y: 1 - draggedCorner.y,
  } as EdgePosition

  const draggedPointMovedWithoutSnap = pickPointOnRect(resizedBounds, draggedCorner)
  const oppositePoint =
    lockedAspectRatio == null
      ? pickPointOnRect(resizedBounds, oppositeCorner)
      : getPointOnDiagonal(draggedCorner, draggedPointMovedWithoutSnap, lockedAspectRatio)

  const { snappedPointOnCanvas, guidelinesWithSnappingVector } = snapPoint(
    elementsToSnapTo,
    selectedElements,
    jsxMetadata,
    canvasScale,
    draggedPointMovedWithoutSnap,
    true,
    lockedAspectRatio != null,
    draggedPointMovedWithoutSnap,
    oppositePoint,
    draggedCorner,
    allElementProps,
    pathTrees,
    resizedBounds,
    centerBased,
  )

  const snapDelta = pointDifference(draggedPointMovedWithoutSnap, snappedPointOnCanvas)
  const snappedBounds = roundRectangleToNearestWhole(
    resizeBoundingBox(resizedBounds, snapDelta, draggedCorner, lockedAspectRatio, centerBased),
  )

  const updatedGuidelinesWithSnapping = pointGuidelineToBoundsEdge(
    guidelinesWithSnappingVector,
    snappedBounds,
  )

  return {
    snapDelta: snapDelta,
    snappedBoundingBox: snappedBounds,
    guidelinesWithSnappingVector: updatedGuidelinesWithSnapping,
  }
}

// returns a point on the diagonal of a rectangle, using a given corner and the aspect ratio of the rectangle, in the direction towards the inside of the rectangle
// from the center of the sides it returns a point on the orthogonal line from the side center
function getPointOnDiagonal(
  edgePosition: EdgePosition,
  cornerPoint: CanvasPoint,
  aspectRatio: number,
) {
  if (edgePosition.x === 0 && edgePosition.y === 0) {
    return canvasPoint({
      x: cornerPoint.x + 100 * aspectRatio,
      y: cornerPoint.y + 100,
    })
  }
  if (edgePosition.x === 0 && edgePosition.y === 1) {
    return canvasPoint({
      x: cornerPoint.x + 100 * aspectRatio,
      y: cornerPoint.y - 100,
    })
  }
  if (edgePosition.x === 1 && edgePosition.y === 1) {
    return canvasPoint({
      x: cornerPoint.x - 100 * aspectRatio,
      y: cornerPoint.y - 100,
    })
  }
  if (edgePosition.x === 1 && edgePosition.y === 0) {
    return canvasPoint({
      x: cornerPoint.x - 100 * aspectRatio,
      y: cornerPoint.y + 100,
    })
  }
  if (edgePosition.x === 0.5 && edgePosition.y === 0) {
    return canvasPoint({
      x: cornerPoint.x,
      y: cornerPoint.y + 100,
    })
  }
  if (edgePosition.x === 0.5 && edgePosition.y === 1) {
    return canvasPoint({
      x: cornerPoint.x,
      y: cornerPoint.y - 100,
    })
  }
  if (edgePosition.x === 0 && edgePosition.y === 0.5) {
    return canvasPoint({
      x: cornerPoint.x + 100,
      y: cornerPoint.y,
    })
  }
  if (edgePosition.x === 1 && edgePosition.y === 0.5) {
    return canvasPoint({
      x: cornerPoint.x + 100,
      y: cornerPoint.y,
    })
  }
  throw new Error(`Edge position ${edgePosition} is not a corner position`)
}

const hasPin = (pin: LayoutPinnedProp, element: JSXElement) => {
  const rawPin = getLayoutProperty(pin, right(element.props), styleStringInArray)
  return isRight(rawPin) && rawPin.value != null
}

export function childrenBoundsToSnapTo(
  resizingFromEdge: EdgePosition,
  targets: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
): ElementPath[] {
  const actualChildren = targets.flatMap((target) => {
    const childPaths = MetadataUtils.getChildrenUnordered(metadata, target).map(
      (child) => child.elementPath,
    )
    return replaceNonDOMElementPathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      childPaths,
    )
  })

  // exclude the children that pinned to the edge/corner that's being resized
  const pinsToExclude = pinsFromEdgePosition(resizingFromEdge)
  return actualChildren.filter((child) => {
    const element = MetadataUtils.findElementByElementPath(metadata, child)
    if (
      element == null ||
      !MetadataUtils.isPositionAbsolute(element) ||
      isLeft(element.element) ||
      element.element.value.type !== 'JSX_ELEMENT'
    ) {
      return false
    }
    const jsxElement = element.element.value
    const hasForbiddenPin = pinsToExclude.some((p) => hasPin(p, jsxElement))
    return !hasForbiddenPin
  })
}

function pinsFromEdgePosition(edgePosition: EdgePosition): Array<LayoutEdgeProp> {
  const leftEdge = edgePosition.x === 0
  const rightEdge = edgePosition.x === 1
  const topEdge = edgePosition.y === 0
  const bottomEdge = edgePosition.y === 1
  return stripNulls([
    leftEdge ? 'left' : null,
    rightEdge ? 'right' : null,
    topEdge ? 'top' : null,
    bottomEdge ? 'bottom' : null,
  ])
}

export interface ChangeBoundsResult {
  commands: Array<CanvasCommand>
  intendedBounds: Array<CanvasFrameAndTarget>
}

export function changeBounds(
  projectContents: ProjectContentTreeRoot,
  startingMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
  originalFrame: CanvasRectangle,
  originalIntendedBounds: Array<CanvasFrameAndTarget>,
  edgePosition: EdgePosition,
  movement: CanvasVector,
): ChangeBoundsResult {
  let commands: Array<CanvasCommand> = []
  let intendedBounds: Array<CanvasFrameAndTarget> = originalIntendedBounds

  selectedElements.forEach((selectedElement) => {
    const element = withUnderlyingTarget(selectedElement, projectContents, null, (_, e) => e)

    const elementMetadata = MetadataUtils.findElementByElementPath(
      startingMetadata,
      selectedElement,
    )
    const elementParentBounds =
      elementMetadata?.specialSizeMeasurements.immediateParentBounds ?? null
    const elementParentFlexDirection =
      elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null
    const elementGlobalFrame = nullIfInfinity(elementMetadata?.globalFrame ?? null)

    if (element != null && isJSXElement(element)) {
      const elementResult = createResizeCommands(
        element,
        selectedElement,
        edgePosition,
        movement,
        elementGlobalFrame,
        elementParentBounds,
        elementParentFlexDirection,
      )
      commands.push(...elementResult.commands)
      if (elementResult.intendedBounds != null) {
        intendedBounds = addOrMergeIntendedBounds(
          intendedBounds,
          originalFrame,
          elementResult.intendedBounds,
        )
      }
    }
  })

  return {
    commands: commands,
    intendedBounds: intendedBounds,
  }
}

export function resizeInspectorStrategy(
  projectContents: ProjectContentTreeRoot,
  originalFrame: CanvasRectangle,
  edgePosition: EdgePosition,
  movement: CanvasVector,
): InspectorStrategy {
  return {
    name: 'Resize by pixels',
    strategy: (
      metadata: ElementInstanceMetadataMap,
      selectedElements: Array<ElementPath>,
      _elementPathTree: ElementPathTrees,
      _allElementProps: AllElementProps,
    ): Array<CanvasCommand> | null => {
      let commands: Array<CanvasCommand> = []
      const changeBoundsResult = changeBounds(
        projectContents,
        metadata,
        selectedElements,
        originalFrame,
        [],
        edgePosition,
        movement,
      )
      commands.push(...changeBoundsResult.commands)
      commands.push(
        pushIntendedBoundsAndUpdateGroups(
          changeBoundsResult.intendedBounds.map((b) => pushIntendedBoundsGroup(b.target, b.frame)),
          'starting-metadata',
        ),
      )
      commands.push(setElementsToRerenderCommand(selectedElements))
      return commands
    },
  }
}

export function directResizeInspectorStrategy(
  projectContents: ProjectContentTreeRoot,
  widthOrHeight: 'width' | 'height',
  newPixelValue: number,
): InspectorStrategy {
  return {
    name: 'Resize to pixel size',
    strategy: (
      metadata: ElementInstanceMetadataMap,
      selectedElements: Array<ElementPath>,
      _elementPathTree: ElementPathTrees,
      _allElementProps: AllElementProps,
    ): Array<CanvasCommand> | null => {
      let commands: Array<CanvasCommand> = []
      for (const selectedElement of selectedElements) {
        const edgePosition: EdgePosition =
          widthOrHeight === 'width' ? EdgePositionRight : EdgePositionBottom
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, selectedElement)
        const originalFrame = elementMetadata?.globalFrame
        const defaultedOriginalFrame: CanvasRectangle =
          originalFrame == null || isInfinityRectangle(originalFrame)
            ? zeroCanvasRect
            : originalFrame
        const movement: CanvasVector = canvasVector({
          x: widthOrHeight === 'width' ? newPixelValue - defaultedOriginalFrame.width : 0,
          y: widthOrHeight === 'height' ? newPixelValue - defaultedOriginalFrame.height : 0,
        })
        const changeBoundsResult = changeBounds(
          projectContents,
          metadata,
          [selectedElement],
          defaultedOriginalFrame,
          [],
          edgePosition,
          movement,
        )
        commands.push(...changeBoundsResult.commands)
        commands.push(
          pushIntendedBoundsAndUpdateGroups(
            changeBoundsResult.intendedBounds.map((b) =>
              pushIntendedBoundsGroup(b.target, b.frame),
            ),
            'starting-metadata',
          ),
        )
      }
      commands.push(setElementsToRerenderCommand(selectedElements))
      return commands
    },
  }
}
