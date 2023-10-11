import { styleStringInArray } from '../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { LayoutEdgeProp, LayoutPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { framePointForPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { mapDropNulls, stripNulls } from '../../../../core/shared/array-utils'
import { isRight, right } from '../../../../core/shared/either'
import type {
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  pointDifference,
  roundRectangleToNearestWhole,
} from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { type CanvasFrameAndTarget, type EdgePosition } from '../../canvas-types'
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
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'

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
    const element = MetadataUtils.getJsxElementChildFromMetadata(metadata, child)
    if (element == null || element.type !== 'JSX_ELEMENT') {
      return false
    }
    const hasForbiddenPin = pinsToExclude.some((p) => hasPin(p, element))
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
