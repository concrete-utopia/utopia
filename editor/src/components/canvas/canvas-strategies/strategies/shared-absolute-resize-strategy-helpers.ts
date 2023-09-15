import { styleStringInArray } from '../../../../utils/common-constants'
import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../../core/layout/layout-helpers-new'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isRight, right } from '../../../../core/shared/either'
import type {
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasRectangle, CanvasVector } from '../../../../core/shared/math-utils'
import { canvasPoint, pointDifference } from '../../../../core/shared/math-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import type { CanvasFrameAndTarget, EdgePosition } from '../../canvas-types'
import { pickPointOnRect, snapPoint } from '../../canvas-utils'
import type { AdjustCssLengthProperties } from '../../commands/adjust-css-length-command'
import {
  adjustCssLengthProperties,
  lengthPropertyToAdjust,
} from '../../commands/adjust-css-length-command'
import { pointGuidelineToBoundsEdge } from '../../controls/guideline-helpers'
import type { GuidelineWithSnappingVectorAndPointsOfRelevance } from '../../guideline'
import type { AbsolutePin, IsCenterBased } from './resize-helpers'
import { resizeBoundingBox } from './resize-helpers'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'

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
          frame: resizeBoundingBox(
            elementGlobalFrame,
            drag,
            edgePosition,
            null,
            'non-center-based',
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

export function runLegacyAbsoluteResizeSnapping(
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  draggedCorner: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  lockedAspectRatio: number | null,
  centerBased: IsCenterBased,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
): {
  snapDelta: CanvasVector
  snappedBoundingBox: CanvasRectangle
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
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
  )

  const snapDelta = pointDifference(draggedPointMovedWithoutSnap, snappedPointOnCanvas)
  const snappedBounds = resizeBoundingBox(
    resizedBounds,
    snapDelta,
    draggedCorner,
    lockedAspectRatio,
    centerBased,
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
