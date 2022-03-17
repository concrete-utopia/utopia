import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import type { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  boundingRectangleArray,
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  offsetPoint,
  pointDifference,
  rectFromPointVector,
  rectFromTwoPoints,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { EdgePosition } from '../canvas-types'
import { isEdgePositionOnSide, pickPointOnRect, snapPoint } from '../canvas-utils'
import {
  adjustCssLengthProperty,
  AdjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { pointGuidelineToBoundsEdge } from '../controls/guideline-helpers'
import { GuidelineWithSnappingVector } from '../guideline'
import { InteractionCanvasState } from './canvas-strategy-types'
import { StrategyState } from './interaction-state'

export function getAbsoluteMoveCommandsForSelectedElement(
  selectedElement: ElementPath,
  drag: CanvasVector,
  canvasState: InteractionCanvasState,
  sessionState: StrategyState,
): Array<AdjustCssLengthProperty> {
  const element: JSXElement | null = getElementFromProjectContents(
    selectedElement,
    canvasState.projectContents,
    canvasState.openFile,
  )
  const elementParentBounds =
    MetadataUtils.findElementByElementPath(
      sessionState.startingMetadata, // TODO should this be using the current metadata?
      selectedElement,
    )?.specialSizeMeasurements.immediateParentBounds ?? null

  if (element == null) {
    return []
  }

  return createMoveCommandsForElement(element, selectedElement, drag, elementParentBounds)
}

function createMoveCommandsForElement(
  element: JSXElement,
  selectedElement: ElementPath,
  drag: CanvasVector,
  elementParentBounds: CanvasRectangle | null,
): AdjustCssLengthProperty[] {
  return mapDropNulls(
    (pin) => {
      const horizontal = isHorizontalPoint(
        // TODO avoid using the loaded FramePoint enum
        framePointForPinnedProp(pin),
      )
      const negative = pin === 'right' || pin === 'bottom'
      const value = getLayoutProperty(pin, right(element.props), ['style'])
      if (isRight(value) && value.value != null) {
        // TODO what to do about missing properties?
        return adjustCssLengthProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          (horizontal ? drag.x : drag.y) * (negative ? -1 : 1),
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
          true,
        )
      } else {
        return null
      }
    },
    ['top', 'bottom', 'left', 'right'] as const,
  )
}

export function getMultiselectBounds(
  jsxMetadata: ElementInstanceMetadataMap,
  selectedElements: Array<ElementPath>,
): CanvasRectangle | null {
  const frames = mapDropNulls((element) => {
    return MetadataUtils.getFrameInCanvasCoords(element, jsxMetadata)
  }, selectedElements)

  return boundingRectangleArray(frames)
}

export function resizeBoundingBox(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
): CanvasRectangle {
  let dragToUse = drag
  let cornerEdgePosition = edgePosition
  let startingCornerPosition = {
    x: 1 - edgePosition.x,
    y: 1 - edgePosition.y,
  } as EdgePosition
  if (isEdgePositionOnSide(edgePosition)) {
    if (edgePosition.x === 0.5) {
      dragToUse = canvasPoint({ x: 0, y: drag.y })
      startingCornerPosition = { x: 0, y: startingCornerPosition.y }
      cornerEdgePosition = { x: 1, y: edgePosition.y }
    } else if (edgePosition.y === 0.5) {
      dragToUse = canvasPoint({ x: drag.x, y: 0 })
      startingCornerPosition = { x: startingCornerPosition.x, y: 0 }
      cornerEdgePosition = { x: edgePosition.x, y: 1 }
    }
  }

  const startingPoint = pickPointOnRect(boundingBox, startingCornerPosition)
  const draggedCorner = pickPointOnRect(boundingBox, cornerEdgePosition)
  const newCorner = offsetPoint(draggedCorner, dragToUse)
  const newBoundingBox = rectFromTwoPoints(startingPoint, newCorner)
  return newBoundingBox
}

export function runLegacyAbsoluteResizeSnapping2(
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  draggedCorner: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  keepAspectRatio: boolean,
): {
  snapDelta: CanvasVector
  snappedBoundingBox: CanvasRectangle
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>
} {
  const oppositeCorner: EdgePosition = {
    x: 1 - draggedCorner.x,
    y: 1 - draggedCorner.y,
  } as EdgePosition

  const oppositePoint = pickPointOnRect(resizedBounds, oppositeCorner)
  const draggedPointMovedWithoutSnap = pickPointOnRect(resizedBounds, draggedCorner)

  const { snappedPointOnCanvas, guidelinesWithSnappingVector } = snapPoint(
    selectedElements,
    jsxMetadata,
    canvasScale,
    draggedPointMovedWithoutSnap,
    true,
    keepAspectRatio,
    draggedPointMovedWithoutSnap,
    oppositePoint,
    draggedCorner,
  )

  const snapDelta = pointDifference(draggedPointMovedWithoutSnap, snappedPointOnCanvas)
  const snappedBounds = resizeBoundingBox(resizedBounds, snapDelta, draggedCorner)

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
