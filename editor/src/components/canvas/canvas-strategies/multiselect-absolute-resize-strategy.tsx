import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { JSXElement } from '../../../core/shared/element-template'
import {
  boundingRectangleArray,
  canvasPoint,
  CanvasPoint,
  canvasRectangle,
  CanvasRectangle,
  offsetPoint,
  pointDifference,
  rectangleDifference,
  rectFromPointVector,
  roundTo,
  transformFrameUsingBoundingBox,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { getElementFromProjectContents } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { EdgePosition } from '../canvas-types'
import { pickPointOnRect } from '../canvas-utils'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { AbsoluteResizeControl } from '../controls/select-mode/absolute-resize-control'
import { AbsolutePin, hasAtLeastTwoPinsPerSide } from './absolute-resize-helpers'
import { CanvasStrategy } from './canvas-strategy-types'

export const multiselectAbsoluteResizeStrategy: CanvasStrategy = {
  id: 'MULTISELECT_ABSOLUTE_RESIZE',
  name: 'Multiselect Absolute Resize',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 1) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return (
          elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
          hasAtLeastTwoPinsPerSide(elementMetadata.props)
        )
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    { control: AbsoluteResizeControl, key: 'absolute-resize-control', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return multiselectAbsoluteResizeStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
    ) {
      const drag = interactionState.interactionData.drag
      const edgePosition = interactionState.activeControl.edgePosition

      const originalFrames = mapDropNulls(
        (path) => MetadataUtils.getFrameInCanvasCoords(path, sessionState.startingMetadata),
        canvasState.selectedElements,
      )
      const originalBoundingBox = boundingRectangleArray(originalFrames)
      if (originalBoundingBox != null) {
        const newBoundingBox = resizeBoundingBox(originalBoundingBox, drag, edgePosition)
        const commandsForSelectedElements = canvasState.selectedElements.flatMap(
          (selectedElement) => {
            const element = getElementFromProjectContents(
              selectedElement,
              canvasState.projectContents,
              canvasState.openFile,
            )
            const originalFrame = MetadataUtils.getFrameInCanvasCoords(
              selectedElement,
              sessionState.startingMetadata,
            )

            if (element == null || originalFrame == null) {
              return []
            }

            const newFrame = transformFrameUsingBoundingBox(
              newBoundingBox,
              originalBoundingBox,
              originalFrame,
            )
            const elementParentBounds =
              MetadataUtils.findElementByElementPath(sessionState.startingMetadata, selectedElement)
                ?.specialSizeMeasurements.immediateParentBounds ?? null

            return createResizeCommandsFromFrame(
              element,
              selectedElement,
              newFrame,
              originalFrame,
              elementParentBounds,
            )
          },
        )
        return [...commandsForSelectedElements, updateHighlightedViews('transient', [])]
      }
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}

function createResizeCommandsFromFrame(
  element: JSXElement,
  selectedElement: ElementPath,
  newFrame: CanvasRectangle,
  originalFrame: CanvasRectangle,
  elementParentBounds: CanvasRectangle | null,
): AdjustCssLengthProperty[] {
  const pins: Array<AbsolutePin> = ['top', 'left', 'width', 'height', 'bottom', 'right']
  return mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const value = getLayoutProperty(pin, right(element.props), ['style'])
    const rectangleDiff = rectangleDifference(originalFrame, newFrame)
    const delta = allPinsFromFrame(rectangleDiff)[pin]
    const roundedDelta = roundTo(delta, 0)
    if (isRight(value) && value.value != null && roundedDelta !== 0) {
      return adjustCssLengthProperty(
        'permanent',
        selectedElement,
        stylePropPathMappingFn(pin, ['style']),
        roundedDelta,
        horizontal ? elementParentBounds?.width : elementParentBounds?.height,
        true,
      )
    } else {
      return null
    }
  }, pins)
}

function resizeBoundingBox(
  boundingBox: CanvasRectangle,
  drag: CanvasPoint,
  edgePosition: EdgePosition,
): CanvasRectangle {
  let dragToUse = drag
  if (edgePosition.x === 0.5) {
    dragToUse = canvasPoint({ x: 0, y: drag.y })
  } else if (edgePosition.y === 0.5) {
    dragToUse = canvasPoint({ x: drag.x, y: 0 })
  }
  const startingCorner: EdgePosition = {
    x: 1 - edgePosition.x,
    y: 1 - edgePosition.y,
  } as EdgePosition
  const startingPoint = pickPointOnRect(boundingBox, startingCorner)
  const draggedCorner = pickPointOnRect(boundingBox, edgePosition)
  const newCorner = offsetPoint(draggedCorner, dragToUse)
  const newSizeVector = pointDifference(startingPoint, newCorner)
  const newFrameFromVector = rectFromPointVector(startingPoint, newSizeVector, false)

  if (edgePosition.x === 0.5) {
    return canvasRectangle({
      x: newFrameFromVector.x - boundingBox.width / 2,
      y: newFrameFromVector.y,
      width: boundingBox.width,
      height: newFrameFromVector.height,
    })
  } else if (edgePosition.y === 0.5) {
    return canvasRectangle({
      x: newFrameFromVector.x,
      y: newFrameFromVector.y - boundingBox.height / 2,
      width: newFrameFromVector.width,
      height: boundingBox.height,
    })
  } else {
    return newFrameFromVector
  }
}

function allPinsFromFrame(frame: CanvasRectangle): { [key: string]: number } {
  return {
    left: frame.x,
    top: frame.y,
    width: frame.width,
    height: frame.height,
    right: frame.x + frame.width,
    bottom: frame.y + frame.height,
  }
}
