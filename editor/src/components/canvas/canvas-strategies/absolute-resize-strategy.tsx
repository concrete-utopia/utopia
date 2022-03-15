import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
  offsetPoint,
  vectorDifference,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { EdgePosition } from '../canvas-types'
import { snapPoint } from '../canvas-utils'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { AbsoluteResizeControl } from '../controls/select-mode/absolute-resize-control'
import { GuidelineWithSnappingVector } from '../guideline'
import { CanvasStrategy } from './canvas-strategy-types'

type AbsolutePin = 'left' | 'top' | 'right' | 'bottom' | 'width' | 'height'

export const absoluteResizeStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_RESIZE',
  name: 'Absolute Resize',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length > 0) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    { control: AbsoluteResizeControl, key: 'absolute-resize-control', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteResizeStrategy.isApplicable(
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
      const snappedDrag = snapDrag(
        canvasState.selectedElements,
        sessionState.startingMetadata,
        canvasState.scale,
        interactionState.interactionData.dragStart,
        drag,
        true,
        false,
        null as any,
        null as any,
        edgePosition,
      )

      const commandsForSelectedElements = canvasState.selectedElements.flatMap(
        (selectedElement) => {
          const element: JSXElement | null = withUnderlyingTarget(
            selectedElement,
            canvasState.projectContents,
            {},
            canvasState.openFile,
            null,
            (_, e) => e,
          )
          const elementParentBounds =
            MetadataUtils.findElementByElementPath(
              sessionState.startingMetadata, // TODO should this be using the current metadata?
              selectedElement,
            )?.specialSizeMeasurements.immediateParentBounds ?? null

          if (element == null) {
            return []
          }

          return createResizeCommands(
            element,
            selectedElement,
            edgePosition,
            snappedDrag.snappedDragVector,
            elementParentBounds,
          )
        },
      )
      return [...commandsForSelectedElements, updateHighlightedViews('transient', [])]
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
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

function createResizeCommands(
  element: JSXElement,
  selectedElement: ElementPath,
  edgePosition: EdgePosition,
  drag: CanvasVector,
  elementParentBounds: CanvasRectangle | null,
): AdjustCssLengthProperty[] {
  const pins = pinsForEdgePosition(edgePosition)
  return mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const negative =
      pin === 'right' ||
      pin === 'bottom' ||
      (pin === 'width' && edgePosition.x === 0) ||
      (pin === 'height' && edgePosition.y === 0)
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
  }, pins)
}

function snapDrag(
  selectedViews: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
  dragStart: CanvasPoint,
  drag: CanvasVector,
  enableSnapping: boolean,
  keepAspectRatio: boolean,
  diagonalA: CanvasPoint,
  diagonalB: CanvasPoint,
  resizingFromPosition: EdgePosition | null,
): {
  snappedDragVector: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>
} {
  const pointToSnap: CanvasPoint = offsetPoint(dragStart, drag)
  const { snappedPointOnCanvas, guidelinesWithSnappingVector } = snapPoint(
    selectedViews,
    jsxMetadata,
    canvasScale,
    pointToSnap,
    enableSnapping,
    keepAspectRatio,
    diagonalA,
    diagonalB,
    resizingFromPosition,
  )
  const snappedDragVector = vectorDifference(dragStart, snappedPointOnCanvas)
  return {
    snappedDragVector: snappedDragVector,
    guidelinesWithSnappingVector: guidelinesWithSnappingVector,
  }
}
