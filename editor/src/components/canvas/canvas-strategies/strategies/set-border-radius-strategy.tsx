import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isLeft, isRight, right } from '../../../../core/shared/either'
import { ElementInstanceMetadata, isJSXElement } from '../../../../core/shared/element-template'
import {
  canvasPoint,
  CanvasPoint,
  canvasVector,
  CanvasVector,
  clamp,
  Size,
  size,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { borderRadiusOffsetPx } from '../../border-radius-utis'
import {
  EdgePosition,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
  EdgePositionTopLeft,
  EdgePositionTopRight,
} from '../../canvas-types'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { BorderRadiusControl } from '../../controls/select-mode/border-radius-control'
import {
  CSSNumberWithRenderedValue,
  measurementBasedOnOtherMeasurement,
} from '../../controls/select-mode/controls-common'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  controlWithProps,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const SetBorderRadiusStrategyId = 'SET_BORDER_RADIUS_STRATEGY'
const StylePaddingProp = stylePropPathMappingFn('borderRadius', ['style'])

export const setBorderRadiusStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  if (
    interactionSession != null &&
    !(
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BORDER_RADIUS_RESIZE_HANDLE'
    )
  ) {
    // We don't want to include this in the strategy picker if any other interaction is active
    return null
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  const element = MetadataUtils.findElementByElementPath(
    canvasState.startingMetadata,
    selectedElement,
  )
  if (element == null) {
    return null
  }

  const borderRadius = borderRadiusFromElement(element)
  if (borderRadius == null) {
    return null
  }

  const elementSize = sizeFromElement(element)
  const borderRadiusAdjustData = borderRadiusAdjustDataFromInteractionSession(interactionSession)

  const dragDelta = clamp(
    -borderRadius.renderedValuePx,
    Math.min(elementSize.width / 2, elementSize.height / 2) - borderRadius.renderedValuePx,
    optionalMap(
      ({ drag, edgePosition }) => deltaFromDrag(drag, edgePosition),
      borderRadiusAdjustData,
    ) ?? 0,
  )

  const borderRadiusOffset = borderRadiusOffsetPx(
    dragDelta != 0,
    borderRadius.renderedValuePx + dragDelta,
  )

  const updatedBorderRadius = measurementBasedOnOtherMeasurement(borderRadius, borderRadiusOffset)

  return {
    id: SetBorderRadiusStrategyId,
    name: 'Set border radius',
    fitness: 1,
    controlsToRender: [
      controlWithProps({
        control: BorderRadiusControl,
        props: {
          selectedElement: selectedElement,
          elementSize: elementSize,
          borderRadius: updatedBorderRadius,
          showIndicatorOnEdge: borderRadiusAdjustData?.edgePosition ?? null,
        },
        key: 'border-radius-handle',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    apply: () =>
      strategyApplicationResult([
        setProperty(
          'always',
          selectedElement,
          StylePaddingProp,
          printCSSNumber(updatedBorderRadius.value, null),
        ),
        setElementsToRerenderCommand(selectedElements),
      ]),
  }
}

interface BorderRadiusAdjustData {
  drag: CanvasVector
  dragStart: CanvasPoint
  edgePosition: EdgePosition
}

function borderRadiusAdjustDataFromInteractionSession(
  interactionSession: InteractionSession | null,
): BorderRadiusAdjustData | null {
  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'BORDER_RADIUS_RESIZE_HANDLE'
  ) {
    return null
  }
  return {
    drag: interactionSession.interactionData.drag ?? canvasVector({ x: 0, y: 0 }),
    dragStart: interactionSession.interactionData.dragStart,
    edgePosition: interactionSession.activeControl.edgePosition,
  }
}

function deltaFromDrag(drag: CanvasVector, edgePosition: EdgePosition): number {
  const { x, y } = edgePosition
  if (x === EdgePositionTopLeft.x && y === EdgePositionTopLeft.y) {
    return Math.max(drag.x, drag.y)
  }

  if (x === EdgePositionTopRight.x && y === EdgePositionTopRight.y) {
    return Math.max(-drag.x, drag.y)
  }

  if (x === EdgePositionBottomLeft.x && y === EdgePositionBottomLeft.y) {
    return Math.max(drag.x, -drag.y)
  }

  if (x === EdgePositionBottomRight.x && y === EdgePositionBottomRight.y) {
    return Math.max(-drag.x, -drag.y)
  }

  return 0
}

function indicatorPositionFromDrag(
  dragStart: CanvasPoint,
  offset: number,
  edgePosition: EdgePosition,
  scale: number,
): CanvasPoint {
  const pointWithOffset = (point: CanvasPoint) =>
    canvasPoint({ x: point.x + 15 / scale, y: point.y + 15 / scale })

  const { x, y } = edgePosition
  if (x === EdgePositionTopLeft.x && y === EdgePositionTopLeft.y) {
    return pointWithOffset(canvasPoint({ x: dragStart.x + offset, y: dragStart.y + offset }))
  }

  if (x === EdgePositionTopRight.x && y === EdgePositionTopRight.y) {
    return pointWithOffset(canvasPoint({ x: dragStart.x - offset, y: dragStart.y + offset }))
  }

  if (x === EdgePositionBottomLeft.x && y === EdgePositionBottomLeft.y) {
    return pointWithOffset(canvasPoint({ x: dragStart.x + offset, y: dragStart.y - offset }))
  }

  if (x === EdgePositionBottomRight.x && y === EdgePositionBottomRight.y) {
    return pointWithOffset(canvasPoint({ x: dragStart.x - offset, y: dragStart.y - offset }))
  }

  return pointWithOffset(dragStart)
}

function borderRadiusFromElement(
  element: ElementInstanceMetadata,
): CSSNumberWithRenderedValue | null {
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const renderedValuePx = element.specialSizeMeasurements.borderRadius
  if (renderedValuePx == null) {
    return null
  }

  const borderRadius = getLayoutProperty('borderRadius', right(element.element.value.props), [
    'style',
  ])
  if (isLeft(borderRadius) || borderRadius.value == null || isRight(borderRadius.value)) {
    return null
  }

  return measurementBasedOnOtherMeasurement(
    {
      value: borderRadius.value.value,
      renderedValuePx: renderedValuePx,
    },
    Math.max(renderedValuePx, 20),
  )
}

function sizeFromElement(element: ElementInstanceMetadata): Size {
  return size(
    element.specialSizeMeasurements.clientWidth,
    element.specialSizeMeasurements.clientHeight,
  )
}
