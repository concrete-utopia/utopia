import { Sides } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { defaultEither, isLeft, isRight, right } from '../../../../core/shared/either'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXAttributes,
} from '../../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasVector,
  CanvasVector,
  clamp,
  Size,
  size,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { Modifiers } from '../../../../utils/modifiers'
import {
  cssNumber,
  CSSNumber,
  ParsedCSSProperties,
  ParsedCSSPropertiesKeys,
  printCSSNumber,
} from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import {
  BorderRadiusAdjustMode,
  borderRadiusOffsetPx,
  BorderRadiusSides,
  BorderRadiusThreshold,
  maxBorderRadius,
} from '../../border-radius-utis'
import {
  EdgePosition,
  EdgePositionBottomLeft,
  EdgePositionBottomRight,
  EdgePositionTopLeft,
  EdgePositionTopRight,
} from '../../canvas-types'
import { CanvasCommand } from '../../commands/commands'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { BorderRadiusControl } from '../../controls/select-mode/border-radius-control'
import {
  cssNumberWithRenderedValue,
  CSSNumberWithRenderedValue,
  measurementBasedOnOtherMeasurement,
  precisionFromModifiers,
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
const StylePaddingProp = <P extends ParsedCSSPropertiesKeys>(p: P) =>
  stylePropPathMappingFn(p, ['style'])

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

  const { commands, updatedBorderRadius, borderRadiusValueForIndicator } =
    setBorderRadiusStrategyRunResult(borderRadius, borderRadiusAdjustData, elementSize)

  return {
    id: SetBorderRadiusStrategyId,
    name: 'Set border radius',
    fitness: 1,
    controlsToRender: [
      controlWithProps({
        control: BorderRadiusControl,
        props: {
          mode: borderRadius.mode,
          selectedElement: selectedElement,
          elementSize: elementSize,
          borderRadius: updatedBorderRadius,
          indicatorValue: borderRadiusValueForIndicator,
          showIndicatorOnEdge: borderRadiusAdjustData?.edgePosition ?? null,
        },
        key: 'border-radius-handle',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    apply: () =>
      strategyApplicationResult([
        commands(selectedElement),
        setElementsToRerenderCommand(selectedElements),
      ]),
  }
}

interface BorderRadiusAdjustData {
  drag: CanvasVector
  dragStart: CanvasPoint
  edgePosition: EdgePosition
  modifiers: Modifiers
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
    modifiers: interactionSession.interactionData.modifiers,
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

interface BorderRadiusData {
  mode: BorderRadiusAdjustMode
  actualBorderRadius: BorderRadiusSides<CSSNumberWithRenderedValue>
  adjustedBorderRadius: BorderRadiusSides<CSSNumberWithRenderedValue>
}

function borderRadiusFromElement(element: ElementInstanceMetadata): BorderRadiusData | null {
  if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
    return null
  }

  const renderedValueSides = element.specialSizeMeasurements.borderRadius
  if (renderedValueSides == null) {
    return null
  }

  const fromProps = borderRadiusFromProps(element.element.value.props)
  const borderRadius = optionalMap(
    (radius) => measurementFromBorderRadius(renderedValueSides, radius),
    fromProps,
  )

  if (borderRadius == null) {
    return null
  }

  const borderRadiusUpperLimit = maxBorderRadius(
    size(element.specialSizeMeasurements.clientWidth, element.specialSizeMeasurements.clientHeight),
  )

  return {
    mode: fromProps?.type === 'sides' ? 'individual' : 'all',
    actualBorderRadius: borderRadius,
    adjustedBorderRadius: mapBorderRadiusSides(
      (n) => adjustBorderRadius({ min: BorderRadiusThreshold, max: borderRadiusUpperLimit }, n),
      borderRadius,
    ),
  }
}

type BorderRadiusFromProps =
  | { type: 'sides'; sides: BorderRadiusSides<CSSNumber> }
  | { type: 'borderRadius'; borderRadius: CSSNumber }

function borderRadiusFromProps(props: JSXAttributes): BorderRadiusFromProps | null {
  const borderRadius = getLayoutProperty('borderRadius', right(props), ['style'])
  if (isRight(borderRadius) && borderRadius.value != null) {
    return isLeft(borderRadius.value)
      ? { type: 'borderRadius', borderRadius: borderRadius.value.value }
      : { type: 'sides', sides: borderRadius.value.value }
  }

  const borderTopLeftRadius = defaultEither(
    null,
    getLayoutProperty('borderTopLeftRadius', right(props), ['style']),
  )
  const borderTopRightRadius = defaultEither(
    null,
    getLayoutProperty('borderTopRightRadius', right(props), ['style']),
  )
  const borderBottomLeftRadius = defaultEither(
    null,
    getLayoutProperty('borderBottomLeftRadius', right(props), ['style']),
  )
  const borderBottomRightRadius = defaultEither(
    null,
    getLayoutProperty('borderBottomRightRadius', right(props), ['style']),
  )

  if (
    borderTopLeftRadius != null ||
    borderTopRightRadius != null ||
    borderBottomLeftRadius != null ||
    borderBottomRightRadius != null
  ) {
    return {
      type: 'sides',
      sides: {
        tl: borderTopLeftRadius ?? cssNumber(0),
        tr: borderTopRightRadius ?? cssNumber(0),
        bl: borderBottomLeftRadius ?? cssNumber(0),
        br: borderBottomRightRadius ?? cssNumber(0),
      },
    }
  }

  return null
}

function sizeFromElement(element: ElementInstanceMetadata): Size {
  return size(
    element.specialSizeMeasurements.clientWidth,
    element.specialSizeMeasurements.clientHeight,
  )
}

function measurementFromBorderRadius(
  sides: Sides,
  borderRadius: BorderRadiusFromProps,
): BorderRadiusSides<CSSNumberWithRenderedValue> | null {
  if (borderRadius.type === 'borderRadius' && sides.top != null) {
    return borderRadiusSidesFromValue({
      renderedValuePx: sides.top,
      value: borderRadius.borderRadius,
    })
  }

  if (borderRadius.type === 'sides') {
    return {
      tl: cssNumberWithRenderedValue(borderRadius.sides.tl, sides.top ?? 0),
      tr: cssNumberWithRenderedValue(borderRadius.sides.tr, sides.right ?? 0),
      bl: cssNumberWithRenderedValue(borderRadius.sides.bl, sides.bottom ?? 0),
      br: cssNumberWithRenderedValue(borderRadius.sides.br, sides.left ?? 0),
    }
  }

  return null
}

function mapBorderRadiusSides<T, U>(
  f: (_: T) => U,
  sides: BorderRadiusSides<T>,
): BorderRadiusSides<U> {
  return {
    tl: f(sides.tl),
    tr: f(sides.tr),
    bl: f(sides.bl),
    br: f(sides.br),
  }
}

function borderRadiusSidesFromValue<T>(radius: T): BorderRadiusSides<T> {
  return {
    tl: radius,
    tr: radius,
    bl: radius,
    br: radius,
  }
}

interface MinMax {
  min: number
  max: number
}

function adjustBorderRadius(
  { min, max }: MinMax,
  borderRadius: CSSNumberWithRenderedValue,
): CSSNumberWithRenderedValue {
  return measurementBasedOnOtherMeasurement(
    borderRadius,
    clamp(min, max, borderRadius.renderedValuePx),
    'precise',
  )
}

interface SetBorderRadiusStrategyRunResult {
  commands: (target: ElementPath) => CanvasCommand
  updatedBorderRadius: BorderRadiusSides<CSSNumberWithRenderedValue>
  borderRadiusValueForIndicator: CSSNumber
}

interface BoderRadiusCorner {
  actual: CSSNumberWithRenderedValue
  adjusted: CSSNumberWithRenderedValue
}

function borderRadiusFromData(
  data: BorderRadiusData,
  edgePosition: EdgePosition,
): BoderRadiusCorner {
  if (data.mode === 'all') {
    return { actual: data.actualBorderRadius.tl, adjusted: data.adjustedBorderRadius.tl }
  }

  const { x, y } = edgePosition
  if (x === EdgePositionTopLeft.x && y === EdgePositionTopLeft.y) {
    return { actual: data.actualBorderRadius.tl, adjusted: data.adjustedBorderRadius.tl }
  }

  if (x === EdgePositionTopRight.x && y === EdgePositionTopRight.y) {
    return { actual: data.actualBorderRadius.tr, adjusted: data.adjustedBorderRadius.tr }
  }

  if (x === EdgePositionBottomLeft.x && y === EdgePositionBottomLeft.y) {
    return { actual: data.actualBorderRadius.bl, adjusted: data.adjustedBorderRadius.bl }
  }

  if (x === EdgePositionBottomRight.x && y === EdgePositionBottomRight.y) {
    return { actual: data.actualBorderRadius.br, adjusted: data.adjustedBorderRadius.br }
  }

  return { actual: data.actualBorderRadius.tl, adjusted: data.adjustedBorderRadius.tl }
}

function longhandFromEdgePosition(
  data: BorderRadiusData,
  edgePosition: EdgePosition,
): keyof ParsedCSSProperties {
  if (data.mode === 'individual') {
    const { x, y } = edgePosition
    if (x === EdgePositionTopLeft.x && y === EdgePositionTopLeft.y) {
      return 'borderTopLeftRadius'
    }

    if (x === EdgePositionTopRight.x && y === EdgePositionTopRight.y) {
      return 'borderTopRightRadius'
    }

    if (x === EdgePositionBottomLeft.x && y === EdgePositionBottomLeft.y) {
      return 'borderBottomLeftRadius'
    }

    if (x === EdgePositionBottomRight.x && y === EdgePositionBottomRight.y) {
      return 'borderBottomRightRadius'
    }
  }
  return 'borderRadius'
}

function setBorderRadiusStrategyRunResult(
  data: BorderRadiusData,
  borderRadiusAdjustData: BorderRadiusAdjustData | null,
  elementSize: Size,
): SetBorderRadiusStrategyRunResult {
  const edgePosition = borderRadiusAdjustData?.edgePosition ?? EdgePositionTopLeft
  const { actual, adjusted } = borderRadiusFromData(data, edgePosition)

  const dragDelta = clamp(
    -adjusted.renderedValuePx,
    maxBorderRadius(elementSize) - adjusted.renderedValuePx,
    optionalMap(({ drag, edgePosition: ep }) => deltaFromDrag(drag, ep), borderRadiusAdjustData) ??
      0,
  )

  const borderRadiusOffset = borderRadiusOffsetPx(
    dragDelta != 0,
    adjusted.renderedValuePx + dragDelta,
  )

  const precision =
    optionalMap(({ modifiers }) => precisionFromModifiers(modifiers), borderRadiusAdjustData) ??
    'precise'
  const updatedBorderRadius = measurementBasedOnOtherMeasurement(
    adjusted,
    borderRadiusOffset,
    precision,
  )

  const isDragging = borderRadiusAdjustData != null

  const borderRadiusValueForIndicator = (isDragging ? updatedBorderRadius : actual).value

  return {
    commands: setPropertyCommand(
      longhandFromEdgePosition(data, edgePosition),
      printCSSNumber(updatedBorderRadius.value, null),
    ),
    updatedBorderRadius: updatedBorderRadius,
    borderRadiusValueForIndicator: borderRadiusValueForIndicator,
  }
}

const setPropertyCommand =
  <P extends ParsedCSSPropertiesKeys>(prop: P, value: string | number) =>
  (target: ElementPath): CanvasCommand =>
    setProperty('always', target, StylePaddingProp(prop), value)
