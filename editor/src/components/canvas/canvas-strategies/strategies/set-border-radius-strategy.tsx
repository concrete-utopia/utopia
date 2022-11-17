import { Sides } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { defaultEither, foldEither, isLeft, isRight, right } from '../../../../core/shared/either'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXAttributes,
  JSXElement,
} from '../../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasVector,
  CanvasVector,
  clamp,
  magnitude,
  product,
  Size,
  size,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifiers } from '../../../../utils/modifiers'
import {
  CSSBorderRadiusIndividual,
  cssNumber,
  CSSNumber,
  ParsedCSSProperties,
  ParsedCSSPropertiesKeys,
  printCSSNumber,
} from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import {
  BorderRadiusAdjustMode,
  BorderRadiusCorner,
  BorderRadiusSides,
  maxBorderRadius,
} from '../../border-radius-control-utils'
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

  const { commands, updatedBorderRadius } = setBorderRadiusStrategyRunResult(
    borderRadius,
    borderRadiusAdjustData,
    elementSize,
  )

  const mode: BorderRadiusAdjustMode =
    borderRadius.mode === 'individual' || borderRadiusAdjustData?.modifiers.cmd === true
      ? 'individual'
      : 'all'

  return {
    id: SetBorderRadiusStrategyId,
    name: 'Set border radius',
    fitness: 1,
    controlsToRender: [
      controlWithProps({
        control: BorderRadiusControl,
        props: {
          mode: mode,
          selectedElement: selectedElement,
          elementSize: elementSize,
          borderRadius: updatedBorderRadius,
          showIndicatorOnCorner: borderRadiusAdjustData?.corner ?? null,
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
  corner: BorderRadiusCorner
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
    corner: interactionSession.activeControl.corner,
    modifiers: interactionSession.interactionData.modifiers,
  }
}

function deltaFromDrag(drag: CanvasVector, corner: BorderRadiusCorner): number {
  switch (corner) {
    case 'tl':
      return Math.floor(product(drag, canvasVector({ x: 1, y: 1 })))
    case 'tr':
      return Math.floor(product(drag, canvasVector({ x: -1, y: 1 })) / Math.SQRT2)
    case 'bl':
      return Math.floor(product(drag, canvasVector({ x: 1, y: -1 })) / Math.SQRT2)
    case 'br':
      return Math.floor(product(drag, canvasVector({ x: -1, y: -1 })) / Math.SQRT2)
    default:
      assertNever(corner)
  }
}

interface BorderRadiusData<T> {
  mode: BorderRadiusAdjustMode
  borderRadius: BorderRadiusSides<T>
}

function borderRadiusFromElement(
  element: ElementInstanceMetadata,
): BorderRadiusData<CSSNumberWithRenderedValue> | null {
  const jsxElement: JSXElement | null = foldEither(
    () => null,
    (e) => (isJSXElement(e) ? e : null),
    element.element,
  )

  if (jsxElement == null) {
    return null
  }

  const renderedValueSides = element.specialSizeMeasurements.borderRadius
  if (renderedValueSides == null) {
    return null
  }

  const fromProps = borderRadiusFromProps(jsxElement.props)
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
    borderRadius: mapBorderRadiusSides(
      (n) => adjustBorderRadius({ min: 0, max: borderRadiusUpperLimit }, n),
      borderRadius,
    ),
  }
}

interface BorderRadiusFromProps {
  type: 'sides' | 'borderRadius'
  sides: BorderRadiusSides<CSSNumber>
}

function borderRadiusFromProps(props: JSXAttributes): BorderRadiusFromProps | null {
  const simpleBorderRadius = simpleBorderRadiusFromProps(props)

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
        tl: borderTopLeftRadius ?? simpleBorderRadius?.tl ?? cssNumber(0),
        tr: borderTopRightRadius ?? simpleBorderRadius?.tr ?? cssNumber(0),
        bl: borderBottomLeftRadius ?? simpleBorderRadius?.bl ?? cssNumber(0),
        br: borderBottomRightRadius ?? simpleBorderRadius?.br ?? cssNumber(0),
      },
    }
  }

  if (simpleBorderRadius != null) {
    return {
      type: 'borderRadius',
      sides: simpleBorderRadius,
    }
  }

  return null
}

function simpleBorderRadiusFromProps(props: JSXAttributes): BorderRadiusSides<CSSNumber> | null {
  const borderRadius = getLayoutProperty('borderRadius', right(props), ['style'])
  if (isRight(borderRadius) && borderRadius.value != null) {
    return isLeft(borderRadius.value)
      ? borderRadiusSidesFromValue(borderRadius.value.value)
      : borderRadius.value.value
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
  return {
    tl: cssNumberWithRenderedValue(borderRadius.sides.tl, sides.top ?? 0),
    tr: cssNumberWithRenderedValue(borderRadius.sides.tr, sides.right ?? 0),
    bl: cssNumberWithRenderedValue(borderRadius.sides.bl, sides.bottom ?? 0),
    br: cssNumberWithRenderedValue(borderRadius.sides.br, sides.left ?? 0),
  }
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
}

interface BoderRadiusCorner {
  borderRadius: CSSNumberWithRenderedValue
  key: keyof CSSBorderRadiusIndividual
}

function borderRadiusFromData(
  data: BorderRadiusData<CSSNumberWithRenderedValue>,
  corner: BorderRadiusCorner,
): BoderRadiusCorner {
  if (data.mode === 'all') {
    return { borderRadius: data.borderRadius[corner], key: corner }
  }

  switch (corner) {
    case 'tl':
      return {
        borderRadius: data.borderRadius.tl,
        key: 'tl',
      }
    case 'tr':
      return {
        borderRadius: data.borderRadius.tr,
        key: 'tr',
      }
    case 'bl':
      return {
        borderRadius: data.borderRadius.bl,
        key: 'bl',
      }
    case 'br':
      return {
        borderRadius: data.borderRadius.br,
        key: 'br',
      }
    default:
      assertNever(corner)
  }
}

function longhandFromEdgePosition(
  mode: BorderRadiusAdjustMode,
  corner: BorderRadiusCorner,
): keyof ParsedCSSProperties {
  if (mode === 'individual') {
    switch (corner) {
      case 'tl':
        return 'borderTopLeftRadius'
      case 'tr':
        return 'borderTopRightRadius'
      case 'bl':
        return 'borderBottomLeftRadius'
      case 'br':
        return 'borderBottomRightRadius'
      default:
        assertNever(corner)
    }
  }
  return 'borderRadius'
}

function updateBorderRadiusFn(
  elementSize: Size,
  borderRadiusAdjustData: BorderRadiusAdjustData | null,
) {
  return (borderRadius: CSSNumberWithRenderedValue) => {
    const dragDelta = clamp(
      -borderRadius.renderedValuePx,
      maxBorderRadius(elementSize) - borderRadius.renderedValuePx,
      optionalMap(({ drag, corner }) => deltaFromDrag(drag, corner), borderRadiusAdjustData) ?? 0,
    )

    const borderRadiusOffset = borderRadius.renderedValuePx + dragDelta

    const precision =
      optionalMap(({ modifiers }) => precisionFromModifiers(modifiers), borderRadiusAdjustData) ??
      'precise'

    const updatedBorderRadius = measurementBasedOnOtherMeasurement(
      borderRadius,
      borderRadiusOffset,
      precision,
    )

    return updatedBorderRadius
  }
}

function setBorderRadiusStrategyRunResult(
  data: BorderRadiusData<CSSNumberWithRenderedValue>,
  borderRadiusAdjustData: BorderRadiusAdjustData | null,
  elementSize: Size,
): SetBorderRadiusStrategyRunResult {
  const edgePosition = borderRadiusAdjustData?.corner ?? 'br'

  const mode: BorderRadiusAdjustMode =
    data.mode === 'individual' || borderRadiusAdjustData?.modifiers.cmd === true
      ? 'individual'
      : 'all'

  if (mode === 'individual') {
    const { borderRadius, key } = borderRadiusFromData(data, edgePosition)
    const updatedBorderRadius = updateBorderRadiusFn(
      elementSize,
      borderRadiusAdjustData,
    )(borderRadius)

    return {
      commands: setStylePropertyCommand(
        longhandFromEdgePosition(mode, edgePosition),
        printCSSNumber(updatedBorderRadius.value, null),
      ),
      updatedBorderRadius: {
        ...data.borderRadius,
        [key]: updatedBorderRadius,
      },
    }
  }

  const allUpdated = mapBorderRadiusSides(
    updateBorderRadiusFn(elementSize, borderRadiusAdjustData),
    data.borderRadius,
  )

  return {
    commands: setStylePropertyCommand('borderRadius', printCSSNumber(allUpdated.tl.value, null)),
    updatedBorderRadius: allUpdated,
  }
}

const StylePaddingProp = <P extends ParsedCSSPropertiesKeys>(p: P) =>
  stylePropPathMappingFn(p, ['style'])

const setStylePropertyCommand =
  <P extends ParsedCSSPropertiesKeys>(prop: P, value: string | number) =>
  (target: ElementPath): CanvasCommand =>
    setProperty('always', target, StylePaddingProp(prop), value)
