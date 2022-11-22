import { Sides } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { defaultEither, foldEither, isLeft, isRight, right } from '../../../../core/shared/either'
import {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  isIntrinsicElement,
  isJSXElement,
  JSXAttributes,
  JSXElement,
  jsxElementName,
  jsxElementNameEquals,
} from '../../../../core/shared/element-template'
import {
  CanvasPoint,
  canvasVector,
  CanvasVector,
  clamp,
  product,
  Size,
  size,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifiers } from '../../../../utils/modifiers'
import { AllElementProps } from '../../../editor/store/editor-state'
import {
  CSSBorderRadius,
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
  BorderRadiusControlMinimumForDisplay,
  BorderRadiusCorner,
  BorderRadiusSides,
  maxBorderRadius,
} from '../../border-radius-control-utils'
import { CanvasCommand } from '../../commands/commands'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { BorderRadiusControl } from '../../controls/select-mode/border-radius-control'
import {
  canShowCanvasPropControl,
  cssNumberEqual,
  cssNumberWithRenderedValue,
  CSSNumberWithRenderedValue,
  getPropertyFromStyle,
  measurementBasedOnOtherMeasurement,
  precisionFromModifiers,
  shouldShowControls,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import { CanvasStrategyFactory, onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const SetBorderRadiusStrategyId = 'SET_BORDER_RADIUS_STRATEGY'

const AllSides: Array<keyof Sides> = ['bottom', 'left', 'right', 'top']

export const setBorderRadiusStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
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

  const elementSize = sizeFromElement(element)

  const canShowBorderRadiusControls = canShowCanvasPropControl(
    canvasState.projectContents,
    canvasState.openFile ?? null,
    element,
    canvasState.scale,
  ).has('borderRadius')
  if (!canShowBorderRadiusControls) {
    return null
  }

  const borderRadius = borderRadiusFromElementProps(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    selectedElement,
  )

  if (borderRadius == null) {
    return null
  }

  if (borderRadius.source === 'code') {
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
            borderRadius: borderRadius.borderRadius,
            showIndicatorOnCorner: null,
            disabled: true,
          },
          key: 'border-radius-handle',
          show: 'visible-except-when-other-strategy-is-active',
        }),
      ],
      apply: () => emptyStrategyApplicationResult,
    }
  }

  const borderRadiusAdjustData = borderRadiusAdjustDataFromInteractionSession(interactionSession)

  const { commands, updatedBorderRadius } = setBorderRadiusStrategyRunResult(
    borderRadius,
    borderRadiusAdjustData,
    elementSize,
    canvasState.scale,
  )

  const mode: BorderRadiusAdjustMode =
    borderRadius.mode === 'individual' || borderRadiusAdjustData?.modifiers.cmd === true
      ? 'individual'
      : 'all'

  return {
    id: SetBorderRadiusStrategyId,
    name: 'Set border radius',
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'BORDER_RADIUS_RESIZE_HANDLE', 1),
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
      return Math.floor(product(drag, canvasVector({ x: 1, y: 1 })) / 2)
    case 'tr':
      return Math.floor(product(drag, canvasVector({ x: -1, y: 1 })) / 2)
    case 'bl':
      return Math.floor(product(drag, canvasVector({ x: 1, y: -1 })) / 2)
    case 'br':
      return Math.floor(product(drag, canvasVector({ x: -1, y: -1 })) / 2)
    default:
      assertNever(corner)
  }
}

interface BorderRadiusData<T> {
  mode: BorderRadiusAdjustMode
  source: 'code' | 'props'
  borderRadius: BorderRadiusSides<T>
}

function borderRadiusFromElementProps(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): BorderRadiusData<CSSNumberWithRenderedValue> | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

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

  const radiusFromProps = optionalMap(
    ({ type, sides }) => ({
      type: type,
      sides: measurementFromBorderRadius(renderedValueSides, sides),
    }),
    borderRadiusFromProps(jsxElement.props),
  )

  const radiusFromAllElementProps = optionalMap(
    ({ type, sides }) => ({
      type: type,
      sides: measurementFromBorderRadius(renderedValueSides, sides),
    }),
    borderRadiusFromAllElementProps(allElementProps, elementPath),
  )
  const measurementsNonZero = AllSides.some((c) => (renderedValueSides[c] ?? 0) > 0)

  const elementIsIntrinsicElementOrScene =
    isIntrinsicElement(jsxElement.name) ||
    jsxElementNameEquals(jsxElement.name, jsxElementName('Scene', []))

  if (
    !(
      elementIsIntrinsicElementOrScene ||
      shouldShowControls({
        propAvailableFromStyle: radiusFromProps != null,
        measurementsNonZero: measurementsNonZero,
      })
    )
  ) {
    return null
  }

  if (radiusFromProps == null) {
    if (!elementIsIntrinsicElementOrScene) {
      return {
        mode: 'all',
        source: 'props',
        borderRadius: borderRadiusSidesFromValue(unitlessCSSNumberWithRenderedValue(0)),
      }
    }
    return null
  }

  if (radiusFromProps != null) {
    const borderRadiusUpperLimit = maxBorderRadius(sizeFromElement(element))

    return {
      mode: radiusFromProps?.type === 'sides' ? 'individual' : 'all',
      source: 'props',
      borderRadius: mapBorderRadiusSides(
        (sides) => adjustBorderRadius({ min: 0, max: borderRadiusUpperLimit }, sides),
        radiusFromProps.sides,
      ),
    }
  }

  if (radiusFromAllElementProps != null) {
    return {
      mode: radiusFromAllElementProps?.type === 'sides' ? 'individual' : 'all',
      source: 'code',
      borderRadius: radiusFromAllElementProps.sides,
    }
  }

  return null
}

function borderRadiusFromAllElementProps(
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): BorderRadiusFromProps | null {
  const borderRadius: BorderRadiusSides<CSSNumber> | null = optionalMap(
    (prop: CSSBorderRadius) =>
      foldEither(
        (b) => borderRadiusSidesFromValue(b),
        (b) => b,
        prop,
      ),
    getPropertyFromStyle(allElementProps, elementPath, 'borderRadius'),
  )

  const borderTopLeftRadius = getPropertyFromStyle(
    allElementProps,
    elementPath,
    'borderTopLeftRadius',
  )
  const borderTopRightRadius = getPropertyFromStyle(
    allElementProps,
    elementPath,
    'borderTopRightRadius',
  )
  const borderBottomLeftRadius = getPropertyFromStyle(
    allElementProps,
    elementPath,
    'borderBottomLeftRadius',
  )
  const borderBottomRightRadius = getPropertyFromStyle(
    allElementProps,
    elementPath,
    'borderBottomRightRadius',
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
        tl: borderTopLeftRadius ?? borderRadius?.tl ?? cssNumber(0),
        tr: borderTopRightRadius ?? borderRadius?.tr ?? cssNumber(0),
        bl: borderBottomLeftRadius ?? borderRadius?.bl ?? cssNumber(0),
        br: borderBottomRightRadius ?? borderRadius?.br ?? cssNumber(0),
      },
    }
  }

  if (borderRadius != null) {
    return {
      type: 'borderRadius',
      sides: borderRadius,
    }
  }

  return null
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
    const { tl, tr, bl, br } = simpleBorderRadius
    const allSidesEqual = [tr, bl, br].every((c) => cssNumberEqual(tl, c))

    return {
      type: allSidesEqual ? 'borderRadius' : 'sides',
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
  return size(element.globalFrame?.width ?? 0, element.globalFrame?.height ?? 0)
}

function measurementFromBorderRadius(
  sides: Sides,
  borderRadius: BorderRadiusSides<CSSNumber>,
): BorderRadiusSides<CSSNumberWithRenderedValue> {
  return {
    tl: cssNumberWithRenderedValue(borderRadius.tl, sides.top ?? 0),
    tr: cssNumberWithRenderedValue(borderRadius.tr, sides.right ?? 0),
    bl: cssNumberWithRenderedValue(borderRadius.bl, sides.bottom ?? 0),
    br: cssNumberWithRenderedValue(borderRadius.br, sides.left ?? 0),
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
  scale: number,
  borderRadiusAdjustData: BorderRadiusAdjustData | null,
) {
  return (borderRadius: CSSNumberWithRenderedValue) => {
    const borderRadiusMaxed = Math.max(
      borderRadius.renderedValuePx,
      BorderRadiusControlMinimumForDisplay(scale),
    )
    const borderRadiusValue =
      borderRadiusAdjustData == null ? borderRadius.renderedValuePx : borderRadiusMaxed

    const dragDelta = clamp(
      -borderRadiusValue,
      maxBorderRadius(elementSize) - borderRadiusValue,
      optionalMap(({ drag, corner }) => deltaFromDrag(drag, corner), borderRadiusAdjustData) ?? 0,
    )

    const borderRadiusOffset = borderRadiusValue + dragDelta

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
  scale: number,
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
      scale,
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
    updateBorderRadiusFn(elementSize, scale, borderRadiusAdjustData),
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
