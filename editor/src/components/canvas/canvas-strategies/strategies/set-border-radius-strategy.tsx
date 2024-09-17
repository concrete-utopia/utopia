import { styleStringInArray } from '../../../../utils/common-constants'
import type { Sides } from 'utopia-api/core'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { defaultEither, foldEither, right } from '../../../../core/shared/either'
import type {
  ElementInstanceMetadata,
  JSXAttributes,
} from '../../../../core/shared/element-template'
import {
  isIntrinsicElement,
  isJSXElement,
  jsxElementName,
  jsxElementNameEquals,
  modifiableAttributeIsAttributeNotFound,
} from '../../../../core/shared/element-template'
import type { CanvasPoint, CanvasVector, Size } from '../../../../core/shared/math-utils'
import {
  canvasVector,
  clamp,
  product,
  roundTo,
  size,
  zeroRectIfNullOrInfinity,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import type { Modifiers } from '../../../../utils/modifiers'
import type {
  CSSBorderRadiusIndividual,
  CSSNumber,
  ParsedCSSPropertiesKeys,
} from '../../../inspector/common/css-utils'
import { cssNumber, ParsedCSSProperties, printCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import type {
  BorderRadiusAdjustMode,
  BorderRadiusCorner,
  BorderRadiusSides,
} from '../../border-radius-control-utils'
import {
  BorderRadiusControlMinimumForDisplay,
  maxBorderRadius,
} from '../../border-radius-control-utils'
import { CSSCursor } from '../../canvas-types'
import type { CanvasCommand } from '../../commands/commands'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { BorderRadiusControl } from '../../controls/select-mode/border-radius-control'
import type { CSSNumberWithRenderedValue } from '../../controls/select-mode/controls-common'
import {
  canShowCanvasPropControl,
  cssNumberEqual,
  cssNumberWithRenderedValue,
  fallbackEmptyValue,
  measurementBasedOnOtherMeasurement,
  precisionFromModifiers,
  shouldShowControls,
  unitlessCSSNumberWithRenderedValue,
} from '../../controls/select-mode/controls-common'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  controlWithProps,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { deleteProperties } from '../../commands/delete-properties-command'
import { allElemsEqual } from '../../../../core/shared/array-utils'
import * as PP from '../../../../core/shared/property-path'
import { withUnderlyingTarget } from '../../../editor/store/editor-state'
import type { ProjectContentTreeRoot } from '../../../assets'
import { getModifiableJSXAttributeAtPath } from '../../../../core/shared/jsx-attribute-utils'
import { showToastCommand } from '../../commands/show-toast-command'
import { activeFrameTargetPath, setActiveFrames } from '../../commands/set-active-frames-command'

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

  const canShowBorderRadiusControls = canShowCanvasPropControl(
    canvasState.projectContents,
    selectedElement,
    canvasState.scale,
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    canvasState.propertyControlsInfo,
  ).has('borderRadius')
  if (!canShowBorderRadiusControls) {
    return null
  }

  const borderRadius = borderRadiusFromElement(element)
  if (borderRadius == null) {
    return null
  }

  const elementSize = sizeFromElement(element)
  const borderRadiusAdjustData = borderRadiusAdjustDataFromInteractionSession(interactionSession)

  const { commands } = setBorderRadiusStrategyRunResult(
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
    descriptiveLabel: 'Changing Border Radius',
    icon: {
      category: 'modalities',
      type: 'border-radius-large',
    },
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'BORDER_RADIUS_RESIZE_HANDLE', 1),
    controlsToRender: [
      controlWithProps({
        control: BorderRadiusControl,
        props: {
          mode: mode,
          selectedElement: selectedElement,
          elementSize: elementSize,
          showIndicatorOnCorner: borderRadiusAdjustData?.corner ?? null,
        },
        key: 'border-radius-handle',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    apply: () =>
      strategyApplicationResult([
        setCursorCommand(CSSCursor.Radius),
        ...commands(selectedElement),
        ...getAddOverflowHiddenCommands(selectedElement, canvasState.projectContents),
        setElementsToRerenderCommand(selectedElements),
        setActiveFrames(
          selectedElements.map((path) => ({
            action: 'set-radius',
            target: activeFrameTargetPath(path),
            source: zeroRectIfNullOrInfinity(
              MetadataUtils.getFrameInCanvasCoords(path, canvasState.startingMetadata),
            ),
          })),
        ),
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
      return roundTo(product(drag, canvasVector({ x: 1, y: 1 })) / 2)
    case 'tr':
      return roundTo(product(drag, canvasVector({ x: -1, y: 1 })) / 2)
    case 'bl':
      return roundTo(product(drag, canvasVector({ x: 1, y: -1 })) / 2)
    case 'br':
      return roundTo(product(drag, canvasVector({ x: -1, y: -1 })) / 2)
    default:
      assertNever(corner)
  }
}

interface BorderRadiusData<T> {
  mode: BorderRadiusAdjustMode
  borderRadius: BorderRadiusSides<T>
}

export function borderRadiusFromElement(
  element: ElementInstanceMetadata,
): BorderRadiusData<CSSNumberWithRenderedValue> | null {
  return foldEither(
    () => null,
    (jsxElement) => {
      if (isJSXElement(jsxElement)) {
        const renderedValueSides = element.specialSizeMeasurements.borderRadius
        if (renderedValueSides == null) {
          return null
        }

        const fromProps = borderRadiusFromProps(jsxElement.props)
        const measurementsNonZero = AllSides.some((c) => {
          const measurement = renderedValueSides[c]
          if (measurement == null) {
            return false
          } else {
            return measurement > 0
          }
        })

        const isElementIntrinsic = isIntrinsicElement(jsxElement.name)

        const elementIsIntrinsicElementOrScene =
          isElementIntrinsic || jsxElementNameEquals(jsxElement.name, jsxElementName('Scene', []))

        if (
          !(
            elementIsIntrinsicElementOrScene ||
            shouldShowControls(fromProps != null, measurementsNonZero)
          )
        ) {
          return null
        }

        const borderRadius = optionalMap(
          (radius) => measurementFromBorderRadius(renderedValueSides, radius),
          fromProps,
        )

        const defaultBorderRadiusSides = borderRadiusSidesFromValue(
          cssNumberWithRenderedValue(cssNumber(0, 'px'), 0),
        )

        if (borderRadius == null && isElementIntrinsic) {
          return {
            mode: 'all',
            borderRadius: defaultBorderRadiusSides,
          }
        }

        const borderRadiusUpperLimit = maxBorderRadius(
          size(
            element.specialSizeMeasurements.clientWidth,
            element.specialSizeMeasurements.clientHeight,
          ),
        )

        const borderRadiusMinMax = { min: 0, max: borderRadiusUpperLimit }
        return {
          mode: fromProps?.type === 'sides' ? 'individual' : 'all',
          borderRadius: mapBorderRadiusSides(
            (n) => adjustBorderRadius(borderRadiusMinMax, n),
            borderRadius ?? defaultBorderRadiusSides,
          ),
        }
      } else {
        return null
      }
    },
    element.element,
  )
}

interface BorderRadiusFromProps {
  type: 'sides' | 'borderRadius'
  sides: BorderRadiusSides<CSSNumber>
}

function borderRadiusFromProps(props: JSXAttributes): BorderRadiusFromProps | null {
  const wrappedProps = right(props)

  const borderRadius = getLayoutProperty('borderRadius', wrappedProps, styleStringInArray)
  const simpleBorderRadius = foldEither(
    () => null,
    (radius) => {
      if (radius == null) {
        return null
      } else {
        return foldEither(borderRadiusSidesFromValue, (value) => value, radius)
      }
    },
    borderRadius,
  )
  const borderTopLeftRadius = defaultEither(
    null,
    getLayoutProperty('borderTopLeftRadius', wrappedProps, styleStringInArray),
  )
  const borderTopRightRadius = defaultEither(
    null,
    getLayoutProperty('borderTopRightRadius', wrappedProps, styleStringInArray),
  )
  const borderBottomLeftRadius = defaultEither(
    null,
    getLayoutProperty('borderBottomLeftRadius', wrappedProps, styleStringInArray),
  )
  const borderBottomRightRadius = defaultEither(
    null,
    getLayoutProperty('borderBottomRightRadius', wrappedProps, styleStringInArray),
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

function sizeFromElement(element: ElementInstanceMetadata): Size {
  const globalFrame = zeroRectIfNullOrInfinity(element.globalFrame)
  return size(globalFrame.width, globalFrame.height)
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
  commands: (target: ElementPath) => Array<CanvasCommand>
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

function longhandFromEdgePosition(corner: BorderRadiusCorner) {
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

    const updatedBorderRadiusSides = {
      ...data.borderRadius,
      [key]: updatedBorderRadius,
    }

    return {
      commands: setLonghandStylePropertyCommand(
        mapBorderRadiusSides((v) => fallbackEmptyValue(v), updatedBorderRadiusSides),
      ),
      updatedBorderRadius: updatedBorderRadiusSides,
    }
  }

  const allUpdated = mapBorderRadiusSides(
    updateBorderRadiusFn(elementSize, scale, borderRadiusAdjustData),
    data.borderRadius,
  )

  return {
    commands: setShorthandStylePropertyCommand(
      printCSSNumber(fallbackEmptyValue(allUpdated.tl), null),
    ),
    updatedBorderRadius: allUpdated,
  }
}

const StyleProp = <P extends ParsedCSSPropertiesKeys>(p: P) =>
  stylePropPathMappingFn(p, styleStringInArray)

const setLonghandStylePropertyCommand =
  (sides: BorderRadiusSides<CSSNumber>) =>
  (target: ElementPath): Array<CanvasCommand> =>
    [
      deleteProperties('always', target, [StyleProp('borderRadius')]),
      setProperty(
        'always',
        target,
        StyleProp('borderTopLeftRadius'),
        printCSSNumber(sides.tl, null),
      ),
      setProperty(
        'always',
        target,
        StyleProp('borderTopRightRadius'),
        printCSSNumber(sides.tr, null),
      ),
      setProperty(
        'always',
        target,
        StyleProp('borderBottomRightRadius'),
        printCSSNumber(sides.br, null),
      ),
      setProperty(
        'always',
        target,
        StyleProp('borderBottomLeftRadius'),
        printCSSNumber(sides.bl, null),
      ),
    ]

const setShorthandStylePropertyCommand =
  (value: string | number) =>
  (target: ElementPath): Array<CanvasCommand> =>
    [
      deleteProperties('always', target, [
        StyleProp('borderTopLeftRadius'),
        StyleProp('borderTopRightRadius'),
        StyleProp('borderBottomLeftRadius'),
        StyleProp('borderBottomRightRadius'),
      ]),
      setProperty('always', target, StyleProp('borderRadius'), value),
    ]

function getAddOverflowHiddenCommands(
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
): Array<CanvasCommand> {
  const overflowProp = PP.create('style', 'overflow')

  const propertyExists = withUnderlyingTarget(target, projectContents, false, (_, element) => {
    if (isJSXElement(element)) {
      return foldEither(
        () => false,
        (value) => !modifiableAttributeIsAttributeNotFound(value),
        getModifiableJSXAttributeAtPath(element.props, overflowProp),
      )
    } else {
      return false
    }
  })

  if (propertyExists) {
    return []
  }
  return [
    showToastCommand('Element now hides overflowing content', 'NOTICE', 'property-added'),
    setProperty('always', target, overflowProp, 'hidden'),
  ]
}
