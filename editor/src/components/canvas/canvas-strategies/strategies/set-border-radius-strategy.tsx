import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  CanvasPoint,
  canvasVector,
  CanvasVector,
  clamp,
  product,
  Size,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { Modifiers } from '../../../../utils/modifiers'
import {
  CSSBorderRadiusIndividual,
  ParsedCSSProperties,
  ParsedCSSPropertiesKeys,
  printCSSNumber,
} from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import {
  BorderRadiusAdjustMode,
  BorderRadiusControlMinimumForDisplay,
  BorderRadiusCorner,
  BorderRadiusData,
  borderRadiusFromElement,
  BorderRadiusCornerMapped,
  mapBorderRadiusSides,
  maxBorderRadius,
  sizeFromElement,
} from '../../border-radius-control-utils'
import { CanvasCommand } from '../../commands/commands'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import { BorderRadiusControl } from '../../controls/select-mode/border-radius-control'
import {
  canShowCanvasPropControl,
  CSSNumberWithRenderedValue,
  measurementBasedOnOtherMeasurement,
  precisionFromModifiers,
} from '../../controls/select-mode/controls-common'
import { CanvasStrategyFactory, onlyFitWhenDraggingThisControl } from '../canvas-strategies'
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
    canvasState.openFile ?? null,
    element,
    canvasState.scale,
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
          borderRadius: borderResizeInteractionOnGoing(interactionSession)
            ? updatedBorderRadius
            : null,
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

interface SetBorderRadiusStrategyRunResult {
  commands: (target: ElementPath) => CanvasCommand
  updatedBorderRadius: BorderRadiusCornerMapped<CSSNumberWithRenderedValue>
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

function borderResizeInteractionOnGoing(interactionSession: InteractionSession | null): boolean {
  return (
    interactionSession != null &&
    interactionSession.interactionData.type === 'DRAG' &&
    interactionSession.activeControl.type === 'BORDER_RADIUS_RESIZE_HANDLE'
  )
}
