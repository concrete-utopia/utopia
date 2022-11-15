import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { canvasPoint, CanvasVector, canvasVector } from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { assertNever } from '../../../../core/shared/utils'
import { Modifiers } from '../../../../utils/modifiers'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import {
  offsetMeasurementByDelta,
  precisionFromModifiers,
} from '../../controls/select-mode/controls-common'
import { FlexGapControl } from '../../controls/select-mode/flex-gap-control'
import {
  FloatingIndicator,
  FloatingIndicatorProps,
} from '../../controls/select-mode/floating-number-indicator'
import {
  cursorFromFlexDirection,
  dragDeltaForOrientation,
  FlexGapData,
  maybeFlexGapFromElement,
  SimpleFlexDirectionForGap,
} from '../../gap-utils'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const SetFlexGapStrategyId = 'SET_FLEX_GAP_STRATEGY'

const StyleGapProp = stylePropPathMappingFn('gap', ['style'])

export const FlexGapTearThreshold: number = -20

export const setFlexGapStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  if (
    interactionSession != null &&
    !(
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'FLEX_GAP_HANDLE'
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
  const children = MetadataUtils.getChildrenPaths(canvasState.startingMetadata, selectedElement)

  const flexGap = maybeFlexGapFromElement(canvasState.startingMetadata, selectedElements[0])
  if (flexGap == null) {
    return null
  }

  const drag = dragFromInteractionSession(interactionSession) ?? canvasVector({ x: 0, y: 0 })

  const rawDragDelta = dragDeltaForOrientation(flexGap.direction, drag)

  const dragDelta = Math.max(
    -flexGap.value.renderedValuePx,
    dragDeltaForOrientation(flexGap.direction, drag),
  )

  const shouldTearOffGap = isDragOverThreshold(flexGap.direction, {
    deltaPx: rawDragDelta,
    gapPx: flexGap.value.renderedValuePx,
  })

  const adjustPrecision =
    optionalMap(precisionFromModifiers, modifiersFromInteractionSession(interactionSession)) ??
    'precise'

  const updatedFlexGapMeasurement = offsetMeasurementByDelta(
    flexGap.value,
    dragDelta,
    adjustPrecision,
  )

  const resizeControl = controlWithProps({
    control: FlexGapControl,
    props: {
      selectedElement: selectedElement,
      flexDirection: flexGap.direction,
      updatedGapValue: updatedFlexGapMeasurement,
    },
    key: 'flex-gap-resize-control',
    show: 'visible-except-when-other-strategy-is-active',
  })

  const maybeIndicatorProps = flexGapValueIndicatorProps(interactionSession, flexGap)

  const controlsToRender = optionalMap(
    (props) => [
      resizeControl,
      controlWithProps({
        control: FloatingIndicator,
        props: props,
        key: 'padding-value-indicator-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    maybeIndicatorProps,
  ) ?? [resizeControl]

  return {
    id: SetFlexGapStrategyId,
    name: 'Set flex gap',
    controlsToRender: controlsToRender,
    fitness: 1,
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.activeControl.type !== 'FLEX_GAP_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      if (shouldTearOffGap) {
        return strategyApplicationResult([
          deleteProperties('always', selectedElement, [StyleGapProp]),
        ])
      }

      return strategyApplicationResult([
        setProperty(
          'always',
          selectedElement,
          StyleGapProp,
          printCSSNumber(updatedFlexGapMeasurement.value, null),
        ),
        setCursorCommand(cursorFromFlexDirection(flexGap.direction)),
        setElementsToRerenderCommand([...selectedElements, ...children]),
      ])
    },
  }
}

function dragFromInteractionSession(
  interactionSession: InteractionSession | null,
): CanvasVector | null {
  if (interactionSession != null && interactionSession.interactionData.type === 'DRAG') {
    return interactionSession.interactionData.drag
  }
  return null
}

function modifiersFromInteractionSession(
  interactionSession: InteractionSession | null,
): Modifiers | null {
  if (interactionSession != null && interactionSession.interactionData.type === 'DRAG') {
    return interactionSession.interactionData.modifiers
  }
  return null
}

function isDragOverThreshold(
  direction: SimpleFlexDirectionForGap,
  { gapPx, deltaPx }: { gapPx: number; deltaPx: number },
): boolean {
  switch (direction) {
    case 'row':
    case 'column':
      return deltaPx + gapPx < FlexGapTearThreshold
    case 'row-reverse':
    case 'column-reverse':
      return deltaPx - gapPx < FlexGapTearThreshold
    default:
      assertNever(direction)
  }
}

function flexGapValueIndicatorProps(
  interactionSession: InteractionSession | null,
  flexGap: FlexGapData,
): FloatingIndicatorProps | null {
  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'FLEX_GAP_HANDLE' ||
    interactionSession.interactionData.drag == null
  ) {
    return null
  }

  const { drag, dragStart } = interactionSession.interactionData

  const rawDragDelta = dragDeltaForOrientation(flexGap.direction, drag)

  const dragDelta = Math.max(-flexGap.value.renderedValuePx, rawDragDelta)

  const updatedFlexGapMeasurement = offsetMeasurementByDelta(
    flexGap.value,
    dragDelta,
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  const position = flexGap.direction.startsWith('row')
    ? canvasPoint({ x: dragStart.x + drag.x, y: dragStart.y })
    : canvasPoint({ x: dragStart.x, y: dragStart.y + drag.y })

  return {
    value:
      rawDragDelta + flexGap.value.renderedValuePx > FlexGapTearThreshold
        ? printCSSNumber(updatedFlexGapMeasurement.value, null)
        : '\u2014', // emdash
    position: position,
  }
}
