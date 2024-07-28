import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  zeroRectIfNullOrInfinity,
  canvasVector,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import { assertNever } from '../../../../core/shared/utils'
import type { Modifiers } from '../../../../utils/modifiers'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { setProperty } from '../../commands/set-property-command'
import {
  indicatorMessage,
  offsetMeasurementByDelta,
  precisionFromModifiers,
} from '../../controls/select-mode/controls-common'
import { FlexGapControl } from '../../controls/select-mode/flex-gap-control'
import type { FloatingIndicatorProps } from '../../controls/select-mode/floating-number-indicator'
import { FloatingIndicator } from '../../controls/select-mode/floating-number-indicator'
import type { FlexGapData, GridGapData } from '../../gap-utils'
import {
  cursorFromAxis,
  cursorFromFlexDirection,
  dragDeltaForOrientation,
  maybeFlexGapData,
  maybeGridGapData,
  recurseIntoChildrenOfMapOrFragment,
} from '../../gap-utils'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { singleAxisAutoLayoutDirections } from './flow-reorder-helpers'
import { colorTheme } from '../../../../uuiui'
import { activeFrameTargetPath, setActiveFrames } from '../../commands/set-active-frames-command'
import { GridGapControl } from '../../controls/select-mode/grid-gap-control'

const SetGridGapStrategyId = 'SET_GRID_GAP_STRATEGY'

const StyleGapProp = stylePropPathMappingFn('gap', styleStringInArray)
const StyleRowGapProp = stylePropPathMappingFn('rowGap', styleStringInArray)
const StyleColumnGapProp = stylePropPathMappingFn('columnGap', styleStringInArray)

export const GridGapTearThreshold: number = -25

export const setGridGapStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
) => {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const selectedElement = selectedElements[0]
  if (
    !MetadataUtils.isGridLayoutedContainer(
      MetadataUtils.findElementByElementPath(canvasState.startingMetadata, selectedElement),
    )
  ) {
    return null
  }

  const children = recurseIntoChildrenOfMapOrFragment(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    selectedElement,
  )

  if (children.length < 2) {
    return null
  }

  // // do not show the gap control if the children are wrapped
  // if (
  //   singleAxisAutoLayoutDirections(
  //     children,
  //     canvasState.startingMetadata,
  //     children[0].specialSizeMeasurements.parentLayoutSystem ?? 'none',
  //     MetadataUtils.flexDirectionToSimpleFlexDirection(
  //       children[0].specialSizeMeasurements.parentFlexDirection ?? 'row',
  //     ),
  //   ) === 'non-single-axis-autolayout'
  // ) {
  //   return null
  // }

  const gridGap = maybeGridGapData(canvasState.startingMetadata, selectedElement)
  if (gridGap == null) {
    return null
  }

  const drag = dragFromInteractionSession(interactionSession) ?? canvasVector({ x: 0, y: 0 })

  const dragDelta = {
    x: Math.max(-gridGap.row.renderedValuePx, drag.x),
    y: Math.max(-gridGap.column.renderedValuePx, drag.y),
  }

  const shouldTearOffGap = {
    x: isDragOverThreshold({ gapPx: gridGap.row.renderedValuePx, deltaPx: dragDelta.x }),
    y: isDragOverThreshold({ gapPx: gridGap.column.renderedValuePx, deltaPx: dragDelta.y }),
  }

  const adjustPrecision =
    optionalMap(precisionFromModifiers, modifiersFromInteractionSession(interactionSession)) ??
    'precise'

  const updatedGridGapMeasurement = {
    x: offsetMeasurementByDelta(gridGap.row, dragDelta.x, adjustPrecision),
    y: offsetMeasurementByDelta(gridGap.column, dragDelta.y, adjustPrecision),
  }

  const resizeControl = controlWithProps({
    control: GridGapControl,
    props: {
      selectedElement: selectedElement,
      updatedGapValueX: isDragOngoing(interactionSession) ? updatedGridGapMeasurement.x : null,
      updatedGapValueY: isDragOngoing(interactionSession) ? updatedGridGapMeasurement.y : null,
    },
    key: 'grid-gap-resize-control',
    show: 'visible-except-when-other-strategy-is-active',
  })

  const maybeIndicatorProps = gridGapValueIndicatorProps(interactionSession, gridGap)

  const controlsToRender = optionalMap(
    (props) => [
      resizeControl,
      controlWithProps({
        control: FloatingIndicator,
        props: {
          ...props,
          color: colorTheme.brandNeonPink.value,
        },
        key: 'padding-value-indicator-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    maybeIndicatorProps,
  ) ?? [resizeControl]

  return {
    id: SetGridGapStrategyId,
    name: 'Set grid gap',
    descriptiveLabel: 'Changing Grid Gap',
    icon: {
      category: 'tools',
      type: 'pointer',
    },
    controlsToRender: controlsToRender,
    fitness: onlyFitWhenDraggingThisControl(interactionSession, 'GRID_GAP_HANDLE', 1),
    apply: () => {
      if (
        interactionSession == null ||
        interactionSession.interactionData.type !== 'DRAG' ||
        interactionSession.activeControl.type !== 'GRID_GAP_HANDLE'
      ) {
        return emptyStrategyApplicationResult
      }

      const axis = interactionSession.activeControl.axis
      const shouldTearOffGapByAxis = axis === 'row' ? shouldTearOffGap.x : shouldTearOffGap.y
      const axisStyleProp = axis === 'row' ? StyleRowGapProp : StyleColumnGapProp
      const gridGapMeasurement =
        axis === 'row' ? updatedGridGapMeasurement.x : updatedGridGapMeasurement.y

      if (shouldTearOffGapByAxis) {
        return strategyApplicationResult([
          deleteProperties('always', selectedElement, [StyleGapProp]),
          deleteProperties('always', selectedElement, [axisStyleProp]),
        ])
      }

      return strategyApplicationResult([
        setProperty(
          'always',
          selectedElement,
          axisStyleProp,
          printCSSNumber(gridGapMeasurement.value, null),
        ),
        setCursorCommand(cursorFromAxis(axis)),
        setElementsToRerenderCommand([...selectedElements, ...children.map((c) => c.elementPath)]),
        setActiveFrames([
          {
            action: 'set-gap',
            target: activeFrameTargetPath(selectedElement),
            source: zeroRectIfNullOrInfinity(
              MetadataUtils.getFrameInCanvasCoords(selectedElement, canvasState.startingMetadata),
            ),
          },
        ]),
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

function isDragOverThreshold({ gapPx, deltaPx }: { gapPx: number; deltaPx: number }): boolean {
  return deltaPx + gapPx < GridGapTearThreshold
}

function gridGapValueIndicatorProps(
  interactionSession: InteractionSession | null,
  gridGap: GridGapData,
): FloatingIndicatorProps | null {
  if (
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'GRID_GAP_HANDLE' ||
    interactionSession.interactionData.drag == null
  ) {
    return null
  }

  const { drag, dragStart } = interactionSession.interactionData

  const rawDragDelta = dragDeltaForOrientation(flexGap.direction, drag)

  const dragDelta = Math.max(-flexGap.value.renderedValuePx, rawDragDelta)

  const rawFlexGapMeasurement = offsetMeasurementByDelta(
    flexGap.value,
    rawDragDelta,
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  const updatedFlexGapMeasurement = offsetMeasurementByDelta(
    flexGap.value,
    dragDelta,
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  const position = flexGap.direction.startsWith('row')
    ? canvasPoint({ x: dragStart.x + drag.x, y: dragStart.y })
    : canvasPoint({ x: dragStart.x, y: dragStart.y + drag.y })

  return {
    value: indicatorMessage(
      rawFlexGapMeasurement.renderedValuePx > GridGapTearThreshold,
      updatedFlexGapMeasurement,
    ),
    position: position,
  }
}

function isDragOngoing(interactionSession: InteractionSession | null): boolean {
  return (
    interactionSession != null &&
    interactionSession.activeControl.type === 'GRID_GAP_HANDLE' &&
    interactionSession.interactionData.type === 'DRAG'
  )
}
