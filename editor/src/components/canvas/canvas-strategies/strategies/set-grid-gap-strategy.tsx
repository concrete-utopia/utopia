import { styleStringInArray } from '../../../../utils/common-constants'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  zeroRectIfNullOrInfinity,
  canvasVector,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { Modifiers } from '../../../../utils/modifiers'
import { printCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { deleteProperties } from '../../commands/delete-properties-command'
import { setCursorCommand } from '../../commands/set-cursor-command'

import { setProperty } from '../../commands/set-property-command'
import {
  fallbackEmptyValue,
  indicatorMessage,
  offsetMeasurementByDelta,
  precisionFromModifiers,
} from '../../controls/select-mode/controls-common'
import type { FloatingIndicatorProps } from '../../controls/select-mode/floating-number-indicator'
import { FloatingIndicator } from '../../controls/select-mode/floating-number-indicator'
import type { GridGapData } from '../../gap-utils'
import {
  cursorFromAxis,
  maybeGridGapData,
  recurseIntoChildrenOfMapOrFragment,
} from '../../gap-utils'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { onlyFitWhenDraggingThisControl } from '../canvas-strategies'
import type { ControlWithProps, InteractionCanvasState } from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { colorTheme } from '../../../../uuiui'
import { activeFrameTargetPath, setActiveFrames } from '../../commands/set-active-frames-command'
import type { GridGapControlProps } from '../../controls/select-mode/grid-gap-control'
import { GridGapControl } from '../../controls/select-mode/grid-gap-control'
import type { GridControlsProps } from '../../controls/grid-controls'
import { controlsForGridPlaceholders } from '../../controls/grid-controls'

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

  const gridGap = maybeGridGapData(canvasState.startingMetadata, selectedElement)
  if (gridGap == null) {
    return null
  }

  const drag = dragFromInteractionSession(interactionSession) ?? canvasVector({ x: 0, y: 0 })

  const dragDelta = {
    x: Math.max(-gridGap.column.renderedValuePx, drag.x),
    y: Math.max(-gridGap.row.renderedValuePx, drag.y),
  }

  const shouldTearOffGap = {
    x: isDragOverThreshold({ gapPx: gridGap.column.renderedValuePx, deltaPx: drag.x }),
    y: isDragOverThreshold({ gapPx: gridGap.row.renderedValuePx, deltaPx: drag.y }),
  }

  const adjustPrecision =
    optionalMap(precisionFromModifiers, modifiersFromInteractionSession(interactionSession)) ??
    'precise'

  const updatedGridGapMeasurement = {
    row: offsetMeasurementByDelta(gridGap.row, dragDelta.y, adjustPrecision),
    column: offsetMeasurementByDelta(gridGap.column, dragDelta.x, adjustPrecision),
  }

  const gridGapControl = controlWithProps({
    control: GridGapControl,
    props: {
      selectedElement: selectedElement,
      updatedGapValueRow: isDragOngoing(interactionSession) ? updatedGridGapMeasurement.row : null,
      updatedGapValueColumn: isDragOngoing(interactionSession)
        ? updatedGridGapMeasurement.column
        : null,
    },
    key: 'grid-gap-resize-control',
    show: 'visible-except-when-other-strategy-is-active',
  })

  const maybeIndicatorProps = gridGapValueIndicatorProps(interactionSession, gridGap)

  const controlsToRender: Array<
    | ControlWithProps<FloatingIndicatorProps>
    | ControlWithProps<GridControlsProps>
    | ControlWithProps<GridGapControlProps>
  > = [gridGapControl]

  // show indicator if needed
  if (maybeIndicatorProps != null) {
    controlsToRender.push(
      controlWithProps({
        control: FloatingIndicator,
        props: {
          ...maybeIndicatorProps,
          color: colorTheme.brandNeonOrange.value,
        },
        key: 'padding-value-indicator-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    )
  }

  // when the drag is ongoing, keep showing the grid cells
  if (isDragOngoing(interactionSession)) {
    controlsToRender.push(controlsForGridPlaceholders(selectedElement))
  }

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
      const shouldTearOffGapByAxis = axis === 'row' ? shouldTearOffGap.y : shouldTearOffGap.x
      const axisStyleProp = axis === 'row' ? StyleRowGapProp : StyleColumnGapProp
      const gridGapMeasurement =
        axis === 'row' ? updatedGridGapMeasurement.row : updatedGridGapMeasurement.column

      if (shouldTearOffGapByAxis) {
        return strategyApplicationResult(
          [deleteProperties('always', selectedElement, [axisStyleProp])],
          selectedElements,
        )
      }

      return strategyApplicationResult(
        [
          setProperty(
            'always',
            selectedElement,
            axisStyleProp,
            printCSSNumber(fallbackEmptyValue(gridGapMeasurement), null),
          ),
          setCursorCommand(cursorFromAxis(axis)),
          setActiveFrames([
            {
              action: 'set-gap',
              target: activeFrameTargetPath(selectedElement),
              source: zeroRectIfNullOrInfinity(
                MetadataUtils.getFrameInCanvasCoords(selectedElement, canvasState.startingMetadata),
              ),
            },
          ]),
        ],
        selectedElements,
      )
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

  const activeControlAxis = interactionSession.activeControl.axis

  const { drag, dragStart } = interactionSession.interactionData

  const rawDragDelta = activeControlAxis === 'row' ? drag.y : drag.x

  const dragDelta = Math.max(-gridGap[activeControlAxis].renderedValuePx, rawDragDelta)

  const rawGridGapMeasurement = offsetMeasurementByDelta(
    gridGap[activeControlAxis],
    rawDragDelta,
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  const updatedGridGapMeasurement = offsetMeasurementByDelta(
    gridGap[activeControlAxis],
    dragDelta,
    precisionFromModifiers(interactionSession.interactionData.modifiers),
  )

  const position =
    activeControlAxis === 'row'
      ? canvasPoint({ x: dragStart.x, y: dragStart.y + drag.y })
      : canvasPoint({ x: dragStart.x + drag.x, y: dragStart.y })

  return {
    value: indicatorMessage(
      rawGridGapMeasurement.renderedValuePx > GridGapTearThreshold,
      updatedGridGapMeasurement,
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
