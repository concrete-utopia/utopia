import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridPositionValue } from '../../../../core/shared/element-template'
import { gridPositionValue } from '../../../../core/shared/element-template'
import { controlsForGridPlaceholders } from '../../controls/grid-controls-for-strategies'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { setGridPropsCommands } from './grid-helpers'
import { getGridChildCellCoordBoundsFromCanvas } from './grid-cell-bounds'
import { accumulatePresses } from './shared-keyboard-strategy-helpers'

export function gridRearrangeResizeKeyboardStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  if (
    interactionSession?.activeControl.type !== 'KEYBOARD_CATCHER_CONTROL' ||
    interactionSession.interactionData.type !== 'KEYBOARD'
  ) {
    return null
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  const target = selectedElements[0]

  if (!MetadataUtils.isGridCell(canvasState.startingMetadata, target)) {
    return null
  }

  const cell = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, target)
  if (cell == null) {
    return null
  }

  const parentGridPath = EP.parentPath(target)

  const gridTemplate = cell.specialSizeMeasurements.parentContainerGridProperties
  const gridCellGlobalFrames = cell.specialSizeMeasurements.parentGridCellGlobalFrames

  const initialCellBounds =
    gridCellGlobalFrames != null
      ? getGridChildCellCoordBoundsFromCanvas(cell, gridCellGlobalFrames)
      : null
  if (initialCellBounds == null) {
    return null
  }

  const resizing =
    Array.from(interactionSession.interactionData.keyStates).at(
      interactionSession.interactionData.keyStates.length - 1,
    )?.modifiers.shift ?? false

  const label = resizing ? 'Grid resize' : 'Grid rearrange'

  return {
    id: 'GRID_KEYBOARD_REARRANGE_RESIZE',
    name: label,
    descriptiveLabel: label,
    icon: {
      category: 'modalities',
      type: 'reorder-large',
    },
    controlsToRender: [controlsForGridPlaceholders(parentGridPath)],
    fitness: fitness(interactionSession),
    apply: () => {
      if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
        return emptyStrategyApplicationResult
      }
      if (
        gridTemplate.gridTemplateColumns?.type !== 'DIMENSIONS' ||
        gridTemplate.gridTemplateRows?.type !== 'DIMENSIONS'
      ) {
        return emptyStrategyApplicationResult
      }

      const interactionData = interactionSession.interactionData

      let gridColumnStart: GridPositionValue = gridPositionValue(initialCellBounds.column)
      let gridColumnEnd: GridPositionValue = gridPositionValue(
        initialCellBounds.column + initialCellBounds.width,
      )
      let gridRowStart: GridPositionValue = gridPositionValue(initialCellBounds.row)
      let gridRowEnd: GridPositionValue = gridPositionValue(
        initialCellBounds.row + initialCellBounds.height,
      )

      const cols = gridTemplate.gridTemplateColumns.dimensions.length
      const rows = gridTemplate.gridTemplateRows.dimensions.length

      for (const keyState of interactionData.keyStates) {
        const resize = keyState.modifiers.shift
        for (const key of keyState.keysPressed) {
          // column changes
          const horizDelta = key === 'left' ? -1 : key === 'right' ? 1 : null
          if (horizDelta != null) {
            const bounds = { start: gridColumnStart, end: gridColumnEnd }
            const { start, end } = processPress(horizDelta, resize, cols, bounds)

            gridColumnStart = start
            gridColumnEnd = end
          }

          // row changes
          const vertDelta = key === 'up' ? -1 : key === 'down' ? 1 : null
          if (vertDelta != null) {
            const bounds = { start: gridRowStart, end: gridRowEnd }
            const { start, end } = processPress(vertDelta, resize, rows, bounds)

            gridRowStart = start
            gridRowEnd = end
          }
        }
      }

      return strategyApplicationResult(
        setGridPropsCommands(target, gridTemplate, {
          gridColumnStart,
          gridColumnEnd,
          gridRowStart,
          gridRowEnd,
        }),
        [parentGridPath],
      )
    },
  }
}

function fitness(interactionSession: InteractionSession | null): number {
  if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
    return 0
  }

  const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
  const matches = accumulatedPresses.some((accumulatedPress) =>
    Array.from(accumulatedPress.keysPressed).some(
      (key) => key === 'left' || key === 'right' || key === 'up' || key === 'down',
    ),
  )

  return matches ? 1 : 0
}

// process a keypress event and return the updated start/end grid cell bounds
function processPress(
  amount: 1 | -1,
  resize: boolean,
  cellsCount: number,
  initialBounds: {
    start: GridPositionValue
    end: GridPositionValue
  },
): {
  start: GridPositionValue
  end: GridPositionValue
} {
  let newBounds = { ...initialBounds }

  const start = newBounds.start.numericalPosition ?? 1
  const end = newBounds.end.numericalPosition ?? 1

  if (resize) {
    newBounds.end = gridPositionValue(Math.max(start + 1, Math.min(cellsCount + 1, end + amount)))
  } else {
    const size = end - start
    const newStart = Math.min(cellsCount - size + 1, Math.max(1, start + amount))
    newBounds.start = gridPositionValue(newStart)
    newBounds.end = gridPositionValue(newStart + size)
  }

  return newBounds
}
