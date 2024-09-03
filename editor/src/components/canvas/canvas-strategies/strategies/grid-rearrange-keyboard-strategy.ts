import * as EP from '../../../../core/shared/element-path'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { emptyModifiers, Modifier } from '../../../../utils/modifiers'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { accumulatePresses } from './shared-keyboard-strategy-helpers'
import type { CanvasCommand } from '../../commands/commands'
import { getElementGridProperties, getGridCellAtPoint, setGridPropsCommands } from './grid-helpers'
import {
  gridPositionValue,
  type GridElementProperties,
} from '../../../../core/shared/element-template'
import { isInfinityRectangle, offsetPoint, windowPoint } from '../../../../core/shared/math-utils'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import { GridControls, GridControlsKey } from '../../controls/grid-controls'

type ArrowKey = 'left' | 'right' | 'up' | 'down'

export function gridRearrangeKeyboardStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
): CanvasStrategy | null {
  if (interactionSession?.activeControl.type !== 'KEYBOARD_CATCHER_CONTROL') {
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

  const grid = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, parentGridPath)
  if (grid == null) {
    return null
  }

  const cellFrame = cell.globalFrame
  if (cellFrame == null || isInfinityRectangle(cellFrame)) {
    return null
  }
  const canvasFrameWidth = cellFrame.width * canvasState.scale
  const canvasFrameHeight = cellFrame.height * canvasState.scale

  const cellOriginPoint = offsetPoint(
    canvasPointToWindowPoint(cellFrame, canvasState.scale, canvasState.canvasOffset),
    windowPoint({ x: 5, y: 5 }),
  )
  const cellOrigin = getGridCellAtPoint(cellOriginPoint, true)
  if (cellOrigin == null) {
    return null
  }

  const cellEndPoint = offsetPoint(
    cellOriginPoint,
    windowPoint({
      x: canvasFrameWidth - 5,
      y: canvasFrameHeight - 5,
    }),
  )
  const cellEnd = getGridCellAtPoint(cellEndPoint, true)
  if (cellEnd == null) {
    return null
  }

  const cellOriginCoords = cellOrigin.coordinates
  const cellEndCoords = cellEnd.coordinates

  const cellWidth = cellEndCoords.column - cellOriginCoords.column + 1
  const cellHeight = cellEndCoords.row - cellOriginCoords.row + 1

  const gridTemplate = grid.specialSizeMeasurements.containerGridProperties

  return {
    id: 'GRID_KEYBOARD_REARRANGE',
    name: 'Grid rearrange',
    descriptiveLabel: 'Grid rearrange',
    icon: {
      category: 'modalities',
      type: 'reorder-large',
    },
    controlsToRender: [
      {
        control: GridControls,
        props: { targets: [parentGridPath] },
        key: GridControlsKey(parentGridPath),
        show: 'always-visible',
        priority: 'bottom',
      },
    ],
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

      let colDelta = interactionData.keyStates.reduce((tot, cur) => {
        let presses = 0
        cur.keysPressed.forEach((key) => {
          presses += key === 'left' ? -1 : key === 'right' ? 1 : 0
        })
        return tot + presses
      }, 0)
      let rowDelta = interactionData.keyStates.reduce((tot, cur) => {
        let presses = 0
        cur.keysPressed.forEach((key) => {
          presses += key === 'up' ? -1 : key === 'down' ? 1 : 0
        })
        return tot + presses
      }, 0)

      let gridProps: Partial<GridElementProperties> = {
        ...cell.specialSizeMeasurements.elementGridProperties,
      }

      function getNewBounds(start: number, cellsCount: number, size: number) {
        const limit = cellsCount - size + 1

        let newFrom = start
        let newTo = start + size

        if (newFrom > limit) {
          newTo = limit + 2 // +2, because +1 for the grid and +1 for its limit
          newFrom = newTo - size
        }
        if (newFrom < 1) {
          newFrom = 1
          newTo = newFrom + size
        }

        return { from: newFrom, to: newTo }
      }

      if (colDelta !== 0) {
        const { from, to } = getNewBounds(
          cellOriginCoords.column + colDelta,
          gridTemplate.gridTemplateColumns.dimensions.length,
          cellWidth,
        )
        gridProps.gridColumnStart = gridPositionValue(from)
        gridProps.gridColumnEnd = gridPositionValue(to)
      }

      if (rowDelta !== 0) {
        const { from, to } = getNewBounds(
          cellOriginCoords.row + rowDelta,
          gridTemplate.gridTemplateRows.dimensions.length,
          cellHeight,
        )
        gridProps.gridRowStart = gridPositionValue(from)
        gridProps.gridRowEnd = gridPositionValue(to)
      }

      return strategyApplicationResult(setGridPropsCommands(target, gridTemplate, gridProps))
    },
  }
}

function fitness(interactionSession: InteractionSession | null): number {
  if (interactionSession == null || interactionSession.interactionData.type !== 'KEYBOARD') {
    return 0
  }

  const accumulatedPresses = accumulatePresses(interactionSession.interactionData.keyStates)
  const matches = accumulatedPresses.some(
    (accumulatedPress) =>
      Array.from(accumulatedPress.keysPressed).some(
        (key) => key === 'left' || key === 'right' || key === 'up' || key === 'down',
      ) && Modifier.equal(accumulatedPress.modifiers, emptyModifiers),
  )

  return matches ? 1 : 0
}
