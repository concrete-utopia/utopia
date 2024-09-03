import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  gridPositionValue,
  type GridElementProperties,
} from '../../../../core/shared/element-template'
import { assertNever } from '../../../../core/shared/utils'
import { emptyModifiers, Modifier } from '../../../../utils/modifiers'
import { GridControls, GridControlsKey } from '../../controls/grid-controls'
import type { CanvasStrategy, InteractionCanvasState } from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession, KeyState } from '../interaction-state'
import { getGridCellBoundsFromCanvas, setGridPropsCommands } from './grid-helpers'
import { accumulatePresses } from './shared-keyboard-strategy-helpers'

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
  const gridTemplate = grid.specialSizeMeasurements.containerGridProperties

  const cellBounds = getGridCellBoundsFromCanvas(cell, canvasState.scale, canvasState.canvasOffset)
  if (cellBounds == null) {
    return null
  }

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

      const horizontalDelta = getKeysDelta(interactionData.keyStates, 'horizontal')
      const verticalDelta = getKeysDelta(interactionData.keyStates, 'vertical')

      let gridProps: Partial<GridElementProperties> = {
        ...cell.specialSizeMeasurements.elementGridProperties,
      }

      if (horizontalDelta !== 0) {
        const { from, to } = getNewBounds(
          cellBounds.originCell.column + horizontalDelta,
          gridTemplate.gridTemplateColumns.dimensions.length,
          cellBounds.width,
        )
        gridProps.gridColumnStart = gridPositionValue(from)
        gridProps.gridColumnEnd = gridPositionValue(to)
      }

      if (verticalDelta !== 0) {
        const { from, to } = getNewBounds(
          cellBounds.originCell.row + verticalDelta,
          gridTemplate.gridTemplateRows.dimensions.length,
          cellBounds.height,
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

function getKeysDelta(keyStates: KeyState[], direction: 'vertical' | 'horizontal'): number {
  return keyStates.reduce((tot, cur) => {
    let presses = 0
    cur.keysPressed.forEach((key) => {
      switch (direction) {
        case 'horizontal':
          presses += key === 'left' ? -1 : key === 'right' ? 1 : 0
          break
        case 'vertical':
          presses += key === 'up' ? -1 : key === 'down' ? 1 : 0
          break
        default:
          assertNever(direction)
      }
    })
    return tot + presses
  }, 0)
}

function getNewBounds(
  start: number,
  cellsCount: number,
  size: number,
): {
  from: number
  to: number
} {
  const lowerLimit = 1
  const upperLimit = cellsCount + 1

  let from = start
  let to = start + size

  if (to > upperLimit) {
    to = upperLimit
    from = to - size
  }
  if (from < lowerLimit) {
    from = lowerLimit
    to = from + size
  }

  return { from: from, to: to }
}
