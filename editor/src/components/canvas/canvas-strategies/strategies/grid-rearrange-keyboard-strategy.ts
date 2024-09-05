import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { GridPositionValue } from '../../../../core/shared/element-template'
import {
  gridPositionValue,
  type GridElementProperties,
} from '../../../../core/shared/element-template'
import { GridControls, GridControlsKey } from '../../controls/grid-controls'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import {
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { getGridCellBoundsFromCanvas, setGridPropsCommands } from './grid-helpers'
import { accumulatePresses } from './shared-keyboard-strategy-helpers'

export function gridRearrangeResizeKeyboardStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customState: CustomStrategyState,
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

  const grid = MetadataUtils.findElementByElementPath(canvasState.startingMetadata, parentGridPath)
  if (grid == null) {
    return null
  }
  const gridTemplate = grid.specialSizeMeasurements.containerGridProperties

  const initialCellBounds = getGridCellBoundsFromCanvas(
    cell,
    canvasState.scale,
    canvasState.canvasOffset,
  )
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

      let gridColumnStart: GridPositionValue = gridPositionValue(initialCellBounds.column)
      let gridColumnEnd: GridPositionValue = gridPositionValue(
        initialCellBounds.column + initialCellBounds.width,
      )
      let gridRowStart: GridPositionValue = gridPositionValue(initialCellBounds.row)
      let gridRowEnd: GridPositionValue = gridPositionValue(
        initialCellBounds.row + initialCellBounds.width,
      )

      const cols = gridTemplate.gridTemplateColumns.dimensions.length
      const rows = gridTemplate.gridTemplateRows.dimensions.length

      for (const key of interactionData.keyStates) {
        for (const press of key.keysPressed) {
          const horizontal = press === 'left' ? -1 : press === 'right' ? 1 : null
          if (horizontal != null) {
            const colsResult = processPress(horizontal, key.modifiers.shift, cols, {
              start: gridColumnStart,
              end: gridColumnEnd,
            })
            gridColumnStart = colsResult.start
            gridColumnEnd = colsResult.end
          }
          const vertical = press === 'up' ? -1 : press === 'down' ? 1 : null
          if (vertical != null) {
            const rowsResult = processPress(vertical, key.modifiers.shift, rows, {
              start: gridRowStart,
              end: gridRowEnd,
            })
            gridRowStart = rowsResult.start
            gridRowEnd = rowsResult.end
          }
        }
      }

      let gridProps: Partial<GridElementProperties> = {
        ...cell.specialSizeMeasurements.elementGridProperties,
      }
      gridProps.gridColumnStart = gridColumnStart
      gridProps.gridColumnEnd = gridColumnEnd
      gridProps.gridRowStart = gridRowStart
      gridProps.gridRowEnd = gridRowEnd

      return strategyApplicationResult(setGridPropsCommands(target, gridTemplate, gridProps))
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

function processPress(
  amount: 1 | -1,
  resize: boolean,
  cellsCount: number,
  initial: {
    start: GridPositionValue
    end: GridPositionValue
  },
): {
  start: GridPositionValue
  end: GridPositionValue
} {
  let result = { ...initial }

  if (resize) {
    const end = result.end.numericalPosition ?? 1
    result.end = gridPositionValue(
      Math.max((result.start.numericalPosition ?? 1) + 1, Math.min(cellsCount + 1, end + amount)),
    )
  } else {
    const start = result.start.numericalPosition ?? 1
    const end = result.end.numericalPosition ?? 1
    const width = end - start
    const newStart = Math.min(cellsCount - width + 1, Math.max(1, start + amount))
    result.start = gridPositionValue(newStart)
    result.end = gridPositionValue(newStart + width)
  }

  return result
}
