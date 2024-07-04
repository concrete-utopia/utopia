import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  GridElementProperties,
} from '../../../../core/shared/element-template'
import type { CanvasVector } from '../../../../core/shared/math-utils'
import {
  offsetPoint,
  rectContainsPoint,
  windowRectangle,
  type WindowPoint,
} from '../../../../core/shared/math-utils'
import { create } from '../../../../core/shared/property-path'
import type { CanvasCommand } from '../../commands/commands'
import { setProperty } from '../../commands/set-property-command'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import type { DragInteractionData } from '../interaction-state'
import type { GridCustomStrategyState } from '../canvas-strategy-types'
import type { GridCellCoordinates } from '../../controls/grid-controls'
import { gridCellCoordinates } from '../../controls/grid-controls'
import * as EP from '../../../../core/shared/element-path'

export function getGridCellUnderMouse(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, false)
}

function getGridCellUnderMouseRecursive(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, true)
}

const gridCellTargetIdPrefix = 'grid-cell-target-'

export function gridCellTargetId(
  gridElementPath: ElementPath,
  row: number,
  column: number,
): string {
  return gridCellTargetIdPrefix + `${EP.toString(gridElementPath)}-${row}-${column}`
}

function isGridCellTargetId(id: string): boolean {
  return id.startsWith(gridCellTargetIdPrefix)
}

function getGridCellAtPoint(
  windowPoint: WindowPoint,
  duplicating: boolean,
): { id: string; coordinates: GridCellCoordinates } | null {
  function maybeRecursivelyFindCellAtPoint(elements: Element[]): Element | null {
    // If this used during duplication, the canvas controls will be in the way and we need to traverse the children too.
    for (const element of elements) {
      if (isGridCellTargetId(element.id)) {
        const rect = element.getBoundingClientRect()
        if (rectContainsPoint(windowRectangle(rect), windowPoint)) {
          return element
        }
      }

      if (duplicating) {
        const child = maybeRecursivelyFindCellAtPoint(Array.from(element.children))
        if (child != null) {
          return child
        }
      }
    }

    return null
  }

  const cellUnderMouse = maybeRecursivelyFindCellAtPoint(
    document.elementsFromPoint(windowPoint.x, windowPoint.y),
  )
  if (cellUnderMouse == null) {
    return null
  }

  const row = cellUnderMouse.getAttribute('data-grid-row')
  const column = cellUnderMouse.getAttribute('data-grid-column')
  return {
    id: cellUnderMouse.id,
    coordinates: gridCellCoordinates(
      row == null ? 0 : parseInt(row),
      column == null ? 0 : parseInt(column),
    ),
  }
}

export function runGridRearrangeMove(
  targetElement: ElementPath,
  selectedElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
  interactionData: DragInteractionData,
  canvasScale: number,
  canvasOffset: CanvasVector,
  customState: GridCustomStrategyState,
  duplicating: boolean,
): {
  commands: CanvasCommand[]
  targetCell: GridCellCoordinates | null
  draggingFromCell: GridCellCoordinates | null
  originalRootCell: GridCellCoordinates | null
  targetRootCell: GridCellCoordinates | null
} {
  if (interactionData.drag == null) {
    return {
      commands: [],
      targetCell: null,
      originalRootCell: null,
      draggingFromCell: null,
      targetRootCell: null,
    }
  }

  const mouseWindowPoint = canvasPointToWindowPoint(
    offsetPoint(interactionData.dragStart, interactionData.drag),
    canvasScale,
    canvasOffset,
  )

  const newTargetCell = getTargetCell(customState.targetCell, duplicating, mouseWindowPoint)
  if (newTargetCell == null) {
    return {
      commands: [],
      targetCell: null,
      draggingFromCell: null,
      originalRootCell: null,
      targetRootCell: null,
    }
  }

  const originalElementMetadata = MetadataUtils.findElementByElementPath(
    jsxMetadata,
    selectedElement,
  )
  if (originalElementMetadata == null) {
    return {
      commands: [],
      targetCell: null,
      originalRootCell: null,
      draggingFromCell: null,
      targetRootCell: null,
    }
  }

  const gridProperties = getElementGridProperties(originalElementMetadata)

  // calculate the difference between the cell the mouse started the interaction from, and the "root"
  // cell of the element, meaning the top-left-most cell the element occupies.
  const draggingFromCell = customState.draggingFromCell ?? newTargetCell
  const rootCell =
    customState.originalRootCell ?? gridCellCoordinates(gridProperties.row, gridProperties.column)
  const coordsDiff = getCellCoordsDelta(draggingFromCell, rootCell)

  // get the new adjusted row
  const row = getCoordBounds(newTargetCell, 'row', gridProperties.width, coordsDiff.row)
  // get the new adjusted column
  const column = getCoordBounds(newTargetCell, 'column', gridProperties.height, coordsDiff.column)

  const targetRootCell = gridCellCoordinates(row.start, column.start)

  return {
    commands: [
      setProperty('always', targetElement, create('style', 'gridColumnStart'), column.start),
      setProperty('always', targetElement, create('style', 'gridColumnEnd'), column.end),
      setProperty('always', targetElement, create('style', 'gridRowStart'), row.start),
      setProperty('always', targetElement, create('style', 'gridRowEnd'), row.end),
    ],
    targetCell: newTargetCell,
    originalRootCell: rootCell,
    draggingFromCell: draggingFromCell,
    targetRootCell: targetRootCell,
  }
}

function getTargetCell(
  previousTargetCell: GridCellCoordinates | null,
  duplicating: boolean,
  mouseWindowPoint: WindowPoint,
): GridCellCoordinates | null {
  let cell = previousTargetCell ?? null
  const cellUnderMouse = duplicating
    ? getGridCellUnderMouseRecursive(mouseWindowPoint)
    : getGridCellUnderMouse(mouseWindowPoint)
  if (cellUnderMouse != null) {
    cell = cellUnderMouse.coordinates
  }
  if (cell == null || cell.row < 1 || cell.column < 1) {
    return null
  }
  return cell
}

function getElementGridProperties(element: ElementInstanceMetadata): {
  row: number
  width: number
  column: number
  height: number
} {
  // get the grid fixtures (start and end for column and row) from the element metadata
  function getGridProperty(field: keyof GridElementProperties, fallback: number) {
    const propValue = element.specialSizeMeasurements.elementGridProperties[field]
    return propValue == null || propValue === 'auto' ? 0 : propValue.numericalPosition ?? fallback
  }
  const column = getGridProperty('gridColumnStart', 0)
  const height = getGridProperty('gridColumnEnd', 1) - column
  const row = getGridProperty('gridRowStart', 0)
  const width = getGridProperty('gridRowEnd', 1) - row

  return {
    row,
    width,
    column,
    height,
  }
}

function getCellCoordsDelta(
  dragFrom: GridCellCoordinates,
  rootCell: GridCellCoordinates,
): GridCellCoordinates {
  const rowDiff = dragFrom.row - rootCell.row
  const columnDiff = dragFrom.column - rootCell.column

  return gridCellCoordinates(rowDiff, columnDiff)
}

function getCoordBounds(
  cell: GridCellCoordinates,
  coord: 'column' | 'row',
  size: number, // width or height
  adjustOffset: number, // adjustment based on the difference between the initial dragging cell and the root cell
): { start: number; end: number } {
  // the start is the first cell's coord the element will occupy
  const start = Math.max(1, cell[coord] - adjustOffset)
  // the end is the last cell's coord the element will occupy
  const end = Math.max(1, start + size)
  return { start, end }
}
