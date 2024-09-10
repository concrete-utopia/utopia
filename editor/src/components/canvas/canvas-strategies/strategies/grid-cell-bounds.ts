import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import type { CanvasVector, WindowPoint, WindowRectangle } from '../../../../core/shared/math-utils'
import {
  isInfinityRectangle,
  offsetPoint,
  rectContainsPoint,
  windowPoint,
  windowRectangle,
} from '../../../../core/shared/math-utils'
import { canvasPointToWindowPoint } from '../../dom-lookup'
import * as EP from '../../../../core/shared/element-path'

export type GridCellCoordinates = { row: number; column: number }

export function gridCellCoordinates(row: number, column: number): GridCellCoordinates {
  return { row: row, column: column }
}

const gridCellTargetIdPrefix = 'grid-cell-target-'

export function gridCellTargetId(
  gridElementPath: ElementPath,
  row: number,
  column: number,
): string {
  return gridCellTargetIdPrefix + `${EP.toString(gridElementPath)}-${row}-${column}`
}

export function getGridCellUnderMouse(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, false)
}

export function getGridCellUnderMouseRecursive(mousePoint: WindowPoint) {
  return getGridCellAtPoint(mousePoint, true)
}

function isGridCellTargetId(id: string): boolean {
  return id.startsWith(gridCellTargetIdPrefix)
}

export function getGridCellAtPoint(
  point: WindowPoint,
  duplicating: boolean,
): { id: string; coordinates: GridCellCoordinates; cellWindowRectangle: WindowRectangle } | null {
  function maybeRecursivelyFindCellAtPoint(
    elements: Element[],
  ): { element: Element; cellWindowRectangle: WindowRectangle } | null {
    // If this used during duplication, the canvas controls will be in the way and we need to traverse the children too.
    for (const element of elements) {
      if (isGridCellTargetId(element.id)) {
        const domRect = element.getBoundingClientRect()
        const windowRect = windowRectangle(domRect)
        if (rectContainsPoint(windowRect, point)) {
          return { element: element, cellWindowRectangle: windowRect }
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
    document.elementsFromPoint(point.x, point.y),
  )
  if (cellUnderMouse == null) {
    return null
  }

  const { element, cellWindowRectangle } = cellUnderMouse
  const row = element.getAttribute('data-grid-row')
  const column = element.getAttribute('data-grid-column')

  return {
    id: element.id,
    cellWindowRectangle: cellWindowRectangle,
    coordinates: gridCellCoordinates(
      row == null ? 0 : parseInt(row),
      column == null ? 0 : parseInt(column),
    ),
  }
}

const GRID_BOUNDS_TOLERANCE = 5 // px

export function getGridCellBoundsFromCanvas(
  cell: ElementInstanceMetadata,
  canvasScale: number,
  canvasOffset: CanvasVector,
) {
  const cellFrame = cell.globalFrame
  if (cellFrame == null || isInfinityRectangle(cellFrame)) {
    return null
  }

  const canvasFrameWidth = cellFrame.width * canvasScale
  const canvasFrameHeight = cellFrame.height * canvasScale

  const cellOriginPoint = offsetPoint(
    canvasPointToWindowPoint(cellFrame, canvasScale, canvasOffset),
    windowPoint({ x: GRID_BOUNDS_TOLERANCE, y: GRID_BOUNDS_TOLERANCE }),
  )
  const cellOrigin = getGridCellAtPoint(cellOriginPoint, true)
  if (cellOrigin == null) {
    return null
  }

  const cellEndPoint = offsetPoint(
    cellOriginPoint,
    windowPoint({
      x: canvasFrameWidth - GRID_BOUNDS_TOLERANCE,
      y: canvasFrameHeight - GRID_BOUNDS_TOLERANCE,
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

  return {
    column: cellOriginCoords.column,
    row: cellOriginCoords.row,
    width: cellWidth,
    height: cellHeight,
  }
}
