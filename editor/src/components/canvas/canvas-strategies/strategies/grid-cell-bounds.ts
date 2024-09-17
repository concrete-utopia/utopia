import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import type { WindowPoint, WindowRectangle } from '../../../../core/shared/math-utils'
import {
  canvasPoint,
  isInfinityRectangle,
  offsetPoint,
  rectContainsPoint,
  windowRectangle,
} from '../../../../core/shared/math-utils'
import * as EP from '../../../../core/shared/element-path'
import {
  getGlobalFramesOfGridCells,
  type GridCellGlobalFrames,
  type TargetGridCellData,
} from './grid-helpers'
import type { CanvasPoint } from '../../../../core/shared/math-utils'

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

export function getGridCellUnderMouseFromMetadata(
  grid: ElementInstanceMetadata,
  point: CanvasPoint,
): TargetGridCellData | null {
  const gridCellGlobalFrames = getGlobalFramesOfGridCells(grid)

  if (gridCellGlobalFrames == null) {
    return null
  }

  return getGridCellUnderPoint(gridCellGlobalFrames, point)
}

// TODO: we could optimize this with binary search, but huge grids are rare
function getGridCellUnderPoint(
  gridCellGlobalFrames: GridCellGlobalFrames,
  point: CanvasPoint,
): TargetGridCellData | null {
  for (let i = 0; i < gridCellGlobalFrames.length; i++) {
    for (let j = 0; j < gridCellGlobalFrames[i].length; j++) {
      if (rectContainsPoint(gridCellGlobalFrames[i][j], point)) {
        return {
          gridCellCoordinates: gridCellCoordinates(i + 1, j + 1),
          cellCanvasRectangle: gridCellGlobalFrames[i][j],
        }
      }
    }
  }
  return null
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

export function getGridCellBoundsFromCanvas(
  cell: ElementInstanceMetadata,
  grid: ElementInstanceMetadata,
) {
  const cellFrame = cell.globalFrame
  if (cellFrame == null || isInfinityRectangle(cellFrame)) {
    return null
  }

  const cellOrigin = getGridCellUnderMouseFromMetadata(grid, cellFrame)
  if (cellOrigin == null) {
    return null
  }

  const cellEndPoint = offsetPoint(
    cellFrame,
    canvasPoint({
      x: cellFrame.width,
      y: cellFrame.height,
    }),
  )
  const cellEnd = getGridCellUnderMouseFromMetadata(grid, cellEndPoint)
  if (cellEnd == null) {
    return null
  }

  const cellOriginCoords = cellOrigin.gridCellCoordinates
  const cellEndCoords = cellEnd.gridCellCoordinates

  const cellWidth = cellEndCoords.column - cellOriginCoords.column + 1
  const cellHeight = cellEndCoords.row - cellOriginCoords.row + 1

  return {
    column: cellOriginCoords.column,
    row: cellOriginCoords.row,
    width: cellWidth,
    height: cellHeight,
  }
}

export function getGridPlaceholderDomElementFromCoordinates(params: {
  row: number
  column: number
}): HTMLElement | null {
  return document.querySelector(
    `[data-grid-row="${params.row}"]` + `[data-grid-column="${params.column}"]`,
  )
}

export function getCellWindowRect(coords: GridCellCoordinates): WindowRectangle | null {
  const element = getGridPlaceholderDomElementFromCoordinates(coords)
  if (element == null) {
    return null
  }

  const domRect = element!.getBoundingClientRect()
  const windowRect = windowRectangle(domRect)
  return windowRect
}
