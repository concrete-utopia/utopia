import type { ElementPath } from 'utopia-shared/src/types'
import type { ElementInstanceMetadata } from '../../../../core/shared/element-template'
import {
  canvasPoint,
  distanceFromPointToRectangle,
  isInfinityRectangle,
  offsetPoint,
  rectContainsPointInclusive,
} from '../../../../core/shared/math-utils'
import * as EP from '../../../../core/shared/element-path'
import type { TargetGridCellData, GridCellGlobalFrames } from './grid-helpers'
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
  const gridCellGlobalFrames = grid.specialSizeMeasurements.gridCellGlobalFrames

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
      if (rectContainsPointInclusive(gridCellGlobalFrames[i][j], point)) {
        return {
          gridCellCoordinates: gridCellCoordinates(i + 1, j + 1),
          cellCanvasRectangle: gridCellGlobalFrames[i][j],
        }
      }
    }
  }
  return null
}

export function getClosestGridCellToPointFromMetadata(
  grid: ElementInstanceMetadata,
  point: CanvasPoint,
): TargetGridCellData | null {
  const gridCellGlobalFrames = grid.specialSizeMeasurements.gridCellGlobalFrames

  if (gridCellGlobalFrames == null) {
    return null
  }

  return getClosestGridCellToPoint(gridCellGlobalFrames, point, 'exclusive')
}

export function getClosestGridCellToPoint(
  gridCellGlobalFrames: GridCellGlobalFrames,
  point: CanvasPoint,
  distanceMatch: 'inclusive' | 'exclusive',
): TargetGridCellData | null {
  let closestCell: TargetGridCellData | null = null
  let closestDistance = Infinity

  for (let i = 0; i < gridCellGlobalFrames.length; i++) {
    for (let j = 0; j < gridCellGlobalFrames[i].length; j++) {
      const currentDistance = distanceFromPointToRectangle(point, gridCellGlobalFrames[i][j])
      const condition =
        distanceMatch === 'inclusive'
          ? currentDistance <= closestDistance
          : currentDistance < closestDistance
      if (condition) {
        closestCell = {
          gridCellCoordinates: gridCellCoordinates(i + 1, j + 1),
          cellCanvasRectangle: gridCellGlobalFrames[i][j],
        }
        closestDistance = currentDistance
      }
    }
  }
  return closestCell
}

export function getGridChildCellCoordBoundsFromCanvas(
  child: ElementInstanceMetadata,
  gridCellGlobalFrames: GridCellGlobalFrames,
) {
  const cellFrame = child.globalFrame
  if (cellFrame == null || isInfinityRectangle(cellFrame) || gridCellGlobalFrames == null) {
    return null
  }

  const cellOrigin = getClosestGridCellToPoint(gridCellGlobalFrames, cellFrame, 'inclusive')
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
  const cellEnd = getClosestGridCellToPoint(gridCellGlobalFrames, cellEndPoint, 'exclusive')
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
