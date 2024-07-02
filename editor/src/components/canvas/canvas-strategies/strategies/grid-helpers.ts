import type { WindowPoint } from '../../../../core/shared/math-utils'
import type { GridCellCoordinates } from '../../controls/grid-controls'
import { gridCellCoordinates } from '../../controls/grid-controls'

export function getGridCellUnderMouse(
  windowPoint: WindowPoint,
): { id: string; coordinates: GridCellCoordinates } | null {
  const cellsUnderMouse = document
    .elementsFromPoint(windowPoint.x, windowPoint.y)
    .filter((el) => el.id.startsWith(`gridcell-`))
  if (cellsUnderMouse.length > 0) {
    const cellUnderMouse = cellsUnderMouse[0]
    const row = cellUnderMouse.getAttribute('data-grid-row')
    const column = cellUnderMouse.getAttribute('data-grid-column')
    return {
      id: cellsUnderMouse[0].id,
      coordinates: gridCellCoordinates(
        row == null ? 0 : parseInt(row),
        column == null ? 0 : parseInt(column),
      ),
    }
  }
  return null
}
