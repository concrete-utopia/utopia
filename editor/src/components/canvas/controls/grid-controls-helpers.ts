import type { CSSProperties } from 'react'
import type { GridMeasurementHelperData } from './grid-controls-for-strategies'

export function getGridHelperStyleMatchingTargetGrid(
  grid: GridMeasurementHelperData,
): CSSProperties {
  let style: CSSProperties = {
    ...grid.computedStyling,
    position: 'absolute',
    top: grid.frame.y,
    left: grid.frame.x,
    width: grid.frame.width,
    height: grid.frame.height,
    display: 'grid',
    pointerEvents: 'none',
    borderTop: `${grid.border?.top}px solid transparent`,
    borderLeft: `${grid.border?.left}px solid transparent`,
    borderBottom: `${grid.border?.bottom}px solid transparent`,
    borderRight: `${grid.border?.right}px solid transparent`,
  }

  // Gap needs to be set only if the other two are not present or we'll have rendering issues
  // due to how measurements are calculated.
  if (grid.rowGap != null && grid.columnGap != null) {
    style.rowGap = grid.rowGap
    style.columnGap = grid.columnGap
    delete style['gap']
  }

  return style
}
