import type { CSSProperties } from 'react'
import type { GridMeasurementHelperData } from './grid-controls-for-strategies'
import { getNullableAutoOrTemplateBaseString } from './grid-controls-for-strategies'

export function getStyleMatchingTargetGrid(grid: GridMeasurementHelperData): CSSProperties {
  let style: CSSProperties = {
    position: 'absolute',
    top: grid.frame.y,
    left: grid.frame.x,
    width: grid.frame.width,
    height: grid.frame.height,
    display: 'grid',
    gridTemplateColumns: getNullableAutoOrTemplateBaseString(grid.gridTemplateColumns),
    gridTemplateRows: getNullableAutoOrTemplateBaseString(grid.gridTemplateRows),
    justifyContent: grid.justifyContent ?? 'initial',
    alignContent: grid.alignContent ?? 'initial',
    pointerEvents: 'none',
    padding:
      grid.padding == null
        ? 0
        : `${grid.padding.top}px ${grid.padding.right}px ${grid.padding.bottom}px ${grid.padding.left}px`,
  }

  // Gap needs to be set only if the other two are not present or we'll have rendering issues
  // due to how measurements are calculated.
  if (grid.rowGap != null && grid.columnGap != null) {
    style.rowGap = grid.rowGap
    style.columnGap = grid.columnGap
  } else {
    if (grid.gap != null) {
      style.gap = grid.gap
    }
    if (grid.rowGap != null) {
      style.rowGap = grid.rowGap
    }
    if (grid.columnGap != null) {
      style.columnGap = grid.columnGap
    }
  }

  return style
}
