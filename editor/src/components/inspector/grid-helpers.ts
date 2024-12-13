import React from 'react'
import type {
  CSSKeyword,
  CSSNumber,
  UnknownOrEmptyInput,
  ValidGridDimensionKeyword,
} from './common/css-utils'
import {
  cssKeyword,
  cssNumber,
  gridCSSKeyword,
  gridCSSNumber,
  isCSSKeyword,
  isCSSNumber,
  isEmptyInputValue,
  isGridCSSNumber,
  type GridDimension,
} from './common/css-utils'
import type { DragInteractionData } from '../canvas/canvas-strategies/interaction-state'
import { offsetPoint } from '../../core/shared/math-utils'
import type { GridCellCoordinates } from '../canvas/canvas-strategies/strategies/grid-cell-bounds'
import {
  getClosestGridCellToPoint,
  gridCellCoordinates,
} from '../canvas/canvas-strategies/strategies/grid-cell-bounds'
import type { GridCellGlobalFrames } from '../canvas/canvas-strategies/strategies/grid-helpers'

export const useGridExpressionInputFocused = () => {
  const [focused, setFocused] = React.useState(false)
  const onFocus = React.useCallback(() => setFocused(true), [])
  const onBlur = React.useCallback(() => setFocused(false), [])
  return { focused, onFocus, onBlur }
}

export function parseGridDimensionInput(
  value: UnknownOrEmptyInput<CSSNumber | CSSKeyword<ValidGridDimensionKeyword>>,
  currentValue: GridDimension | null,
) {
  if (isCSSNumber(value)) {
    const maybeUnit =
      currentValue != null && isGridCSSNumber(currentValue) ? currentValue.value.unit : null
    return gridCSSNumber(
      cssNumber(value.value, value.unit ?? maybeUnit),
      currentValue?.lineName ?? null,
    )
  } else if (isCSSKeyword(value)) {
    return gridCSSKeyword(value, currentValue?.lineName ?? null)
  } else if (isEmptyInputValue(value)) {
    return gridCSSKeyword(cssKeyword('auto'), currentValue?.lineName ?? null)
  } else {
    return null
  }
}

export const gridDimensionDropdownKeywords = [
  { label: 'Auto', value: cssKeyword('auto') },
  { label: 'Min-Content', value: cssKeyword('min-content') },
  { label: 'Max-Content', value: cssKeyword('max-content') },
]

export const GridPositioningProps: Array<keyof React.CSSProperties> = [
  'gridColumn',
  'gridRow',
  'gridColumnStart',
  'gridColumnEnd',
  'gridRowStart',
  'gridRowEnd',
]

export function getTargetGridCellData(
  interactionData: DragInteractionData,
  gridCellGlobalFrames: GridCellGlobalFrames,
  mouseCellPosInOriginalElement: GridCellCoordinates,
): {
  targetCellCoords: GridCellCoordinates
  targetRootCell: GridCellCoordinates
} | null {
  if (interactionData.drag == null) {
    return null
  }
  const mousePos = offsetPoint(interactionData.dragStart, interactionData.drag)
  const targetCellData = getClosestGridCellToPoint(gridCellGlobalFrames, mousePos, 'exclusive')
  if (targetCellData == null) {
    return null
  }
  const targetCellCoords = targetCellData?.gridCellCoordinates
  if (targetCellCoords == null) {
    return null
  }

  const row = Math.max(targetCellCoords.row - mouseCellPosInOriginalElement.row, 1)
  const column = Math.max(targetCellCoords.column - mouseCellPosInOriginalElement.column, 1)

  const targetRootCell = gridCellCoordinates(row, column)

  return {
    targetCellCoords,
    targetRootCell,
  }
}
