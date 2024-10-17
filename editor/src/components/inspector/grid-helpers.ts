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
      currentValue?.areaName ?? null,
    )
  } else if (isCSSKeyword(value)) {
    return gridCSSKeyword(value, currentValue?.areaName ?? null)
  } else if (isEmptyInputValue(value)) {
    return gridCSSKeyword(cssKeyword('auto'), currentValue?.areaName ?? null)
  } else {
    return null
  }
}

export const gridDimensionDropdownKeywords = [
  { label: 'Auto', value: cssKeyword('auto') },
  { label: 'Min-Content', value: cssKeyword('min-content') },
  { label: 'Max-Content', value: cssKeyword('max-content') },
]
