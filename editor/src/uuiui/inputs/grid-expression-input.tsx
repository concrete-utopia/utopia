import React from 'react'
import {
  cssKeyword,
  isGridCSSNumber,
  isValidGridDimensionKeyword,
  parseCSSNumber,
  parseGridCSSMinmaxOrRepeat,
  printGridDimension,
  type CSSKeyword,
  type CSSNumber,
  type GridDimension,
  type ValidGridDimensionKeyword,
} from '../../components/inspector/common/css-utils'
import { isRight } from '../../core/shared/either'
import { StringInput } from './string-input'

interface GridExpressionInputProps {
  testId: string
  value: GridDimension
  onUpdateNumberOrKeyword: (v: CSSNumber | CSSKeyword<ValidGridDimensionKeyword>) => void
  onUpdateDimension: (v: GridDimension) => void
  onFocus: () => void
  onBlur: () => void
}

export const GridExpressionInput = React.memo(
  ({
    testId,
    value,
    onUpdateNumberOrKeyword,
    onUpdateDimension,
    onFocus,
    onBlur,
  }: GridExpressionInputProps) => {
    const [printValue, setPrintValue] = React.useState<string>(printGridDimension(value))
    React.useEffect(() => setPrintValue(printGridDimension(value)), [value])

    const onChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
      setPrintValue(e.target.value)
    }, [])

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent) => {
        if (e.key !== 'Enter') {
          return
        }
        if (isValidGridDimensionKeyword(printValue)) {
          return onUpdateNumberOrKeyword(cssKeyword(printValue))
        }

        const defaultUnit = isGridCSSNumber(value) ? value.value.unit : 'px'
        const maybeNumber = parseCSSNumber(printValue, 'AnyValid', defaultUnit)
        if (isRight(maybeNumber)) {
          return onUpdateNumberOrKeyword(maybeNumber.value)
        }

        const maybeMinmax = parseGridCSSMinmaxOrRepeat(printValue)
        if (maybeMinmax != null) {
          return onUpdateDimension({
            ...maybeMinmax,
            areaName: value.areaName,
          } as GridDimension)
        }

        if (printValue === '') {
          return onUpdateNumberOrKeyword(cssKeyword('auto'))
        }

        setPrintValue(printGridDimension(value))
      },
      [printValue, onUpdateNumberOrKeyword, onUpdateDimension, value],
    )

    return (
      <StringInput
        style={{ width: '100%' }}
        testId={testId}
        value={printValue}
        onChange={onChange}
        onKeyDown={onKeyDown}
        onFocus={onFocus}
        onBlur={onBlur}
      />
    )
  },
)
GridExpressionInput.displayName = 'GridExpressionInput'
