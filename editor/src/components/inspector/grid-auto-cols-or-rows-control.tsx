import React from 'react'
import { useDispatch } from '../editor/store/dispatch-context'
import { UtopiaTheme } from '../../uuiui'
import type {
  CSSKeyword,
  CSSNumber,
  UnknownOrEmptyInput,
  ValidGridDimensionKeyword,
} from './common/css-utils'
import {
  cssKeyword,
  gridCSSKeyword,
  printArrayGridDimensions,
  type GridDimension,
} from './common/css-utils'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { setProperty } from '../canvas/commands/set-property-command'
import * as PP from '../../core/shared/property-path'
import { type ElementInstanceMetadata } from '../../core/shared/element-template'
import { GridExpressionInput } from '../../uuiui/inputs/grid-expression-input'
import {
  gridDimensionDropdownKeywords,
  parseGridDimensionInput,
  useGridExpressionInputFocused,
} from './grid-helpers'

export const GridAutoColsOrRowsControl = React.memo(
  (props: { axis: 'column' | 'row'; grid: ElementInstanceMetadata; label: string }) => {
    const autoColsOrRowsValueFocused = useGridExpressionInputFocused()

    return (
      <div
        style={{
          display: 'grid',
          gridAutoFlow: 'column',
          alignItems: 'center',
          gap: 6,
          gridTemplateColumns: autoColsOrRowsValueFocused.focused
            ? '60px auto'
            : `60px auto ${UtopiaTheme.layout.inputHeight.default}px`,
          gridTemplateRows: '1fr',
          width: '100%',
        }}
      >
        <GridAutoColsOrRowsControlInner {...props} />
      </div>
    )
  },
)
GridAutoColsOrRowsControl.displayName = 'GridAutoColsOrRowsControl'

export const GridAutoColsOrRowsControlInner = React.memo(
  (props: { axis: 'column' | 'row'; grid: ElementInstanceMetadata; label: string }) => {
    const value = React.useMemo(() => {
      const template = props.grid.specialSizeMeasurements.containerGridPropertiesFromProps
      const data = props.axis === 'column' ? template.gridAutoColumns : template.gridAutoRows
      if (data?.type !== 'DIMENSIONS') {
        return null
      }
      return data.dimensions[0]
    }, [props.grid, props.axis])

    const dispatch = useDispatch()

    const onUpdateDimension = React.useCallback(
      (newDimension: GridDimension) => {
        dispatch([
          applyCommandsAction([
            setProperty(
              'always',
              props.grid.elementPath,
              PP.create('style', props.axis === 'column' ? 'gridAutoColumns' : 'gridAutoRows'),
              printArrayGridDimensions([newDimension]),
            ),
          ]),
        ])
      },
      [props.grid, props.axis, dispatch],
    )

    const onUpdateNumberOrKeyword = React.useCallback(
      (newValue: UnknownOrEmptyInput<CSSNumber | CSSKeyword<ValidGridDimensionKeyword>>) => {
        const parsed = parseGridDimensionInput(newValue, null)
        if (parsed == null) {
          return
        }
        onUpdateDimension(parsed)
      },
      [onUpdateDimension],
    )

    const autoColsOrRowsValueFocused = useGridExpressionInputFocused()

    return (
      <React.Fragment>
        <div>{props.label}</div>
        <GridExpressionInput
          testId={GridAutoColsOrRowsControlTestId(props.axis)}
          value={value ?? gridCSSKeyword(cssKeyword('auto'), null)}
          onUpdateNumberOrKeyword={onUpdateNumberOrKeyword}
          onUpdateDimension={onUpdateDimension}
          onFocus={autoColsOrRowsValueFocused.onFocus}
          onBlur={autoColsOrRowsValueFocused.onBlur}
          keywords={gridDimensionDropdownKeywords}
          defaultValue={gridCSSKeyword(cssKeyword('auto'), null)}
        />
      </React.Fragment>
    )
  },
)
GridAutoColsOrRowsControlInner.displayName = 'GridAutoColsOrRowsControlInner'

export function GridAutoColsOrRowsControlTestId(axis: 'column' | 'row'): string {
  return `grid-template-auto-${axis}`
}
