import React from 'react'
import {
  cssKeyword,
  isGridCSSNumber,
  isValidGridDimensionKeyword,
  parseCSSNumber,
  parseGridCSSMinmaxOrRepeat,
  stringifyGridDimension,
  type CSSKeyword,
  type CSSNumber,
  type GridDimension,
  type ValidGridDimensionKeyword,
} from '../../components/inspector/common/css-utils'
import { isRight } from '../../core/shared/either'
import { StringInput } from './string-input'
import type { DropdownMenuItem } from '../radix-components'
import {
  DropdownMenu,
  regularDropdownMenuItem,
  separatorDropdownMenuItem,
} from '../radix-components'
import { Icons, SmallerIcons } from '../icons'
import { NO_OP } from '../../core/shared/utils'
import type { ControlStatus } from '../../uuiui-deps'

interface GridExpressionInputProps {
  testId: string
  value: GridDimension
  onUpdateNumberOrKeyword: (v: CSSNumber | CSSKeyword<ValidGridDimensionKeyword>) => void
  onUpdateDimension: (v: GridDimension) => void
  onFocus: () => void
  onBlur: () => void
  keywords: Array<{ label: string; value: CSSKeyword<any> }>
}

export const GridExpressionInput = React.memo(
  ({
    testId,
    value,
    onUpdateNumberOrKeyword,
    onUpdateDimension,
    onFocus,
    onBlur,
    keywords,
  }: GridExpressionInputProps) => {
    const [printValue, setPrintValue] = React.useState<string>(stringifyGridDimension(value))
    React.useEffect(() => setPrintValue(stringifyGridDimension(value)), [value])

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

        setPrintValue(stringifyGridDimension(value))
      },
      [printValue, onUpdateNumberOrKeyword, onUpdateDimension, value],
    )

    const [hover, setHover] = React.useState(false)
    const [dropdownOpen, setDropdownOpen] = React.useState(false)
    const onMouseOver = React.useCallback(() => {
      setHover(true)
    }, [])
    const onMouseOut = React.useCallback(() => {
      setHover(false)
    }, [])

    const dropdownButtonId = `${testId}-dropdown`

    const dropdownButton = React.useCallback(
      () => (
        <SmallerIcons.ExpansionArrowDown
          testId={dropdownButtonId}
          style={{
            visibility: hover || dropdownOpen ? 'visible' : 'hidden',
            cursor: 'pointer',
            marginRight: 5,
          }}
        />
      ),
      [dropdownButtonId, hover, dropdownOpen],
    )

    const dropdownItems = React.useMemo((): DropdownMenuItem[] => {
      let items: DropdownMenuItem[] = []
      items.push(
        regularDropdownMenuItem({
          id: 'dropdown-input-value',
          icon: <Icons.Checkmark color='white' width={16} height={16} />,
          label: printValue,
          disabled: true,
          onSelect: NO_OP,
        }),
      )
      if (keywords.length > 0) {
        items.push(separatorDropdownMenuItem('dropdown-separator'))
      }
      items.push(
        ...keywords.map((keyword, idx): DropdownMenuItem => {
          return regularDropdownMenuItem({
            id: `dropdown-label-${keyword.value.value}`,
            icon: <div style={{ width: 16, height: 16 }} />,
            label: keyword.label,
            onSelect: () => onUpdateNumberOrKeyword(keyword.value),
          })
        }),
      )
      return items
    }, [keywords, printValue, onUpdateNumberOrKeyword])

    return (
      <div
        style={{
          position: 'relative',
          borderRadius: 2,
          display: 'flex',
          alignItems: 'center',
        }}
        onMouseOver={onMouseOver}
        onMouseOut={onMouseOut}
      >
        <StringInput
          style={{ width: '100%' }}
          testId={testId}
          value={printValue}
          onChange={onChange}
          onKeyDown={onKeyDown}
          onFocus={onFocus}
          onBlur={onBlur}
        />
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            position: 'absolute',
            top: 0,
            bottom: 0,
            right: 0,
          }}
        >
          <DropdownMenu
            align='end'
            items={dropdownItems}
            opener={dropdownButton}
            onOpenChange={setDropdownOpen}
            style={{
              marginTop: 8,
            }}
          />
        </div>
      </div>
    )
  },
)
GridExpressionInput.displayName = 'GridExpressionInput'
