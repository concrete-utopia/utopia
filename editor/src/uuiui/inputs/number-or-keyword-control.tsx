import type { CSSProperties } from 'react'
import React from 'react'
import { Icons } from '../icons'
import {
  DropdownMenu,
  regularDropdownMenuItem,
  separatorDropdownMenuItem,
  type DropdownMenuItem,
} from '../radix-components'
import { NumberOrKeywordInput } from './new-number-or-keyword-input'
import type {
  CSSKeyword,
  CSSNumber,
  UnknownOrEmptyInput,
} from '../../components/inspector/common/css-utils'
import { isCSSNumber } from '../../components/inspector/common/css-utils'
import { NO_OP } from '../../core/shared/utils'
import type { ControlStatus } from '../../uuiui-deps'

type NumberOrKeywordControlProps<T extends string> = {
  testId: string
  style?: CSSProperties
  onSubmitValue: (value: UnknownOrEmptyInput<CSSNumber | CSSKeyword<T>>) => void
  value: CSSNumber | CSSKeyword<T>
  keywords: Array<{ label: string; value: CSSKeyword<T> }>
  keywordTypeCheck: (keyword: unknown) => keyword is T
  controlStatus?: ControlStatus
}

export function NumberOrKeywordControl<T extends string>(props: NumberOrKeywordControlProps<T>) {
  const { onSubmitValue: propsOnSubmitValue } = props

  const [hover, setHover] = React.useState(false)
  const [dropdownOpen, setDropdownOpen] = React.useState(false)

  const onSubmitValue = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber | CSSKeyword<T>>) => {
      propsOnSubmitValue(value)
      setHover(false)
    },
    [propsOnSubmitValue],
  )

  const dropdownButtonId = `${props.testId}-dropdown`

  const dropdownButton = React.useCallback(
    () => (
      <Icons.ExpansionArrowDown
        testId={dropdownButtonId}
        style={{
          visibility: hover || dropdownOpen ? 'visible' : 'hidden',
          cursor: 'pointer',
        }}
      />
    ),
    [dropdownButtonId, hover, dropdownOpen],
  )

  const dropdownItems = React.useMemo((): DropdownMenuItem[] => {
    let items: DropdownMenuItem[] = []
    if (props.controlStatus !== 'off') {
      items.push(
        regularDropdownMenuItem({
          id: 'dropdown-input-value',
          icon: <Icons.Checkmark color='white' width={16} height={16} />,
          label: `${props.value.value}${isCSSNumber(props.value) ? props.value.unit ?? '' : ''}`,
          disabled: true,
          onSelect: NO_OP,
        }),
        separatorDropdownMenuItem('dropdown-separator'),
      )
    }
    items.push(
      ...props.keywords.map((keyword): DropdownMenuItem => {
        return regularDropdownMenuItem({
          id: `dropdown-label-${keyword.value.value}`,
          icon: <div style={{ width: 16, height: 16 }} />,
          label: keyword.label,
          onSelect: () => onSubmitValue(keyword.value),
        })
      }),
    )
    return items
  }, [props.value, props.keywords, onSubmitValue, props.controlStatus])

  return (
    <div
      style={{
        ...props.style,
        position: 'relative',
        borderRadius: 2,
        display: 'flex',
        alignItems: 'center',
      }}
      onMouseOver={() => setHover(true)}
      onMouseOut={() => setHover(false)}
    >
      <NumberOrKeywordInput
        value={props.value}
        testId={props.testId}
        showBorder={hover || dropdownOpen}
        onSubmitValue={onSubmitValue}
        incrementalControls={false}
        validKeywords={props.keywords.map((k) => k.value)}
        style={{ flex: 1 }}
        controlStatus={hover || dropdownOpen ? 'detected' : props.controlStatus}
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
}
