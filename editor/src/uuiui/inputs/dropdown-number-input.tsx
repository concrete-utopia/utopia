import type { CSSProperties } from 'react'
import React from 'react'
import { Icons } from '../icons'
import { DropdownMenu, type DropdownMenuItem } from '../radix-components'
import { NumberOrKeywordInput } from './new-number-or-keyword-control'
import type {
  CSSKeyword,
  CSSNumber,
  UnknownOrEmptyInput,
} from '../../components/inspector/common/css-utils'
import { cssKeyword, isCSSNumber } from '../../components/inspector/common/css-utils'
import { NO_OP } from '../../core/shared/utils'

type DropdownNumberInputProps = {
  onSubmitValue: (value: UnknownOrEmptyInput<CSSNumber | CSSKeyword>) => void
  value: CSSNumber | CSSKeyword
  testId: string
  keywords: { label: string; value: string }[]
  style?: CSSProperties
}

export const DropdownNumberInput = React.memo((props: DropdownNumberInputProps) => {
  const { onSubmitValue: propsOnSubmitValue } = props

  const [hover, setHover] = React.useState(false)
  const [dropdownOpen, setDropdownOpen] = React.useState(false)

  const onSubmitValue = React.useCallback(
    (value: UnknownOrEmptyInput<CSSNumber | CSSKeyword>) => {
      propsOnSubmitValue(value)
      setHover(false)
    },
    [propsOnSubmitValue],
  )
  const dropdownButton = React.useCallback(() => <Icons.ExpansionArrowDown />, [])

  const dropdownItems = React.useMemo(
    (): DropdownMenuItem[] => [
      {
        id: 'dropdown-input-value',
        icon: <Icons.Checkmark color='white' width={16} height={16} />,
        label: `${props.value.value}${isCSSNumber(props.value) ? props.value.unit ?? '' : ''}`,
        disabled: true,
        onSelect: NO_OP,
      },
      {
        id: 'dropdown-separator',
        label: 'separator',
        separator: true,
        onSelect: NO_OP,
      },
      ...props.keywords.map((keyword): DropdownMenuItem => {
        return {
          id: `dropdown-label-${keyword.value}`,
          icon: <div style={{ width: 16, height: 16 }} />,
          label: keyword.label,
          onSelect: () => onSubmitValue(cssKeyword(keyword.value)),
        }
      }),
    ],
    [props.value, props.keywords, onSubmitValue],
  )

  return (
    <div
      style={{
        position: 'relative',
        borderRadius: 2,
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
        style={props.style}
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
          cursor: 'pointer',
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
})
DropdownNumberInput.displayName = 'KeywordNumberInput'
