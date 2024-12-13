import type { CSSProperties } from 'react'
import React from 'react'
import { Icons, SmallerIcons } from '../icons'
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
import type { IcnProps } from '../icn'
import { mapDropNulls } from '../../core/shared/array-utils'

export type KeywordForControl<T extends string> =
  | { label: string; value: CSSKeyword<T> }
  | 'separator'

type NumberOrKeywordControlProps<T extends string> = {
  testId: string
  style?: CSSProperties
  onSubmitValue: (value: UnknownOrEmptyInput<CSSNumber | CSSKeyword<T>>) => void
  value: CSSNumber | CSSKeyword<T>
  valueAlias?: string
  keywords: Array<KeywordForControl<T>>
  keywordTypeCheck: (keyword: unknown) => keyword is T
  controlStatus?: ControlStatus
  labelInner?: string | IcnProps
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
    if (props.controlStatus !== 'off') {
      items.push(
        regularDropdownMenuItem({
          id: 'dropdown-input-value',
          icon: <Icons.Checkmark color='white' width={16} height={16} />,
          label: `${props.value.value}${isCSSNumber(props.value) ? props.value.unit ?? '' : ''}${
            props.valueAlias != null ? ` (${props.valueAlias})` : ''
          }`,
          disabled: true,
          onSelect: NO_OP,
        }),
      )
    }
    if (props.keywords.length > 0) {
      items.push(separatorDropdownMenuItem('dropdown-separator'))
    }
    items.push(
      ...props.keywords.map((keyword, idx): DropdownMenuItem => {
        if (keyword === 'separator') {
          return separatorDropdownMenuItem(`keyword-separator-${idx}`)
        }
        return regularDropdownMenuItem({
          id: `dropdown-label-${keyword.value.value}`,
          icon: <div style={{ width: 16, height: 16 }} />,
          label: keyword.label,
          onSelect: () => onSubmitValue(keyword.value),
        })
      }),
    )
    return items
  }, [props.value, props.keywords, onSubmitValue, props.controlStatus, props.valueAlias])

  const keywordsWithoutSeparators = React.useMemo(() => {
    return mapDropNulls((k) => (k !== 'separator' ? k : null), props.keywords).map((k) => k.value)
  }, [props.keywords])

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
        validKeywords={keywordsWithoutSeparators}
        style={{ flex: 1 }}
        controlStatus={hover || dropdownOpen ? 'detected' : props.controlStatus}
        labelInner={props.labelInner}
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
