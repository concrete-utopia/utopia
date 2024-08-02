/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type {
  CSSKeyword,
  CSSNumber,
  UnknownOrEmptyInput,
} from '../../components/inspector/common/css-utils'
import {
  cssNumber,
  isCSSNumber,
  isEmptyInputValue,
  isUnknownInputValue,
  parseCSSNumber,
} from '../../components/inspector/common/css-utils'
import type {
  InspectorControlProps,
  OnSubmitValue,
  OnSubmitValueOrUnknownOrEmpty,
} from '../../components/inspector/controls/control'
import { isRight } from '../../core/shared/either'
import { StringInput } from './string-input'

export interface NumberOrKeywordInputProps<T extends string> extends InspectorControlProps {
  value: CSSNumber | CSSKeyword<T>
  onSubmitValue: OnSubmitValueOrUnknownOrEmpty<CSSNumber | CSSKeyword<T>>
  validKeywords: Array<CSSKeyword<T>>
  labelBelowStyle?: React.CSSProperties
  incrementalControls?: boolean
  showBorder?: boolean
}

export function NumberOrKeywordInput<T extends string>(props: NumberOrKeywordInputProps<T>) {
  const { onSubmitValue: propsOnSubmitValue } = props

  const [stringValue, setStringValue] = React.useState(toStringValue(props.value))
  React.useEffect(() => setStringValue(toStringValue(props.value)), [props.value])

  const onSubmitValue: OnSubmitValue<UnknownOrEmptyInput<string>> = React.useCallback(
    (value) => {
      const parsedNewValue = parseInput<T>(value, props.validKeywords)
      if (parsedNewValue === 'invalid') {
        setStringValue(toStringValue(props.value))
        return
      }

      if (!isUnknownInputValue(value)) {
        setStringValue(isEmptyInputValue(value) ? '' : value)
      }
      propsOnSubmitValue(parsedNewValue)
    },
    [props.value, props.validKeywords, propsOnSubmitValue],
  )

  const onChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setStringValue(e.target.value),
    [],
  )

  return (
    <StringInput
      style={props.style}
      showBorder={props.showBorder}
      className={props.className}
      id={props.id}
      controlStatus={props.controlStatus}
      DEPRECATED_labelBelow={props.DEPRECATED_labelBelow}
      value={stringValue}
      onChange={onChange}
      onSubmitValue={onSubmitValue}
      testId={props.testId}
    />
  )
}

function parseInput<T extends string>(
  value: UnknownOrEmptyInput<string>,
  validKeywords: Array<CSSKeyword<T>>,
): UnknownOrEmptyInput<CSSNumber | CSSKeyword<T> | 'invalid'> {
  if (!isUnknownInputValue(value) && !isEmptyInputValue(value)) {
    const parsedNumber = parseCSSNumber(value, 'AnyValid', null)
    if (isRight(parsedNumber)) {
      return parsedNumber.value
    } else {
      const keyword = validKeywords.find((k) => k.value === value)
      if (keyword != null) {
        return keyword
      } else {
        return 'invalid'
      }
    }
  }
  return cssNumber(0)
}

function toStringValue(v: CSSNumber | CSSKeyword) {
  return isCSSNumber(v) ? `${v.value}${v.unit ?? ''}` : v.value
}
