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
  emptyInputValue,
  isCSSNumber,
  isEmptyInputValue,
  isUnknownInputValue,
  parseCSSNumber,
  unknownInputValue,
} from '../../components/inspector/common/css-utils'
import type {
  InspectorControlProps,
  OnSubmitValue,
  OnSubmitValueOrUnknownOrEmpty,
} from '../../components/inspector/controls/control'
import { isRight } from '../../core/shared/either'
import { StringInput } from './string-input'
import { Icn, type IcnProps } from '../icn'

export interface NumberOrKeywordInputProps<T extends string> extends InspectorControlProps {
  value: CSSNumber | CSSKeyword<T>
  onSubmitValue: OnSubmitValueOrUnknownOrEmpty<CSSNumber | CSSKeyword<T>>
  validKeywords: Array<CSSKeyword<T>>
  incrementalControls?: boolean
  showBorder?: boolean
  labelInner?: string | IcnProps
}

export function NumberOrKeywordInput<T extends string>(props: NumberOrKeywordInputProps<T>) {
  const { onSubmitValue: propsOnSubmitValue } = props

  const [stringValue, setStringValue] = React.useState(toStringValue(props.value))
  React.useEffect(() => setStringValue(toStringValue(props.value)), [props.value])

  const onSubmitValue: OnSubmitValue<UnknownOrEmptyInput<string>> = React.useCallback(
    (value) => {
      const parsedNewValue = parseInput<T>(value, props.validKeywords)
      if (isUnknownInputValue(parsedNewValue)) {
        setStringValue(toStringValue(props.value))
        propsOnSubmitValue(parsedNewValue.value === '' ? emptyInputValue() : parsedNewValue)
      } else {
        propsOnSubmitValue(parsedNewValue)
      }
    },
    [props.value, props.validKeywords, propsOnSubmitValue],
  )

  const onChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setStringValue(e.target.value),
    [],
  )

  return (
    <div style={{ position: 'relative' }}>
      <div
        style={{
          position: 'absolute',
          left: 0,
          top: 0,
          bottom: 0,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        {typeof props.labelInner === 'object' && 'type' in props.labelInner ? (
          <Icn {...props.labelInner} />
        ) : (
          props.labelInner
        )}
      </div>
      <StringInput
        innerStyle={{ paddingLeft: props.labelInner != null ? 15 : 0 }}
        style={props.style}
        showBorder={props.showBorder}
        className={props.className}
        id={props.id}
        controlStatus={props.controlStatus}
        value={stringValue}
        onChange={onChange}
        onSubmitValue={onSubmitValue}
        testId={props.testId}
      />
    </div>
  )
}

function parseInput<T extends string>(
  value: UnknownOrEmptyInput<string>,
  validKeywords: Array<CSSKeyword<T>>,
): UnknownOrEmptyInput<CSSNumber | CSSKeyword<T>> {
  if (!isUnknownInputValue(value) && !isEmptyInputValue(value)) {
    const parsedNumber = parseCSSNumber(value, 'AnyValid', null)
    if (isRight(parsedNumber)) {
      return parsedNumber.value
    } else {
      return validKeywords.find((k) => k.value === value) ?? unknownInputValue(value)
    }
  }
  return value
}

function toStringValue(v: CSSNumber | CSSKeyword) {
  return isCSSNumber(v) ? `${v.value}${v.unit ?? ''}` : v.value
}
