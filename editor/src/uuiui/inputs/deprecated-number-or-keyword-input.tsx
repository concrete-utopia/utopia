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
  isCSSNumber,
  isUnknownInputValue,
  parseCSSNumber,
} from '../../components/inspector/common/css-utils'
import type {
  InspectorControlProps,
  OnSubmitValue,
  OnSubmitValueOrUnknownOrEmpty,
} from '../../components/inspector/controls/control'
import type {
  KeywordControlOptions,
  ValidKeywords,
} from '../../components/inspector/controls/keyword-control'
import {
  KeywordControl,
  parseValidKeyword,
} from '../../components/inspector/controls/keyword-control'
import { isRight } from '../../core/shared/either'
import type { NumberInputOptions } from './number-input'
import { NumberInput } from './number-input'

function parseUnknownInputValueAsNumberOrKeyword(
  newValue: UnknownOrEmptyInput<CSSNumber | CSSKeyword>,
  validKeywords: ValidKeywords,
): UnknownOrEmptyInput<CSSNumber | CSSKeyword> {
  if (isUnknownInputValue(newValue)) {
    const parsedNumber = parseCSSNumber(newValue.value, 'AnyValid', null)
    if (isRight(parsedNumber)) {
      return parsedNumber.value
    } else {
      const parsedKeyword = parseValidKeyword(newValue.value, validKeywords)
      if (isRight(parsedKeyword)) {
        return parsedKeyword.value
      }
    }
  }
  return newValue
}

interface NumberOrKeywordControlProps extends InspectorControlProps {
  value: CSSNumber | CSSKeyword
  onSubmitValue: OnSubmitValueOrUnknownOrEmpty<CSSNumber | CSSKeyword>
  onTransientSubmitValue: OnSubmitValueOrUnknownOrEmpty<CSSNumber>
  numberInputOptions: NumberInputOptions
  keywordControlOptions: KeywordControlOptions
  labelInner: string
}

export const DEPRECATED_NumberOrKeywordControl = React.memo<NumberOrKeywordControlProps>(
  ({
    value,
    numberInputOptions,
    keywordControlOptions,
    onSubmitValue: propsOnSubmitValue,
    onTransientSubmitValue,
    style,
    id,
    testId,
    className,
    controlStatus,
    labelInner,
  }) => {
    const onSubmitValue: OnSubmitValue<UnknownOrEmptyInput<CSSNumber | CSSKeyword>> =
      React.useCallback(
        (newValue) => {
          const parsedNewValue = parseUnknownInputValueAsNumberOrKeyword(
            newValue,
            keywordControlOptions.validKeywords,
          )
          propsOnSubmitValue(parsedNewValue)
        },
        [keywordControlOptions.validKeywords, propsOnSubmitValue],
      )
    if (isCSSNumber(value)) {
      return (
        <NumberInput
          style={style}
          className={className}
          id={id}
          testId={testId}
          value={value}
          onSubmitValue={onSubmitValue}
          onTransientSubmitValue={onTransientSubmitValue}
          controlStatus={controlStatus}
          labelInner={labelInner}
          incrementControls={false}
          {...numberInputOptions}
        />
      )
    } else {
      return (
        <KeywordControl
          style={style}
          className={className}
          id={id}
          testId={testId}
          value={value}
          onSubmitValue={onSubmitValue}
          controlStatus={controlStatus}
          {...keywordControlOptions}
        />
      )
    }
  },
)
