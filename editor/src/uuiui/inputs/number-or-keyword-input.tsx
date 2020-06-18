/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { betterReactMemo, OnSubmitValue, OnSubmitValueOrUnknownOrEmpty } from 'uuiui-deps'
import {
  CSSKeyword,
  CSSNumber,
  isCSSNumber,
  isUnknownInputValue,
  parseCSSNumber,
  UnknownOrEmptyInput,
} from '../../components/inspector/common/css-utils'
import { InspectorControlProps } from '../../components/inspector/controls/control'
import {
  KeywordControl,
  KeywordControlOptions,
  parseValidKeyword,
  ValidKeywords,
} from '../../components/inspector/controls/keyword-control'
import { isRight } from '../../core/shared/either'
import { NumberInput, NumberInputOptions } from './number-input'

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
}

export const NumberOrKeywordControl = betterReactMemo<NumberOrKeywordControlProps>(
  'NumberOrKeywordControl',
  ({
    value,
    numberInputOptions,
    keywordControlOptions,
    onSubmitValue: propsOnSubmitValue,
    onTransientSubmitValue,
    style,
    id,
    className,
    controlStatus,
    DEPRECATED_labelBelow,
  }) => {
    const onSubmitValue: OnSubmitValue<UnknownOrEmptyInput<
      CSSNumber | CSSKeyword
    >> = React.useCallback(
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
          value={value}
          onSubmitValue={onSubmitValue}
          onTransientSubmitValue={onTransientSubmitValue}
          controlStatus={controlStatus}
          DEPRECATED_labelBelow={DEPRECATED_labelBelow}
          allowUnknownInputValuesToSubmit
          {...numberInputOptions}
        />
      )
    } else {
      return (
        <KeywordControl
          style={style}
          className={className}
          id={id}
          value={value}
          onSubmitValue={onSubmitValue}
          controlStatus={controlStatus}
          DEPRECATED_labelBelow={DEPRECATED_labelBelow}
          {...keywordControlOptions}
        />
      )
    }
  },
)
