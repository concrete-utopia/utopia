/** @jsx jsx */
import { jsx } from '@emotion/react'
import classNames from 'classnames'
import * as React from 'react'
import { Either, isRight, left, right } from '../../../core/shared/either'
import { StringInput } from '../../../uuiui'
import { betterReactMemo, getControlStyles } from '../../../uuiui-deps'
import { cssKeyword, CSSKeyword, emptyInputValue, unknownInputValue } from '../common/css-utils'
import { usePropControlledState } from '../common/inspector-utils'
import { InspectorControlProps, OnSubmitValueOrUnknownOrEmpty } from './control'

export type ValidKeywords = Array<string> | 'all'

export interface KeywordControlOptions {
  validKeywords: ValidKeywords
  focusOnMount?: boolean
}

export interface KeywordControlProps extends KeywordControlOptions, InspectorControlProps {
  value: CSSKeyword
  onSubmitValue: OnSubmitValueOrUnknownOrEmpty<CSSKeyword>
}

export function parseValidKeyword(
  value: string,
  validKeywords: ValidKeywords,
): Either<string, CSSKeyword> {
  if (validKeywords === 'all' || validKeywords.includes(value)) {
    return right(cssKeyword(value))
  } else {
    return left('Keyword is invalid')
  }
}

export const KeywordControl = betterReactMemo<KeywordControlProps>(
  'KeywordControl',
  ({
    value,
    id,
    testId,
    style,
    className,
    onSubmitValue,
    controlStatus = 'simple',
    validKeywords,
    DEPRECATED_labelBelow,
  }) => {
    const controlStyles = getControlStyles(controlStatus)
    const [mixed, setMixed] = usePropControlledState<boolean>(controlStyles.mixed)
    const [stateValue, setStateValue] = usePropControlledState<string>(value.value)
    const ref = React.useRef<HTMLInputElement>(null)

    const { showContent, mixed: controlStylesMixed } = controlStyles
    const getDisplayValue = React.useCallback(() => {
      return showContent && !controlStylesMixed ? stateValue : ''
    }, [controlStylesMixed, showContent, stateValue])

    const getValueString = (e: React.ChangeEvent<HTMLInputElement>): string => {
      return e.currentTarget.value
    }

    const inputOnBlur = React.useCallback(() => {
      const newValue = getDisplayValue()
      if (newValue === '') {
        onSubmitValue(emptyInputValue())
      } else if (validKeywords === 'all' || isRight(parseValidKeyword(newValue, validKeywords))) {
        onSubmitValue(cssKeyword(newValue))
      } else {
        onSubmitValue(unknownInputValue(newValue))
      }
    }, [getDisplayValue, onSubmitValue, validKeywords])

    const inputOnChange = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => {
        setStateValue(getValueString(e))
        setMixed(false)
      },
      [setMixed, setStateValue],
    )

    const inputClassName = classNames('keyword-control', className)

    return (
      <StringInput
        id={id}
        testId={testId}
        ref={ref}
        disabled={!controlStyles.interactive}
        className={inputClassName}
        placeholder={mixed ? 'mixed' : undefined}
        onBlur={inputOnBlur}
        onChange={inputOnChange}
        value={getDisplayValue()}
        autoComplete='off'
        spellCheck={false}
        style={style}
        controlStatus={controlStatus}
        DEPRECATED_labelBelow={DEPRECATED_labelBelow}
      />
    )
  },
)
