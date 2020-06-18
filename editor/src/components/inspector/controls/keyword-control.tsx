/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as classNames from 'classnames'
import * as React from 'react'
import { StringInput } from 'uuiui'
import {
  betterReactMemo,
  ControlStatus,
  getControlStyles,
  OnSubmitValueOrUnknownOrEmpty,
} from 'uuiui-deps'
import { Either, isRight, left, right } from '../../../core/shared/either'
import { cssKeyword, CSSKeyword, emptyInputValue, unknownInputValue } from '../common/css-utils'
import { usePropControlledState } from '../common/inspector-utils'
import { InspectorControlProps } from './control'

export type ValidKeywords = Array<string> | 'all'

export interface KeywordControlOptions {
  validKeywords: ValidKeywords
  focusOnMount?: boolean
}

export interface KeywordControlProps extends KeywordControlOptions, InspectorControlProps {
  value: CSSKeyword
  onSubmitValue: OnSubmitValueOrUnknownOrEmpty<CSSKeyword>
  controlStatus: ControlStatus
  style?: React.CSSProperties
  className?: string
}

function parseValidKeyword(
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
  ({ value, id, style, className, onSubmitValue, controlStatus, validKeywords }) => {
    const controlStyles = getControlStyles(controlStatus)
    const [mixed, setMixed] = usePropControlledState<boolean>(controlStyles.mixed)
    const [stateValue, setStateValue] = usePropControlledState<string>(value.value)
    const ref = React.useRef<HTMLInputElement>(null)

    const getDisplayValue = () => {
      return controlStyles.showContent && !controlStyles.mixed ? stateValue : ''
    }

    const getValueString = (e: React.ChangeEvent<HTMLInputElement>): string => {
      return e.currentTarget.value
    }

    const inputOnBlur = () => {
      const newValue = getDisplayValue()
      if (newValue === '') {
        onSubmitValue(emptyInputValue())
      } else if (validKeywords !== 'all' && isRight(parseValidKeyword(newValue, validKeywords))) {
        onSubmitValue(cssKeyword(newValue))
      } else {
        onSubmitValue(unknownInputValue(newValue))
      }
    }

    const inputOnChange = (e: React.ChangeEvent<HTMLInputElement>) => {
      setStateValue(getValueString(e))
      setMixed(false)
    }

    const inputClassName = classNames('keyword-control', className)

    return (
      <StringInput
        id={id}
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
      />
    )
  },
)
