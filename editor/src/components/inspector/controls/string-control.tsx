/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import classNames from 'classnames'
import React from 'react'
import { StringInput } from '../../../uuiui'
import { usePropControlledState_DEPRECATED } from '../common/inspector-utils'
import type { DEPRECATEDControlProps } from './control'

export interface StringControlOptions {
  DEPRECATED_labelBelow?: React.ReactChild
}

export const StringControl = React.memo(
  React.forwardRef<HTMLInputElement, DEPRECATEDControlProps<string>>(
    ({ value: propsValue, controlStyles, onBlur, onSubmitValue, ...props }, ref) => {
      const [mixed, setMixed] = usePropControlledState_DEPRECATED<boolean>(controlStyles.mixed)
      const [editDisabled, setEditDisabled] = usePropControlledState_DEPRECATED<boolean>(
        !controlStyles.interactive,
      )
      const [stateValue, setStateValue] = usePropControlledState_DEPRECATED<string>(propsValue)

      const { showContent, mixed: controlStylesMixed } = controlStyles
      const getDisplayValue = React.useCallback(() => {
        return showContent && !controlStylesMixed ? stateValue : ''
      }, [showContent, controlStylesMixed, stateValue])

      const getValueString = React.useCallback(
        (e: React.SyntheticEvent<HTMLInputElement>): string => {
          return e.currentTarget.value
        },
        [],
      )

      const inputOnBlur = React.useCallback(
        (e: React.FocusEvent<HTMLInputElement>) => {
          if (onBlur != null) {
            onBlur(e)
          }
          if (propsValue !== stateValue) {
            onSubmitValue(stateValue)
          }
        },
        [onBlur, onSubmitValue, propsValue, stateValue],
      )

      const inputOnChange = React.useCallback(
        (e: React.ChangeEvent<HTMLInputElement>) => {
          const value = getValueString(e)
          setStateValue(value)
          setMixed(false)
          setEditDisabled(false)
        },
        [getValueString, setEditDisabled, setMixed, setStateValue],
      )

      const inputClassName = classNames('string-control', props.controlClassName)

      return (
        // this form madness is a hack due to chrome ignoring autoComplete='off' on individual `input`s
        <StringInput
          ref={ref}
          id={props.id}
          testId={props.testId}
          onContextMenu={props.onContextMenu}
          disabled={editDisabled}
          className={inputClassName}
          placeholder={mixed ? 'mixed' : undefined}
          onFocus={props.onFocus}
          focusOnMount={props.focus}
          onBlur={inputOnBlur}
          onChange={inputOnChange}
          value={getDisplayValue()}
          autoComplete='off'
          spellCheck={false}
          DEPRECATED_labelBelow={
            (props.DEPRECATED_controlOptions as StringControlOptions)?.DEPRECATED_labelBelow
          }
          style={props.style}
          controlStatus={props.controlStatus}
        />
      )
    },
  ),
)
