/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as classNames from 'classnames'
import * as React from 'react'
import { StringInput } from 'uuiui'
import { ControlProps, GenericControlOptions } from './control'
import { usePropControlledState } from '../common/inspector-utils'
import { betterReactMemo } from 'uuiui-deps'

export interface StringControlOptions extends GenericControlOptions {
  tooltipContent?: React.ReactElement<any> | string
  tooltipTrigger?: string
  tooltipHideOnClick?: boolean
}

export const StringControl = betterReactMemo<ControlProps<string>>(
  'StringControl',
  ({ value: propsValue, allowEditOnDoubleClick = false, ...props }) => {
    const [mixed, setMixed] = usePropControlledState<boolean>(props.controlStyles.mixed)
    const [editDisabled, setEditDisabled] = usePropControlledState<boolean>(
      !props.controlStyles.interactive,
    )
    const [stateValue, setStateValue] = usePropControlledState<string>(propsValue)

    const getDisplayValue = () => {
      return props.controlStyles.showContent && !props.controlStyles.mixed ? stateValue : ''
    }

    const getValueString = (e: React.SyntheticEvent<HTMLInputElement>): string => {
      return e.currentTarget.value || ''
    }

    const inputOnBlur = (e: React.FocusEvent<HTMLInputElement>) => {
      if (props.onBlur != null) {
        props.onBlur(e)
      }
      props.onSubmitValue(getDisplayValue())
    }

    const inputOnChange = (e: React.ChangeEvent<HTMLInputElement>) => {
      const value = getValueString(e)
      setStateValue(value)
      setMixed(false)
      setEditDisabled(false)
    }

    const inputOnDoubleClick = () => {
      if (allowEditOnDoubleClick) {
        setEditDisabled(false)
      }
    }

    const inputClassName = classNames('string-control', props.controlClassName)

    const controlOptions: StringControlOptions =
      props.controlOptions != null ? props.controlOptions : {}

    return (
      // this form madness is a hack due to chrome ignoring autoComplete='off' on individual `input`s
      <StringInput
        id={props.id}
        onContextMenu={props.onContextMenu}
        disabled={editDisabled}
        className={inputClassName}
        placeholder={mixed ? 'mixed' : undefined}
        onFocus={props.onFocus}
        onBlur={inputOnBlur}
        onChange={inputOnChange}
        onDoubleClick={inputOnDoubleClick}
        value={getDisplayValue()}
        autoComplete='off'
        spellCheck={false}
        tooltipContent={controlOptions.tooltipContent}
        tooltipTrigger={controlOptions.tooltipTrigger}
        tooltipHideOnClick={controlOptions.tooltipHideOnClick}
        labelBelow={controlOptions.labelBelow}
        style={props.style}
        controlStatus={props.controlStatus}
      />
    )
  },
)
