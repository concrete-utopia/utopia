/** @jsx jsx */
import { jsx } from '@emotion/core'
import styled from '@emotion/styled'
import Tippy from '@tippyjs/react'
import * as React from 'react'
import {
  ControlStatus,
  ControlStyles,
  getControlStyles,
} from '../../components/inspector/common/control-status'
import { UtopiaTheme } from '../styles/theme'
import { OnSubmitValue } from '../../components/inspector/controls/control'
import { stopPropagation } from '../../components/inspector/common/inspector-utils'
import { betterReactMemo } from 'uuiui-deps'
import { InspectorInput } from './base-input'

export interface StringInputProps extends React.InputHTMLAttributes<HTMLInputElement> {
  labelBelow?: string
  tooltipContent?: React.ReactElement<any> | string
  tooltipTrigger?: string
  tooltipHideOnClick?: boolean
  controlStatus?: ControlStatus
  stopEnterAndEscPropagation?: boolean
  focusOnMount?: boolean
  placeholder?: string
  onSubmitValue?: OnSubmitValue<string>
}

// TODO FIX FUCKED UP REFS SITUATION
export const StringInput = betterReactMemo(
  'StringInput',
  React.forwardRef<HTMLInputElement, StringInputProps>(
    (
      {
        labelBelow,
        tooltipContent,
        tooltipTrigger = 'mouseenter focus',
        tooltipHideOnClick,
        controlStatus = 'simple',
        stopEnterAndEscPropagation = true,
        style,
        focusOnMount = false,
        placeholder: initialPlaceHolder,
        onSubmitValue,
        ...inputProps
      },
      initialRef: any, // React.RefObject<HTMLInputElement>
    ) => {
      const ref = initialRef != null ? initialRef : React.createRef()

      const [focused, setFocused] = React.useState<boolean>(false)

      React.useEffect(() => {
        if (focusOnMount && ref.current != null) {
          ref.current.focus()
        }
      }, [focusOnMount, ref])

      const disabled = controlStatus === 'disabled'
      const controlStyles: ControlStyles = getControlStyles(controlStatus)

      const inputPropsKeyDown = inputProps.onKeyDown

      const onKeyDown = React.useCallback(
        (e: React.KeyboardEvent<HTMLInputElement>) => {
          if (inputPropsKeyDown != null) {
            inputPropsKeyDown(e)
          }
          if (
            e.key === 'ArrowRight' ||
            e.key === 'ArrowLeft' ||
            e.key === 'ArrowUp' ||
            e.key === 'ArrowDown'
          ) {
            // handle navigation events without & with modifiers
            e.nativeEvent.stopImmediatePropagation()
          }
          if (e.key === 'Escape' || e.key === 'Enter') {
            e.preventDefault()
            if (stopEnterAndEscPropagation) {
              e.nativeEvent.stopImmediatePropagation()
            }
            if (ref.current != null) {
              ref.current.blur()
            }
          }
        },
        [inputPropsKeyDown, ref, stopEnterAndEscPropagation],
      )

      const onFocus = React.useCallback(
        (e: React.FocusEvent<HTMLInputElement>) => {
          if (inputProps.onFocus != null) {
            // FIXME Should we be doing this in the case where the input is disabled? This feels wrong...
            inputProps.onFocus(e)
          }
          if (disabled) {
            e.preventDefault()
            e.target.blur()
          } else {
            setFocused(true)
            e.target.select()
            e.persist()
          }
        },
        [disabled, inputProps],
      )

      const onBlur = React.useCallback(
        (e: React.FocusEvent<HTMLInputElement>) => {
          setFocused(false)
          if (inputProps.onBlur != null) {
            inputProps.onBlur(e)
          }
          if (onSubmitValue != null && inputProps.value != null) {
            onSubmitValue(inputProps.value + '')
          }
        },
        [inputProps, onSubmitValue],
      )

      let placeholder = initialPlaceHolder
      if (controlStyles.unknown) {
        placeholder = 'unknown'
      } else if (controlStyles.mixed) {
        placeholder = 'mixed'
      }

      const inputElement = (
        <InspectorInput
          {...inputProps}
          controlStyles={controlStyles}
          focused={focused}
          value={inputProps.value}
          css={{
            '&::placeholder': {
              fontStyle: 'italic',
              color: UtopiaTheme.color.subduedForeground.value,
            },
          }}
          onKeyDown={onKeyDown}
          onFocus={onFocus}
          onBlur={onBlur}
          className={inputProps.className}
          ref={ref}
          placeholder={placeholder}
          disabled={!controlStyles.interactive}
          autoComplete='off'
          spellCheck={false}
        />
      )

      return (
        <form autoComplete='off' style={style} onMouseDown={stopPropagation}>
          {tooltipContent == null ? (
            inputElement
          ) : (
            <Tippy
              content={tooltipContent}
              trigger={tooltipTrigger}
              hideOnClick={tooltipHideOnClick}
              placement='bottom'
              delay={[0, 0]}
              arrow={true}
              animation='fade'
            >
              {inputElement}
            </Tippy>
          )}
          {labelBelow == null ? null : (
            <LabelBelow htmlFor={inputProps.id} style={{ color: controlStyles.secondaryColor }}>
              {labelBelow}
            </LabelBelow>
          )}
        </form>
      )
    },
  ),
)

const LabelBelow = styled.label({
  fontSize: 9,
  paddingTop: 2,
  textAlign: 'center',
  display: 'block',
})
