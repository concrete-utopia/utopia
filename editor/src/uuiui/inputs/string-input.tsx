/** @jsx jsx */
import { jsx } from '@emotion/core'
import styled from '@emotion/styled'
import * as React from 'react'
import { ControlStyles, getControlStyles } from '../../components/inspector/common/control-status'
import { UtopiaTheme } from '../styles/theme'
import { OnSubmitValue } from '../../components/inspector/controls/control'
import { stopPropagation } from '../../components/inspector/common/inspector-utils'
import { betterReactMemo, ControlStatus } from 'uuiui-deps'
import { InspectorInput } from './base-input'

interface StringInputOptions {
  focusOnMount?: true
}

export interface StringInputProps
  extends StringInputOptions,
    React.InputHTMLAttributes<HTMLInputElement> {
  placeholder?: string
  onSubmitValue?: OnSubmitValue<string>
  style?: React.CSSProperties
  id?: string
  className?: string
  DEPRECATED_labelBelow?: React.ReactChild
  controlStatus?: ControlStatus
}

// TODO FIX FUCKED UP REFS SITUATION
export const StringInput = betterReactMemo(
  'StringInput',
  React.forwardRef<HTMLInputElement, StringInputProps>(
    (
      {
        controlStatus = 'simple',
        style,
        focusOnMount = false,
        placeholder: initialPlaceHolder,
        onSubmitValue,
        DEPRECATED_labelBelow: labelBelow,
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
            e.nativeEvent.stopImmediatePropagation()
            if (ref.current != null) {
              ref.current.blur()
            }
          }
        },
        [inputPropsKeyDown, ref],
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

      return (
        <form autoComplete='off' style={style} onMouseDown={stopPropagation}>
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
