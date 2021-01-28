/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import composeRefs from '@seznam/compose-react-refs'
import * as React from 'react'
import {
  ControlStatus,
  ControlStyles,
  getControlStyles,
} from '../../components/inspector/common/control-status'
import { stopPropagation } from '../../components/inspector/common/inspector-utils'
import { OnSubmitValue } from '../../components/inspector/controls/control'
import { betterReactMemo } from '../../uuiui-deps'
import { UtopiaTheme } from '../styles/theme'
import { InspectorInput } from './base-input'

interface StringInputOptions {
  focusOnMount?: boolean
}

export interface StringInputProps
  extends StringInputOptions,
    React.InputHTMLAttributes<HTMLInputElement> {
  testId: string
  placeholder?: string
  onSubmitValue?: OnSubmitValue<string>
  style?: React.CSSProperties
  id?: string
  className?: string
  DEPRECATED_labelBelow?: React.ReactChild
  controlStatus?: ControlStatus
}

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
      propsRef,
    ) => {
      const ref = React.useRef<HTMLInputElement>(null)

      const [focused, setFocused] = React.useState<boolean>(false)

      React.useEffect(() => {
        if (focusOnMount && typeof ref !== 'function' && ref.current != null) {
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
            e.stopPropagation()
          }
          if (e.key === 'Escape' || e.key === 'Enter') {
            e.preventDefault()
            e.stopPropagation()
            if (typeof ref !== 'function' && ref.current != null) {
              ref.current.blur()
            }
          }
        },
        [inputPropsKeyDown, ref],
      )

      const onFocus = React.useCallback(
        (e: React.FocusEvent<HTMLInputElement>) => {
          if (disabled) {
            e.preventDefault()
            e.target.blur()
          } else {
            if (inputProps.onFocus != null) {
              inputProps.onFocus(e)
            }
            setFocused(true)
            e.target.select()
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
          if (onSubmitValue != null && e.target.value != null) {
            // Coerce the content of the input to a string.
            const inputValue = `${e.target.value}`
            // If the input element currently has some content but the props do not _or_
            // if the value from the props differs from the value currently in the input element.
            if (inputProps.value == null || `${inputProps.value}` !== inputValue) {
              onSubmitValue(inputValue)
            }
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
            testId={inputProps.testId}
            controlStatus={controlStatus}
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
            ref={composeRefs(ref, propsRef)}
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
