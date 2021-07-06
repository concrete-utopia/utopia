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
import { InspectorInput, InspectorInputEmotionStyle } from './base-input'

interface StringInputOptions {
  focusOnMount?: boolean
}

export interface StringInputProps
  extends StringInputOptions,
    React.InputHTMLAttributes<HTMLInputElement> {
  testId: string
  placeholder?: string
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
        DEPRECATED_labelBelow: labelBelow,
        testId,
        ...inputProps
      },
      propsRef,
    ) => {
      const ref = React.useRef<HTMLInputElement>(null)

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
        },
        [inputPropsKeyDown],
      )

      let placeholder = initialPlaceHolder
      if (controlStyles.unknown) {
        placeholder = 'unknown'
      } else if (controlStyles.mixed) {
        placeholder = 'mixed'
      }

      return (
        <form autoComplete='off' style={style} onMouseDown={stopPropagation}>
          <div
            className='string-input-container'
            css={{
              borderRadius: 2,
              color: controlStyles.mainColor,
              backgroundColor: controlStyles.backgroundColor,
              position: 'relative',
              '&:hover': {
                boxShadow: `inset 0px 0px 0px 1px ${UtopiaTheme.color.primary.value}`,
              },
              '&:focus-within': {
                boxShadow: `inset 0px 0px 0px 1px ${UtopiaTheme.color.primary.value}`,
              },
            }}
          >
            <HeadlessStringInput
              {...inputProps}
              data-testid={testId}
              data-controlstatus={controlStatus}
              value={inputProps.value}
              css={[
                {
                  color: controlStyles.mainColor,
                  '&::placeholder': {
                    fontStyle: 'italic',
                    color: UtopiaTheme.color.subduedForeground.value,
                  },
                },
                InspectorInputEmotionStyle({
                  controlStyles,
                  hasLabel: false,
                }),
              ]}
              onKeyDown={onKeyDown}
              className={inputProps.className}
              ref={composeRefs(ref, propsRef)}
              placeholder={placeholder}
              disabled={disabled}
              autoComplete='off'
              spellCheck={false}
            />
            {labelBelow == null ? null : (
              <LabelBelow htmlFor={inputProps.id} style={{ color: controlStyles.secondaryColor }}>
                {labelBelow}
              </LabelBelow>
            )}
          </div>
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

export type HeadlessStringInputProps = React.InputHTMLAttributes<HTMLInputElement>

export const HeadlessStringInput = React.forwardRef<HTMLInputElement, HeadlessStringInputProps>(
  (props, propsRef) => {
    const { disabled, onKeyDown, onFocus } = props

    const ref = React.useRef<HTMLInputElement>(null)

    const handleOnKeyDown = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (onKeyDown != null) {
          onKeyDown(e)
        }
        if (e.key === 'Escape' || e.key === 'Enter') {
          e.preventDefault()
          e.stopPropagation()
          if (typeof ref !== 'function' && ref.current != null) {
            ref.current.blur()
          }
        }
      },
      [onKeyDown, ref],
    )

    const handleOnFocus = React.useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        // TODO do we actually need to manage disabled?
        if (disabled) {
          e.preventDefault()
          e.target.blur()
        } else {
          if (onFocus != null) {
            onFocus(e)
          }
          e.target.select()
        }
      },
      [disabled, onFocus],
    )

    return (
      <input
        ref={composeRefs(ref, propsRef)}
        {...props}
        onKeyDown={handleOnKeyDown}
        onFocus={handleOnFocus}
      />
    )
  },
)
