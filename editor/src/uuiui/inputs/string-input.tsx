/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import composeRefs from '@seznam/compose-react-refs'
import React from 'react'
import type { ControlStatus } from '../../components/inspector/common/control-status'
import type { ControlStyles } from '../../components/inspector/common/control-styles'
import { getControlStyles } from '../../components/inspector/common/control-styles'
import { preventDefault, stopPropagation } from '../../components/inspector/common/inspector-utils'
import { useColorTheme } from '../styles/theme'
import { InspectorInputEmotionStyle, getControlStylesAwarePlaceholder } from './base-input'
import { useControlsDisabledInSubtree } from '../utilities/disable-subtree'

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

export const StringInput = React.memo(
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
      const colorTheme = useColorTheme()
      const ref = React.useRef<HTMLInputElement>(null)

      React.useEffect(() => {
        if (focusOnMount && typeof ref !== 'function' && ref.current != null) {
          ref.current.focus()
        }
      }, [focusOnMount, ref])

      const controlStyles: ControlStyles = getControlStyles(controlStatus)
      const controlsDisabled = useControlsDisabledInSubtree()
      const disabled = !controlStyles.interactive || controlsDisabled

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

      const placeholder = getControlStylesAwarePlaceholder(controlStyles) ?? initialPlaceHolder

      return (
        <form
          autoComplete='off'
          style={style}
          onMouseDown={stopPropagation}
          onSubmit={preventDefault}
        >
          <div
            className='string-input-container'
            css={{
              borderRadius: 2,
              color: controlStyles.mainColor,
              position: 'relative',
              background: colorTheme.bg2.value,
              '&:hover': {
                boxShadow: `inset 0px 0px 0px 1px ${colorTheme.fg7.value}`,
              },
              '&:focus-within': {
                boxShadow: `inset 0px 0px 0px 1px ${colorTheme.dynamicBlue.value}`,
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
                    color: colorTheme.subduedForeground.value,
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

export type HeadlessStringInputProps = React.InputHTMLAttributes<HTMLInputElement> & {
  onSubmitValue?: (value: string) => void
  onEscape?: () => void
}

export const HeadlessStringInput = React.forwardRef<HTMLInputElement, HeadlessStringInputProps>(
  (props, propsRef) => {
    const { onSubmitValue, onEscape, ...otherProps } = props
    const { disabled, onKeyDown, onFocus } = otherProps

    const ref = React.useRef<HTMLInputElement>(null)

    const handleOnKeyDown = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (onKeyDown != null) {
          onKeyDown(e)
        }
        if (e.key === 'Escape' || e.key === 'Enter') {
          e.preventDefault()
          e.stopPropagation()
          // eslint-disable-next-line no-unused-expressions
          ref.current?.blur()
          if (e.key === 'Enter') {
            // eslint-disable-next-line no-unused-expressions
            onSubmitValue?.(e.currentTarget.value)
          }
          if (e.key === 'Escape') {
            // eslint-disable-next-line no-unused-expressions
            onEscape?.()
          }
        }
      },
      [onKeyDown, ref, onSubmitValue, onEscape],
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
        {...otherProps}
        disabled={disabled}
        onKeyDown={handleOnKeyDown}
        onFocus={handleOnFocus}
      />
    )
  },
)
