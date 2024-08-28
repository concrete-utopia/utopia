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
import { UtopiaTheme, useColorTheme } from '../styles/theme'
import { InspectorInputEmotionStyle, getControlStylesAwarePlaceholder } from './base-input'
import { useControlsDisabledInSubtree } from '../utilities/disable-subtree'
import { dataPasteHandler } from '../../utils/paste-handler'

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
  growInputAutomatically?: boolean
  includeBoxShadow?: boolean
  onSubmitValue?: (value: string) => void
  onEscape?: () => void
  pasteHandler?: boolean
  showBorder?: boolean
  innerStyle?: React.CSSProperties
}

export const StringInput = React.memo(
  React.forwardRef<HTMLInputElement, StringInputProps>(
    (
      {
        controlStatus = 'simple',
        style,
        innerStyle,
        focusOnMount = false,
        includeBoxShadow = true,
        placeholder: initialPlaceHolder,
        DEPRECATED_labelBelow: labelBelow,
        testId,
        showBorder,
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
            style={innerStyle}
            css={{
              borderRadius: UtopiaTheme.inputBorderRadius,
              color: controlStyles.mainColor,
              position: 'relative',
              background: 'transparent',
              boxShadow: showBorder ? `inset 0px 0px 0px 1px ${colorTheme.fg7.value}` : undefined,
              '&:hover': {
                boxShadow: includeBoxShadow
                  ? `inset 0px 0px 0px 1px ${colorTheme.fg7.value}`
                  : undefined,
              },
              '&:focus-within': {
                boxShadow: includeBoxShadow
                  ? `inset 0px 0px 0px 1px ${colorTheme.dynamicBlue.value}`
                  : undefined,
              },
            }}
          >
            <HeadlessStringInput
              {...inputProps}
              testId={testId}
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
                  controlStyles: controlStyles,
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
              growInputAutomatically={inputProps.growInputAutomatically}
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
  growInputAutomatically?: boolean
  testId: string
  pasteHandler?: boolean
}

export const HeadlessStringInput = React.forwardRef<HTMLInputElement, HeadlessStringInputProps>(
  (props, propsRef) => {
    const {
      onSubmitValue,
      onEscape,
      onChange,
      growInputAutomatically = false,
      style = {},
      value,
      testId,
      pasteHandler,
      ...otherProps
    } = props
    const { disabled, onKeyDown, onFocus } = otherProps

    const ref = React.useRef<HTMLInputElement>(null)

    const spanRef = React.useRef<HTMLSpanElement>(null)

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

    const inputComponent = (
      <input
        ref={composeRefs(ref, propsRef)}
        data-testid={testId}
        {...dataPasteHandler(props.pasteHandler)}
        {...otherProps}
        disabled={disabled}
        onKeyDown={handleOnKeyDown}
        onFocus={handleOnFocus}
        onChange={onChange}
        style={{
          ...style,
          gridRowStart: growInputAutomatically ? 1 : style.gridRowStart,
          gridColumnStart: growInputAutomatically ? 1 : style.gridColumnStart,
        }}
        value={value ?? ''}
      />
    )

    if (growInputAutomatically) {
      return (
        <div
          style={{
            display: 'inline-grid',
            background: 'transparent',
          }}
        >
          {inputComponent}
          <span
            ref={spanRef}
            data-testid={`${testId}-span`}
            {...otherProps}
            style={{
              ...style,
              visibility: 'hidden',
              whiteSpace: 'nowrap',
              gridRowStart: 1,
              gridColumnStart: 1,
            }}
          >
            {value ?? ''}
          </span>
        </div>
      )
    } else {
      return inputComponent
    }
  },
)
