/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import type { Interpolation } from '@emotion/react'
import { jsx } from '@emotion/react'
import type { MouseEventHandler } from 'react'
import React from 'react'
import type {
  CSSNumber,
  CSSNumberType,
  CSSNumberUnit,
  UnknownOrEmptyInput,
} from '../../components/inspector/common/css-utils'
import {
  cssNumber,
  cssNumberToString,
  emptyInputValue,
  getCSSNumberUnit,
  isCSSNumber,
  isEmptyInputValue,
  isUnknownInputValue,
  parseCSSNumber,
  setCSSNumberValue,
  unknownInputValue,
} from '../../components/inspector/common/css-utils'
import type {
  OnUnsetValues,
  SubmitValueFactoryReturn,
} from '../../components/inspector/common/property-path-hooks'
import type {
  InspectorControlProps,
  OnSubmitValue,
  OnSubmitValueOrEmpty,
  OnSubmitValueOrUnknownOrEmpty,
} from '../../components/inspector/controls/control'
import type { Either } from '../../core/shared/either'
import { isLeft, mapEither } from '../../core/shared/either'
import { clampValue } from '../../core/shared/math-utils'
import { memoize } from '../../core/shared/memoize'
import type { ControlStyles } from '../../uuiui-deps'
import { getControlStyles, CSSCursor } from '../../uuiui-deps'
import type { IcnProps } from '../icn'
import { Icn } from '../icn'
import { useColorTheme, UtopiaTheme } from '../styles/theme'
import { FlexRow } from '../widgets/layout/flex-row'
import type { BaseInputProps, BoxCorners, ChainedType } from './base-input'
import {
  getBorderRadiusStyles,
  getControlStylesAwarePlaceholder,
  InspectorInput,
} from './base-input'
import { usePropControlledStateV2 } from '../../components/inspector/common/inspector-utils'
import { useIsMyProject } from '../../components/editor/store/collaborative-editing'
import { useControlsDisabledInSubtree } from '../utilities/disable-subtree'

export type LabelDragDirection = 'horizontal' | 'vertical'

function getDisplayValueNotMemoized(
  value: CSSNumber | null,
  defaultUnitToHide: CSSNumberUnit | null,
  mixed: boolean,
  showContent: boolean,
): string {
  if (!mixed && value != null && showContent) {
    const unit = getCSSNumberUnit(value)
    const showUnit = unit !== defaultUnitToHide
    return cssNumberToString(value, showUnit)
  } else {
    return ''
  }
}

const getDisplayValue = memoize(getDisplayValueNotMemoized, { maxSize: 1000 })

function parseDisplayValueNotMemoized(
  input: string,
  numberType: CSSNumberType,
  defaultUnit: CSSNumberUnit | null,
): Either<string, CSSNumber> {
  const parsedInput = parseCSSNumber(input, numberType)
  return mapEither((value: CSSNumber) => {
    if (value.unit == null) {
      return cssNumber(value.value, defaultUnit)
    } else {
      return value
    }
  }, parsedInput)
}

const parseDisplayValue = memoize(parseDisplayValueNotMemoized, { maxSize: 1000 })

const dragDeltaSign = (start: number, end: number, directionAdjustment: 1 | -1): 1 | -1 => {
  const raw = (start - end) * directionAdjustment
  return raw >= 0 ? 1 : -1
}

const calculateDragDirectionDelta = (
  start: number,
  end: number,
  scalingFactor: number,
  directionAdjustment: 1 | -1,
): number => {
  const sign = dragDeltaSign(start, end, directionAdjustment)
  const rawAbsDelta = Math.abs(start - end)
  const scaledAbsDelta = Math.floor(rawAbsDelta / scalingFactor)
  return sign * scaledAbsDelta
}

const calculateDragDelta = (
  dragOriginX: number,
  dragOriginY: number,
  dragScreenX: number,
  dragScreenY: number,
  labelDragDirection: LabelDragDirection,
  scalingFactor: number = 2,
) => {
  if (labelDragDirection === 'horizontal') {
    return calculateDragDirectionDelta(dragOriginX, dragScreenX, scalingFactor, 1)
  } else {
    return calculateDragDirectionDelta(dragOriginY, dragScreenY, scalingFactor, -1)
  }
}

let incrementTimeout: number | undefined = undefined
let incrementAnimationFrame: number | undefined = undefined
const repeatThreshold: number = 500

export interface NumberInputOptions {
  labelInner?: string | IcnProps
  minimum?: number
  maximum?: number
  stepSize?: number
  incrementControls?: boolean
  chained?: ChainedType
  height?: number
  roundCorners?: BoxCorners
  numberType: CSSNumberType
  defaultUnitToHide: CSSNumberUnit | null
  pasteHandler?: boolean
}

export interface AbstractNumberInputProps<T extends CSSNumber | number>
  extends NumberInputOptions,
    BaseInputProps,
    InspectorControlProps {
  value: T | null | undefined
  DEPRECATED_labelBelow?: React.ReactChild
  labelBelowStyle?: React.CSSProperties
  invalid?: boolean
}

export interface NumberInputProps extends AbstractNumberInputProps<CSSNumber> {
  onSubmitValue?: OnSubmitValueOrUnknownOrEmpty<CSSNumber>
  onTransientSubmitValue?: OnSubmitValueOrUnknownOrEmpty<CSSNumber>
  onForcedSubmitValue?: OnSubmitValueOrUnknownOrEmpty<CSSNumber>
  setGlobalCursor?: (cursor: CSSCursor | null) => void
  onMouseEnter?: MouseEventHandler
  onMouseLeave?: MouseEventHandler
}

const ScrubThreshold = 3

export const NumberInput = React.memo<NumberInputProps>(
  ({
    value: propsValue,
    style,
    testId,
    inputProps = {},
    id,
    className,
    DEPRECATED_labelBelow,
    labelBelowStyle = {},
    labelInner,
    minimum: unscaledMinimum = -Infinity,
    maximum: unscaledMaximum = Infinity,
    stepSize: unscaledStepSize,
    incrementControls = true,
    chained = 'not-chained',
    height = UtopiaTheme.layout.inputHeight.default,
    roundCorners = 'all',
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    controlStatus = 'simple',
    focusOnMount = false,
    numberType,
    defaultUnitToHide,
    setGlobalCursor,
    onMouseEnter,
    onMouseLeave,
    invalid,
    pasteHandler,
  }) => {
    const ref = React.useRef<HTMLInputElement>(null)
    const colorTheme = useColorTheme()

    const controlStyles = React.useMemo((): ControlStyles => {
      return {
        ...getControlStyles(controlStatus),
        invalid: invalid ?? false,
      }
    }, [controlStatus, invalid])

    const controlsDisabledInSubtree = useControlsDisabledInSubtree()

    const disabled = !controlStyles.interactive || controlsDisabledInSubtree

    const { mixed, showContent } = React.useMemo(
      () => ({
        mixed: controlStyles.mixed,
        showContent: controlStyles.showContent && !controlStyles.invalid,
      }),
      [controlStyles],
    )

    const [value, setValue] = usePropControlledStateV2(propsValue ?? null)
    const [displayValue, setDisplayValue] = usePropControlledStateV2(
      getDisplayValue(value, defaultUnitToHide, mixed, showContent),
    )
    React.useEffect(() => {
      if (mixed) {
        setDisplayValue('')
      }
    }, [mixed, setDisplayValue])

    const valueUnit = React.useMemo(() => value?.unit ?? null, [value])

    const [isActuallyFocused, setIsActuallyFocused] = React.useState<boolean>(false)
    const [isFauxcused, setIsFauxcused] = React.useState<boolean>(false)
    const isFocused = isActuallyFocused || isFauxcused

    const [labelDragDirection, setLabelDragDirection] =
      React.useState<LabelDragDirection>('horizontal')

    const [, setValueAtDragOriginState] = React.useState<number>(0)
    const valueAtDragOrigin = React.useRef(0)
    const setValueAtDragOrigin = (n: number) => {
      valueAtDragOrigin.current = n
      setValueAtDragOriginState(n)
    }

    const [, setDragOriginXState] = React.useState<number>(-Infinity)
    const dragOriginX = React.useRef(-Infinity)
    const setDragOriginX = (n: number) => {
      dragOriginX.current = n
      setDragOriginXState(n)
    }

    const [, setDragOriginYState] = React.useState<number>(-Infinity)
    const dragOriginY = React.useRef(-Infinity)
    const setDragOriginY = (n: number) => {
      dragOriginY.current = n
      setDragOriginYState(n)
    }

    const [, setScrubThresholdPassedState] = React.useState<boolean>(false)
    const scrubThresholdPassed = React.useRef(false)
    const setScrubThresholdPassed = (b: boolean) => {
      scrubThresholdPassed.current = b
      setScrubThresholdPassedState(b)
    }

    const [valueChangedSinceFocus, setValueChangedSinceFocus] = React.useState<boolean>(false)

    const scaleFactor = valueUnit === '%' ? 100 : 1
    const minimum = scaleFactor * unscaledMinimum
    const maximum = scaleFactor * unscaledMaximum
    const stepSize = unscaledStepSize == null ? 1 : unscaledStepSize * scaleFactor

    const repeatedValueRef = React.useRef(value ?? null)

    const updateValue = React.useCallback(
      (newValue: CSSNumber | null) => {
        setValue(newValue)
        setDisplayValue(getDisplayValue(newValue, defaultUnitToHide, mixed, showContent))
      },
      [setValue, setDisplayValue, defaultUnitToHide, mixed, showContent],
    )

    const incrementBy = React.useCallback(
      (incrementStepSize: number, shiftKey: boolean, transient: boolean) => {
        if (value == null) {
          return cssNumber(0)
        }
        const delta = incrementStepSize * (shiftKey ? 10 : 1)
        const newNumericValue = clampValue(value.value + delta, minimum, maximum)
        const newValue = setCSSNumberValue(value, newNumericValue)
        if (transient) {
          if (onTransientSubmitValue != null) {
            onTransientSubmitValue(newValue)
          } else if (onSubmitValue != null) {
            onSubmitValue(newValue)
          }
        } else {
          if (onForcedSubmitValue != null) {
            onForcedSubmitValue(newValue)
          } else if (onSubmitValue != null) {
            onSubmitValue(newValue)
          }
        }
        repeatedValueRef.current = newValue
        setValueChangedSinceFocus(true)
        updateValue(newValue)
        return newValue
      },
      [
        maximum,
        minimum,
        onForcedSubmitValue,
        onSubmitValue,
        onTransientSubmitValue,
        value,
        updateValue,
      ],
    )

    const repeatIncrement = React.useCallback(
      (
        currentValue: CSSNumber,
        incrementStepSize: number,
        shiftKey: boolean,
        transient: boolean,
      ) => {
        const newValue = incrementBy(incrementStepSize, shiftKey, transient)
        incrementAnimationFrame = window.requestAnimationFrame(() =>
          repeatIncrement(newValue, incrementStepSize, shiftKey, transient),
        )
      },
      [incrementBy],
    )

    const setScrubValue = React.useCallback(
      (
        unit: CSSNumberUnit | null,
        screenX: number,
        screenY: number,
        scrubDragOriginX: number,
        scrubDragOriginY: number,
        transient: boolean,
      ) => {
        const primaryAxisDelta = calculateDragDelta(
          scrubDragOriginX,
          scrubDragOriginY,
          screenX,
          screenY,
          labelDragDirection,
        )
        const numericValue = clampValue(
          valueAtDragOrigin.current - stepSize * primaryAxisDelta,
          minimum,
          maximum,
        )
        const newValue = cssNumber(numericValue, unit)

        if (transient) {
          if (onTransientSubmitValue != null) {
            onTransientSubmitValue(newValue)
          } else if (onSubmitValue != null) {
            onSubmitValue(newValue)
          }
        } else {
          if (onForcedSubmitValue != null) {
            onForcedSubmitValue(newValue)
          } else if (onSubmitValue != null) {
            onSubmitValue(newValue)
          }
        }
        updateValue(newValue)
        return newValue
      },
      [
        labelDragDirection,
        maximum,
        minimum,
        stepSize,
        onForcedSubmitValue,
        onSubmitValue,
        onTransientSubmitValue,
        updateValue,
      ],
    )

    React.useEffect(() => {
      if (focusOnMount && ref.current != null) {
        ref.current.focus()
      }
    }, [focusOnMount, ref])

    const onThresholdPassed = (e: MouseEvent, fn: () => void) => {
      const thresholdPassed =
        scrubThresholdPassed.current || Math.abs(e.screenX - dragOriginX.current) >= ScrubThreshold
      if (thresholdPassed) {
        fn()
      }
    }

    const scrubOnMouseMove = React.useCallback(
      (e: MouseEvent) => {
        onThresholdPassed(e, () => {
          if (!scrubThresholdPassed.current) {
            setScrubThresholdPassed(true)
          }
          setScrubValue(
            valueUnit,
            e.screenX,
            e.screenY,
            dragOriginX.current,
            dragOriginY.current,
            true,
          )
        })
      },
      [setScrubValue, valueUnit],
    )

    const scrubOnMouseUp = React.useCallback(
      (e: MouseEvent) => {
        window.removeEventListener('mouseup', scrubOnMouseUp)
        window.removeEventListener('mousemove', scrubOnMouseMove)

        setIsFauxcused(false)
        ref.current?.focus()

        onThresholdPassed(e, () => {
          setScrubValue(
            valueUnit,
            e.screenX,
            e.screenY,
            dragOriginX.current,
            dragOriginY.current,
            false,
          )
        })
        setScrubThresholdPassed(false)
        setGlobalCursor?.(null)
      },
      [scrubOnMouseMove, setScrubValue, valueUnit, ref, setGlobalCursor],
    )

    const rc = roundCorners == null ? 'all' : roundCorners
    const borderRadiusStyles = getBorderRadiusStyles(chained, rc)

    const onFocus = React.useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        setIsActuallyFocused(true)
        e.target.select()
        if (inputProps.onFocus != null) {
          inputProps.onFocus(e)
        }
      },
      [inputProps],
    )

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'ArrowUp') {
          updateValue(incrementBy(stepSize, e.shiftKey, false))
        } else if (e.key === 'ArrowDown') {
          e.preventDefault()
          updateValue(incrementBy(-stepSize, e.shiftKey, false))
        } else if (e.key === 'Enter' || e.key === 'Escape') {
          e.nativeEvent.stopImmediatePropagation()
          e.preventDefault()
          ref.current?.blur()
        }
      },
      [incrementBy, stepSize, ref, updateValue],
    )

    const onKeyUp = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        // todo make sure this isn't doubling up the value submit
        if ((e.key === 'ArrowUp' || e.key === 'ArrowDown') && onForcedSubmitValue != null) {
          if (value != null) {
            onForcedSubmitValue(value)
          }
        }
      },
      [onForcedSubmitValue, value],
    )

    const onBlur = React.useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        setIsActuallyFocused(false)

        function getNewValue() {
          if (displayValue == '') {
            return emptyInputValue()
          }
          const parsed = parseDisplayValue(displayValue, numberType, defaultUnitToHide)
          if (isLeft(parsed)) {
            return unknownInputValue(displayValue)
          }
          return parsed.value
        }

        const newValue = getNewValue()
        if (isUnknownInputValue(newValue)) {
          updateValue(value)
          return
        }
        updateValue(isEmptyInputValue(newValue) ? cssNumber(0) : newValue)

        if (inputProps.onBlur != null) {
          inputProps.onBlur(e)
        }
        if (valueChangedSinceFocus) {
          setValueChangedSinceFocus(false)
          if (onSubmitValue != null) {
            onSubmitValue(newValue)
          }
        }
      },
      [
        inputProps,
        onSubmitValue,
        valueChangedSinceFocus,
        defaultUnitToHide,
        numberType,
        updateValue,
        value,
        displayValue,
      ],
    )

    const onChange = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => {
        if (inputProps.onChange != null) {
          inputProps.onChange(e)
        }
        setValueChangedSinceFocus(true)
        setDisplayValue(e.target.value)
      },
      [inputProps, setDisplayValue],
    )

    const onIncrementMouseUp = React.useCallback(() => {
      window.removeEventListener('mouseup', onIncrementMouseUp)
      setIsFauxcused(false)
      ref.current?.focus()
      if (incrementTimeout != null) {
        window.clearTimeout(incrementTimeout)
        incrementTimeout = undefined
      }
      if (incrementAnimationFrame != null) {
        window.cancelAnimationFrame(incrementAnimationFrame ?? 0)
        incrementAnimationFrame = undefined
      }

      // Clear transient state by setting the final value
      if (onSubmitValue != null) {
        onSubmitValue(
          repeatedValueRef.current != null
            ? repeatedValueRef.current
            : unknownInputValue(displayValue),
        )
      }

      if (repeatedValueRef.current != null) {
        updateValue(repeatedValueRef.current)
      }
    }, [ref, onSubmitValue, updateValue, displayValue])

    const onIncrementMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        if (disabled) {
          return
        }
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          window.addEventListener('mouseup', onIncrementMouseUp)
          const shiftKey = e.shiftKey
          const newValue = incrementBy(stepSize, shiftKey, false)
          incrementTimeout = window.setTimeout(() => {
            repeatIncrement(newValue, stepSize, shiftKey, true)
          }, repeatThreshold)
        }
      },
      [incrementBy, stepSize, repeatIncrement, onIncrementMouseUp, disabled],
    )

    const onDecrementMouseUp = React.useCallback(() => {
      window.removeEventListener('mouseup', onDecrementMouseUp)
      setIsFauxcused(false)
      ref.current?.focus()
      if (incrementTimeout != null) {
        window.clearTimeout(incrementTimeout)
        incrementTimeout = undefined
      }
      if (incrementAnimationFrame != null) {
        window.cancelAnimationFrame(incrementAnimationFrame ?? 0)
        incrementAnimationFrame = undefined
      }

      // Clear transient state by setting the final value
      if (onSubmitValue != null) {
        onSubmitValue(
          repeatedValueRef.current != null
            ? repeatedValueRef.current
            : unknownInputValue(displayValue),
        )
      }

      if (repeatedValueRef.current != null) {
        updateValue(repeatedValueRef.current)
      }
    }, [ref, onSubmitValue, displayValue, updateValue])

    const onDecrementMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        if (disabled) {
          return
        }
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          window.addEventListener('mouseup', onDecrementMouseUp)
          const shiftKey = e.shiftKey
          const newValue = incrementBy(-stepSize, shiftKey, false)
          incrementTimeout = window.setTimeout(
            () => repeatIncrement(newValue, -stepSize, shiftKey, true),
            repeatThreshold,
          )
        }
      },
      [incrementBy, stepSize, repeatIncrement, onDecrementMouseUp, disabled],
    )

    const onLabelMouseDown = React.useCallback(
      (e: React.MouseEvent<HTMLDivElement>) => {
        if (disabled) {
          return
        }
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          window.addEventListener('mousemove', scrubOnMouseMove)
          window.addEventListener('mouseup', scrubOnMouseUp)
          setLabelDragDirection('horizontal')
          setValueAtDragOrigin(value?.value ?? 0)
          setDragOriginX(e.screenX)
          setDragOriginY(e.screenY)
          setGlobalCursor?.(CSSCursor.ResizeEW)
        }
      },
      [scrubOnMouseMove, scrubOnMouseUp, setGlobalCursor, value, disabled],
    )

    const placeholder = getControlStylesAwarePlaceholder(controlStyles)

    const chainedStyles: Interpolation<any> | undefined =
      (chained === 'first' || chained === 'middle') && !isFocused
        ? {
            '&:not(:hover)::after': {
              content: '""',
              width: 1,
              height: UtopiaTheme.layout.inputHeight.default / 2,
              backgroundColor: controlStyles.borderColor,
              zIndex: 2,
              position: 'absolute',
              top: UtopiaTheme.layout.inputHeight.default / 2,
              right: 0,
              transform: 'translateX(0.5px)',
            },
          }
        : undefined

    return (
      <div onMouseEnter={onMouseEnter} onMouseLeave={onMouseLeave} style={style}>
        <div
          className='number-input-container'
          css={{
            color: controlStyles.mainColor,
            zIndex: isFocused ? 3 : undefined,
            position: 'relative',
            borderRadius: 2,
            display: 'flex',
            flexDirection: 'row',
            alignItems: 'center',
            boxShadow: 'inset 0px 0px 0px 1px transparent',
            ...chainedStyles,
            '&:hover': {
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.fg7.value}`,
            },
            '&:focus-within': {
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.dynamicBlue.value}`,
            },
            '&:hover input': {
              color: controlStyles.mainColor,
            },
            '&:focus-within input': {
              color: controlStyles.mainColor,
            },
          }}
        >
          {labelInner != null ? (
            <div
              className='number-input-innerLabel'
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                userSelect: 'none',
                width: 20,
                height: 20,
                display: 'block',
              }}
            >
              <div
                style={{
                  position: 'absolute',
                  left: 0,
                  top: 2,
                  textAlign: 'center',
                  fontWeight: 600,
                  fontSize: '9px',
                  width: '100%',
                  height: '100%',
                  color: colorTheme.fg7.value,
                }}
                onMouseDown={onLabelMouseDown}
              >
                {typeof labelInner === 'object' && 'type' in labelInner ? (
                  <Icn {...labelInner} />
                ) : (
                  labelInner
                )}
              </div>
            </div>
          ) : null}
          {incrementControls && !disabled ? (
            <div
              className='number-input-increment-controls'
              css={{
                position: 'absolute',
                top: 0,
                right: 1,
                flexDirection: 'column',
                alignItems: 'stretch',
                width: 11,
                height: UtopiaTheme.layout.inputHeight.default,
                boxShadow: `1px 0 ${controlStyles.borderColor} inset`,
                display: 'none',
                '.number-input-container:hover &': {
                  display: 'block',
                },
                '.number-input-container:focus-within &': {
                  display: 'block',
                },
              }}
            >
              <div
                css={{
                  height: '50%',
                  opacity: 0.6,
                  position: 'relative',
                  borderTopRightRadius: borderRadiusStyles.borderTopRightRadius,
                  ':active': {
                    opacity: 1,
                  },
                  '::after': {
                    content: '""',
                    width: 'calc(100% - 1px)',
                    height: 1,
                    position: 'absolute',
                    right: 1,
                    bottom: 0,
                    transform: 'translateY(0.5px)',
                    pointerEvents: 'none',
                  },
                }}
                onMouseDown={onIncrementMouseDown}
              >
                <Icn category='controls/input' type='up' color='secondary' width={11} height={11} />
              </div>
              <div
                css={{
                  height: '50%',
                  opacity: 0.6,
                  position: 'relative',
                  borderBottomRightRadius: borderRadiusStyles.borderBottomRightRadius,
                  ':active': {
                    opacity: 1,
                  },
                  '::after': {
                    content: '""',
                    width: 'calc(100% - 1px)',
                    height: 1,
                    position: 'absolute',
                    right: 1,
                    bottom: 0,
                    transform: 'translateY(0.5px)',
                    pointerEvents: 'none',
                  },
                }}
                onMouseDown={onDecrementMouseDown}
              >
                <Icn
                  category='controls/input'
                  type='down'
                  color='secondary'
                  width={11}
                  height={11}
                />
              </div>
            </div>
          ) : null}
          {DEPRECATED_labelBelow == null && controlStatus != 'off' ? null : (
            <React.Fragment>
              {isFauxcused ? (
                <div
                  style={{
                    background: 'transparent',
                    zIndex: 1,
                  }}
                ></div>
              ) : null}
              <div
                data-testid={`${testId}-mouse-down-handler`}
                onMouseDown={onLabelMouseDown}
                style={{
                  paddingLeft: 4,
                  cursor: CSSCursor.ResizeEW,
                  fontSize: 9,
                  textAlign: 'center',
                  display: 'block',
                  color: controlStyles.secondaryColor,
                  ...labelBelowStyle,
                }}
              >
                {DEPRECATED_labelBelow}
              </div>
            </React.Fragment>
          )}
          <InspectorInput
            {...inputProps}
            chained={chained}
            controlStyles={controlStyles}
            controlStatus={controlStatus}
            testId={testId}
            pasteHandler={pasteHandler}
            disabled={disabled}
            focused={isFocused}
            hasLabel={labelInner != null}
            roundCorners={roundCorners}
            mixed={mixed}
            value={displayValue}
            ref={ref}
            style={{
              color: colorTheme.fg1.value,
            }}
            css={{
              '::placeholder': { color: invalid ? colorTheme.error.value : undefined },
            }}
            className='number-input'
            height={height}
            id={id}
            placeholder={placeholder}
            onFocus={onFocus}
            onKeyDown={onKeyDown}
            onKeyUp={onKeyUp}
            onBlur={onBlur}
            onChange={onChange}
            autoComplete='off'
          />
        </div>
      </div>
    )
  },
)
NumberInput.displayName = 'NumberInput'

interface SimpleNumberInputProps extends Omit<AbstractNumberInputProps<number>, 'numberType'> {
  onSubmitValue: OnSubmitValueOrEmpty<number>
  onTransientSubmitValue: OnSubmitValueOrEmpty<number>
  onForcedSubmitValue: OnSubmitValueOrEmpty<number>
  setGlobalCursor?: (cursor: CSSCursor | null) => void
}

interface SimpleCSSNumberInputProps extends AbstractNumberInputProps<CSSNumber> {
  onSubmitValue: OnSubmitValueOrEmpty<number>
  onTransientSubmitValue: OnSubmitValueOrEmpty<number>
  onForcedSubmitValue: OnSubmitValueOrEmpty<number>
  setGlobalCursor?: (cursor: CSSCursor | null) => void
}

function wrappedSimpleOnSubmitValue(
  onSubmitValue: OnSubmitValueOrEmpty<number>,
): OnSubmitValueOrUnknownOrEmpty<CSSNumber> {
  return (value) => {
    if (isCSSNumber(value)) {
      onSubmitValue(value.value)
    }
  }
}

export const SimpleNumberInput = React.memo(
  ({
    value,
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    ...sharedProps
  }: SimpleNumberInputProps) => {
    const wrappedProps: NumberInputProps = {
      ...sharedProps,
      value: value == null ? null : cssNumber(value),
      onSubmitValue: wrappedSimpleOnSubmitValue(onSubmitValue),
      onTransientSubmitValue: wrappedSimpleOnSubmitValue(onTransientSubmitValue),
      onForcedSubmitValue: wrappedSimpleOnSubmitValue(onForcedSubmitValue),
      numberType: 'Unitless',
    }
    return <NumberInput {...wrappedProps} />
  },
)

export const SimpleCSSNumberInput = React.memo(
  ({
    value,
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    numberType,
    ...sharedProps
  }: SimpleCSSNumberInputProps) => {
    const wrappedProps: NumberInputProps = {
      ...sharedProps,
      value: value,
      onSubmitValue: wrappedSimpleOnSubmitValue(onSubmitValue),
      onTransientSubmitValue: wrappedSimpleOnSubmitValue(onTransientSubmitValue),
      onForcedSubmitValue: wrappedSimpleOnSubmitValue(onForcedSubmitValue),
      numberType: 'AnyValid',
    }
    return <NumberInput {...wrappedProps} />
  },
)

interface SimplePercentInputProps extends Omit<AbstractNumberInputProps<number>, 'numberType'> {
  onSubmitValue: OnSubmitValueOrEmpty<number>
  onTransientSubmitValue: OnSubmitValueOrEmpty<number>
  onForcedSubmitValue: OnSubmitValueOrEmpty<number>
}

function wrappedPercentOnSubmitValue(
  onSubmitValue: OnSubmitValueOrEmpty<number>,
): OnSubmitValueOrUnknownOrEmpty<CSSNumber> {
  return (value) => {
    if (isCSSNumber(value)) {
      onSubmitValue(value.value / 100)
    }
  }
}

export const SimplePercentInput = React.memo(
  ({
    value,
    onSubmitValue,
    onTransientSubmitValue,
    onForcedSubmitValue,
    ...sharedProps
  }: SimplePercentInputProps) => {
    const wrappedProps: NumberInputProps = {
      ...sharedProps,
      value: value == null ? null : cssNumber(value * 100, '%'),
      onSubmitValue: wrappedPercentOnSubmitValue(onSubmitValue),
      onTransientSubmitValue: wrappedPercentOnSubmitValue(onTransientSubmitValue),
      onForcedSubmitValue: wrappedPercentOnSubmitValue(onForcedSubmitValue),
      numberType: 'Percent',
    }
    return <NumberInput {...wrappedProps} />
  },
)

interface ChainedNumberControlProps {
  propsArray: Array<Omit<NumberInputProps, 'chained' | 'id'>>
  idPrefix: string
  style?: React.CSSProperties
  setGlobalCursor?: (cursor: CSSCursor | null) => void
}

export const ChainedNumberInput: React.FunctionComponent<
  React.PropsWithChildren<ChainedNumberControlProps>
> = React.memo(({ propsArray, idPrefix, style, setGlobalCursor }) => {
  return (
    <FlexRow style={style}>
      {propsArray.map((props, i) => {
        switch (i) {
          case 0: {
            return (
              <NumberInput
                key={`${idPrefix}-${i}`}
                id={`${idPrefix}-${i}`}
                {...props}
                chained='first'
                roundCorners='left'
                setGlobalCursor={setGlobalCursor}
                incrementControls={false}
              />
            )
          }
          case propsArray.length - 1: {
            return (
              <NumberInput
                key={`${idPrefix}-${i}`}
                id={`${idPrefix}-${i}`}
                {...props}
                chained='last'
                roundCorners='right'
                setGlobalCursor={setGlobalCursor}
                incrementControls={false}
              />
            )
          }
          default: {
            return (
              <NumberInput
                key={`${idPrefix}-${i}`}
                id={`${idPrefix}-${i}`}
                {...props}
                chained='middle'
                roundCorners='none'
                setGlobalCursor={setGlobalCursor}
                incrementControls={false}
              />
            )
          }
        }
      })}
    </FlexRow>
  )
})

export function wrappedEmptyOrUnknownOnSubmitValue<T>(
  onSubmitValue: OnSubmitValue<T>,
  onUnsetValue?: OnUnsetValues,
): OnSubmitValueOrUnknownOrEmpty<T> {
  return (value) => {
    if (isEmptyInputValue(value)) {
      if (onUnsetValue != null) {
        onUnsetValue()
      }
    } else if (!isUnknownInputValue(value)) {
      onSubmitValue(value)
    }
  }
}

export function useWrappedEmptyOrUnknownOnSubmitValue<T>(
  onSubmitValue: OnSubmitValue<T>,
  onUnsetValue?: OnUnsetValues,
): OnSubmitValueOrUnknownOrEmpty<T> {
  return React.useCallback(
    (value) => wrappedEmptyOrUnknownOnSubmitValue(onSubmitValue, onUnsetValue)(value),
    [onSubmitValue, onUnsetValue],
  )
}

/**
 * An easy wrapper for the submitValueFactory that applies useWrappedEmptyOrUnknownOnSubmitValue
 * to each submitValue type.
 */
export function useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue<T>(
  submitValueReturn: SubmitValueFactoryReturn<T>,
  onUnsetValue?: OnUnsetValues,
): SubmitValueFactoryReturn<UnknownOrEmptyInput<T>> {
  const index0 = useWrappedEmptyOrUnknownOnSubmitValue(submitValueReturn[0], onUnsetValue)
  const index1 = useWrappedEmptyOrUnknownOnSubmitValue(submitValueReturn[1], onUnsetValue)
  return [index0, index1]
}
