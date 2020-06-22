/** @jsx jsx */
import { jsx, ObjectInterpolation } from '@emotion/core'
import * as classNames from 'classnames'
import * as React from 'react'
import {
  betterReactMemo,
  CSSCursor,
  EitherUtils,
  getControlStyles,
  OnSubmitValue,
  OnSubmitValueOrEmpty,
  OnSubmitValueOrUnknownOrEmpty,
  usePropControlledState,
} from 'uuiui-deps'
import {
  cssNumber,
  CSSNumber,
  cssNumberToString,
  CSSNumberType,
  CSSNumberUnit,
  emptyInputValue,
  EmptyInputValue,
  getCSSNumberUnit,
  isEmptyInputValue,
  parseCSSNumber,
  setCSSNumberValue,
  UnknownOrEmptyInput,
} from '../../components/inspector/common/css-utils'
import { OnUnsetValues } from '../../components/inspector/common/property-path-hooks'
import { clampValue } from '../../core/shared/math-utils'
import { Icn, IcnProps } from '../icn'
import { colorTheme, UtopiaTheme } from '../styles/theme'
import { FlexRow } from '../widgets/layout/flex-row'
import {
  BoxCorners,
  ChainedType,
  getBorderRadiusStyles,
  InspectorInput,
  BaseInputProps,
} from './base-input'
import { InspectorControlProps } from '../../components/inspector/controls/control'

export type LabelDragDirection = 'horizontal' | 'vertical'

const getDisplayValue = (
  value: CSSNumber | null,
  defaultUnitToHide: CSSNumberUnit | null,
  mixed: boolean,
): string => {
  if (!mixed && value != null) {
    const unit = getCSSNumberUnit(value)
    const showUnit = unit !== defaultUnitToHide
    return cssNumberToString(value, showUnit)
  } else {
    return ''
  }
}

const parseDisplayValue = (
  input: string,
  numberType: CSSNumberType,
  defaultUnit: CSSNumberUnit | null,
): EitherUtils.Either<string, CSSNumber> => {
  const parsedInput = parseCSSNumber(input, numberType)
  return EitherUtils.mapEither((value: CSSNumber) => {
    if (value.unit == null) {
      return cssNumber(value.value, defaultUnit)
    } else {
      return value
    }
  }, parsedInput)
}

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
  defaultUnitToHide?: CSSNumberUnit
}

export interface AbstractNumberInputProps<T extends CSSNumber | number>
  extends NumberInputOptions,
    BaseInputProps,
    InspectorControlProps {
  value: T | null | undefined
  onSubmitValue?: OnSubmitValueOrEmpty<T>
  onTransientSubmitValue?: OnSubmitValueOrEmpty<T>
  onForcedSubmitValue?: OnSubmitValueOrEmpty<T>
  DEPRECATED_labelBelow?: React.ReactChild
}

export interface NumberInputProps extends AbstractNumberInputProps<CSSNumber> {}

const ScrubThreshold = 3

export const NumberInput = betterReactMemo<NumberInputProps>(
  'NumberInput',
  ({
    value: propsValue,
    style,
    inputProps = {},
    id,
    className,
    DEPRECATED_labelBelow,
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
    defaultUnitToHide = null,
  }) => {
    const ref = React.useRef<HTMLInputElement>(null)
    const controlStyles = getControlStyles(controlStatus)
    const backgroundImage = React.useMemo(
      () => `linear-gradient(to right, transparent 0, ${controlStyles.backgroundColor} 6px)`,
      [controlStyles],
    )

    const [mixed, setMixed] = React.useState<boolean>(controlStyles.mixed)
    const [
      stateValue,
      setStateValueDirectly,
      forceStateValueToUpdateFromProps,
    ] = usePropControlledState(getDisplayValue(propsValue ?? null, defaultUnitToHide, mixed))
    const updateStateValue = React.useCallback(
      (newValue: CSSNumber) =>
        setStateValueDirectly(getDisplayValue(newValue, defaultUnitToHide, false)),
      [defaultUnitToHide, setStateValueDirectly],
    )
    const parsedStateValue = parseDisplayValue(stateValue, numberType, defaultUnitToHide)
    const parsedStateValueUnit = EitherUtils.unwrapEither(
      EitherUtils.mapEither((v) => v.unit, parsedStateValue),
      null,
    )

    const [isActuallyFocused, setIsActuallyFocused] = React.useState<boolean>(false)
    const [isFauxcused, setIsFauxcused] = React.useState<boolean>(false)
    const isFocused = isActuallyFocused || isFauxcused

    const [labelDragDirection, setLabelDragDirection] = React.useState<LabelDragDirection>(
      'horizontal',
    )

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

    const scaleFactor = parsedStateValueUnit === '%' ? 100 : 1
    const minimum = scaleFactor * unscaledMinimum
    const maximum = scaleFactor * unscaledMaximum
    const stepSize = unscaledStepSize == null ? 1 : unscaledStepSize * scaleFactor

    const incrementBy = React.useCallback(
      (
        currentValue: CSSNumber,
        incrementStepSize: number,
        shiftKey: boolean,
        transient: boolean,
      ) => {
        const delta = incrementStepSize * (shiftKey ? 10 : 1)
        const newNumericValue = clampValue(currentValue.value + delta, minimum, maximum)
        const newValue = setCSSNumberValue(currentValue, newNumericValue)
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
        updateStateValue(newValue)
        return newValue
      },
      [
        updateStateValue,
        maximum,
        minimum,
        onForcedSubmitValue,
        onSubmitValue,
        onTransientSubmitValue,
      ],
    )

    const repeatIncrement = React.useCallback(
      (
        currentValue: CSSNumber,
        incrementStepSize: number,
        shiftKey: boolean,
        transient: boolean,
      ) => {
        const newValue = incrementBy(currentValue, incrementStepSize, shiftKey, transient)
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
        updateStateValue(newValue)
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
        updateStateValue,
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
            parsedStateValueUnit,
            e.screenX,
            e.screenY,
            dragOriginX.current,
            dragOriginY.current,
            true,
          )
        })
      },
      [setScrubValue, parsedStateValueUnit],
    )

    const scrubOnMouseUp = React.useCallback(
      (e: MouseEvent) => {
        window.removeEventListener('mouseup', scrubOnMouseUp)
        window.removeEventListener('mousemove', scrubOnMouseMove)

        setIsFauxcused(false)
        ref.current?.focus()

        onThresholdPassed(e, () => {
          setScrubValue(
            parsedStateValueUnit,
            e.screenX,
            e.screenY,
            dragOriginX.current,
            dragOriginY.current,
            false,
          )
        })
        setScrubThresholdPassed(false)
      },
      [scrubOnMouseMove, setScrubValue, parsedStateValueUnit, ref],
    )

    const rc = roundCorners == null ? 'all' : roundCorners
    const borderRadiusStyles = getBorderRadiusStyles(chained, rc)

    const onFocus = React.useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        setIsActuallyFocused(true)
        if (inputProps.onFocus != null) {
          inputProps.onFocus(e)
        }
      },
      [inputProps],
    )

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        if (e.key === 'ArrowUp' && propsValue != null) {
          incrementBy(propsValue, stepSize, e.shiftKey, false)
        } else if (e.key === 'ArrowDown' && propsValue != null) {
          incrementBy(propsValue, -stepSize, e.shiftKey, false)
        } else if (e.key === 'Enter' || e.key === 'Escape') {
          e.nativeEvent.stopImmediatePropagation()
          e.preventDefault()
          ref.current?.blur()
        }
      },
      [incrementBy, propsValue, stepSize, ref],
    )

    const onKeyUp = React.useCallback(
      (e: React.KeyboardEvent<HTMLInputElement>) => {
        // todo make sure this isn't doubling up the value submit
        if (
          (e.key === 'ArrowUp' || e.key === 'ArrowDown') &&
          onForcedSubmitValue != null &&
          propsValue != null
        ) {
          onForcedSubmitValue(propsValue)
        }
      },
      [onForcedSubmitValue, propsValue],
    )

    const onBlur = React.useCallback(
      (e: React.FocusEvent<HTMLInputElement>) => {
        setIsActuallyFocused(false)
        if (inputProps.onBlur != null) {
          inputProps.onBlur(e)
        }
        if (valueChangedSinceFocus) {
          setValueChangedSinceFocus(false)
          if (onSubmitValue != null) {
            if (stateValue === '') {
              onSubmitValue(emptyInputValue())
              forceStateValueToUpdateFromProps()
            } else if (EitherUtils.isRight(parsedStateValue)) {
              onSubmitValue(parsedStateValue.value)
            } else {
              forceStateValueToUpdateFromProps()
            }
          }
        }
      },
      [
        inputProps,
        onSubmitValue,
        stateValue,
        parsedStateValue,
        valueChangedSinceFocus,
        forceStateValueToUpdateFromProps,
      ],
    )

    const onChange = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => {
        const value = e.target.value
        if (inputProps.onChange != null) {
          inputProps.onChange(e)
        }
        setValueChangedSinceFocus(true)
        setMixed(false)
        setStateValueDirectly(value)
      },
      [inputProps, setStateValueDirectly],
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
    }, [ref])

    const onIncrementMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          if (propsValue != null) {
            window.addEventListener('mouseup', onIncrementMouseUp)
            const shiftKey = e.shiftKey
            const newValue = incrementBy(propsValue, stepSize, shiftKey, false)
            incrementTimeout = window.setTimeout(() => {
              repeatIncrement(newValue, stepSize, shiftKey, true)
            }, repeatThreshold)
          }
        }
      },
      [incrementBy, propsValue, stepSize, repeatIncrement, onIncrementMouseUp],
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
    }, [ref])

    const onDecrementMouseDown = React.useCallback(
      (e: React.MouseEvent) => {
        if (e.button === 0) {
          e.stopPropagation()
          setIsFauxcused(true)
          if (propsValue != null) {
            window.addEventListener('mouseup', onDecrementMouseUp)
            const shiftKey = e.shiftKey
            const newValue = incrementBy(propsValue, -stepSize, shiftKey, false)
            incrementTimeout = window.setTimeout(
              () => repeatIncrement(newValue, -stepSize, shiftKey, true),
              repeatThreshold,
            )
          }
        }
      },
      [incrementBy, propsValue, stepSize, repeatIncrement, onDecrementMouseUp],
    )

    const onLabelMouseDown = React.useCallback(
      (e: React.MouseEvent<HTMLDivElement>) => {
        if (e.button === 0) {
          e.stopPropagation()
          if (propsValue != null) {
            setIsFauxcused(true)
            window.addEventListener('mousemove', scrubOnMouseMove)
            window.addEventListener('mouseup', scrubOnMouseUp)
            setLabelDragDirection('horizontal')
            setValueAtDragOrigin(propsValue.value)
            setDragOriginX(e.screenX)
            setDragOriginY(e.screenY)
          }
        }
      },
      [propsValue, scrubOnMouseMove, scrubOnMouseUp],
    )

    const formClassName = classNames('number-input-wrapper', className)

    let placeholder: string = ''
    if (controlStyles.unknown) {
      placeholder = 'unknown'
    } else if (controlStyles.mixed) {
      placeholder = 'mixed'
    }

    const chainedStyles: ObjectInterpolation<any> | undefined =
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
      // this form madness is a hack due to chrome ignoring autoComplete='off' on individual `input`s
      <form className={formClassName} style={style} autoComplete='off'>
        <div
          className='number-input-container'
          css={{
            zIndex: isFocused ? 3 : undefined,
            position: 'relative',
            ...chainedStyles,
          }}
        >
          <InspectorInput
            {...inputProps}
            chained={chained}
            controlStyles={controlStyles}
            focused={isFocused}
            labelInner={labelInner}
            roundCorners={roundCorners}
            mixed={mixed}
            value={stateValue}
            ref={ref}
            className='number-input'
            height={height}
            id={id}
            placeholder={placeholder}
            onFocus={onFocus}
            onKeyDown={onKeyDown}
            onKeyUp={onKeyUp}
            onBlur={onBlur}
            onChange={onChange}
          />
          {labelInner != null ? (
            <div
              className='number-input-innerLabel'
              css={{
                position: 'absolute',
                top: 1,
                right: 1,
                userSelect: 'none',
                pointerEvents: 'none',
                width: 20,
                height: 20,
                backgroundImage: backgroundImage,
                borderRadius: UtopiaTheme.inputBorderRadius,
                display: 'block',
                '.number-input-container:hover &': {
                  display: incrementControls && controlStyles.interactive ? 'none' : 'block',
                },
              }}
            >
              <div
                style={{
                  position: 'absolute',
                  pointerEvents: 'none',
                  left: 2,
                  top: 5,
                  fontWeight: 'bold',
                  fontSize: '9px',
                  width: '100%',
                  height: '100%',
                  color: controlStyles.secondaryColor,
                }}
              >
                {typeof labelInner === 'object' && 'type' in labelInner ? (
                  <Icn {...labelInner} />
                ) : (
                  labelInner
                )}
              </div>
            </div>
          ) : null}
          {incrementControls && controlStyles.interactive ? (
            <div
              className='number-input-increment-controls'
              css={{
                position: 'absolute',
                top: 0,
                right: 0,
                flexDirection: 'column',
                alignItems: 'stretch',
                width: 11,
                height: UtopiaTheme.layout.inputHeight.default,
                backgroundColor: UtopiaTheme.color.inspectorEmphasizedBackground.value,
                boxShadow: `1px 0 ${controlStyles.borderColor} inset`,
                display: 'none',
                '.number-input-container:hover &': {
                  display: 'block',
                },
              }}
            >
              <div
                css={{
                  height: '50%',
                  boxShadow: `-1px 1px ${
                    isFocused
                      ? UtopiaTheme.color.inspectorFocusedColor.value
                      : controlStyles.borderColor
                  } inset, 1px 0 ${controlStyles.borderColor} inset`,
                  position: 'relative',
                  borderTopRightRadius: borderRadiusStyles.borderTopRightRadius,
                  ':active': {
                    backgroundColor: colorTheme.buttonActiveBackground.value,
                  },
                  '::after': {
                    content: '""',
                    width: 'calc(100% - 1px)',
                    height: 1,
                    position: 'absolute',
                    right: 1,
                    bottom: 0,
                    transform: 'translateY(0.5px)',
                    backgroundColor: controlStyles.borderColor,
                    pointerEvents: 'none',
                  },
                }}
                onMouseDown={onIncrementMouseDown}
              >
                <Icn category='controls/input' type='up' color='gray' width={11} height={11} />
              </div>
              <div
                css={{
                  height: '50%',
                  boxShadow: `-1px -1px ${
                    isFocused
                      ? UtopiaTheme.color.inspectorFocusedColor.value
                      : controlStyles.borderColor
                  } inset, 1px 0 ${controlStyles.borderColor} inset`,
                  borderBottomRightRadius: borderRadiusStyles.borderBottomRightRadius,
                  ':active': {
                    backgroundColor: colorTheme.buttonActiveBackground.value,
                  },
                }}
                onMouseDown={onDecrementMouseDown}
              >
                <Icn category='controls/input' type='down' color='gray' width={11} height={11} />
              </div>
            </div>
          ) : null}
        </div>
        {DEPRECATED_labelBelow == null && controlStatus != 'off' ? null : (
          <div
            onMouseDown={onLabelMouseDown}
            style={{
              cursor: CSSCursor.ResizeEW,
              fontSize: 9,
              textAlign: 'center',
              display: 'block',
              color: controlStyles.secondaryColor,
              paddingTop: 2,
            }}
          >
            {DEPRECATED_labelBelow != null ? DEPRECATED_labelBelow : null}
          </div>
        )}
      </form>
    )
  },
)
interface SimpleNumberInputProps extends Omit<AbstractNumberInputProps<number>, 'numberType'> {}

function wrappedSimpleOnSubmitValue(
  onSubmitValue: OnSubmitValueOrEmpty<number> | undefined,
): OnSubmitValueOrEmpty<CSSNumber> | undefined {
  if (onSubmitValue == null) {
    return undefined
  } else {
    return (value: CSSNumber | EmptyInputValue) =>
      onSubmitValue(isEmptyInputValue(value) ? value : value.value)
  }
}

export const SimpleNumberInput = betterReactMemo(
  'SimpleNumberInput',
  (props: SimpleNumberInputProps) => {
    const {
      value,
      onSubmitValue,
      onTransientSubmitValue,
      onForcedSubmitValue,
      ...sharedProps
    } = props
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

interface SimplePercentInputProps extends Omit<AbstractNumberInputProps<number>, 'numberType'> {}

function wrappedPercentOnSubmitValue(
  onSubmitValue: OnSubmitValueOrEmpty<number> | undefined,
): OnSubmitValueOrEmpty<CSSNumber> | undefined {
  if (onSubmitValue == null) {
    return undefined
  } else {
    return (value: CSSNumber | EmptyInputValue) =>
      onSubmitValue(isEmptyInputValue(value) ? value : value.value / 100)
  }
}

export const SimplePercentInput = betterReactMemo(
  'SimplePercentInput',
  (props: SimplePercentInputProps) => {
    const {
      value,
      onSubmitValue,
      onTransientSubmitValue,
      onForcedSubmitValue,
      ...sharedProps
    } = props
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
}

export const ChainedNumberInput: React.FunctionComponent<ChainedNumberControlProps> = betterReactMemo(
  'ChainedNumberInput',
  ({ propsArray, idPrefix, style }) => {
    return (
      <FlexRow style={style}>
        {propsArray.map((props, i) => {
          switch (i) {
            case 0: {
              return (
                <NumberInput
                  id={`${idPrefix}-${i}`}
                  {...props}
                  chained='first'
                  roundCorners='left'
                />
              )
            }
            case propsArray.length - 1: {
              return (
                <NumberInput
                  id={`${idPrefix}-${i}`}
                  {...props}
                  chained='last'
                  roundCorners='right'
                />
              )
            }
            default: {
              return (
                <NumberInput
                  id={`${idPrefix}-${i}`}
                  {...props}
                  chained='middle'
                  roundCorners='none'
                />
              )
            }
          }
        })}
      </FlexRow>
    )
  },
)

export function useWrappedEmptyOnSubmitValue<T>(
  onSubmitValue: OnSubmitValue<T>,
  onUnsetValue: OnUnsetValues,
): OnSubmitValueOrEmpty<T> {
  return React.useCallback(
    (value: T | EmptyInputValue) => {
      if (isEmptyInputValue(value)) {
        onUnsetValue()
      } else {
        onSubmitValue(value)
      }
    },
    [onSubmitValue, onUnsetValue],
  )
}
